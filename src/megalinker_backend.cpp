#include "megalinker_backend.h"
#include "backend_registry.h"
#include "codegen.h"
#include "constants.h"
#include "function_key.h"
#include "semantics.h"
#include "common.h"
#include <algorithm>
#include <cctype>
#include <cstring>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <queue>
#include <sstream>
#include <optional>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace vexel {

using megalinker_codegen::CCodegenResult;
using megalinker_codegen::CallTargetInfo;
using megalinker_codegen::CodegenABI;
using megalinker_codegen::CodeGenerator;
using megalinker_codegen::GeneratedFunctionInfo;
using megalinker_codegen::PtrKind;

namespace {

static void write_file(const std::string& path, const std::string& content) {
    std::ofstream file(path);
    if (!file) {
        throw CompileError("Cannot write file: " + path, SourceLocation());
    }
    file << content;
}

static bool has_annotation(const std::vector<Annotation>& anns, const std::string& name) {
    return std::any_of(anns.begin(), anns.end(), [&](const Annotation& ann) { return ann.name == name; });
}

static std::optional<int> sdcccall_mode_for_function_decl(const StmtPtr& decl) {
    if (!decl || decl->kind != Stmt::Kind::FuncDecl) return std::nullopt;

    std::optional<int> mode;
    for (const auto& ann : decl->annotations) {
        if (ann.name != "sdcccall") continue;

        if (!decl->is_external && !decl->is_exported) {
            throw CompileError("Megalinker backend: [[sdcccall]] is only valid on ABI-visible functions (&! and &^)",
                               ann.location);
        }
        if (ann.args.size() != 1) {
            throw CompileError("Megalinker backend: [[sdcccall]] requires exactly one argument (0 or 1)",
                               ann.location);
        }

        const std::string& arg = ann.args[0];
        int parsed = -1;
        if (arg == "0") {
            parsed = 0;
        } else if (arg == "1") {
            parsed = 1;
        } else {
            throw CompileError("Megalinker backend: [[sdcccall]] argument must be 0 or 1",
                               ann.location);
        }

        if (mode && *mode != parsed) {
            throw CompileError("Megalinker backend: conflicting [[sdcccall]] annotations on the same function",
                               ann.location);
        }
        mode = parsed;
    }
    return mode;
}

static std::string sdcccall_suffix_for_function_decl(const StmtPtr& decl) {
    std::optional<int> mode = sdcccall_mode_for_function_decl(decl);
    if (!mode.has_value()) return "";
    return " __sdcccall(" + std::to_string(*mode) + ")";
}

static bool contains_named_struct_type(TypePtr type) {
    if (!type) return false;
    switch (type->kind) {
        case Type::Kind::Named:
            return true;
        case Type::Kind::Array:
            return contains_named_struct_type(type->element_type);
        case Type::Kind::TypeOf:
            if (type->typeof_expr && type->typeof_expr->type) {
                return contains_named_struct_type(type->typeof_expr->type);
            }
            return false;
        default:
            return false;
    }
}

static void validate_megalinker_external_function_abi(const Module& mod) {
    auto fail = [](const StmtPtr& stmt, const SourceLocation& loc, const std::string& detail) {
        std::string name;
        if (stmt) {
            if (!stmt->type_namespace.empty()) {
                name = stmt->type_namespace + "::" + stmt->func_name;
            } else {
                name = stmt->func_name;
            }
        }
        if (name.empty()) name = "<anonymous>";
        throw CompileError("Megalinker backend does not support named-struct " + detail +
                               " on external function '" + name + "'",
                           loc);
    };

    for (const auto& stmt : mod.top_level) {
        if (!stmt || stmt->kind != Stmt::Kind::FuncDecl || !stmt->is_external) continue;

        for (size_t i = 0; i < stmt->ref_param_types.size() && i < stmt->ref_params.size(); ++i) {
            TypePtr type = stmt->ref_param_types[i];
            if (contains_named_struct_type(type)) {
                fail(stmt,
                     type ? type->location : stmt->location,
                     "receiver type '" + (type ? type->to_string() : std::string("unknown")) + "'");
            }
        }
        for (const auto& param : stmt->params) {
            if (contains_named_struct_type(param.type)) {
                fail(stmt,
                     param.type ? param.type->location : param.location,
                     "parameter type '" + (param.type ? param.type->to_string() : std::string("unknown")) + "'");
            }
        }
        if (contains_named_struct_type(stmt->return_type)) {
            fail(stmt,
                 stmt->return_type ? stmt->return_type->location : stmt->location,
                 "return type '" + (stmt->return_type ? stmt->return_type->to_string() : std::string("unknown")) + "'");
        }
        for (const auto& type : stmt->return_types) {
            if (contains_named_struct_type(type)) {
                fail(stmt,
                     type ? type->location : stmt->location,
                     "tuple return element type '" + (type ? type->to_string() : std::string("unknown")) + "'");
            }
        }
    }
}

static void validate_megalinker_function_annotations(const Module& mod) {
    for (const auto& stmt : mod.top_level) {
        if (!stmt || stmt->kind != Stmt::Kind::FuncDecl) continue;
        (void)sdcccall_mode_for_function_decl(stmt);
    }
}

static BackendAnalysisRequirements megalinker_analysis_requirements(const Compiler::Options&,
                                                                    std::string&) {
    BackendAnalysisRequirements req;
    req.required_passes = kAllAnalysisPasses;
    req.default_entry_reentrancy = 'R';
    req.default_exit_reentrancy = 'R';
    return req;
}

static ReentrancyMode megalinker_boundary_reentrancy_mode(const Symbol& sym,
                                                          ReentrancyBoundaryKind boundary,
                                                          const Compiler::Options&,
                                                          std::string& error) {
    (void)boundary;
    (void)error;
    if (!sym.declaration) {
        return ReentrancyMode::Default;
    }
    bool is_nonreentrant = has_annotation(sym.declaration->annotations, "nonreentrant");
    if (is_nonreentrant) return ReentrancyMode::NonReentrant;
    return ReentrancyMode::Default;
}

static std::string qualified_name(const StmtPtr& stmt) {
    if (!stmt) return "";
    if (!stmt->type_namespace.empty()) {
        return stmt->type_namespace + "::" + stmt->func_name;
    }
    return stmt->func_name;
}

static std::string symbol_key(const std::string& name, int scope_id) {
    if (scope_id >= 0) {
        return name + "#" + std::to_string(scope_id);
    }
    return name;
}

static int scope_id_for_symbol(const Symbol* sym, int entry_instance_id) {
    if (!sym || sym->is_local) return -1;
    if (sym->instance_id < 0 || sym->instance_id == entry_instance_id) return -1;
    return sym->instance_id;
}

static std::string symbol_key_for(const Symbol* sym, int entry_instance_id) {
    if (!sym) return "";
    return symbol_key(sym->name, scope_id_for_symbol(sym, entry_instance_id));
}

static std::string func_key_for(const Symbol* sym, int entry_instance_id) {
    if (!sym) return "";
    return reachability_key(sym->name, scope_id_for_symbol(sym, entry_instance_id));
}

static std::string sanitize_tag(const std::string& input) {
    std::string out;
    out.reserve(input.size());
    for (unsigned char c : input) {
        if (std::isalnum(c) || c == '_') {
            out.push_back(static_cast<char>(c));
        } else {
            out.push_back('_');
        }
    }
    if (out.empty()) out = "v";
    return out;
}

struct FunctionPrototype {
    std::string declaration;
    std::string arg_list;
    std::string return_type;
    bool returns_void = true;
};

static FunctionPrototype build_function_prototype(const GeneratedFunctionInfo& info,
                                                  const std::string& symbol_name,
                                                  bool include_storage,
                                                  const std::string& suffix = std::string()) {
    FunctionPrototype proto;
    proto.return_type = info.return_type.empty() ? "void" : info.return_type;
    proto.returns_void = info.returns_void;

    std::ostringstream decl;
    if (include_storage) {
        decl << info.storage;
    }
    decl << proto.return_type << " " << symbol_name << "(";
    if (info.params.empty()) {
        decl << "void";
    } else {
        for (size_t i = 0; i < info.params.size(); ++i) {
            if (i > 0) decl << ", ";
            decl << info.params[i].type << " " << info.params[i].name;
            if (i > 0) proto.arg_list += ", ";
            proto.arg_list += info.params[i].name;
        }
    }
    decl << ")";
    decl << suffix;
    decl << ";";
    proto.declaration = decl.str();
    return proto;
}

static void collect_call_exprs(ExprPtr expr, std::vector<ExprPtr>& calls) {
    if (!expr) return;
    if (expr->kind == Expr::Kind::Call) {
        calls.push_back(expr);
    }

    switch (expr->kind) {
        case Expr::Kind::Call:
            for (const auto& rec : expr->receivers) collect_call_exprs(rec, calls);
            for (const auto& arg : expr->args) collect_call_exprs(arg, calls);
            collect_call_exprs(expr->operand, calls);
            break;
        case Expr::Kind::Binary:
            collect_call_exprs(expr->left, calls);
            collect_call_exprs(expr->right, calls);
            break;
        case Expr::Kind::Unary:
            collect_call_exprs(expr->operand, calls);
            break;
        case Expr::Kind::Index:
            collect_call_exprs(expr->left, calls);
            collect_call_exprs(expr->right, calls);
            break;
        case Expr::Kind::Member:
            collect_call_exprs(expr->operand, calls);
            break;
        case Expr::Kind::ArrayLiteral:
        case Expr::Kind::TupleLiteral:
            for (const auto& elem : expr->elements) collect_call_exprs(elem, calls);
            break;
        case Expr::Kind::Block:
            for (const auto& st : expr->statements) {
                if (!st) continue;
                if (st->expr) collect_call_exprs(st->expr, calls);
                if (st->return_expr) collect_call_exprs(st->return_expr, calls);
            }
            collect_call_exprs(expr->result_expr, calls);
            break;
        case Expr::Kind::Conditional:
            collect_call_exprs(expr->condition, calls);
            collect_call_exprs(expr->true_expr, calls);
            collect_call_exprs(expr->false_expr, calls);
            break;
        case Expr::Kind::Cast:
            collect_call_exprs(expr->operand, calls);
            break;
        case Expr::Kind::Assignment:
            collect_call_exprs(expr->left, calls);
            collect_call_exprs(expr->right, calls);
            break;
        case Expr::Kind::Range:
            collect_call_exprs(expr->left, calls);
            collect_call_exprs(expr->right, calls);
            break;
        case Expr::Kind::Length:
            collect_call_exprs(expr->operand, calls);
            break;
        case Expr::Kind::Iteration:
        case Expr::Kind::Repeat:
            collect_call_exprs(expr->left, calls);
            if (expr->right) collect_call_exprs(expr->right, calls);
            break;
        default:
            break;
    }
}

static bool expr_has_explicit_return(ExprPtr expr);

static bool stmt_has_explicit_return(StmtPtr stmt) {
    if (!stmt) return false;
    switch (stmt->kind) {
        case Stmt::Kind::Return:
            return true;
        case Stmt::Kind::Expr:
            return expr_has_explicit_return(stmt->expr);
        case Stmt::Kind::VarDecl:
            return expr_has_explicit_return(stmt->var_init);
        case Stmt::Kind::ConditionalStmt:
            return expr_has_explicit_return(stmt->condition) ||
                   stmt_has_explicit_return(stmt->true_stmt);
        default:
            return false;
    }
}

static bool expr_has_explicit_return(ExprPtr expr) {
    if (!expr) return false;
    switch (expr->kind) {
        case Expr::Kind::Block:
            for (const auto& st : expr->statements) {
                if (stmt_has_explicit_return(st)) return true;
            }
            return expr_has_explicit_return(expr->result_expr);
        case Expr::Kind::Conditional:
            return expr_has_explicit_return(expr->condition) ||
                   expr_has_explicit_return(expr->true_expr) ||
                   expr_has_explicit_return(expr->false_expr);
        case Expr::Kind::Call:
            for (const auto& rec : expr->receivers) {
                if (expr_has_explicit_return(rec)) return true;
            }
            for (const auto& arg : expr->args) {
                if (expr_has_explicit_return(arg)) return true;
            }
            return expr_has_explicit_return(expr->operand);
        case Expr::Kind::Binary:
            return expr_has_explicit_return(expr->left) || expr_has_explicit_return(expr->right);
        case Expr::Kind::Unary:
        case Expr::Kind::Cast:
        case Expr::Kind::Length:
            return expr_has_explicit_return(expr->operand);
        case Expr::Kind::Index:
            return expr_has_explicit_return(expr->left) || expr_has_explicit_return(expr->right);
        case Expr::Kind::Member:
            return expr_has_explicit_return(expr->operand);
        case Expr::Kind::ArrayLiteral:
        case Expr::Kind::TupleLiteral:
            for (const auto& elem : expr->elements) {
                if (expr_has_explicit_return(elem)) return true;
            }
            return false;
        case Expr::Kind::Assignment:
            return expr_has_explicit_return(expr->left) || expr_has_explicit_return(expr->right);
        case Expr::Kind::Range:
        case Expr::Kind::Iteration:
        case Expr::Kind::Repeat:
            return expr_has_explicit_return(expr->left) || expr_has_explicit_return(expr->right);
        default:
            return false;
    }
}

static size_t expr_inline_cost(ExprPtr expr);

static size_t stmt_inline_cost(StmtPtr stmt) {
    if (!stmt) return 0;
    size_t cost = 1;
    switch (stmt->kind) {
        case Stmt::Kind::Expr:
            return cost + expr_inline_cost(stmt->expr);
        case Stmt::Kind::VarDecl:
            return cost + expr_inline_cost(stmt->var_init);
        case Stmt::Kind::Return:
            return cost + expr_inline_cost(stmt->return_expr);
        case Stmt::Kind::ConditionalStmt:
            return cost + expr_inline_cost(stmt->condition) + stmt_inline_cost(stmt->true_stmt);
        default:
            return cost;
    }
}

static size_t expr_inline_cost(ExprPtr expr) {
    if (!expr) return 0;
    size_t cost = 1;
    switch (expr->kind) {
        case Expr::Kind::Call:
            cost += 8;
            for (const auto& rec : expr->receivers) cost += expr_inline_cost(rec);
            for (const auto& arg : expr->args) cost += expr_inline_cost(arg);
            cost += expr_inline_cost(expr->operand);
            return cost;
        case Expr::Kind::Iteration:
        case Expr::Kind::Repeat:
            cost += 4;
            return cost + expr_inline_cost(expr->left) + expr_inline_cost(expr->right);
        case Expr::Kind::Block:
            for (const auto& st : expr->statements) cost += stmt_inline_cost(st);
            return cost + expr_inline_cost(expr->result_expr);
        case Expr::Kind::Conditional:
            return cost + expr_inline_cost(expr->condition) +
                   expr_inline_cost(expr->true_expr) + expr_inline_cost(expr->false_expr);
        case Expr::Kind::Binary:
            return cost + expr_inline_cost(expr->left) + expr_inline_cost(expr->right);
        case Expr::Kind::Unary:
        case Expr::Kind::Cast:
        case Expr::Kind::Length:
            return cost + expr_inline_cost(expr->operand);
        case Expr::Kind::Index:
            return cost + expr_inline_cost(expr->left) + expr_inline_cost(expr->right);
        case Expr::Kind::Member:
            return cost + expr_inline_cost(expr->operand);
        case Expr::Kind::ArrayLiteral:
        case Expr::Kind::TupleLiteral:
            for (const auto& elem : expr->elements) cost += expr_inline_cost(elem);
            return cost;
        case Expr::Kind::Assignment:
            return cost + expr_inline_cost(expr->left) + expr_inline_cost(expr->right);
        case Expr::Kind::Range:
            return cost + expr_inline_cost(expr->left) + expr_inline_cost(expr->right);
        default:
            return cost;
    }
}

static std::unordered_set<std::string> compute_recursive_function_keys(
    const std::unordered_map<std::string, std::unordered_set<std::string>>& edges) {
    std::unordered_set<std::string> nodes;
    for (const auto& kv : edges) {
        nodes.insert(kv.first);
        for (const auto& succ : kv.second) nodes.insert(succ);
    }

    std::unordered_map<std::string, std::unordered_set<std::string>> reverse;
    for (const auto& kv : edges) {
        reverse[kv.first];
        for (const auto& succ : kv.second) {
            reverse[succ].insert(kv.first);
        }
    }

    std::unordered_set<std::string> visited;
    std::vector<std::string> order;
    std::function<void(const std::string&)> dfs1 = [&](const std::string& node) {
        if (visited.count(node)) return;
        visited.insert(node);
        auto it = edges.find(node);
        if (it != edges.end()) {
            for (const auto& succ : it->second) dfs1(succ);
        }
        order.push_back(node);
    };
    for (const auto& node : nodes) dfs1(node);

    visited.clear();
    std::unordered_set<std::string> recursive;
    std::function<void(const std::string&, std::vector<std::string>&)> dfs2 =
        [&](const std::string& node, std::vector<std::string>& comp) {
            if (visited.count(node)) return;
            visited.insert(node);
            comp.push_back(node);
            auto it = reverse.find(node);
            if (it == reverse.end()) return;
            for (const auto& pred : it->second) dfs2(pred, comp);
        };

    for (auto it = order.rbegin(); it != order.rend(); ++it) {
        if (visited.count(*it)) continue;
        std::vector<std::string> comp;
        dfs2(*it, comp);
        if (comp.size() > 1) {
            for (const auto& node : comp) recursive.insert(node);
            continue;
        }
        const std::string& node = comp.front();
        auto edge_it = edges.find(node);
        if (edge_it != edges.end() && edge_it->second.count(node)) {
            recursive.insert(node);
        }
    }

    return recursive;
}

static std::string array_size_str(const AnalyzedProgram& analyzed, TypePtr type,
                                  int instance_id,
                                  const SourceLocation& loc) {
    if (!type || type->kind != Type::Kind::Array || !type->array_size) {
        throw CompileError("Array size must be compile-time constant", loc);
    }
    if (!analyzed.optimization) {
        throw CompileError("Array size must be compile-time constant", loc);
    }
    auto it = analyzed.optimization->constexpr_values.find(
        expr_fact_key(instance_id, type->array_size.get()));
    if (it == analyzed.optimization->constexpr_values.end()) {
        throw CompileError("Array size must be compile-time constant", loc);
    }
    const CTValue& size_val = it->second;
    if (std::holds_alternative<int64_t>(size_val)) {
        return std::to_string(std::get<int64_t>(size_val));
    }
    if (std::holds_alternative<uint64_t>(size_val)) {
        return std::to_string(std::get<uint64_t>(size_val));
    }
    throw CompileError("Array size must be an integer constant", loc);
}

struct GlobalInfo {
    StmtPtr decl;
    const Symbol* sym = nullptr;
    bool is_rom = false;
    bool is_pointer_like = false;
    std::string module_name;
    std::string c_name;
    int scope_id = -1;
};

static bool expr_uses_rom_symbol(ExprPtr expr,
                                 const std::unordered_map<std::string, GlobalInfo>& globals,
                                 const AnalyzedProgram& analyzed,
                                 int instance_id,
                                 int entry_instance_id);

static bool stmt_uses_rom_symbol(StmtPtr stmt,
                                 const std::unordered_map<std::string, GlobalInfo>& globals,
                                 const AnalyzedProgram& analyzed,
                                 int instance_id,
                                 int entry_instance_id) {
    if (!stmt) return false;
    switch (stmt->kind) {
        case Stmt::Kind::Expr:
            return expr_uses_rom_symbol(stmt->expr, globals, analyzed, instance_id, entry_instance_id);
        case Stmt::Kind::Return:
            return expr_uses_rom_symbol(stmt->return_expr, globals, analyzed, instance_id, entry_instance_id);
        case Stmt::Kind::VarDecl:
            return expr_uses_rom_symbol(stmt->var_init, globals, analyzed, instance_id, entry_instance_id);
        case Stmt::Kind::ConditionalStmt:
            if (expr_uses_rom_symbol(stmt->condition, globals, analyzed, instance_id, entry_instance_id)) return true;
            return stmt_uses_rom_symbol(stmt->true_stmt, globals, analyzed, instance_id, entry_instance_id);
        default:
            return false;
    }
}

static bool expr_uses_rom_symbol(ExprPtr expr,
                                 const std::unordered_map<std::string, GlobalInfo>& globals,
                                 const AnalyzedProgram& analyzed,
                                 int instance_id,
                                 int entry_instance_id) {
    if (!expr) return false;
    switch (expr->kind) {
        case Expr::Kind::Identifier: {
            Symbol* sym = analyzed.binding_for ? analyzed.binding_for(instance_id, expr.get()) : nullptr;
            if (!sym) return false;
            std::string key = symbol_key_for(sym, entry_instance_id);
            auto it = globals.find(key);
            return it != globals.end() && it->second.is_rom;
        }
        case Expr::Kind::Call:
            for (const auto& rec : expr->receivers) {
                if (expr_uses_rom_symbol(rec, globals, analyzed, instance_id, entry_instance_id)) return true;
            }
            for (const auto& arg : expr->args) {
                if (expr_uses_rom_symbol(arg, globals, analyzed, instance_id, entry_instance_id)) return true;
            }
            if (expr->operand && expr->operand->kind != Expr::Kind::Identifier) {
                return expr_uses_rom_symbol(expr->operand, globals, analyzed, instance_id, entry_instance_id);
            }
            return false;
        case Expr::Kind::Binary:
            return expr_uses_rom_symbol(expr->left, globals, analyzed, instance_id, entry_instance_id) ||
                   expr_uses_rom_symbol(expr->right, globals, analyzed, instance_id, entry_instance_id);
        case Expr::Kind::Unary:
            return expr_uses_rom_symbol(expr->operand, globals, analyzed, instance_id, entry_instance_id);
        case Expr::Kind::Index:
            return expr_uses_rom_symbol(expr->left, globals, analyzed, instance_id, entry_instance_id) ||
                   expr_uses_rom_symbol(expr->right, globals, analyzed, instance_id, entry_instance_id);
        case Expr::Kind::Member:
            return expr_uses_rom_symbol(expr->operand, globals, analyzed, instance_id, entry_instance_id);
        case Expr::Kind::ArrayLiteral:
        case Expr::Kind::TupleLiteral:
            for (const auto& elem : expr->elements) {
                if (expr_uses_rom_symbol(elem, globals, analyzed, instance_id, entry_instance_id)) return true;
            }
            return false;
        case Expr::Kind::Block:
            for (const auto& st : expr->statements) {
                if (stmt_uses_rom_symbol(st, globals, analyzed, instance_id, entry_instance_id)) return true;
            }
            return expr_uses_rom_symbol(expr->result_expr, globals, analyzed, instance_id, entry_instance_id);
        case Expr::Kind::Conditional:
            return expr_uses_rom_symbol(expr->condition, globals, analyzed, instance_id, entry_instance_id) ||
                   expr_uses_rom_symbol(expr->true_expr, globals, analyzed, instance_id, entry_instance_id) ||
                   expr_uses_rom_symbol(expr->false_expr, globals, analyzed, instance_id, entry_instance_id);
        case Expr::Kind::Cast:
            return expr_uses_rom_symbol(expr->operand, globals, analyzed, instance_id, entry_instance_id);
        case Expr::Kind::Assignment:
            return expr_uses_rom_symbol(expr->left, globals, analyzed, instance_id, entry_instance_id) ||
                   expr_uses_rom_symbol(expr->right, globals, analyzed, instance_id, entry_instance_id);
        case Expr::Kind::Range:
            return expr_uses_rom_symbol(expr->left, globals, analyzed, instance_id, entry_instance_id) ||
                   expr_uses_rom_symbol(expr->right, globals, analyzed, instance_id, entry_instance_id);
        case Expr::Kind::Length:
            return expr_uses_rom_symbol(expr->operand, globals, analyzed, instance_id, entry_instance_id);
        case Expr::Kind::Iteration:
        case Expr::Kind::Repeat:
            return expr_uses_rom_symbol(expr->left, globals, analyzed, instance_id, entry_instance_id) ||
                   expr_uses_rom_symbol(expr->right, globals, analyzed, instance_id, entry_instance_id);
        default:
            return false;
    }
}

struct Variant {
    std::string id;
    std::string func_key;
    std::string signature_key;
    std::string caller_id;
    StmtPtr decl;
    const Symbol* sym = nullptr;
    int instance_id = -1;
    int scope_id = -1;
    std::string ref_key;
    char reent_key = 'N';
    std::vector<PtrKind> param_kinds;
    std::unordered_map<std::string, PtrKind> param_kind_by_name;
    char page = 'A';
    bool alters_caller_page = false;
    bool needs_restore = false;
    std::string name;
    std::string c_name;
    std::string module_name;
    GeneratedFunctionInfo info;
    size_t inline_depth = 0;
    bool emit_definition = true;
};

static std::string segment_expr(char page, const std::string& module) {
    std::string p(1, page);
    return "((const uint8_t)&__ML_SEGMENT_" + p + "_" + module + ")";
}

static PtrKind infer_ptr_kind(ExprPtr expr,
                              const Variant& variant,
                              const std::unordered_map<std::string, GlobalInfo>& globals,
                              const AnalyzedProgram& analyzed,
                              int entry_instance_id) {
    if (!expr || !expr->type) return PtrKind::Ram;
    if (!megalinker_semantics::is_pointer_like_type(expr->type)) return PtrKind::Ram;

    switch (expr->kind) {
        case Expr::Kind::StringLiteral:
        case Expr::Kind::ArrayLiteral:
            return PtrKind::Far;
        case Expr::Kind::Identifier: {
            auto it = variant.param_kind_by_name.find(expr->name);
            if (it != variant.param_kind_by_name.end()) {
                return it->second;
            }
            Symbol* sym = analyzed.binding_for ? analyzed.binding_for(variant.instance_id, expr.get()) : nullptr;
            if (!sym) return PtrKind::Ram;
            std::string key = symbol_key_for(sym, entry_instance_id);
            auto git = globals.find(key);
            if (git != globals.end() && git->second.is_rom) {
                return PtrKind::Far;
            }
            return PtrKind::Ram;
        }
        case Expr::Kind::Call:
            return PtrKind::Far;
        case Expr::Kind::Conditional: {
            PtrKind a = infer_ptr_kind(expr->true_expr, variant, globals, analyzed, entry_instance_id);
            PtrKind b = infer_ptr_kind(expr->false_expr, variant, globals, analyzed, entry_instance_id);
            return (a == PtrKind::Far || b == PtrKind::Far) ? PtrKind::Far : PtrKind::Ram;
        }
        case Expr::Kind::Block:
            return infer_ptr_kind(expr->result_expr, variant, globals, analyzed, entry_instance_id);
        case Expr::Kind::Cast:
            return infer_ptr_kind(expr->operand, variant, globals, analyzed, entry_instance_id);
        case Expr::Kind::Member:
            if (expr->operand && expr->operand->kind == Expr::Kind::Identifier) {
                Symbol* sym = analyzed.binding_for ? analyzed.binding_for(variant.instance_id, expr->operand.get()) : nullptr;
                if (!sym) return PtrKind::Ram;
                std::string key = symbol_key_for(sym, entry_instance_id);
                auto git = globals.find(key);
                if (git != globals.end() && git->second.is_rom) {
                    return PtrKind::Far;
                }
            }
            return PtrKind::Ram;
        default:
            return PtrKind::Ram;
    }
}

static std::vector<char> available_reent_keys(const AnalysisFacts& facts, const Symbol* sym) {
    std::vector<char> keys;
    if (sym) {
        auto it = facts.reentrancy_variants.find(sym);
        if (it != facts.reentrancy_variants.end()) {
            keys.assign(it->second.begin(), it->second.end());
        }
    }
    if (keys.empty()) keys.push_back('N');
    std::sort(keys.begin(), keys.end());
    return keys;
}

static std::string choose_ref_key(const AnalysisFacts& facts, const Symbol* sym,
                                  const StmtPtr& decl, const std::string& desired) {
    if (!decl) return desired;
    auto it = sym ? facts.ref_variants.find(sym) : facts.ref_variants.end();
    if (it == facts.ref_variants.end() || it->second.empty()) {
        return desired.empty() ? std::string(decl->ref_params.size(), 'M') : desired;
    }
    if (it->second.count(desired)) return desired;
    return std::string(decl->ref_params.size(), 'M');
}

static char choose_reent_key(const AnalysisFacts& facts, const Symbol* sym, char desired) {
    auto it = sym ? facts.reentrancy_variants.find(sym) : facts.reentrancy_variants.end();
    if (it == facts.reentrancy_variants.end() || it->second.empty()) {
        return desired;
    }
    if (it->second.count(desired)) return desired;
    std::vector<char> keys(it->second.begin(), it->second.end());
    std::sort(keys.begin(), keys.end());
    return keys.front();
}

static std::string param_sig(const StmtPtr& decl, const std::vector<PtrKind>& kinds) {
    if (!decl) return "";
    std::string sig;
    size_t kidx = 0;
    for (const auto& param : decl->params) {
        if (param.is_expression_param) continue;
        if (megalinker_semantics::is_pointer_like_type(param.type)) {
            PtrKind k = (kidx < kinds.size()) ? kinds[kidx] : PtrKind::Ram;
            sig.push_back(k == PtrKind::Far ? 'F' : 'R');
        } else {
            sig.push_back('-');
        }
        kidx++;
    }
    return sig;
}

static std::string signature_key(const std::string& func_key,
                                 char reent_key,
                                 const std::string& ref_key,
                                 const std::string& pk) {
    return func_key + "|r" + std::string(1, reent_key) + "|ref" + ref_key + "|pk" + pk;
}

static std::string trampoline_name(const StmtPtr& decl,
                                   char reent_key,
                                   const std::string& ref_key,
                                   const std::string& pk) {
    std::string name = qualified_name(decl) + "__tramp";
    name += (reent_key == 'R') ? "__reent" : "__nonreent";
    if (!ref_key.empty()) {
        bool all_mut = std::all_of(ref_key.begin(), ref_key.end(), [](char c) { return c == 'M'; });
        if (!all_mut) name += "__ref" + ref_key;
    }
    if (!pk.empty()) name += "__pk" + pk;
    return name;
}

static bool parse_caller_limit_option(const std::unordered_map<std::string, std::string>& opts,
                                      size_t& out) {
    auto it = opts.find("caller_limit");
    if (it == opts.end()) return false;
    char* end = nullptr;
    long value = std::strtol(it->second.c_str(), &end, 10);
    if (!end || *end != '\0' || value <= 0) {
        throw CompileError("Megalinker backend: caller_limit must be a positive integer", SourceLocation());
    }
    out = static_cast<size_t>(value);
    return true;
}

struct InlineConfig {
    bool inline_default = true;
    size_t max_cost = 200;
    size_t max_depth = 8;
    size_t max_expansions = 64;
};

static bool parse_boolean_flag(const std::string& value, bool& out) {
    if (value == "1" || value == "true" || value == "on" || value == "yes") {
        out = true;
        return true;
    }
    if (value == "0" || value == "false" || value == "off" || value == "no") {
        out = false;
        return true;
    }
    return false;
}

static bool parse_positive_size_t(const std::string& value, size_t& out) {
    if (value.empty()) return false;
    char* end = nullptr;
    long parsed = std::strtol(value.c_str(), &end, 10);
    if (!end || *end != '\0' || parsed <= 0) {
        return false;
    }
    out = static_cast<size_t>(parsed);
    return true;
}

static bool parse_inline_default_option(const std::unordered_map<std::string, std::string>& opts,
                                        bool& out) {
    auto it = opts.find("inline_default");
    if (it == opts.end()) return false;
    if (!parse_boolean_flag(it->second, out)) {
        throw CompileError("Megalinker backend: inline_default must be on/off (or true/false)",
                           SourceLocation());
    }
    return true;
}

static bool parse_inline_size_option(const std::unordered_map<std::string, std::string>& opts,
                                     const std::string& key,
                                     size_t& out,
                                     const std::string& error_label) {
    auto it = opts.find(key);
    if (it == opts.end()) return false;
    if (!parse_positive_size_t(it->second, out)) {
        throw CompileError("Megalinker backend: " + error_label + " must be a positive integer",
                           SourceLocation());
    }
    return true;
}

static InlineConfig load_inline_config(const std::unordered_map<std::string, std::string>& opts) {
    InlineConfig cfg;
    (void)parse_inline_default_option(opts, cfg.inline_default);
    (void)parse_inline_size_option(opts, "inline_max_cost", cfg.max_cost, "inline_max_cost");
    (void)parse_inline_size_option(opts, "inline_max_depth", cfg.max_depth, "inline_max_depth");
    (void)parse_inline_size_option(opts, "inline_max_expansions", cfg.max_expansions, "inline_max_expansions");
    return cfg;
}

static bool is_valid_symbol_prefix(const std::string& value) {
    if (value.empty()) return false;
    unsigned char first = static_cast<unsigned char>(value.front());
    if (!(std::isalpha(first) || value.front() == '_')) return false;
    for (size_t i = 1; i < value.size(); ++i) {
        unsigned char c = static_cast<unsigned char>(value[i]);
        if (!(std::isalnum(c) || value[i] == '_')) {
            return false;
        }
    }
    return true;
}

static bool parse_internal_prefix_option(const std::unordered_map<std::string, std::string>& opts,
                                         std::string& out) {
    auto it = opts.find("internal_prefix");
    if (it == opts.end()) return false;
    if (!is_valid_symbol_prefix(it->second)) {
        throw CompileError("Megalinker backend: internal_prefix must be a valid C identifier prefix",
                           SourceLocation());
    }
    out = it->second;
    return true;
}

static bool parse_positive_int_option(const std::string& value, std::string& error) {
    size_t ignored = 0;
    if (!parse_positive_size_t(value, ignored)) {
        error = "Megalinker backend: --caller-limit requires a positive integer";
        return false;
    }
    return true;
}

static bool parse_inline_default_arg(const std::string& value, std::string& error) {
    bool ignored = false;
    if (!parse_boolean_flag(value, ignored)) {
        error = "Megalinker backend: --inline-default requires on/off (or true/false)";
        return false;
    }
    return true;
}

static bool parse_inline_positive_arg(const std::string& value,
                                      const std::string& flag,
                                      std::string& error) {
    size_t ignored = 0;
    if (!parse_positive_size_t(value, ignored)) {
        error = "Megalinker backend: " + flag + " requires a positive integer";
        return false;
    }
    return true;
}

static bool parse_internal_prefix_arg(const std::string& value, std::string& error) {
    if (!is_valid_symbol_prefix(value)) {
        error = "Megalinker backend: --internal-prefix requires a valid C identifier prefix";
        return false;
    }
    return true;
}

static void validate_megalinker_backend_options(const Compiler::Options& options, std::string& error) {
    for (const auto& entry : options.backend_options) {
        if (entry.first != "caller_limit" &&
            entry.first != "internal_prefix" &&
            entry.first != "inline_default" &&
            entry.first != "inline_max_cost" &&
            entry.first != "inline_max_depth" &&
            entry.first != "inline_max_expansions") {
            error = "Megalinker backend: unknown backend option key '" + entry.first + "'";
            return;
        }
    }

    auto caller_it = options.backend_options.find("caller_limit");
    if (caller_it != options.backend_options.end()) {
        if (!parse_positive_int_option(caller_it->second, error)) {
            return;
        }
    }

    auto prefix_it = options.backend_options.find("internal_prefix");
    if (prefix_it != options.backend_options.end()) {
        if (!parse_internal_prefix_arg(prefix_it->second, error)) {
            return;
        }
    }

    auto inline_default_it = options.backend_options.find("inline_default");
    if (inline_default_it != options.backend_options.end()) {
        if (!parse_inline_default_arg(inline_default_it->second, error)) {
            return;
        }
    }

    auto inline_cost_it = options.backend_options.find("inline_max_cost");
    if (inline_cost_it != options.backend_options.end()) {
        if (!parse_inline_positive_arg(inline_cost_it->second, "--inline-max-cost", error)) {
            return;
        }
    }

    auto inline_depth_it = options.backend_options.find("inline_max_depth");
    if (inline_depth_it != options.backend_options.end()) {
        if (!parse_inline_positive_arg(inline_depth_it->second, "--inline-max-depth", error)) {
            return;
        }
    }

    auto inline_exp_it = options.backend_options.find("inline_max_expansions");
    if (inline_exp_it != options.backend_options.end()) {
        if (!parse_inline_positive_arg(inline_exp_it->second, "--inline-max-expansions", error)) {
            return;
        }
    }
}

static bool parse_megalinker_option(int argc,
                                    char** argv,
                                    int& index,
                                    Compiler::Options& options,
                                    std::string& error) {
    const char* arg = argv[index];
    if (std::strcmp(arg, "--caller-limit") == 0) {
        if (index + 1 >= argc) {
            error = "Megalinker backend: --caller-limit requires an argument";
            return true;
        }
        std::string value = argv[index + 1];
        if (!parse_positive_int_option(value, error)) {
            return true;
        }
        options.backend_options["caller_limit"] = value;
        index++;
        return true;
    }
    constexpr const char* kPrefix = "--caller-limit=";
    if (std::strncmp(arg, kPrefix, std::strlen(kPrefix)) == 0) {
        std::string value = arg + std::strlen(kPrefix);
        if (!parse_positive_int_option(value, error)) {
            return true;
        }
        options.backend_options["caller_limit"] = value;
        return true;
    }
    if (std::strcmp(arg, "--internal-prefix") == 0) {
        if (index + 1 >= argc) {
            error = "Megalinker backend: --internal-prefix requires an argument";
            return true;
        }
        std::string value = argv[index + 1];
        if (!parse_internal_prefix_arg(value, error)) {
            return true;
        }
        options.backend_options["internal_prefix"] = value;
        index++;
        return true;
    }
    constexpr const char* kInternalPrefix = "--internal-prefix=";
    if (std::strncmp(arg, kInternalPrefix, std::strlen(kInternalPrefix)) == 0) {
        std::string value = arg + std::strlen(kInternalPrefix);
        if (!parse_internal_prefix_arg(value, error)) {
            return true;
        }
        options.backend_options["internal_prefix"] = value;
        return true;
    }
    if (std::strcmp(arg, "--inline-default") == 0) {
        if (index + 1 >= argc) {
            error = "Megalinker backend: --inline-default requires an argument";
            return true;
        }
        std::string value = argv[index + 1];
        if (!parse_inline_default_arg(value, error)) {
            return true;
        }
        options.backend_options["inline_default"] = value;
        index++;
        return true;
    }
    constexpr const char* kInlineDefaultPrefix = "--inline-default=";
    if (std::strncmp(arg, kInlineDefaultPrefix, std::strlen(kInlineDefaultPrefix)) == 0) {
        std::string value = arg + std::strlen(kInlineDefaultPrefix);
        if (!parse_inline_default_arg(value, error)) {
            return true;
        }
        options.backend_options["inline_default"] = value;
        return true;
    }
    if (std::strcmp(arg, "--inline-max-cost") == 0) {
        if (index + 1 >= argc) {
            error = "Megalinker backend: --inline-max-cost requires an argument";
            return true;
        }
        std::string value = argv[index + 1];
        if (!parse_inline_positive_arg(value, "--inline-max-cost", error)) {
            return true;
        }
        options.backend_options["inline_max_cost"] = value;
        index++;
        return true;
    }
    constexpr const char* kInlineMaxCostPrefix = "--inline-max-cost=";
    if (std::strncmp(arg, kInlineMaxCostPrefix, std::strlen(kInlineMaxCostPrefix)) == 0) {
        std::string value = arg + std::strlen(kInlineMaxCostPrefix);
        if (!parse_inline_positive_arg(value, "--inline-max-cost", error)) {
            return true;
        }
        options.backend_options["inline_max_cost"] = value;
        return true;
    }
    if (std::strcmp(arg, "--inline-max-depth") == 0) {
        if (index + 1 >= argc) {
            error = "Megalinker backend: --inline-max-depth requires an argument";
            return true;
        }
        std::string value = argv[index + 1];
        if (!parse_inline_positive_arg(value, "--inline-max-depth", error)) {
            return true;
        }
        options.backend_options["inline_max_depth"] = value;
        index++;
        return true;
    }
    constexpr const char* kInlineMaxDepthPrefix = "--inline-max-depth=";
    if (std::strncmp(arg, kInlineMaxDepthPrefix, std::strlen(kInlineMaxDepthPrefix)) == 0) {
        std::string value = arg + std::strlen(kInlineMaxDepthPrefix);
        if (!parse_inline_positive_arg(value, "--inline-max-depth", error)) {
            return true;
        }
        options.backend_options["inline_max_depth"] = value;
        return true;
    }
    if (std::strcmp(arg, "--inline-max-expansions") == 0) {
        if (index + 1 >= argc) {
            error = "Megalinker backend: --inline-max-expansions requires an argument";
            return true;
        }
        std::string value = argv[index + 1];
        if (!parse_inline_positive_arg(value, "--inline-max-expansions", error)) {
            return true;
        }
        options.backend_options["inline_max_expansions"] = value;
        index++;
        return true;
    }
    constexpr const char* kInlineMaxExpPrefix = "--inline-max-expansions=";
    if (std::strncmp(arg, kInlineMaxExpPrefix, std::strlen(kInlineMaxExpPrefix)) == 0) {
        std::string value = arg + std::strlen(kInlineMaxExpPrefix);
        if (!parse_inline_positive_arg(value, "--inline-max-expansions", error)) {
            return true;
        }
        options.backend_options["inline_max_expansions"] = value;
        return true;
    }
    return false;
}

static void print_megalinker_usage(std::ostream& os) {
    os << "  --caller-limit <n>  Caller-variant limit before nonbanked trampoline fallback (default: 10)\n";
    os << "  --internal-prefix <id>  Prefix for internal generated symbols (default: vx_)\n";
    os << "  --inline-default <on|off>  Hard-inline internal calls by default (default: on)\n";
    os << "  --inline-max-cost <n>  Max callee AST cost for hard inlining (default: 200)\n";
    os << "  --inline-max-depth <n>  Max nested hard-inline depth (default: 8)\n";
    os << "  --inline-max-expansions <n>  Max hard-inline expansions per function (default: 64)\n";
    os << "  Notes: inline-first for internal calls; inlining is skipped for [[noinline]],\n";
    os << "         recursive callees, explicit returns, and inline-limit violations.\n";
    os << "         [[sdcccall(0|1)]] is valid only on ABI-visible functions (&^, &!).\n";
}

} // namespace

static void emit_megalinker_backend(const BackendInput& input) {
    const AnalyzedProgram& analyzed = input.program;
    if (!analyzed.module || !analyzed.analysis || !analyzed.optimization) {
        throw CompileError("Megalinker backend requires full analyzed program input",
                           SourceLocation());
    }
    std::string internal_prefix = "vx_";
    (void)parse_internal_prefix_option(input.options.backend_options, internal_prefix);
    std::string load_module_a_fn = internal_prefix + "load_module_id_a";
    std::string load_module_b_fn = internal_prefix + "load_module_id_b";
    std::string strlen_far_a_fn = internal_prefix + "strlen_far_a";
    std::string strlen_far_b_fn = internal_prefix + "strlen_far_b";

    const Module& module = *analyzed.module;
    const AnalysisFacts& analysis = *analyzed.analysis;

    // Backend ABI policy: external function boundaries never carry named structs.
    if (analyzed.program) {
        for (const auto& mod_info : analyzed.program->modules) {
            validate_megalinker_external_function_abi(mod_info.module);
            validate_megalinker_function_annotations(mod_info.module);
        }
    } else {
        validate_megalinker_external_function_abi(module);
        validate_megalinker_function_annotations(module);
    }

    // Build a header with types only (no function prototypes)
    AnalysisFacts header_facts = analysis;
    header_facts.reachable_functions.clear();
    AnalyzedProgram header_input = analyzed;
    header_input.analysis = &header_facts;

    CodeGenerator header_codegen;
    CodegenABI header_abi;
    header_codegen.set_abi(header_abi);
    header_codegen.set_internal_symbol_prefix(internal_prefix);
    CCodegenResult header_result = header_codegen.generate(module, header_input);

    std::filesystem::path header_path = input.outputs.dir / (input.outputs.stem + ".h");
    std::filesystem::path runtime_path = input.outputs.dir / (input.outputs.stem + "__runtime.c");
    std::filesystem::path out_dir = input.outputs.dir / "megalinker";
    std::filesystem::create_directories(out_dir);

    if (input.options.verbose) {
        std::cout << "Writing header: " << header_path << std::endl;
        std::cout << "Writing runtime: " << runtime_path << std::endl;
    }

    int entry_instance_id = analyzed.entry_instance_id;
    auto bind_symbol = [&](int instance_id, const void* node) -> Symbol* {
        if (!analyzed.binding_for || !node) return nullptr;
        return analyzed.binding_for(instance_id, node);
    };

    // Collect globals from the frontend's live symbol set.
    std::unordered_map<std::string, GlobalInfo> globals;
    for (const Symbol* sym : analysis.used_global_vars) {
        if (!sym || sym->is_local) continue;
        if (sym->kind != Symbol::Kind::Variable && sym->kind != Symbol::Kind::Constant) continue;
        StmtPtr stmt = sym->declaration;
        if (!stmt || stmt->kind != Stmt::Kind::VarDecl) continue;

        int scope_id = scope_id_for_symbol(sym, entry_instance_id);
        std::string key = symbol_key(stmt->var_name, scope_id);
        GlobalInfo info;
        info.decl = stmt;
        info.sym = sym;
        info.scope_id = scope_id;
        info.is_pointer_like = megalinker_semantics::is_pointer_like_type(stmt->var_type);
        info.c_name = header_codegen.mangle(stmt->var_name);
        if (scope_id >= 0) {
            info.c_name += "_s" + std::to_string(scope_id);
        }
        bool force_ram = has_annotation(stmt->annotations, "nonbanked");
        VarMutability mut = stmt->is_mutable ? VarMutability::Mutable : VarMutability::Constexpr;
        auto mit = analysis.var_mutability.find(sym);
        if (mit != analysis.var_mutability.end()) mut = mit->second;
        info.is_rom = !force_ram && (mut == VarMutability::Constexpr);
        if (info.is_rom) {
            info.module_name = "rom_" + info.c_name;
        }
        globals[key] = info;
    }

    struct FunctionInfo {
        const Symbol* sym = nullptr;
        StmtPtr decl;
        int instance_id = -1;
        int scope_id = -1;
    };

    // Collect functions from the frontend's reachable set.
    std::unordered_map<std::string, FunctionInfo> function_map;
    for (const Symbol* sym : analysis.reachable_functions) {
        if (!sym || sym->kind != Symbol::Kind::Function || sym->is_external) continue;
        StmtPtr stmt = sym->declaration;
        if (!stmt || stmt->kind != Stmt::Kind::FuncDecl || stmt->is_external) continue;

        bool has_expr_params = false;
        for (const auto& p : stmt->params) {
            if (p.is_expression_param) {
                has_expr_params = true;
                break;
            }
        }
        if (has_expr_params) continue;

        int scope_id = scope_id_for_symbol(sym, entry_instance_id);
        std::string key = func_key_for(sym, entry_instance_id);
        FunctionInfo info;
        info.sym = sym;
        info.decl = stmt;
        info.instance_id = sym->instance_id;
        info.scope_id = scope_id;
        function_map[key] = info;
    }

    InlineConfig inline_cfg = load_inline_config(input.options.backend_options);

    std::unordered_map<std::string, std::unordered_set<std::string>> function_call_edges;
    std::unordered_map<std::string, size_t> inline_cost_by_key;
    std::unordered_map<std::string, bool> explicit_return_by_key;
    for (const auto& kv : function_map) {
        const std::string& key = kv.first;
        const FunctionInfo& finfo = kv.second;
        inline_cost_by_key[key] = expr_inline_cost(finfo.decl ? finfo.decl->body : nullptr);
        explicit_return_by_key[key] = expr_has_explicit_return(finfo.decl ? finfo.decl->body : nullptr);

        std::unordered_set<std::string> callees;
        std::vector<ExprPtr> calls;
        collect_call_exprs(finfo.decl ? finfo.decl->body : nullptr, calls);
        for (const auto& call : calls) {
            if (!call || !call->operand || call->operand->kind != Expr::Kind::Identifier) continue;
            Symbol* callee_sym = bind_symbol(finfo.instance_id, call->operand.get());
            if (!callee_sym || callee_sym->kind != Symbol::Kind::Function ||
                callee_sym->is_external || !callee_sym->declaration) {
                continue;
            }
            std::string callee_key = func_key_for(callee_sym, entry_instance_id);
            if (function_map.count(callee_key)) {
                callees.insert(callee_key);
            }
        }
        function_call_edges[key] = std::move(callees);
    }
    std::unordered_set<std::string> recursive_function_keys =
        compute_recursive_function_keys(function_call_edges);

    struct VariantBuildInfo {
        std::unordered_map<std::string, Variant> variants;
        std::unordered_map<std::string, std::unordered_map<const Expr*, std::string>> call_targets;
        std::unordered_map<std::string, std::unordered_map<const Expr*, CallTargetInfo>> call_overrides;
        std::unordered_map<std::string, int> variant_counts;
        std::unordered_map<std::string, std::unordered_set<std::string>> signature_callers;
        std::unordered_set<std::string> trampoline_signatures;
        std::unordered_map<std::string, std::string> trampoline_variants;
        std::unordered_map<std::string, std::string> trampoline_names;
        std::vector<std::string> order;
    } build;

    size_t caller_limit = 10;
    (void)parse_caller_limit_option(input.options.backend_options, caller_limit);
    std::unordered_map<std::string, size_t> inline_expansion_count;
    std::unordered_set<std::string> inline_skip_warnings_emitted;

    std::queue<std::string> pending;

    auto add_variant = [&](const std::string& func_key,
                           const FunctionInfo& info,
                           const std::string& caller_id,
                           char reent_key,
                           const std::string& ref_key,
                           const std::vector<PtrKind>& param_kinds,
                           char page,
                           bool emit_definition,
                           size_t inline_depth) -> std::string {
        const StmtPtr& decl = info.decl;
        std::string pk = param_sig(decl, param_kinds);
        std::string sig_key = signature_key(func_key, reent_key, ref_key, pk);
        std::string caller_tag = caller_id;
        if (caller_tag.empty()) {
            caller_tag = "entry";
        } else {
            auto it = build.variants.find(caller_id);
            if (it != build.variants.end()) {
                caller_tag = it->second.name;
            }
        }
        caller_tag = sanitize_tag(caller_tag);

        std::string id = func_key + "|r" + std::string(1, reent_key) + "|ref" + ref_key + "|from" + caller_id +
                         "|pk" + pk + "|p" + std::string(1, page);
        auto it = build.variants.find(id);
        if (it != build.variants.end()) {
            it->second.emit_definition = it->second.emit_definition || emit_definition;
            it->second.inline_depth = std::min(it->second.inline_depth, inline_depth);
            return id;
        }

        int& count = build.variant_counts[func_key];
        count++;
        if (count > 100) {
            throw CompileError("Megalinker backend: exceeded 100 variants for function '" + qualified_name(decl) + "'",
                               decl ? decl->location : SourceLocation());
        }

        Variant v;
        v.id = id;
        v.func_key = func_key;
        v.signature_key = sig_key;
        v.caller_id = caller_id;
        v.decl = decl;
        v.sym = info.sym;
        v.instance_id = info.instance_id;
        v.scope_id = info.scope_id;
        v.ref_key = ref_key;
        v.reent_key = reent_key;
        v.param_kinds = param_kinds;
        v.page = page;
        v.emit_definition = emit_definition;
        v.inline_depth = inline_depth;

        std::string name = qualified_name(decl);
        name += (reent_key == 'R') ? "__reent" : "__nonreent";
        if (!ref_key.empty()) {
            bool all_mut = std::all_of(ref_key.begin(), ref_key.end(), [](char c) { return c == 'M'; });
            if (!all_mut) name += "__ref" + ref_key;
        }
        name += "__from_" + caller_tag;
        if (!pk.empty()) name += "__pk" + pk;
        name += "__p";
        name.push_back(page);

        v.name = name;
        std::string suffix = (info.scope_id >= 0) ? "_s" + std::to_string(info.scope_id) : "";
        v.c_name = header_codegen.mangle(name) + suffix;
        v.module_name = v.c_name;
        if (!emit_definition && !caller_id.empty()) {
            auto caller_it = build.variants.find(caller_id);
            if (caller_it != build.variants.end()) {
                v.module_name = caller_it->second.module_name;
            }
        }

        size_t param_idx = 0;
        for (const auto& param : decl->params) {
            if (param.is_expression_param) continue;
            if (param_idx < param_kinds.size() &&
                megalinker_semantics::is_pointer_like_type(param.type)) {
                v.param_kind_by_name[param.name] = param_kinds[param_idx];
            }
            param_idx++;
        }

        build.variants[id] = v;
        build.order.push_back(id);
        pending.push(id);
        return id;
    };

    // Seed variants from exported functions
    for (const auto& kv : function_map) {
        const std::string& func_key = kv.first;
        const FunctionInfo& info = kv.second;
        StmtPtr decl = info.decl;
        const Symbol* sym = info.sym;
        if (!decl || !decl->is_exported) continue;

        std::vector<char> reent_keys = available_reent_keys(analysis, sym);
        std::string base_ref = decl->ref_params.empty() ? "" : std::string(decl->ref_params.size(), 'M');

        for (char reent_key : reent_keys) {
            std::string ref_key = choose_ref_key(analysis, sym, decl, base_ref);
            std::vector<PtrKind> param_kinds;
            for (const auto& param : decl->params) {
                if (param.is_expression_param) continue;
                if (megalinker_semantics::is_pointer_like_type(param.type)) {
                    param_kinds.push_back(PtrKind::Far);
                } else {
                    param_kinds.push_back(PtrKind::Ram);
                }
            }
            (void)add_variant(func_key, info, "", reent_key, ref_key, param_kinds, 'A', true, 0);
        }
    }

    // BFS through call graph
    while (!pending.empty()) {
        std::string vid = pending.front();
        pending.pop();
        auto vit = build.variants.find(vid);
        if (vit == build.variants.end()) continue;
        Variant& variant = vit->second;
        if (!variant.decl || !variant.decl->body) continue;

        std::vector<ExprPtr> calls;
        collect_call_exprs(variant.decl->body, calls);
        for (const auto& call : calls) {
            if (!call || call->kind != Expr::Kind::Call) continue;
            if (!call->operand || call->operand->kind != Expr::Kind::Identifier) continue;

            Symbol* sym = bind_symbol(variant.instance_id, call->operand.get());
            if (!sym || sym->kind != Symbol::Kind::Function || !sym->declaration) continue;
            if (sym->declaration->is_external) continue;

            std::string callee_key = func_key_for(sym, entry_instance_id);

            auto f_it = function_map.find(callee_key);
            if (f_it == function_map.end()) continue;

            char desired_reent = choose_reent_key(analysis, sym, variant.reent_key);
            std::string desired_ref = megalinker_semantics::ref_variant_key_for_call(
                call, sym->declaration->ref_params.size());
            desired_ref = choose_ref_key(analysis, sym, sym->declaration, desired_ref);

            std::vector<PtrKind> param_kinds;
            size_t arg_idx = 0;
            for (size_t i = 0; i < sym->declaration->params.size(); ++i) {
                const auto& param = sym->declaration->params[i];
                if (param.is_expression_param) {
                    arg_idx++;
                    continue;
                }
                ExprPtr arg_expr = (arg_idx < call->args.size()) ? call->args[arg_idx] : nullptr;
                if (megalinker_semantics::is_pointer_like_type(param.type)) {
                    param_kinds.push_back(infer_ptr_kind(arg_expr, variant, globals, analyzed, entry_instance_id));
                } else {
                    param_kinds.push_back(PtrKind::Ram);
                }
                arg_idx++;
            }

            bool hard_inline = inline_cfg.inline_default;
            std::string inline_skip_reason;
            if (hard_inline) {
                if (has_annotation(sym->declaration->annotations, "noinline")) {
                    hard_inline = false;
                    inline_skip_reason = "annotated [[noinline]]";
                } else if (recursive_function_keys.count(callee_key)) {
                    hard_inline = false;
                    inline_skip_reason = "recursive function";
                } else {
                    auto ret_it = explicit_return_by_key.find(callee_key);
                    if (ret_it != explicit_return_by_key.end() && ret_it->second) {
                        hard_inline = false;
                        inline_skip_reason = "explicit return statements";
                    }
                }
                if (hard_inline) {
                    auto cost_it = inline_cost_by_key.find(callee_key);
                    size_t cost = (cost_it != inline_cost_by_key.end()) ? cost_it->second : 0;
                    if (cost > inline_cfg.max_cost) {
                        hard_inline = false;
                        inline_skip_reason = "cost " + std::to_string(cost) + " > " +
                                             std::to_string(inline_cfg.max_cost);
                    }
                }
                if (hard_inline && (variant.inline_depth + 1 > inline_cfg.max_depth)) {
                    hard_inline = false;
                    inline_skip_reason = "depth " + std::to_string(variant.inline_depth + 1) + " > " +
                                         std::to_string(inline_cfg.max_depth);
                }
                if (hard_inline && inline_expansion_count[callee_key] >= inline_cfg.max_expansions) {
                    hard_inline = false;
                    inline_skip_reason = "expansions " +
                                         std::to_string(inline_expansion_count[callee_key]) + " >= " +
                                         std::to_string(inline_cfg.max_expansions);
                }
            }

            if (!hard_inline && inline_cfg.inline_default &&
                !inline_skip_reason.empty() && inline_skip_reason != "annotated [[noinline]]") {
                std::string warn_key = callee_key + "|" + inline_skip_reason;
                if (inline_skip_warnings_emitted.insert(warn_key).second) {
                    if (input.options.verbose) {
                        std::cout << "Megalinker: not inlining "
                                  << qualified_name(sym->declaration) << " (" << inline_skip_reason << ")\n";
                    } else {
                        std::cerr << "Megalinker: info: not inlining "
                                  << qualified_name(sym->declaration) << " (" << inline_skip_reason << ")\n";
                    }
                }
            }

            if (hard_inline) {
                std::string callee_id = add_variant(callee_key,
                                                    f_it->second,
                                                    variant.id,
                                                    desired_reent,
                                                    desired_ref,
                                                    param_kinds,
                                                    variant.page,
                                                    false,
                                                    variant.inline_depth + 1);
                CallTargetInfo info;
                info.inline_body = true;
                info.inline_variant_id = callee_id;
                info.page = variant.page;
                build.call_overrides[variant.id][call.get()] = info;
                inline_expansion_count[callee_key]++;
                continue;
            }

            std::string pk = param_sig(sym->declaration, param_kinds);
            std::string sig_key = signature_key(callee_key, desired_reent, desired_ref, pk);

            bool use_trampoline = build.trampoline_signatures.count(sig_key) > 0;
            if (!use_trampoline) {
                auto& callers = build.signature_callers[sig_key];
                callers.insert(variant.id);
                if (callers.size() > caller_limit) {
                    use_trampoline = true;
                    build.trampoline_signatures.insert(sig_key);
                    if (input.options.verbose) {
                        std::cout << "Megalinker: using nonbanked trampoline for "
                                  << qualified_name(sym->declaration) << " (callers="
                                  << callers.size() << ", limit=" << caller_limit << ")\n";
                    } else {
                        std::cerr << "Megalinker: info: using nonbanked trampoline for "
                                  << qualified_name(sym->declaration) << " (callers="
                                  << callers.size() << ", limit=" << caller_limit << ")\n";
                    }
                }
            }

            if (use_trampoline) {
                auto it = build.trampoline_variants.find(sig_key);
                if (it == build.trampoline_variants.end()) {
                    std::string tramp_name = trampoline_name(sym->declaration, desired_reent, desired_ref, pk);
                    build.trampoline_names[sig_key] = header_codegen.mangle(tramp_name);
                    std::string tramp_id = add_variant(callee_key, f_it->second, "", desired_reent,
                                                       desired_ref, param_kinds, 'A', true, 0);
                    build.trampoline_variants[sig_key] = tramp_id;
                }
                CallTargetInfo info;
                info.name = build.trampoline_names[sig_key];
                info.name_is_mangled = true;
                info.module_id_expr.clear();
                info.page = variant.page;
                build.call_overrides[variant.id][call.get()] = info;
                continue;
            }

            char callee_page = (variant.page == 'A') ? 'B' : 'A';
            std::string callee_id = add_variant(callee_key, f_it->second, variant.id,
                                                desired_reent, desired_ref, param_kinds, callee_page, true, 0);
            build.call_targets[variant.id][call.get()] = callee_id;
        }
    }

    // Determine restore needs per variant
    for (const auto& id : build.order) {
        Variant& variant = build.variants[id];
        bool alters = false;
        auto ct = build.call_targets.find(id);
        if (ct != build.call_targets.end() && !ct->second.empty()) {
            alters = true;
        }
        auto co = build.call_overrides.find(id);
        if (!alters && co != build.call_overrides.end() && !co->second.empty()) {
            alters = true;
        }
        if (!alters && variant.decl && variant.decl->body) {
            alters = expr_uses_rom_symbol(variant.decl->body, globals, analyzed,
                                          variant.instance_id, entry_instance_id);
        }
        variant.alters_caller_page = alters;
        if (!variant.caller_id.empty()) {
            if (build.variants.find(variant.caller_id) != build.variants.end()) {
                // Conservative restore: each callee variant is already caller-specialized,
                // so restoring the caller page is always correct and avoids under-modeling
                // page changes from backend-specific lowering details.
                variant.needs_restore = true;
            }
        }
    }

    // Emit variants
    std::string header_include = "#include \"" + header_path.filename().string() + "\"\n";
    std::ostringstream header_builder;
    header_builder << header_result.header;

    // Extern declarations for globals
    for (const auto& kv : globals) {
        const GlobalInfo& info = kv.second;
        StmtPtr decl = info.decl;
        if (!decl) continue;
        std::string name = header_codegen.mangle(decl->var_name);
        if (info.scope_id >= 0) name += "_s" + std::to_string(info.scope_id);
        std::string mut = megalinker_semantics::mutability_prefix(analysis, info.sym, decl);
        if (decl->var_type && decl->var_type->kind == Type::Kind::Array) {
            int size_instance_id = info.sym ? info.sym->instance_id : entry_instance_id;
            TypePtr base = decl->var_type;
            while (base && base->kind == Type::Kind::Array) {
                base = base->element_type;
            }
            if (!base) {
                throw CompileError("Array declaration has missing base element type", decl->location);
            }
            std::string decl_type = header_codegen.type_to_c(base) + " " + name;
            for (TypePtr dim = decl->var_type; dim && dim->kind == Type::Kind::Array; dim = dim->element_type) {
                decl_type += "[" + array_size_str(analyzed, dim, size_instance_id, decl->location) + "]";
            }
            header_builder << "extern " << mut << decl_type << ";\n";
        } else {
            if (!decl->var_type) {
                throw CompileError("Internal error: global '" + decl->var_name +
                                   "' missing type during megalinker codegen", decl->location);
            }
            std::string type = header_codegen.type_to_c(decl->var_type);
            header_builder << "extern " << mut << type << " " << name << ";\n";
        }
    }

    header_builder << "\n#ifndef VX_FARPTR\n";
    header_builder << "#define VX_FARPTR(mod, addr) ((((uint32_t)(mod)) << 16) | ((uint16_t)(addr)))\n";
    header_builder << "#endif\n";
    header_builder << "#ifndef VX_FARPTR_MOD\n";
    header_builder << "#define VX_FARPTR_MOD(ptr) ((uint8_t)(((uint32_t)(ptr) >> 16) & 0xFF))\n";
    header_builder << "#endif\n";
    header_builder << "#ifndef VX_FARPTR_ADDR\n";
    header_builder << "#define VX_FARPTR_ADDR(ptr) ((uint16_t)((uint32_t)(ptr) & 0xFFFF))\n";
    header_builder << "#endif\n";
    header_builder << "void " << load_module_a_fn << "(uint8_t seg);\n";
    header_builder << "void " << load_module_b_fn << "(uint8_t seg);\n";
    header_builder << "uint16_t " << strlen_far_a_fn << "(uint32_t ptr);\n";
    header_builder << "uint16_t " << strlen_far_b_fn << "(uint32_t ptr);\n";

    // Segment declarations for all modules
    std::unordered_set<std::string> module_names;
    for (const auto& id : build.order) {
        const Variant& variant = build.variants[id];
        if (!variant.emit_definition) continue;
        module_names.insert(variant.module_name);
    }
    for (const auto& kv : globals) {
        if (kv.second.is_rom) {
            module_names.insert(kv.second.module_name);
        }
    }
    for (const auto& mod : module_names) {
        header_builder << "extern const uint8_t __ML_SEGMENT_A_" << mod << ";\n";
        header_builder << "extern const uint8_t __ML_SEGMENT_B_" << mod << ";\n";
    }

    // Generate function prototypes from variant ABI signatures.
    std::unordered_map<std::string, FunctionPrototype> variant_prototypes;
    for (const auto& id : build.order) {
        Variant& variant = build.variants[id];
        if (!variant.emit_definition) continue;
        CodegenABI abi;
        abi.lower_aggregates = true;
        abi.multi_file_globals = true;
        abi.load_module_a_fn = load_module_a_fn;
        abi.load_module_b_fn = load_module_b_fn;
        abi.strlen_far_a_fn = strlen_far_a_fn;
        abi.strlen_far_b_fn = strlen_far_b_fn;
        if (variant.needs_restore) {
            auto cit = build.variants.find(variant.caller_id);
            if (cit != build.variants.end()) {
                char restore_page = cit->second.page;
                std::string restore_fn = (restore_page == 'A') ? load_module_a_fn : load_module_b_fn;
                abi.return_prefix = restore_fn + "(" +
                                    segment_expr(restore_page, cit->second.module_name) + ");";
            }
        }
        abi.func_page = [&](const std::string&) { return variant.page; };
        abi.func_module_id_expr = [&](const std::string&, char) {
            char load_page = (variant.page == 'A') ? 'B' : 'A';
            return segment_expr(load_page, variant.module_name);
        };
        abi.func_return_ptr_kind = [&](const std::string&) { return PtrKind::Far; };
        abi.symbol_ptr_kind = [&](const std::string& name, int scope_id) {
            auto it = variant.param_kind_by_name.find(name);
            if (it != variant.param_kind_by_name.end()) return it->second;
            std::string key = symbol_key(name, scope_id);
            auto git = globals.find(key);
            if (git != globals.end() && git->second.is_rom && git->second.is_pointer_like) {
                return PtrKind::Far;
            }
            return PtrKind::Ram;
        };
        abi.expr_ptr_kind = [&](const ExprPtr& expr) {
            return infer_ptr_kind(expr, variant, globals, analyzed, entry_instance_id);
        };
        abi.symbol_module_id_expr = [&](const std::string& name, int scope_id, char current_page) {
            std::string key = symbol_key(name, scope_id);
            auto git = globals.find(key);
            if (git != globals.end() && git->second.is_rom && git->second.is_pointer_like) {
                char load_page = (current_page == 'A') ? 'B' : 'A';
                return segment_expr(load_page, git->second.module_name);
            }
            return std::string();
        };
        abi.symbol_load_expr = [&](const std::string& name, int scope_id, char current_page) {
            std::string key = symbol_key(name, scope_id);
            auto git = globals.find(key);
            if (git != globals.end() && git->second.is_rom && !git->second.is_pointer_like) {
                char load_page = (current_page == 'A') ? 'B' : 'A';
                return segment_expr(load_page, git->second.module_name);
            }
            return std::string();
        };
        abi.resolve_call = [&](const ExprPtr& call_expr,
                               const std::string&,
                               const std::string&,
                               const std::string& caller_variant_id,
                               char,
                               const std::string&) {
            CallTargetInfo info;
            auto oit = build.call_overrides.find(caller_variant_id);
            if (oit != build.call_overrides.end()) {
                auto oit2 = oit->second.find(call_expr.get());
                if (oit2 != oit->second.end()) {
                    return oit2->second;
                }
            }
            auto it = build.call_targets.find(caller_variant_id);
            if (it == build.call_targets.end()) return info;
            auto it2 = it->second.find(call_expr.get());
            if (it2 == it->second.end()) return info;
            auto vit = build.variants.find(it2->second);
            if (vit == build.variants.end()) return info;
            const Variant& callee = vit->second;
            info.name = callee.name;
            info.page = callee.page;
            info.module_id_expr = segment_expr(callee.page, callee.module_name);
            return info;
        };

        CodeGenerator gen;
        gen.set_abi(abi);
        gen.set_internal_symbol_prefix(internal_prefix);
        variant.info = gen.generate_single_function(module, variant.decl, analyzed, abi, variant.instance_id,
                                                    variant.ref_key, variant.reent_key,
                                                    variant.name, variant.id);
        variant_prototypes[id] = build_function_prototype(variant.info, variant.c_name, false);
    }

    for (const auto& id : build.order) {
        auto it = variant_prototypes.find(id);
        if (it != variant_prototypes.end()) {
            header_builder << it->second.declaration << "\n";
        }
    }

    // Trampoline prototypes
    for (const auto& kv : build.trampoline_variants) {
        const std::string& sig_key = kv.first;
        const std::string& variant_id = kv.second;
        auto pit = variant_prototypes.find(variant_id);
        if (pit == variant_prototypes.end()) continue;
        auto nit = build.trampoline_names.find(sig_key);
        if (nit == build.trampoline_names.end()) continue;
        const FunctionPrototype tramp_proto = build_function_prototype(build.variants[variant_id].info, nit->second, false);
        header_builder << "__nonbanked " << tramp_proto.declaration << "\n";
    }

    // Exported wrappers
    for (const auto& kv : function_map) {
        const FunctionInfo& info = kv.second;
        StmtPtr decl = info.decl;
        if (!decl || !decl->is_exported) continue;

        // Choose first variant for the export (page A entry)
        Variant* entry = nullptr;
        for (const auto& id : build.order) {
            Variant& v = build.variants[id];
            if (v.emit_definition && v.decl == decl && v.page == 'A') {
                entry = &v;
                break;
            }
        }
        if (!entry) continue;

        std::string wrapper_name = header_codegen.mangle_export(qualified_name(decl));
        auto pit = variant_prototypes.find(entry->id);
        if (pit != variant_prototypes.end()) {
            const FunctionPrototype wrapper_proto =
                build_function_prototype(entry->info,
                                         wrapper_name,
                                         false,
                                         sdcccall_suffix_for_function_decl(decl));
            header_builder << "__nonbanked " << wrapper_proto.declaration << "\n";
        }
    }

    write_file(header_path.string(), header_builder.str());

    // Emit function files
    for (const auto& id : build.order) {
        Variant& variant = build.variants[id];
        if (!variant.emit_definition) continue;
        std::ostringstream body;
        body << "// page " << (variant.page == 'A' ? "A" : "B") << "\n";
        body << variant.info.code << "\n";
        std::filesystem::path path = out_dir / (variant.module_name + ".c");
        write_file(path.string(), header_include + body.str());
    }

    // Emit ROM globals (one file per symbol) and RAM globals
    CodeGenerator var_codegen;
    CodegenABI var_abi;
    var_abi.multi_file_globals = true;
    var_abi.load_module_a_fn = load_module_a_fn;
    var_abi.load_module_b_fn = load_module_b_fn;
    var_abi.strlen_far_a_fn = strlen_far_a_fn;
    var_abi.strlen_far_b_fn = strlen_far_b_fn;
    var_codegen.set_abi(var_abi);
    var_codegen.set_internal_symbol_prefix(internal_prefix);
    AnalysisFacts var_facts = analysis;
    for (const auto& kv : globals) {
        const GlobalInfo& info = kv.second;
        if (info.sym) {
            var_facts.used_global_vars.insert(info.sym);
        }
    }
    AnalyzedProgram var_input = analyzed;
    var_input.analysis = &var_facts;
    (void)var_codegen.generate(module, var_input);

    std::ostringstream ram_body;
    for (const auto& info : var_codegen.variables()) {
        if (!info.declaration) continue;
        const Symbol* sym = info.symbol;
        int scope_id = scope_id_for_symbol(sym, entry_instance_id);
        std::string key = sym ? symbol_key(sym->name, scope_id)
                              : symbol_key(info.declaration->var_name, scope_id);
        auto git = globals.find(key);
        if (git != globals.end() && git->second.is_rom) {
            std::filesystem::path path = out_dir / (git->second.module_name + ".c");
            write_file(path.string(), header_include + info.code + "\n");
        } else {
            ram_body << info.code << "\n";
        }
    }
    if (!ram_body.str().empty()) {
        std::filesystem::path path = out_dir / "ram_globals.c";
        write_file(path.string(), header_include + ram_body.str());
    }

    // Runtime helpers
    std::ostringstream runtime;
    runtime << header_include;
    runtime << "#include <stdint.h>\n";
    runtime << "extern volatile uint8_t __ML_address_a;\n";
    runtime << "extern volatile uint8_t __ML_address_b;\n";
    runtime << "void " << load_module_a_fn << "(uint8_t seg) { __ML_address_a = seg; }\n";
    runtime << "void " << load_module_b_fn << "(uint8_t seg) { __ML_address_b = seg; }\n";
    runtime << "uint16_t " << strlen_far_a_fn << "(uint32_t ptr) { uint8_t seg = VX_FARPTR_MOD(ptr); "
            << load_module_a_fn
            << "(seg); const char* s = (const char*)VX_FARPTR_ADDR(ptr); uint16_t n = 0; while (s[n]) n++; return n; }\n";
    runtime << "uint16_t " << strlen_far_b_fn << "(uint32_t ptr) { uint8_t seg = VX_FARPTR_MOD(ptr); "
            << load_module_b_fn
            << "(seg); const char* s = (const char*)VX_FARPTR_ADDR(ptr); uint16_t n = 0; while (s[n]) n++; return n; }\n";

    // Trampoline wrappers
    for (const auto& kv : build.trampoline_variants) {
        const std::string& sig_key = kv.first;
        const std::string& variant_id = kv.second;
        auto pit = variant_prototypes.find(variant_id);
        if (pit == variant_prototypes.end()) continue;
        auto nit = build.trampoline_names.find(sig_key);
        if (nit == build.trampoline_names.end()) continue;

        Variant& target = build.variants[variant_id];
        FunctionPrototype tramp_proto = build_function_prototype(target.info, nit->second, false);
        std::string args = tramp_proto.arg_list;
        std::string sig = "__nonbanked " + tramp_proto.declaration;
        size_t semi = sig.rfind(';');
        if (semi != std::string::npos) sig.erase(semi, 1);
        runtime << sig << " {\n";
        runtime << "  uint8_t old_a = __ML_address_a;\n";
        runtime << "  uint8_t old_b = __ML_address_b;\n";
        runtime << "  " << (target.page == 'A' ? load_module_a_fn : load_module_b_fn) << "("
                << segment_expr(target.page, target.module_name) << ");\n";
        if (tramp_proto.returns_void) {
            runtime << "  " << target.c_name << "(" << args << ");\n";
        } else {
            runtime << "  " << tramp_proto.return_type << " result = " << target.c_name << "(" << args << ");\n";
        }
        runtime << "  __ML_address_a = old_a;\n";
        runtime << "  __ML_address_b = old_b;\n";
        if (tramp_proto.returns_void) {
            runtime << "  return;\n";
        } else {
            runtime << "  return result;\n";
        }
        runtime << "}\n";
    }

    // Exported wrappers
    for (const auto& kv : function_map) {
        const FunctionInfo& info = kv.second;
        StmtPtr decl = info.decl;
        if (!decl || !decl->is_exported) continue;
        Variant* entry = nullptr;
        for (const auto& id : build.order) {
            Variant& v = build.variants[id];
            if (v.emit_definition && v.decl == decl && v.page == 'A') {
                entry = &v;
                break;
            }
        }
        if (!entry) continue;

        std::string wrapper_name = header_codegen.mangle_export(qualified_name(decl));
        FunctionPrototype wrapper_proto =
            build_function_prototype(entry->info,
                                     wrapper_name,
                                     false,
                                     sdcccall_suffix_for_function_decl(decl));
        if (wrapper_proto.declaration.empty()) continue;

        // Build wrapper signature
        std::string sig;
        sig.reserve(wrapper_proto.declaration.size() + 32);
        sig += "__nonbanked ";
        sig += wrapper_proto.declaration;
        // Replace prototype semicolon with body
        size_t semi = sig.rfind(';');
        if (semi != std::string::npos) sig.erase(semi, 1);
        sig += " {\n";

        std::string args = wrapper_proto.arg_list;

        runtime << sig;
        runtime << "  ";
        runtime << (entry->page == 'A' ? load_module_a_fn : load_module_b_fn) << "(";
        runtime << segment_expr(entry->page, entry->module_name) << ");\n";
        if (wrapper_proto.returns_void) {
            runtime << "  " << entry->c_name << "(" << args << ");\n";
        } else {
            runtime << "  return " << entry->c_name << "(" << args << ");\n";
        }
        runtime << "}\n";
    }

    write_file(runtime_path.string(), runtime.str());
}

void register_backend_megalinker() {
    Backend backend;
    backend.info.name = "megalinker";
    backend.info.description = "Megalinker banked backend";
    backend.info.version = "v0.4.0";
    backend.emit = emit_megalinker_backend;
    backend.analysis_requirements = megalinker_analysis_requirements;
    backend.boundary_reentrancy_mode = megalinker_boundary_reentrancy_mode;
    backend.validate_options = validate_megalinker_backend_options;
    backend.parse_option = parse_megalinker_option;
    backend.print_usage = print_megalinker_usage;
    (void)register_backend(backend);
}

} // namespace vexel
