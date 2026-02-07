#include "megalinker_backend.h"
#include "backend_registry.h"
#include "codegen.h"
#include "constants.h"
#include "function_key.h"
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

static std::string strip_static(const std::string& input) {
    std::string out = input;
    for (;;) {
        size_t pos = out.find("static ");
        if (pos == std::string::npos) break;
        out.erase(pos, 7);
    }
    return out;
}

static std::string replace_identifier_token(const std::string& input,
                                            const std::string& from,
                                            const std::string& to) {
    std::string out;
    out.reserve(input.size());
    for (size_t i = 0; i < input.size();) {
        unsigned char c = static_cast<unsigned char>(input[i]);
        if (std::isalnum(c) || c == '_') {
            size_t j = i;
            while (j < input.size()) {
                unsigned char d = static_cast<unsigned char>(input[j]);
                if (!std::isalnum(d) && d != '_') break;
                j++;
            }
            std::string token = input.substr(i, j - i);
            if (token == from) {
                out += to;
            } else {
                out += token;
            }
            i = j;
        } else {
            out.push_back(input[i]);
            i++;
        }
    }
    return out;
}

static std::string build_arg_list_from_proto(const std::string& proto) {
    size_t lparen = proto.find('(');
    size_t rparen = proto.rfind(')');
    if (lparen == std::string::npos || rparen == std::string::npos || rparen <= lparen) {
        return "";
    }
    std::string params = proto.substr(lparen + 1, rparen - lparen - 1);
    auto is_space = [](unsigned char ch) { return std::isspace(ch) != 0; };
    while (!params.empty() && is_space(static_cast<unsigned char>(params.front()))) params.erase(params.begin());
    while (!params.empty() && is_space(static_cast<unsigned char>(params.back()))) params.pop_back();
    if (params.empty() || params == "void") return "";

    std::vector<std::string> names;
    size_t start = 0;
    while (start < params.size()) {
        size_t comma = params.find(',', start);
        std::string part = (comma == std::string::npos) ? params.substr(start) : params.substr(start, comma - start);
        start = (comma == std::string::npos) ? params.size() : comma + 1;

        // trim
        size_t p0 = part.find_first_not_of(" \t\r\n");
        size_t p1 = part.find_last_not_of(" \t\r\n");
        if (p0 == std::string::npos) continue;
        part = part.substr(p0, p1 - p0 + 1);

        // find last identifier token
        size_t i = part.size();
        while (i > 0 && !(std::isalnum(static_cast<unsigned char>(part[i - 1])) || part[i - 1] == '_')) i--;
        size_t end = i;
        while (i > 0 && (std::isalnum(static_cast<unsigned char>(part[i - 1])) || part[i - 1] == '_')) i--;
        if (end > i) {
            names.push_back(part.substr(i, end - i));
        }
    }

    std::ostringstream oss;
    for (size_t i = 0; i < names.size(); ++i) {
        if (i > 0) oss << ", ";
        oss << names[i];
    }
    return oss.str();
}

static std::string trim_copy(const std::string& input) {
    size_t start = input.find_first_not_of(" \t\r\n");
    if (start == std::string::npos) return "";
    size_t end = input.find_last_not_of(" \t\r\n");
    return input.substr(start, end - start + 1);
}

static bool find_identifier_token(const std::string& input,
                                  const std::string& token,
                                  size_t& out_start,
                                  size_t& out_end) {
    for (size_t i = 0; i < input.size();) {
        unsigned char c = static_cast<unsigned char>(input[i]);
        if (std::isalnum(c) || c == '_') {
            size_t j = i;
            while (j < input.size()) {
                unsigned char d = static_cast<unsigned char>(input[j]);
                if (!std::isalnum(d) && d != '_') break;
                j++;
            }
            if (input.compare(i, j - i, token) == 0) {
                out_start = i;
                out_end = j;
                return true;
            }
            i = j;
        } else {
            i++;
        }
    }
    return false;
}

static std::string extract_return_type(const std::string& proto, const std::string& func_name) {
    size_t start = 0;
    size_t end = 0;
    if (!find_identifier_token(proto, func_name, start, end)) {
        return "";
    }
    return trim_copy(proto.substr(0, start));
}

static bool is_pointer_like(TypePtr type) {
    if (!type) return false;
    if (type->kind == Type::Kind::Array) return true;
    if (type->kind == Type::Kind::Primitive && type->primitive == PrimitiveType::String) return true;
    return false;
}

static bool is_addressable_lvalue(ExprPtr expr) {
    if (!expr) return false;
    switch (expr->kind) {
        case Expr::Kind::Identifier:
            return true;
        case Expr::Kind::Member:
        case Expr::Kind::Index:
            return is_addressable_lvalue(expr->operand);
        default:
            return false;
    }
}

static bool is_mutable_lvalue(ExprPtr expr) {
    if (!expr) return false;
    switch (expr->kind) {
        case Expr::Kind::Identifier:
            return expr->is_mutable_binding;
        case Expr::Kind::Member:
        case Expr::Kind::Index:
            return is_mutable_lvalue(expr->operand);
        default:
            return false;
    }
}

static std::string ref_variant_key(ExprPtr call, size_t ref_count) {
    std::string key;
    key.reserve(ref_count);
    for (size_t i = 0; i < ref_count; ++i) {
        bool is_mut = false;
        if (call && i < call->receivers.size()) {
            is_mut = is_addressable_lvalue(call->receivers[i]) && is_mutable_lvalue(call->receivers[i]);
        }
        key.push_back(is_mut ? 'M' : 'N');
    }
    return key;
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

static std::string mutability_prefix(const AnalysisFacts& facts, const Symbol* sym, StmtPtr stmt) {
    auto it = sym ? facts.var_mutability.find(sym) : facts.var_mutability.end();
    VarMutability kind = stmt->is_mutable ? VarMutability::Mutable : VarMutability::Constexpr;
    if (it != facts.var_mutability.end()) {
        kind = it->second;
    }
    switch (kind) {
        case VarMutability::Mutable:
            return "VX_MUTABLE ";
        case VarMutability::NonMutableRuntime:
            return "VX_NON_MUTABLE ";
        case VarMutability::Constexpr:
            return "VX_CONSTEXPR ";
        default:
            return "";
    }
}

static std::string array_size_str(const AnalyzedProgram& analyzed, TypePtr type,
                                  const SourceLocation& loc) {
    if (!type || type->kind != Type::Kind::Array || !type->array_size) {
        throw CompileError("Array size must be compile-time constant", loc);
    }
    if (!analyzed.optimization) {
        throw CompileError("Array size must be compile-time constant", loc);
    }
    auto it = analyzed.optimization->constexpr_values.find(type->array_size.get());
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
    if (!is_pointer_like(expr->type)) return PtrKind::Ram;

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
        if (is_pointer_like(param.type)) {
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

static size_t caller_limit_from_env() {
    const char* env = std::getenv("VEXEL_MEGALINKER_CALLER_LIMIT");
    if (!env || !*env) return 10;
    char* end = nullptr;
    long value = std::strtol(env, &end, 10);
    if (!end || *end != '\0' || value <= 0) return 10;
    return static_cast<size_t>(value);
}

static bool parse_caller_limit_option(const std::unordered_map<std::string, std::string>& opts,
                                      size_t& out) {
    auto it = opts.find("caller_limit");
    if (it == opts.end()) {
        it = opts.find("megalinker.caller_limit");
    }
    if (it == opts.end()) return false;
    char* end = nullptr;
    long value = std::strtol(it->second.c_str(), &end, 10);
    if (!end || *end != '\0' || value <= 0) {
        throw CompileError("Megalinker backend: caller_limit must be a positive integer", SourceLocation());
    }
    out = static_cast<size_t>(value);
    return true;
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
    if (it == opts.end()) {
        it = opts.find("megalinker.internal_prefix");
    }
    if (it == opts.end()) return false;
    if (!is_valid_symbol_prefix(it->second)) {
        throw CompileError("Megalinker backend: internal_prefix must be a valid C identifier prefix",
                           SourceLocation());
    }
    out = it->second;
    return true;
}

static bool parse_positive_int_option(const std::string& value, std::string& error) {
    if (value.empty()) {
        error = "Megalinker backend: --caller-limit requires a positive integer";
        return false;
    }
    char* end = nullptr;
    long parsed = std::strtol(value.c_str(), &end, 10);
    if (!end || *end != '\0' || parsed <= 0) {
        error = "Megalinker backend: --caller-limit requires a positive integer";
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
    return false;
}

static void print_megalinker_usage(std::ostream& os) {
    os << "  --caller-limit <n>  Caller-variant limit before nonbanked trampoline fallback (default: 10)\n";
    os << "  --internal-prefix <id>  Prefix for internal generated symbols (default: vx_)\n";
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

    std::filesystem::path legacy_path = input.outputs.dir / (input.outputs.stem + ".c");
    if (std::filesystem::exists(legacy_path)) {
        std::filesystem::remove(legacy_path);
    }

    std::filesystem::path header_path = input.outputs.dir / (input.outputs.stem + ".h");
    std::filesystem::path runtime_path = input.outputs.dir / (input.outputs.stem + "__runtime.c");
    std::filesystem::path out_dir = input.outputs.dir / "megalinker";
    std::filesystem::create_directories(out_dir);

    if (input.options.verbose) {
        std::cout << "Writing header: " << header_path << std::endl;
        std::cout << "Writing runtime: " << runtime_path << std::endl;
    }

    const Program* program = analyzed.program;
    int entry_instance_id = analyzed.entry_instance_id;
    auto bind_symbol = [&](int instance_id, const void* node) -> Symbol* {
        if (!analyzed.binding_for || !node) return nullptr;
        return analyzed.binding_for(instance_id, node);
    };

    // Collect globals
    std::unordered_map<std::string, GlobalInfo> globals;
    if (program) {
        for (const auto& instance : program->instances) {
            const Module& module = program->modules[static_cast<size_t>(instance.module_id)].module;
            for (const auto& stmt : module.top_level) {
                if (!stmt || stmt->kind != Stmt::Kind::VarDecl) continue;
                Symbol* sym = bind_symbol(instance.id, stmt.get());
                if (!sym) continue;
                int scope_id = scope_id_for_symbol(sym, entry_instance_id);
                std::string key = symbol_key(stmt->var_name, scope_id);
                GlobalInfo info;
                info.decl = stmt;
                info.sym = sym;
                info.scope_id = scope_id;
                info.is_pointer_like = is_pointer_like(stmt->var_type);
                bool is_exported = has_annotation(stmt->annotations, "export");
                info.c_name = is_exported ? header_codegen.mangle_export(stmt->var_name)
                                          : header_codegen.mangle(stmt->var_name);
                if (scope_id >= 0) {
                    info.c_name += "_s" + std::to_string(scope_id);
                }
                bool force_ram = has_annotation(stmt->annotations, "nonbanked");
                VarMutability mut = stmt->is_mutable ? VarMutability::Mutable : VarMutability::Constexpr;
                auto mit = analysis.var_mutability.find(sym);
                if (mit != analysis.var_mutability.end()) mut = mit->second;
                info.is_rom = !force_ram && (mut == VarMutability::Constexpr);
                if (!analysis.used_global_vars.count(sym) && !info.is_rom) {
                    continue;
                }
                if (info.is_rom) {
                    info.module_name = "rom_" + info.c_name;
                }
                globals[key] = info;
            }
        }
    } else {
        for (const auto& stmt : module.top_level) {
            if (!stmt || stmt->kind != Stmt::Kind::VarDecl) continue;
            GlobalInfo info;
            info.decl = stmt;
            info.is_pointer_like = is_pointer_like(stmt->var_type);
            bool is_exported = has_annotation(stmt->annotations, "export");
            info.c_name = is_exported ? header_codegen.mangle_export(stmt->var_name)
                                      : header_codegen.mangle(stmt->var_name);
            bool force_ram = has_annotation(stmt->annotations, "nonbanked");
            VarMutability mut = stmt->is_mutable ? VarMutability::Mutable : VarMutability::Constexpr;
            info.is_rom = !force_ram && (mut == VarMutability::Constexpr);
            if (info.is_rom) {
                info.module_name = "rom_" + info.c_name;
            }
            globals[stmt->var_name] = info;
        }
    }

    struct FunctionInfo {
        const Symbol* sym = nullptr;
        StmtPtr decl;
        int instance_id = -1;
        int scope_id = -1;
    };

    // Collect functions
    std::unordered_map<std::string, FunctionInfo> function_map;
    if (program) {
        for (const auto& instance : program->instances) {
            const Module& module = program->modules[static_cast<size_t>(instance.module_id)].module;
            for (const auto& stmt : module.top_level) {
                if (!stmt || stmt->kind != Stmt::Kind::FuncDecl) continue;
                if (stmt->is_external) continue;
                Symbol* sym = bind_symbol(instance.id, stmt.get());
                if (!sym || sym->kind != Symbol::Kind::Function) continue;
                if (!analysis.reachable_functions.count(sym)) {
                    continue;
                }
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
                info.instance_id = instance.id;
                info.scope_id = scope_id;
                function_map[key] = info;
            }
        }
    } else {
        for (const auto& stmt : module.top_level) {
            if (!stmt || stmt->kind != Stmt::Kind::FuncDecl) continue;
            if (stmt->is_external) continue;
            bool has_expr_params = false;
            for (const auto& p : stmt->params) {
                if (p.is_expression_param) {
                    has_expr_params = true;
                    break;
                }
            }
            if (has_expr_params) continue;
            std::string key = reachability_key(qualified_name(stmt), -1);
            FunctionInfo info;
            info.decl = stmt;
            function_map[key] = info;
        }
    }

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

    size_t caller_limit = caller_limit_from_env();
    (void)parse_caller_limit_option(input.options.backend_options, caller_limit);

    std::queue<std::string> pending;

    auto add_variant = [&](const std::string& func_key,
                           const FunctionInfo& info,
                           const std::string& caller_id,
                           char reent_key,
                           const std::string& ref_key,
                           const std::vector<PtrKind>& param_kinds,
                           char page) -> std::string {
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

        size_t param_idx = 0;
        for (const auto& param : decl->params) {
            if (param.is_expression_param) continue;
            if (param_idx < param_kinds.size() && is_pointer_like(param.type)) {
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
                if (is_pointer_like(param.type)) {
                    param_kinds.push_back(PtrKind::Far);
                } else {
                    param_kinds.push_back(PtrKind::Ram);
                }
            }
            (void)add_variant(func_key, info, "", reent_key, ref_key, param_kinds, 'A');
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
            std::string desired_ref = ref_variant_key(call, sym->declaration->ref_params.size());
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
                if (is_pointer_like(param.type)) {
                    param_kinds.push_back(infer_ptr_kind(arg_expr, variant, globals, analyzed, entry_instance_id));
                } else {
                    param_kinds.push_back(PtrKind::Ram);
                }
                arg_idx++;
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
                                                       desired_ref, param_kinds, 'A');
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
                                                desired_reent, desired_ref, param_kinds, callee_page);
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
        if (!alters && variant.decl && variant.decl->body) {
            alters = expr_uses_rom_symbol(variant.decl->body, globals, analyzed,
                                          variant.instance_id, entry_instance_id);
        }
        variant.alters_caller_page = alters;
        if (!variant.caller_id.empty() && alters) {
            if (build.variants.find(variant.caller_id) != build.variants.end()) {
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
        bool is_exported = has_annotation(decl->annotations, "export");
        std::string name = is_exported ? header_codegen.mangle_export(decl->var_name)
                                       : header_codegen.mangle(decl->var_name);
        if (info.scope_id >= 0) name += "_s" + std::to_string(info.scope_id);
        std::string mut = mutability_prefix(analysis, info.sym, decl);
        if (decl->var_type && decl->var_type->kind == Type::Kind::Array) {
            std::string elem_type = header_codegen.type_to_c(decl->var_type->element_type);
            std::string size = array_size_str(analyzed, decl->var_type, decl->location);
            header_builder << "extern " << mut << elem_type << " " << name << "[" << size << "];\n";
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
        module_names.insert(build.variants[id].module_name);
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

    // Generate function prototypes from variant definitions
    std::unordered_map<std::string, std::string> variant_prototypes;
    for (const auto& id : build.order) {
        Variant& variant = build.variants[id];
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

        size_t brace = variant.info.code.find('{');
        if (brace != std::string::npos) {
            std::string proto = variant.info.code.substr(0, brace);
            while (!proto.empty() && std::isspace(static_cast<unsigned char>(proto.back()))) proto.pop_back();
            variant_prototypes[id] = proto + ";";
        }
    }

    for (const auto& id : build.order) {
        auto it = variant_prototypes.find(id);
        if (it != variant_prototypes.end()) {
            header_builder << strip_static(it->second) << "\n";
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
        std::string proto = replace_identifier_token(pit->second, build.variants[variant_id].c_name, nit->second);
        proto = strip_static(proto);
        header_builder << "__nonbanked " << proto << "\n";
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
            if (v.decl == decl && v.page == 'A') {
                entry = &v;
                break;
            }
        }
        if (!entry) continue;

        std::string wrapper_name = header_codegen.mangle_export(qualified_name(decl));
        std::string proto = variant_prototypes[entry->id];
        if (!proto.empty()) {
            std::string replaced = replace_identifier_token(proto, entry->c_name, wrapper_name);
            replaced = strip_static(replaced);
            header_builder << "__nonbanked " << replaced << "\n";
        }
    }

    write_file(header_path.string(), header_builder.str());

    // Emit function files
    for (const auto& id : build.order) {
        Variant& variant = build.variants[id];
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
        if (info.is_rom && info.sym) {
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
        std::string proto = pit->second;
        proto = replace_identifier_token(proto, target.c_name, nit->second);
        proto = strip_static(proto);

        std::string args = build_arg_list_from_proto(proto);
        std::string ret_type = extract_return_type(proto, nit->second);
        std::string ret_trim = trim_copy(ret_type);
        bool returns_void = ret_trim.empty() || ret_trim == "void";

        std::string sig = "__nonbanked " + proto;
        size_t semi = sig.rfind(';');
        if (semi != std::string::npos) sig.erase(semi, 1);
        runtime << sig << " {\n";
        runtime << "  uint8_t old_a = __ML_address_a;\n";
        runtime << "  uint8_t old_b = __ML_address_b;\n";
        runtime << "  " << (target.page == 'A' ? load_module_a_fn : load_module_b_fn) << "("
                << segment_expr(target.page, target.module_name) << ");\n";
        if (returns_void) {
            runtime << "  " << target.c_name << "(" << args << ");\n";
        } else {
            runtime << "  " << ret_type << " result = " << target.c_name << "(" << args << ");\n";
        }
        runtime << "  __ML_address_a = old_a;\n";
        runtime << "  __ML_address_b = old_b;\n";
        if (returns_void) {
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
            if (v.decl == decl && v.page == 'A') {
                entry = &v;
                break;
            }
        }
        if (!entry) continue;

        std::string wrapper_name = header_codegen.mangle_export(qualified_name(decl));
        std::string proto = variant_prototypes[entry->id];
        if (proto.empty()) continue;

        proto = replace_identifier_token(proto, entry->c_name, wrapper_name);
        proto = strip_static(proto);

        // Build wrapper signature
        std::string sig;
        sig.reserve(proto.size() + 32);
        sig += "__nonbanked ";
        sig += proto;
        // Replace prototype semicolon with body
        size_t semi = sig.rfind(';');
        if (semi != std::string::npos) sig.erase(semi, 1);
        sig += " {\n";

        // Build argument list by parsing parameters from prototype
        std::string args = build_arg_list_from_proto(proto);

        runtime << sig;
        runtime << "  ";
        runtime << (entry->page == 'A' ? load_module_a_fn : load_module_b_fn) << "(";
        runtime << segment_expr(entry->page, entry->module_name) << ");\n";
        bool returns_void = (entry->decl->return_types.empty() && !entry->decl->return_type);
        if (returns_void && entry->decl->body && entry->decl->body->type) returns_void = false;
        if (!entry->decl->return_types.empty()) returns_void = true;
        if (entry->decl->return_type && entry->decl->return_type->kind == Type::Kind::Named) returns_void = true;

        if (returns_void) {
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
    backend.info.version = "v0.3.0";
    backend.emit = emit_megalinker_backend;
    backend.parse_option = parse_megalinker_option;
    backend.print_usage = print_megalinker_usage;
    (void)register_backend(backend);
}

} // namespace vexel
