#include "megalinker_backend.h"
#include "backend_registry.h"
#include "codegen.h"
#include "constants.h"
#include "function_key.h"
#include <algorithm>
#include <filesystem>
#include <functional>
#include <fstream>
#include <iostream>
#include <queue>
#include <sstream>
#include <unordered_map>
#include <unordered_set>

namespace vexel {

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

static std::string function_key(const StmtPtr& stmt) {
    return reachability_key(qualified_name(stmt), stmt ? stmt->scope_instance_id : -1);
}

static std::string build_return_type(CodeGenerator& codegen, StmtPtr func) {
    if (!func) return "void";
    if (!func->return_types.empty()) {
        std::string tuple_name = std::string(TUPLE_TYPE_PREFIX) + std::to_string(func->return_types.size());
        for (const auto& t : func->return_types) {
            tuple_name += "_";
            if (t) {
                tuple_name += t->to_string();
            } else {
                tuple_name += "unknown";
            }
        }
        return codegen.mangle(tuple_name);
    }
    if (func->return_type) {
        return codegen.type_to_c(func->return_type);
    }
    return "void";
}

static std::string build_param_list(CodeGenerator& codegen, StmtPtr func, bool with_types) {
    if (!func) return "";
    std::ostringstream oss;
    bool first = true;

    for (size_t i = 0; i < func->ref_params.size(); ++i) {
        if (!first) oss << ", ";
        first = false;
        std::string name = codegen.mangle(func->ref_params[i]);
        if (with_types) {
            std::string ref_type = "void*";
            if (!func->type_namespace.empty() && i == 0) {
                ref_type = codegen.mangle(func->type_namespace) + "*";
            }
            oss << ref_type << " " << name;
        } else {
            oss << name;
        }
    }

    for (const auto& param : func->params) {
        if (param.is_expression_param) continue;
        if (!first) oss << ", ";
        first = false;
        std::string name = codegen.mangle(param.name);
        if (with_types) {
            std::string type = param.type ? codegen.type_to_c(param.type) : "int";
            oss << type << " " << name;
        } else {
            oss << name;
        }
    }

    if (first && with_types) {
        oss << "void";
    }
    return oss.str();
}

static std::string build_arg_list(CodeGenerator& codegen, StmtPtr func) {
    return build_param_list(codegen, func, false);
}

static void collect_calls(ExprPtr expr, std::unordered_set<std::string>& calls) {
    if (!expr) return;

    switch (expr->kind) {
        case Expr::Kind::Call:
            if (expr->operand && expr->operand->kind == Expr::Kind::Identifier) {
                calls.insert(reachability_key(expr->operand->name, expr->operand->scope_instance_id));
            }
            for (const auto& rec : expr->receivers) {
                collect_calls(rec, calls);
            }
            for (const auto& arg : expr->args) {
                collect_calls(arg, calls);
            }
            collect_calls(expr->operand, calls);
            break;

        case Expr::Kind::Binary:
            collect_calls(expr->left, calls);
            collect_calls(expr->right, calls);
            break;

        case Expr::Kind::Unary:
            collect_calls(expr->operand, calls);
            break;

        case Expr::Kind::Index:
            collect_calls(expr->left, calls);
            collect_calls(expr->right, calls);
            break;

        case Expr::Kind::Member:
            collect_calls(expr->operand, calls);
            break;

        case Expr::Kind::ArrayLiteral:
        case Expr::Kind::TupleLiteral:
            for (const auto& elem : expr->elements) {
                collect_calls(elem, calls);
            }
            break;

        case Expr::Kind::Block:
            for (const auto& st : expr->statements) {
                if (!st) continue;
                if (st->expr) collect_calls(st->expr, calls);
                if (st->return_expr) collect_calls(st->return_expr, calls);
            }
            collect_calls(expr->result_expr, calls);
            break;

        case Expr::Kind::Conditional:
            collect_calls(expr->condition, calls);
            collect_calls(expr->true_expr, calls);
            collect_calls(expr->false_expr, calls);
            break;

        case Expr::Kind::Cast:
            collect_calls(expr->operand, calls);
            break;

        case Expr::Kind::Assignment:
            collect_calls(expr->left, calls);
            collect_calls(expr->right, calls);
            break;

        case Expr::Kind::Range:
            collect_calls(expr->left, calls);
            collect_calls(expr->right, calls);
            break;

        case Expr::Kind::Length:
            collect_calls(expr->operand, calls);
            break;

        case Expr::Kind::Iteration:
        case Expr::Kind::Repeat:
            collect_calls(expr->left, calls);
            if (expr->right) collect_calls(expr->right, calls);
            break;

        default:
            break;
    }
}

static bool has_variant(const AnalysisFacts& facts, const std::string& key, char variant) {
    auto it = facts.reentrancy_variants.find(key);
    if (it == facts.reentrancy_variants.end()) {
        return variant == 'N';
    }
    return it->second.count(variant) > 0;
}

static bool info_is_reentrant_variant(const AnalysisFacts& facts,
                                      const GeneratedFunctionInfo& info,
                                      const std::string& func_key) {
    auto it = facts.reentrancy_variants.find(func_key);
    if (it == facts.reentrancy_variants.end() || it->second.size() <= 1) {
        if (it != facts.reentrancy_variants.end()) {
            return it->second.count('R') > 0 && it->second.count('N') == 0;
        }
        return false;
    }
    if (info.qualified_name.find("__reent") != std::string::npos) return true;
    if (info.qualified_name.find("__nonreent") != std::string::npos) return false;
    return false;
}

} // namespace

static void emit_megalinker_backend(const BackendContext& ctx) {
    CodeGenerator codegen;
    CCodegenResult result = codegen.generate(ctx.module, &ctx.checker, &ctx.analysis, &ctx.optimization);

    std::filesystem::path legacy_path = ctx.outputs.dir / (ctx.outputs.stem + ".c");
    if (std::filesystem::exists(legacy_path)) {
        std::filesystem::remove(legacy_path);
    }

    std::filesystem::path header_path = ctx.outputs.dir / (ctx.outputs.stem + ".h");
    std::filesystem::path runtime_path = ctx.outputs.dir / (ctx.outputs.stem + "__runtime.c");
    std::filesystem::path out_dir = ctx.outputs.dir / "megalinker";
    std::filesystem::create_directories(out_dir);

    if (ctx.options.verbose) {
        std::cout << "Writing header: " << header_path << std::endl;
        std::cout << "Writing runtime: " << runtime_path << std::endl;
    }

    std::ostringstream header_builder;
    header_builder << result.header;
    header_builder << "\nextern int __vexel_current_page;\n";
    for (const auto& info : codegen.functions()) {
        if (!info.declaration || !info.declaration->is_exported) continue;
        std::string ret = build_return_type(codegen, info.declaration);
        std::string params = build_param_list(codegen, info.declaration, true);
        header_builder << ret << " " << info.c_name << "_pageA(" << params << ");\n";
        header_builder << ret << " " << info.c_name << "_pageB(" << params << ");\n";
    }
    write_file(header_path.string(), header_builder.str());

    std::string header_include = "#include \"" + header_path.filename().string() + "\"\n";

    std::ostringstream runtime_builder;
    runtime_builder << header_include << "#include \"megalinker.h\"\n";
    runtime_builder << "int __vexel_current_page = 0;\n";

    std::unordered_map<std::string, StmtPtr> function_map;
    for (const auto& info : codegen.functions()) {
        if (!info.declaration) continue;
        std::string key = function_key(info.declaration);
        function_map.emplace(key, info.declaration);
    }

    auto ensure_fn_type = [&](TypePtr type, const SourceLocation& loc, const std::string& what) {
        if (!type) {
            return;
        }
        if (type->kind == Type::Kind::Primitive) {
            if (type->primitive == PrimitiveType::F32 || type->primitive == PrimitiveType::F64) {
                throw CompileError("Megalinker backend does not support floating-point " + what, loc);
            }
            return;
        }
        throw CompileError("Megalinker backend cannot pass '" + what + "' by value", loc);
    };

    auto ensure_non_float_var = [&](TypePtr type, const SourceLocation& loc, const std::string& name) {
        if (!type) {
            throw CompileError("Megalinker backend requires explicit type for global '" + name + "'", loc);
        }
        if (type->kind == Type::Kind::Primitive &&
            (type->primitive == PrimitiveType::F32 || type->primitive == PrimitiveType::F64)) {
            throw CompileError("Megalinker backend does not support floating-point globals: " + name, loc);
        }
    };

    for (const auto& info : codegen.functions()) {
        if (!info.declaration) continue;
        StmtPtr stmt = info.declaration;
        if (!stmt->return_types.empty()) {
            throw CompileError("Megalinker backend cannot return tuples by value", stmt->location);
        }
        ensure_fn_type(stmt->return_type, stmt->location, "return type of " + qualified_name(stmt));
        for (size_t i = 0; i < stmt->params.size(); ++i) {
            ensure_fn_type(stmt->params[i].type, stmt->location,
                           "parameter " + std::to_string(i) + " of " + qualified_name(stmt));
        }
    }

    for (const auto& var : codegen.variables()) {
        if (!var.declaration) continue;
        ensure_non_float_var(var.declaration->var_type, var.declaration->location, var.declaration->var_name);
    }

    std::unordered_map<std::string, std::vector<std::string>> adj;
    std::unordered_map<std::string, int> color;

    for (const auto& entry : function_map) {
        const std::string& key = entry.first;
        StmtPtr stmt = entry.second;
        adj[key];
        color[key] = -1;
        if (!has_variant(ctx.analysis, key, 'N')) {
            continue;
        }
        if (!stmt || !stmt->body) continue;

        std::unordered_set<std::string> calls;
        collect_calls(stmt->body, calls);

        if (calls.count(key)) {
            throw CompileError("Megalinker backend: recursion in '" + qualified_name(stmt) +
                               "' requires a reentrant-only path", stmt->location);
        }

        for (const auto& callee_key : calls) {
            if (function_map.count(callee_key) == 0) continue;
            if (!has_variant(ctx.analysis, callee_key, 'N')) continue;
            adj[key].push_back(callee_key);
            adj[callee_key].push_back(key);
        }
    }

    auto bfs_color = [&](const std::string& start) {
        std::queue<std::string> q;
        q.push(start);
        while (!q.empty()) {
            std::string cur = q.front();
            q.pop();
            int cur_color = color[cur];
            for (const auto& neigh : adj[cur]) {
                if (!has_variant(ctx.analysis, neigh, 'N')) continue;
                if (color[neigh] == -1) {
                    color[neigh] = 1 - cur_color;
                    q.push(neigh);
                } else if (color[neigh] == cur_color) {
                    auto it = function_map.find(neigh);
                    StmtPtr decl = (it != function_map.end()) ? it->second : nullptr;
                    throw CompileError("Megalinker backend: alternation conflict between '" + cur +
                                       "' and '" + neigh + "'.", decl ? decl->location : SourceLocation());
                }
            }
        }
    };

    int next_export_color = 0;
    for (const auto& entry : function_map) {
        StmtPtr stmt = entry.second;
        if (!stmt || !stmt->is_exported) continue;
        const std::string& key = entry.first;
        if (!has_variant(ctx.analysis, key, 'N')) continue;
        if (color[key] == -1) {
            color[key] = next_export_color;
            next_export_color = 1 - next_export_color;
            bfs_color(key);
        }
    }

    int next_color = next_export_color;
    for (const auto& entry : color) {
        if (entry.second != -1) continue;
        const std::string& key = entry.first;
        if (!has_variant(ctx.analysis, key, 'N')) continue;
        color[key] = next_color;
        next_color = 1 - next_color;
        bfs_color(key);
    }

    auto write_megalinker_file = [&](const std::string& stem, const std::string& body) {
        std::filesystem::path path = out_dir / stem;
        write_file(path.string(), header_include + body);
    };

    for (const auto& info : codegen.functions()) {
        if (!info.declaration) continue;
        std::string key = function_key(info.declaration);
        bool is_reent = info_is_reentrant_variant(ctx.analysis, info, key);
        int page = is_reent ? 0 : (color.count(key) ? color[key] : 0);
        if (page < 0) page = 0;
        std::string suffix = page == 0 ? "_pageA" : "_pageB";
        std::string filename = info.c_name + suffix + ".c";
        std::ostringstream body;
        body << "// page " << (page == 0 ? "A" : "B") << "\n";
        body << info.code << "\n";

        if (info.declaration->is_exported) {
            std::string ret = build_return_type(codegen, info.declaration);
            std::string params = build_param_list(codegen, info.declaration, true);
            std::string args = build_arg_list(codegen, info.declaration);
            bool returns_void = (ret == "void");
            body << ret << " " << info.c_name << "_pageA(" << params << ") {\n";
            if (!returns_void) {
                body << "  return " << info.c_name << "(" << args << ");\n";
            } else {
                body << "  " << info.c_name << "(" << args << ");\n";
            }
            body << "}\n";
            body << ret << " " << info.c_name << "_pageB(" << params << ") {\n";
            if (!returns_void) {
                body << "  return " << info.c_name << "(" << args << ");\n";
            } else {
                body << "  " << info.c_name << "(" << args << ");\n";
            }
            body << "}\n";
        }

        write_megalinker_file(filename, body.str());
    }

    std::ostringstream rom;
    std::ostringstream ram;

    for (const auto& var : codegen.variables()) {
        if (!var.declaration) continue;
        bool force_ram = has_annotation(var.declaration->annotations, "nonbanked");
        auto mut_it = ctx.analysis.var_mutability.find(var.declaration.get());
        bool is_mutable = var.declaration->is_mutable;
        if (mut_it != ctx.analysis.var_mutability.end()) {
            is_mutable = (mut_it->second == VarMutability::Mutable ||
                          mut_it->second == VarMutability::NonMutableRuntime);
        }
        if (force_ram || is_mutable) {
            ram << var.code << "\n";
        } else {
            rom << var.code << "\n";
        }
    }

    write_megalinker_file("rom_globals_pageA.c", rom.str());
    write_megalinker_file("ram_globals.c", ram.str());

    std::unordered_map<std::string, std::string> func_key_to_cname;
    for (const auto& info : codegen.functions()) {
        if (!info.declaration) continue;
        std::string key = function_key(info.declaration);
        if (!has_variant(ctx.analysis, key, 'N')) continue;
        if (info_is_reentrant_variant(ctx.analysis, info, key)) continue;
        if (!func_key_to_cname.count(key)) {
            func_key_to_cname[key] = info.c_name;
        }
    }

    std::unordered_map<std::string, int> index, lowlink;
    std::unordered_set<std::string> on_stack;
    std::vector<std::string> stack;
    int idx = 0;
    std::vector<std::vector<std::string>> sccs;

    std::function<void(const std::string&)> strongconnect = [&](const std::string& v) {
        index[v] = lowlink[v] = idx++;
        stack.push_back(v);
        on_stack.insert(v);
        for (const auto& w : adj[v]) {
            if (!has_variant(ctx.analysis, w, 'N')) continue;
            if (!index.count(w)) {
                strongconnect(w);
                lowlink[v] = std::min(lowlink[v], lowlink[w]);
            } else if (on_stack.count(w)) {
                lowlink[v] = std::min(lowlink[v], index[w]);
            }
        }
        if (lowlink[v] == index[v]) {
            std::vector<std::string> comp;
            while (!stack.empty()) {
                std::string w = stack.back();
                stack.pop_back();
                on_stack.erase(w);
                comp.push_back(w);
                if (w == v) break;
            }
            if (comp.size() > 1) sccs.push_back(comp);
        }
    };

    for (const auto& kv : adj) {
        if (!index.count(kv.first) && has_variant(ctx.analysis, kv.first, 'N')) {
            strongconnect(kv.first);
        }
    }

    for (const auto& comp : sccs) {
        std::vector<std::string> members;
        for (const auto& name : comp) {
            if (!has_variant(ctx.analysis, name, 'N')) continue;
            if (func_key_to_cname.count(name)) {
                members.push_back(name);
            }
        }
        if (members.size() <= 1) continue;
        const std::string& anchor = members.front();
        std::string anchor_c = func_key_to_cname[anchor];
        for (size_t i = 1; i < members.size(); ++i) {
            std::string src_c = func_key_to_cname[members[i]];
            runtime_builder << "ML_MOVE_SYMBOLS_TO(" << anchor_c << "," << src_c << ");\n";
        }
    }

    write_file(runtime_path.string(), runtime_builder.str());
}

void register_backend_megalinker() {
    Backend backend;
    backend.info.name = "megalinker";
    backend.info.description = "Megalinker banked backend";
    backend.info.version = "v0.2.1";
    backend.emit = emit_megalinker_backend;
    (void)register_backend(backend);
}

} // namespace vexel
