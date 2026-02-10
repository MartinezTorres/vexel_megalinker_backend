#include "codegen.h"
#include "analysis.h"
#include "evaluator.h"
#include "expr_access.h"
#include "function_key.h"
#include "optimizer.h"
#include "typechecker.h"
#include "constants.h"

#include <algorithm>
#include <functional>
#include <iomanip>
#include <cctype>
#include <tuple>
#include <sstream>
#include <deque>

namespace {

std::string escape_c_string(const std::string& input) {
    std::ostringstream oss;
    oss << std::hex << std::setfill('0');
    for (unsigned char c : input) {
        switch (c) {
            case '\\':
                oss << "\\\\";
                break;
            case '"':
                oss << "\\\"";
                break;
            case '\n':
                oss << "\\n";
                break;
            case '\r':
                oss << "\\r";
                break;
            case '\t':
                oss << "\\t";
                break;
            default:
                if (c >= 0x20 && c <= 0x7e) {
                    oss << static_cast<char>(c);
                } else {
                    oss << "\\x" << std::uppercase << std::setw(2) << static_cast<int>(c) << std::nouppercase;
                }
                break;
        }
    }
    return oss.str();
}

} // namespace

namespace vexel::megalinker_codegen {
std::string CodeGenerator::gen_expr(ExprPtr expr) {
    if (!expr) return "";
    std::string void_name;
    if (is_void_call(expr, &void_name) && !allow_void_call) {
        throw CompileError(
            "Function '" + void_name +
            "' has no return type; its result cannot be used as a value. "
            "Declare a return type or call it as a statement.",
            expr->location);
    }

    bool fold_side_effect_free =
        expr->kind != Expr::Kind::Assignment &&
        expr->kind != Expr::Kind::Iteration &&
        expr->kind != Expr::Kind::Repeat &&
        expr->kind != Expr::Kind::Block;
    if (allow_constexpr_fold && fold_side_effect_free) {
        CTValue folded;
        if (try_evaluate(expr, folded)) {
            auto scalar_literal = [&](const CTValue& value) -> std::optional<std::string> {
                if (std::holds_alternative<int64_t>(value)) {
                    return std::to_string(std::get<int64_t>(value));
                }
                if (std::holds_alternative<uint64_t>(value)) {
                    return std::to_string(std::get<uint64_t>(value));
                }
                if (std::holds_alternative<bool>(value)) {
                    return std::get<bool>(value) ? "1" : "0";
                }
                if (std::holds_alternative<double>(value)) {
                    return std::to_string(std::get<double>(value));
                }
                return std::nullopt;
            };

            if (auto lit = scalar_literal(folded)) {
                return *lit;
            }

            if (std::holds_alternative<std::shared_ptr<CTArray>>(folded) &&
                expr->type && expr->type->kind == Type::Kind::Array &&
                expr->type->element_type) {
                auto array_value = std::get<std::shared_ptr<CTArray>>(folded);
                if (array_value) {
                    std::string temp = fresh_temp();
                    std::string elem_type = gen_type(expr->type->element_type);
                    std::ostringstream init;
                    init << storage_prefix() << elem_type << " " << temp << "["
                         << array_value->elements.size() << "] = {";
                    for (size_t i = 0; i < array_value->elements.size(); ++i) {
                        auto lit = scalar_literal(array_value->elements[i]);
                        if (!lit.has_value()) {
                            init.str("");
                            break;
                        }
                        if (i > 0) init << ", ";
                        init << *lit;
                    }
                    if (!init.str().empty()) {
                        init << "};";
                        emit(init.str());
                        return temp;
                    }
                }
            }

        }
    }

    switch (expr->kind) {
        case Expr::Kind::IntLiteral:
            return std::to_string((int64_t)expr->uint_val);
        case Expr::Kind::FloatLiteral:
            return std::to_string(expr->float_val);
        case Expr::Kind::StringLiteral:
            {
                std::string lit = "\"" + escape_c_string(expr->string_val) + "\"";
                if (ptr_kind_for_expr(expr) == PtrKind::Far) {
                    std::string mod = current_module_id_expr.empty() ? "0" : current_module_id_expr;
                    return "VX_FARPTR(" + mod + ", " + lit + ")";
                }
                return lit;
            }
        case Expr::Kind::CharLiteral:
            return std::to_string(expr->uint_val);
        case Expr::Kind::Identifier:
            // Check if this is an expression parameter reference
            if (expr->is_expr_param_ref && expr_param_substitutions.count(expr->name)) {
                // Substitute the expression
                return gen_expr(expr_param_substitutions[expr->name]);
            }
            {
                auto value_it = value_param_replacements.find(expr->name);
                if (value_it != value_param_replacements.end()) {
                    return value_it->second;
                }
            }
            // Check if this is the underscore loop variable
            if (expr->name == "_" && !underscore_var.empty()) {
                return underscore_var;
            }
            {
                Symbol* sym = binding_for(expr);
                std::string name = mangle_name(expr->name);
                bool is_ref_param = current_ref_params.count(expr->name) > 0;
                bool is_agg_param = current_aggregate_params.count(expr->name) > 0;
                if (sym) {
                    name += instance_suffix(sym);
                }
                int scope_id = sym ? scope_id_for_symbol(sym) : -1;

                if (abi.symbol_load_expr) {
                    std::string load_expr = abi.symbol_load_expr(expr->name, scope_id, current_bank_page);
                    if (!load_expr.empty()) {
                        std::string load_fn = load_module_fn((current_bank_page == 'A') ? 'B' : 'A');
                        emit(load_fn + "(" + load_expr + ");");
                    }
                }

                if (expr->type && is_pointer_like(expr->type) &&
                    ptr_kind_for_symbol(sym) == PtrKind::Far) {
                    std::string mod_expr;
                    if (abi.symbol_module_id_expr) {
                        mod_expr = abi.symbol_module_id_expr(expr->name, scope_id, current_bank_page);
                    }
                    if (!mod_expr.empty()) {
                        return "VX_FARPTR(" + mod_expr + ", " + name + ")";
                    }
                }
                if (is_ref_param) {
                    return "(*" + name + ")";
                }
                if (is_agg_param) {
                    return "(*" + name + ")";
                }
                return name;
            }
        case Expr::Kind::Binary:
            return gen_binary(expr);
        case Expr::Kind::Unary:
            return gen_unary(expr);
        case Expr::Kind::Call:
            return gen_call(expr);
        case Expr::Kind::Index:
            return gen_index(expr);
        case Expr::Kind::Member:
            return gen_member(expr);
        case Expr::Kind::ArrayLiteral:
            return gen_array_literal(expr);
        case Expr::Kind::TupleLiteral:
            return gen_tuple_literal(expr);
        case Expr::Kind::Block:
            return gen_block(expr);
        case Expr::Kind::Conditional:
            return gen_conditional(expr);
        case Expr::Kind::Cast:
            return gen_cast(expr);
        case Expr::Kind::Assignment:
            return gen_assignment(expr);
        case Expr::Kind::Range:
            return gen_range(expr);
        case Expr::Kind::Length:
            return gen_length(expr);
        case Expr::Kind::Iteration:
            return gen_iteration(expr);
        case Expr::Kind::Repeat:
            return gen_repeat(expr);
        case Expr::Kind::Resource:
            return "";
        case Expr::Kind::Process:
            return "";
    }
    return "";
}

std::string CodeGenerator::gen_binary(ExprPtr expr) {
    std::string left;
    {
        VoidCallGuard guard(*this, false);
        left = gen_expr(expr->left);
    }

    // Preserve runtime short-circuit semantics for logical operators.
    if (expr->op == "&&" || expr->op == "||") {
        std::string tmp = fresh_temp();
        std::string result_type = c_type_for_expr(expr);
        if (!declared_temps.count(tmp)) {
            emit(storage_prefix() + result_type + " " + tmp + ";");
            declared_temps.insert(tmp);
        }
        emit(tmp + " = " + left + ";");

        if (expr->op == "&&") {
            emit("if (" + tmp + ") {");
        } else {
            emit("if (!" + tmp + ") {");
        }

        std::string right;
        {
            VoidCallGuard guard(*this, false);
            right = gen_expr(expr->right);
        }
        emit(tmp + " = " + right + ";");
        emit("}");
        return tmp;
    }

    std::string right;
    {
        VoidCallGuard guard(*this, false);
        right = gen_expr(expr->right);
    }
    if (expr->op == "==" || expr->op == "!=") {
        TypePtr cmp_type = expr->left ? expr->left->type : nullptr;
        if (!cmp_type && expr->left && expr->left->kind == Expr::Kind::Identifier) {
            Symbol* sym = binding_for(expr->left);
            if (sym) {
                cmp_type = sym->type;
            }
        }
        if (!cmp_type && expr->right) {
            cmp_type = expr->right->type;
        }
        if (cmp_type &&
            (cmp_type->kind == Type::Kind::Array ||
             cmp_type->kind == Type::Kind::Named ||
             (cmp_type->kind == Type::Kind::Primitive && cmp_type->primitive == PrimitiveType::String))) {
            std::string cmp_name = ensure_comparator(cmp_type);
            if (!expr->type || expr->type->kind == Type::Kind::TypeVar) {
                return "(" + cmp_name + "(" + left + ", " + right + ") " + expr->op + " 0)";
            }
            std::string tmp = fresh_temp();
            std::string result_type = c_type_for_expr(expr);
            if (!declared_temps.count(tmp)) {
                emit(storage_prefix() + result_type + " " + tmp + ";");
                declared_temps.insert(tmp);
            }
            emit(tmp + " = (" + cmp_name + "(" + left + ", " + right + ") " + expr->op + " 0);");
            return tmp;
        }
    }
    if (!expr->type || expr->type->kind == Type::Kind::TypeVar) {
        return "(" + left + " " + expr->op + " " + right + ")";
    }
    std::string tmp = fresh_temp();
    std::string result_type = c_type_for_expr(expr);
    if (!declared_temps.count(tmp)) {
        emit(storage_prefix() + result_type + " " + tmp + ";");
        declared_temps.insert(tmp);
    }
    emit(tmp + " = (" + left + " " + expr->op + " " + right + ");");
    return tmp;
}

std::string CodeGenerator::gen_unary(ExprPtr expr) {
    std::string operand;
    {
        VoidCallGuard guard(*this, false);
        operand = gen_expr(expr->operand);
    }
    if (!expr->type || expr->type->kind == Type::Kind::TypeVar) {
        return "(" + expr->op + operand + ")";
    }
    std::string tmp = fresh_temp();
    std::string result_type = c_type_for_expr(expr);
    if (!declared_temps.count(tmp)) {
        emit(storage_prefix() + result_type + " " + tmp + ";");
        declared_temps.insert(tmp);
    }
    emit(tmp + " = (" + expr->op + operand + ");");
    return tmp;
}

std::string CodeGenerator::gen_call(ExprPtr expr) {
    // Check if this is a type constructor call
    if (expr->operand && expr->operand->kind == Expr::Kind::Identifier &&
        expr->type && expr->type->kind == Type::Kind::Named) {

        Symbol* sym = binding_for(expr->operand);

        if (sym && sym->kind == Symbol::Kind::Type && sym->declaration) {
            // Generate C struct initialization
            std::string type_name = mangle_name(expr->operand->name);
            std::string result = "((" + type_name + "){";

            // Match arguments to fields by position
            {
                VoidCallGuard guard(*this, false);
                for (size_t i = 0; i < expr->args.size() && i < sym->declaration->fields.size(); i++) {
                    if (i > 0) result += ", ";
                    result += "." + mangle_name(sym->declaration->fields[i].name) + " = ";
                    result += gen_expr(expr->args[i]);
                }
            }

            result += "})";
            return result;
        }
    }

    // Check if this function has expression parameters - if so, inline it
    if (expr->operand && expr->operand->kind == Expr::Kind::Identifier) {
        Symbol* sym = binding_for(expr->operand);
        if (sym && sym->kind == Symbol::Kind::Function && sym->declaration) {
            // Check if any parameters are expression parameters
            bool has_expr_params = false;
            for (const auto& param : sym->declaration->params) {
                if (param.is_expression_param) {
                    has_expr_params = true;
                    break;
                }
            }

            if (has_expr_params) {
                // Inline the function body with expression parameter substitutions
                // Save current substitutions
                auto saved_substitutions = expr_param_substitutions;
                expr_param_substitutions.clear();
                auto saved_value_replacements = value_param_replacements;
                value_param_replacements.clear();

                // Map expression parameters to their argument expressions
                for (size_t i = 0; i < expr->args.size() && i < sym->declaration->params.size(); i++) {
                    const auto& param = sym->declaration->params[i];
                    if (param.is_expression_param) {
                        expr_param_substitutions[param.name] = expr->args[i];
                    } else {
                        std::string arg_expr;
                        {
                            VoidCallGuard guard(*this, false);
                            arg_expr = gen_expr(expr->args[i]);
                        }
                        value_param_replacements[param.name] = arg_expr;
                    }
                }

                // Generate the inlined function body
                std::string result = gen_expr(sym->declaration->body);

                // Restore previous substitutions
                expr_param_substitutions = saved_substitutions;
                value_param_replacements = saved_value_replacements;

                return result;
            }
        }
    }

    // Regular function call or method call
    std::string func_name;
    std::string ref_key;
    std::vector<std::string> all_args;

    Symbol* sym = nullptr;
    StmtPtr callee_decl;
    bool is_external = false;
    std::string base_name;
    std::string func_key;
    char call_reentrancy_key = current_reentrancy_key;

    // Get function name (already qualified by type checker for methods)
    if (expr->operand && expr->operand->kind == Expr::Kind::Identifier) {
        // Look up symbol to get original function name and instance
        base_name = expr->operand->name;
        sym = binding_for(expr->operand);
        if (sym && sym->declaration && sym->kind == Symbol::Kind::Function) {
            callee_decl = sym->declaration;
            base_name = sym->name;
            is_external = sym->is_external || callee_decl->is_external;
        }

        if (callee_decl) {
            if (!is_external) {
                auto reent_it = facts.reentrancy_variants.find(sym);
                if (reent_it == facts.reentrancy_variants.end() || reent_it->second.empty()) {
                    throw CompileError("Internal error: missing reentrancy variants for callee '" +
                                           base_name + "'",
                                       expr->location);
                }
                if (!reent_it->second.count(call_reentrancy_key)) {
                    if (reent_it->second.size() == 1) {
                        call_reentrancy_key = *reent_it->second.begin();
                    } else {
                        throw CompileError("Internal error: caller/callee reentrancy mismatch for '" +
                                               base_name + "'",
                                           expr->location);
                    }
                }
            }

            if (!callee_decl->ref_params.empty()) {
                if (is_external) {
                    ref_key = std::string(callee_decl->ref_params.size(), 'M');
                } else {
                    ref_key = ref_variant_key(expr, callee_decl->ref_params.size());
                }
            }
            if (!is_external) {
                func_key = func_key_for(sym);
                std::string variant = variant_name(base_name, sym, call_reentrancy_key, ref_key);
                func_name = mangle_name(variant) + instance_suffix(sym);
            } else if (!ref_key.empty()) {
                std::string variant = ref_variant_name(base_name, ref_key);
                func_name = mangle_name(variant);
            } else {
                func_name = mangle_name(base_name);
            }
        } else {
            func_name = mangle_name(base_name);
        }
    } else if (expr->operand) {
        VoidCallGuard guard(*this, false);
        func_name = gen_expr(expr->operand);
    }

    CallTargetInfo target;
    if (abi.resolve_call && callee_decl && !is_external && !func_key.empty()) {
        target = abi.resolve_call(expr, base_name, func_key, current_variant_id, current_bank_page, ref_key);
        if (!target.name.empty()) {
            if (target.name_is_mangled) {
                func_name = target.name;
            } else {
                func_name = mangle_name(target.name);
            }
            func_name += instance_suffix(sym);
        }
    }

    bool returns_aggregate = false;
    bool returns_value = false;
    std::string agg_out_type;
    TypePtr return_type_ptr = nullptr;
    if (abi.lower_aggregates && callee_decl && !is_external) {
        bool returns_tuple = !callee_decl->return_types.empty();
        if (!returns_tuple) {
            if (callee_decl->return_type) {
                return_type_ptr = callee_decl->return_type;
            } else if (callee_decl->body && callee_decl->body->type) {
                return_type_ptr = callee_decl->body->type;
            }
        }
        returns_value = returns_tuple || (return_type_ptr && return_type_ptr->kind != Type::Kind::TypeVar);
        if (returns_tuple || is_aggregate_type(return_type_ptr)) {
            returns_aggregate = true;
            if (returns_tuple) {
                std::string tuple_name = std::string(TUPLE_TYPE_PREFIX) + std::to_string(callee_decl->return_types.size());
                for (const auto& t : callee_decl->return_types) {
                    tuple_name += "_";
                    if (t) {
                        tuple_name += t->to_string();
                    } else {
                        tuple_name += "unknown";
                    }
                }
                agg_out_type = mangle_name(tuple_name);
            } else if (return_type_ptr) {
                agg_out_type = gen_type(return_type_ptr);
            }
        }
    } else if (callee_decl && !is_external) {
        bool returns_tuple = !callee_decl->return_types.empty();
        if (!returns_tuple) {
            if (callee_decl->return_type) {
                return_type_ptr = callee_decl->return_type;
            } else if (callee_decl->body && callee_decl->body->type) {
                return_type_ptr = callee_decl->body->type;
            }
        }
        returns_value = returns_tuple || (return_type_ptr && return_type_ptr->kind != Type::Kind::TypeVar);
    }

    bool call_nonreentrant_frame =
        (!is_external && callee_decl && !callee_decl->is_exported && call_reentrancy_key == 'N');

    std::vector<bool> param_is_aggregate;
    if (abi.lower_aggregates && callee_decl) {
        for (const auto& param : callee_decl->params) {
            if (param.is_expression_param) continue;
            param_is_aggregate.push_back(param.type && is_aggregate_type(param.type));
        }
    }

    // Handle method calls with receivers - add them as first arguments
    if (!expr->receivers.empty()) {
        for (size_t i = 0; i < expr->receivers.size(); i++) {
            ExprPtr rec = expr->receivers[i];
            bool by_ref = receiver_is_mutable_arg(rec);
            if (!ref_key.empty() && i < ref_key.size()) {
                by_ref = ref_key[i] == 'M';
            }
            if (abi.lower_aggregates && rec && rec->type && is_aggregate_type(rec->type)) {
                by_ref = true;
            }
            if (by_ref) {
                if (is_addressable_lvalue(rec) && is_mutable_lvalue(rec)) {
                    std::string rec_expr;
                    {
                        VoidCallGuard guard(*this, false);
                        FoldGuard fold_guard(*this, false);
                        rec_expr = gen_expr(rec);
                    }
                    if (!rec || !rec->type || rec->type->kind == Type::Kind::TypeVar) {
                        all_args.push_back("&" + rec_expr);
                    } else {
                        std::string ptr_temp = fresh_temp();
                        std::string rtype = gen_type(rec->type);
                        emit(rtype + "* " + ptr_temp + " = &" + rec_expr + ";");
                        all_args.push_back(ptr_temp);
                    }
                } else {
                    std::string temp = fresh_temp();
                    if (!rec || !rec->type) {
                        throw CompileError("Missing receiver type for call", rec ? rec->location : expr->location);
                    }
                    std::string rtype = gen_type(rec->type);
                    std::string rec_expr;
                    {
                        VoidCallGuard guard(*this, false);
                        rec_expr = gen_expr(rec);
                    }
                    emit(rtype + " " + temp + " = " + rec_expr + ";");
                    all_args.push_back("&" + temp);
                }
            } else {
                std::string rec_expr;
                {
                    VoidCallGuard guard(*this, false);
                    rec_expr = gen_expr(rec);
                }
                if (!rec || !rec->type || rec->type->kind == Type::Kind::TypeVar) {
                    all_args.push_back(rec_expr);
                } else {
                    std::string staged = fresh_temp();
                    std::string stype = c_type_for_expr(rec);
                    emit(stype + " " + staged + " = " + rec_expr + ";");
                    all_args.push_back(staged);
                }
            }
        }
    }

    // Add regular arguments (skip expression parameters)
    size_t param_idx = 0;
    size_t agg_idx = 0;
    for (size_t i = 0; i < expr->args.size(); i++) {
        // Check if this argument corresponds to an expression parameter
        if (expr->operand && expr->operand->kind == Expr::Kind::Identifier) {
            Symbol* psym = binding_for(expr->operand);
            if (psym && psym->kind == Symbol::Kind::Function && psym->declaration &&
                param_idx < psym->declaration->params.size() &&
                psym->declaration->params[param_idx].is_expression_param) {
                param_idx++;
                continue;  // Skip expression parameters
            }
        }
        bool by_ref = (agg_idx < param_is_aggregate.size() && param_is_aggregate[agg_idx]);
        std::string arg_expr;
        {
            VoidCallGuard guard(*this, false);
            arg_expr = gen_expr(expr->args[i]);
        }
        if (by_ref) {
            if (is_addressable_lvalue(expr->args[i])) {
                {
                    VoidCallGuard guard(*this, false);
                    FoldGuard fold_guard(*this, false);
                    arg_expr = gen_expr(expr->args[i]);
                }
                if (!expr->args[i] || !expr->args[i]->type || expr->args[i]->type->kind == Type::Kind::TypeVar) {
                    all_args.push_back("&" + arg_expr);
                } else {
                    std::string temp = fresh_temp();
                    std::string atype = gen_type(expr->args[i]->type);
                    emit(atype + "* " + temp + " = &" + arg_expr + ";");
                    all_args.push_back(temp);
                }
            } else {
                std::string temp = fresh_temp();
                if (!expr->args[i] || !expr->args[i]->type) {
                    throw CompileError("Missing argument type for call", expr->args[i] ? expr->args[i]->location : expr->location);
                }
                std::string atype = gen_type(expr->args[i]->type);
                emit(atype + " " + temp + " = " + arg_expr + ";");
                all_args.push_back("&" + temp);
            }
        } else {
            if (!expr->args[i] || !expr->args[i]->type || expr->args[i]->type->kind == Type::Kind::TypeVar) {
                all_args.push_back(arg_expr);
            } else {
                std::string temp = fresh_temp();
                std::string atype = c_type_for_expr(expr->args[i]);
                emit(atype + " " + temp + " = " + arg_expr + ";");
                all_args.push_back(temp);
            }
        }
        param_idx++;
        agg_idx++;
    }

    std::string out_temp;
    if (returns_aggregate && !call_nonreentrant_frame) {
        out_temp = fresh_temp();
        if (!declared_temps.count(out_temp)) {
            emit(storage_prefix() + agg_out_type + " " + out_temp + ";");
            declared_temps.insert(out_temp);
        }
        all_args.insert(all_args.begin(), "&" + out_temp);
    }

    if (!target.module_id_expr.empty()) {
        std::string load_fn = load_module_fn(target.page);
        emit(load_fn + "(" + target.module_id_expr + ");");
    }

    if (call_nonreentrant_frame) {
        for (size_t i = 0; i < all_args.size(); ++i) {
            emit(nonreentrant_arg_slot_name(func_name, i) + " = " + all_args[i] + ";");
        }
        emit(func_name + "();");
        if (returns_aggregate || returns_value) {
            std::string ret_temp = fresh_temp();
            std::string ret_type = returns_aggregate ? agg_out_type : c_type_for_expr(expr);
            if (!declared_temps.count(ret_temp)) {
                emit(storage_prefix() + ret_type + " " + ret_temp + ";");
                declared_temps.insert(ret_temp);
            }
            emit(ret_temp + " = " + nonreentrant_ret_slot_name(func_name) + ";");
            return ret_temp;
        }
        return "";
    }

    std::string result = func_name + "(";
    for (size_t i = 0; i < all_args.size(); i++) {
        if (i > 0) result += ", ";
        result += all_args[i];
    }
    result += ")";

    if (returns_aggregate) {
        emit(result + ";");
        if (allow_void_call) {
            return "";
        }
        return out_temp;
    }

    return result;
}

std::string CodeGenerator::gen_index(ExprPtr expr) {
    std::string arr;
    std::string idx;
    {
        VoidCallGuard guard(*this, false);
        arr = gen_expr(expr->operand);
        idx = gen_expr(expr->args[0]);
    }
    if (ptr_kind_for_expr(expr->operand) == PtrKind::Far) {
        std::string ptr_expr = arr;
        if (expr_has_side_effects(expr->operand)) {
            std::string temp = fresh_temp();
            if (!declared_temps.count(temp)) {
                emit(storage_prefix() + std::string("uint32_t ") + temp + ";");
                declared_temps.insert(temp);
            }
            emit(temp + " = " + arr + ";");
            ptr_expr = temp;
        }
        std::string load_fn = load_module_fn((current_bank_page == 'A') ? 'B' : 'A');
        emit(load_fn + "(VX_FARPTR_MOD(" + ptr_expr + "));");
        std::string elem_type = require_type(expr->type,
                                             expr->location,
                                             "index expression element type");
        return "(((" + elem_type + "*)VX_FARPTR_ADDR(" + ptr_expr + "))[" + idx + "])";
    }
    return arr + "[" + idx + "]";
}

std::string CodeGenerator::gen_member(ExprPtr expr) {
    bool operand_is_ref = false;
    std::string obj;
    if (expr->operand && expr->operand->kind == Expr::Kind::Identifier &&
        (current_ref_params.count(expr->operand->name) ||
         current_aggregate_params.count(expr->operand->name))) {
        operand_is_ref = true;
        obj = mangle_name(expr->operand->name);
        Symbol* sym = binding_for(expr->operand);
        if (sym) {
            obj += instance_suffix(sym);
        }
    } else {
        VoidCallGuard guard(*this, false);
        obj = gen_expr(expr->operand);
    }

    // Check if the operand is a reference parameter (pointer in C)
    std::string accessor = operand_is_ref ? "->" : ".";

    // Don't mangle compiler-generated tuple field names (e.g., __0, __1)
    std::string member_name = expr->name;
    if (member_name.size() < 2 || member_name.substr(0, 2) != MANGLED_PREFIX) {
        member_name = mangle_name(member_name);
    }

    return obj + accessor + member_name;
}

std::string CodeGenerator::gen_array_literal(ExprPtr expr) {
    std::string temp = fresh_temp();

    // Determine element type
    if (!expr->type || expr->type->kind != Type::Kind::Array || !expr->type->element_type) {
        throw CompileError("Missing array element type for array literal", expr->location);
    }
    std::string elem_type = gen_type(expr->type->element_type);

    size_t count = expr->elements.size();
    size_t storage_count = count == 0 ? 1 : count;
    // Generate array declaration and initialization
    // Use static to ensure it persists beyond the current scope (important for struct fields)
    emit("static " + elem_type + " " + temp + "[" + std::to_string(storage_count) + "] = {");
    {
        VoidCallGuard guard(*this, false);
        for (size_t i = 0; i < count; i++) {
            if (i > 0) emit(", ");
            emit(gen_expr(expr->elements[i]));
        }
    }
    emit("};");

    if (ptr_kind_for_expr(expr) == PtrKind::Far) {
        std::string mod = current_module_id_expr.empty() ? "0" : current_module_id_expr;
        return "VX_FARPTR(" + mod + ", " + temp + ")";
    }
    return temp;
}

std::string CodeGenerator::gen_tuple_literal(ExprPtr expr) {
    // Generate tuple struct literal: (Type){.field0 = val0, .field1 = val1, ...}
    std::string type_name = require_type(expr->type,
                                         expr->location,
                                         "tuple literal type");

    // Track this tuple type for declaration generation
    if (expr->type && expr->type->kind == Type::Kind::Named) {
        std::string tuple_name = expr->type->type_name;
        if (tuple_name.find(TUPLE_TYPE_PREFIX) == 0 && !tuple_types.count(tuple_name)) {
            // Collect element types
            std::vector<TypePtr> elem_types;
            for (const auto& elem : expr->elements) {
                elem_types.push_back(elem->type);
            }
            tuple_types[tuple_name] = elem_types;
        }
    }

    std::string result = "((" + type_name + "){";
    {
        VoidCallGuard guard(*this, false);
        for (size_t i = 0; i < expr->elements.size(); i++) {
            if (i > 0) result += ", ";
            result += ".__" + std::to_string(i) + " = ";
            result += gen_expr(expr->elements[i]);
        }
    }
    result += "})";

    return result;
}

std::string CodeGenerator::gen_block(ExprPtr expr) {
    if (allow_void_call) {
        emit("{");
        for (const auto& stmt : expr->statements) {
            gen_stmt(stmt);
        }
        if (expr->result_expr) {
            std::string result = gen_expr(expr->result_expr);
            if (!result.empty()) {
                emit(result + ";");
            }
        }
        emit("}");
        return "";
    }

    if (!expr->result_expr) {
        throw CompileError("Block expression has no result; use it as a statement or add a final expression",
                           expr->location);
    }

    std::string temp = fresh_temp();
    std::string result_type;

    // Infer result type from expression type or result_expr type
    if (expr->type) {
        result_type = gen_type(expr->type);
        if (is_pointer_like(expr->type) && ptr_kind_for_expr(expr) == PtrKind::Far) {
            result_type = "uint32_t";
        }
    } else if (expr->result_expr && expr->result_expr->type) {
        result_type = gen_type(expr->result_expr->type);
        if (is_pointer_like(expr->result_expr->type) &&
            ptr_kind_for_expr(expr->result_expr) == PtrKind::Far) {
            result_type = "uint32_t";
        }
    } else {
        throw CompileError("Missing type for block expression result", expr->location);
    }

    // Declare temp variable outside block scope only if not already declared
    if (!declared_temps.count(temp)) {
        emit(storage_prefix() + result_type + " " + temp + ";");
        declared_temps.insert(temp);
    }

    emit("{");
    for (const auto& stmt : expr->statements) {
        gen_stmt(stmt);
    }

    std::string result;
    {
        VoidCallGuard guard(*this, false);
        result = gen_expr(expr->result_expr);
    }
    emit(temp + " = " + result + ";");
    // Release the result temp if it's a temporary
    if (result.rfind("tmp", 0) == 0 &&
        (!expr->result_expr || !expr->result_expr->type ||
         expr->result_expr->type->kind != Type::Kind::Array)) {
        release_temp(result);
    }
    emit("}");

    return temp;
}

std::string CodeGenerator::gen_block_optimized(ExprPtr expr) {
    (void)expr;
    return "";
}

std::string CodeGenerator::gen_call_optimized_with_evaluator(ExprPtr expr, CompileTimeEvaluator& evaluator) {
    if (!expr || expr->kind != Expr::Kind::Call) return "";
    if (!expr->operand || expr->operand->kind != Expr::Kind::Identifier) return "";

    std::string func_name = expr->operand->name;

    // Check if it's an external function
    Symbol* sym = binding_for(expr->operand);
    if (!sym || sym->kind != Symbol::Kind::Function || !sym->declaration) return "";
    if (!sym->declaration->is_external) return "";

    // Try to evaluate all arguments at compile-time
    std::vector<std::string> arg_strs;

    for (const auto& arg : expr->args) {
        CTValue arg_val;
        if (evaluator.try_evaluate(arg, arg_val)) {
            if (std::holds_alternative<int64_t>(arg_val)) {
                arg_strs.push_back(std::to_string(std::get<int64_t>(arg_val)));
            } else if (std::holds_alternative<uint64_t>(arg_val)) {
                arg_strs.push_back(std::to_string(std::get<uint64_t>(arg_val)));
            } else {
                return "";
            }
        } else {
            return "";
        }
    }

    // Generate optimized call
    std::string result = mangle_name(func_name) + "(";
    for (size_t i = 0; i < arg_strs.size(); i++) {
        if (i > 0) result += ", ";
        result += arg_strs[i];
    }
    result += ")";
    return result;
}

std::string CodeGenerator::gen_conditional(ExprPtr expr) {
    // Try to evaluate condition at compile time for dead branch elimination
    {
        CTValue cond_val;
        if (try_evaluate(expr->condition, cond_val)) {
            // Condition is compile-time constant - eliminate dead branch
            bool is_true = false;
            if (std::holds_alternative<int64_t>(cond_val)) {
                is_true = std::get<int64_t>(cond_val) != 0;
            } else if (std::holds_alternative<bool>(cond_val)) {
                is_true = std::get<bool>(cond_val);
            } else if (std::holds_alternative<uint64_t>(cond_val)) {
                is_true = std::get<uint64_t>(cond_val) != 0;
            }

            if (is_true) {
                if (allow_void_call) {
                    return gen_expr(expr->true_expr);
                }
                VoidCallGuard guard(*this, false);
                return gen_expr(expr->true_expr);
            } else {
                if (allow_void_call) {
                    return gen_expr(expr->false_expr);
                }
                VoidCallGuard guard(*this, false);
                return gen_expr(expr->false_expr);
            }
        }
    }

    // Runtime conditional
    std::string cond;
    {
        VoidCallGuard guard(*this, false);
        cond = gen_expr(expr->condition);
    }
    std::string true_expr;
    std::string false_expr;
    if (allow_void_call) {
        true_expr = gen_expr(expr->true_expr);
        false_expr = gen_expr(expr->false_expr);
    } else {
        VoidCallGuard guard(*this, false);
        true_expr = gen_expr(expr->true_expr);
        false_expr = gen_expr(expr->false_expr);
    }
    return "(" + cond + " ? " + true_expr + " : " + false_expr + ")";
}

std::string CodeGenerator::gen_cast(ExprPtr expr) {
    // Primitive to byte array conversion (big-endian order)
    if (expr->target_type && expr->target_type->kind == Type::Kind::Array &&
        expr->target_type->element_type &&
        expr->target_type->element_type->kind == Type::Kind::Primitive &&
        expr->target_type->element_type->primitive == PrimitiveType::U8 &&
        expr->operand && expr->operand->type &&
        expr->operand->type->kind == Type::Kind::Primitive &&
        !is_float(expr->operand->type->primitive)) {

        int64_t length = resolve_array_length(expr->target_type, expr->location);
        int bits = type_bits(expr->operand->type->primitive);
        if (bits / 8 != length) {
            throw CompileError("Array length/type size mismatch in cast", expr->location);
        }

        std::string source_val;
        {
            VoidCallGuard guard(*this, false);
            source_val = gen_expr(expr->operand);
        }
        std::string source_tmp = fresh_temp();
        std::string source_type = gen_type(expr->operand->type);
        if (!declared_temps.count(source_tmp)) {
            emit(storage_prefix() + source_type + " " + source_tmp + ";");
            declared_temps.insert(source_tmp);
        }
        emit(source_tmp + " = " + source_val + ";");

        std::string result = fresh_temp();
        std::string elem_type = gen_type(expr->target_type->element_type);
        std::string size_str = std::to_string(length);
        if (!declared_temps.count(result)) {
            emit(storage_prefix() + elem_type + " " + result + "[" + size_str + "];");
            declared_temps.insert(result);
        }

        for (int64_t i = 0; i < length; ++i) {
            int64_t shift = (length - 1 - i) * 8;
            emit(result + "[" + std::to_string(i) + "] = (" + elem_type + ")((" + source_tmp +
                 " >> " + std::to_string(shift) + ") & 0xFF);");
        }

        return result;
    }

    // Special-case: pack boolean arrays into unsigned integers
    if (expr->operand && expr->operand->type &&
        expr->operand->type->kind == Type::Kind::Array &&
        expr->operand->type->element_type &&
        expr->operand->type->element_type->kind == Type::Kind::Primitive &&
        expr->operand->type->element_type->primitive == PrimitiveType::Bool &&
        expr->target_type && expr->target_type->kind == Type::Kind::Primitive &&
        is_unsigned_int(expr->target_type->primitive)) {

        int64_t length = resolve_array_length(expr->operand->type, expr->location);
        std::string target = gen_type(expr->target_type);
        std::string source;
        {
            VoidCallGuard guard(*this, false);
            source = gen_expr(expr->operand);
        }
        std::string temp = fresh_temp();
        if (!declared_temps.count(temp)) {
            emit(storage_prefix() + target + " " + temp + ";");
            declared_temps.insert(temp);
        }
        emit(temp + " = 0;");
        for (int64_t i = 0; i < length; ++i) {
            int64_t shift = (length - 1 - i);
            emit(temp + " |= (" + source + "[" + std::to_string(i) + "] ? (" + target + ")(1u << " + std::to_string(shift) + ") : 0);");
        }
        return temp;
    }

    std::string target = gen_type(expr->target_type);
    std::string operand;
    {
        VoidCallGuard guard(*this, false);
        operand = gen_expr(expr->operand);
    }
    return "((" + target + ")" + operand + ")";
}

std::string CodeGenerator::gen_assignment(ExprPtr expr) {
    // Use the flag set by the typechecker to determine if this creates a new variable
    if (expr->creates_new_variable) {
        TypePtr var_type = expr->left->type ? expr->left->type : expr->type;
        Symbol* decl_sym = binding_for(expr->left);
        if ((!var_type || var_type->kind != Type::Kind::Array) &&
            decl_sym && decl_sym->type && decl_sym->type->kind == Type::Kind::Array) {
            var_type = decl_sym->type;
        }
        std::string var_type_str = gen_type(var_type);
        if (var_type && is_pointer_like(var_type) && var_type->kind != Type::Kind::Array) {
            if (ptr_kind_for_symbol(decl_sym) == PtrKind::Far) {
                var_type_str = "uint32_t";
            }
        }
        std::string var_name = mangle_name(expr->left->name);
        if (decl_sym) {
            var_name += instance_suffix(decl_sym);
        }

        // For array declarations, we need to handle the literal specially
        if (var_type && var_type->kind == Type::Kind::Array && expr->right->kind == Expr::Kind::ArrayLiteral) {
            // Generate array literal inline with correct type
            std::string elem_type = gen_type(var_type->element_type);

            // Get array size
            std::string size_str = std::to_string(resolve_array_length(var_type, expr->location));

            emit(elem_type + " " + var_name + "[" + size_str + "] = {");
            {
                VoidCallGuard guard(*this, false);
                for (size_t i = 0; i < expr->right->elements.size(); i++) {
                    if (i > 0) emit(", ");
                    emit(gen_expr(expr->right->elements[i]));
                }
            }
            emit("};");

            std::string temp = fresh_temp();
            if (!declared_temps.count(temp)) {
                emit(storage_prefix() + std::string("int ") + temp + " = 0;");
                declared_temps.insert(temp);
            } else {
                emit(temp + " = 0;");
            }
            return temp;
        }

        if (var_type && var_type->kind == Type::Kind::Array) {
            std::string elem_type = require_type(var_type->element_type,
                                                 expr->location,
                                                 "array declaration element type");
            std::string size_str = std::to_string(resolve_array_length(var_type, expr->location));
            emit(elem_type + " " + var_name + "[" + size_str + "];");

            std::string rhs;
            {
                VoidCallGuard guard(*this, false);
                rhs = gen_expr(expr->right);
            }

            std::string idx = fresh_temp();
            if (!declared_temps.count(idx)) {
                emit(storage_prefix() + std::string("int ") + idx + ";");
                declared_temps.insert(idx);
            }
            emit("for (" + idx + " = 0; " + idx + " < " + size_str + "; ++" + idx + ") {");
            emit(var_name + "[" + idx + "] = " + rhs + "[" + idx + "];");
            emit("}");

            std::string temp = fresh_temp();
            if (!declared_temps.count(temp)) {
                emit(storage_prefix() + std::string("int ") + temp + " = 0;");
                declared_temps.insert(temp);
            } else {
                emit(temp + " = 0;");
            }
            return temp;
        }

        std::string rhs;
        {
            VoidCallGuard guard(*this, false);
            rhs = gen_expr(expr->right);
        }
        // Release RHS temp if it's a temporary
        if (rhs.rfind("tmp", 0) == 0 &&
            (!expr->right || !expr->right->type || expr->right->type->kind != Type::Kind::Array)) {
            release_temp(rhs);
        }
        std::string temp = fresh_temp();
        emit(var_type_str + " " + var_name + " = " + rhs + ";");
        if (!declared_temps.count(temp)) {
            emit(storage_prefix() + std::string("int ") + temp + " = 0;"); // Assignment as expression returns dummy value
            declared_temps.insert(temp);
        } else {
            emit(temp + " = 0;");
        }
        return temp;
    }

    // Regular assignment
    std::string lhs;
    std::string rhs;
    {
        VoidCallGuard guard(*this, false);
        FoldGuard lhs_guard(*this, false);
        lhs = gen_expr(expr->left);
    }
    {
        VoidCallGuard guard(*this, false);
        rhs = gen_expr(expr->right);
    }
    // Release RHS temp if it's a temporary
    if (rhs.rfind("tmp", 0) == 0 &&
        (!expr->right || !expr->right->type || expr->right->type->kind != Type::Kind::Array)) {
        release_temp(rhs);
    }

    TypePtr lhs_type = expr->left ? expr->left->type : nullptr;
    if ((!lhs_type || lhs_type->kind != Type::Kind::Array) &&
        expr->left && expr->left->kind == Expr::Kind::Identifier) {
        if (Symbol* lhs_sym = binding_for(expr->left)) {
            if (lhs_sym->type && lhs_sym->type->kind == Type::Kind::Array) {
                lhs_type = lhs_sym->type;
            }
        }
    }
    if (lhs_type && lhs_type->kind == Type::Kind::Array) {
        std::string size_str = std::to_string(resolve_array_length(lhs_type, expr->location));
        std::string idx = fresh_temp();
        if (!declared_temps.count(idx)) {
            emit(storage_prefix() + std::string("int ") + idx + ";");
            declared_temps.insert(idx);
        }
        emit("for (" + idx + " = 0; " + idx + " < " + size_str + "; ++" + idx + ") {");
        emit(lhs + "[" + idx + "] = " + rhs + "[" + idx + "];");
        emit("}");
        std::string temp = fresh_temp();
        if (!declared_temps.count(temp)) {
            emit(storage_prefix() + std::string("int ") + temp + " = 0;");
            declared_temps.insert(temp);
        } else {
            emit(temp + " = 0;");
        }
        return temp;
    }

    return "(" + lhs + " = " + rhs + ")";
}

std::optional<std::pair<int64_t, int64_t>> CodeGenerator::evaluate_range(ExprPtr range_expr) {
    if (!range_expr || range_expr->kind != Expr::Kind::Range) {
        return std::nullopt;
    }

    CTValue start_val, end_val;
    if (try_evaluate(range_expr->left, start_val) &&
        try_evaluate(range_expr->right, end_val)) {
        return std::make_pair(std::get<int64_t>(start_val), std::get<int64_t>(end_val));
    }
    return std::nullopt;
}

std::string CodeGenerator::gen_range(ExprPtr expr) {
    auto bounds = evaluate_range(expr);
    if (!bounds) {
        throw CompileError("Range bounds must be compile-time constants", expr->location);
    }

    int64_t start = bounds->first;
    int64_t end = bounds->second;

    if (start == end) {
        throw CompileError("Range cannot produce an empty array; bounds must differ", expr->location);
    }

    std::string temp = fresh_temp();
    if (!expr->type || expr->type->kind != Type::Kind::Array || !expr->type->element_type) {
        throw CompileError("Internal error: range expression missing element type", expr->location);
    }
    std::string elem_type = require_type(expr->type->element_type,
                                         expr->location,
                                         "range element type");

    int64_t size = (start < end) ? (end - start) : (start - end);
    if (size <= 0) {
        throw CompileError("Invalid range bounds", expr->location);
    }

    std::ostringstream init;
    init << storage_prefix() << elem_type << " " << temp << "[" << size << "] = {";
    bool first = true;
    if (start < end) {
        for (int64_t i = start; i < end; ++i) {
            if (!first) init << ", ";
            first = false;
            init << i;
        }
    } else {
        for (int64_t i = start; i > end; --i) {
            if (!first) init << ", ";
            first = false;
            init << i;
        }
    }
    init << "};";
    emit(init.str());
    return temp;
}

std::string CodeGenerator::gen_length(ExprPtr expr) {
    // Array length or absolute value
    if (expr->operand->type) {
        if (expr->operand->type->kind == Type::Kind::Array) {
            // Array length - try to evaluate at compile time
            return std::to_string(resolve_array_length(expr->operand->type, expr->location));
        } else if (expr->operand->type->kind == Type::Kind::Primitive &&
                   expr->operand->type->primitive == PrimitiveType::String) {
            // String length - compile time constant
            if (expr->operand->kind == Expr::Kind::StringLiteral) {
                return std::to_string(expr->operand->string_val.size());
            }
            std::string operand;
            {
                VoidCallGuard guard(*this, false);
                operand = gen_expr(expr->operand);
            }
            if (ptr_kind_for_expr(expr->operand) == PtrKind::Far) {
                std::string fn = strlen_far_fn((current_bank_page == 'A') ? 'B' : 'A');
                return fn + "(" + operand + ")";
            }
            return "strlen(" + operand + ")";
        }
    }

    // Absolute value for numeric types
    std::string operand;
    {
        VoidCallGuard guard(*this, false);
        operand = gen_expr(expr->operand);
    }
    if (expr->operand->type && expr->operand->type->kind == Type::Kind::Primitive) {
        if (is_float(expr->operand->type->primitive)) {
            return "fabs(" + operand + ")";
        } else if (is_signed_int(expr->operand->type->primitive)) {
            return "abs(" + operand + ")";
        } else {
            // Unsigned - identity
            return operand;
        }
    }
    return "abs(" + operand + ")";
}

std::string CodeGenerator::gen_iteration(ExprPtr expr) {
    if (!expr->operand || !expr->operand->type || expr->operand->type->kind != Type::Kind::Array) {
        throw CompileError("Iteration requires array or range", expr->location);
    }

    TypePtr array_type = expr->operand->type;
    TypePtr element_type = array_type->element_type;
    std::string element_c_type = require_type(element_type,
                                              array_type->location,
                                              "iteration element type");

    std::string size_str;
    int64_t element_count = 0;
    auto value_to_int64 = [&](const CTValue& v, const SourceLocation& loc) -> int64_t {
        if (std::holds_alternative<int64_t>(v)) {
            return std::get<int64_t>(v);
        }
        if (std::holds_alternative<uint64_t>(v)) {
            return static_cast<int64_t>(std::get<uint64_t>(v));
        }
        throw CompileError("Iteration bounds must be integer constants", loc);
    };

    if (expr->operand->kind == Expr::Kind::Range) {
        CTValue start_val, end_val;
        if (!try_evaluate(expr->operand->left, start_val) ||
            !try_evaluate(expr->operand->right, end_val)) {
            throw CompileError("Range iteration requires compile-time constant bounds", expr->location);
        }
        int64_t start = value_to_int64(start_val, expr->operand->left->location);
        int64_t end = value_to_int64(end_val, expr->operand->right->location);
        element_count = (start < end) ? (end - start) : (start - end);
        if (element_count <= 0) {
            throw CompileError("Range iteration produced empty sequence", expr->location);
        }
        size_str = std::to_string(element_count);
    } else {
        element_count = resolve_array_length(array_type, expr->location);
        if (element_count < 0) {
            throw CompileError("Array size cannot be negative", expr->location);
        }
        size_str = std::to_string(element_count);
    }

    std::string array_expr;
    {
        VoidCallGuard guard(*this, false);
        array_expr = gen_expr(expr->operand);
    }
    std::string array_ptr = fresh_temp();
    std::string loop_var = fresh_temp();
    std::string underscore = fresh_temp();

    emit("{");
    emit("  " + element_c_type + "* " + array_ptr + " = " + array_expr + ";");

    if (expr->is_sorted_iteration && element_count > 1) {
        if (!element_type) {
            throw CompileError("Cannot sort array with unknown element type", expr->location);
        }
        std::string copy_var = fresh_temp();
        std::string sort_buffer = fresh_temp();
        std::string cmp_name = ensure_comparator(element_type);
        std::string i_var = fresh_temp();
        std::string j_var = fresh_temp();
        std::string key_var = fresh_temp();

        emit("  " + storage_prefix() + element_c_type + " " + sort_buffer + "[" + size_str + "];");
        emit("  for (int " + copy_var + " = 0; " + copy_var + " < " + size_str + "; " + copy_var + "++) {");
        emit("    " + sort_buffer + "[" + copy_var + "] = " + array_ptr + "[" + copy_var + "];");
        emit("  }");
        emit("  for (int " + i_var + " = 1; " + i_var + " < " + size_str + "; " + i_var + "++) {");
        emit("    " + element_c_type + " " + key_var + " = " + sort_buffer + "[" + i_var + "];");
        emit("    int " + j_var + " = " + i_var + " - 1;");
        emit("    while (" + j_var + " >= 0 && " + cmp_name + "(" + sort_buffer + "[" + j_var + "], " + key_var + ") > 0) {");
        emit("      " + sort_buffer + "[" + j_var + " + 1] = " + sort_buffer + "[" + j_var + "];");
        emit("      " + j_var + " = " + j_var + " - 1;");
        emit("    }");
        emit("    " + sort_buffer + "[" + j_var + " + 1] = " + key_var + ";");
        emit("  }");
        emit("  " + array_ptr + " = " + sort_buffer + ";");
    }

    emit("  for (int " + loop_var + " = 0; " + loop_var + " < " + size_str + "; " + loop_var + "++) {");
    emit("    " + element_c_type + " " + underscore + " = " + array_ptr + "[" + loop_var + "];");

    std::string saved_underscore = underscore_var;
    underscore_var = underscore;

    std::string body_code;
    {
        VoidCallGuard guard(*this, true);
        body_code = gen_expr(expr->right);
    }
    if (!body_code.empty()) {
        emit("    " + body_code + ";");
    }

    underscore_var = saved_underscore;

    emit("  }");
    emit("}");

    return "";
}

std::string CodeGenerator::gen_repeat(ExprPtr expr) {
    std::string cond;
    {
        VoidCallGuard guard(*this, false);
        cond = gen_expr(expr->condition);
    }
    emit("while (" + cond + ") {");
    std::string body_code;
    {
        VoidCallGuard guard(*this, true);
        body_code = gen_expr(expr->right);
    }
    if (!body_code.empty()) {
        emit("  " + body_code + ";");
    }
    emit("}");
    return "";
}


} // namespace vexel::megalinker_codegen
