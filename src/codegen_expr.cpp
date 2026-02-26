#include "codegen.h"
#include "analysis.h"
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

bool is_fixed_primitive_type_codegen(const vexel::TypePtr& type) {
    return type &&
           type->kind == vexel::Type::Kind::Primitive &&
           (vexel::is_signed_fixed(type->primitive) || vexel::is_unsigned_fixed(type->primitive));
}

bool fixed_native_meta_codegen(const vexel::TypePtr& type,
                               uint64_t& total_bits,
                               bool& is_signed_raw,
                               int64_t& fractional_bits) {
    if (!is_fixed_primitive_type_codegen(type)) return false;
    int64_t bits_i64 = vexel::type_bits(type->primitive, type->integer_bits, type->fractional_bits);
    if (!(bits_i64 == 8 || bits_i64 == 16 || bits_i64 == 32 || bits_i64 == 64)) return false;
    total_bits = static_cast<uint64_t>(bits_i64);
    is_signed_raw = (type->primitive == vexel::PrimitiveType::FixedInt);
    fractional_bits = type->fractional_bits;
    return true;
}

bool fixed_muldiv_meta_supported_codegen(const vexel::TypePtr& type,
                                         uint64_t& total_bits,
                                         bool& is_signed_raw,
                                         int64_t& fractional_bits) {
    if (!fixed_native_meta_codegen(type, total_bits, is_signed_raw, fractional_bits)) return false;
    return total_bits == 8 || total_bits == 16 || total_bits == 32;
}

std::string pow2_u64_literal_codegen(uint64_t shift) {
    return std::to_string(1ULL << shift) + "ULL";
}

std::string pow2_i64_literal_codegen(uint64_t shift) {
    return std::to_string(1LL << shift) + "LL";
}

std::string div_pow2_u64_expr_codegen(const std::string& value_u64, uint64_t shift) {
    if (shift >= 64) return "((uint64_t)0)";
    return "((uint64_t)(" + value_u64 + ") >> " + std::to_string(shift) + ")";
}

std::string div_pow2_s64_expr_codegen(const std::string& value_i64, uint64_t shift) {
    if (shift >= 64) return "((int64_t)0)";
    if (shift == 0) return "((int64_t)(" + value_i64 + "))";
    std::string v = "((int64_t)(" + value_i64 + "))";
    std::string mag = "((uint64_t)(-(" + v + " + 1)) + 1ULL)";
    std::string q = "((" + mag + ") >> " + std::to_string(shift) + ")";
    return "((" + v + " < 0) ? -(int64_t)(" + q + ") : (int64_t)(((uint64_t)" + v + ") >> " +
           std::to_string(shift) + "))";
}

std::string mul_pow2_u64_expr_codegen(const std::string& value_u64, uint64_t shift) {
    if (shift >= 64) return "((uint64_t)0)";
    return "((uint64_t)((uint64_t)(" + value_u64 + ") * " + pow2_u64_literal_codegen(shift) + "))";
}

std::string fixed_muldiv_raw_expr_codegen(const vexel::TypePtr& fixed_type,
                                          const std::string& raw_c_type,
                                          const std::string& lhs,
                                          const std::string& rhs,
                                          const std::string& op,
                                          const vexel::SourceLocation& loc) {
    uint64_t bits = 0;
    bool signed_raw = false;
    int64_t frac = 0;
    if (!fixed_muldiv_meta_supported_codegen(fixed_type, bits, signed_raw, frac)) {
        throw vexel::CompileError(
            "Fixed-point operator '" + op +
                "' currently supports only native storage widths up to 32 bits (8/16/32)",
            loc);
    }

    if (signed_raw) {
        std::string l = "((int64_t)((" + raw_c_type + ")" + lhs + "))";
        std::string r = "((int64_t)((" + raw_c_type + ")" + rhs + "))";
        if (op == "*") {
            std::string prod = "((" + l + ") * (" + r + "))";
            if (frac >= 0) {
                return "((" + raw_c_type + ")(" +
                       div_pow2_s64_expr_codegen(prod, static_cast<uint64_t>(frac)) + "))";
            }
            uint64_t k = static_cast<uint64_t>(-frac);
            if (k >= bits) return "((" + raw_c_type + ")0)";
            return "((" + raw_c_type + ")(" +
                   mul_pow2_u64_expr_codegen("(uint64_t)(" + prod + ")", k) + "))";
        }
        if (op == "/") {
            if (frac >= 0) {
                std::string num = "((" + l + ") * " + pow2_i64_literal_codegen(static_cast<uint64_t>(frac)) + ")";
                return "((" + raw_c_type + ")((" + num + ") / (" + r + ")))";
            }
            uint64_t k = static_cast<uint64_t>(-frac);
            if (k >= bits) return "((" + raw_c_type + ")0)";
            std::string den = "((" + r + ") * " + pow2_i64_literal_codegen(k) + ")";
            return "((" + raw_c_type + ")((" + l + ") / (" + den + ")))";
        }
        if (op == "%") {
            std::string q = "((" + raw_c_type + ")((" + l + ") / (" + r + ")))";
            std::string prod_back = "((" + raw_c_type + ")((" + q + ") * ((" + raw_c_type + ")" + rhs + ")))";
            return "((" + raw_c_type + ")((" + raw_c_type + ")" + lhs + " - " + prod_back + "))";
        }
    } else {
        std::string l = "((uint64_t)((" + raw_c_type + ")" + lhs + "))";
        std::string r = "((uint64_t)((" + raw_c_type + ")" + rhs + "))";
        if (op == "*") {
            std::string prod = "((uint64_t)((" + l + ") * (" + r + ")))";
            if (frac >= 0) {
                return "((" + raw_c_type + ")(" +
                       div_pow2_u64_expr_codegen(prod, static_cast<uint64_t>(frac)) + "))";
            }
            uint64_t k = static_cast<uint64_t>(-frac);
            if (k >= bits) return "((" + raw_c_type + ")0)";
            return "((" + raw_c_type + ")(" + mul_pow2_u64_expr_codegen(prod, k) + "))";
        }
        if (op == "/") {
            if (frac >= 0) {
                std::string num = "((uint64_t)((" + l + ") * " + pow2_u64_literal_codegen(static_cast<uint64_t>(frac)) + "))";
                return "((" + raw_c_type + ")((" + num + ") / (" + r + ")))";
            }
            uint64_t k = static_cast<uint64_t>(-frac);
            if (k >= bits) return "((" + raw_c_type + ")0)";
            std::string den = "((uint64_t)((" + r + ") * " + pow2_u64_literal_codegen(k) + "))";
            return "((" + raw_c_type + ")((" + l + ") / (" + den + ")))";
        }
        if (op == "%") {
            std::string q = "((" + raw_c_type + ")((" + l + ") / (" + r + ")))";
            std::string prod_back = "((" + raw_c_type + ")((" + q + ") * ((" + raw_c_type + ")" + rhs + ")))";
            return "((" + raw_c_type + ")((" + raw_c_type + ")" + lhs + " - " + prod_back + "))";
        }
    }

    throw vexel::CompileError("Unsupported fixed-point operator in megalinker codegen: " + op, loc);
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
        if (lookup_constexpr_value(expr, folded)) {
            auto scalar_literal = [&](const CTValue& value) -> std::optional<std::string> {
                return folded_scalar_expr_literal(value, expr ? expr->type : nullptr, expr ? expr->location : SourceLocation());
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
                        auto lit = folded_scalar_expr_literal(array_value->elements[i],
                                                              expr->type->element_type,
                                                              expr->location);
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
            if (expr->has_exact_int_val && is_extended_integer_type(expr->type)) {
                return emit_extint_temp_literal(expr->type, expr->exact_int_val, expr->literal_is_unsigned, expr->location);
            }
            if (expr->has_exact_int_val) {
                if (expr->exact_int_val.fits_i64()) return std::to_string(expr->exact_int_val.to_i64());
                if (expr->exact_int_val.fits_u64()) return std::to_string(expr->exact_int_val.to_u64());
            }
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
                if (sym && sym->is_backend_bound && sym->is_local) {
                    return "(*" + name + "__ptr)";
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

    bool left_extint = is_extended_integer_expr(expr->left);
    bool right_extint = is_extended_integer_expr(expr->right);

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

    if (left_extint || right_extint) {
        return gen_extint_binary(expr, left, right);
    }
    if (expr && expr->type && is_fixed_primitive_type_codegen(expr->type) &&
        (expr->op == "*" || expr->op == "/" || expr->op == "%")) {
        std::string tmp = fresh_temp();
        std::string result_type = c_type_for_expr(expr);
        if (!declared_temps.count(tmp)) {
            emit(storage_prefix() + result_type + " " + tmp + ";");
            declared_temps.insert(tmp);
        }
        emit(tmp + " = " +
             fixed_muldiv_raw_expr_codegen(expr->type, gen_type(expr->type), left, right, expr->op, expr->location) +
             ";");
        return tmp;
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
    if (is_extended_integer_type(expr ? expr->type : nullptr) ||
        is_extended_integer_expr(expr ? expr->operand : nullptr)) {
        return gen_extint_unary(expr, operand);
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

std::string CodeGenerator::gen_inline_call(ExprPtr expr,
                                           Symbol* callee_sym,
                                           StmtPtr callee_decl,
                                           const std::string& ref_key,
                                           char call_reentrancy_key,
                                           const CallTargetInfo& target) {
    if (!callee_decl || !callee_decl->body) {
        return "";
    }

    struct InlineStateGuard {
        CodeGenerator& gen;
        std::unordered_map<std::string, ExprPtr> expr_subs;
        std::unordered_map<std::string, std::string> value_repls;
        std::unordered_set<std::string> ref_params;
        std::unordered_set<std::string> agg_params;
        std::string variant_id;
        char reent_key;
        std::string func_key;
        const Symbol* func_symbol;
        int instance_id;
        char bank_page;
        std::string module_id_expr;

        explicit InlineStateGuard(CodeGenerator& g)
            : gen(g),
              expr_subs(g.expr_param_substitutions),
              value_repls(g.value_param_replacements),
              ref_params(g.current_ref_params),
              agg_params(g.current_aggregate_params),
              variant_id(g.current_variant_id),
              reent_key(g.current_reentrancy_key),
              func_key(g.current_func_key),
              func_symbol(g.current_func_symbol),
              instance_id(g.current_instance_id),
              bank_page(g.current_bank_page),
              module_id_expr(g.current_module_id_expr) {}

        ~InlineStateGuard() {
            gen.expr_param_substitutions = std::move(expr_subs);
            gen.value_param_replacements = std::move(value_repls);
            gen.current_ref_params = std::move(ref_params);
            gen.current_aggregate_params = std::move(agg_params);
            gen.current_variant_id = std::move(variant_id);
            gen.current_reentrancy_key = reent_key;
            gen.current_func_key = std::move(func_key);
            gen.current_func_symbol = func_symbol;
            gen.current_instance_id = instance_id;
            gen.current_bank_page = bank_page;
            gen.current_module_id_expr = std::move(module_id_expr);
        }
    } guard(*this);

    expr_param_substitutions.clear();
    value_param_replacements.clear();
    current_ref_params.clear();
    current_aggregate_params.clear();
    current_reentrancy_key = call_reentrancy_key;
    if (callee_sym) {
        current_func_key = func_key_for(callee_sym);
        current_func_symbol = callee_sym;
        current_instance_id = callee_sym->instance_id;
    }
    if (!target.inline_variant_id.empty()) {
        current_variant_id = target.inline_variant_id;
    }
    current_bank_page = target.page;

    auto fresh_inline_slot = [&]() -> std::string {
        return "__vx_inl_" + std::to_string(temp_counter++);
    };

    // Bind receiver parameters.
    for (size_t i = 0; i < callee_decl->ref_params.size(); ++i) {
        if (i >= expr->receivers.size()) break;
        ExprPtr rec = expr->receivers[i];
        const std::string& param_name = callee_decl->ref_params[i];
        TypePtr rec_type = (i < callee_decl->ref_param_types.size()) ? callee_decl->ref_param_types[i] : rec->type;

        bool by_ref = receiver_is_mutable_arg(rec);
        if (!ref_key.empty() && i < ref_key.size()) {
            by_ref = ref_key[i] == 'M';
        }
        if (abi.lower_aggregates && rec_type && is_aggregate_type(rec_type)) {
            by_ref = true;
        }

        std::string rec_expr;
        {
            VoidCallGuard rec_guard(*this, false);
            rec_expr = gen_expr(rec);
        }

        if (by_ref) {
            std::string ptr_expr;
            if (is_addressable_lvalue(rec) && is_mutable_lvalue(rec)) {
                if (!rec || !rec->type || rec->type->kind == Type::Kind::TypeVar) {
                    ptr_expr = "&" + rec_expr;
                } else {
                    std::string ptr_temp = fresh_inline_slot();
                    std::string rtype = gen_type(rec->type);
                    emit(storage_prefix() + rtype + "* " + ptr_temp + " = &" + rec_expr + ";");
                    ptr_expr = ptr_temp;
                }
            } else {
                if (!rec || !rec->type) {
                    throw CompileError("Missing receiver type for inline call",
                                       rec ? rec->location : expr->location);
                }
                std::string val_temp = fresh_inline_slot();
                std::string rtype = gen_type(rec->type);
                emit(storage_prefix() + rtype + " " + val_temp + " = " + rec_expr + ";");
                ptr_expr = "&" + val_temp;
            }

            std::string slot = fresh_inline_slot();
            std::string slot_type = rec_type ? gen_type(rec_type) : "void";
            if (slot_type == "void") slot_type = "void*";
            emit(storage_prefix() + slot_type + "* " + slot + " = " + ptr_expr + ";");
            value_param_replacements[param_name] = "(*" + slot + ")";
        } else {
            if (!rec || !rec->type || rec->type->kind == Type::Kind::TypeVar) {
                value_param_replacements[param_name] = rec_expr;
            } else {
                std::string slot = fresh_inline_slot();
                std::string slot_type = c_type_for_expr(rec);
                emit(storage_prefix() + slot_type + " " + slot + " = " + rec_expr + ";");
                value_param_replacements[param_name] = slot;
            }
        }
    }

    // Bind regular parameters.
    size_t arg_idx = 0;
    for (size_t i = 0; i < callee_decl->params.size(); ++i) {
        const auto& param = callee_decl->params[i];
        ExprPtr arg_expr_node = (arg_idx < expr->args.size()) ? expr->args[arg_idx] : nullptr;
        arg_idx++;

        if (param.is_expression_param) {
            if (arg_expr_node) {
                expr_param_substitutions[param.name] = arg_expr_node;
            }
            continue;
        }

        bool by_ref = abi.lower_aggregates && param.type && is_aggregate_type(param.type);

        std::string arg_expr;
        {
            VoidCallGuard arg_guard(*this, false);
            arg_expr = arg_expr_node ? gen_expr(arg_expr_node) : std::string();
        }

        if (by_ref) {
            std::string ptr_expr;
            if (arg_expr_node && is_addressable_lvalue(arg_expr_node)) {
                if (!arg_expr_node->type || arg_expr_node->type->kind == Type::Kind::TypeVar) {
                    ptr_expr = "&" + arg_expr;
                } else {
                    std::string ptr_temp = fresh_inline_slot();
                    std::string atype = gen_type(arg_expr_node->type);
                    emit(storage_prefix() + atype + "* " + ptr_temp + " = &" + arg_expr + ";");
                    ptr_expr = ptr_temp;
                }
            } else {
                if (!arg_expr_node || !arg_expr_node->type) {
                    throw CompileError("Missing argument type for inline call",
                                       arg_expr_node ? arg_expr_node->location : expr->location);
                }
                std::string val_temp = fresh_inline_slot();
                std::string atype = gen_type(arg_expr_node->type);
                emit(storage_prefix() + atype + " " + val_temp + " = " + arg_expr + ";");
                ptr_expr = "&" + val_temp;
            }

            std::string slot = fresh_inline_slot();
            std::string slot_type = param.type ? gen_type(param.type) : "void";
            if (slot_type == "void") slot_type = "void*";
            emit(storage_prefix() + slot_type + "* " + slot + " = " + ptr_expr + ";");
            value_param_replacements[param.name] = "(*" + slot + ")";
        } else {
            if (arg_expr_node && arg_expr_node->type && arg_expr_node->type->kind != Type::Kind::TypeVar) {
                std::string slot = fresh_inline_slot();
                std::string slot_type = c_type_for_expr(arg_expr_node);
                emit(storage_prefix() + slot_type + " " + slot + " = " + arg_expr + ";");
                value_param_replacements[param.name] = slot;
            } else {
                value_param_replacements[param.name] = arg_expr;
            }
        }
    }

    std::string result;
    if (allow_void_call) {
        VoidCallGuard body_guard(*this, true);
        result = gen_expr(callee_decl->body);
        if (!result.empty()) {
            emit(result + ";");
        }
        return "";
    }

    {
        VoidCallGuard body_guard(*this, false);
        result = gen_expr(callee_decl->body);
    }
    return result;
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
                std::string alias_lookup_name = base_name;
                if (sym && callee_decl && is_bundled_std_math_function(sym, callee_decl)) {
                    alias_lookup_name = "std::math::" + callee_decl->func_name;
                }
                func_name = external_link_name(alias_lookup_name, mangle_name(base_name));
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

    if (target.inline_body && callee_decl && !is_external) {
        return gen_inline_call(expr, sym, callee_decl, ref_key, call_reentrancy_key, target);
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

std::string CodeGenerator::gen_array_initializer(ExprPtr expr) {
    if (!expr || expr->kind != Expr::Kind::ArrayLiteral) {
        throw CompileError("Expected array literal for initializer", expr ? expr->location : SourceLocation());
    }
    std::string init = "{";
    for (size_t i = 0; i < expr->elements.size(); i++) {
        if (i > 0) init += ", ";
        ExprPtr elem = expr->elements[i];
        if (elem && elem->kind == Expr::Kind::ArrayLiteral) {
            init += gen_array_initializer(elem);
            continue;
        }
        VoidCallGuard guard(*this, false);
        init += gen_expr(elem);
    }
    init += "}";
    return init;
}

void CodeGenerator::emit_array_literal_assignments(const std::string& target, ExprPtr expr, TypePtr array_type) {
    if (!expr || expr->kind != Expr::Kind::ArrayLiteral) {
        throw CompileError("Expected array literal for element-wise initialization",
                           expr ? expr->location : SourceLocation());
    }
    if (!array_type || array_type->kind != Type::Kind::Array) {
        throw CompileError("Missing array type for element-wise initialization",
                           expr ? expr->location : SourceLocation());
    }

    TypePtr elem_type = array_type->element_type;
    for (size_t i = 0; i < expr->elements.size(); ++i) {
        ExprPtr elem = expr->elements[i];
        std::string slot = target + "[" + std::to_string(i) + "]";
        if (elem && elem->kind == Expr::Kind::ArrayLiteral) {
            emit_array_literal_assignments(slot, elem, elem_type);
            continue;
        }
        std::string value;
        {
            VoidCallGuard guard(*this, false);
            value = gen_expr(elem);
        }
        emit(slot + " = " + value + ";");
    }
}

std::string CodeGenerator::gen_array_literal(ExprPtr expr) {
    std::string temp = fresh_temp();

    // Determine element type
    if (!expr->type || expr->type->kind != Type::Kind::Array || !expr->type->element_type) {
        throw CompileError("Missing array element type for array literal", expr->location);
    }
    // Reentrant functions can use block-scope aggregate initialization directly.
    // Non-reentrant functions use static-frame locals, so initialize element-wise at runtime
    // to avoid invalid non-constant static initializers and preserve fresh-value semantics.
    std::string array_decl = gen_object_decl(expr->type,
                                             temp,
                                             expr->location,
                                             "array literal temporary");
    if (storage_prefix().empty()) {
        emit(array_decl + " = " + gen_array_initializer(expr) + ";");
    } else {
        emit(storage_prefix() + array_decl + ";");
        emit_array_literal_assignments(temp, expr, expr->type);
    }

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

std::string CodeGenerator::gen_conditional(ExprPtr expr) {
    // Try to evaluate condition at compile time for dead branch elimination
    {
        bool is_true = false;
        if (constexpr_condition(expr->condition, is_true)) {
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
    if (is_extended_integer_type(expr ? expr->type : nullptr)) {
        return gen_extint_conditional(expr, cond, true_expr, false_expr);
    }
    return "(" + cond + " ? " + true_expr + " : " + false_expr + ")";
}

std::string CodeGenerator::gen_cast(ExprPtr expr) {
    if ((expr && expr->target_type && is_extended_integer_type(expr->target_type)) ||
        (expr && expr->operand && expr->operand->type && is_extended_integer_type(expr->operand->type))) {
        std::string operand;
        {
            VoidCallGuard guard(*this, false);
            operand = gen_expr(expr->operand);
        }
        return gen_extint_cast(expr, operand);
    }

    // Primitive to byte array conversion (big-endian order)
    if (expr->target_type && expr->target_type->kind == Type::Kind::Array &&
        expr->target_type->element_type &&
        expr->target_type->element_type->kind == Type::Kind::Primitive &&
        expr->target_type->element_type->primitive == PrimitiveType::UInt &&
        expr->target_type->element_type->integer_bits == 8 &&
        expr->operand && expr->operand->type &&
        expr->operand->type->kind == Type::Kind::Primitive &&
        !is_float(expr->operand->type->primitive)) {

        int64_t length = resolve_array_length(expr->target_type, expr->location);
        int64_t bits = type_bits(expr->operand->type->primitive,
                                 expr->operand->type->integer_bits,
                                 expr->operand->type->fractional_bits);
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

    auto is_fixed_primitive_type_local = [&](TypePtr type) {
        return type &&
               type->kind == Type::Kind::Primitive &&
               (is_signed_fixed(type->primitive) || is_unsigned_fixed(type->primitive));
    };
    auto fixed_native_meta = [&](TypePtr type, uint64_t& total_bits, bool& is_signed_raw, int64_t& frac_bits) {
        if (!is_fixed_primitive_type_local(type)) return false;
        int64_t bits_i64 = type_bits(type->primitive, type->integer_bits, type->fractional_bits);
        if (!(bits_i64 == 8 || bits_i64 == 16 || bits_i64 == 32 || bits_i64 == 64)) return false;
        total_bits = static_cast<uint64_t>(bits_i64);
        is_signed_raw = (type->primitive == PrimitiveType::FixedInt);
        frac_bits = type->fractional_bits;
        return true;
    };
    auto primitive_intlike_meta = [&](TypePtr type, bool& is_signed, uint64_t& bits) {
        if (!type || type->kind != Type::Kind::Primitive) return false;
        if (is_signed_int(type->primitive)) {
            if (type->integer_bits == 0) return false;
            is_signed = true;
            bits = type->integer_bits;
            return true;
        }
        if (is_unsigned_int(type->primitive)) {
            if (type->integer_bits == 0) return false;
            is_signed = false;
            bits = type->integer_bits;
            return true;
        }
        if (type->primitive == PrimitiveType::Bool) {
            is_signed = false;
            bits = 1;
            return true;
        }
        return false;
    };
    auto declare_temp = [&](TypePtr type) {
        std::string name = fresh_temp();
        if (!declared_temps.count(name)) {
            emit(storage_prefix() + gen_type(type) + " " + name + ";");
            declared_temps.insert(name);
        }
        return name;
    };
    auto pow2_u64_literal = [&](uint64_t shift) {
        return std::to_string((1ULL << shift)) + "ULL";
    };
    auto div_pow2_u64_expr = [&](const std::string& value_u64, uint64_t shift) {
        if (shift >= 64) return std::string("((uint64_t)0)");
        return "((uint64_t)(" + value_u64 + ") >> " + std::to_string(shift) + ")";
    };
    auto div_pow2_s64_expr = [&](const std::string& value_i64, uint64_t shift) {
        if (shift >= 64) return std::string("((int64_t)0)");
        if (shift == 0) return "((int64_t)(" + value_i64 + "))";
        std::string v = "((int64_t)(" + value_i64 + "))";
        std::string mag = "((uint64_t)(-(" + v + " + 1)) + 1ULL)";
        std::string q = "((" + mag + ") >> " + std::to_string(shift) + ")";
        return "((" + v + " < 0) ? -(int64_t)(" + q + ") : (int64_t)(((uint64_t)" + v + ") >> " +
               std::to_string(shift) + "))";
    };
    auto mul_pow2_u64_wrap_expr = [&](const std::string& value_u64, uint64_t shift) {
        if (shift >= 64) return std::string("((uint64_t)0)");
        return "((uint64_t)((uint64_t)(" + value_u64 + ") * " + pow2_u64_literal(shift) + "))";
    };

    TypePtr source_type = expr && expr->operand ? expr->operand->type : nullptr;
    const bool source_is_fixed = is_fixed_primitive_type_local(source_type);
    const bool target_is_fixed = is_fixed_primitive_type_local(expr ? expr->target_type : nullptr);
    if (source_is_fixed || target_is_fixed) {
        if (!source_type || source_type->kind != Type::Kind::Primitive ||
            !expr->target_type || expr->target_type->kind != Type::Kind::Primitive) {
            throw CompileError("Fixed-point casts currently support only primitive numeric/bool casts",
                               expr ? expr->location : SourceLocation());
        }

        uint64_t src_fixed_bits = 0;
        bool src_fixed_signed = false;
        int64_t src_frac = 0;
        if (source_is_fixed &&
            !fixed_native_meta(source_type, src_fixed_bits, src_fixed_signed, src_frac)) {
            throw CompileError("Fixed-point casts currently support only native storage widths (8/16/32/64)",
                               expr->location);
        }
        uint64_t dst_fixed_bits = 0;
        bool dst_fixed_signed = false;
        int64_t dst_frac = 0;
        if (target_is_fixed &&
            !fixed_native_meta(expr->target_type, dst_fixed_bits, dst_fixed_signed, dst_frac)) {
            throw CompileError("Fixed-point casts currently support only native storage widths (8/16/32/64)",
                               expr->location);
        }
        (void)dst_fixed_bits;
        (void)dst_fixed_signed;

        std::string src_tmp = declare_temp(source_type);
        emit(src_tmp + " = " + operand + ";");

        auto scaled_i64_or_u64 = [&](bool src_signed, int64_t scale_shift) -> std::pair<std::string, bool> {
            if (src_signed) {
                std::string base_i64 = "((int64_t)" + src_tmp + ")";
                if (scale_shift > 0) {
                    return {mul_pow2_u64_wrap_expr("(uint64_t)" + base_i64,
                                                   static_cast<uint64_t>(scale_shift)),
                            false};
                }
                if (scale_shift < 0) {
                    return {"(uint64_t)(" +
                                div_pow2_s64_expr(base_i64, static_cast<uint64_t>(-scale_shift)) + ")",
                            false};
                }
                return {"(uint64_t)(" + base_i64 + ")", false};
            }

            std::string base_u64 = "((uint64_t)" + src_tmp + ")";
            if (scale_shift > 0) {
                return {mul_pow2_u64_wrap_expr(base_u64, static_cast<uint64_t>(scale_shift)), true};
            }
            if (scale_shift < 0) {
                return {div_pow2_u64_expr(base_u64, static_cast<uint64_t>(-scale_shift)), true};
            }
            return {base_u64, true};
        };

        if (target_is_fixed) {
            if (is_float(source_type->primitive)) {
                std::string out_tmp = declare_temp(expr->target_type);
                emit(out_tmp + " = (" + target + ")trunc(ldexp((double)" + src_tmp + ", " +
                     std::to_string(dst_frac) + "));");
                return out_tmp;
            }
            bool src_signed = false;
            uint64_t src_bits = 0;
            if (source_is_fixed) {
                src_signed = src_fixed_signed;
                src_bits = src_fixed_bits;
                (void)src_bits;
            } else {
                if (!primitive_intlike_meta(source_type, src_signed, src_bits)) {
                    throw CompileError("Fixed-point casts currently support only primitive numeric/bool casts",
                                       expr->location);
                }
                (void)src_bits;
            }
            int64_t scale_shift = source_is_fixed ? (dst_frac - src_frac) : dst_frac;
            auto scaled = scaled_i64_or_u64(src_signed, scale_shift);
            std::string out_tmp = declare_temp(expr->target_type);
            emit(out_tmp + " = (" + target + ")(" + scaled.first + ");");
            return out_tmp;
        }

        if (expr->target_type->primitive == PrimitiveType::Bool) {
            std::string out_tmp = declare_temp(expr->target_type);
            emit(out_tmp + " = (_Bool)(" + src_tmp + " != 0);");
            return out_tmp;
        }

        if (is_float(expr->target_type->primitive)) {
            std::string out_tmp = declare_temp(expr->target_type);
            std::string raw_as_double = source_is_fixed && src_fixed_signed
                ? "((double)(int64_t)" + src_tmp + ")"
                : "((double)(uint64_t)" + src_tmp + ")";
            emit(out_tmp + " = (" + target + ")ldexp(" + raw_as_double + ", " +
                 std::to_string(-src_frac) + ");");
            return out_tmp;
        }

        bool target_is_intlike = is_signed_int(expr->target_type->primitive) ||
                                 is_unsigned_int(expr->target_type->primitive);
        if (!target_is_intlike) {
            throw CompileError("Fixed-point casts currently support only primitive numeric/bool casts",
                               expr->location);
        }

        bool src_signed = source_is_fixed ? src_fixed_signed : false;
        uint64_t src_bits = source_is_fixed ? src_fixed_bits : 0;
        (void)src_bits;
        if (!source_is_fixed) {
            if (!primitive_intlike_meta(source_type, src_signed, src_bits)) {
                throw CompileError("Fixed-point casts currently support only primitive numeric/bool casts",
                                   expr->location);
            }
        }

        int64_t scale_shift = source_is_fixed ? (-src_frac) : 0;
        auto scaled = scaled_i64_or_u64(src_signed, scale_shift);
        std::string out_tmp = declare_temp(expr->target_type);
        emit(out_tmp + " = (" + target + ")(" + scaled.first + ");");
        return out_tmp;
    }

    return "((" + target + ")" + operand + ")";
}

std::string CodeGenerator::gen_assignment(ExprPtr expr) {
    const std::string assign_op = expr->op.empty() ? "=" : expr->op;
    // Use the flag set by the typechecker to determine if this creates a new variable
    if (expr->creates_new_variable) {
        if (assign_op != "=") {
            throw CompileError("Internal error: compound assignment cannot declare a new variable", expr->location);
        }
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
            emit(gen_object_decl(var_type,
                                 var_name,
                                 expr->location,
                                 "array declaration '" + expr->left->name + "'") +
                 " = " + gen_array_initializer(expr->right) + ";");

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
            emit(gen_object_decl(var_type,
                                 var_name,
                                 expr->location,
                                 "array declaration '" + expr->left->name + "'") + ";");

            std::string rhs;
            {
                VoidCallGuard guard(*this, false);
                rhs = gen_expr(expr->right);
            }
            emit("memcpy(" + var_name + ", " + rhs + ", sizeof(" + var_name + "));");

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

    TypePtr lhs_type = expr->left ? expr->left->type : nullptr;
    if ((!lhs_type || lhs_type->kind != Type::Kind::Array) &&
        expr->left && expr->left->kind == Expr::Kind::Identifier) {
        if (Symbol* lhs_sym = binding_for(expr->left)) {
            if (lhs_sym->type && lhs_sym->type->kind == Type::Kind::Array) {
                lhs_type = lhs_sym->type;
            }
        }
    }

    if (assign_op == "&&=" || assign_op == "||=") {
        if (!lhs_type || lhs_type->kind != Type::Kind::Primitive ||
            lhs_type->primitive != PrimitiveType::Bool) {
            throw CompileError("Internal error: logical compound assignment requires boolean lhs", expr->location);
        }
        std::string rhs;
        std::ostringstream rhs_capture;
        output_stack.push(&rhs_capture);
        try {
            VoidCallGuard guard(*this, false);
            rhs = gen_expr(expr->right);
        } catch (...) {
            output_stack.pop();
            throw;
        }
        output_stack.pop();

        auto append_captured = [&](const std::string& code) {
            if (code.empty()) return;
            if (output_stack.empty()) {
                output_stack.push(&body);
            }
            (*output_stack.top()) << code;
        };

        const std::string lhs_type_str = gen_type(lhs_type);
        std::string ptr_tmp = fresh_temp();
        if (!declared_temps.count(ptr_tmp)) {
            emit(storage_prefix() + lhs_type_str + "* " + ptr_tmp + " = &(" + lhs + ");");
            declared_temps.insert(ptr_tmp);
        } else {
            emit(ptr_tmp + " = &(" + lhs + ");");
        }

        emit(std::string("if (*") + ptr_tmp + ") {");
        if (assign_op == "&&=") {
            append_captured(rhs_capture.str());
            emit(std::string("*") + ptr_tmp + " = " + rhs + ";");
        } else {
            emit(std::string("*") + ptr_tmp + " = 1;");
        }
        emit("} else {");
        if (assign_op == "||=") {
            append_captured(rhs_capture.str());
            emit(std::string("*") + ptr_tmp + " = " + rhs + ";");
        } else {
            emit(std::string("*") + ptr_tmp + " = 0;");
        }
        emit("}");

        std::string result_tmp = fresh_temp();
        if (!declared_temps.count(result_tmp)) {
            emit(storage_prefix() + lhs_type_str + " " + result_tmp + ";");
            declared_temps.insert(result_tmp);
        }
        emit(result_tmp + " = *" + ptr_tmp + ";");
        return result_tmp;
    }

    {
        VoidCallGuard guard(*this, false);
        rhs = gen_expr(expr->right);
    }
    if (is_extended_integer_type(lhs_type)) {
        std::string result = gen_extint_assignment(expr, lhs_type, lhs, rhs, assign_op);
        if (rhs.rfind("tmp", 0) == 0 &&
            (!expr->right || !expr->right->type || expr->right->type->kind != Type::Kind::Array)) {
            release_temp(rhs);
        }
        return result;
    }
    if (lhs_type && is_fixed_primitive_type_codegen(lhs_type) &&
        (assign_op == "*=" || assign_op == "/=" || assign_op == "%=")) {
        std::string lhs_type_str = gen_type(lhs_type);
        std::string ptr_tmp = fresh_temp();
        if (!declared_temps.count(ptr_tmp)) {
            emit(storage_prefix() + lhs_type_str + "* " + ptr_tmp + " = &(" + lhs + ");");
            declared_temps.insert(ptr_tmp);
        } else {
            emit(ptr_tmp + " = &(" + lhs + ");");
        }
        std::string lhs_deref = std::string("*") + ptr_tmp;
        emit(lhs_deref + " = " +
             fixed_muldiv_raw_expr_codegen(lhs_type, lhs_type_str, lhs_deref, rhs,
                                           assign_op.substr(0, assign_op.size() - 1),
                                           expr->location) +
             ";");
        std::string result_tmp = fresh_temp();
        if (!declared_temps.count(result_tmp)) {
            emit(storage_prefix() + lhs_type_str + " " + result_tmp + ";");
            declared_temps.insert(result_tmp);
        }
        emit(result_tmp + " = " + lhs_deref + ";");
        if (rhs.rfind("tmp", 0) == 0 &&
            (!expr->right || !expr->right->type || expr->right->type->kind != Type::Kind::Array)) {
            release_temp(rhs);
        }
        return result_tmp;
    }
    // Release RHS temp if it's a temporary
    if (rhs.rfind("tmp", 0) == 0 &&
        (!expr->right || !expr->right->type || expr->right->type->kind != Type::Kind::Array)) {
        release_temp(rhs);
    }

    if (lhs_type && lhs_type->kind == Type::Kind::Array) {
        emit("memcpy(" + lhs + ", " + rhs + ", sizeof(" + lhs + "));");
        std::string temp = fresh_temp();
        if (!declared_temps.count(temp)) {
            emit(storage_prefix() + std::string("int ") + temp + " = 0;");
            declared_temps.insert(temp);
        } else {
            emit(temp + " = 0;");
        }
        return temp;
    }

    return "(" + lhs + " " + assign_op + " " + rhs + ")";
}

std::optional<std::pair<int64_t, int64_t>> CodeGenerator::evaluate_range(ExprPtr range_expr) {
    if (!range_expr || range_expr->kind != Expr::Kind::Range) {
        return std::nullopt;
    }

    CTValue start_val, end_val;
    if (lookup_constexpr_value(range_expr->left, start_val) &&
        lookup_constexpr_value(range_expr->right, end_val)) {
    auto to_i64 = [](const CTValue& v, int64_t& out) -> bool {
            return ctvalue_to_i64_exact(v, out);
        };
        int64_t start = 0;
        int64_t end = 0;
        if (!to_i64(start_val, start) || !to_i64(end_val, end)) {
            return std::nullopt;
        }
        return std::make_pair(start, end);
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
    bool elem_extint = is_extended_integer_type(expr->type->element_type);
    if (start < end) {
        for (int64_t i = start; i < end; ++i) {
            if (!first) init << ", ";
            first = false;
            if (elem_extint) {
                init << emit_extint_const_initializer(expr->type->element_type,
                                                      APInt(i),
                                                      i >= 0,
                                                      expr->location);
            } else {
                init << i;
            }
        }
    } else {
        for (int64_t i = start; i > end; --i) {
            if (!first) init << ", ";
            first = false;
            if (elem_extint) {
                init << emit_extint_const_initializer(expr->type->element_type,
                                                      APInt(i),
                                                      i >= 0,
                                                      expr->location);
            } else {
                init << i;
            }
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
        } else if (is_extended_integer_type(expr->operand->type)) {
            bool is_signed = false;
            uint64_t bits = 0;
            analyze_extint_type(expr->operand->type, is_signed, bits);
            if (!is_signed) return operand;
            std::string temp = fresh_temp();
            std::string t = gen_type(expr->operand->type);
            if (!declared_temps.count(temp)) {
                emit(storage_prefix() + t + " " + temp + ";");
                declared_temps.insert(temp);
            }
            emit(temp + " = " + operand + ";");
            emit("if (vx_ai_signbit(" + temp + ".b, sizeof(" + temp + ".b), " + std::to_string(extint_sign_mask(bits)) + ")) {");
            emit("  vx_ai_neg(" + temp + ".b, " + temp + ".b, sizeof(" + temp + ".b), " + std::to_string(extint_top_mask(bits)) + ");");
            emit("}");
            return temp;
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
        int64_t out = 0;
        if (ctvalue_to_i64_exact(v, out)) return out;
        throw CompileError("Iteration bounds must be integer constants", loc);
    };

    if (expr->operand->kind == Expr::Kind::Range) {
        CTValue start_val, end_val;
        if (!lookup_constexpr_value(expr->operand->left, start_val) ||
            !lookup_constexpr_value(expr->operand->right, end_val)) {
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
