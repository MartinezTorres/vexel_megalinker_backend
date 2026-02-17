#pragma once
#include "analysis.h"
#include "ast.h"
#include <string>

namespace vexel::megalinker_semantics {

inline bool is_pointer_like_type(TypePtr type) {
    if (!type) return false;
    if (type->kind == Type::Kind::Array) return true;
    if (type->kind == Type::Kind::Primitive && type->primitive == PrimitiveType::String) return true;
    return false;
}

inline bool is_addressable_lvalue_expr(ExprPtr expr) {
    if (!expr) return false;
    switch (expr->kind) {
        case Expr::Kind::Identifier:
            return true;
        case Expr::Kind::Member:
        case Expr::Kind::Index:
            return is_addressable_lvalue_expr(expr->operand);
        default:
            return false;
    }
}

inline bool is_mutable_lvalue_expr(ExprPtr expr) {
    if (!expr) return false;
    switch (expr->kind) {
        case Expr::Kind::Identifier:
            return expr->is_mutable_binding;
        case Expr::Kind::Member:
        case Expr::Kind::Index:
            return is_mutable_lvalue_expr(expr->operand);
        default:
            return false;
    }
}

inline std::string ref_variant_key_for_call(const ExprPtr& call, size_t ref_count) {
    std::string key;
    key.reserve(ref_count);
    for (size_t i = 0; i < ref_count; ++i) {
        bool is_mut = false;
        if (call && i < call->receivers.size()) {
            is_mut =
                is_addressable_lvalue_expr(call->receivers[i]) &&
                is_mutable_lvalue_expr(call->receivers[i]);
        }
        key.push_back(is_mut ? 'M' : 'N');
    }
    return key;
}

inline std::string mutability_prefix(const AnalysisFacts& facts, const Symbol* sym, const StmtPtr& stmt) {
    if (!stmt) return "";
    auto it = sym ? facts.var_mutability.find(sym) : facts.var_mutability.end();
    VarMutability kind = stmt->is_mutable ? VarMutability::Mutable : VarMutability::Constexpr;
    if (it != facts.var_mutability.end()) {
        kind = it->second;
    }
    switch (kind) {
        case VarMutability::Mutable:
            return "VX_MUTABLE ";
        case VarMutability::Constexpr:
            return "VX_CONSTEXPR ";
        default:
            return "";
    }
}

} // namespace vexel::megalinker_semantics
