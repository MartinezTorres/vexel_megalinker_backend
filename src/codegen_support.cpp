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
#include <cstdlib>
#include <limits>
#include <tuple>
#include <sstream>
#include <deque>

namespace {

std::string sanitize_identifier(const std::string& input) {
    std::string result;
    result.reserve(input.size());
    for (unsigned char c : input) {
        if (std::isalnum(c)) {
            result.push_back(static_cast<char>(c));
        } else {
            result.push_back('_');
        }
    }
    if (result.empty() || std::isdigit(static_cast<unsigned char>(result.front()))) {
        result.insert(result.begin(), '_');
    }
    return result;
}

bool is_std_math_libc_name(const std::string& name) {
    static const std::unordered_set<std::string> kNames = {
        "sin", "cos", "tan", "asin", "acos", "atan", "exp", "log", "log2", "log10",
        "floor", "ceil", "trunc", "round", "fabs", "sqrt",
        "pow", "atan2", "fmod",
        "sinf", "cosf", "tanf", "asinf", "acosf", "atanf", "expf", "logf", "log2f", "log10f",
        "floorf", "ceilf", "truncf", "roundf", "fabsf", "sqrtf",
        "powf", "atan2f", "fmodf"
    };
    return kNames.count(name) > 0;
}

} // namespace

namespace vexel::megalinker_codegen {
std::string CodeGenerator::require_type(TypePtr type, const SourceLocation& loc, const std::string& context) {
    if (!type) {
        throw CompileError("Missing type during code generation: " + context, loc);
    }
    return gen_type(type);
}

std::string CodeGenerator::gen_type(TypePtr type) {
    if (!type) return "void";

    switch (type->kind) {
        case Type::Kind::Primitive:
            switch (type->primitive) {
                case PrimitiveType::Int:
                    switch (type->integer_bits) {
                        case 8: return "int8_t";
                        case 16: return "int16_t";
                        case 32: return "int32_t";
                        case 64: return "int64_t";
                        default:
                            ensure_extint_type(true, type->integer_bits);
                            return extint_type_name(true, type->integer_bits);
                    }
                case PrimitiveType::UInt:
                    switch (type->integer_bits) {
                        case 8: return "uint8_t";
                        case 16: return "uint16_t";
                        case 32: return "uint32_t";
                        case 64: return "uint64_t";
                        default:
                            ensure_extint_type(false, type->integer_bits);
                            return extint_type_name(false, type->integer_bits);
                    }
                case PrimitiveType::F16: return "_Float16";
                case PrimitiveType::F32: return "float";
                case PrimitiveType::F64: return "double";
                case PrimitiveType::Bool: return "_Bool";
                case PrimitiveType::String: return "const char*";
            }
            break;

        case Type::Kind::Array: {
            std::string elem = gen_type(type->element_type);
            // Simplified: size would need compile-time evaluation
            return elem + "*";
        }

        case Type::Kind::Named:
            if (type_map.count(type->type_name)) {
                return type_map[type->type_name];
            }
            return mangle_name(type->type_name);

        case Type::Kind::TypeVar:
            throw CompileError("Type could not be inferred; add an explicit type annotation", type->location);
        case Type::Kind::TypeOf:
            throw CompileError("Internal error: unresolved #[...] reached megalinker backend", type->location);
    }

    return "void";
}

std::string CodeGenerator::gen_object_decl(TypePtr type,
                                           const std::string& name,
                                           const SourceLocation& loc,
                                           const std::string& context) {
    if (!type) {
        throw CompileError("Missing type during code generation: " + context, loc);
    }
    TypePtr base = type;
    while (base && base->kind == Type::Kind::Array) {
        base = base->element_type;
    }
    std::string decl = require_type(base, loc, context + " base type");
    if (!name.empty()) {
        decl += " " + name;
    }
    append_array_suffix(type, loc, context, decl);
    return decl;
}

void CodeGenerator::append_array_suffix(TypePtr type,
                                        const SourceLocation& loc,
                                        const std::string& context,
                                        std::string& out) {
    if (!type) {
        throw CompileError("Missing type during code generation: " + context, loc);
    }
    TypePtr dim = type;
    while (dim && dim->kind == Type::Kind::Array) {
        int64_t len = resolve_array_length(dim, loc);
        out += "[" + std::to_string(len) + "]";
        dim = dim->element_type;
    }
}

std::string CodeGenerator::mangle_name(const std::string& name) {
    return mangle_name_with_prefix(name, internal_symbol_prefix);
}

std::string CodeGenerator::mangle_export_name(const std::string& name) {
    return mangle_name_with_prefix(name, "vx_");
}

std::string CodeGenerator::mangle_name_with_prefix(const std::string& name,
                                                   const std::string& prefix) const {
    // Special case: main function doesn't get mangled
    if (name == "main") {
        return "main";
    }

    std::string result = prefix.empty() ? "vx_" : prefix;
    auto append_encoded = [&](unsigned char c) {
        if (std::isalnum(c) || c == '_') {
            result.push_back(static_cast<char>(c));
        } else {
            std::ostringstream oss;
            oss << "_" << std::uppercase << std::hex << std::setw(2) << std::setfill('0')
                << static_cast<int>(c);
            result += oss.str();
        }
    };

    // Replace :: with __ for method names
    for (size_t i = 0; i < name.size(); i++) {
        if (i + 1 < name.size() && name[i] == ':' && name[i+1] == ':') {
            result += MANGLED_PREFIX;
            i++; // Skip the second :
        } else {
            append_encoded(static_cast<unsigned char>(name[i]));
        }
    }
    return result;
}

std::string CodeGenerator::fresh_temp() {
    if (!available_temps.empty()) {
        std::string temp = available_temps.top();
        available_temps.pop();
        live_temps.insert(temp);
        return temp;
    }
    std::string temp = "tmp" + std::to_string(temp_counter++);
    live_temps.insert(temp);
    return temp;
}

void CodeGenerator::release_temp(const std::string& temp) {
    (void)temp;
    // Temp names are currently type-erased. Avoid invalid scalar reinit on reused struct temps.
}

void CodeGenerator::emit(const std::string& code) {
    if (output_stack.empty()) {
        output_stack.push(&body);
    }
    (*output_stack.top()) << code << "\n";
}

void CodeGenerator::emit_header(const std::string& code) {
    header << code << "\n";
}

std::string CodeGenerator::storage_prefix() const {
    return current_function_non_reentrant ? "static " : "";
}

bool CodeGenerator::use_nonreentrant_frame_abi(bool is_exported) const {
    // ABI boundaries stay native; frame ABI is only for internal non-reentrant functions.
    return current_function_non_reentrant && !is_exported;
}

std::string CodeGenerator::nonreentrant_arg_slot_name(const std::string& c_name, size_t index) const {
    return "__vx_nr_arg_" + c_name + "_" + std::to_string(index);
}

std::string CodeGenerator::nonreentrant_ret_slot_name(const std::string& c_name) const {
    return "__vx_nr_ret_" + c_name;
}

std::string CodeGenerator::external_link_name(const std::string& qualified_name,
                                              const std::string& fallback_c_name) const {
    const std::string prefix = "std::math::";
    if (qualified_name.rfind(prefix, 0) != 0) {
        return fallback_c_name;
    }
    std::string local = qualified_name.substr(prefix.size());
    if (!is_std_math_libc_name(local)) {
        return fallback_c_name;
    }
    return local;
}

bool CodeGenerator::is_bundled_std_math_function(const Symbol* sym, StmtPtr decl) const {
    if (!sym || !decl || !analyzed_program || !analyzed_program->program) {
        return false;
    }
    if (sym->module_id < 0) {
        return false;
    }
    const ModuleInfo* mod = analyzed_program->program->module(sym->module_id);
    if (!mod || mod->origin != ModuleOrigin::BundledStd) {
        return false;
    }
    const std::string& path = mod->path;
    return path == "std/math.vx" ||
           (path.size() >= 11 && path.compare(path.size() - 11, 11, "std/math.vx") == 0);
}

std::string CodeGenerator::load_module_fn(char page) const {
    if (page == 'A') {
        if (!abi.load_module_a_fn.empty()) return abi.load_module_a_fn;
        return "vx_load_module_id_a";
    }
    if (!abi.load_module_b_fn.empty()) return abi.load_module_b_fn;
    return "vx_load_module_id_b";
}

std::string CodeGenerator::strlen_far_fn(char page) const {
    if (page == 'A') {
        if (!abi.strlen_far_a_fn.empty()) return abi.strlen_far_a_fn;
        return "vx_strlen_far_a";
    }
    if (!abi.strlen_far_b_fn.empty()) return abi.strlen_far_b_fn;
    return "vx_strlen_far_b";
}

void CodeGenerator::emit_return_stmt(const std::string& expr) {
    if (current_nonreentrant_frame_abi) {
        if (!expr.empty()) {
            if (current_nonreentrant_returns_value && !current_nonreentrant_return_slot.empty()) {
                emit(current_nonreentrant_return_slot + " = " + expr + ";");
            } else {
                // Preserve side effects for `return expr` in void/non-value contexts.
                emit(expr + ";");
            }
        }
        if (!abi.return_prefix.empty()) {
            emit(abi.return_prefix);
        }
        emit("return;");
        return;
    }

    if (current_returns_aggregate) {
        if (!expr.empty()) {
            emit("*" + aggregate_out_param + " = " + expr + ";");
        }
        if (!abi.return_prefix.empty()) {
            emit(abi.return_prefix);
        }
        emit("return;");
        return;
    }

    if (!abi.return_prefix.empty()) {
        emit(abi.return_prefix);
    }
    if (expr.empty()) {
        emit("return;");
    } else {
        emit("return " + expr + ";");
    }
}

void CodeGenerator::append_return_prefix(std::ostringstream& out) const {
    if (!abi.return_prefix.empty()) {
        out << abi.return_prefix << "\n";
    }
}

int64_t CodeGenerator::resolve_array_length(TypePtr type, const SourceLocation& loc) {
    if (!type || type->kind != Type::Kind::Array || !type->array_size) {
        throw CompileError("Cannot determine array length for comparator generation", loc);
    }
    CTValue size_val;
    if (!lookup_constexpr_value(type->array_size, size_val)) {
        ExprPtr size_expr = type->array_size;
        if (size_expr && size_expr->kind == Expr::Kind::IntLiteral) {
            if (size_expr->has_exact_int_val && size_expr->exact_int_val.fits_i64()) {
                return size_expr->exact_int_val.to_i64();
            }
            if (size_expr->has_exact_int_val && size_expr->exact_int_val.fits_u64()) {
                uint64_t u = size_expr->exact_int_val.to_u64();
                if (u > static_cast<uint64_t>(std::numeric_limits<int64_t>::max())) {
                    throw CompileError("Array length exceeds backend host limit", loc);
                }
                return static_cast<int64_t>(u);
            }
            return static_cast<int64_t>(size_expr->uint_val);
        }
        throw CompileError("Array length must be compile-time constant (frontend CTE fact missing)",
                           loc);
    }

    int64_t out_i64 = 0;
    if (ctvalue_to_i64_exact(size_val, out_i64)) {
        return out_i64;
    }
    uint64_t out_u64 = 0;
    if (ctvalue_to_u64_exact(size_val, out_u64)) {
        if (out_u64 > static_cast<uint64_t>(std::numeric_limits<int64_t>::max())) {
            throw CompileError("Array length exceeds backend host limit", loc);
        }
        return static_cast<int64_t>(out_u64);
    }

    throw CompileError("Array length must be an integer constant", loc);
}

std::string CodeGenerator::ensure_comparator(TypePtr type) {
    if (!type) {
        throw CompileError("Cannot compare value of unknown type", SourceLocation());
    }

    std::string key = type->to_string();
    auto it = comparator_cache.find(key);
    if (it != comparator_cache.end()) {
        return it->second;
    }

    std::string func_name = "vx_cmp_" + sanitize_identifier(key) + "_" + std::to_string(comparator_cache.size());
    comparator_cache[key] = func_name;

    std::ostringstream fn;
    fn << "static int " << func_name << "(" << gen_type(type) << " lhs, " << gen_type(type) << " rhs) {\n";

    switch (type->kind) {
        case Type::Kind::Primitive: {
            if (type->primitive == PrimitiveType::String) {
                fn << "    if (!lhs && !rhs) return 0;\n";
                fn << "    if (!lhs) return -1;\n";
                fn << "    if (!rhs) return 1;\n";
                fn << "    int cmp = strcmp(lhs, rhs);\n";
                fn << "    if (cmp < 0) return -1;\n";
                fn << "    if (cmp > 0) return 1;\n";
                fn << "    return 0;\n";
            } else if (is_extended_integer_type(type)) {
                bool is_signed = false;
                uint64_t bits = 0;
                analyze_extint_type(type, is_signed, bits);
                ensure_extint_type(is_signed, bits);
                if (is_signed) {
                    fn << "    return vx_ai_scmp(lhs.b, rhs.b, sizeof(lhs.b), "
                       << static_cast<unsigned>(extint_sign_mask(bits)) << ");\n";
                } else {
                    fn << "    return vx_ai_ucmp(lhs.b, rhs.b, sizeof(lhs.b));\n";
                }
            } else {
                fn << "    if (lhs < rhs) return -1;\n";
                fn << "    if (lhs > rhs) return 1;\n";
                fn << "    return 0;\n";
            }
            break;
        }
        case Type::Kind::Array: {
            int64_t length = resolve_array_length(type, type->location);
            std::string elem_cmp = ensure_comparator(type->element_type);
            fn << "    for (int i = 0; i < " << length << "; ++i) {\n";
            fn << "        int cmp = " << elem_cmp << "(lhs[i], rhs[i]);\n";
            fn << "        if (cmp != 0) return cmp;\n";
            fn << "    }\n";
            fn << "    return 0;\n";
            break;
        }
        case Type::Kind::Named: {
            if (!analyzed_program || !analyzed_program->lookup_type_symbol) {
                throw CompileError("Internal error: comparator generation without type lookup",
                                   type->location);
            }
            Symbol* sym = analyzed_program->lookup_type_symbol(current_instance_id, type->type_name);
            if (!sym || sym->kind != Symbol::Kind::Type || !sym->declaration || sym->declaration->kind != Stmt::Kind::TypeDecl) {
                throw CompileError("Cannot compare values of type " + type->type_name, type->location);
            }
            auto decl = sym->declaration;
            fn << "    int cmp;\n";
            for (const auto& field : decl->fields) {
                if (!field.type) {
                    throw CompileError("Field type inference required before comparison for " + field.name, field.location);
                }
                std::string field_cmp = ensure_comparator(field.type);
                fn << "    cmp = " << field_cmp << "(lhs." << mangle_name(field.name) << ", rhs." << mangle_name(field.name) << ");\n";
                fn << "    if (cmp != 0) return cmp;\n";
            }
            fn << "    return 0;\n";
            break;
        }
        case Type::Kind::TypeVar:
            throw CompileError("Cannot compare generic type without concrete instantiation", type->location);
        case Type::Kind::TypeOf:
            throw CompileError("Internal error: unresolved #[...] reached megalinker backend", type->location);
    }

    fn << "}\n";
    comparator_definitions.push_back(fn.str());
    return func_name;
}


} // namespace vexel::megalinker_codegen
