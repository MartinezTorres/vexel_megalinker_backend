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
#include <cstdlib>
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
                case PrimitiveType::I8: return "int8_t";
                case PrimitiveType::I16: return "int16_t";
                case PrimitiveType::I32: return "int32_t";
                case PrimitiveType::I64: return "int64_t";
                case PrimitiveType::U8: return "uint8_t";
                case PrimitiveType::U16: return "uint16_t";
                case PrimitiveType::U32: return "uint32_t";
                case PrimitiveType::U64: return "uint64_t";
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
    }

    return "void";
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
    if (live_temps.count(temp)) {
        live_temps.erase(temp);
        available_temps.push(temp);
    }
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
    if (!try_evaluate(type->array_size, size_val)) {
        if (type->array_size->kind == Expr::Kind::Identifier) {
            if (Symbol* size_sym = binding_for(type->array_size)) {
                if (!size_sym->is_mutable &&
                    size_sym->declaration &&
                    size_sym->declaration->var_init &&
                    try_evaluate(size_sym->declaration->var_init, size_val)) {
                    // resolved through immutable declaration initializer
                } else {
                    throw CompileError("Array length must be compile-time constant", loc);
                }
            } else {
                throw CompileError("Array length must be compile-time constant", loc);
            }
        } else
        if (type->array_size->kind == Expr::Kind::IntLiteral &&
            !type->array_size->raw_literal.empty()) {
            std::string raw = type->array_size->raw_literal;
            raw.erase(std::remove(raw.begin(), raw.end(), '_'), raw.end());
            char* end = nullptr;
            unsigned long long parsed = std::strtoull(raw.c_str(), &end, 0);
            if (end && *end == '\0') {
                return static_cast<int64_t>(parsed);
            }
            throw CompileError("Array length must be compile-time constant", loc);
        }
        else {
            throw CompileError("Array length must be compile-time constant", loc);
        }
    }

    if (std::holds_alternative<int64_t>(size_val)) {
        return std::get<int64_t>(size_val);
    }
    if (std::holds_alternative<uint64_t>(size_val)) {
        return static_cast<int64_t>(std::get<uint64_t>(size_val));
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
    }

    fn << "}\n";
    comparator_definitions.push_back(fn.str());
    return func_name;
}


} // namespace vexel::megalinker_codegen
