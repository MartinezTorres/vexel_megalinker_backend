#include "codegen.h"

#include "cte_value.h"

#include <sstream>
#include <limits>

namespace {

using vexel::APInt;
using vexel::CTValue;
using vexel::PrimitiveType;
using vexel::Type;
using vexel::TypePtr;
using vexel::SourceLocation;
using vexel::CompileError;

bool is_integer_primitive_kind(const TypePtr& type, bool& is_signed, uint64_t& bits) {
    if (!type || type->kind != Type::Kind::Primitive) return false;
    if (type->primitive == PrimitiveType::Int) {
        is_signed = true;
        bits = type->integer_bits;
        return bits > 0;
    }
    if (type->primitive == PrimitiveType::UInt) {
        is_signed = false;
        bits = type->integer_bits;
        return bits > 0;
    }
    if (type->primitive == PrimitiveType::FixedInt || type->primitive == PrimitiveType::FixedUInt) {
        int64_t total_bits = vexel::type_bits(type->primitive, type->integer_bits, type->fractional_bits);
        if (total_bits <= 0) return false;
        is_signed = (type->primitive == PrimitiveType::FixedInt);
        bits = static_cast<uint64_t>(total_bits);
        return true;
    }
    return false;
}

void replace_all(std::string& text, const std::string& from, const std::string& to) {
    if (from.empty() || from == to) return;
    size_t pos = 0;
    while ((pos = text.find(from, pos)) != std::string::npos) {
        text.replace(pos, from.size(), to);
        pos += to.size();
    }
}

std::string render_extint_alias_macros(const std::string& helper_prefix) {
    if (helper_prefix == "vx_ai_") return "";
    static const char* kNames[] = {
        "zero","copy","mask_top","is_zero","signbit","ucmp","scmp","get_bit","set_bit",
        "shl1_inplace","add","sub","neg","not","and","or","xor","shl","shr_u","shr_s",
        "mul","udivmod","cast","to_u64_trunc","to_i64_trunc","from_u64","from_i64",
        "to_double_u","from_double_u"
    };
    std::ostringstream os;
    for (const char* name : kNames) {
        os << "#define vx_ai_" << name << " " << helper_prefix << name << "\n";
    }
    os << "\n";
    return os.str();
}

std::string render_extint_runtime_source() {
    return R"VEXEL_EXTINT(
static void vx_ai_zero(uint8_t* out, size_t n) {
    memset(out, 0, n);
}

static void vx_ai_copy(uint8_t* out, const uint8_t* in, size_t n) {
    if (out != in) memcpy(out, in, n);
}

static void vx_ai_mask_top(uint8_t* v, size_t n, uint8_t top_mask) {
    if (n == 0) return;
    v[n - 1] &= top_mask;
}

static int vx_ai_is_zero(const uint8_t* v, size_t n) {
    for (size_t i = 0; i < n; ++i) {
        if (v[i] != 0) return 0;
    }
    return 1;
}

static int vx_ai_signbit(const uint8_t* v, size_t n, uint8_t sign_mask) {
    if (n == 0) return 0;
    return (v[n - 1] & sign_mask) ? 1 : 0;
}

static int vx_ai_ucmp(const uint8_t* a, const uint8_t* b, size_t n) {
    for (size_t i = n; i > 0; --i) {
        size_t idx = i - 1;
        if (a[idx] < b[idx]) return -1;
        if (a[idx] > b[idx]) return 1;
    }
    return 0;
}

static int vx_ai_scmp(const uint8_t* a, const uint8_t* b, size_t n, uint8_t sign_mask) {
    int sa = vx_ai_signbit(a, n, sign_mask);
    int sb = vx_ai_signbit(b, n, sign_mask);
    if (sa != sb) return sa ? -1 : 1;
    int u = vx_ai_ucmp(a, b, n);
    return sa ? -u : u;
}

static int vx_ai_get_bit(const uint8_t* v, size_t n, unsigned long bit_index) {
    unsigned long byte_index = bit_index / 8u;
    unsigned long bit = bit_index % 8u;
    if ((size_t)byte_index >= n) return 0;
    return (v[byte_index] >> bit) & 1u;
}

static void vx_ai_set_bit(uint8_t* v, size_t n, unsigned long bit_index, int bit_value) {
    unsigned long byte_index = bit_index / 8u;
    unsigned long bit = bit_index % 8u;
    if ((size_t)byte_index >= n) return;
    uint8_t mask = (uint8_t)(1u << bit);
    if (bit_value) v[byte_index] |= mask;
    else v[byte_index] &= (uint8_t)~mask;
}

static void vx_ai_shl1_inplace(uint8_t* v, size_t n, uint8_t top_mask) {
    unsigned carry = 0u;
    for (size_t i = 0; i < n; ++i) {
        unsigned next = (unsigned)((v[i] >> 7) & 1u);
        v[i] = (uint8_t)(((unsigned)v[i] << 1u) | carry);
        carry = next;
    }
    vx_ai_mask_top(v, n, top_mask);
}

static void vx_ai_add(uint8_t* out, const uint8_t* a, const uint8_t* b, size_t n, uint8_t top_mask) {
    unsigned carry = 0u;
    for (size_t i = 0; i < n; ++i) {
        unsigned sum = (unsigned)a[i] + (unsigned)b[i] + carry;
        out[i] = (uint8_t)(sum & 0xFFu);
        carry = (sum >> 8) & 0xFFu;
    }
    vx_ai_mask_top(out, n, top_mask);
}

static void vx_ai_sub(uint8_t* out, const uint8_t* a, const uint8_t* b, size_t n, uint8_t top_mask) {
    unsigned borrow = 0u;
    for (size_t i = 0; i < n; ++i) {
        unsigned av = (unsigned)a[i];
        unsigned bv = (unsigned)b[i] + borrow;
        if (av >= bv) {
            out[i] = (uint8_t)(av - bv);
            borrow = 0u;
        } else {
            out[i] = (uint8_t)(256u + av - bv);
            borrow = 1u;
        }
    }
    vx_ai_mask_top(out, n, top_mask);
}

static void vx_ai_neg(uint8_t* out, const uint8_t* a, size_t n, uint8_t top_mask) {
    for (size_t i = 0; i < n; ++i) out[i] = (uint8_t)~a[i];
    vx_ai_mask_top(out, n, top_mask);
    unsigned carry = 1u;
    for (size_t i = 0; i < n; ++i) {
        unsigned sum = (unsigned)out[i] + carry;
        out[i] = (uint8_t)(sum & 0xFFu);
        carry = (sum >> 8) & 0xFFu;
        if (!carry) break;
    }
    vx_ai_mask_top(out, n, top_mask);
}

static void vx_ai_not(uint8_t* out, const uint8_t* a, size_t n, uint8_t top_mask) {
    for (size_t i = 0; i < n; ++i) out[i] = (uint8_t)~a[i];
    vx_ai_mask_top(out, n, top_mask);
}

static void vx_ai_and(uint8_t* out, const uint8_t* a, const uint8_t* b, size_t n, uint8_t top_mask) {
    for (size_t i = 0; i < n; ++i) out[i] = (uint8_t)(a[i] & b[i]);
    vx_ai_mask_top(out, n, top_mask);
}

static void vx_ai_or(uint8_t* out, const uint8_t* a, const uint8_t* b, size_t n, uint8_t top_mask) {
    for (size_t i = 0; i < n; ++i) out[i] = (uint8_t)(a[i] | b[i]);
    vx_ai_mask_top(out, n, top_mask);
}

static void vx_ai_xor(uint8_t* out, const uint8_t* a, const uint8_t* b, size_t n, uint8_t top_mask) {
    for (size_t i = 0; i < n; ++i) out[i] = (uint8_t)(a[i] ^ b[i]);
    vx_ai_mask_top(out, n, top_mask);
}

static void vx_ai_shl(uint8_t* out, const uint8_t* a, size_t n, unsigned long shift, uint8_t top_mask) {
    unsigned long byte_shift = shift / 8u;
    unsigned bit_shift = (unsigned)(shift % 8u);
    for (size_t i = 0; i < n; ++i) out[i] = 0;
    if (n == 0) return;
    if (byte_shift >= (unsigned long)n) {
        vx_ai_mask_top(out, n, top_mask);
        return;
    }
    for (size_t i = n; i > 0; --i) {
        size_t dst = i - 1;
        if ((unsigned long)dst < byte_shift) continue;
        size_t src = dst - (size_t)byte_shift;
        unsigned v = (unsigned)a[src] << bit_shift;
        if (bit_shift != 0u && src > 0) {
            v |= (unsigned)a[src - 1] >> (8u - bit_shift);
        }
        out[dst] = (uint8_t)(v & 0xFFu);
    }
    vx_ai_mask_top(out, n, top_mask);
}

static void vx_ai_shr_u(uint8_t* out, const uint8_t* a, size_t n, unsigned long shift, uint8_t top_mask) {
    unsigned long byte_shift = shift / 8u;
    unsigned bit_shift = (unsigned)(shift % 8u);
    for (size_t i = 0; i < n; ++i) out[i] = 0;
    if (n == 0) return;
    if (byte_shift >= (unsigned long)n) {
        vx_ai_mask_top(out, n, top_mask);
        return;
    }
    for (size_t i = 0; i < n; ++i) {
        size_t src = i + (size_t)byte_shift;
        if (src >= n) break;
        unsigned v = (unsigned)a[src] >> bit_shift;
        if (bit_shift != 0u && (src + 1) < n) {
            v |= ((unsigned)a[src + 1] << (8u - bit_shift)) & 0xFFu;
        }
        out[i] = (uint8_t)(v & 0xFFu);
    }
    vx_ai_mask_top(out, n, top_mask);
}

static void vx_ai_shr_s(uint8_t* out, const uint8_t* a, size_t n, unsigned long shift, uint8_t top_mask, uint8_t sign_mask) {
    int neg = vx_ai_signbit(a, n, sign_mask);
    vx_ai_shr_u(out, a, n, shift, top_mask);
    if (!neg || n == 0) return;
    unsigned long total_bits = (unsigned long)n * 8u;
    unsigned long used_bits = 0;
    for (unsigned bit = 0; bit < 8u; ++bit) {
        if (top_mask & (uint8_t)(1u << bit)) used_bits = (unsigned long)((n - 1) * 8u + bit + 1u);
    }
    if (used_bits == 0) used_bits = total_bits;
    if (shift >= used_bits) {
        for (size_t i = 0; i < n; ++i) out[i] = 0xFFu;
        vx_ai_mask_top(out, n, top_mask);
        return;
    }
    for (unsigned long bit = used_bits - shift; bit < used_bits; ++bit) {
        vx_ai_set_bit(out, n, bit, 1);
    }
    vx_ai_mask_top(out, n, top_mask);
}

static void vx_ai_mul(uint8_t* out, const uint8_t* a, const uint8_t* b, size_t n, uint8_t top_mask) {
    for (size_t i = 0; i < n; ++i) out[i] = 0;
    for (size_t i = 0; i < n; ++i) {
        unsigned carry = 0u;
        for (size_t j = 0; j + i < n; ++j) {
            size_t k = i + j;
            unsigned prod = (unsigned)out[k] + (unsigned)a[i] * (unsigned)b[j] + carry;
            out[k] = (uint8_t)(prod & 0xFFu);
            carry = (prod >> 8) & 0xFFFFu;
        }
    }
    vx_ai_mask_top(out, n, top_mask);
}

static void vx_ai_udivmod(uint8_t* out_q, uint8_t* out_r,
                          const uint8_t* a, const uint8_t* b,
                          size_t n, uint8_t top_mask) {
    for (size_t i = 0; i < n; ++i) { out_q[i] = 0; out_r[i] = 0; }
    if (n == 0) return;
    if (vx_ai_is_zero(b, n)) return;
    unsigned long used_bits = 0;
    for (unsigned bit = 0; bit < 8u; ++bit) {
        if (top_mask & (uint8_t)(1u << bit)) used_bits = (unsigned long)((n - 1) * 8u + bit + 1u);
    }
    if (used_bits == 0) used_bits = (unsigned long)n * 8u;
    for (unsigned long iter = used_bits; iter > 0; --iter) {
        unsigned long bit = iter - 1u;
        vx_ai_shl1_inplace(out_r, n, top_mask);
        vx_ai_set_bit(out_r, n, 0u, vx_ai_get_bit(a, n, bit));
        if (vx_ai_ucmp(out_r, b, n) >= 0) {
            vx_ai_sub(out_r, out_r, b, n, top_mask);
            vx_ai_set_bit(out_q, n, bit, 1);
        }
    }
    vx_ai_mask_top(out_q, n, top_mask);
    vx_ai_mask_top(out_r, n, top_mask);
}

static void vx_ai_cast(uint8_t* out, size_t out_n, uint8_t out_top_mask,
                       const uint8_t* in, size_t in_n, int sign_extend, uint8_t in_sign_mask) {
    size_t count = out_n < in_n ? out_n : in_n;
    for (size_t i = 0; i < count; ++i) out[i] = in[i];
    uint8_t fill = 0;
    if (sign_extend && in_n > 0 && (in[in_n - 1] & in_sign_mask)) fill = 0xFFu;
    for (size_t i = count; i < out_n; ++i) out[i] = fill;
    vx_ai_mask_top(out, out_n, out_top_mask);
}

static uint64_t vx_ai_to_u64_trunc(const uint8_t* in, size_t n) {
    uint64_t out = 0;
    size_t limit = (n < 8u) ? n : 8u;
    for (size_t i = 0; i < limit; ++i) {
        out |= ((uint64_t)in[i]) << (8u * (unsigned)i);
    }
    return out;
}

static int64_t vx_ai_to_i64_trunc(const uint8_t* in, size_t n, uint8_t sign_mask) {
    uint64_t raw = vx_ai_to_u64_trunc(in, n);
    unsigned bits = 0u;
    if (n == 0) return 0;
    for (unsigned bit = 0; bit < 8u; ++bit) {
        if (sign_mask & (uint8_t)(1u << bit)) bits = (unsigned)((n - 1u) * 8u + bit + 1u);
    }
    if (bits == 0u) bits = (unsigned)(n * 8u);
    if (bits >= 64u) return (int64_t)raw;
    uint64_t mask = (((uint64_t)1) << bits) - 1u;
    raw &= mask;
    uint64_t sign = ((uint64_t)1) << (bits - 1u);
    if (raw & sign) raw |= ~mask;
    return (int64_t)raw;
}

static void vx_ai_from_u64(uint8_t* out, size_t n, uint8_t top_mask, uint64_t value) {
    for (size_t i = 0; i < n; ++i) {
        out[i] = (uint8_t)(value & 0xFFu);
        value >>= 8u;
    }
    vx_ai_mask_top(out, n, top_mask);
}

static void vx_ai_from_i64(uint8_t* out, size_t n, uint8_t top_mask, int64_t value) {
    uint64_t raw = (uint64_t)value;
    size_t limit = (n < 8u) ? n : 8u;
    for (size_t i = 0; i < limit; ++i) {
        out[i] = (uint8_t)(raw & 0xFFu);
        raw >>= 8u;
    }
    uint8_t fill = (value < 0) ? 0xFFu : 0x00u;
    for (size_t i = limit; i < n; ++i) {
        out[i] = fill;
    }
    vx_ai_mask_top(out, n, top_mask);
}

static double vx_ai_to_double_u(const uint8_t* in, size_t n) {
    double out = 0.0;
    for (size_t i = n; i > 0; --i) {
        out = (out * 256.0) + (double)in[i - 1];
    }
    return out;
}

static void vx_ai_from_double_u(uint8_t* out, size_t n, uint8_t top_mask, double value) {
    vx_ai_zero(out, n);
    if (!(value > 0.0) || !isfinite(value)) {
        vx_ai_mask_top(out, n, top_mask);
        return;
    }
    double x = floor(value);
    for (size_t i = 0; i < n; ++i) {
        if (!(x > 0.0)) break;
        double q = floor(x / 256.0);
        double rem = x - (q * 256.0);
        if (rem < 0.0) rem = 0.0;
        if (rem > 255.0) rem = fmod(rem, 256.0);
        out[i] = (uint8_t)rem;
        x = q;
    }
    vx_ai_mask_top(out, n, top_mask);
}
)VEXEL_EXTINT";
}

std::string bytes_initializer_from_apint(bool target_signed, uint64_t bits, const APInt& value) {
    APInt wrapped = target_signed ? value.wrapped_signed(bits).wrapped_unsigned(bits)
                                  : value.wrapped_unsigned(bits);
    boost::multiprecision::cpp_int raw = wrapped.raw();
    size_t n = static_cast<size_t>((bits + 7u) / 8u);
    std::ostringstream os;
    os << "{";
    for (size_t i = 0; i < n; ++i) {
        unsigned byte = static_cast<unsigned>((raw & 0xFF).convert_to<unsigned>());
        if (i) os << ", ";
        os << "0x";
        os << std::hex << std::uppercase;
        if (byte < 16) os << "0";
        os << byte;
        os << std::dec << std::nouppercase;
        raw >>= 8;
    }
    os << "}";
    return os.str();
}

} // namespace

namespace vexel::megalinker_codegen {

bool CodeGenerator::analyze_extint_type(TypePtr type, bool& is_signed, uint64_t& bits) const {
    TypePtr resolved = resolve_type(type);
    if (!is_integer_primitive_kind(resolved, is_signed, bits)) return false;
    return bits != 8 && bits != 16 && bits != 32 && bits != 64;
}

bool CodeGenerator::is_extended_integer_type(TypePtr type) const {
    bool is_signed = false;
    uint64_t bits = 0;
    return analyze_extint_type(type, is_signed, bits);
}

bool CodeGenerator::is_native_integer_type(TypePtr type) const {
    bool is_signed = false;
    uint64_t bits = 0;
    TypePtr resolved = resolve_type(type);
    if (!is_integer_primitive_kind(resolved, is_signed, bits)) return false;
    return bits == 8 || bits == 16 || bits == 32 || bits == 64;
}

bool CodeGenerator::is_extended_integer_expr(ExprPtr expr) const {
    return expr && is_extended_integer_type(expr->type);
}

std::string CodeGenerator::extint_type_name(bool is_signed, uint64_t bits) const {
    const std::string prefix = internal_symbol_prefix.empty() ? "vx_" : internal_symbol_prefix;
    return prefix + (is_signed ? "i" : "u") + std::to_string(bits) + "_t";
}

uint8_t CodeGenerator::extint_top_mask(uint64_t bits) const {
    unsigned rem = static_cast<unsigned>(bits % 8u);
    if (rem == 0u) return 0xFFu;
    return static_cast<uint8_t>((1u << rem) - 1u);
}

uint8_t CodeGenerator::extint_sign_mask(uint64_t bits) const {
    unsigned bit = static_cast<unsigned>((bits - 1u) % 8u);
    return static_cast<uint8_t>(1u << bit);
}

size_t CodeGenerator::extint_num_bytes(uint64_t bits, const SourceLocation& loc, const std::string& context) const {
    if (bits == 0) {
        throw CompileError("Invalid zero-width integer in " + context, loc);
    }
    uint64_t bytes_u64 = (bits + 7u) / 8u;
    if (bytes_u64 > static_cast<uint64_t>(std::numeric_limits<size_t>::max())) {
        throw CompileError("Integer width too large for backend code generation in " + context, loc);
    }
    return static_cast<size_t>(bytes_u64);
}

void CodeGenerator::ensure_extint_runtime() {
    if (!extint_runtime_source.empty()) return;
    const std::string symbol_prefix = internal_symbol_prefix.empty() ? "vx_" : internal_symbol_prefix;
    const std::string helper_prefix = symbol_prefix + "ai_";
    extint_runtime_source = render_extint_alias_macros(helper_prefix) + render_extint_runtime_source();
    if (helper_prefix != "vx_ai_") {
        replace_all(extint_runtime_source, "vx_ai_", helper_prefix);
        // Restore alias macro left-hand sides after global replacement.
        replace_all(extint_runtime_source, "#define " + helper_prefix, "#define vx_ai_");
    }
}

void CodeGenerator::ensure_extint_type(TypePtr type, const SourceLocation& loc, const std::string& context) {
    bool is_signed = false;
    uint64_t bits = 0;
    if (!analyze_extint_type(type, is_signed, bits)) return;
    if (bits == 0) {
        throw CompileError("Invalid integer width in " + context, loc);
    }
    ensure_extint_type(is_signed, bits);
}

void CodeGenerator::ensure_extint_type(bool is_signed, uint64_t bits) {
    if (!extint_types_used.insert({is_signed, bits}).second) {
        return;
    }
    ensure_extint_runtime();
    size_t bytes = extint_num_bytes(bits, SourceLocation(), "arbitrary-width integer helper");
    std::ostringstream os;
    os << "typedef struct { unsigned char b[" << bytes << "]; } " << extint_type_name(is_signed, bits) << ";\n";
    extint_header_defs += os.str();
}

bool CodeGenerator::ctvalue_to_apint_value(const CTValue& v, APInt& out, bool& is_unsigned) const {
    return ctvalue_to_exact_int(v, out, is_unsigned);
}

std::string CodeGenerator::emit_extint_const_initializer(TypePtr target_type,
                                                         const APInt& value,
                                                         bool value_is_unsigned,
                                                         const SourceLocation& loc) {
    (void)value_is_unsigned;
    bool is_signed = false;
    uint64_t bits = 0;
    if (!analyze_extint_type(target_type, is_signed, bits)) {
        throw CompileError("Internal error: expected arbitrary-width integer target for constant initializer", loc);
    }
    ensure_extint_type(is_signed, bits);
    std::string bytes = bytes_initializer_from_apint(is_signed, bits, value);
    return "((" + extint_type_name(is_signed, bits) + "){" + bytes + "})";
}

std::string CodeGenerator::emit_extint_temp_literal(TypePtr target_type,
                                                    const APInt& value,
                                                    bool value_is_unsigned,
                                                    const SourceLocation& loc) {
    return emit_extint_const_initializer(target_type, value, value_is_unsigned, loc);
}

std::optional<std::string> CodeGenerator::folded_scalar_expr_literal(const CTValue& value,
                                                                     TypePtr expected_type,
                                                                     const SourceLocation& loc) {
    APInt exact;
    bool is_unsigned = false;
    if (ctvalue_to_apint_value(value, exact, is_unsigned)) {
        if (is_extended_integer_type(expected_type)) {
            return emit_extint_temp_literal(expected_type, exact, is_unsigned, loc);
        }
        if (exact.fits_i64()) return std::to_string(exact.to_i64());
        if (exact.fits_u64()) return std::to_string(exact.to_u64());
        return std::nullopt;
    }
    if (std::holds_alternative<bool>(value)) {
        return std::get<bool>(value) ? "1" : "0";
    }
    if (std::holds_alternative<double>(value)) {
        return std::to_string(std::get<double>(value));
    }
    return std::nullopt;
}

std::string CodeGenerator::extint_shift_amount_expr(const std::string& rhs_expr, TypePtr rhs_type, const SourceLocation& loc) {
    if (is_extended_integer_type(rhs_type)) {
        bool rhs_signed = false;
        uint64_t rhs_bits = 0;
        analyze_extint_type(rhs_type, rhs_signed, rhs_bits);
        ensure_extint_type(rhs_signed, rhs_bits);
        size_t n = extint_num_bytes(rhs_bits, loc, "shift amount");
        (void)n;
        return "(unsigned long)vx_ai_to_u64_trunc((" + rhs_expr + ").b, sizeof((" + rhs_expr + ").b))";
    }
    return "(unsigned long)(" + rhs_expr + ")";
}

std::string CodeGenerator::gen_extint_unary(ExprPtr expr, const std::string& operand) {
    bool is_signed = false;
    uint64_t bits = 0;
    if (!analyze_extint_type(expr ? expr->type : nullptr, is_signed, bits)) {
        throw CompileError("Internal error: extint unary emission without extint result type", expr ? expr->location : SourceLocation());
    }
    ensure_extint_type(is_signed, bits);
    std::string tmp = fresh_temp();
    std::string rtype = gen_type(expr->type);
    if (!declared_temps.count(tmp)) {
        emit(storage_prefix() + rtype + " " + tmp + ";");
        declared_temps.insert(tmp);
    }
    if (expr->op == "-") {
        emit("vx_ai_neg(" + tmp + ".b, (" + operand + ").b, sizeof(" + tmp + ".b), " + std::to_string(extint_top_mask(bits)) + ");");
        return tmp;
    }
    if (expr->op == "~") {
        emit("vx_ai_not(" + tmp + ".b, (" + operand + ").b, sizeof(" + tmp + ".b), " + std::to_string(extint_top_mask(bits)) + ");");
        return tmp;
    }
    if (expr->op == "+") {
        emit(tmp + " = " + operand + ";");
        return tmp;
    }
    throw CompileError("Unsupported unary operator '" + expr->op + "' for arbitrary-width integer", expr->location);
}

std::string CodeGenerator::gen_extint_binary(ExprPtr expr, const std::string& left, const std::string& right) {
    if (!expr) return "";
    std::string left_use = left;
    std::string right_use = right;
    if (expr->left && is_extended_integer_type(expr->left->type)) {
        std::string tmp = fresh_temp();
        std::string t = gen_type(expr->left->type);
        if (!declared_temps.count(tmp)) {
            emit(storage_prefix() + t + " " + tmp + ";");
            declared_temps.insert(tmp);
        }
        emit(tmp + " = " + left + ";");
        left_use = tmp;
    }
    if (expr->right && is_extended_integer_type(expr->right->type)) {
        std::string tmp = fresh_temp();
        std::string t = gen_type(expr->right->type);
        if (!declared_temps.count(tmp)) {
            emit(storage_prefix() + t + " " + tmp + ";");
            declared_temps.insert(tmp);
        }
        emit(tmp + " = " + right + ";");
        right_use = tmp;
    }
    const std::string& op = expr->op;
    if (op == "==" || op == "!=" || op == "<" || op == "<=" || op == ">" || op == ">=") {
        TypePtr cmp_type = expr->left ? expr->left->type : expr->right ? expr->right->type : nullptr;
        bool is_signed = false;
        uint64_t bits = 0;
        if (!analyze_extint_type(cmp_type, is_signed, bits)) {
            throw CompileError("Internal error: extint comparison without extint operand type", expr->location);
        }
        ensure_extint_type(is_signed, bits);
        std::string cmp_tmp = fresh_temp();
        if (!declared_temps.count(cmp_tmp)) {
            emit(storage_prefix() + std::string("int ") + cmp_tmp + ";");
            declared_temps.insert(cmp_tmp);
        }
        std::string cmp_fn = is_signed ? "vx_ai_scmp" : "vx_ai_ucmp";
        std::string args = "(" + left_use + ").b, (" + right_use + ").b, sizeof((" + left_use + ").b)";
        if (is_signed) {
            args += ", " + std::to_string(extint_sign_mask(bits));
        }
        emit(cmp_tmp + " = " + cmp_fn + "(" + args + ");");
        std::string out = fresh_temp();
        if (!declared_temps.count(out)) {
            emit(storage_prefix() + std::string("_Bool ") + out + ";");
            declared_temps.insert(out);
        }
        emit(out + " = (" + cmp_tmp + " " + op + " 0);");
        return out;
    }

    bool result_signed = false;
    uint64_t result_bits = 0;
    if (!analyze_extint_type(expr->type, result_signed, result_bits)) {
        throw CompileError("Internal error: extint binary emission without extint result type", expr->location);
    }
    ensure_extint_type(result_signed, result_bits);
    std::string out = fresh_temp();
    std::string out_type = gen_type(expr->type);
    if (!declared_temps.count(out)) {
        emit(storage_prefix() + out_type + " " + out + ";");
        declared_temps.insert(out);
    }

    std::string top_mask = std::to_string(extint_top_mask(result_bits));
    std::string sign_mask = std::to_string(extint_sign_mask(result_bits));

    auto fixed_extint_meta = [&](TypePtr type, uint64_t& bits, bool& is_signed_raw, int64_t& frac_bits) {
        if (!type || type->kind != Type::Kind::Primitive) return false;
        if (type->primitive != PrimitiveType::FixedInt && type->primitive != PrimitiveType::FixedUInt) return false;
        int64_t bits_i64 = type_bits(type->primitive, type->integer_bits, type->fractional_bits);
        if (bits_i64 <= 0) return false;
        bits = static_cast<uint64_t>(bits_i64);
        is_signed_raw = (type->primitive == PrimitiveType::FixedInt);
        frac_bits = type->fractional_bits;
        return true;
    };
    auto add_checked_u64 = [&](uint64_t a, uint64_t b, const std::string& context) -> uint64_t {
        if (a > std::numeric_limits<uint64_t>::max() - b) {
            throw CompileError("Fixed-point width overflow in " + context, expr->location);
        }
        return a + b;
    };
    auto abs_frac_u64 = [&](int64_t frac_bits, const std::string& context) -> uint64_t {
        if (frac_bits == std::numeric_limits<int64_t>::min()) {
            throw CompileError("Fixed-point fractional width overflow in " + context, expr->location);
        }
        return frac_bits < 0 ? static_cast<uint64_t>(-frac_bits) : static_cast<uint64_t>(frac_bits);
    };
    uint64_t fixed_bits = 0;
    bool fixed_signed = false;
    int64_t fixed_frac = 0;
    if ((op == "*" || op == "/") &&
        fixed_extint_meta(expr ? expr->type : nullptr, fixed_bits, fixed_signed, fixed_frac) &&
        fixed_frac != 0) {
        uint64_t work_bits = 0;
        if (op == "*") {
            work_bits = add_checked_u64(fixed_bits, fixed_bits, "fixed-point multiplication");
            if (fixed_frac < 0) {
                work_bits = add_checked_u64(work_bits, abs_frac_u64(fixed_frac, "fixed-point multiplication"),
                                            "fixed-point multiplication");
            }
        } else {
            work_bits = add_checked_u64(fixed_bits, fixed_bits, "fixed-point division");
            work_bits = add_checked_u64(work_bits, abs_frac_u64(fixed_frac, "fixed-point division"),
                                        "fixed-point division");
        }
        if (work_bits <= fixed_bits) {
            work_bits = add_checked_u64(fixed_bits, 1, "fixed-point widening");
        }

        ensure_extint_type(fixed_signed, work_bits);
        std::string work_type = extint_type_name(fixed_signed, work_bits);
        std::string work_top_mask = std::to_string(extint_top_mask(work_bits));
        std::string work_sign_mask = std::to_string(extint_sign_mask(work_bits));
        auto declare_work_temp = [&]() {
            std::string tmp = fresh_temp();
            if (!declared_temps.count(tmp)) {
                emit(storage_prefix() + work_type + " " + tmp + ";");
                declared_temps.insert(tmp);
            }
            return tmp;
        };
        std::string lhs_wide = declare_work_temp();
        std::string rhs_wide = declare_work_temp();
        emit("vx_ai_cast(" + lhs_wide + ".b, sizeof(" + lhs_wide + ".b), " + work_top_mask +
             ", (" + left_use + ").b, sizeof((" + left_use + ").b), " +
             std::string(fixed_signed ? "1" : "0") + ", " + std::to_string(extint_sign_mask(fixed_bits)) + ");");
        emit("vx_ai_cast(" + rhs_wide + ".b, sizeof(" + rhs_wide + ".b), " + work_top_mask +
             ", (" + right_use + ").b, sizeof((" + right_use + ").b), " +
             std::string(fixed_signed ? "1" : "0") + ", " + std::to_string(extint_sign_mask(fixed_bits)) + ");");

        std::string scaled = declare_work_temp();
        if (op == "*") {
            std::string prod = declare_work_temp();
            emit("vx_ai_mul(" + prod + ".b, (" + lhs_wide + ").b, (" + rhs_wide + ").b, sizeof(" + prod + ".b), " +
                 work_top_mask + ");");
            if (fixed_frac > 0) {
                uint64_t shift = static_cast<uint64_t>(fixed_frac);
                if (fixed_signed) {
                    std::string neg = fresh_temp();
                    if (!declared_temps.count(neg)) {
                        emit(storage_prefix() + std::string("_Bool ") + neg + ";");
                        declared_temps.insert(neg);
                    }
                    std::string abs = declare_work_temp();
                    std::string shr = declare_work_temp();
                    emit(neg + " = vx_ai_signbit(" + prod + ".b, sizeof(" + prod + ".b), " + work_sign_mask + ");");
                    emit("if (" + neg + ") vx_ai_neg(" + abs + ".b, " + prod + ".b, sizeof(" + abs + ".b), " +
                         work_top_mask + "); else " + abs + " = " + prod + ";");
                    emit("vx_ai_shr_u(" + shr + ".b, " + abs + ".b, sizeof(" + shr + ".b), " + std::to_string(shift) +
                         ", " + work_top_mask + ");");
                    emit("if (" + neg + ") vx_ai_neg(" + scaled + ".b, " + shr + ".b, sizeof(" + scaled + ".b), " +
                         work_top_mask + "); else " + scaled + " = " + shr + ";");
                } else {
                    emit("vx_ai_shr_u(" + scaled + ".b, " + prod + ".b, sizeof(" + scaled + ".b), " +
                         std::to_string(shift) + ", " + work_top_mask + ");");
                }
            } else if (fixed_frac < 0) {
                emit("vx_ai_shl(" + scaled + ".b, " + prod + ".b, sizeof(" + scaled + ".b), " +
                     std::to_string(static_cast<uint64_t>(-fixed_frac)) + ", " + work_top_mask + ");");
            } else {
                emit(scaled + " = " + prod + ";");
            }
        } else {
            std::string num = declare_work_temp();
            std::string den = declare_work_temp();
            emit(num + " = " + lhs_wide + ";");
            emit(den + " = " + rhs_wide + ";");
            if (fixed_frac > 0) {
                std::string num_shifted = declare_work_temp();
                emit("vx_ai_shl(" + num_shifted + ".b, " + num + ".b, sizeof(" + num + ".b), " +
                     std::to_string(static_cast<uint64_t>(fixed_frac)) + ", " + work_top_mask + ");");
                emit(num + " = " + num_shifted + ";");
            } else if (fixed_frac < 0) {
                std::string den_shifted = declare_work_temp();
                emit("vx_ai_shl(" + den_shifted + ".b, " + den + ".b, sizeof(" + den + ".b), " +
                     std::to_string(static_cast<uint64_t>(-fixed_frac)) + ", " + work_top_mask + ");");
                emit(den + " = " + den_shifted + ";");
            }

            if (!fixed_signed) {
                std::string rem = declare_work_temp();
                emit("vx_ai_udivmod(" + scaled + ".b, " + rem + ".b, " + num + ".b, " + den + ".b, sizeof(" +
                     scaled + ".b), " + work_top_mask + ");");
            } else {
                std::string q = declare_work_temp();
                std::string rem = declare_work_temp();
                std::string num_abs = declare_work_temp();
                std::string den_abs = declare_work_temp();
                std::string num_neg = fresh_temp();
                std::string den_neg = fresh_temp();
                if (!declared_temps.count(num_neg)) {
                    emit(storage_prefix() + std::string("_Bool ") + num_neg + ";");
                    declared_temps.insert(num_neg);
                }
                if (!declared_temps.count(den_neg)) {
                    emit(storage_prefix() + std::string("_Bool ") + den_neg + ";");
                    declared_temps.insert(den_neg);
                }
                emit(num_neg + " = vx_ai_signbit(" + num + ".b, sizeof(" + num + ".b), " + work_sign_mask + ");");
                emit(den_neg + " = vx_ai_signbit(" + den + ".b, sizeof(" + den + ".b), " + work_sign_mask + ");");
                emit("if (" + num_neg + ") vx_ai_neg(" + num_abs + ".b, " + num + ".b, sizeof(" + num_abs + ".b), " +
                     work_top_mask + "); else " + num_abs + " = " + num + ";");
                emit("if (" + den_neg + ") vx_ai_neg(" + den_abs + ".b, " + den + ".b, sizeof(" + den_abs + ".b), " +
                     work_top_mask + "); else " + den_abs + " = " + den + ";");
                emit("vx_ai_udivmod(" + q + ".b, " + rem + ".b, " + num_abs + ".b, " + den_abs + ".b, sizeof(" +
                     q + ".b), " + work_top_mask + ");");
                emit(scaled + " = " + q + ";");
                emit("if (" + num_neg + " != " + den_neg + ") vx_ai_neg(" + scaled + ".b, " + scaled +
                     ".b, sizeof(" + scaled + ".b), " + work_top_mask + ");");
            }
        }

        emit("vx_ai_cast(" + out + ".b, sizeof(" + out + ".b), " + top_mask +
             ", " + scaled + ".b, sizeof(" + scaled + ".b), " +
             std::string(fixed_signed ? "1" : "0") + ", " + work_sign_mask + ");");
        return out;
    }

    if (op == "+") {
        emit("vx_ai_add(" + out + ".b, (" + left_use + ").b, (" + right_use + ").b, sizeof(" + out + ".b), " + top_mask + ");");
        return out;
    }
    if (op == "-") {
        emit("vx_ai_sub(" + out + ".b, (" + left_use + ").b, (" + right_use + ").b, sizeof(" + out + ".b), " + top_mask + ");");
        return out;
    }
    if (op == "*") {
        emit("vx_ai_mul(" + out + ".b, (" + left_use + ").b, (" + right_use + ").b, sizeof(" + out + ".b), " + top_mask + ");");
        return out;
    }
    if (op == "&") {
        emit("vx_ai_and(" + out + ".b, (" + left_use + ").b, (" + right_use + ").b, sizeof(" + out + ".b), " + top_mask + ");");
        return out;
    }
    if (op == "|") {
        emit("vx_ai_or(" + out + ".b, (" + left_use + ").b, (" + right_use + ").b, sizeof(" + out + ".b), " + top_mask + ");");
        return out;
    }
    if (op == "^") {
        emit("vx_ai_xor(" + out + ".b, (" + left_use + ").b, (" + right_use + ").b, sizeof(" + out + ".b), " + top_mask + ");");
        return out;
    }
    if (op == "<<" || op == ">>") {
        std::string shift_expr = extint_shift_amount_expr(right_use, expr->right ? expr->right->type : nullptr, expr->location);
        if (op == "<<") {
            emit("vx_ai_shl(" + out + ".b, (" + left_use + ").b, sizeof(" + out + ".b), " + shift_expr + ", " + top_mask + ");");
        } else if (result_signed) {
            emit("vx_ai_shr_s(" + out + ".b, (" + left_use + ").b, sizeof(" + out + ".b), " + shift_expr + ", " + top_mask + ", " + sign_mask + ");");
        } else {
            emit("vx_ai_shr_u(" + out + ".b, (" + left_use + ").b, sizeof(" + out + ".b), " + shift_expr + ", " + top_mask + ");");
        }
        return out;
    }
    if (op == "/" || op == "%") {
        if (!result_signed) {
            std::string q = (op == "/") ? out : fresh_temp();
            std::string r = (op == "%") ? out : fresh_temp();
            if (q != out && !declared_temps.count(q)) {
                emit(storage_prefix() + out_type + " " + q + ";");
                declared_temps.insert(q);
            }
            if (r != out && !declared_temps.count(r)) {
                emit(storage_prefix() + out_type + " " + r + ";");
                declared_temps.insert(r);
            }
            emit("vx_ai_udivmod(" + q + ".b, " + r + ".b, (" + left_use + ").b, (" + right_use + ").b, sizeof(" + out + ".b), " + top_mask + ");");
            return out;
        }

        // Signed division/modulo via unsigned helper on absolute values.
        std::string l_abs = fresh_temp();
        std::string r_abs = fresh_temp();
        std::string q = fresh_temp();
        std::string rem = fresh_temp();
        for (const std::string* name : {&l_abs, &r_abs, &q, &rem}) {
            if (!declared_temps.count(*name)) {
                emit(storage_prefix() + out_type + " " + *name + ";");
                declared_temps.insert(*name);
            }
        }
        std::string l_neg = fresh_temp();
        std::string r_neg = fresh_temp();
        if (!declared_temps.count(l_neg)) {
            emit(storage_prefix() + std::string("_Bool ") + l_neg + ";");
            declared_temps.insert(l_neg);
        }
        if (!declared_temps.count(r_neg)) {
            emit(storage_prefix() + std::string("_Bool ") + r_neg + ";");
            declared_temps.insert(r_neg);
        }
        emit(l_neg + " = vx_ai_signbit((" + left_use + ").b, sizeof(" + out + ".b), " + sign_mask + ");");
        emit(r_neg + " = vx_ai_signbit((" + right_use + ").b, sizeof(" + out + ".b), " + sign_mask + ");");
        emit("if (" + l_neg + ") vx_ai_neg(" + l_abs + ".b, (" + left_use + ").b, sizeof(" + l_abs + ".b), " + top_mask + "); else " + l_abs + " = " + left_use + ";");
        emit("if (" + r_neg + ") vx_ai_neg(" + r_abs + ".b, (" + right_use + ").b, sizeof(" + r_abs + ".b), " + top_mask + "); else " + r_abs + " = " + right_use + ";");
        emit("vx_ai_udivmod(" + q + ".b, " + rem + ".b, " + l_abs + ".b, " + r_abs + ".b, sizeof(" + q + ".b), " + top_mask + ");");
        if (op == "/") {
            emit(out + " = " + q + ";");
            emit("if (" + l_neg + " != " + r_neg + ") vx_ai_neg(" + out + ".b, " + out + ".b, sizeof(" + out + ".b), " + top_mask + ");");
        } else {
            emit(out + " = " + rem + ";");
            emit("if (" + l_neg + ") vx_ai_neg(" + out + ".b, " + out + ".b, sizeof(" + out + ".b), " + top_mask + ");");
        }
        return out;
    }

    throw CompileError("Unsupported binary operator '" + op + "' for arbitrary-width integer", expr->location);
}

std::string CodeGenerator::gen_extint_cast(ExprPtr expr, const std::string& operand) {
    if (!expr || !expr->target_type || !expr->operand || !expr->operand->type) {
        throw CompileError("Internal error: invalid extint cast emission", expr ? expr->location : SourceLocation());
    }

    bool src_signed = false;
    uint64_t src_bits = 0;
    bool src_ext = analyze_extint_type(expr->operand->type, src_signed, src_bits);
    bool dst_signed = false;
    uint64_t dst_bits = 0;
    bool dst_ext = analyze_extint_type(expr->target_type, dst_signed, dst_bits);
    std::string operand_use = operand;
    if (src_ext) {
        std::string tmp = fresh_temp();
        std::string t = gen_type(expr->operand->type);
        if (!declared_temps.count(tmp)) {
            emit(storage_prefix() + t + " " + tmp + ";");
            declared_temps.insert(tmp);
        }
        emit(tmp + " = " + operand + ";");
        operand_use = tmp;
    }

    if (dst_ext) {
        ensure_extint_type(dst_signed, dst_bits);
        std::string out = fresh_temp();
        std::string out_type = gen_type(expr->target_type);
        if (!declared_temps.count(out)) {
            emit(storage_prefix() + out_type + " " + out + ";");
            declared_temps.insert(out);
        }
        if (src_ext) {
            ensure_extint_type(src_signed, src_bits);
            emit("vx_ai_cast(" + out + ".b, sizeof(" + out + ".b), " + std::to_string(extint_top_mask(dst_bits)) +
                 ", (" + operand_use + ").b, sizeof((" + operand_use + ").b), " +
                 std::string(src_signed ? "1" : "0") + ", " + std::to_string(extint_sign_mask(src_bits)) + ");");
            return out;
        }

        TypePtr src_ty = expr->operand->type;
        if (src_ty->kind == Type::Kind::Primitive && src_ty->primitive == PrimitiveType::Bool) {
            emit("vx_ai_from_u64(" + out + ".b, sizeof(" + out + ".b), " + std::to_string(extint_top_mask(dst_bits)) +
                 ", (uint64_t)((" + operand + ") ? 1u : 0u));");
            return out;
        }
        if (src_ty->kind == Type::Kind::Primitive && is_float(src_ty->primitive)) {
            std::string src_tmp = fresh_temp();
            std::string src_type = gen_type(src_ty);
            if (!declared_temps.count(src_tmp)) {
                emit(storage_prefix() + src_type + " " + src_tmp + ";");
                declared_temps.insert(src_tmp);
            }
            emit(src_tmp + " = " + operand + ";");
            emit("vx_ai_from_double_u(" + out + ".b, sizeof(" + out + ".b), " + std::to_string(extint_top_mask(dst_bits)) +
                 ", fabs((double)" + src_tmp + "));");
            emit("if (" + src_tmp + " < 0) vx_ai_neg(" + out + ".b, " + out + ".b, sizeof(" + out + ".b), " +
                 std::to_string(extint_top_mask(dst_bits)) + ");");
            return out;
        }
        if (src_ty->kind == Type::Kind::Primitive &&
            (src_ty->primitive == PrimitiveType::Int || src_ty->primitive == PrimitiveType::UInt)) {
            if (src_ty->primitive == PrimitiveType::Int) {
                emit("vx_ai_from_i64(" + out + ".b, sizeof(" + out + ".b), " + std::to_string(extint_top_mask(dst_bits)) +
                     ", (int64_t)(" + operand + "));");
            } else {
                emit("vx_ai_from_u64(" + out + ".b, sizeof(" + out + ".b), " + std::to_string(extint_top_mask(dst_bits)) +
                     ", (uint64_t)(" + operand + "));");
            }
            return out;
        }
        throw CompileError("Unsupported cast to arbitrary-width integer in C backend", expr->location);
    }

    if (src_ext) {
        ensure_extint_type(src_signed, src_bits);
        if (!expr->target_type || expr->target_type->kind != Type::Kind::Primitive) {
            throw CompileError("Unsupported cast from arbitrary-width integer in C backend", expr->location);
        }

        PrimitiveType p = expr->target_type->primitive;
        if (p == PrimitiveType::Bool) {
            return "(!vx_ai_is_zero((" + operand_use + ").b, sizeof((" + operand_use + ").b)))";
        }
        if (p == PrimitiveType::Int) {
            std::string base = "vx_ai_to_i64_trunc((" + operand_use + ").b, sizeof((" + operand_use + ").b), " +
                               std::to_string(extint_sign_mask(src_bits)) + ")";
            return "((" + gen_type(expr->target_type) + ")" + base + ")";
        }
        if (p == PrimitiveType::UInt) {
            std::string base = "vx_ai_to_u64_trunc((" + operand_use + ").b, sizeof((" + operand_use + ").b))";
            return "((" + gen_type(expr->target_type) + ")" + base + ")";
        }
        if (is_float(p)) {
            std::string mag = "vx_ai_to_double_u((" + operand_use + ").b, sizeof((" + operand_use + ").b))";
            if (src_signed) {
                std::string tmp = fresh_temp();
                std::string ttype = extint_type_name(true, src_bits);
                if (!declared_temps.count(tmp)) {
                    emit(storage_prefix() + ttype + " " + tmp + ";");
                    declared_temps.insert(tmp);
                }
                emit(tmp + " = " + operand_use + ";");
                emit("if (vx_ai_signbit(" + tmp + ".b, sizeof(" + tmp + ".b), " + std::to_string(extint_sign_mask(src_bits)) +
                     ")) vx_ai_neg(" + tmp + ".b, " + tmp + ".b, sizeof(" + tmp + ".b), " + std::to_string(extint_top_mask(src_bits)) + ");");
                mag = "(vx_ai_signbit((" + operand_use + ").b, sizeof((" + operand_use + ").b), " + std::to_string(extint_sign_mask(src_bits)) +
                      ") ? -vx_ai_to_double_u(" + tmp + ".b, sizeof(" + tmp + ".b)) : vx_ai_to_double_u(" + tmp + ".b, sizeof(" + tmp + ".b)))";
            }
            return "((" + gen_type(expr->target_type) + ")" + mag + ")";
        }
        throw CompileError("Unsupported cast from arbitrary-width integer in C backend", expr->location);
    }

    throw CompileError("Internal error: extint cast path called without arbitrary-width operand/target", expr->location);
}

std::string CodeGenerator::gen_extint_assignment(ExprPtr expr,
                                                 TypePtr lhs_type,
                                                 const std::string& lhs,
                                                 const std::string& rhs,
                                                 const std::string& assign_op) {
    bool is_signed = false;
    uint64_t bits = 0;
    if (!analyze_extint_type(lhs_type, is_signed, bits)) {
        throw CompileError("Internal error: extint assignment without extint lhs type", expr ? expr->location : SourceLocation());
    }
    ensure_extint_type(is_signed, bits);
    std::string lhs_ptr = fresh_temp();
    std::string lhs_type_str = gen_type(lhs_type);
    if (!declared_temps.count(lhs_ptr)) {
        emit(storage_prefix() + lhs_type_str + "* " + lhs_ptr + " = &(" + lhs + ");");
        declared_temps.insert(lhs_ptr);
    } else {
        emit(lhs_ptr + " = &(" + lhs + ");");
    }
    const std::string top_mask = std::to_string(extint_top_mask(bits));
    const std::string sign_mask = std::to_string(extint_sign_mask(bits));

    auto copy_result = [&]() -> std::string {
        std::string result_tmp = fresh_temp();
        if (!declared_temps.count(result_tmp)) {
            emit(storage_prefix() + lhs_type_str + " " + result_tmp + ";");
            declared_temps.insert(result_tmp);
        }
        emit(result_tmp + " = *" + lhs_ptr + ";");
        return result_tmp;
    };

    if (assign_op == "=") {
        emit("*" + lhs_ptr + " = " + rhs + ";");
        return copy_result();
    }
    std::string lhs_val = fresh_temp();
    if (!declared_temps.count(lhs_val)) {
        emit(storage_prefix() + lhs_type_str + " " + lhs_val + ";");
        declared_temps.insert(lhs_val);
    }
    emit(lhs_val + " = *" + lhs_ptr + ";");
    const bool lhs_is_fixed = lhs_type &&
                              lhs_type->kind == Type::Kind::Primitive &&
                              (lhs_type->primitive == PrimitiveType::FixedInt ||
                               lhs_type->primitive == PrimitiveType::FixedUInt);
    const int64_t lhs_frac = lhs_is_fixed ? lhs_type->fractional_bits : 0;
    if ((assign_op == "*=" || assign_op == "/=") && lhs_is_fixed && lhs_frac != 0) {
        std::string op = (assign_op == "*=") ? "*" : "/";
        auto fake = Expr::make_binary(op, expr ? expr->left : nullptr, expr ? expr->right : nullptr,
                                      expr ? expr->location : SourceLocation());
        fake->type = lhs_type;
        std::string value = gen_extint_binary(fake, lhs_val, rhs);
        emit("*" + lhs_ptr + " = " + value + ";");
        return copy_result();
    }
    if (assign_op == "+=") {
        emit("vx_ai_add(" + lhs_ptr + "->b, " + lhs_val + ".b, (" + rhs + ").b, sizeof(" + lhs_ptr + "->b), " + top_mask + ");");
        return copy_result();
    }
    if (assign_op == "-=") {
        emit("vx_ai_sub(" + lhs_ptr + "->b, " + lhs_val + ".b, (" + rhs + ").b, sizeof(" + lhs_ptr + "->b), " + top_mask + ");");
        return copy_result();
    }
    if (assign_op == "*=") {
        emit("vx_ai_mul(" + lhs_ptr + "->b, " + lhs_val + ".b, (" + rhs + ").b, sizeof(" + lhs_ptr + "->b), " + top_mask + ");");
        return copy_result();
    }
    if (assign_op == "&=") {
        emit("vx_ai_and(" + lhs_ptr + "->b, " + lhs_val + ".b, (" + rhs + ").b, sizeof(" + lhs_ptr + "->b), " + top_mask + ");");
        return copy_result();
    }
    if (assign_op == "|=") {
        emit("vx_ai_or(" + lhs_ptr + "->b, " + lhs_val + ".b, (" + rhs + ").b, sizeof(" + lhs_ptr + "->b), " + top_mask + ");");
        return copy_result();
    }
    if (assign_op == "^=") {
        emit("vx_ai_xor(" + lhs_ptr + "->b, " + lhs_val + ".b, (" + rhs + ").b, sizeof(" + lhs_ptr + "->b), " + top_mask + ");");
        return copy_result();
    }
    if (assign_op == "<<=") {
        std::string sh = extint_shift_amount_expr(rhs, expr && expr->right ? expr->right->type : nullptr,
                                                  expr ? expr->location : SourceLocation());
        emit("vx_ai_shl(" + lhs_ptr + "->b, " + lhs_val + ".b, sizeof(" + lhs_ptr + "->b), " + sh + ", " + top_mask + ");");
        return copy_result();
    }
    if (assign_op == ">>=") {
        std::string sh = extint_shift_amount_expr(rhs, expr && expr->right ? expr->right->type : nullptr,
                                                  expr ? expr->location : SourceLocation());
        if (is_signed) {
            emit("vx_ai_shr_s(" + lhs_ptr + "->b, " + lhs_val + ".b, sizeof(" + lhs_ptr + "->b), " + sh + ", " + top_mask + ", " + sign_mask + ");");
        } else {
            emit("vx_ai_shr_u(" + lhs_ptr + "->b, " + lhs_val + ".b, sizeof(" + lhs_ptr + "->b), " + sh + ", " + top_mask + ");");
        }
        return copy_result();
    }
    if (assign_op == "/=" || assign_op == "%=") {
        std::string q = (assign_op == "/=") ? "*"+lhs_ptr : fresh_temp();
        std::string r = (assign_op == "%=") ? "*"+lhs_ptr : fresh_temp();
        if (q != "*"+lhs_ptr && !declared_temps.count(q)) {
            emit(storage_prefix() + lhs_type_str + " " + q + ";");
            declared_temps.insert(q);
        }
        if (r != "*"+lhs_ptr && !declared_temps.count(r)) {
            emit(storage_prefix() + lhs_type_str + " " + r + ";");
            declared_temps.insert(r);
        }
        if (!is_signed) {
            std::string q_expr = (assign_op == "/=") ? lhs_ptr + "->b" : q + ".b";
            std::string r_expr = (assign_op == "%=") ? lhs_ptr + "->b" : r + ".b";
            emit("vx_ai_udivmod(" + q_expr + ", " + r_expr + ", " + lhs_val + ".b, (" + rhs + ").b, sizeof(" + lhs_ptr + "->b), " + top_mask + ");");
            return copy_result();
        }
        // Signed /= and %= reuse binary path by computing and assigning.
        std::string op = (assign_op == "/=") ? "/" : "%";
        auto fake = Expr::make_binary(op, expr ? expr->left : nullptr, expr ? expr->right : nullptr, expr ? expr->location : SourceLocation());
        fake->type = lhs_type;
        std::string value = gen_extint_binary(fake, lhs_val, rhs);
        emit("*" + lhs_ptr + " = " + value + ";");
        return copy_result();
    }
    throw CompileError("Unsupported compound assignment '" + assign_op + "' for arbitrary-width integer", expr ? expr->location : SourceLocation());
}

std::string CodeGenerator::gen_extint_conditional(ExprPtr expr,
                                                  const std::string& cond,
                                                  const std::string& true_expr,
                                                  const std::string& false_expr) {
    if (!expr || !expr->type || !is_extended_integer_type(expr->type)) {
        throw CompileError("Internal error: extint conditional emission without extint result type",
                           expr ? expr->location : SourceLocation());
    }
    std::string temp = fresh_temp();
    std::string t = gen_type(expr->type);
    if (!declared_temps.count(temp)) {
        emit(storage_prefix() + t + " " + temp + ";");
        declared_temps.insert(temp);
    }
    emit("if (" + cond + ") {");
    emit(temp + " = " + true_expr + ";");
    emit("} else {");
    emit(temp + " = " + false_expr + ";");
    emit("}");
    return temp;
}

} // namespace vexel::megalinker_codegen
