#pragma once
#include "analyzed_program.h"
#include "ast.h"
#include "analysis.h"
#include "evaluator.h"
#include <sstream>
#include <unordered_set>
#include <unordered_map>
#include <vector>
#include <stack>
#include <optional>
#include <functional>

namespace vexel::megalinker_codegen {

struct CCodegenResult {
    std::string header;
    std::string source;
};

struct GeneratedFunctionInfo {
    StmtPtr declaration;
    std::string qualified_name;  // e.g., Vec::push
    std::string c_name;          // mangled C symbol
    std::string storage;         // "" or "static "
    std::string code;            // complete function definition text
};

struct GeneratedVarInfo {
    StmtPtr declaration;
    const Symbol* symbol = nullptr;
    std::string code;
};

enum class PtrKind {
    Ram,
    Far,
};

struct CallTargetInfo {
    std::string name;
    std::string module_id_expr;
    char page = 'A';
    bool name_is_mangled = false;
};

struct CodegenABI {
    bool lower_aggregates = false;
    bool multi_file_globals = false;
    std::string return_prefix;
    std::string load_module_a_fn;
    std::string load_module_b_fn;
    std::string strlen_far_a_fn;
    std::string strlen_far_b_fn;
    std::function<PtrKind(const ExprPtr&)> expr_ptr_kind;
    std::function<PtrKind(const std::string& name, int scope_id)> symbol_ptr_kind;
    std::function<std::string(const std::string& func_key, char page)> func_module_id_expr;
    std::function<char(const std::string& func_key)> func_page;
    std::function<PtrKind(const std::string& func_key)> func_return_ptr_kind;
    std::function<std::string(const std::string& name, int scope_id, char current_page)> symbol_module_id_expr;
    std::function<std::string(const std::string& name, int scope_id, char current_page)> symbol_load_expr;
    std::function<CallTargetInfo(const ExprPtr& call_expr,
                                 const std::string& callee_name,
                                 const std::string& callee_key,
                                 const std::string& caller_variant_id,
                                 char caller_page,
                                 const std::string& ref_key)> resolve_call;
};

// CodeGenerator translates the type-checked AST into C code.
// Generates both header (.h) and source (.c) files with:
// - Type declarations and forward declarations
// - Function definitions with name mangling
// - Compile-time constant evaluation and dead branch elimination
// - Temporary variable management and reuse optimization
class CodeGenerator {
    std::ostringstream header;
    std::ostringstream body;
    std::vector<GeneratedFunctionInfo> generated_functions;
    std::vector<GeneratedVarInfo> generated_vars;
    int temp_counter;
    std::stack<std::string> available_temps;
    std::unordered_set<std::string> live_temps;
    std::unordered_set<std::string> declared_temps;
    std::unordered_map<std::string, std::string> type_map;
    const AnalyzedProgram* analyzed_program = nullptr;
    std::stack<std::ostringstream*> output_stack;
    std::unordered_map<std::string, std::string> comparator_cache;
    std::vector<std::string> comparator_definitions;
    bool in_function = false;
    AnalysisFacts facts;
    const OptimizationFacts* optimization = nullptr;
    char current_reentrancy_key = 'N';
    CodegenABI abi;
    std::string current_module_id_expr = "0";
    char current_bank_page = 'A';
    std::string current_func_key;
    std::string current_variant_id;
    std::string current_variant_name_override;
    int current_instance_id = -1;
    int entry_instance_id = 0;
    const Symbol* current_func_symbol = nullptr;

public:
    CodeGenerator();
    CCodegenResult generate(const Module& mod, const AnalyzedProgram& analyzed);
    GeneratedFunctionInfo generate_single_function(const Module& mod,
                                                   StmtPtr func,
                                                   const AnalyzedProgram& analyzed,
                                                   const CodegenABI& options,
                                                   int instance_id,
                                                   const std::string& ref_key,
                                                   char reent_key,
                                                   const std::string& variant_name_override,
                                                   const std::string& variant_id_override);
    void set_abi(const CodegenABI& options) { abi = options; }
    const std::unordered_set<const Symbol*>& reachable() const { return facts.reachable_functions; }
    const std::vector<GeneratedFunctionInfo>& functions() const { return generated_functions; }
    const std::vector<GeneratedVarInfo>& variables() const { return generated_vars; }
    std::string type_to_c(TypePtr type) { return gen_type(type); }
    std::string mangle(const std::string& name) { return mangle_name(name); }
    std::string mangle_export(const std::string& name) { return mangle_export_name(name); }
    void set_internal_symbol_prefix(const std::string& prefix) {
        internal_symbol_prefix = prefix.empty() ? "vx_" : prefix;
    }
private:
    std::unordered_set<std::string> current_ref_params;  // Track reference parameters in current function
    std::unordered_map<std::string, std::vector<TypePtr>> tuple_types;  // Track tuple types: name -> element types
    std::unordered_map<std::string, ExprPtr> expr_param_substitutions;  // Maps $param names to their expressions
    std::unordered_map<std::string, std::string> value_param_replacements;  // Maps value params when inlining
    std::string underscore_var;  // Current loop underscore variable name (empty when not in iteration)
    bool current_function_non_reentrant = false;
    bool current_returns_aggregate = false;
    bool current_nonreentrant_frame_abi = false;
    bool current_nonreentrant_returns_value = false;
    std::string current_nonreentrant_return_slot;
    std::string aggregate_out_param;
    std::string aggregate_out_type;
    std::unordered_set<std::string> current_aggregate_params;
    std::string internal_symbol_prefix = "vx_";

    void gen_module(const Module& mod);
    void gen_stmt(StmtPtr stmt);
    void gen_func_decl(StmtPtr stmt, const std::string& ref_key, char reent_key);
    void gen_type_decl(StmtPtr stmt);
    void gen_var_decl(StmtPtr stmt);

    std::string gen_expr(ExprPtr expr);
    std::string gen_binary(ExprPtr expr);
    std::string gen_unary(ExprPtr expr);
    std::string gen_call(ExprPtr expr);
    std::string gen_index(ExprPtr expr);
    std::string gen_member(ExprPtr expr);
    std::string gen_array_literal(ExprPtr expr);
    std::string gen_tuple_literal(ExprPtr expr);
    std::string gen_block(ExprPtr expr);
    std::string gen_block_optimized(ExprPtr expr);
    std::string gen_call_optimized_with_evaluator(ExprPtr expr, CompileTimeEvaluator& evaluator);
    std::string gen_conditional(ExprPtr expr);
    std::string gen_cast(ExprPtr expr);
    std::string gen_assignment(ExprPtr expr);
    std::string gen_range(ExprPtr expr);
    std::string gen_length(ExprPtr expr);
    std::string gen_iteration(ExprPtr expr);
    std::string gen_repeat(ExprPtr expr);

    bool is_compile_time_init(StmtPtr stmt) const;
    std::string mutability_prefix(StmtPtr stmt) const;
    std::string ref_variant_key(const ExprPtr& call, size_t ref_count) const;
    std::vector<std::string> ref_variant_keys_for(StmtPtr stmt) const;
    std::string ref_variant_name(const std::string& func_name, const std::string& ref_key) const;
    std::vector<char> reentrancy_keys_for(const Symbol* func_sym) const;
    std::string reentrancy_variant_name(const std::string& func_name, const Symbol* func_sym, char reent_key) const;
    std::string variant_name(const std::string& func_name, const Symbol* func_sym, char reent_key, const std::string& ref_key) const;
    bool receiver_is_mutable_arg(ExprPtr expr) const;
    bool try_evaluate(ExprPtr expr, CTValue& out) const;
    int fact_instance_id_for_expr(ExprPtr expr) const;
    int fact_instance_id_for_stmt(StmtPtr stmt) const;
    bool is_addressable_lvalue(ExprPtr expr) const;
    bool is_mutable_lvalue(ExprPtr expr) const;
    bool is_void_call(ExprPtr expr, std::string* name_out = nullptr) const;
    bool is_aggregate_type(TypePtr type) const;
    bool is_pointer_like(TypePtr type) const;
    TypePtr resolve_type(TypePtr type) const;
    TypePtr resolve_ref_param_type_or_fail(StmtPtr stmt, size_t index) const;
    PtrKind ptr_kind_for_expr(const ExprPtr& expr) const;
    PtrKind ptr_kind_for_symbol(const std::string& name, int scope_id) const;
    PtrKind ptr_kind_for_symbol(const Symbol* sym) const;
    std::string c_type_for_expr(ExprPtr expr);
    bool expr_has_side_effects(ExprPtr expr) const;
    std::string require_type(TypePtr type, const SourceLocation& loc, const std::string& context);
    std::string gen_type(TypePtr type);
    std::string mangle_name(const std::string& name);
    std::string mangle_export_name(const std::string& name);
    std::string mangle_name_with_prefix(const std::string& name, const std::string& prefix) const;
    std::string fresh_temp();
    void release_temp(const std::string& temp);

    std::optional<std::pair<int64_t, int64_t>> evaluate_range(ExprPtr range_expr);
    std::string storage_prefix() const;
    bool use_nonreentrant_frame_abi(bool is_exported) const;
    std::string nonreentrant_arg_slot_name(const std::string& c_name, size_t index) const;
    std::string nonreentrant_ret_slot_name(const std::string& c_name) const;
    std::string load_module_fn(char page) const;
    std::string strlen_far_fn(char page) const;
    std::string ensure_comparator(TypePtr type);
    int64_t resolve_array_length(TypePtr type, const SourceLocation& loc);
    void emit_return_stmt(const std::string& expr);
    void append_return_prefix(std::ostringstream& out) const;
    void validate_codegen_invariants(const Module& mod);
    void validate_codegen_invariants(StmtPtr func);
    void validate_codegen_invariants_impl(const std::vector<StmtPtr>& stmts, bool use_facts, bool top_level);

    void emit(const std::string& code);
    void emit_header(const std::string& code);

    struct VoidCallGuard {
        CodeGenerator& gen;
        bool prev;
        explicit VoidCallGuard(CodeGenerator& g, bool allow) : gen(g), prev(g.allow_void_call) {
            gen.allow_void_call = allow;
        }
        ~VoidCallGuard() { gen.allow_void_call = prev; }
    };

    struct FoldGuard {
        CodeGenerator& gen;
        bool prev;
        explicit FoldGuard(CodeGenerator& g, bool allow) : gen(g), prev(g.allow_constexpr_fold) {
            gen.allow_constexpr_fold = allow;
        }
        ~FoldGuard() { gen.allow_constexpr_fold = prev; }
    };

    bool allow_void_call = false;
    bool allow_constexpr_fold = true;
    Symbol* binding_for(const void* node) const;
    Symbol* binding_for(ExprPtr expr) const { return binding_for(expr.get()); }
    Symbol* binding_for(StmtPtr stmt) const { return binding_for(stmt.get()); }
    std::string instance_suffix(const Symbol* sym) const;
    std::string func_key_for(const Symbol* sym) const;
    int scope_id_for_symbol(const Symbol* sym) const;
};

}
