#pragma once

#include "ast.h"
#include "common.h"

#include <optional>
#include <string>

namespace vexel::megalinker_sdcccall {

inline std::optional<int> mode_for_function_decl(const StmtPtr& decl,
                                                 bool require_abi_visible,
                                                 const std::string& backend_name) {
    if (!decl || decl->kind != Stmt::Kind::FuncDecl) return std::nullopt;

    std::optional<int> mode;
    for (const auto& ann : decl->annotations) {
        if (ann.name != "sdcccall") continue;

        if (require_abi_visible && !decl->is_external && !decl->is_exported) {
            throw CompileError(backend_name +
                                   ": [[sdcccall]] is only valid on ABI-visible functions (&! and &^)",
                               ann.location);
        }
        if (ann.args.size() != 1) {
            throw CompileError(backend_name +
                                   ": [[sdcccall]] requires exactly one argument (0 or 1)",
                               ann.location);
        }

        const std::string& arg = ann.args[0];
        int parsed = -1;
        if (arg == "0") {
            parsed = 0;
        } else if (arg == "1") {
            parsed = 1;
        } else {
            throw CompileError(backend_name + ": [[sdcccall]] argument must be 0 or 1",
                               ann.location);
        }

        if (mode && *mode != parsed) {
            throw CompileError(backend_name +
                                   ": conflicting [[sdcccall]] annotations on the same function",
                               ann.location);
        }
        mode = parsed;
    }
    return mode;
}

inline std::string suffix_for_function_decl(const StmtPtr& decl,
                                            bool require_abi_visible,
                                            const std::string& backend_name) {
    std::optional<int> mode = mode_for_function_decl(decl, require_abi_visible, backend_name);
    if (!mode.has_value()) return "";
    return " __sdcccall(" + std::to_string(*mode) + ")";
}

} // namespace vexel::megalinker_sdcccall
