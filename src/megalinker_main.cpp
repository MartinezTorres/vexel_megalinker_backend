#include "megalinker_backend.h"
#include "compiler.h"
#include "lexer.h"
#include "parser.h"
#include "typechecker.h"
#include "lowered_printer.h"
#include <filesystem>
#include <iostream>
#include <cstring>

// Dedicated CLI for the Megalinker backend.
static void print_usage(const char* prog) {
    std::cout << "Vexel Compiler (Megalinker backend)\n";
    std::cout << "Usage: " << prog << " [options] <input.vx>\n\n";
    std::cout << "Options:\n";
    std::cout << "  -o <path>    Output path (base name for generated files, default: out)\n";
    std::cout << "  -b <name>    Backend (optional compatibility flag: accepts megalinker only)\n";
    std::cout << "  -L           Emit lowered Vexel subset alongside backend output\n";
    std::cout << "  --emit-analysis Emit analysis report alongside backend output\n";
    std::cout << "  --allow-process Enable process expressions (executes host commands; disabled by default)\n";
    std::cout << "  --caller-limit <n> Caller-variant limit before trampoline fallback\n";
    std::cout << "  --internal-prefix <id> Prefix for internal generated symbols\n";
    std::cout << "  --backend-opt <k=v> Backend-specific option (repeatable)\n";
    std::cout << "  -v           Verbose output\n";
    std::cout << "  -h           Show this help\n";
}

int main(int argc, char** argv) {
    vexel::register_backend_megalinker();

    vexel::Compiler::Options opts;
    opts.output_file = "out";
    opts.backend = "megalinker";

    for (int i = 1; i < argc; i++) {
        if (std::strcmp(argv[i], "-h") == 0 || std::strcmp(argv[i], "--help") == 0) {
            print_usage(argv[0]);
            return 0;
        } else if (std::strcmp(argv[i], "-v") == 0) {
            opts.verbose = true;
        } else if (std::strcmp(argv[i], "-L") == 0 || std::strcmp(argv[i], "--emit-lowered") == 0) {
            opts.emit_lowered = true;
        } else if (std::strcmp(argv[i], "--emit-analysis") == 0) {
            opts.emit_analysis = true;
        } else if (std::strcmp(argv[i], "--allow-process") == 0) {
            opts.allow_process = true;
        } else if (std::strcmp(argv[i], "--caller-limit") == 0 || std::strncmp(argv[i], "--caller-limit=", 15) == 0) {
            const char* value = nullptr;
            if (std::strncmp(argv[i], "--caller-limit=", 15) == 0) {
                value = argv[i] + 15;
            } else if (i + 1 < argc) {
                value = argv[++i];
            } else {
                std::cerr << "Error: --caller-limit requires an argument\n";
                return 1;
            }
            opts.backend_options["caller_limit"] = value;
        } else if (std::strcmp(argv[i], "--internal-prefix") == 0 || std::strncmp(argv[i], "--internal-prefix=", 18) == 0) {
            const char* value = nullptr;
            if (std::strncmp(argv[i], "--internal-prefix=", 18) == 0) {
                value = argv[i] + 18;
            } else if (i + 1 < argc) {
                value = argv[++i];
            } else {
                std::cerr << "Error: --internal-prefix requires an argument\n";
                return 1;
            }
            opts.backend_options["internal_prefix"] = value;
        } else if (std::strcmp(argv[i], "--backend-opt") == 0 || std::strncmp(argv[i], "--backend-opt=", 14) == 0) {
            const char* opt = nullptr;
            if (std::strncmp(argv[i], "--backend-opt=", 14) == 0) {
                opt = argv[i] + 14;
            } else if (i + 1 < argc) {
                opt = argv[++i];
            } else {
                std::cerr << "Error: --backend-opt requires an argument\n";
                return 1;
            }
            const char* eq = std::strchr(opt, '=');
            if (!eq || eq == opt || *(eq + 1) == '\0') {
                std::cerr << "Error: --backend-opt expects key=value\n";
                return 1;
            }
            std::string key(opt, eq - opt);
            std::string value(eq + 1);
            opts.backend_options[key] = value;
        } else if (std::strcmp(argv[i], "-o") == 0) {
            if (i + 1 < argc) {
                opts.output_file = argv[++i];
            } else {
                std::cerr << "Error: -o requires an argument\n";
                return 1;
            }
        } else if (std::strcmp(argv[i], "-b") == 0 || std::strcmp(argv[i], "--backend") == 0) {
            if (i + 1 < argc) {
                const char* backend = argv[++i];
                if (std::strcmp(backend, "megalinker") != 0) {
                    std::cerr << "Error: megalinker CLI only supports backend=megalinker\n";
                    return 1;
                }
            } else {
                std::cerr << "Error: -b/--backend requires an argument\n";
                return 1;
            }
        } else if (argv[i][0] == '-') {
            std::cerr << "Error: Unknown option: " << argv[i] << "\n";
            return 1;
        } else {
            opts.input_file = argv[i];
        }
    }

    if (opts.input_file.empty()) {
        std::cerr << "Error: No input file specified\n";
        print_usage(argv[0]);
        return 1;
    }

    try {
        vexel::Compiler compiler(opts);
        (void)compiler.compile();
        return 0;
    } catch (const vexel::CompileError& e) {
        std::cerr << "Error";
        if (!e.location.filename.empty()) {
            std::cerr << " at " << e.location.filename << ":" << e.location.line << ":" << e.location.column;
        }
        std::cerr << ": " << e.what() << "\n";
        return 1;
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << "\n";
        return 1;
    }
}
