# Vexel Megalinker Backend (experimental)

This backend emits banked-style C output meant to be consumed by SDCC + Megalinker.
It is kept under `backends/ext` to exercise the backend plugin interface.

## Build
```
VEXEL_ROOT=../../.. make
```

Artifacts:
- `$(VEXEL_BUILD)/backends/megalinker/libvexel-megalinker.a`

## Usage
```
$(VEXEL_BUILD)/vexel -b megalinker -o out path/to/main.vx
```

Outputs:
- `out.h` (public header)
- `out__runtime.c` (runtime/shared directives)
- `megalinker/*.c` (one file per function + globals)

## Notes
- Frontend integers are parametric (`#iN`/`#uN`), but this backend currently accepts 8/16/32/64 widths for emitted C types.
- Exported (`&^`) functions get a `__nonbanked` wrapper that targets a page-A entry variant.
- Non-reentrant functions are alternated between page A and page B across the call graph.
- Immutable globals are emitted one-per-symbol under `megalinker/rom_<name>.c`; mutable/runtime-initialized globals go to `megalinker/ram_globals.c`.
- Dead declarations are not emitted; generated output contains only live globals/functions.
- Global linkage forms:
  - `!name:#T;` lowers as `extern` symbol declaration (top-level only).
  - `!!name:#T;` lowers as address-backed lvalue:
    - top-level: preprocessor dereference macro
    - local: `volatile <T>* const <name>__ptr` plus dereferenced uses
    - requires `[[addr(...)]]`/`[[address(...)]]` annotation.
- Bank-switching is emitted as direct segment assignments (no helper macros required).
- Inline-first policy:
  - Internal calls are hard-inlined by default (backend inlining, not C `inline`).
  - `[[noinline]]` on a function forces symbol emission and call-based lowering.
  - Recursive functions and functions with explicit `->` return statements are kept as calls.
  - Inlining limits are configurable:
    - `--inline-default <on|off>` (default `on`)
    - `--inline-max-cost <n>` (default `200`)
    - `--inline-max-depth <n>` (default `8`)
    - `--inline-max-expansions <n>` (default `64`)
- Caller explosion protection: if a function signature has too many distinct callers, calls are routed through a `__nonbanked` trampoline that saves/restores both pages. Default limit is 10 and can be configured with:
  - `--backend-opt caller_limit=<n>`
- Internal symbol prefixing (to avoid link collisions across multiple Vexel units in one C codebase):
  - `--internal-prefix <id>` when using the unified `vexel` driver
  - `--backend-opt internal_prefix=<id>`
  - Exported entrypoints/exports keep their public symbol names; only internal generated symbols are prefixed.
- Named-struct ABI policy:
  - External/imported function boundaries (`&!`) do not support named-struct types in parameters/receivers/returns.
  - Exported named-struct globals are supported.

## Reentrancy Contract (Key Behavior)
- This backend explicitly recognizes `[[nonreentrant]]` on ABI-boundary functions (`&^` exports and `&!` externals).
- Default boundary mode is reentrant for both entry and exit boundaries (`R/R`).
- `[[nonreentrant]]` overrides the boundary mode for the annotated entry/exit symbol.
- Internal call variants are selected from frontend-provided reentrancy analysis; backend codegen does not recompute the graph.
- Internal non-reentrant variants may use frame ABI; ABI boundaries remain native C ABI.

## SDCC Calling Convention Hint
- This backend recognizes `[[sdcccall(0)]]` and `[[sdcccall(1)]]` on ABI-visible functions only:
  - exports (`&^`)
  - extern/import boundaries (`&!`)
- The annotation is emitted as SDCC `__sdcccall(0|1)` on the generated ABI declaration/definition.
- Using `[[sdcccall(...)]]` on non-ABI functions is a backend error.

## Global Placement Hint
- This backend recognizes `[[nonbanked]]` on global variables to force RAM placement (skip ROM banking).

## Include path
Generated C files include `megalinker.h`. Make sure your C build includes:
```
-I/path/to/backends/ext/megalinker/include
```
