# Vexel Megalinker Backend (experimental)

This backend emits banked-style C output meant to be consumed by SDCC + Megalinker.
It is intentionally kept out-of-tree to exercise the backend plugin interface.

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
- Exported (`&^`) functions get a `__nonbanked` wrapper that targets a page-A entry variant.
- Non-reentrant functions are alternated between page A and page B across the call graph.
- Immutable globals are emitted one-per-symbol under `megalinker/rom_<name>.c`; mutable/runtime-initialized globals go to `megalinker/ram_globals.c`.
- Dead declarations are not emitted; generated output contains only live globals/functions.
- Bank-switching is emitted as direct segment assignments (no helper macros required).
- Caller explosion protection: if a function signature has too many distinct callers, calls are routed through a `__nonbanked` trampoline that saves/restores both pages. Default limit is 10 and can be configured with:
  - `--backend-opt caller_limit=<n>`
- Internal symbol prefixing (to avoid link collisions across multiple Vexel units in one C codebase):
  - `--internal-prefix <id>` when using the unified `vexel` driver
  - `--backend-opt internal_prefix=<id>`
  - Exported entrypoints/exports keep their public symbol names; only internal generated symbols are prefixed.

## Reentrancy Contract (Key Behavior)
- This backend explicitly recognizes `[[nonreentrant]]` on ABI-boundary functions (`&^` exports and `&!` externals).
- Default boundary mode is reentrant for both entry and exit boundaries (`R/R`).
- `[[nonreentrant]]` overrides the boundary mode for the annotated entry/exit symbol.
- Internal call variants are selected from frontend-provided reentrancy analysis; backend codegen does not recompute the graph.
- Internal non-reentrant variants may use frame ABI; ABI boundaries remain native C ABI.

## Global Placement Hint
- This backend recognizes `[[nonbanked]]` on global variables to force RAM placement (skip ROM banking).

## Include path
Generated C files include `megalinker.h`. Make sure your C build includes:
```
-I/path/to/backends/ext/megalinker/include
```
