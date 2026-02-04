# Vexel Megalinker Backend (experimental)

This backend emits banked-style C output meant to be consumed by SDCC + Megalinker.
It is intentionally kept out-of-tree to exercise the backend plugin interface.

## Build
```
VEXEL_ROOT=../.. make
```

Artifacts:
- `$(VEXEL_BUILD)/backends/megalinker/libvexel-megalinker.a`
- `$(VEXEL_BUILD)/vexel-megalinker`

## Usage
```
$(VEXEL_BUILD)/vexel-megalinker -o out path/to/main.vx
```

Outputs:
- `out.h` (public header)
- `out__runtime.c` (runtime/shared directives)
- `megalinker/*.c` (one file per function + globals)

## Notes
- Exported (`&^`) functions get a `__nonbanked` wrapper that targets a page-A entry variant.
- Non-reentrant functions are alternated between page A and page B across the call graph.
- Immutable globals are emitted one-per-symbol under `megalinker/rom_<name>.c`; mutable/runtime-initialized globals go to `megalinker/ram_globals.c`.
- Bank-switching is emitted as direct segment assignments (no helper macros required).
- Caller explosion protection: if a function signature has too many distinct callers, calls are routed through a `__nonbanked` trampoline that saves/restores both pages. Default limit is 10 and can be configured with:
  - `--backend-opt caller_limit=<n>` (preferred)
  - `VEXEL_MEGALINKER_CALLER_LIMIT=<n>` (fallback)

## Include path
Generated C files include `megalinker.h`. Make sure your C build includes:
```
-I/path/to/ext/vexel_megalinker_backend/include
```
