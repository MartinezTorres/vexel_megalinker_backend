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
- Exported (`&^`) functions get `_pageA`/`_pageB` entry points.
- Non-reentrant functions are alternated between page A and page B.
- Immutable globals are placed in `rom_globals_pageA.c`; mutable/runtime-initialized globals go to `ram_globals.c`.
- Floating-point and struct-by-value returns/params are rejected.
- Bank-switching macros are **not** injected yet; wrappers are direct calls.

## Include path
Generated C files include `megalinker.h`. Make sure your C build includes:
```
-I/path/to/ext/vexel_megalinker_backend/include
```
