#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="${VEXEL_ROOT_DIR:-}"
if [[ -z "$ROOT" ]]; then
  ROOT="$(cd "$SCRIPT_DIR/../../../.." && pwd)"
fi

cleanup() {
  rm -f "$SCRIPT_DIR/out.c" "$SCRIPT_DIR/out.h" "$SCRIPT_DIR/out__runtime.c" \
    "$SCRIPT_DIR/out.analysis.txt" \
    "$SCRIPT_DIR/bad.c" "$SCRIPT_DIR/bad.h" "$SCRIPT_DIR/bad__runtime.c" \
    "$SCRIPT_DIR/struct_global.c" "$SCRIPT_DIR/struct_global.h" "$SCRIPT_DIR/struct_global__runtime.c" \
    "$SCRIPT_DIR/bad_struct_import.vx" "$SCRIPT_DIR/struct_export_global.vx" \
    "$SCRIPT_DIR/wrap.c" "$SCRIPT_DIR/wrap.h" "$SCRIPT_DIR/wrap__runtime.c" \
    "$SCRIPT_DIR/wrap_void.vx" "$SCRIPT_DIR/wrap_struct.vx" \
    "$SCRIPT_DIR/pref.c" "$SCRIPT_DIR/pref.h" "$SCRIPT_DIR/pref__runtime.c" \
    "$SCRIPT_DIR/pref.analysis.txt" \
    "$SCRIPT_DIR/inline_default.vx" "$SCRIPT_DIR/inline_noinline.vx" \
    "$SCRIPT_DIR/inl.c" "$SCRIPT_DIR/inl.h" "$SCRIPT_DIR/inl__runtime.c" \
    "$SCRIPT_DIR/noinl.c" "$SCRIPT_DIR/noinl.h" "$SCRIPT_DIR/noinl__runtime.c"
  rm -rf "$SCRIPT_DIR/megalinker"
}
trap cleanup EXIT

if ! VEXEL_ROOT_DIR="$ROOT" make -s -C "$ROOT" driver >/tmp/driver_build.out 2>/tmp/driver_build.err; then
  cat /tmp/driver_build.out /tmp/driver_build.err
  exit 1
fi

if rg -q "CompileTimeEvaluator" "$ROOT/backends/ext/megalinker/src"; then
  echo "backend must not own compile-time evaluator semantics"
  exit 1
fi
if ! rg -q "backend received module without analysis facts" "$ROOT/backends/ext/megalinker/src/codegen.cpp"; then
  echo "missing analysis-facts contract guard"
  exit 1
fi
if ! rg -q "backend single-function emit missing analysis facts" "$ROOT/backends/ext/megalinker/src/codegen.cpp"; then
  echo "missing single-function analysis-facts contract guard"
  exit 1
fi
if rg -q '^static bool is_pointer_like\(|^static bool is_addressable_lvalue\(|^static bool is_mutable_lvalue\(|^static std::string ref_variant_key\(|^static std::string mutability_prefix\(' \
  "$ROOT/backends/ext/megalinker/src/megalinker_backend.cpp"; then
  echo "duplicate semantics helpers must not live in megalinker_backend.cpp"
  exit 1
fi
if ! rg -q 'megalinker_semantics::is_pointer_like_type\(' "$ROOT/backends/ext/megalinker/src/megalinker_backend.cpp"; then
  echo "megalinker backend must use shared local semantics helper for pointer-like checks"
  exit 1
fi

pushd "$SCRIPT_DIR" >/dev/null

if ! "$ROOT/build/vexel" -b megalinker --backend-opt caller_limit=1 -o out input.vx \
  >/tmp/megalinker_compile.out 2>/tmp/megalinker_compile.err; then
  cat /tmp/megalinker_compile.out /tmp/megalinker_compile.err
  exit 1
fi

if [[ ! -f out__runtime.c ]]; then
  echo "missing runtime output"
  exit 1
fi
if [[ ! -d megalinker ]]; then
  echo "missing megalinker output dir"
  exit 1
fi
has_rom_table1=0
has_rom_table2=0
[[ -f megalinker/rom_vx_TABLE1.c ]] && has_rom_table1=1
[[ -f megalinker/rom_vx_TABLE2.c ]] && has_rom_table2=1
if [[ $has_rom_table1 -ne $has_rom_table2 ]]; then
  echo "inconsistent rom globals emission"
  exit 1
fi
if [[ -f megalinker/rom_vx_TABLE_UNUSED.c ]]; then
  echo "unused rom global should be elided"
  exit 1
fi
if ! rg -q "extern [^;]*vx_grid\\[2\\]\\[3\\];" out.h; then
  echo "missing nested exported GRID shape in header"
  exit 1
fi
if ! rg -q "extern [^;]*vx_rgb\\[2\\]\\[2\\]\\[3\\];" out.h; then
  echo "missing nested exported RGB shape in header"
  exit 1
fi
if ! rg -q "vx_grid\\[2\\]\\[3\\]" megalinker/rom_vx_grid.c; then
  echo "missing nested GRID definition shape"
  exit 1
fi
if ! rg -q "vx_rgb\\[2\\]\\[2\\]\\[3\\]" megalinker/rom_vx_rgb.c; then
  echo "missing nested RGB definition shape"
  exit 1
fi
if [[ ! -f megalinker/ram_globals.c ]]; then
  echo "missing ram globals"
  exit 1
fi
if rg -q "\\bvx_TABLE_UNUSED\\b" out.h megalinker/*.c out__runtime.c; then
  echo "unused global symbol leaked into output"
  exit 1
fi

if ! rg -q "__tramp" out__runtime.c; then
  echo "missing trampoline"
  exit 1
fi
if ! rg --no-ignore -q "__tramp" megalinker; then
  echo "trampoline not used in call sites"
  exit 1
fi

reader_file="$(find megalinker -maxdepth 1 -type f -name 'vx_reader*.c' | head -n 1 || true)"
if [[ -z "$reader_file" ]]; then
  echo "reader function not found"
  exit 1
fi
if ! rg -U "vx_load_module_id_[ab]\\([^\\n]*\\);\\n\\s*return" "$reader_file" >/dev/null; then
  echo "missing restore before return"
  exit 1
fi

if rg -q "if \\(!?seg\\)" out__runtime.c; then
  echo "unexpected branch in load helper"
  exit 1
fi
if ! rg -q "__ML_SEGMENT_A_" out__runtime.c; then
  echo "missing segment A paging references"
  exit 1
fi
if ! rg --no-ignore -q "^// VEXEL: kind=function" megalinker; then
  echo "missing function trait comments"
  exit 1
fi
if rg -q "VX_REENTRANT|VX_NON_REENTRANT|VX_ENTRYPOINT|VX_INLINE|VX_NOINLINE|VX_PURE|VX_NO_GLOBAL_WRITE|VX_REF_MASK" out.h out__runtime.c megalinker/*.c; then
  echo "unexpected legacy function annotation macros"
  exit 1
fi

cat > "$SCRIPT_DIR/bad_struct_import.vx" <<'EOF'
#Vec(x:#i32, y:#i32);
&!scale(v:#Vec, k:#i32) -> #Vec;
&^main() -> #i32 {
  p:#Vec = Vec(3, 4);
  q:#Vec = scale(p, 2);
  q.x + q.y
}
EOF

if "$ROOT/build/vexel" -b megalinker -o bad "$SCRIPT_DIR/bad_struct_import.vx" \
  >/tmp/megalinker_bad.out 2>/tmp/megalinker_bad.err; then
  echo "named-struct external ABI should be rejected"
  exit 1
fi
if ! rg -q "Megalinker backend does not support named-struct" /tmp/megalinker_bad.err; then
  cat /tmp/megalinker_bad.out /tmp/megalinker_bad.err
  echo "missing clear named-struct external ABI error"
  exit 1
fi

cat > "$SCRIPT_DIR/struct_export_global.vx" <<'EOF'
#Pixel(r:#u8, g:#u8, b:#u8);
^palette:#Pixel[2] = [Pixel(1, 2, 3), Pixel(4, 5, 6)];
&^main() -> #i32 { 0 }
EOF

if ! "$ROOT/build/vexel" -b megalinker -o struct_global "$SCRIPT_DIR/struct_export_global.vx" \
  >/tmp/megalinker_struct_global.out 2>/tmp/megalinker_struct_global.err; then
  cat /tmp/megalinker_struct_global.out /tmp/megalinker_struct_global.err
  echo "exported named-struct globals should compile"
  exit 1
fi
if ! rg -q "extern [^;]*vx_Pixel vx_palette\\[2\\];" struct_global.h; then
  echo "missing exported named-struct global in header"
  exit 1
fi
if ! rg -q "vx_Pixel vx_palette\\[2\\]" megalinker/rom_vx_palette.c; then
  echo "missing exported named-struct global definition"
  exit 1
fi

cat > "$SCRIPT_DIR/wrap_void.vx" <<'EOF'
G:#i32;
[[noinline]]
&set1() { G = 1; }
&caller1() { set1(); }
&caller2() { set1(); }
&^main() -> #i32 {
  G = 0;
  caller1();
  caller2();
  G
}
EOF

if ! "$ROOT/build/vexel" -b megalinker --backend-opt caller_limit=1 -o wrap "$SCRIPT_DIR/wrap_void.vx" \
  >/tmp/megalinker_wrap_void.out 2>/tmp/megalinker_wrap_void.err; then
  cat /tmp/megalinker_wrap_void.out /tmp/megalinker_wrap_void.err
  echo "void trampoline case failed to compile"
  exit 1
fi
if ! rg -q "^__nonbanked int32_t vx_set1__tramp__reent\\(void\\)" wrap__runtime.c; then
  echo "missing canonical trampoline signature"
  exit 1
fi
if ! rg -q "int32_t result = vx_set1__reent__from_entry__pA\\(\\);" wrap__runtime.c; then
  echo "missing canonical trampoline return binding"
  exit 1
fi
if rg -q "^\\s*// VEXEL: kind=function" wrap__runtime.c; then
  echo "trampoline wrapper must not leak parsed trait comments into runtime"
  exit 1
fi

cat > "$SCRIPT_DIR/wrap_struct.vx" <<'EOF'
#Vec(x:#i32, y:#i32);
&^makev() -> #Vec {
  Vec(1, 2)
}
&^main() -> #i32 { 0 }
EOF

if ! "$ROOT/build/vexel" -b megalinker -o wrap "$SCRIPT_DIR/wrap_struct.vx" \
  >/tmp/megalinker_wrap_struct.out 2>/tmp/megalinker_wrap_struct.err; then
  cat /tmp/megalinker_wrap_struct.out /tmp/megalinker_wrap_struct.err
  echo "struct return wrapper case failed to compile"
  exit 1
fi
if ! rg -q "^__nonbanked void vx_makev\\(vx_Vec\\* __vx_out\\)" wrap__runtime.c; then
  echo "exported struct wrapper must use out-parameter signature"
  exit 1
fi
if rg -q "return vx_makev__reent__from_entry__pA\\(" wrap__runtime.c; then
  echo "exported struct wrapper must not return lowered aggregate call"
  exit 1
fi

if ! "$ROOT/build/vexel" -b megalinker --internal-prefix mltest_ -o pref input.vx \
  >/tmp/megalinker_compile_pref.out 2>/tmp/megalinker_compile_pref.err; then
  cat /tmp/megalinker_compile_pref.out /tmp/megalinker_compile_pref.err
  exit 1
fi
if ! rg -q "void mltest_load_module_id_a\\(uint8_t seg\\)" pref__runtime.c; then
  echo "missing prefixed runtime helper"
  exit 1
fi
if ! rg -q "^__nonbanked int32_t vx_entry_add\\(" pref__runtime.c; then
  echo "missing stable exported symbol with internal prefix option"
  exit 1
fi
if rg -q "^__nonbanked int32_t mltest_entry_add\\(" pref__runtime.c; then
  echo "internal prefix leaked into exported symbol"
  exit 1
fi

cat > "$SCRIPT_DIR/inline_default.vx" <<'EOF'
G:#i32;
&inline_target(x:#i32) -> #i32 { x + 1 }
&^main() -> #i32 {
  G = 41;
  inline_target(G)
}
EOF

if ! "$ROOT/build/vexel" -b megalinker -o inl "$SCRIPT_DIR/inline_default.vx" \
  >/tmp/megalinker_inline_default.out 2>/tmp/megalinker_inline_default.err; then
  cat /tmp/megalinker_inline_default.out /tmp/megalinker_inline_default.err
  echo "inline-default case failed to compile"
  exit 1
fi
if find megalinker -maxdepth 1 -type f -name 'vx_inline_target*.c' | grep -q .; then
  echo "inline-default should not emit a standalone inline_target function file"
  exit 1
fi
if rg -q "vx_inline_target" inl.h inl__runtime.c megalinker/*.c; then
  echo "inline-default should eliminate direct symbol usage for inline_target"
  exit 1
fi

cat > "$SCRIPT_DIR/inline_noinline.vx" <<'EOF'
G:#i32;
[[noinline]]
&forced_target(x:#i32) -> #i32 { x + 1 }
&^main() -> #i32 {
  G = 41;
  forced_target(G)
}
EOF

if ! "$ROOT/build/vexel" -b megalinker -o noinl "$SCRIPT_DIR/inline_noinline.vx" \
  >/tmp/megalinker_inline_noinline.out 2>/tmp/megalinker_inline_noinline.err; then
  cat /tmp/megalinker_inline_noinline.out /tmp/megalinker_inline_noinline.err
  echo "noinline case failed to compile"
  exit 1
fi
if ! find megalinker -maxdepth 1 -type f -name 'vx_forced_target*.c' | grep -q .; then
  echo "noinline function must emit a standalone function file"
  exit 1
fi
if ! rg -q "vx_forced_target" noinl__runtime.c megalinker/*.c; then
  echo "noinline function symbol should be referenced in generated output"
  exit 1
fi

echo "ok"

popd >/dev/null
