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
    "$SCRIPT_DIR/noinl.c" "$SCRIPT_DIR/noinl.h" "$SCRIPT_DIR/noinl__runtime.c" \
    "$SCRIPT_DIR/sdcc.c" "$SCRIPT_DIR/sdcc.h" "$SCRIPT_DIR/sdcc__runtime.c" \
    "$SCRIPT_DIR/sdcccall_ok.vx" "$SCRIPT_DIR/sdcccall_bad_scope.vx" "$SCRIPT_DIR/sdcccall_bad_arg.vx" \
    "$SCRIPT_DIR/linkage.c" "$SCRIPT_DIR/linkage.h" "$SCRIPT_DIR/linkage__runtime.c" \
    "$SCRIPT_DIR/linkage.vx" "$SCRIPT_DIR/bad_width.vx" \
    "$SCRIPT_DIR/fixedabi.c" "$SCRIPT_DIR/fixedabi.h" "$SCRIPT_DIR/fixedabi__runtime.c" \
    "$SCRIPT_DIR/fixedabi.vx" \
    "$SCRIPT_DIR/fixedcast.c" "$SCRIPT_DIR/fixedcast.h" "$SCRIPT_DIR/fixedcast__runtime.c" \
    "$SCRIPT_DIR/fixedcast.vx" \
    "$SCRIPT_DIR/fixedops.c" "$SCRIPT_DIR/fixedops.h" "$SCRIPT_DIR/fixedops__runtime.c" \
    "$SCRIPT_DIR/fixedops.vx" \
    "$SCRIPT_DIR/fixedbit.c" "$SCRIPT_DIR/fixedbit.h" "$SCRIPT_DIR/fixedbit__runtime.c" \
    "$SCRIPT_DIR/fixedbit.vx" \
    "$SCRIPT_DIR/fixedbit72.c" "$SCRIPT_DIR/fixedbit72.h" "$SCRIPT_DIR/fixedbit72__runtime.c" \
    "$SCRIPT_DIR/fixedbit72.vx" \
    "$SCRIPT_DIR/fixedarith72.c" "$SCRIPT_DIR/fixedarith72.h" "$SCRIPT_DIR/fixedarith72__runtime.c" \
    "$SCRIPT_DIR/fixedarith72.vx" \
    "$SCRIPT_DIR/fixedarithfrac72.c" "$SCRIPT_DIR/fixedarithfrac72.h" "$SCRIPT_DIR/fixedarithfrac72__runtime.c" \
    "$SCRIPT_DIR/fixedarithfrac72.vx" \
    "$SCRIPT_DIR/fixedmul72.c" "$SCRIPT_DIR/fixedmul72.h" "$SCRIPT_DIR/fixedmul72__runtime.c" \
    "$SCRIPT_DIR/fixedmul72.vx" \
    "$SCRIPT_DIR/fixedmulfrac72.c" "$SCRIPT_DIR/fixedmulfrac72.h" "$SCRIPT_DIR/fixedmulfrac72__runtime.c" \
    "$SCRIPT_DIR/fixedmulfrac72.vx" \
    "$SCRIPT_DIR/fixedmul64.c" "$SCRIPT_DIR/fixedmul64.h" "$SCRIPT_DIR/fixedmul64__runtime.c" \
    "$SCRIPT_DIR/fixedmul64.vx" \
    "$SCRIPT_DIR/fixedbitfrac72.c" "$SCRIPT_DIR/fixedbitfrac72.h" "$SCRIPT_DIR/fixedbitfrac72__runtime.c" \
    "$SCRIPT_DIR/fixedbitfrac72.vx" \
    "$SCRIPT_DIR/fixedfloat.c" "$SCRIPT_DIR/fixedfloat.h" "$SCRIPT_DIR/fixedfloat__runtime.c" \
    "$SCRIPT_DIR/fixedfloat.vx" \
    "$SCRIPT_DIR/fixedcast72.c" "$SCRIPT_DIR/fixedcast72.h" "$SCRIPT_DIR/fixedcast72__runtime.c" \
    "$SCRIPT_DIR/fixedcast72.vx" \
    "$SCRIPT_DIR/fixedcastfrac72.c" "$SCRIPT_DIR/fixedcastfrac72.h" "$SCRIPT_DIR/fixedcastfrac72__runtime.c" \
    "$SCRIPT_DIR/fixedcastfrac72.vx" \
    "$SCRIPT_DIR/boolpack72.c" "$SCRIPT_DIR/boolpack72.h" "$SCRIPT_DIR/boolpack72__runtime.c" \
    "$SCRIPT_DIR/boolpack72.vx" \
    "$SCRIPT_DIR/fixedden.c" "$SCRIPT_DIR/fixedden.h" "$SCRIPT_DIR/fixedden__runtime.c" \
    "$SCRIPT_DIR/fixedden.vx" \
    "$SCRIPT_DIR/mathclass.c" "$SCRIPT_DIR/mathclass.h" "$SCRIPT_DIR/mathclass__runtime.c" \
    "$SCRIPT_DIR/bundled_std_math_classify.vx" \
    "$SCRIPT_DIR/mathovr.c" "$SCRIPT_DIR/mathovr.h" "$SCRIPT_DIR/mathovr__runtime.c" \
    "$SCRIPT_DIR/local_std_math_override.vx" \
    "$SCRIPT_DIR/arraytmp.c" "$SCRIPT_DIR/arraytmp.h" "$SCRIPT_DIR/arraytmp__runtime.c" \
    "$SCRIPT_DIR/arraytmp.vx"
  rm -rf "$SCRIPT_DIR/megalinker"
  rm -rf "$SCRIPT_DIR/std"
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

cat > "$SCRIPT_DIR/fixedabi.vx" <<'EOF'
&^wrap_u(v:#u8.8) -> #u8.8 { tmp:#u8.8 = v; tmp }
&^wrap_s(v:#i10.6) -> #i10.6 { tmp:#i10.6 = v; tmp }
&^main() -> #i32 { 0 }
EOF

if ! "$ROOT/build/vexel" -b megalinker -o fixedabi "$SCRIPT_DIR/fixedabi.vx" \
  >/tmp/megalinker_fixedabi.out 2>/tmp/megalinker_fixedabi.err; then
  cat /tmp/megalinker_fixedabi.out /tmp/megalinker_fixedabi.err
  echo "fixed-point ABI storage type regression case failed to compile"
  exit 1
fi
if ! rg -q "vx_wrap_u\\(uint16_t vx_v\\);" fixedabi.h; then
  echo "missing unsigned fixed-point raw storage wrapper signature in megalinker header"
  exit 1
fi
if ! rg -q "vx_wrap_s\\(int16_t vx_v\\);" fixedabi.h; then
  echo "missing signed fixed-point raw storage wrapper signature in megalinker header"
  exit 1
fi

cat > "$SCRIPT_DIR/fixedcast.vx" <<'EOF'
&^up_u(x:#u16.0) -> #u8.8 { (#u8.8)x }
&^down_u(x:#u8.8) -> #u16.0 { (#u16.0)x }
&^step_u(x:#u8.0) -> #u10.-2 { (#u10.-2)x }
&^unstep_u(x:#u10.-2) -> #u8.0 { (#u8.0)x }
&^nz_u(x:#u8.8) -> #b { (#b)x }
&^retag_s(x:#i8.8) -> #i10.6 { (#i10.6)x }
&^main() -> #i32 { 0 }
EOF

if ! "$ROOT/build/vexel" -b megalinker -o fixedcast "$SCRIPT_DIR/fixedcast.vx" \
  >/tmp/megalinker_fixedcast.out 2>/tmp/megalinker_fixedcast.err; then
  cat /tmp/megalinker_fixedcast.out /tmp/megalinker_fixedcast.err
  echo "fixed-point cast regression case failed to compile"
  exit 1
fi
if ! rg -q "vx_up_u\\(uint16_t vx_x\\);" fixedcast.h; then
  echo "missing fixed-point cast wrapper signature in megalinker header"
  exit 1
fi

cat > "$SCRIPT_DIR/fixedops.vx" <<'EOF'
&^acc_u(a:#u8.8, b:#u8.8, c:#u8.8) -> #u8.8 {
  x:#u8.8 = a;
  x *= b;
  x /= c;
  x %= b;
  x
}
&^acc_s(a:#i8.8, b:#i8.8, c:#i8.8) -> #i8.8 {
  x:#i8.8 = a;
  x *= b;
  x /= c;
  x %= b;
  x
}
&^main() -> #i32 { 0 }
EOF

if ! "$ROOT/build/vexel" -b megalinker -o fixedops "$SCRIPT_DIR/fixedops.vx" \
  >/tmp/megalinker_fixedops.out 2>/tmp/megalinker_fixedops.err; then
  cat /tmp/megalinker_fixedops.out /tmp/megalinker_fixedops.err
  echo "fixed-point mul/div/mod regression case failed to compile"
  exit 1
fi
if ! rg -q "vx_acc_u\\(uint16_t vx_a, uint16_t vx_b, uint16_t vx_c\\);" fixedops.h; then
  echo "missing fixed-point mul/div/mod wrapper signature in megalinker header"
  exit 1
fi

cat > "$SCRIPT_DIR/fixedbit.vx" <<'EOF'
&^bitmix(a:#u8.0, b:#u8.0, s:#u8.0) -> #u8.0 {
  x:#u8.0 = a;
  x &= b;
  x |= a;
  x ^= b;
  x <<= s;
  x >>= s;
  ~x
}
&^main() -> #i32 { 0 }
EOF

if ! "$ROOT/build/vexel" -b megalinker -o fixedbit "$SCRIPT_DIR/fixedbit.vx" \
  >/tmp/megalinker_fixedbit.out 2>/tmp/megalinker_fixedbit.err; then
  cat /tmp/megalinker_fixedbit.out /tmp/megalinker_fixedbit.err
  echo "fixed-point zero-fraction bitwise/shift regression case failed to compile"
  exit 1
fi
if ! rg -q "vx_bitmix\\(uint8_t vx_a, uint8_t vx_b, uint8_t vx_s\\);" fixedbit.h; then
  echo "missing fixed-point zero-fraction bitwise/shift wrapper signature in megalinker header"
  exit 1
fi

cat > "$SCRIPT_DIR/fixedbit72.vx" <<'EOF'
&^bitmix72(a:#u72.0, b:#u72.0, s:#u72.0) -> #u72.0 {
  x:#u72.0 = a;
  x = b;
  x &= a;
  x |= b;
  x ^= a;
  x <<= s;
  x >>= s;
  (~a) & x
}
&^main() -> #i32 { 0 }
EOF

if ! "$ROOT/build/vexel" -b megalinker -o fixedbit72 "$SCRIPT_DIR/fixedbit72.vx" \
  >/tmp/megalinker_fixedbit72.out 2>/tmp/megalinker_fixedbit72.err; then
  cat /tmp/megalinker_fixedbit72.out /tmp/megalinker_fixedbit72.err
  echo "non-native fixed-point zero-fraction bitwise/shift regression case failed to compile"
  exit 1
fi
if ! rg -q "^__nonbanked void vx_bitmix72\\(vx_u72_t\\* __vx_out, vx_u72_t\\* vx_a, vx_u72_t\\* vx_b, vx_u72_t\\* vx_s\\);" fixedbit72.h; then
  echo "missing non-native fixed-point zero-fraction bitwise/shift wrapper signature in megalinker header"
  exit 1
fi
if ! rg -q "vx_ai_and\\(" fixedbit72__runtime.c megalinker/*.c || \
   ! rg -q "vx_ai_shl\\(" fixedbit72__runtime.c megalinker/*.c || \
   ! rg -q "vx_ai_shr_u\\(" fixedbit72__runtime.c megalinker/*.c || \
   ! rg -q "vx_ai_not\\(" fixedbit72__runtime.c megalinker/*.c; then
  echo "missing extint helper lowering for non-native fixed-point bitwise/shift"
  exit 1
fi

cat > "$SCRIPT_DIR/fixedarith72.vx" <<'EOF'
&^arith72(a:#i72.0, b:#i72.0) -> #i72.0 {
  -a + b - a
}
&^cmp72(a:#i72.0, b:#i72.0) -> #b {
  (a + b) >= (a - b)
}
&^acc72(a:#u72.0, b:#u72.0) -> #u72.0 {
  x:#u72.0 = a;
  x = b;
  x += a;
  x -= b;
  x
}
&^main() -> #i32 { 0 }
EOF

if ! "$ROOT/build/vexel" -b megalinker -o fixedarith72 "$SCRIPT_DIR/fixedarith72.vx" \
  >/tmp/megalinker_fixedarith72.out 2>/tmp/megalinker_fixedarith72.err; then
  cat /tmp/megalinker_fixedarith72.out /tmp/megalinker_fixedarith72.err
  echo "non-native fixed-point zero-fraction arithmetic/comparison regression case failed to compile"
  exit 1
fi
if ! rg -q "vx_arith72" fixedarith72.h || ! rg -q "vx_cmp72" fixedarith72.h || ! rg -q "vx_acc72" fixedarith72.h; then
  echo "missing non-native fixed-point arithmetic/comparison wrapper declarations"
  exit 1
fi
if ! rg -q "vx_ai_add\\(" fixedarith72__runtime.c megalinker/*.c || \
   ! rg -q "vx_ai_sub\\(" fixedarith72__runtime.c megalinker/*.c || \
   ! rg -q "vx_ai_neg\\(" fixedarith72__runtime.c megalinker/*.c || \
   ! rg -q "vx_ai_(s|u)cmp\\(" fixedarith72__runtime.c megalinker/*.c; then
  echo "missing extint helper lowering for non-native fixed-point arithmetic/comparison"
  exit 1
fi

cat > "$SCRIPT_DIR/fixedarithfrac72.vx" <<'EOF'
&^arith72f(a:#i40.32, b:#i40.32) -> #i40.32 {
  -a + b - a
}
&^cmp72f(a:#i40.32, b:#i40.32) -> #b {
  (a + b) >= (a - b)
}
&^acc72f(a:#u40.32, b:#u40.32) -> #u40.32 {
  x:#u40.32 = a;
  x += b;
  x -= a;
  x
}
&^main() -> #i32 { 0 }
EOF

if ! "$ROOT/build/vexel" -b megalinker -o fixedarithfrac72 "$SCRIPT_DIR/fixedarithfrac72.vx" \
  >/tmp/megalinker_fixedarithfrac72.out 2>/tmp/megalinker_fixedarithfrac72.err; then
  cat /tmp/megalinker_fixedarithfrac72.out /tmp/megalinker_fixedarithfrac72.err
  echo "non-native fixed-point non-zero-fraction arithmetic/comparison regression case failed to compile"
  exit 1
fi
if ! rg -q "vx_arith72f" fixedarithfrac72.h || ! rg -q "vx_cmp72f" fixedarithfrac72.h || ! rg -q "vx_acc72f" fixedarithfrac72.h; then
  echo "missing non-native fixed-point non-zero-fraction wrapper declarations"
  exit 1
fi
if ! rg -q "vx_ai_add\\(" fixedarithfrac72__runtime.c megalinker/*.c || \
   ! rg -q "vx_ai_sub\\(" fixedarithfrac72__runtime.c megalinker/*.c || \
   ! rg -q "vx_ai_neg\\(" fixedarithfrac72__runtime.c megalinker/*.c || \
   ! rg -q "vx_ai_(s|u)cmp\\(" fixedarithfrac72__runtime.c megalinker/*.c; then
  echo "missing extint helper lowering for non-native fixed-point non-zero-fraction arithmetic/comparison"
  exit 1
fi

cat > "$SCRIPT_DIR/fixedmul72.vx" <<'EOF'
&^acc72_u(a:#u72.0, b:#u72.0, c:#u72.0) -> #u72.0 {
  x:#u72.0 = a;
  x = x * b;
  x = x / c;
  x = x % b;
  x *= b;
  x /= c;
  x %= b;
  x
}
&^acc72_s(a:#i72.0, b:#i72.0, c:#i72.0) -> #i72.0 {
  x:#i72.0 = a;
  x = x * b;
  x = x / c;
  x = x % b;
  x *= b;
  x /= c;
  x %= b;
  x
}
&^main() -> #i32 { 0 }
EOF

if ! "$ROOT/build/vexel" -b megalinker -o fixedmul72 "$SCRIPT_DIR/fixedmul72.vx" \
  >/tmp/megalinker_fixedmul72.out 2>/tmp/megalinker_fixedmul72.err; then
  cat /tmp/megalinker_fixedmul72.out /tmp/megalinker_fixedmul72.err
  echo "non-native fixed-point zero-fraction mul/div/mod regression case failed to compile"
  exit 1
fi
if ! rg -q "vx_acc72_u" fixedmul72.h || ! rg -q "vx_acc72_s" fixedmul72.h; then
  echo "missing non-native fixed-point mul/div/mod wrapper declarations"
  exit 1
fi
if ! rg -q "vx_ai_mul\\(" fixedmul72__runtime.c megalinker/*.c || \
   ! rg -q "vx_ai_udivmod\\(" fixedmul72__runtime.c megalinker/*.c; then
  echo "missing extint helper lowering for non-native fixed-point mul/div/mod"
  exit 1
fi

cat > "$SCRIPT_DIR/fixedmulfrac72.vx" <<'EOF'
&^acc72_u(a:#u40.32, b:#u40.32, c:#u40.32) -> #u40.32 {
  x:#u40.32 = a;
  x = x * b;
  x = x / c;
  x = x % b;
  x *= b;
  x /= c;
  x %= b;
  x
}
&^main() -> #i32 { 0 }
EOF

if ! "$ROOT/build/vexel" -b megalinker -o fixedmulfrac72 "$SCRIPT_DIR/fixedmulfrac72.vx" \
  >/tmp/megalinker_fixedmulfrac72.out 2>/tmp/megalinker_fixedmulfrac72.err; then
  cat /tmp/megalinker_fixedmulfrac72.out /tmp/megalinker_fixedmulfrac72.err
  echo "non-native fixed-point non-zero-fraction mul/div/mod regression case failed to compile"
  exit 1
fi
if ! rg -q "vx_acc72_u" fixedmulfrac72.h; then
  echo "missing non-native fixed-point non-zero-fraction mul/div/mod wrapper declarations"
  exit 1
fi
if ! rg -q "vx_ai_mul\\(" fixedmulfrac72__runtime.c megalinker/*.c || \
   ! rg -q "vx_ai_udivmod\\(" fixedmulfrac72__runtime.c megalinker/*.c || \
   ! rg -q "vx_ai_shr_u\\(" fixedmulfrac72__runtime.c megalinker/*.c; then
  echo "missing extint helper lowering for non-native fixed-point non-zero-fraction mul/div/mod"
  exit 1
fi

cat > "$SCRIPT_DIR/fixedmul64.vx" <<'EOF'
&^acc64(a:#u32.32, b:#u32.32, c:#u32.32) -> #u32.32 {
  x:#u32.32 = a;
  x = x * b;
  x = x / c;
  x = x % b;
  x *= b;
  x /= c;
  x %= b;
  x
}
&^main() -> #i32 { 0 }
EOF

if ! "$ROOT/build/vexel" -b megalinker -o fixedmul64 "$SCRIPT_DIR/fixedmul64.vx" \
  >/tmp/megalinker_fixedmul64.out 2>/tmp/megalinker_fixedmul64.err; then
  cat /tmp/megalinker_fixedmul64.out /tmp/megalinker_fixedmul64.err
  echo "native 64-bit fixed-point mul/div/mod regression case failed to compile"
  exit 1
fi
if ! rg -q "vx_acc64" fixedmul64.h; then
  echo "missing native 64-bit fixed-point mul/div/mod wrapper declarations"
  exit 1
fi
if ! rg -q "vx_ai_mul\\(" fixedmul64__runtime.c megalinker/*.c || \
   ! rg -q "vx_ai_udivmod\\(" fixedmul64__runtime.c megalinker/*.c; then
  echo "missing helper lowering for native 64-bit fixed-point mul/div/mod"
  exit 1
fi

cat > "$SCRIPT_DIR/fixedbitfrac72.vx" <<'EOF'
&^ops(a:#i40.8, b:#i40.8) -> #i40.8 {
  x:#i40.8 = a;
  x = x & b;
  x = x | b;
  x = x ^ b;
  x = ~x;
  x >>= b;
  x <<= b;
  x
}
&^main() -> #i32 { 0 }
EOF

if ! "$ROOT/build/vexel" -b megalinker -o fixedbitfrac72 "$SCRIPT_DIR/fixedbitfrac72.vx" \
  >/tmp/megalinker_fixedbitfrac72.out 2>/tmp/megalinker_fixedbitfrac72.err; then
  cat /tmp/megalinker_fixedbitfrac72.out /tmp/megalinker_fixedbitfrac72.err
  echo "signed fractional fixed-point bitwise/shift regression case failed to compile"
  exit 1
fi
if ! rg -q "vx_ops" fixedbitfrac72.h; then
  echo "missing signed fractional fixed-point bitwise/shift wrapper declarations"
  exit 1
fi
if ! rg -q "vx_ai_shr_s\\(" fixedbitfrac72__runtime.c megalinker/*.c || \
   ! rg -q "vx_ai_not\\(" fixedbitfrac72__runtime.c megalinker/*.c; then
  echo "missing helper lowering for signed fractional fixed-point bitwise/shift"
  exit 1
fi

cat > "$SCRIPT_DIR/fixedfloat.vx" <<'EOF'
&^to_fixed(x:#f64) -> #u8.8 { (#u8.8)x }
&^to_float(x:#u8.8) -> #f64 { (#f64)x }
&^main() -> #i32 { 0 }
EOF

if ! "$ROOT/build/vexel" -b megalinker -o fixedfloat "$SCRIPT_DIR/fixedfloat.vx" \
  >/tmp/megalinker_fixedfloat.out 2>/tmp/megalinker_fixedfloat.err; then
  cat /tmp/megalinker_fixedfloat.out /tmp/megalinker_fixedfloat.err
  echo "fixed-point float cast regression case failed to compile"
  exit 1
fi
if ! rg -q "vx_to_fixed\\(double vx_x\\);" fixedfloat.h; then
  echo "missing fixed-point float cast wrapper signature in megalinker header"
  exit 1
fi

cat > "$SCRIPT_DIR/fixedcast72.vx" <<'EOF'
&^cast_up(a:#u72.0) -> #u80.0 { (#u80.0)a }
&^cast_i(a:#i72.0) -> #i32 { (#i32)a }
&^cast_f(a:#u72.0) -> #f64 { (#f64)a }
&^from_f(a:#f64) -> #u72.0 { (#u72.0)a }
&^tail_byte(a:#u72.0) -> #u8 {
  bytes:#u8[9] = (#u8[9])a;
  bytes[8]
}
&^main() -> #i32 { 0 }
EOF

if ! "$ROOT/build/vexel" -b megalinker -o fixedcast72 "$SCRIPT_DIR/fixedcast72.vx" \
  >/tmp/megalinker_fixedcast72.out 2>/tmp/megalinker_fixedcast72.err; then
  cat /tmp/megalinker_fixedcast72.out /tmp/megalinker_fixedcast72.err
  echo "non-native fixed-point zero-fraction cast regression case failed to compile"
  exit 1
fi
if ! rg -q "vx_cast_up" fixedcast72.h || ! rg -q "vx_tail_byte" fixedcast72.h; then
  echo "missing non-native fixed-point cast wrapper declarations"
  exit 1
fi
if ! rg -q "vx_ai_cast\\(" fixedcast72__runtime.c megalinker/*.c || \
   ! rg -q "vx_ai_to_double_u\\(" fixedcast72__runtime.c megalinker/*.c || \
   ! rg -q "vx_ai_from_double_u\\(" fixedcast72__runtime.c megalinker/*.c || \
   ! rg -q "\\.b\\[" fixedcast72__runtime.c megalinker/*.c; then
  echo "missing extint helper lowering for non-native fixed-point casts"
  exit 1
fi

cat > "$SCRIPT_DIR/fixedcastfrac72.vx" <<'EOF'
&^to_u16_8(a:#u40.32) -> #u16.8 { (#u16.8)a }
&^roundtrip_u(a:#u40.32) -> #u40.32 { (#u40.32)((#u16.8)a) }
&^to_i24(a:#i40.32) -> #i24 { (#i24)a }
&^to_i40_32(a:#i24) -> #i40.32 { (#i40.32)a }
&^to_float(a:#i40.32) -> #f64 { (#f64)a }
&^from_float(a:#f64) -> #i40.32 { (#i40.32)a }
&^to_bool(a:#i40.32) -> #b { (#b)a }
&^to_u40(a:#i40.32) -> #u40 { (#u40)a }
&^main() -> #i32 { 0 }
EOF

if ! "$ROOT/build/vexel" -b megalinker -o fixedcastfrac72 "$SCRIPT_DIR/fixedcastfrac72.vx" \
  >/tmp/megalinker_fixedcastfrac72.out 2>/tmp/megalinker_fixedcastfrac72.err; then
  cat /tmp/megalinker_fixedcastfrac72.out /tmp/megalinker_fixedcastfrac72.err
  echo "non-native fixed-point non-zero-fraction cast regression case failed to compile"
  exit 1
fi
if ! rg -q "vx_roundtrip_u" fixedcastfrac72.h || ! rg -q "vx_from_float" fixedcastfrac72.h; then
  echo "missing non-native fixed-point non-zero-fraction cast wrapper declarations"
  exit 1
fi
if ! rg -q "vx_ai_shl\\(" fixedcastfrac72__runtime.c megalinker/*.c || \
   ! rg -q "vx_ai_shr_u\\(" fixedcastfrac72__runtime.c megalinker/*.c || \
   ! rg -q "vx_ai_from_double_u\\(" fixedcastfrac72__runtime.c megalinker/*.c || \
   ! rg -q "vx_ai_to_double_u\\(" fixedcastfrac72__runtime.c megalinker/*.c; then
  echo "missing extint helper lowering for non-native fixed-point non-zero-fraction casts"
  exit 1
fi

cat > "$SCRIPT_DIR/boolpack72.vx" <<'EOF'
&!seed() -> #b;
&^pack72() -> #u72 {
  bits:#b[72];
  i:#u8 = 0;
  (i < (#u8)72)@{
    bits[(#i32)i] = seed();
    i += (#u8)1;
  };
  (#u72)bits
}
&^main() -> #i32 { 0 }
EOF

if ! "$ROOT/build/vexel" -b megalinker -o boolpack72 "$SCRIPT_DIR/boolpack72.vx" \
  >/tmp/megalinker_boolpack72.out 2>/tmp/megalinker_boolpack72.err; then
  cat /tmp/megalinker_boolpack72.out /tmp/megalinker_boolpack72.err
  echo "bool-array to arbitrary-width integer cast regression case failed to compile"
  exit 1
fi
if ! rg -q "vx_pack72" boolpack72.h; then
  echo "missing bool-array arbitrary-width cast wrapper declaration"
  exit 1
fi
if ! rg -q "vx_ai_zero\\(" boolpack72__runtime.c megalinker/*.c || \
   ! rg -q "vx_ai_set_bit\\(" boolpack72__runtime.c megalinker/*.c; then
  echo "missing extint helper lowering for bool-array arbitrary-width cast"
  exit 1
fi

cat > "$SCRIPT_DIR/fixedden.vx" <<'EOF'
&^safe_div(a:#u100.-92, b:#u100.-92) -> #u100.-92 { a / b }
&^main() -> #i32 { 0 }
EOF

if ! "$ROOT/build/vexel" -b megalinker -o fixedden "$SCRIPT_DIR/fixedden.vx" \
  >/tmp/megalinker_fixedden.out 2>/tmp/megalinker_fixedden.err; then
  cat /tmp/megalinker_fixedden.out /tmp/megalinker_fixedden.err
  echo "fixed-point large-negative-frac division regression case failed to compile"
  exit 1
fi
if ! rg -q "== 0" fixedden__runtime.c megalinker/*.c || ! rg -q " / " fixedden__runtime.c megalinker/*.c; then
  echo "missing fixed-point division zero-guard path for large negative fractional width"
  exit 1
fi

cat > "$SCRIPT_DIR/arraytmp.vx" <<'EOF'
#Box(v:#i32);
&(lhs)#Box::.+(rhs:#Box) -> #Box { Box(lhs.v + rhs.v) }
&!seed() -> #i32;
&work() -> #i32 {
  base:#i32 = seed();
  xs:#i32[2] = [1, 2];
  off:#Box = Box(10);
  boxes:#Box[2] = [Box(base), Box(base + 1)];
  xs .+= base;
  boxes .+= off;
  xs[0] + boxes[1].v
}
&^main() -> #i32 {
  work()
}
EOF

if ! "$ROOT/build/vexel" -b megalinker -o arraytmp "$SCRIPT_DIR/arraytmp.vx" \
  >/tmp/megalinker_arraytmp.out 2>/tmp/megalinker_arraytmp.err; then
  cat /tmp/megalinker_arraytmp.out /tmp/megalinker_arraytmp.err
  echo "runtime array-literal temporary regression case failed to compile"
  exit 1
fi
if ! rg -q 'tmp[0-9]+\[[^]]+\][[:space:]]*=[[:space:]]*[{]tmp' arraytmp__runtime.c megalinker/*.c; then
  echo "array temporary regression case did not trigger runtime array literal temp emission"
  exit 1
fi
if rg -q '^[[:space:]]*static[[:space:]]+.*tmp[0-9]+\[[^]]+\][[:space:]]*=[[:space:]]*[{]tmp' arraytmp__runtime.c megalinker/*.c; then
  echo "runtime array literal temporaries must not use static aggregate initializers with runtime values"
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

cat > "$SCRIPT_DIR/sdcccall_ok.vx" <<'EOF'
[[sdcccall(0)]]
&!ext_add(a:#i32, b:#i32) -> #i32;

[[sdcccall(1)]]
&^entry(x:#i32) -> #i32 {
  ext_add(x, 1)
}

&^main() -> #i32 {
  entry(2)
}
EOF

if ! "$ROOT/build/vexel" -b megalinker -o sdcc "$SCRIPT_DIR/sdcccall_ok.vx" \
  >/tmp/megalinker_sdcccall_ok.out 2>/tmp/megalinker_sdcccall_ok.err; then
  cat /tmp/megalinker_sdcccall_ok.out /tmp/megalinker_sdcccall_ok.err
  echo "sdcccall ABI annotation case failed to compile"
  exit 1
fi
if ! rg -Uq "int32_t vx_ext_add\\([\\s\\S]*\\) __sdcccall\\(0\\);" sdcc.h; then
  echo "missing __sdcccall(0) on external function prototype"
  exit 1
fi
if ! rg -q "^__nonbanked int32_t vx_entry\\(int32_t vx_x\\) __sdcccall\\(1\\);" sdcc.h; then
  echo "missing __sdcccall(1) on exported wrapper prototype"
  exit 1
fi
if ! rg -q "^__nonbanked int32_t vx_entry\\(int32_t vx_x\\) __sdcccall\\(1\\) \\{" sdcc__runtime.c; then
  echo "missing __sdcccall(1) on exported wrapper definition"
  exit 1
fi

cat > "$SCRIPT_DIR/sdcccall_bad_scope.vx" <<'EOF'
[[sdcccall(1)]]
&helper(x:#i32) -> #i32 {
  x + 1
}

&^main() -> #i32 {
  helper(0)
}
EOF

if "$ROOT/build/vexel" -b megalinker -o sdcc "$SCRIPT_DIR/sdcccall_bad_scope.vx" \
  >/tmp/megalinker_sdcccall_bad_scope.out 2>/tmp/megalinker_sdcccall_bad_scope.err; then
  echo "sdcccall on non-ABI function should be rejected"
  exit 1
fi
if ! rg -q "\\[\\[sdcccall\\]\\] is only valid on ABI-visible functions" /tmp/megalinker_sdcccall_bad_scope.err; then
  cat /tmp/megalinker_sdcccall_bad_scope.out /tmp/megalinker_sdcccall_bad_scope.err
  echo "missing clear scope error for sdcccall"
  exit 1
fi

cat > "$SCRIPT_DIR/sdcccall_bad_arg.vx" <<'EOF'
[[sdcccall(2)]]
&^main() -> #i32 {
  0
}
EOF

if "$ROOT/build/vexel" -b megalinker -o sdcc "$SCRIPT_DIR/sdcccall_bad_arg.vx" \
  >/tmp/megalinker_sdcccall_bad_arg.out 2>/tmp/megalinker_sdcccall_bad_arg.err; then
  echo "sdcccall with invalid argument should be rejected"
  exit 1
fi
if ! rg -q "\\[\\[sdcccall\\]\\] argument must be 0 or 1" /tmp/megalinker_sdcccall_bad_arg.err; then
  cat /tmp/megalinker_sdcccall_bad_arg.out /tmp/megalinker_sdcccall_bad_arg.err
  echo "missing clear argument error for sdcccall"
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

cat > "$SCRIPT_DIR/linkage.vx" <<'EOF'
!PORT:#u8;
[[addr(0x4001)]]
!!CTRL:#u8;

&^main() -> #i32 {
  [[addr(0x4002)]]
  !!data:#u8;
  data = CTRL;
  (#i32)data + (#i32)PORT
}
EOF

if ! "$ROOT/build/vexel" -b megalinker -o linkage "$SCRIPT_DIR/linkage.vx" \
  >/tmp/megalinker_linkage.out 2>/tmp/megalinker_linkage.err; then
  cat /tmp/megalinker_linkage.out /tmp/megalinker_linkage.err
  echo "backend-bound linkage case failed to compile"
  exit 1
fi
if ! rg -q "^extern uint8_t vx_PORT;" megalinker/ram_globals.c; then
  echo "missing extern lowering for !PORT in ram globals unit"
  exit 1
fi
if ! rg -q "^#define vx_CTRL .*0x4001" megalinker/ram_globals.c; then
  echo "missing top-level backend-bound macro lowering"
  exit 1
fi
if ! rg -q "volatile uint8_t\\* const vx_data__ptr = \\(volatile uint8_t\\*\\)\\(uintptr_t\\)\\(0x4002\\);" megalinker/vx_main__reent__from_entry__pA.c; then
  echo "missing local backend-bound pointer lowering"
  exit 1
fi

cat > "$SCRIPT_DIR/bundled_std_math_classify.vx" <<'EOF'
::std::math;
&!seed() -> #f64;
&^main() -> #i32 {
  a:#b = std::math::isfinite(seed());
  b:#b = std::math::isnan(std::math::sqrt(seed()));
  (#i32)a + (#i32)b
}
EOF

if ! "$ROOT/build/vexel" -b megalinker -o mathclass "$SCRIPT_DIR/bundled_std_math_classify.vx" \
  >/tmp/megalinker_math_class.out 2>/tmp/megalinker_math_class.err; then
  cat /tmp/megalinker_math_class.out /tmp/megalinker_math_class.err
  echo "bundled std::math classification case failed to compile"
  exit 1
fi
if ! rg -q "isfinite\\(" mathclass__runtime.c megalinker/*.c; then
  echo "bundled std::math classification should map to libc isfinite"
  exit 1
fi
if ! rg -q "isnan\\(" mathclass__runtime.c megalinker/*.c; then
  echo "bundled std::math classification should map to libc isnan"
  exit 1
fi
if rg -q "vx_isfinite\\(" mathclass.h mathclass__runtime.c megalinker/*.c; then
  echo "bundled std::math classification should not use mangled vx_isfinite symbol"
  exit 1
fi
if rg -q "vx_isnan\\(" mathclass.h mathclass__runtime.c megalinker/*.c; then
  echo "bundled std::math classification should not use mangled vx_isnan symbol"
  exit 1
fi

mkdir -p "$SCRIPT_DIR/std"
cat > "$SCRIPT_DIR/std/math.vx" <<'EOF'
&!sqrt(x:#f64) -> #f64;
EOF

cat > "$SCRIPT_DIR/local_std_math_override.vx" <<'EOF'
::std::math;
&!seed() -> #f64;
&^main() -> #i32 {
  x:#f64 = std::math::sqrt(seed());
  (#i32)x
}
EOF

if ! "$ROOT/build/vexel" -b megalinker -o mathovr "$SCRIPT_DIR/local_std_math_override.vx" \
  >/tmp/megalinker_math_override.out 2>/tmp/megalinker_math_override.err; then
  cat /tmp/megalinker_math_override.out /tmp/megalinker_math_override.err
  echo "local std::math override case failed to compile"
  exit 1
fi
if ! rg -q "vx_sqrt\\(" mathovr.h mathovr__runtime.c megalinker/*.c; then
  echo "local std::math override should use mangled vx_sqrt symbol"
  exit 1
fi
if rg -q "double sqrt\\(" mathovr.h; then
  echo "local std::math override must not map to libc sqrt in header"
  exit 1
fi

cat > "$SCRIPT_DIR/bad_width.vx" <<'EOF'
&!seed() -> #u8;
&^main() -> #i32 {
  x:#u13 = (#u13)(seed());
  x <<= (#u8)3;
  (#i32)((#u8)x)
}
EOF

if ! "$ROOT/build/vexel" -b megalinker -o bad "$SCRIPT_DIR/bad_width.vx" \
  >/tmp/megalinker_bad_width.out 2>/tmp/megalinker_bad_width.err; then
  cat /tmp/megalinker_bad_width.out /tmp/megalinker_bad_width.err
  echo "arbitrary-width integer lowering should compile in megalinker backend"
  exit 1
fi
if ! rg --no-ignore -q "typedef struct \\{ unsigned char b\\[2\\]; \\} vx_u13_t;" megalinker/*.c; then
  echo "missing helper-backed #u13 type in megalinker function units"
  exit 1
fi
if ! rg -q "vx_ai_shl|vx_ai_udivmod|vx_ai_add" bad__runtime.c megalinker/*.c; then
  echo "missing arbitrary-width helper usage in megalinker output"
  exit 1
fi
if ! "$ROOT/build/vexel" -b megalinker --backend-opt internal_prefix=mltest_ -o bad "$SCRIPT_DIR/bad_width.vx" \
  >/tmp/megalinker_bad_width_pref.out 2>/tmp/megalinker_bad_width_pref.err; then
  cat /tmp/megalinker_bad_width_pref.out /tmp/megalinker_bad_width_pref.err
  echo "prefixed arbitrary-width lowering should compile"
  exit 1
fi
if ! rg --no-ignore -q "typedef struct \\{ unsigned char b\\[2\\]; \\} mltest_u13_t;" megalinker/*.c; then
  echo "internal prefix must apply to arbitrary-width helper typedef names"
  exit 1
fi
if ! rg --no-ignore -q "static void mltest_ai_shl\\(" bad__runtime.c megalinker/*.c; then
  echo "internal prefix must apply to arbitrary-width helper function names"
  exit 1
fi

echo "ok"

popd >/dev/null
