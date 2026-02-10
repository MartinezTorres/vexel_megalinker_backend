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
    "$SCRIPT_DIR/pref.c" "$SCRIPT_DIR/pref.h" "$SCRIPT_DIR/pref__runtime.c" \
    "$SCRIPT_DIR/pref.analysis.txt"
  rm -rf "$SCRIPT_DIR/megalinker"
}
trap cleanup EXIT

if ! VEXEL_ROOT_DIR="$ROOT" make -s -C "$ROOT" driver >/tmp/driver_build.out 2>/tmp/driver_build.err; then
  cat /tmp/driver_build.out /tmp/driver_build.err
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
if [[ ! -f megalinker/rom_vx_G1.c || ! -f megalinker/rom_vx_G2.c ]]; then
  echo "missing rom globals"
  exit 1
fi
if [[ ! -f megalinker/ram_globals.c ]]; then
  echo "missing ram globals"
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

reader_file="$(rg --no-ignore -l "reader" megalinker 2>/dev/null | head -n 1 || true)"
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

if ! "$ROOT/build/vexel" -b megalinker --internal-prefix mltest_ -o pref input.vx \
  >/tmp/megalinker_compile_pref.out 2>/tmp/megalinker_compile_pref.err; then
  cat /tmp/megalinker_compile_pref.out /tmp/megalinker_compile_pref.err
  exit 1
fi
if ! rg -q "void mltest_load_module_id_a\\(uint8_t seg\\)" pref__runtime.c; then
  echo "missing prefixed runtime helper"
  exit 1
fi
if ! rg -q "^int32_t vx_entry_add\\(" pref__runtime.c; then
  echo "missing stable exported symbol with internal prefix option"
  exit 1
fi
if rg -q "^int32_t mltest_entry_add\\(" pref__runtime.c; then
  echo "internal prefix leaked into exported symbol"
  exit 1
fi

echo "ok"

popd >/dev/null
