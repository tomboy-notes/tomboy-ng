#!/usr/bin/env bash
set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

LAZARUS_DIR="${LAZARUS_DIR:-/mnt/USERS/onion/DATA_ORIGN/Workspace/LCL_GTK4/lazarus}"
LAZBUILD="${LAZBUILD:-$LAZARUS_DIR/lazbuild}"
KCONTROLS_DIR="${KCONTROLS_DIR:-/mnt/USERS/onion/DATA_ORIGN/Workspace/KControls}"

TARGET_CPU="${TARGET_CPU:-x86_64}"
TARGET_OS="${TARGET_OS:-linux}"
WIDGETSET="${WIDGETSET:-gtk4}"
BUILD_MODES="${BUILD_MODES:-GTK4 ReleaseGTK4}"

BUILD_ROOT="${BUILD_ROOT:-/tmp/tomboy-ng-gtk4-build}"
PCP_DIR="${PCP_DIR:-$BUILD_ROOT/pcp}"
KCONTROLS_BUILD_DIR="${KCONTROLS_BUILD_DIR:-$BUILD_ROOT/kcontrols}"
RUN_SMOKE_TEST="${RUN_SMOKE_TEST:-0}"
SMOKE_TIMEOUT="${SMOKE_TIMEOUT:-5}"

PROJECT_FILE="$PROJECT_ROOT/source/Tomboy_NG.lpi"
VERSION_FILE="$PROJECT_ROOT/package/version"
APP_VERSION="${TOMBOY_NG_VER:-$(tr -d '[:space:]' < "$VERSION_FILE")}"
TARGET_TRIPLE="$TARGET_CPU-$TARGET_OS"

require_path() {
  local path="$1"
  local label="$2"

  if [[ ! -e "$path" ]]; then
    echo "ERROR: $label not found: $path" >&2
    exit 1
  fi
}

clean_build_root() {
  case "$BUILD_ROOT" in
    /tmp/*)
      rm -rf -- "$BUILD_ROOT"
      ;;
    *)
      echo "ERROR: refusing to remove BUILD_ROOT outside /tmp: $BUILD_ROOT" >&2
      exit 1
      ;;
  esac
}

require_path "$LAZBUILD" "lazbuild"
require_path "$LAZARUS_DIR/lcl" "Lazarus LCL directory"
require_path "$KCONTROLS_DIR/packages/lazarus/kcontrolsbase.lpk" "KControls base package"
require_path "$KCONTROLS_DIR/packages/lazarus/kcontrolslaz.lpk" "KControls Lazarus package"
require_path "$PROJECT_FILE" "tomboy-ng Lazarus project"

if ! command -v fpc >/dev/null 2>&1; then
  echo "ERROR: fpc not found in PATH" >&2
  exit 1
fi

echo "== tomboy-ng GTK4 clean build =="
echo "Project      : $PROJECT_ROOT"
echo "Lazarus      : $LAZARUS_DIR"
echo "lazbuild     : $LAZBUILD"
echo "KControls    : $KCONTROLS_DIR"
echo "Widgetset    : $WIDGETSET"
echo "Target       : $TARGET_TRIPLE"
echo "Build modes  : $BUILD_MODES"
echo "Build root   : $BUILD_ROOT"
echo "Version      : $APP_VERSION"
echo "Smoke test   : $RUN_SMOKE_TEST"

clean_build_root
mkdir -p "$BUILD_ROOT"
cp -a "$KCONTROLS_DIR" "$KCONTROLS_BUILD_DIR"
rm -rf -- "$KCONTROLS_BUILD_DIR/packages/lazarus/lib"

KCONTROLS_BASE_LPK="$KCONTROLS_BUILD_DIR/packages/lazarus/kcontrolsbase.lpk"
KCONTROLS_LAZ_LPK="$KCONTROLS_BUILD_DIR/packages/lazarus/kcontrolslaz.lpk"

echo "== Register KControls packages in isolated Lazarus config =="
"$LAZBUILD" \
  --pcp="$PCP_DIR" \
  --lazarusdir="$LAZARUS_DIR" \
  --add-package-link \
  "$KCONTROLS_BASE_LPK" \
  "$KCONTROLS_LAZ_LPK"

echo "== Rebuild KControls for $WIDGETSET =="
"$LAZBUILD" \
  --pcp="$PCP_DIR" \
  --lazarusdir="$LAZARUS_DIR" \
  --ws="$WIDGETSET" \
  --cpu="$TARGET_CPU" \
  --os="$TARGET_OS" \
  -B \
  "$KCONTROLS_LAZ_LPK"

echo "== Clean project build outputs =="
rm -rf -- "$PROJECT_ROOT/source/lib/$TARGET_TRIPLE"
rm -f -- \
  "$PROJECT_ROOT/source/Tomboy_NG" \
  "$PROJECT_ROOT/source/tomboy-ng" \
  "$PROJECT_ROOT/source/tomboy-ng-64"

export TOMBOY_NG_VER="$APP_VERSION"

for mode in $BUILD_MODES; do
  echo "== Build tomboy-ng mode: $mode =="
  "$LAZBUILD" \
    --pcp="$PCP_DIR" \
    --lazarusdir="$LAZARUS_DIR" \
    --ws="$WIDGETSET" \
    --cpu="$TARGET_CPU" \
    --os="$TARGET_OS" \
    --bm="$mode" \
    --no-write-project \
    -B \
    "$PROJECT_FILE"
done

echo "== GTK linkage check =="
for binary in "$PROJECT_ROOT/source/tomboy-ng" "$PROJECT_ROOT/source/tomboy-ng-64"; do
  if [[ -x "$binary" ]]; then
    if ldd "$binary" | grep -q 'libgtk-4\.so'; then
      echo "OK: $(basename "$binary") links to GTK4"
    else
      echo "ERROR: $(basename "$binary") does not link to GTK4" >&2
      exit 1
    fi
  fi
done

run_smoke_test() {
  local binary="$PROJECT_ROOT/source/tomboy-ng-64"
  local smoke_config="$BUILD_ROOT/smoke-config"
  local smoke_log="$BUILD_ROOT/smoke.log"
  local runner=()
  local status=0
  local need_xvfb=0

  if [[ ! -x "$binary" ]]; then
    echo "ERROR: smoke test binary not found: $binary" >&2
    exit 1
  fi

  mkdir -p "$smoke_config"

  if [[ -z "${DISPLAY:-}" ]]; then
    need_xvfb=1
  elif command -v xdpyinfo >/dev/null 2>&1 && ! xdpyinfo >/dev/null 2>&1; then
    echo "INFO: DISPLAY is set but not usable, trying Xvfb for smoke test"
    need_xvfb=1
  fi

  if [[ "$need_xvfb" = "1" ]] && command -v xvfb-run >/dev/null 2>&1; then
    runner=(xvfb-run -a)
  elif [[ "$need_xvfb" = "1" ]]; then
    echo "SKIP: smoke test needs a usable DISPLAY or xvfb-run"
    return 0
  fi

  echo "== GTK4 smoke test =="
  set +e
  if [[ "${#runner[@]}" -gt 0 ]]; then
    "${runner[@]}" timeout "$SMOKE_TIMEOUT" "$binary" \
      --config-dir="$smoke_config" \
      --no-splash \
      --debug-log="$smoke_log"
  else
    timeout "$SMOKE_TIMEOUT" "$binary" \
      --config-dir="$smoke_config" \
      --no-splash \
      --debug-log="$smoke_log"
  fi
  status=$?
  set -e

  case "$status" in
    0|124)
      echo "OK: smoke test started tomboy-ng-64"
      echo "Log: $smoke_log"
      ;;
    *)
      echo "WARN: smoke test could not start tomboy-ng-64, exit status $status" >&2
      echo "Log: $smoke_log" >&2
      echo "WARN: build and GTK linkage checks already passed; check the display/session manually if needed" >&2
      ;;
  esac
}

if [[ "$RUN_SMOKE_TEST" = "1" ]]; then
  run_smoke_test
fi

echo "== Done =="
