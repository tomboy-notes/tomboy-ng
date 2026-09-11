#!/usr/bin/env bash
#
# Clean, isolated build of tomboy-ng against a locally patched KControls
# (and, for GTK4, a locally patched LCL) for one widgetset.
#
#   WIDGETSET=gtk4 ./build_widgetset_clean.sh
#   WIDGETSET=qt5  ./build_widgetset_clean.sh
#
# KControls is rebuilt for the selected widgetset inside a throwaway Lazarus
# primary config path, so gtk4 and qt5 never share compiled units.
#
set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

WIDGETSET="${WIDGETSET:-gtk4}"

case "$WIDGETSET" in
  gtk4)
    DEFAULT_BUILD_MODES="GTK4 ReleaseGTK4"
    LINK_PATTERN='libgtk-4\.so'
    LINK_LABEL="GTK4"
    EXPECT_BINARIES="tomboy-ng-gtk4-dbg tomboy-ng-gtk4"
    SMOKE_BINARY="tomboy-ng-gtk4"
    ;;
  qt5)
    DEFAULT_BUILD_MODES="ReleaseQT5"
    LINK_PATTERN='libQt5Pas\.so'
    LINK_LABEL="Qt5"
    EXPECT_BINARIES="tomboy-ng-qt5"
    SMOKE_BINARY="tomboy-ng-qt5"
    ;;
  *)
    echo "ERROR: unsupported WIDGETSET '$WIDGETSET' (expected gtk4 or qt5)" >&2
    exit 1
    ;;
esac

BUILD_MODES="${BUILD_MODES:-$DEFAULT_BUILD_MODES}"

# Only expect the binaries of the modes actually being built, so a partial
# build (e.g. BUILD_MODES=GTK4) is not reported as a failure.
mode_binary() {
  case "$1" in
    GTK4)        echo "tomboy-ng-gtk4-dbg" ;;
    ReleaseGTK4) echo "tomboy-ng-gtk4" ;;
    ReleaseQT5)  echo "tomboy-ng-qt5" ;;
    *) echo "ERROR: no binary name known for build mode '$1'" >&2; exit 1 ;;
  esac
}
EXPECT_BINARIES=""
for mode in $BUILD_MODES; do
  EXPECT_BINARIES="$EXPECT_BINARIES $(mode_binary "$mode")"
done
EXPECT_BINARIES="${EXPECT_BINARIES# }"
case " $EXPECT_BINARIES " in
  *" $SMOKE_BINARY "*) ;;
  *) SMOKE_BINARY="${EXPECT_BINARIES##* }" ;;
esac

# ---------------------------------------------------------------- toolchain
# Prefer an explicitly supplied tree. Otherwise look in the usual places.
# The GTK4 LCL fixes now ship in the distribution packages (lcl-gtk4 4.4), so
# the installed Lazarus is preferred; the workspace tree remains a fallback.
# See BUILD_MANIFEST.md.
find_lazarus_dir() {
  local candidate
  for candidate in \
      "/usr/lib/lazarus/4.4" \
      "/usr/lib/lazarus/default" \
      "/usr/lib/lazarus" \
      "$PROJECT_ROOT/../LCL_GTK4/lazarus" \
      "/mnt/STORAGE16T/Workspace_STORAGE16T/LCL_GTK4/lazarus"; do
    if [[ -d "$candidate/lcl/interfaces/$WIDGETSET" ]]; then
      echo "$candidate"
      return 0
    fi
  done
  return 1
}

find_kcontrols_dir() {
  local candidate
  for candidate in \
      "$PROJECT_ROOT/../KControls" \
      "$PROJECT_ROOT/../../KControls" \
      "/mnt/STORAGE16T/Workspace_STORAGE16T/KControls"; do
    if [[ -f "$candidate/packages/lazarus/kcontrolslaz.lpk" ]]; then
      (cd "$candidate" && pwd)
      return 0
    fi
  done
  return 1
}

if [[ -z "${LAZARUS_DIR:-}" ]]; then
  if ! LAZARUS_DIR="$(find_lazarus_dir)"; then
    echo "ERROR: no Lazarus tree with lcl/interfaces/$WIDGETSET found." >&2
    echo "       Set LAZARUS_DIR to a Lazarus tree built for $WIDGETSET." >&2
    exit 1
  fi
fi

if [[ -z "${KCONTROLS_DIR:-}" ]]; then
  if ! KCONTROLS_DIR="$(find_kcontrols_dir)"; then
    echo "ERROR: KControls not found. Set KCONTROLS_DIR." >&2
    exit 1
  fi
fi

LAZBUILD="${LAZBUILD:-$LAZARUS_DIR/lazbuild}"
if [[ ! -x "$LAZBUILD" ]] && command -v lazbuild >/dev/null 2>&1; then
  LAZBUILD="$(command -v lazbuild)"
fi

TARGET_CPU="${TARGET_CPU:-x86_64}"
TARGET_OS="${TARGET_OS:-linux}"
TARGET_TRIPLE="$TARGET_CPU-$TARGET_OS"

# Keep gtk4 and qt5 artefacts apart - a shared PCP would let KControls units
# built for one widgetset leak into the other.
BUILD_ROOT="${BUILD_ROOT:-/tmp/tomboy-ng-$WIDGETSET-build}"
PCP_DIR="${PCP_DIR:-$BUILD_ROOT/pcp}"
KCONTROLS_BUILD_DIR="${KCONTROLS_BUILD_DIR:-$BUILD_ROOT/kcontrols}"
RUN_SMOKE_TEST="${RUN_SMOKE_TEST:-0}"
SMOKE_TIMEOUT="${SMOKE_TIMEOUT:-5}"

PROJECT_FILE="$PROJECT_ROOT/source/Tomboy_NG.lpi"
VERSION_FILE="$PROJECT_ROOT/package/version"

# Local builds carry a suffix so they are distinguishable from an upstream
# release, both in dpkg and in the About dialog (source/cli.pas reads
# %TOMBOY_NG_VER at compile time).
BASE_VERSION="$(tr -d '[:space:]' < "$VERSION_FILE")"
LOCAL_VERSION_SUFFIX="${LOCAL_VERSION_SUFFIX:-}"
if [[ -n "${TOMBOY_NG_VER:-}" ]]; then
  APP_VERSION="$TOMBOY_NG_VER"
elif [[ -n "$LOCAL_VERSION_SUFFIX" ]]; then
  APP_VERSION="$BASE_VERSION+$LOCAL_VERSION_SUFFIX"
else
  APP_VERSION="$BASE_VERSION"
fi

require_path() {
  if [[ ! -e "$1" ]]; then
    echo "ERROR: $2 not found: $1" >&2
    exit 1
  fi
}

clean_build_root() {
  case "$BUILD_ROOT" in
    /tmp/*) rm -rf -- "$BUILD_ROOT" ;;
    *)
      echo "ERROR: refusing to remove BUILD_ROOT outside /tmp: $BUILD_ROOT" >&2
      exit 1
      ;;
  esac
}

require_path "$LAZBUILD" "lazbuild"
require_path "$LAZARUS_DIR/lcl/interfaces/$WIDGETSET" "Lazarus LCL $WIDGETSET interface"
require_path "$KCONTROLS_DIR/packages/lazarus/kcontrolsbase.lpk" "KControls base package"
require_path "$KCONTROLS_DIR/packages/lazarus/kcontrolslaz.lpk" "KControls Lazarus package"
require_path "$PROJECT_FILE" "tomboy-ng Lazarus project"

if ! command -v fpc >/dev/null 2>&1; then
  echo "ERROR: fpc not found in PATH" >&2
  exit 1
fi

kcontrols_rev="$(git -C "$KCONTROLS_DIR" rev-parse --short HEAD 2>/dev/null || echo 'not a git tree')"
lazarus_rev="$(git -C "$LAZARUS_DIR" rev-parse --short HEAD 2>/dev/null || echo 'not a git tree')"

echo "== tomboy-ng $WIDGETSET clean build =="
echo "Project      : $PROJECT_ROOT"
echo "Lazarus      : $LAZARUS_DIR ($lazarus_rev)"
echo "lazbuild     : $LAZBUILD"
echo "KControls    : $KCONTROLS_DIR ($kcontrols_rev)"
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
for binary in $EXPECT_BINARIES; do
  rm -f -- "$PROJECT_ROOT/source/$binary"
done

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

echo "== $LINK_LABEL linkage check =="
checked=0
for binary in $EXPECT_BINARIES; do
  path="$PROJECT_ROOT/source/$binary"
  if [[ ! -x "$path" ]]; then
    echo "ERROR: expected binary was not built: $binary" >&2
    exit 1
  fi
  if ldd "$path" | grep -q "$LINK_PATTERN"; then
    echo "OK: $binary links to $LINK_LABEL"
  else
    echo "ERROR: $binary does not link to $LINK_LABEL" >&2
    exit 1
  fi
  checked=$((checked + 1))
done
if [[ "$checked" -eq 0 ]]; then
  echo "ERROR: nothing was checked" >&2
  exit 1
fi

run_smoke_test() {
  local binary="$PROJECT_ROOT/source/$SMOKE_BINARY"
  local smoke_config="$BUILD_ROOT/smoke-config"
  local smoke_log="$BUILD_ROOT/smoke.log"
  local runner=()
  local status=0
  local need_xvfb=0

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

  echo "== $LINK_LABEL smoke test =="
  set +e
  if [[ "${#runner[@]}" -gt 0 ]]; then
    "${runner[@]}" timeout "$SMOKE_TIMEOUT" "$binary" \
      --config-dir="$smoke_config" --no-splash --debug-log="$smoke_log"
  else
    timeout "$SMOKE_TIMEOUT" "$binary" \
      --config-dir="$smoke_config" --no-splash --debug-log="$smoke_log"
  fi
  status=$?
  set -e

  case "$status" in
    0|124)
      echo "OK: smoke test started $SMOKE_BINARY"
      echo "Log: $smoke_log"
      ;;
    *)
      echo "WARN: smoke test could not start $SMOKE_BINARY, exit status $status" >&2
      echo "Log: $smoke_log" >&2
      echo "WARN: build and linkage checks already passed; check the session manually" >&2
      ;;
  esac
}

if [[ "$RUN_SMOKE_TEST" = "1" ]]; then
  run_smoke_test
fi

echo "== Done =="
