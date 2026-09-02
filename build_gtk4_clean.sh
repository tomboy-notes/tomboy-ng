#!/usr/bin/env bash
#
# Backwards compatible wrapper. The build logic now lives in
# build_widgetset_clean.sh, which handles gtk4 and qt5.
#
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WIDGETSET=gtk4 exec "$HERE/build_widgetset_clean.sh" "$@"
