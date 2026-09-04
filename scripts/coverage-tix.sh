#!/usr/bin/env bash
# Print the path of the .tix a suite wrote most recently under a coverage builddir.
#
# The builddir keeps the package directory of every version it ever built, so an
# older ecluse-<version>/ tree can carry a stale .tix of the same name, and `find`
# orders nothing. The newest by modification time is the one the suite just wrote.
#
# Usage: scripts/coverage-tix.sh <suite> [builddir]   (builddir default: dist-coverage)
set -euo pipefail

suite="${1:?usage: scripts/coverage-tix.sh <suite> [builddir]}"
builddir="${2:-dist-coverage}"

tix="$(find "$builddir" -type f -name "${suite}.tix" -printf '%T@\t%p\n' | sort -n | tail -n1 | cut -f2-)"
if [ -z "$tix" ]; then
  echo "coverage-tix: no ${suite}.tix found under $builddir/ (did the suite run?)" >&2
  exit 1
fi
printf '%s\n' "$tix"
