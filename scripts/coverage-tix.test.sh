#!/usr/bin/env bash
# Unit tests for scripts/coverage-tix.sh: the newest .tix wins, whatever find's order.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

fail=0
check() {
  local label="$1" expected="$2" actual="$3"
  if [ "$expected" = "$actual" ]; then
    echo "ok   - $label"
  else
    echo "FAIL - $label: expected '$expected', got '$actual'"
    fail=1
  fi
}

old="$tmp/build/ghc/ecluse-0.1.0/t/ecluse-unit/hpc/vanilla/tix"
new="$tmp/build/ghc/ecluse-0.2.0/t/ecluse-unit/hpc/vanilla/tix"
mkdir -p "$old" "$new"
printf 'stale\n' > "$old/ecluse-unit.tix"
printf 'fresh\n' > "$new/ecluse-unit.tix"
touch -d '2026-08-29 20:40:05' "$old/ecluse-unit.tix"
touch -d '2026-09-04 01:00:00' "$new/ecluse-unit.tix"

check "picks the newest tix over a stale same-named one" \
  "$new/ecluse-unit.tix" "$(bash "$here/coverage-tix.sh" ecluse-unit "$tmp")"

# The stale tree is newer on disk when a restored cache was extracted after the
# build wrote, so the pick follows the file, not the directory.
touch -d '2026-09-04 02:00:00' "$old"
check "orders by the file's mtime, not its directory's" \
  "$new/ecluse-unit.tix" "$(bash "$here/coverage-tix.sh" ecluse-unit "$tmp")"

check "fails when the suite wrote nothing" \
  "1" "$(bash "$here/coverage-tix.sh" ecluse-integration "$tmp" >/dev/null 2>&1; echo $?)"

exit "$fail"
