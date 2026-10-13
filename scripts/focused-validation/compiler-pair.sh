#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
#
# SPDX-License-Identifier: MIT

# Start separately built compiler suites and release them after both create a directory.
set -euo pipefail
left="${1:?left checkout}"
right="${2:?right checkout}"
evidence="${3:?evidence directory}"
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
mkdir -p "$evidence/tmp"
export TMPDIR="$evidence/tmp"
mkfifo "$evidence/left.gate" "$evidence/right.gate"
launch() {
  local checkout="$1" name="$2"
  cd "$checkout"
  export FOCUSED_READY="$evidence/$name.ready" FOCUSED_GATE="$evidence/$name.gate"
  local binary
  binary="$(cabal list-bin ecluse-core-unit --builddir=dist-focused --ghc-options=-rtsopts)"
  exec bash "$here/../process-capture.sh" "$evidence/$name/status" "$evidence/$name/run.log" \
    timeout 120 "$binary" --match Ecluse.Core.Osv.Compile --seed 501 --no-color +RTS -N1 -RTS
}
launch "$left" left &
left_pid=$!
launch "$right" right &
right_pid=$!
trap 'kill "$left_pid" "$right_pid" 2>/dev/null || true' EXIT
timeout 60 bash -c 'until [[ -s "$1/left.ready" && -s "$1/right.ready" ]]; do sleep 0.05; done' _ "$evidence"
left_dir="$(cat "$evidence/left.ready")"
right_dir="$(cat "$evidence/right.ready")"
test "$left_dir" != "$right_dir"
test "$(dirname "$left_dir")" = "$TMPDIR"
test "$(dirname "$right_dir")" = "$TMPDIR"
test -d "$left_dir"
test -d "$right_dir"
printf '%s\n%s\n' "$left_dir" "$right_dir" > "$evidence/overlap.txt"
printf x > "$evidence/left.gate"
printf x > "$evidence/right.gate"
left_status=0
right_status=0
wait "$left_pid" || left_status=$?
wait "$right_pid" || right_status=$?
trap - EXIT
printf '%s\n' "$left_status" > "$evidence/left-launch-status"
printf '%s\n' "$right_status" > "$evidence/right-launch-status"
test "$left_status" -eq 0
test "$right_status" -eq 0
grep -h '^FOCUSED_DIRECTORY ' "$evidence/left/run.log" "$evidence/right/run.log" > "$evidence/directories.txt"
while IFS= read -r line; do
  directory="${line#FOCUSED_DIRECTORY }"
  test ! -e "$directory"
done < "$evidence/directories.txt"
find "$TMPDIR" -mindepth 1 -print > "$evidence/residual.txt"
test ! -s "$evidence/residual.txt"
