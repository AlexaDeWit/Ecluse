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
left_pid='' right_pid=''
left_gate_fd='' right_gate_fd=''
left_status=not-started right_status=not-started
joined=false
pair_check='gate-setup'
finish_pair() {
  local result=$?
  trap - EXIT
  if [[ "$result" -ne 0 ]]; then
    printf 'Compiler pair failed: %s (status %s)\n' "$pair_check" "$result" | tee "$evidence/failure.txt" >&2
  fi
  if [[ "$joined" != true ]]; then
    [[ -z "$left_pid" ]] || kill "$left_pid" 2>/dev/null || true
    [[ -z "$right_pid" ]] || kill "$right_pid" 2>/dev/null || true
    if [[ -n "$left_pid" ]]; then left_status=0; wait "$left_pid" || left_status=$?; fi
    if [[ -n "$right_pid" ]]; then right_status=0; wait "$right_pid" || right_status=$?; fi
  fi
  printf '%s\n' "$left_status" > "$evidence/left-launch-status"
  printf '%s\n' "$right_status" > "$evidence/right-launch-status"
  [[ -z "$left_gate_fd" ]] || exec {left_gate_fd}>&-
  [[ -z "$right_gate_fd" ]] || exec {right_gate_fd}>&-
  for gate in "$evidence/left.gate" "$evidence/right.gate"; do
    if [[ -p "$gate" ]]; then
      rm -- "$gate"
      printf 'removed\t%s\n' "${gate##*/}" >> "$evidence/gates.tsv"
    fi
  done
  exit "$result"
}
trap finish_pair EXIT
mkfifo "$evidence/left.gate" "$evidence/right.gate"
printf 'created\tleft.gate\ncreated\tright.gate\n' > "$evidence/gates.tsv"
# A nonblocking FIFO reader sees EOF without a live writer.
exec {left_gate_fd}<>"$evidence/left.gate"
exec {right_gate_fd}<>"$evidence/right.gate"
launch() {
  local checkout="$1" name="$2"
  trap - EXIT
  exec {left_gate_fd}>&- {right_gate_fd}>&-
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
pair_check='ready-directories'
timeout 60 bash -c 'until [[ -s "$1/left.ready" && -s "$1/right.ready" ]]; do sleep 0.05; done' _ "$evidence"
left_dir="$(cat "$evidence/left.ready")"
right_dir="$(cat "$evidence/right.ready")"
pair_check='distinct-directories'
test "$left_dir" != "$right_dir"
pair_check='left-parent'
test "$(dirname "$left_dir")" = "$TMPDIR"
pair_check='right-parent'
test "$(dirname "$right_dir")" = "$TMPDIR"
pair_check='left-directory-exists'
test -d "$left_dir"
pair_check='right-directory-exists'
test -d "$right_dir"
printf '%s\n%s\n' "$left_dir" "$right_dir" > "$evidence/overlap.txt"
pair_check='gate-release'
printf x >&"$left_gate_fd"
printf x >&"$right_gate_fd"
printf 'released\tleft.gate\treleased-byte=x\nreleased\tright.gate\treleased-byte=x\n' >> "$evidence/gates.tsv"
left_status=0
right_status=0
wait "$left_pid" || left_status=$?
wait "$right_pid" || right_status=$?
joined=true
pair_check='left-process-capture'
test "$left_status" -eq 0
pair_check='right-process-capture'
test "$right_status" -eq 0
pair_check='observed-directories'
grep -h '^FOCUSED_DIRECTORY ' "$evidence/left/run.log" "$evidence/right/run.log" > "$evidence/directories.txt"
while IFS= read -r line; do
  directory="${line#FOCUSED_DIRECTORY }"
  pair_check="directory-cleanup:$directory"
  test ! -e "$directory"
done < "$evidence/directories.txt"
pair_check='temporary-parent-cleanup'
find "$TMPDIR" -mindepth 1 -print > "$evidence/residual.txt"
test ! -s "$evidence/residual.txt"
