#!/usr/bin/env bash
# Exercise output monitoring and cancellation with bounded local child processes.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
test_tmp="$(mktemp -d)"
active_pid=''
cleanup() {
  if [[ -n "$active_pid" ]]; then
    kill -TERM "$active_pid" 2>/dev/null || true
    wait "$active_pid" 2>/dev/null || true
  fi
  rm -rf -- "$test_tmp"
}
trap cleanup EXIT
mkdir "$test_tmp/resources"
export TMPDIR="$test_tmp/resources"
export CI_BUILD_QUIET_SECONDS=1 CI_BUILD_POLL_SECONDS=1 CI_BUILD_MAX_SNAPSHOTS=2
bash_bin="$(command -v bash)"
secret='unused-private-argument-4ae7f81c'

fail() { echo "FAIL: $*" >&2; exit 1; }

wait_for() {
  local attempt
  for ((attempt = 0; attempt < 200; attempt++)); do
    if "$@"; then return; fi
    sleep 0.05
  done
  fail 'timed out waiting for the test child'
}

resources_removed() {
  [[ -z "$(find "$TMPDIR" -mindepth 1 -print -quit)" ]] || fail 'temporary resources remain'
}

run() {
  local expected="$1" status=0
  shift
  env PATH="${diagnostics_path:-$PATH}" timeout --kill-after=3 12 "$bash_bin" "$here/ci-build-diagnostics.sh" "$@" \
    > "$test_tmp/output" 2> "$test_tmp/error" || status=$?
  [[ "$status" = "$expected" ]] || fail "expected exit $expected, got $status"
  ! grep -q "$secret" "$test_tmp/error" || fail 'diagnostics exposed a command argument'
  resources_removed
}

run 23 "$bash_bin" -c '
  until [[ $(grep -c "^build-diagnostics: snapshot " "$1") -ge 2 ]]; do sleep 0.05; done
  sleep 1.2
  exit 23
' bash "$test_tmp/error" "$secret"
[[ "$(grep -c '^build-diagnostics: snapshot ' "$test_tmp/error")" = 2 ]] || fail 'silent build must reach the snapshot cap'
grep -q 'snapshot 2' "$test_tmp/error" || fail 'diagnostics reset the output clock'
[[ ! -s "$test_tmp/output" ]] || fail 'diagnostics reached stdout'

run 0 "$bash_bin" -c 'for ((n=0; n<60; n++)); do printf "\0"; printf "x" >&2; sleep 0.05; done' bash "$secret"
cmp -s <(head -c 60 /dev/zero) "$test_tmp/output" || fail 'binary stdout changed'
cmp -s <(printf 'x%.0s' {1..60}) "$test_tmp/error" || fail 'chatty stderr changed or triggered diagnostics'

run 0 "$bash_bin" -c 'exec >&-; for ((n=0; n<60; n++)); do printf x >&2; sleep 0.05; done' bash "$secret"
[[ ! -s "$test_tmp/output" ]] || fail 'closed stdout received output'
cmp -s <(printf 'x%.0s' {1..60}) "$test_tmp/error" || fail 'closing stdout hid active stderr'

CI_BUILD_QUIET_SECONDS=10 run 0 "$bash_bin" -c 'printf "%s\0tail" "$1"; printf "error\rpartial" >&2' bash 'one argument with spaces'
cmp -s <(printf 'one argument with spaces\0tail') "$test_tmp/output" || fail 'argv or partial stdout changed'
cmp -s <(printf 'error\rpartial') "$test_tmp/error" || fail 'partial stderr changed'
run 42 "$bash_bin" -c 'exit 42' bash "$secret"
run 143 "$bash_bin" -c 'kill -TERM $$' bash "$secret"
run 127 "$test_tmp/no-such-command"

# The child cannot exit until the parent sees its partial output.
CI_BUILD_QUIET_SECONDS=10 "$bash_bin" "$here/ci-build-diagnostics.sh" "$bash_bin" -c '
  printf ready
  while [[ ! -f "$1" ]]; do sleep 0.05; done
' bash "$test_tmp/ack" > "$test_tmp/output" 2> "$test_tmp/error" &
active_pid=$!
wait_for grep -q ready "$test_tmp/output"
touch "$test_tmp/ack"
wait "$active_pid"
active_pid=''
resources_removed

mkdir "$test_tmp/bin"
for tool in mktemp mkfifo tee sleep rm timeout head; do
  ln -s "$(command -v "$tool")" "$test_tmp/bin/$tool"
done
diagnostics_path="$test_tmp/bin" run 0 "$bash_bin" -c 'sleep 4' bash "$secret"
grep -q 'diagnostic unavailable' "$test_tmp/error" || fail 'missing diagnostic tools were not reported'

for signal in TERM HUP; do
  "$bash_bin" "$here/ci-build-diagnostics.sh" "$bash_bin" -c '
    trap "" TERM HUP
    printf "%s\n" "$$" > "$1/child"
    sleep 30 &
    printf "%s\n" "$!" > "$1/descendant"
    wait
  ' bash "$test_tmp" "$secret" > "$test_tmp/output" 2> "$test_tmp/error" &
  active_pid=$!
  wait_for test -s "$test_tmp/descendant"
  child="$(<"$test_tmp/child")"
  descendant="$(<"$test_tmp/descendant")"
  helper_pids="$(ps --ppid "$active_pid" -o pid=)"
  kill -"$signal" "$active_pid"
  status=0
  wait "$active_pid" || status=$?
  active_pid=''
  expected=143
  [[ "$signal" != HUP ]] || expected=129
  [[ "$status" = "$expected" ]] || fail "cancellation returned $status instead of $expected"
  for pid in "$child" "$descendant" $helper_pids; do
    state="$(ps -p "$pid" -o stat=)" || continue
    [[ "$state" = Z* ]] || fail "process $pid survived cancellation"
  done
  for group in $helper_pids; do
    if ps -eo pgid=,stat= | awk -v group="$group" '$1 == group && $2 !~ /^Z/ {found=1} END {exit !found}'; then
      fail "process group $group survived cancellation"
    fi
  done
  ! grep -q "$secret" "$test_tmp/error" || fail 'cancellation exposed a command argument'
  resources_removed
  rm "$test_tmp/child" "$test_tmp/descendant"
done

status=0
timeout --preserve-status --signal=INT --kill-after=5 1 \
  "$bash_bin" "$here/ci-build-diagnostics.sh" "$bash_bin" -c 'sleep 30' bash "$secret" \
  > "$test_tmp/output" 2> "$test_tmp/error" || status=$?
[[ "$status" = 130 ]] || fail "INT returned $status instead of 130"
! grep -q "$secret" "$test_tmp/error" || fail 'INT exposed a command argument'
resources_removed
echo 'ok: build output, diagnostics, status and cancellation'
