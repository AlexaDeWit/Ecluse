#!/usr/bin/env bash
# Exercise ownership, failures and daemon exclusion without starting containers.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir "$work/bin"
export TEST_TRACE="$work/trace"
cat > "$work/bin/docker" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$*" >> "$TEST_TRACE"
case "$*" in
  'ps -aq '*) echo container ;;
  'network ls -q '*) echo network ;;
  'rm -f '*) exit "${FAIL_REMOVE:-0}" ;;
esac
STUB
chmod +x "$work/bin/docker"
export PATH="$work/bin:$PATH"
export ECLTEST_RUN_ID=run-a ECLTEST_SHARD=pypi
script="$here/test-containers.sh"

bash "$script" run bash -c 'test "$ECLTEST_CONTAINER_LOCK" = held'
grep -q 'label=com.ecluse.test.run=run-a --filter label=com.ecluse.test.shard=pypi' "$TEST_TRACE"
if grep -q 'images -q' "$TEST_TRACE"; then
  echo 'automatic cleanup touched shared images' >&2
  exit 1
fi
status=0
bash "$script" run bash -c 'exit 17' || status=$?
[ "$status" = 17 ]
status=0
FAIL_REMOVE=1 bash "$script" run true || status=$?
[ "$status" = 1 ]
status=0
FAIL_REMOVE=1 bash "$script" run bash -c 'exit 17' || status=$?
[ "$status" = 17 ]

wait_for() {
  local n
  for ((n = 0; n < 200; n++)); do
    [ -f "$1" ] && return 0
    sleep 0.01
  done
  echo "timed out waiting for $1" >&2
  return 1
}
export TEST_WORK="$work"
bash "$script" run bash -c 'touch "$TEST_WORK/entered"; while [ ! -f "$TEST_WORK/release" ]; do sleep 0.01; done' &
first=$!
wait_for "$work/entered"
bash "$script" run bash -c 'touch "$TEST_WORK/second"' &
second=$!
bash "$script" reap --all &
cleaner=$!
sleep 0.1
[ ! -f "$work/second" ]
if grep -q 'images -q' "$TEST_TRACE"; then
  echo 'explicit cleanup entered an active lifetime' >&2
  exit 1
fi
touch "$work/release"
wait "$first"
wait "$second"
wait "$cleaner"
[ -f "$work/second" ]
grep -q 'images -q' "$TEST_TRACE"
rm "$work/entered"
bash "$script" run bash -c 'touch "$TEST_WORK/entered"; exec sleep 30' &
cancelled=$!
wait_for "$work/entered"
kill -TERM "$cancelled"
status=0
wait "$cancelled" || status=$?
[ "$status" = 143 ]
timeout 8 bash "$script" run true
cat > "$work/descendant.sh" <<'CHILD'
#!/usr/bin/env bash
trap '' TERM
printf '%s\n' "$BASHPID" > "$TEST_WORK/descendant-pid"
while true; do
  echo heartbeat >> "$TEST_WORK/heartbeat"
  sleep 0.02
done
CHILD
bash "$script" run bash -c 'bash "$TEST_WORK/descendant.sh" & wait' &
parent=$!
wait_for "$work/descendant-pid"
wait_for "$work/heartbeat"
kill -TERM "$parent"
status=0
wait "$parent" || status=$?
[ "$status" = 143 ]
before="$(wc -l < "$work/heartbeat")"
sleep 0.1
[ "$(wc -l < "$work/heartbeat")" = "$before" ]
timeout 8 bash "$script" run true
printf 'ok - invocation ownership, failures, descendant cancellation and shared daemon exclusion\n'
