#!/usr/bin/env bash
# Preserve process failures and stop descendants before publishing a cancellation status.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
mkdir -p scratchpad
work="$(mktemp -d scratchpad/process-capture.XXXXXX)"
trap 'rm -rf "$work"' EXIT
export TEST_CAPTURE_WORK="$work"
bash "$here/process-capture.sh" "$work/status" "$work/output" bash -c 'echo current; exit 7'
[ "$(cat "$work/status")" = 7 ]
[ "$(cat "$work/output")" = current ]
bash "$here/process-capture.sh" "$work/split.status" "$work/native.json" --stderr "$work/native.stderr" \
  bash -c 'printf "{\"status\":\"ok\"}\n"; printf "retained diagnostic\n" >&2'
[ "$(cat "$work/split.status")" = 0 ]
[ "$(cat "$work/native.json")" = '{"status":"ok"}' ]
[ "$(cat "$work/native.stderr")" = 'retained diagnostic' ]
cat > "$work/child" <<'CHILD'
#!/usr/bin/env bash
trap '' TERM
while true; do
  echo heartbeat >> "$TEST_CAPTURE_WORK/heartbeat"
  sleep 0.02
done
CHILD
bash "$here/process-capture.sh" "$work/status" "$work/output" bash -c 'bash "$TEST_CAPTURE_WORK/child" & wait' &
parent=$!
for ((attempt = 0; attempt < 200; attempt++)); do
  [ ! -f "$work/heartbeat" ] || break
  sleep 0.01
done
test -s "$work/heartbeat"
kill -TERM "$parent"
wait "$parent"
[ "$(cat "$work/status")" = 143 ]
before="$(wc -l < "$work/heartbeat")"
sleep 0.1
[ "$(wc -l < "$work/heartbeat")" = "$before" ]
printf 'ok - native status and descendant cancellation survive capture\n'
