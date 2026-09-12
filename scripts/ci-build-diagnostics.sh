#!/usr/bin/env bash
# Run argv on Linux, preserving stdout, stderr and the command's exit status.
# Output silence triggers bounded diagnostics. Cancellation stops the command group.
set -euo pipefail

quiet_seconds="${CI_BUILD_QUIET_SECONDS:-120}"
poll_seconds="${CI_BUILD_POLL_SECONDS:-1}"
max_snapshots="${CI_BUILD_MAX_SNAPSHOTS:-20}"
for value in "$quiet_seconds" "$poll_seconds" "$max_snapshots"; do
  [[ "$value" =~ ^[1-9][0-9]{0,5}$ ]] || {
    echo 'build-diagnostics: intervals and snapshot limit must be integers from 1 to 999999' >&2
    exit 2
  }
done
[[ $# -gt 0 ]] || { echo 'usage: ci-build-diagnostics.sh command [argument ...]' >&2; exit 2; }

diagnostic_command() {
  if ! timeout --kill-after=1 5 "$@" 2>/dev/null | head -n 201; then
    echo 'build-diagnostics: diagnostic unavailable'
  fi
}

snapshot() {
  printf 'build-diagnostics: snapshot %s, build PID %s, output quiet for %s seconds\n' "$1" "$child_pid" "$2"
  diagnostic_command ps -eo pid,ppid,comm,stat,pcpu,pmem,rss,etime,wchan:32 --sort=-rss
  diagnostic_command free -m
  diagnostic_command df -h --output=size,used,avail,pcent,target / "$diagnostics_tmp" /nix
  diagnostic_command df --output=itotal,iused,iavail,ipcent,target / "$diagnostics_tmp" /nix
}

# tee reads only the command's bytes. Its Linux IO counters need no growing log file.
relay_bytes() {
  local pid="$1" key value rest
  [[ -r "/proc/$pid/io" ]] || return 1
  while read -r key value rest; do
    if [[ "$key" = rchar: ]]; then
      printf '%s\n' "$value"
      return
    fi
  done 2>/dev/null < "/proc/$pid/io"
  return 1
}

watch_output() {
  trap - EXIT INT HUP TERM
  local current index last_output=$SECONDS last_snapshot=$SECONDS count=0 warned=0
  local -a previous=(0 0) pids=("$stdout_pid" "$stderr_pid")
  while true; do
    sleep "$poll_seconds"
    for index in 0 1; do
      [[ -n "${pids[index]}" ]] || continue
      if current="$(relay_bytes "${pids[index]}")"; then
        if [[ "$current" != "${previous[index]}" ]]; then
          last_output=$SECONDS
          previous[index]=$current
        fi
      elif ! kill -0 "${pids[index]}" 2>/dev/null; then
        pids[index]=''
      elif ((warned == 0)); then
        echo 'build-diagnostics: output counters unavailable' >&2
        warned=1
      fi
    done
    if ((count < max_snapshots && SECONDS - last_output >= quiet_seconds && SECONDS - last_snapshot >= quiet_seconds)); then
      count=$((count + 1))
      snapshot "$count" "$((SECONDS - last_output))" >&2 || true
      last_snapshot=$SECONDS
    fi
  done
}

stop_helper() {
  local pid="$1"
  [[ -n "$pid" ]] || return 0
  kill -TERM -- "-$pid" 2>/dev/null || true
  kill -KILL -- "-$pid" 2>/dev/null || true
  wait "$pid" 2>/dev/null || true
}

cleanup() {
  stop_helper "$watcher_pid"
  stop_helper "$stdout_pid"
  stop_helper "$stderr_pid"
  rm -rf -- "$diagnostics_tmp"
}

cancel() {
  local signal="$1" status="$2" attempt
  trap '' INT HUP TERM
  if [[ -n "$child_pid" ]]; then
    kill -"$signal" -- "-$child_pid" 2>/dev/null || true
    for ((attempt = 0; attempt < 20; attempt++)); do
      kill -0 -- "-$child_pid" 2>/dev/null || break
      sleep 0.1
    done
    kill -KILL -- "-$child_pid" 2>/dev/null || true
    wait "$child_pid" 2>/dev/null || true
  fi
  exit "$status"
}

if ! diagnostics_tmp="$(mktemp -d "${TMPDIR:-/tmp}/ci-build-diagnostics.XXXXXXXX")"; then
  echo 'build-diagnostics: temporary directory unavailable' >&2
  exec "$@"
fi
child_pid='' stdout_pid='' stderr_pid='' watcher_pid='' pending_signal=''
trap cleanup EXIT
trap 'pending_signal=INT' INT
trap 'pending_signal=HUP' HUP
trap 'pending_signal=TERM' TERM
if ! command -v tee >/dev/null || ! mkfifo "$diagnostics_tmp/stdout" "$diagnostics_tmp/stderr"; then
  cleanup
  trap - EXIT INT HUP TERM
  echo 'build-diagnostics: output relays unavailable' >&2
  exec "$@"
fi

# Job control gives each helper and the command a group. Disable notices before waiting.
set -m
tee < "$diagnostics_tmp/stdout" &
stdout_pid=$!
tee < "$diagnostics_tmp/stderr" >&2 &
stderr_pid=$!
"$@" <&0 > "$diagnostics_tmp/stdout" 2> "$diagnostics_tmp/stderr" &
child_pid=$!
watch_output &
watcher_pid=$!
set +m
trap 'cancel INT 130' INT
trap 'cancel HUP 129' HUP
trap 'cancel TERM 143' TERM
case "$pending_signal" in
  INT) cancel INT 130 ;;
  HUP) cancel HUP 129 ;;
  TERM) cancel TERM 143 ;;
esac

status=0
wait "$child_pid" || status=$?
stop_helper "$watcher_pid"
watcher_pid=''
wait "$stdout_pid" || true
stdout_pid=''
wait "$stderr_pid" || true
stderr_pid=''
exit "$status"
