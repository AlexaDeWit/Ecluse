#!/usr/bin/env bash
# Shared process-group cancellation for commands that own child processes.

stop_child() {
  process_signal_status="$1"
  local attempt
  trap '' INT TERM
  kill -TERM -- "-$child_pid" 2>/dev/null || true
  for ((attempt = 0; attempt < 50; attempt++)); do
    kill -0 -- "-$child_pid" 2>/dev/null || break
    sleep 0.1
  done
  kill -KILL -- "-$child_pid" 2>/dev/null || true
  wait "$child_pid" || true
}

run_process_group() {
  local status=0 process_signal_status=0 child_pid
  set -m
  "$@" &
  child_pid=$!
  set +m
  trap 'stop_child 130' INT
  trap 'stop_child 143' TERM
  wait "$child_pid" || status=$?
  trap - INT TERM
  [ "$process_signal_status" -eq 0 ] || status="$process_signal_status"
  return "$status"
}
