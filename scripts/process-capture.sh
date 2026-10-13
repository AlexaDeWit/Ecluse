#!/usr/bin/env bash
# Persist native status and output so Task can publish evidence before returning failure.
# An optional --stderr PATH after the output path preserves the streams separately.
set -euo pipefail
status_path="${1:?status path}"
log_path="${2:?log path}"
shift 2
stderr_path=''
if [ "${1:-}" = --stderr ]; then
  stderr_path="${2:?stderr path}"
  shift 2
fi
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$here/process-group.sh"
mkdir -p "$(dirname "$status_path")" "$(dirname "$log_path")"
rm -f "$status_path"
status=0
if [ -n "$stderr_path" ]; then
  mkdir -p "$(dirname "$stderr_path")"
  run_process_group "$@" > "$log_path" 2> "$stderr_path" || status=$?
else
  run_process_group "$@" > "$log_path" 2>&1 || status=$?
fi
if ! cat "$log_path"; then
  [ "$status" -ne 0 ] || status=1
fi
if [ -n "$stderr_path" ] && ! cat "$stderr_path" >&2; then
  [ "$status" -ne 0 ] || status=1
fi
printf '%s\n' "$status" > "$status_path"
