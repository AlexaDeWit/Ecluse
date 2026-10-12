#!/usr/bin/env bash
# Persist native status and output so Task can publish evidence before returning failure.
set -euo pipefail
status_path="${1:?status path}"
log_path="${2:?log path}"
shift 2
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$here/process-group.sh"
mkdir -p "$(dirname "$status_path")" "$(dirname "$log_path")"
rm -f "$status_path"
status=0
run_process_group "$@" > "$log_path" 2>&1 || status=$?
if ! cat "$log_path"; then
  [ "$status" -ne 0 ] || status=1
fi
printf '%s\n' "$status" > "$status_path"
