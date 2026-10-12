#!/usr/bin/env bash
# Return the first recorded failure after Task finishes the required publication steps.
set -euo pipefail
status=0
for path in "$@"; do
  value="$(cat "$path")"
  [[ "$value" =~ ^[0-9]+$ ]] && [ "$value" -le 255 ] || exit 2
  if [ "$status" -eq 0 ] && [ "$value" -ne 0 ]; then
    status="$value"
  fi
done
exit "$status"
