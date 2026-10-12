#!/usr/bin/env bash
# Preserve superseded files before bootstrap, argument validation, or resource acquisition.
set -euo pipefail
root="${1:?artifact root}"
shard="${2:?shard}"
attempt="${3:?attempt directory}"
[[ "$shard" =~ ^[a-z0-9][a-z0-9-]*$ ]] || exit 2
mkdir -p "$attempt"
for previous in "$root/reports/$shard.json" "$root/reports/all.json"; do
  if [ -e "$previous" ]; then
    mv "$previous" "$attempt/previous-$(basename "$previous")"
  fi
done
printf '%s\n' "$attempt" > "$root/reports/$shard.attempt"
