#!/usr/bin/env bash
# The runner plan owns scheduling. Reject an empty or repeated partition.
set -euo pipefail
partitions="$(jq -ce '.partitions | select(type == "array" and length > 0)
  | select(all(.[]; type == "string" and test("^[a-z0-9][a-z0-9-]*$")))
  | select(length == (unique | length))' "${1:?expected plan path}")"
printf 'partitions=%s\n' "$partitions" >> "${GITHUB_OUTPUT:-/dev/stdout}"
