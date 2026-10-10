#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
#
# SPDX-License-Identifier: MIT
set -euo pipefail

# shellcheck source-path=SCRIPTDIR
# shellcheck source=common.sh
source "$(dirname -- "${BASH_SOURCE[0]}")/common.sh"
unset GHCRTS
readonly verify_chunks=(1 1024 32768)
check_inputs
mkdir -p -- "$artifact_dir/verification"
: > "$artifact_dir/verification.jsonl"
while IFS= read -r fixture; do
  for chunk in "${verify_chunks[@]}"; do
    for variant in "${variants[@]}"; do
      binary=$(binary_path "$variant" token-fold)
      result="$artifact_dir/verification/$variant.$fixture.$chunk.txt"
      "$binary" verify "$chunk" "$work_dir/original/benchmarks/json-data/$fixture" > "$result"
      [[ $(cat -- "$result") != Nothing ]] || fail "Verification failed: $variant/$fixture/$chunk"
      cmp -- "$artifact_dir/verification/original.$fixture.$chunk.txt" "$result"
      jq -nc --arg variant "$variant" --arg fixture "$fixture" --argjson chunk "$chunk" --rawfile result "$result" \
        '{variant:$variant,fixture:$fixture,chunk:$chunk,result:$result}' >> "$artifact_dir/verification.jsonl"
    done
  done
done < <(jq -r '.[].path' "$artifact_dir/fixtures.json")
check_inputs
printf '%s\n' 'All fixture token digests match at each verification chunk size.'
