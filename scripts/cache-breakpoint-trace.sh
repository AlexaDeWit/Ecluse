#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
# SPDX-License-Identifier: MIT
set -euo pipefail

input=${1:?HTTP trace}
output=${2:?output directory}
jq -s '{requests:length,failedResponses:([.[]|select(.status == null or .status < 200 or .status >= 300)]|length),missingSuccessfulWeights:([.[]|select(.status != null and .status >= 200 and .status < 300 and (.accountedBytes == null or .accountedBytes <= 0))]|length)}' "$input" > "$output/model-completeness.json"
if ! jq -e '.missingSuccessfulWeights == 0 and .requests > 0' "$output/model-completeness.json" > /dev/null; then
    printf 'The trace lacks successful request weights. No model was emitted.\n' >&2
    exit 1
fi
jq -s '[.[] | select(.accountedBytes != null and .accountedBytes > 0 and (.key | startswith("private-miss/") | not)) | {trKey:.package,trStart:.startMicros,trEnd:.endMicros,trWeight:.accountedBytes,trAccess:(if (.key | contains("/-/")) then "Artifact" else "Listing" end),trSuccess:(.status != null and .status >= 200 and .status < 300)}]' "$input" > "$output/model-input.json"
