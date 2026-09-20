#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
# SPDX-License-Identifier: MIT
set -euo pipefail

repo=$(git rev-parse --show-toplevel)
temporary=$(mktemp -d "$repo/scratchpad/trace-test.XXXXXX")
trap 'rm -rf "$temporary"' EXIT
printf '%s\n' '{"key":"a","package":"a","startMicros":1,"endMicros":2,"status":200,"accountedBytes":null}' > "$temporary/http.jsonl"
if bash "$repo/scripts/cache-breakpoint-trace.sh" "$temporary/http.jsonl" "$temporary"; then
    printf 'Missing successful weight was accepted.\n' >&2
    exit 1
fi
test ! -f "$temporary/model-input.json"
printf '%s\n' '{"key":"a","package":"a","startMicros":1,"endMicros":2,"status":200,"accountedBytes":10}' '{"key":"missing","package":"missing","startMicros":3,"endMicros":4,"status":404,"accountedBytes":null}' > "$temporary/http.jsonl"
bash "$repo/scripts/cache-breakpoint-trace.sh" "$temporary/http.jsonl" "$temporary"
jq -e '.failedResponses == 1 and .missingSuccessfulWeights == 0' "$temporary/model-completeness.json" > /dev/null
jq -e 'length == 1 and .[0].trWeight == 10' "$temporary/model-input.json" > /dev/null
