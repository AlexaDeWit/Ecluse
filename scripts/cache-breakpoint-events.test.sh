#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
# SPDX-License-Identifier: MIT
set -euo pipefail

repo=$(git rev-parse --show-toplevel)
temporary=$(mktemp -d "$repo/scratchpad/event-test.XXXXXX")
trap 'rm -rf "$temporary"' EXIT
cat > "$temporary/events.jsonl" <<'EOF'
{"kind":"artifact-full-hit","key":"a","bytes":20,"expiryNs":200,"observedNs":140}
{"kind":"insert","key":"a","bytes":20,"insertNs":100,"expiryNs":200,"observedNs":150}
{"kind":"insert","key":"b","bytes":30,"insertNs":120,"expiryNs":220,"observedNs":130}
{"kind":"remove","key":"a","bytes":20,"expiryNs":200,"observedNs":135,"cause":"capacity","bytePressure":true,"countPressure":false}
EOF
bash "$repo/scripts/cache-breakpoint-events.sh" "$temporary/events.jsonl" "$temporary/report.json"
jq -e '.admittedBytes == 50 and .reuse[0].delayNs == 40 and .reuse[0].interveningAdmittedBytes == 30 and .bytePressureRemovals == 1 and .countPressureRemovals == 0' "$temporary/report.json" > /dev/null
printf '%s\n' '{"kind":"listing-hit","key":"absent","bytes":20,"expiryNs":100,"observedNs":10}' > "$temporary/missing.jsonl"
if bash "$repo/scripts/cache-breakpoint-events.sh" "$temporary/missing.jsonl" "$temporary/missing-report.json"; then
    printf 'Unmatched reuse was accepted.\n' >&2
    exit 1
fi
