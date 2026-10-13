#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
#
# SPDX-License-Identifier: MIT

# Require the production redirect mutation to fail only its named runtime assertion.
set -euo pipefail
evidence="${1:?result directory}"
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
bash "$here/integration-controls.sh" "$evidence" integration/egress/native-003
test "$(cat "$evidence/integration/native-status")" -eq 1
test "$(cat "$evidence/integration/audit-status")" -ne 0
test -s "$evidence/integration/plan.json"
jq -e --slurpfile plan "$evidence/integration/plan.json" '
  ([.cases[].caseId] | sort) == ([$plan[0].catalogue.catalogueGroups[].groupCases[].caseId] | sort) and
  ([.groups[].groupId] | sort) == ([$plan[0].catalogue.catalogueGroups[].groupId] | sort) and
  all(.groups[]; .status == "passed" or (.groupId == "integration/egress" and .status == "failed")) and
  .identity.source == env.FOCUSED_SOURCE and
  ([.cases[] | select(.status != "passed") | .caseId] == ["integration/egress/native-003"]) and
  ([.cases[] | select(.caseId == "integration/egress/native-003")][0] |
    .status == "failed" and (.diagnostics | join(" ") | contains("ExpectedButGot") and contains("Right 302") and contains("Right 503")))
' "$evidence/integration/report.json"
grep -F 'integration/egress/native-003' "$evidence/integration/run.log"
