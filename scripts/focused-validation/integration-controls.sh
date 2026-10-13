#!/usr/bin/env bash
# Require the reviewed integration cases independently of the candidate's own catalogue.
set -euo pipefail
evidence="${1:?result directory}"
failure="${2:-}"
[[ -z "$failure" || "$failure" == integration/egress/native-003 ]]
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
required="$here/../../taskfiles/focused-validation/required-integration-controls.txt"
cp "$required" "$evidence/required-integration-controls.txt"
jq -e --arg failure "$failure" --rawfile required "$required" '
  .cases as $cases |
  [($required | split("\n") | map(select(length > 0)))[] as $id |
    {caseId: $id,
     expected: (if $id == $failure then "failed" else "passed" end),
     observed: [$cases[] | select(.caseId == $id) | .status]}] |
  if all(.[]; .observed == [.expected]) then .
  else ., error("a mandatory integration control is missing, duplicated, or has the wrong status") end
' "$evidence/integration/report.json" > "$evidence/integration-control-counts.json"
