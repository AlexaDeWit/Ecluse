#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
#
# SPDX-License-Identifier: MIT

# Count completed mandatory examples in an already passing native Hspec log.
set -euo pipefail
evidence="${1:?result directory}"
required="${2:?required example names}"
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
bash "$here/assertion.sh" "$evidence" pass
test -s "$required"
cp "$required" "$evidence/required-controls.txt"
awk '
  NR == FNR { required[$0] = 0; next }
  { sub(/^[[:space:]]+/, ""); if ($0 in required) required[$0]++ }
  END {
    for (name in required) {
      print required[name] "\t" name
      if (required[name] != 1) failed = 1
    }
    exit failed
  }
' "$required" "$evidence/run.log" > "$evidence/executed-control-counts.tsv"
