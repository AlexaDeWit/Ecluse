#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
# SPDX-License-Identifier: MIT
set -euo pipefail
project=$1
revision=$2
digest=$3
actual=$(awk '
  function finish() {
    if (location == "https://github.com/kazu-yamamoto/http2.git") {
      print block
    }
    block = ""; location = ""
  }
  /^source-repository-package[[:space:]]*$/ { finish(); active = 1; next }
  active && /^[^[:space:]-]/ { finish(); active = 0 }
  active && /^[[:space:]]+[^[:space:]-]/ {
    line = $0
    sub(/^[[:space:]]+/, "", line)
    sub(/:[[:space:]]+/, ": ", line)
    block = block line "\n"
    if ($1 == "location:") location = $2
  }
  END { finish() }
' "$project")
expected=$(cat <<EOF
type: git
location: https://github.com/kazu-yamamoto/http2.git
tag: $revision
post-checkout-command: ecluse-apply-http2-patch $digest
EOF
)
[[ "$actual" == "$expected" ]] || { echo "http2 source or patch differs between Cabal and Nix." >&2; exit 1; }
