#!/usr/bin/env bash
# Bind each runtime pass to the exact clean source or one recorded patch.
set -euo pipefail
evidence="${1:?evidence directory}"
patch="${2:-}"
[[ "${GITHUB_ACTIONS:-}" == true ]]
[[ "${FOCUSED_SOURCE:?}" =~ ^[0-9a-f]{40}$ ]]
test "$(git rev-parse HEAD)" = "$FOCUSED_SOURCE"
git diff --cached --exit-code
mkdir -p "$evidence"
git rev-parse HEAD 'HEAD^{tree}' > "$evidence/source.txt"
git diff --binary > "$evidence/working.patch"
if [[ -n "$patch" ]]; then
  cmp "$patch" "$evidence/working.patch"
else
  test ! -s "$evidence/working.patch"
fi
sha256sum "$evidence/working.patch" > "$evidence/working.patch.sha256"
uname -m > "$evidence/architecture.txt"
