#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
#
# SPDX-License-Identifier: MIT

# Record the selected source and require an unchanged tracked tree.
set -euo pipefail
source_sha="${FOCUSED_SOURCE:?full source SHA}"
evidence="${1:?evidence directory}"
[[ "$source_sha" =~ ^[0-9a-f]{40}$ ]]
[[ "${GITHUB_ACTIONS:-}" == true ]]
[[ "$(git rev-parse HEAD)" == "$source_sha" ]]
git diff --exit-code
git diff --cached --exit-code
mkdir -p "$evidence"
git rev-parse HEAD 'HEAD^{tree}' > "$evidence/source.txt"
uname -m > "$evidence/architecture.txt"
git status --porcelain --untracked-files=no > "$evidence/tracked-status.txt"
test ! -s "$evidence/tracked-status.txt"
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cp "$here/../../taskfiles/focused-validation/support-commits.txt" "$evidence/support-commits.txt"
while IFS= read -r support; do
  printf 'Required ancestor: %s\n' "$support" >> "$evidence/support-verification.log"
  git merge-base --is-ancestor "$support" "$source_sha"
done < "$evidence/support-commits.txt"
