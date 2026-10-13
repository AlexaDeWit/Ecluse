#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
#
# SPDX-License-Identifier: MIT

# Record the selected source and require an unchanged tracked tree.
set -euo pipefail
evidence="${1:?evidence directory}"
mkdir -p "$evidence"
source_sha="${FOCUSED_SOURCE:-}"
printf 'expected-source=%s\nactions=%s\ndirectory=%s\n' \
  "$source_sha" "${GITHUB_ACTIONS:-}" "$PWD" > "$evidence/context.txt"
identity_check='source-format'
identity_status=0
trap 'identity_status=$?
  printf "Identity check failed: %s (status %s)\n" "$identity_check" "$identity_status" | tee "$evidence/failure.txt" >&2
  cat "$evidence/context.txt" >&2
  if [[ -f "$evidence/source.txt" ]]; then cat "$evidence/source.txt" >&2; fi
  exit "$identity_status"' ERR
[[ "$source_sha" =~ ^[0-9a-f]{40}$ ]]
identity_check='github-actions'
[[ "${GITHUB_ACTIONS:-}" == true ]]
identity_check='read-checkout'
git rev-parse HEAD 'HEAD^{tree}' > "$evidence/source.txt"
identity_check='checkout-head'
[[ "$(head -n 1 "$evidence/source.txt")" == "$source_sha" ]]
identity_check='unstaged-source'
git diff --exit-code > "$evidence/unstaged.patch"
identity_check='staged-source'
git diff --cached --exit-code > "$evidence/staged.patch"
identity_check='architecture'
uname -m > "$evidence/architecture.txt"
identity_check='tracked-status'
git status --porcelain --untracked-files=no > "$evidence/tracked-status.txt"
test ! -s "$evidence/tracked-status.txt"
identity_check='support-contract'
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cp "$here/../../taskfiles/focused-validation/support-commits.txt" "$evidence/support-commits.txt"
while IFS= read -r support; do
  identity_check="support-ancestor:$support"
  printf 'Required ancestor: %s\n' "$support" >> "$evidence/support-verification.log"
  git merge-base --is-ancestor "$support" "$source_sha"
done < "$evidence/support-commits.txt"
