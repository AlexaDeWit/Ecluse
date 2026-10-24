#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
# SPDX-License-Identifier: MIT
set -euo pipefail

if [[ $# != 3 ]]; then
  echo "Expected source revision, patch path and patch digest." >&2
  exit 1
fi
revision=$1
patch_path=$2
expected_digest=$3
[[ $(git rev-parse HEAD) == "$revision" ]] || { echo "http2 source revision differs." >&2; exit 1; }
[[ -f "$patch_path" ]] || { echo "http2 patch is missing." >&2; exit 1; }
actual_digest=$(sha256sum "$patch_path")
[[ ${actual_digest%% *} == "$expected_digest" ]] || { echo "http2 patch digest differs." >&2; exit 1; }
git diff --cached --quiet || { echo "http2 source has staged changes." >&2; exit 1; }
[[ -z $(git ls-files --others) ]] || { echo "http2 source has untracked files." >&2; exit 1; }

if git diff --quiet; then
  git apply --unidiff-zero --check "$patch_path"
  git apply --unidiff-zero "$patch_path"
fi
# Offline Cabal reruns the hook without resetting the checkout.
git -c core.quotePath=true diff --no-ext-diff --binary --unified=0 --no-renames HEAD | cmp -s "$patch_path" - || {
  echo "http2 source differs from the expected patched tree." >&2
  exit 1
}
