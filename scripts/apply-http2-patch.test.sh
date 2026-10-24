#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
# SPDX-License-Identifier: MIT
set -euo pipefail
repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
fixture=$(mktemp -d)
trap 'rm -rf "$fixture"' EXIT
mkdir "$fixture/source"
cd "$fixture/source"
git init -q
printf 'before\n' > field.txt
git add field.txt
git -c user.name=Fixture -c user.email=fixture@example.invalid -c commit.gpgsign=false commit -qm fixture
revision=$(git rev-parse HEAD)
printf 'after\n' > field.txt
git diff --binary --unified=0 > "$fixture/change.patch"
digest=$(sha256sum "$fixture/change.patch")
digest=${digest%% *}
git restore field.txt
apply_patch() {
  bash "$repo_root/scripts/apply-http2-patch.sh" "$revision" "$fixture/change.patch" "$digest"
}
expect_failure() {
  if "$@" > /dev/null 2>&1; then
    echo "Expected rejection: $*" >&2
    exit 1
  fi
}
apply_patch
[[ $(cat field.txt) == after ]]
apply_patch
printf 'other\n' >> field.txt
expect_failure apply_patch
git restore field.txt
printf 'untracked\n' > extra.txt
expect_failure apply_patch
rm extra.txt
printf 'staged\n' > field.txt
git add field.txt
expect_failure apply_patch
git reset -q --hard HEAD
expect_failure bash "$repo_root/scripts/apply-http2-patch.sh" "$revision" "$fixture/missing.patch" "$digest"
expect_failure bash "$repo_root/scripts/apply-http2-patch.sh" "$revision" "$fixture/change.patch" wrong
expect_failure bash "$repo_root/scripts/apply-http2-patch.sh" wrong "$fixture/change.patch" "$digest"
lockstep="$repo_root/scripts/check-http2-lockstep.sh"
project="$fixture/cabal.project"
cat > "$project" <<EOF
source-repository-package
  type: git
  location: https://github.com/other/project.git
  tag: other
source-repository-package
  type: git
  location: https://github.com/kazu-yamamoto/http2.git
  tag: $revision
  post-checkout-command: ecluse-apply-http2-patch $digest
EOF
bash "$lockstep" "$project" "$revision" "$digest"
expect_failure bash "$lockstep" "$project" "$revision" wrong
expect_failure bash "$lockstep" "$project" wrong "$digest"
printf '    unexpected-argument\n' >> "$project"
expect_failure bash "$lockstep" "$project" "$revision" "$digest"
printf 'http2 patch and lockstep tests passed\n'
