#!/usr/bin/env bash
# Apply one reviewed patch only to its exact source blobs on a disposable runner.
set -euo pipefail
patch="${1:?patch path}"
evidence="${2:?evidence directory}"
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
bash "$here/identity.sh" "$evidence"
sha256sum --check "$patch.sha256"
git apply --check --unidiff-zero "$patch"
cp "$patch" "$evidence/change.patch"
git apply --unidiff-zero "$patch"
git diff --binary > "$evidence/applied.patch"
