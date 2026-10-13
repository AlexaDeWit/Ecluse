#!/usr/bin/env bash
# Restore only the recorded temporary patch and verify the original tracked source.
set -euo pipefail
evidence="${1:?evidence directory}"
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
if ! git diff --quiet; then
  bash "$here/working-tree.sh" "$evidence/before-restore" "$evidence/applied.patch"
  git apply --reverse "$evidence/applied.patch"
fi
bash "$here/identity.sh" "$evidence/restored-source"
