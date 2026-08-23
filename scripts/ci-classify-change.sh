#!/usr/bin/env bash
# Decide whether a pull request touches documentation only, for .github/workflows/ci.yml.
#
# Fails closed. Anything but a pull request, an unreadable file list, and any path
# outside the allow-list below all classify as not documentation-only, so every Haskell
# job runs. A directory this repo adds later is covered until someone adds it here.
set -euo pipefail

# Paths that cannot reach a Haskell build. The static-checks job runs on every PR
# whatever this says, so the format, lint, SPDX, and site gates still cover all of them.
# A rename reports both of its paths below, so moving code under one of these still counts
# as reaching code.
doc_path='^([A-Za-z0-9_]+\.md|DCO|LICENSE|CITATION\.cff)$|^(docs|web|threat-modelling|LICENSES|\.agents|\.claude)/'

out="${GITHUB_OUTPUT:-/dev/stdout}"

decide() {
  echo "classify: $2"
  echo "docs-only=$1" >> "$out"
  exit 0
}

if [ "${EVENT_NAME:-}" != "pull_request" ] || [ -z "${PR_NUMBER:-}" ]; then
  decide false "not a pull request, every job runs."
fi

files="$(gh api --paginate "repos/$REPO/pulls/$PR_NUMBER/files" \
  --jq '.[] | .filename, (.previous_filename // empty)')" || files=""
[ -n "$files" ] || decide false "could not read the changed-file list, every job runs."

outside="$(printf '%s\n' "$files" | grep -Ev "$doc_path" || true)"
if [ -n "$outside" ]; then
  printf '%s\n' "$outside" | sed 's/^/  outside the allow-list: /'
  decide false "the change reaches code, every job runs."
fi

printf '%s\n' "$files" | sed 's/^/  documentation: /'
decide true "documentation only, the Haskell jobs are skipped."
