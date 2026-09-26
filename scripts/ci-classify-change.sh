#!/usr/bin/env bash
# Classify a pull request's changed paths for .github/workflows/ci.yml. Two outputs:
#
#   docs-only      "true" when every path is documentation. The Haskell jobs skip.
#   release-build  "false" when every path is documentation or Haskell source that the
#                  regular jobs build. The release dry-run skips.
#
# Fails closed. Anything but a pull request, an unreadable file list, and any path
# outside a list below give docs-only=false and release-build=true, so every job runs.
# A directory this repo adds later is covered until someone adds it here.
set -euo pipefail

# Paths that cannot reach a Haskell build. The static-checks job runs on every PR
# whatever this says, so the format, lint, SPDX, and site gates still cover all of them.
doc_path='^([A-Za-z0-9_]+\.md|DCO|LICENSE|CITATION\.cff)$|^(docs|web|threat-modelling|LICENSES|\.agents|\.claude)/'

# Haskell source directories, runbooks, and analysis-tool configuration. The build job
# compiles the source and the docs job builds it through Nix, so only the image build
# itself goes unexercised. ecluse.cabal is absent on purpose: it can change the Nix build.
source_path='^(src|core|runtime|app|gen|manifest|site|test|bench|acceptance|config|runbooks)/'
source_path="$source_path"'|^(\.hlint\.yaml|\.stan\.toml|fourmolu\.yaml|weeder\.toml|codecov\.yml)$'

out="${GITHUB_OUTPUT:-/dev/stdout}"

decide() {
  echo "classify: $3"
  printf 'docs-only=%s\nrelease-build=%s\n' "$1" "$2" >> "$out"
  exit 0
}

if [ "${EVENT_NAME:-}" != "pull_request" ] || [ -z "${PR_NUMBER:-}" ]; then
  decide false true "not a pull request, every job runs."
fi

# Both paths of a rename, so moving code under a listed directory still counts.
files="$(gh api --paginate "repos/$REPO/pulls/$PR_NUMBER/files" \
  --jq '.[] | .filename, (.previous_filename // empty)')" || files=""
[ -n "$files" ] || decide false true "could not read the changed-file list, every job runs."

# The paths outside the pattern $1, one per line.
outside() {
  printf '%s\n' "$files" | grep -Ev "$1" || true
}

reaches_release="$(outside "$doc_path|$source_path")"
if [ -n "$reaches_release" ]; then
  printf '%s\n' "$reaches_release" | sed 's/^/  reaches the release build: /'
  decide false true "the change reaches the release build, every job runs."
fi

reaches_code="$(outside "$doc_path")"
if [ -n "$reaches_code" ]; then
  printf '%s\n' "$reaches_code" | sed 's/^/  outside the documentation list: /'
  decide false false "the change reaches code but not the release build, the release dry-run is skipped."
fi

printf '%s\n' "$files" | sed 's/^/  documentation: /'
decide true false "documentation only, the Haskell jobs and the release dry-run are skipped."
