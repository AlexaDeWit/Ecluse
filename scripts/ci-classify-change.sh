#!/usr/bin/env bash
# Classify a pull request's change for .github/workflows/ci.yml. Fails closed: anything but a pull
# request, an unreadable file list, or a path outside the lists below runs every job.
#   docs-only        "true" when every path is documentation. The Haskell jobs skip.
#   release-build    "false" when no path can change the release image. The release dry-run skips.
#   release-rebuild  "false" on every pull request. The second image build and its comparison skip.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Paths that cannot reach a Haskell build. The static-checks job runs on every PR
# whatever this says, so the format, lint, SPDX, and site gates still cover all of them.
doc_path='^([A-Za-z0-9_]+\.md|DCO|LICENSE|CITATION\.cff)$|^(docs|web|threat-modelling|LICENSES|\.agents|\.claude)/'

# Haskell source, runbooks, and analysis-tool configuration. e2e-prepare builds the arm64 image from
# this source, so the dry-run adds only the amd64 build.
source_path='^(src|core|runtime|app|gen|manifest|site|test|bench|acceptance|config|runbooks)/'
source_path="$source_path"'|^(\.hlint\.yaml|\.stan\.toml|fourmolu\.yaml|weeder\.toml|codecov\.yml)$'

# Script tests, and scripts that release.yml, release-build.yml, the dry-run jobs, and the
# Taskfiles' top-level vars never run. A new script runs the dry-run until it is listed here.
tool_scripts=(
  artifact-invalidate artifact-lock ci-gate-verdict ci-runner-policy coverage coverage-tix
  docker-image-config docker-prepull docs-open gen-bench-corpus gen-version-fixtures init-release
  load-cgroup load-sweep metadata-ticks new-worktree perf-baseline process-capture process-group
  process-status prune-caches reap-hls-caches rm-worktree sarif-locations source-inventory
  test-containers verify-release
)
tool_path="$(printf '%s|' "${tool_scripts[@]}")"
source_path="$source_path"'|^scripts/[a-z0-9-]+\.test\.sh$'
source_path="$source_path|^scripts/(${tool_path%|})\\.sh\$"

# Paths inside source_path that still run the dry-run: test/oracles/ feeds the `.#ci`
# shell the release build enters.
release_path='^test/oracles/'

out="${GITHUB_OUTPUT:-/dev/stdout}"

decide() {
  echo "classify: $4"
  printf 'docs-only=%s\nrelease-build=%s\nrelease-rebuild=%s\n' "$1" "$2" "$3" >> "$out"
  exit 0
}

# On the PR's merge commit, the first parent is the base the PR is tested against.
cabal_harmless() {
  if [ -z "${HEAD_SHA:-}" ] || [ "$(git rev-parse -q --verify 'HEAD^2')" != "$HEAD_SHA" ]; then
    echo "  the checkout is not the merge commit of PR head ${HEAD_SHA:-(unknown)}"
    return 1
  fi
  git diff --no-color --no-ext-diff --no-textconv --unified=1000000 'HEAD^1' HEAD -- ecluse.cabal |
    bash "$here/ci-cabal-diff.sh"
}

if [ "${EVENT_NAME:-}" != "pull_request" ] || [ -z "${PR_NUMBER:-}" ]; then
  decide false true true "not a pull request, every job runs."
fi

# Both paths of a rename, so moving code under a listed directory still counts.
files="$(gh api --paginate "repos/$REPO/pulls/$PR_NUMBER/files" \
  --jq '.[] | .filename, (.previous_filename // empty)')" || files=""
[ -n "$files" ] || decide false true true "could not read the changed-file list, every job runs."

outside() {
  printf '%s\n' "$files" | grep -Ev "$1" || true
}

reaches_release="$(
  outside "$doc_path|$source_path"
  printf '%s\n' "$files" | grep -E "$release_path" || true
)"
if [ "$reaches_release" = "ecluse.cabal" ] && cabal_harmless; then
  reaches_release=""
fi
if [ -n "$reaches_release" ]; then
  printf '%s\n' "$reaches_release" | sed 's/^/  reaches the release build: /'
  decide false true false "the change reaches the release build, the release dry-run runs."
fi

reaches_code="$(outside "$doc_path")"
if [ -n "$reaches_code" ]; then
  printf '%s\n' "$reaches_code" | sed 's/^/  outside the documentation list: /'
  decide false false false "the change reaches code but not the release image, the dry-run skips."
fi

printf '%s\n' "$files" | sed 's/^/  documentation: /'
decide true false false "documentation only, the Haskell jobs and the release dry-run are skipped."
