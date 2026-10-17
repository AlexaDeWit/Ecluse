#!/usr/bin/env bash
# Deterministic unit test for ci-classify-change.sh. Stubs for `gh` and `git` on PATH serve the
# changed-file list and the ecluse.cabal diff, so no network or repository is needed. The case that
# must never happen is a skipped job for a path outside the lists. Run via `task test-scripts`.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
script="$here/ci-classify-change.sh"

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir "$work/bin"
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
[ -z "${STUB_GH_FAIL:-}" ] || exit 1
printf '%s' "${STUB_FILES:-}"
STUB
cat > "$work/bin/git" <<'STUB'
#!/usr/bin/env bash
case "$*" in
  "rev-parse -q --verify HEAD^2")
    [ -z "${STUB_PARENT_FAIL:-}" ] || exit 1
    printf '%s\n' "${STUB_PARENT-head}" ;;
  "diff --no-color --no-ext-diff --no-textconv --unified=1000000 HEAD^1 HEAD -- ecluse.cabal")
    [ -z "${STUB_DIFF_FAIL:-}" ] || exit 1
    printf '%s' "${STUB_DIFF:-}" ;;
  *) exit 1 ;;
esac
STUB
chmod +x "$work/bin/gh" "$work/bin/git"

# A whole-file diff of ecluse.cabal, as `git diff --unified=1000000` prints it. $1 the changed line.
cabal_diff() {
  printf '%s\n' '--- a/ecluse.cabal' '+++ b/ecluse.cabal' '@@ -1,4 +1,5 @@' ' library' \
    '   exposed-modules:' '     Ecluse' "$1" '   build-depends: base'
}
module_diff="$(cabal_diff '+    Ecluse.Extra')"
depends_diff="$(cabal_diff '+  build-depends: text')"

fail=0

# Assert the outputs for one event: $1 name, $2-$4 docs-only, release-build and release-rebuild, $5
# event, then the changed paths. STUB_ variables pass through, and HEAD_SHA defaults to the stub's.
check() {
  local name="$1" want="docs-only=$2 release-build=$3 release-rebuild=$4 " event="$5" got
  shift 5
  : > "$work/out"
  STUB_FILES="$(printf '%s\n' "$@")" \
    EVENT_NAME="$event" PR_NUMBER=1 REPO=owner/repo GITHUB_OUTPUT="$work/out" \
    HEAD_SHA="${HEAD_SHA-head}" PATH="$work/bin:$PATH" bash "$script" >/dev/null
  got="$(tr '\n' ' ' < "$work/out")"
  if [ "$got" = "$want" ]; then
    printf 'ok   - %s\n' "$name"
  else
    printf 'FAIL - %s (want %s, got %s)\n' "$name" "$want" "$got"
    fail=1
  fi
}

check "documentation alone skips the Haskell jobs and the dry-run" true false false pull_request \
  README.md docs/testing.md web/content/docs/index.md
check "Haskell source runs the Haskell jobs and skips the dry-run" false false false pull_request \
  src/Ecluse.hs core/src/Ecluse/Core/Package.hs test/unit/Spec.hs docs/testing.md
check "a runbook or an analysis config skips the dry-run" false false false pull_request \
  runbooks/release.md .hlint.yaml weeder.toml
check "an included Taskfile runs the dry-run" false true false pull_request taskfiles/test.yml
check "a script the release never runs skips the dry-run" false false false pull_request \
  scripts/coverage.sh scripts/test-containers.sh
check "a script test skips the dry-run" false false false pull_request scripts/image-archive.test.sh
check "the npm oracle under test/ runs the dry-run" false true false pull_request \
  test/unit/Spec.hs test/oracles/package-lock.json
STUB_DIFF="$module_diff" check "an ecluse.cabal module-list edit skips the dry-run" \
  false false false pull_request ecluse.cabal src/Ecluse.hs
STUB_DIFF="$depends_diff" check "an ecluse.cabal build-depends edit runs the dry-run" \
  false true false pull_request ecluse.cabal src/Ecluse.hs
STUB_DIFF="$module_diff" check "ecluse.cabal beside another release path runs the dry-run" \
  false true false pull_request ecluse.cabal flake.lock
STUB_DIFF="$module_diff" STUB_DIFF_FAIL=1 check "an unreadable ecluse.cabal diff runs the dry-run" \
  false true false pull_request ecluse.cabal
STUB_DIFF="$module_diff" STUB_PARENT_FAIL=1 \
  check "a checkout with no merge commit runs the dry-run" false true false pull_request ecluse.cabal
STUB_DIFF="$module_diff" STUB_PARENT=other check "a merge commit of another head runs the dry-run" \
  false true false pull_request ecluse.cabal
STUB_DIFF="$module_diff" STUB_PARENT='' HEAD_SHA='' check "an unknown PR head runs the dry-run" \
  false true false pull_request ecluse.cabal
check "an empty ecluse.cabal diff runs the dry-run" false true false pull_request ecluse.cabal
check "the freeze runs the dry-run" false true false pull_request cabal.project.freeze
check "cabal.project runs the dry-run" false true false pull_request cabal.project
check "the flake runs the dry-run" false true false pull_request flake.lock
check "the root Taskfile runs the dry-run" false true false pull_request Taskfile.yml
check "a workflow runs the dry-run" false true false pull_request .github/workflows/ci.yml
check "a CI action runs the dry-run" false true false pull_request \
  .github/actions/setup-toolchain/action.yml
check "a script the release runs runs the dry-run" false true false pull_request \
  scripts/push-multiarch.sh
check "the classifier runs the dry-run" false true false pull_request scripts/ci-cabal-diff.sh
check "an unlisted script runs the dry-run" false true false pull_request scripts/new-tool.sh
check "an unlisted top-level path runs everything" false true false pull_request newdir/file.txt
check "a nested Markdown file outside the lists runs everything" false true false pull_request \
  scripts/notes.md
check "a push to main runs everything" false true true push src/Ecluse.hs
check "the nightly schedule runs everything" false true true schedule
check "a manual dispatch runs everything" false true true workflow_dispatch

: > "$work/out"
STUB_GH_FAIL=1 EVENT_NAME=pull_request PR_NUMBER=1 REPO=owner/repo GITHUB_OUTPUT="$work/out" \
  PATH="$work/bin:$PATH" bash "$script" >/dev/null
everything="docs-only=false release-build=true release-rebuild=true "
if [ "$(tr '\n' ' ' < "$work/out")" = "$everything" ]; then
  printf 'ok   - %s\n' "an unreadable file list runs everything"
else
  printf 'FAIL - %s\n' "an unreadable file list runs everything"
  fail=1
fi

exit "$fail"
