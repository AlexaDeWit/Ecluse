#!/usr/bin/env bash
# Deterministic unit test for ci-classify-change.sh. A stub `gh` on PATH serves the
# changed-file list, so no network is needed. The case that must never happen is a
# skipped job for a path outside the lists. Run via `task test-scripts`.
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
chmod +x "$work/bin/gh"

fail=0

# Assert both outputs for one event. $1 name, $2 docs-only, $3 release-build, $4 event,
# then the changed paths, one per argument.
check() {
  local name="$1" want_docs="$2" want_release="$3" event="$4" got
  shift 4
  : > "$work/out"
  STUB_FILES="$(printf '%s\n' "$@")" \
    EVENT_NAME="$event" PR_NUMBER=1 REPO=owner/repo GITHUB_OUTPUT="$work/out" \
    PATH="$work/bin:$PATH" bash "$script" >/dev/null
  got="$(tr '\n' ' ' < "$work/out")"
  if [ "$got" = "docs-only=$want_docs release-build=$want_release " ]; then
    printf 'ok   - %s\n' "$name"
  else
    printf 'FAIL - %s (want docs-only=%s release-build=%s, got %s)\n' \
      "$name" "$want_docs" "$want_release" "$got"
    fail=1
  fi
}

check "documentation alone skips the Haskell jobs and the dry-run" true false pull_request \
  README.md docs/testing.md web/content/docs/index.md
check "Haskell source runs the Haskell jobs and skips the dry-run" false false pull_request \
  src/Ecluse.hs core/src/Ecluse/Core/Package.hs test/unit/Spec.hs docs/testing.md
check "a runbook or an analysis config skips the dry-run" false false pull_request \
  runbooks/release.md .hlint.yaml weeder.toml
check "the npm oracle under test/ runs the dry-run" false true pull_request \
  test/unit/Spec.hs test/oracles/package-lock.json
check "ecluse.cabal runs the dry-run" false true pull_request \
  ecluse.cabal src/Ecluse.hs
check "the freeze runs the dry-run" false true pull_request cabal.project.freeze
check "cabal.project runs the dry-run" false true pull_request cabal.project
check "the flake runs the dry-run" false true pull_request flake.lock
check "the Taskfile runs the dry-run" false true pull_request Taskfile.yml
check "a workflow runs the dry-run" false true pull_request .github/workflows/ci.yml
check "a CI action runs the dry-run" false true pull_request .github/actions/setup-toolchain/action.yml
check "a script runs the dry-run" false true pull_request scripts/push-multiarch.sh
check "an unlisted top-level path runs everything" false true pull_request newdir/file.txt
check "a nested Markdown file outside the lists runs everything" false true pull_request scripts/notes.md
check "a push to main runs everything" false true push src/Ecluse.hs
check "the nightly schedule runs everything" false true schedule
check "a manual dispatch runs everything" false true workflow_dispatch

: > "$work/out"
STUB_GH_FAIL=1 EVENT_NAME=pull_request PR_NUMBER=1 REPO=owner/repo GITHUB_OUTPUT="$work/out" \
  PATH="$work/bin:$PATH" bash "$script" >/dev/null
if [ "$(tr '\n' ' ' < "$work/out")" = "docs-only=false release-build=true " ]; then
  printf 'ok   - %s\n' "an unreadable file list runs everything"
else
  printf 'FAIL - %s\n' "an unreadable file list runs everything"
  fail=1
fi

exit "$fail"
