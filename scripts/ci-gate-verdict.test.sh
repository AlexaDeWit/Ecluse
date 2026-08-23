#!/usr/bin/env bash
# Deterministic unit test for ci-gate-verdict.sh, the CI gate's verdict. The gate is the
# branch-protection authority, so the one case that must never pass is a job that never
# ran without the documentation-only filter behind it. Run via `task test-scripts`.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
script="$here/ci-gate-verdict.sh"

fail=0

# Assert the verdict for one result set. $1 name, $2 expected exit, then the env.
check() {
  local name="$1" want="$2" got=0
  shift 2
  env "$@" bash "$script" >/dev/null 2>&1 || got=$?
  if [ "$got" = "$want" ]; then
    printf 'ok   - %s\n' "$name"
  else
    printf 'FAIL - %s (want exit %s, got %s)\n' "$name" "$want" "$got"
    fail=1
  fi
}

all_pass="CHANGES=success STATIC_CHECKS=success BUILD_TEST=success DOCS=success E2E=success WEEDER=success STAN=success"

# shellcheck disable=SC2086 # the shared result set is a deliberate word-split list
check "a code PR with every job green passes" 0 \
  $all_pass DOCS_ONLY=false

# shellcheck disable=SC2086
check "a code PR with a failing job fails" 1 \
  $all_pass E2E=failure DOCS_ONLY=false

# shellcheck disable=SC2086
check "a code PR with a job skipped outside the filter fails" 1 \
  $all_pass BUILD_TEST=skipped DOCS_ONLY=false

check "a documentation-only PR passes with the five Haskell jobs skipped" 0 \
  CHANGES=success STATIC_CHECKS=success \
  BUILD_TEST=skipped DOCS=skipped E2E=skipped WEEDER=skipped STAN=skipped \
  DOCS_ONLY=true

# shellcheck disable=SC2086
check "a failed classifier fails, so a broken filter never waves a PR through" 1 \
  $all_pass CHANGES=failure DOCS_ONLY=true

# shellcheck disable=SC2086
check "static-checks is never skippable, even on a documentation-only PR" 1 \
  $all_pass STATIC_CHECKS=skipped DOCS_ONLY=true

# shellcheck disable=SC2086
check "a cancelled job fails" 1 \
  $all_pass STAN=cancelled DOCS_ONLY=false

exit "$fail"
