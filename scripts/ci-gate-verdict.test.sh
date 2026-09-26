#!/usr/bin/env bash
# Deterministic unit test for ci-gate-verdict.sh, the CI gate's verdict. The gate is the
# branch-protection authority, so the one case that must never pass is a job that never
# ran without a classifier filter behind it. Run via `task test-scripts`.
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

all_pass="CHANGES=success STATIC_CHECKS=success BUILD=success COVERAGE=success CODECOV_NOTIFY=success DOCS=success E2E=success WEEDER=success STAN=success RELEASE_DRY_RUN=success RELEASE_DRY_RUN_ASSEMBLE=success RELEASE_DRY_RUN_BOOT=success RELEASE_BUILD=true"
dry_run_skipped="RELEASE_DRY_RUN=skipped RELEASE_DRY_RUN_ASSEMBLE=skipped RELEASE_DRY_RUN_BOOT=skipped"

# shellcheck disable=SC2086 # the shared result set is a deliberate word-split list
check "a code PR with every job green passes" 0 \
  $all_pass DOCS_ONLY=false

# shellcheck disable=SC2086
check "a code PR with a failing job fails" 1 \
  $all_pass E2E=failure DOCS_ONLY=false

# shellcheck disable=SC2086
check "a code PR with a job skipped outside the filter fails" 1 \
  $all_pass BUILD=skipped DOCS_ONLY=false

# shellcheck disable=SC2086
check "a code PR with a failed coverage leg fails" 1 \
  $all_pass COVERAGE=failure CODECOV_NOTIFY=skipped DOCS_ONLY=false

# shellcheck disable=SC2086
check "an unsent Codecov status fails behind green coverage" 1 \
  $all_pass CODECOV_NOTIFY=failure DOCS_ONLY=false

# shellcheck disable=SC2086
check "a skipped Codecov notify fails behind green coverage" 1 \
  $all_pass CODECOV_NOTIFY=skipped DOCS_ONLY=false

# shellcheck disable=SC2086
check "a documentation-only PR passes with the Haskell jobs, notify, and the dry-run skipped" 0 \
  CHANGES=success STATIC_CHECKS=success \
  BUILD=skipped COVERAGE=skipped CODECOV_NOTIFY=skipped \
  DOCS=skipped E2E=skipped WEEDER=skipped STAN=skipped \
  $dry_run_skipped DOCS_ONLY=true RELEASE_BUILD=false

# shellcheck disable=SC2086
check "a failed classifier fails, so a broken filter never waves a PR through" 1 \
  $all_pass CHANGES=failure DOCS_ONLY=true

# shellcheck disable=SC2086
check "static-checks is never skippable, even on a documentation-only PR" 1 \
  $all_pass STATIC_CHECKS=skipped DOCS_ONLY=true

# shellcheck disable=SC2086
check "a cancelled job fails" 1 \
  $all_pass STAN=cancelled DOCS_ONLY=false

# shellcheck disable=SC2086
check "a source-only PR passes with the release dry-run skipped" 0 \
  $all_pass $dry_run_skipped DOCS_ONLY=false RELEASE_BUILD=false

# shellcheck disable=SC2086
check "a skipped dry-run fails when the change reaches the release build" 1 \
  $all_pass $dry_run_skipped DOCS_ONLY=false RELEASE_BUILD=true

# shellcheck disable=SC2086
check "a skipped dry-run fails when the classifier output is missing" 1 \
  $all_pass $dry_run_skipped DOCS_ONLY=false RELEASE_BUILD=

# shellcheck disable=SC2086
check "the documentation-only filter never skips the dry-run on its own" 1 \
  $all_pass $dry_run_skipped DOCS_ONLY=true RELEASE_BUILD=true

# shellcheck disable=SC2086
check "a failed dry-run fails" 1 \
  $all_pass RELEASE_DRY_RUN=failure RELEASE_DRY_RUN_ASSEMBLE=skipped RELEASE_DRY_RUN_BOOT=skipped

# shellcheck disable=SC2086
check "a failed multi-arch assembly fails" 1 \
  $all_pass RELEASE_DRY_RUN_ASSEMBLE=failure

# shellcheck disable=SC2086
check "an image that fails to start fails" 1 \
  $all_pass RELEASE_DRY_RUN_BOOT=failure

# shellcheck disable=SC2086
check "a skipped boot behind a green dry-run fails whatever the filter says" 1 \
  $all_pass RELEASE_DRY_RUN_BOOT=skipped RELEASE_BUILD=false

exit "$fail"
