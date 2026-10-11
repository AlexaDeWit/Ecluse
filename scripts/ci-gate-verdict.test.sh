#!/usr/bin/env bash
# A skipped job without a classifier filter must fail. Run via `task test-scripts`.
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

# A later assignment to the same variable overrides these in `env`.
all_pass=(
  CHANGES=success STATIC_CHECKS=success BUILD=success ALLOCATION=success COVERAGE=success CODECOV_NOTIFY=success
  DOCS=success E2E_PREPARE=success E2E_SHARDS=success E2E=success WEEDER=success STAN=success RELEASE_DRY_RUN=success
  RELEASE_DRY_RUN_ASSEMBLE=success RELEASE_DRY_RUN_BOOT=success RELEASE_COMPARE=success RELEASE_BUILD=true
)
release_skipped=(RELEASE_DRY_RUN=skipped RELEASE_DRY_RUN_ASSEMBLE=skipped RELEASE_DRY_RUN_BOOT=skipped RELEASE_COMPARE=skipped)

check "a code PR with every job green passes" 0 \
  "${all_pass[@]}" DOCS_ONLY=false

check "a code PR with a failing job fails" 1 \
  "${all_pass[@]}" E2E=failure DOCS_ONLY=false

check "a code PR with a job skipped outside the filter fails" 1 \
  "${all_pass[@]}" BUILD=skipped DOCS_ONLY=false

check "a leg over its allocation budget fails" 1 \
  "${all_pass[@]}" ALLOCATION=failure DOCS_ONLY=false

check "a skipped allocation check fails outside the documentation-only filter" 1 \
  "${all_pass[@]}" ALLOCATION=skipped DOCS_ONLY=false

check "a code PR with a failed coverage leg fails" 1 \
  "${all_pass[@]}" COVERAGE=failure CODECOV_NOTIFY=skipped DOCS_ONLY=false

check "an unsent Codecov status fails behind green coverage" 1 \
  "${all_pass[@]}" CODECOV_NOTIFY=failure DOCS_ONLY=false

check "a skipped Codecov notify fails behind green coverage" 1 \
  "${all_pass[@]}" CODECOV_NOTIFY=skipped DOCS_ONLY=false

check "a documentation-only PR passes with the Haskell jobs, notify, and release jobs skipped" 0 \
  CHANGES=success STATIC_CHECKS=success \
  BUILD=skipped ALLOCATION=skipped COVERAGE=skipped CODECOV_NOTIFY=skipped \
  DOCS=skipped E2E_PREPARE=skipped E2E_SHARDS=skipped E2E=skipped WEEDER=skipped STAN=skipped \
  "${release_skipped[@]}" DOCS_ONLY=true RELEASE_BUILD=false

check "a failed classifier fails, so a broken filter never waves a PR through" 1 \
  "${all_pass[@]}" CHANGES=failure DOCS_ONLY=true

check "static-checks is never skippable, even on a documentation-only PR" 1 \
  "${all_pass[@]}" STATIC_CHECKS=skipped DOCS_ONLY=true

check "a cancelled job fails" 1 \
  "${all_pass[@]}" STAN=cancelled DOCS_ONLY=false

check "a source-only PR passes with the release jobs skipped" 0 \
  "${all_pass[@]}" "${release_skipped[@]}" DOCS_ONLY=false RELEASE_BUILD=false

check "a skipped dry-run fails when the change reaches the release build" 1 \
  "${all_pass[@]}" "${release_skipped[@]}" DOCS_ONLY=false RELEASE_BUILD=true

check "a skipped dry-run fails when the classifier output is missing" 1 \
  "${all_pass[@]}" "${release_skipped[@]}" DOCS_ONLY=false RELEASE_BUILD=

check "the documentation-only filter never skips the dry-run on its own" 1 \
  "${all_pass[@]}" "${release_skipped[@]}" DOCS_ONLY=true RELEASE_BUILD=true

check "a failed dry-run fails" 1 \
  "${all_pass[@]}" RELEASE_DRY_RUN=failure RELEASE_DRY_RUN_ASSEMBLE=skipped RELEASE_DRY_RUN_BOOT=skipped

check "a failed multi-arch assembly fails" 1 \
  "${all_pass[@]}" RELEASE_DRY_RUN_ASSEMBLE=failure

check "an image that fails to start fails" 1 \
  "${all_pass[@]}" RELEASE_DRY_RUN_BOOT=failure

check "a skipped boot behind a green dry-run fails whatever the filter says" 1 \
  "${all_pass[@]}" RELEASE_DRY_RUN_BOOT=skipped RELEASE_BUILD=false

check "an image mismatch fails the gate" 1 \
  "${all_pass[@]}" RELEASE_COMPARE=failure

check "a cancelled image comparison fails the gate" 1 \
  "${all_pass[@]}" RELEASE_COMPARE=cancelled

check "a skipped comparison after a failed rebuild fails the gate" 1 \
  "${all_pass[@]}" RELEASE_COMPARE=skipped

check "a skipped comparison after a failed dry-run fails the gate" 1 \
  "${all_pass[@]}" RELEASE_DRY_RUN=failure RELEASE_COMPARE=skipped

check "a skipped comparison fails when the classifier output is missing" 1 \
  "${all_pass[@]}" RELEASE_COMPARE=skipped RELEASE_BUILD=

check "the documentation-only filter never skips the comparison on its own" 1 \
  "${all_pass[@]}" RELEASE_COMPARE=skipped DOCS_ONLY=true

check "a missing comparison result fails the gate" 1 \
  "${all_pass[@]}" RELEASE_COMPARE=

check "a failed comparison is not excused by the release filter" 1 \
  "${all_pass[@]}" RELEASE_COMPARE=failure RELEASE_BUILD=false

for job in E2E_PREPARE E2E_SHARDS E2E; do
  for result in failure skipped cancelled missing ''; do
    check "$job $result fails despite the other E2E stages succeeding" 1 \
      "${all_pass[@]}" "$job=$result" DOCS_ONLY=false
  done
done

exit "$fail"
