#!/usr/bin/env bash
# Decide the CI gate from its dependencies' results, for .github/workflows/ci.yml.
# Only a classifier filter can excuse a skipped job.
set -euo pipefail

verdict=0

# $3 names the classifier filter that may skip the job: docs, release, rebuild, or none.
require() {
  local job="$1" result="$2" filter="${3:-none}"
  if [ "$result" = "success" ]; then
    echo "ok      $job"
    return 0
  fi
  if [ "$result" = "skipped" ]; then
    if [ "$filter" = "docs" ] && [ "${DOCS_ONLY:-}" = "true" ]; then
      echo "ok      $job: skipped by the documentation-only filter"
      return 0
    fi
    if [ "$filter" = "release" ] && [ "${RELEASE_BUILD:-}" = "false" ]; then
      echo "ok      $job: skipped, the change cannot reach the release build"
      return 0
    fi
    if [ "$filter" = "rebuild" ] && [ "${RELEASE_REBUILD:-}" = "false" ]; then
      echo "ok      $job: skipped, a pull request runs no second image build"
      return 0
    fi
  fi
  echo "FAILED  $job: $result"
  verdict=1
  return 0
}

require changes "${CHANGES:-missing}"
require static-checks "${STATIC_CHECKS:-missing}"
require build "${BUILD:-missing}" docs
require allocation "${ALLOCATION:-missing}" docs
require coverage "${COVERAGE:-missing}" docs

# Coverage failure already fails the gate and prevents codecov-notify from running.
if [ "${COVERAGE:-missing}" = "success" ]; then
  require codecov-notify "${CODECOV_NOTIFY:-missing}"
else
  echo "ok      codecov-notify: not run, coverage did not succeed"
fi
require docs "${DOCS:-missing}" docs
require e2e-prepare "${E2E_PREPARE:-missing}" docs
require e2e-conformance "${E2E_CONFORMANCE:-missing}" docs
require e2e "${E2E:-missing}" docs
require backend-conformance "${BACKEND_CONFORMANCE:-missing}" docs
require weeder "${WEEDER:-missing}" docs
require stan "${STAN:-missing}" docs

# The assemble and boot jobs need the dry-run's images, so the same reasoning applies.
require release-dry-run "${RELEASE_DRY_RUN:-missing}" release
if [ "${RELEASE_DRY_RUN:-missing}" = "success" ]; then
  require release-dry-run-assemble "${RELEASE_DRY_RUN_ASSEMBLE:-missing}"
  require release-dry-run-boot "${RELEASE_DRY_RUN_BOOT:-missing}"
else
  echo "ok      release-dry-run-assemble, release-dry-run-boot: not run, release-dry-run did not succeed"
fi

# A failed build also skips the comparison, so only the rebuild filter excuses a skip.
require release-compare "${RELEASE_COMPARE:-missing}" rebuild

exit "$verdict"
