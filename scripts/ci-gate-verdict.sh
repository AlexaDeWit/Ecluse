#!/usr/bin/env bash
# Decide the CI gate from its dependencies' results, for .github/workflows/ci.yml.
#
# A skip counts as a pass from exactly one source: the documentation-only filter, and
# only for the jobs that filter may skip. Every other skip, failure, or cancellation
# fails the gate, so a job that silently never ran can never read as green.
set -euo pipefail

verdict=0

require() {
  local job="$1" result="$2" skippable="${3:-no}"
  if [ "$result" = "success" ]; then
    echo "ok      $job"
    return 0
  fi
  if [ "$result" = "skipped" ] && [ "$skippable" = "yes" ] && [ "${DOCS_ONLY:-}" = "true" ]; then
    echo "ok      $job: skipped by the documentation-only filter"
    return 0
  fi
  echo "FAILED  $job: $result"
  verdict=1
  return 0
}

require changes "${CHANGES:-missing}"
require static-checks "${STATIC_CHECKS:-missing}"
require build-test "${BUILD_TEST:-missing}" yes
require docs "${DOCS:-missing}" yes
require e2e "${E2E:-missing}" yes
require weeder "${WEEDER:-missing}" yes
require stan "${STAN:-missing}" yes

exit "$verdict"
