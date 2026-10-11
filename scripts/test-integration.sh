#!/usr/bin/env bash
# Preserve the full suite by default and expose Hspec selection outside coverage.
set -euo pipefail
args=(cabal test ecluse-integration --test-show-details=direct)
if [ -n "${ECLTEST_INTEGRATION_MATCH:-}" ]; then
  args+=(--test-option=--match "--test-option=$ECLTEST_INTEGRATION_MATCH" --test-option=--fail-on=empty)
fi
exec bash scripts/test-containers.sh run "${args[@]}"
