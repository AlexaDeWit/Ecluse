#!/usr/bin/env bash
# Run one fixed package-store contract and audit its execution report.
set -euo pipefail

case "${1:-}" in
  verdaccio|codeartifact) subject="$1" ;;
  *) echo 'package-store conformance: expected verdaccio or codeartifact' >&2; exit 2 ;;
esac
[ "$#" -eq 1 ] || { echo 'package-store conformance: fixed subjects accept no extra arguments' >&2; exit 2; }

component="ecluse-conformance-$subject"
manifest="$component-manifest"
audit="$component-audit"
mkdir -p scratchpad/backend-conformance
root="$(mktemp -d "scratchpad/backend-conformance/$subject.XXXXXXXXXX")"
printf 'Package-store conformance artifacts: %s\n' "$root"

export ECLTEST_REPORT_PATH="$root/report.json"
export ECLTEST_RUN_ID="${ECLTEST_RUN_ID:-$(bash scripts/test-containers.sh run-id)}"
export ECLTEST_SOURCE="${ECLTEST_SOURCE:-$(git rev-parse HEAD)}"
export ECLTEST_ARCHITECTURE="${ECLTEST_ARCHITECTURE:-$(uname -m)}"
export ECLTEST_REQUIRED=1

cabal build "$manifest" "$component" "$audit"
manifest_bin="$(cabal list-bin "$manifest")"
component_bin="$(cabal list-bin "$component")"
audit_bin="$(cabal list-bin "$audit")"
"$manifest_bin" > "$root/plan.json"
export ECLTEST_PLAN_DIGEST
ECLTEST_PLAN_DIGEST="$(sha256sum "$root/plan.json" | cut -d' ' -f1)"

status=0
"$component_bin" > "$root/run.log" 2>&1 || status=$?
cat "$root/run.log"
if [ ! -s "$ECLTEST_REPORT_PATH" ]; then
  echo 'package-store conformance: missing current execution report' >&2
  exit 1
fi
"$audit_bin" "$root/plan.json" "$ECLTEST_REPORT_PATH" || status=1
exit "$status"
