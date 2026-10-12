#!/usr/bin/env bash
# Audit one ecosystem against current unit, integration and E2E reports.
set -euo pipefail

case "${1:-}" in
  npm|pypi) subject="$1" ;;
  *) echo 'ecosystem completion: expected npm or pypi' >&2; exit 2 ;;
esac
[ "$#" -eq 1 ] || { echo 'ecosystem completion: fixed subjects accept no extra arguments' >&2; exit 2; }

e2e_root="${ECLTEST_ARTIFACT_DIR:-}"
core_root="${ECLTEST_CORE_UNIT_ARTIFACT_DIR:-}"
runtime_root="${ECLTEST_RUNTIME_UNIT_ARTIFACT_DIR:-}"
integration_root="${ECLTEST_INTEGRATION_ARTIFACT_DIR:-}"
[ -n "$e2e_root" ] && [ -n "$core_root" ] && [ -n "$runtime_root" ] && [ -n "$integration_root" ] \
  || { echo 'ecosystem completion: set all four artifact directories' >&2; exit 2; }
for path in \
  "$core_root/unit-plan.json" \
  "$core_root/unit-report.json" \
  "$runtime_root/unit-plan.json" \
  "$runtime_root/unit-report.json" \
  "$e2e_root/identity.json" \
  "$e2e_root/plan.json" \
  "$e2e_root/reports/$subject.json" \
  "$integration_root/plan.json" \
  "$integration_root/report.json"; do
  [ -s "$path" ] || { echo "ecosystem completion: missing evidence: $path" >&2; exit 1; }
done

export ECLTEST_IMAGE_ID
ECLTEST_IMAGE_ID="$(jq -er '.image | select(test("^sha256:[a-f0-9]{64}$"))' "$e2e_root/identity.json")"
export ECLTEST_REQUIRED=1
component="ecluse-completion-$subject-audit"
cabal build "$component"
binary="$(cabal list-bin "$component")"
"$binary" \
  "$core_root/unit-plan.json" \
  "$core_root/unit-report.json" \
  "$runtime_root/unit-plan.json" \
  "$runtime_root/unit-report.json" \
  "$integration_root/plan.json" \
  "$integration_root/report.json" \
  "$e2e_root/plan.json" \
  "$e2e_root/reports/$subject.json"
