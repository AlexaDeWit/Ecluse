#!/usr/bin/env bash
# Run the fixed integration component and audit its complete execution report.
set -euo pipefail

mode="${1:-run}"
[ "$#" -eq 0 ] || shift
case "$mode" in
  run|coverage|validate-coverage) ;;
  *) echo "integration: unknown mode '$mode'" >&2; exit 2 ;;
esac
validate_coverage_options() {
  local option
  for option in "$@"; do
    case "$option" in
      --enable-coverage|--builddir=dist-coverage|--test-show-details=direct|-fdev-http-egress|--flags=dev-http-egress) ;;
      *) echo "integration: unsupported coverage option '$option'" >&2; return 2 ;;
    esac
  done
}
if [ "$mode" = coverage ] || [ "$mode" = validate-coverage ]; then
  validate_coverage_options "$@"
  [ "$mode" != validate-coverage ] || exit 0
elif [ "$#" -ne 0 ]; then
  echo 'integration: the fixed suite accepts no arguments' >&2
  exit 2
fi

# Metadata tools get separate tick files even when their dependencies use HPC.
metadata() {
  local component="$1" binary directory status=0
  shift
  cabal build "$component" >&2
  binary="$(cabal list-bin "$component")"
  mkdir -p scratchpad/integration-discovery
  directory="$(mktemp -d scratchpad/integration-discovery/run.XXXXXXXXXX)"
  HPCTIXFILE="$directory/metadata.tix" "$binary" "$@" || status=$?
  rm -f "$directory/metadata.tix"
  rmdir "$directory"
  return "$status"
}
export ECLTEST_SHARD="${ECLTEST_SHARD:-integration}"
export ECLTEST_RUN_ID="${ECLTEST_RUN_ID:-$(bash scripts/test-containers.sh run-id)}"
export ECLTEST_SOURCE="${ECLTEST_SOURCE:-$(git rev-parse HEAD)}"
export ECLTEST_ARCHITECTURE="${ECLTEST_ARCHITECTURE:-$(uname -m)}"
root="${ECLTEST_INTEGRATION_ARTIFACT_DIR:-scratchpad/integration/$ECLTEST_RUN_ID}"
mkdir -p "$root"
export ECLTEST_REPORT_PATH="$root/report.json"
rm -f "$ECLTEST_REPORT_PATH"
metadata ecluse-integration-manifest > "$root/plan.json"
export ECLTEST_PLAN_DIGEST
ECLTEST_PLAN_DIGEST="$(sha256sum "$root/plan.json" | cut -d' ' -f1)"
if [ "$mode" = coverage ]; then
  command=(cabal test ecluse-integration "$@")
else
  command=(cabal test ecluse-integration --test-show-details=direct)
fi
if [ "${ECLTEST_CONTAINER_LOCK:-}" != held ]; then
  bash scripts/test-containers.sh run "${command[@]}"
else
  "${command[@]}"
fi
metadata ecluse-integration-audit "$root/plan.json" "$ECLTEST_REPORT_PATH"
