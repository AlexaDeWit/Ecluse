#!/usr/bin/env bash
# Select complete integration groups without changing the instrumented suite boundary.
set -euo pipefail

mode="${1:-run}"
[ "$#" -eq 0 ] || shift
case "$mode" in
  run|list|plan|coverage) ;;
  *) echo "integration: unknown mode '$mode'" >&2; exit 2 ;;
esac
if [ -n "${ECLTEST_INTEGRATION_MATCH:-}" ]; then
  echo 'integration: MATCH is replaced by stable GROUP selection' >&2
  exit 2
fi

# Coverage supplies its already instrumented executable and runs every group.
if [ -n "${ECLTEST_INTEGRATION_BINARY:-}" ]; then
  binary="$ECLTEST_INTEGRATION_BINARY"
else
  cabal build ecluse-integration >&2
  binary="$(cabal list-bin ecluse-integration)"
fi
catalogue() {
  local directory status=0
  mkdir -p scratchpad/integration-discovery
  directory="$(mktemp -d scratchpad/integration-discovery/run.XXXXXXXXXX)"
  HPCTIXFILE="$directory/catalogue.tix" "$binary" "$@" || status=$?
  rm -f "$directory/catalogue.tix"
  rmdir "$directory"
  return "$status"
}
case "$mode" in
  list) catalogue --catalogue-list; exit ;;
  plan) catalogue --catalogue-plan; exit ;;
esac

args=()
[ -z "${ECLTEST_INTEGRATION_GROUP:-}" ] || args+=(--group "$ECLTEST_INTEGRATION_GROUP")
[ -z "${ECLTEST_INTEGRATION_SHARD:-}" ] || args+=(--shard "$ECLTEST_INTEGRATION_SHARD")
[ -z "${ECLTEST_INTEGRATION_ECOSYSTEM:-}" ] || args+=(--ecosystem "$ECLTEST_INTEGRATION_ECOSYSTEM")
[ -z "${ECLTEST_INTEGRATION_BACKEND:-}" ] || args+=(--backend "$ECLTEST_INTEGRATION_BACKEND")
[ -z "${ECLTEST_INTEGRATION_CAPABILITY:-}" ] || args+=(--capability "$ECLTEST_INTEGRATION_CAPABILITY")
if [ "$mode" = coverage ]; then
  if [ "${#args[@]}" -gt 0 ]; then
    echo 'integration: coverage must run the complete suite without selection' >&2
    exit 2
  fi
else
  args+=("$@")
fi
selected="$(catalogue --catalogue-select "${args[@]}")"
export ECLTEST_RUN_ID="${ECLTEST_RUN_ID:-$(bash scripts/test-containers.sh run-id)}"
export ECLTEST_SOURCE="${ECLTEST_SOURCE:-$(git rev-parse HEAD)}"
export ECLTEST_ARCHITECTURE="${ECLTEST_ARCHITECTURE:-$(uname -m)}"
export ECLTEST_SHARD="${ECLTEST_SHARD:-${ECLTEST_INTEGRATION_SHARD:-integration}}"
root="${ECLTEST_INTEGRATION_ARTIFACT_DIR:-scratchpad/integration/$ECLTEST_RUN_ID}"
mkdir -p "$root"
catalogue --catalogue-plan > "$root/plan.json"
printf '%s\n' "$selected" > "$root/selected.json"
export ECLTEST_PLAN_DIGEST
ECLTEST_PLAN_DIGEST="$(sha256sum "$root/plan.json" | cut -d' ' -f1)"
if [ "$mode" = coverage ]; then
  command=(cabal test ecluse-integration "$@" --test-option=--report "--test-option=$root/report.json")
else
  command=("$binary" "${args[@]}" --report "$root/report.json")
fi
if jq -e 'any(.[]; .groupPrerequisites | index("docker"))' "$root/selected.json" >/dev/null; then
  if [ "${ECLTEST_CONTAINER_LOCK:-}" = held ]; then
    exec "${command[@]}"
  fi
  exec bash scripts/test-containers.sh run "${command[@]}"
fi
exec "${command[@]}"
