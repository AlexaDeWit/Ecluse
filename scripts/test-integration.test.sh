#!/usr/bin/env bash
# Exercise integration discovery, selection, identities, and coverage routing without services.
set -euo pipefail
root="$(git rev-parse --show-toplevel)"
cd "$root"
mkdir -p scratchpad
fixture="$(mktemp -d scratchpad/integration-script.XXXXXXXXXX)"
trap 'rm -rf "$fixture"' EXIT
fixture="$(realpath "$fixture")"
export ECLTEST_INTEGRATION_BINARY="$fixture/runner"
export ECLTEST_INTEGRATION_ARTIFACT_DIR="$fixture/artifacts"
export ECLTEST_SCRIPT_LOG="$fixture/log"
export ECLTEST_SOURCE=test-source
export ECLTEST_ARCHITECTURE=test-architecture
export ECLTEST_RUN_ID=test-run
export PATH="$fixture:$PATH"
unset ECLTEST_INTEGRATION_GROUP ECLTEST_INTEGRATION_SHARD ECLTEST_INTEGRATION_ECOSYSTEM ECLTEST_INTEGRATION_BACKEND ECLTEST_INTEGRATION_CAPABILITY ECLTEST_INTEGRATION_MATCH
cat > "$fixture/runner" <<'RUNNER'
#!/usr/bin/env bash
set -euo pipefail
case "${1:-}" in
  --catalogue-list|--catalogue-plan)
    printf '{"schemaVersion":1}\n'
    printf 'instrumentation\n' > "${HPCTIXFILE:?discovery must isolate instrumentation}"
    ;;
  --catalogue-select)
    shift
    if [ "${1:-}" = --group ] && [ "${2:-}" = absent ]; then exit 19; fi
    printf '[{"groupPrerequisites":[]}]\n'
    ;;
  *)
    [ "${ECLTEST_SOURCE:?}" = test-source ]
    [ "${ECLTEST_ARCHITECTURE:?}" = test-architecture ]
    [ "${ECLTEST_RUN_ID:?}" = test-run ]
    [ "${#ECLTEST_PLAN_DIGEST}" -eq 64 ]
    printf 'runner %s\n' "$*" >> "$ECLTEST_SCRIPT_LOG"
    ;;
esac
RUNNER
cat > "$fixture/docker" <<'DOCKER'
#!/usr/bin/env bash
printf 'unexpected Docker acquisition\n' >> "$ECLTEST_SCRIPT_LOG"
exit 23
DOCKER
cat > "$fixture/cabal" <<'CABAL'
#!/usr/bin/env bash
set -euo pipefail
[ "${ECLTEST_SHARD:?}" = coverage-owner ]
printf 'cabal %s\n' "$*" >> "$ECLTEST_SCRIPT_LOG"
CABAL
chmod +x "$fixture/runner" "$fixture/docker" "$fixture/cabal"

bash scripts/test-integration.sh list | jq -e '.schemaVersion == 1' >/dev/null
bash scripts/test-integration.sh plan | jq -e '.schemaVersion == 1' >/dev/null
[ ! -e "$ECLTEST_SCRIPT_LOG" ]
if ECLTEST_INTEGRATION_GROUP=absent bash scripts/test-integration.sh; then
  echo 'unknown integration group passed' >&2
  exit 1
fi
[ ! -e "$ECLTEST_SCRIPT_LOG" ]
ECLTEST_INTEGRATION_GROUP=integration/egress bash scripts/test-integration.sh
rg -q '^runner --group integration/egress --report ' "$ECLTEST_SCRIPT_LOG"
[ "$(wc -l < "$ECLTEST_SCRIPT_LOG")" -eq 1 ]
if ECLTEST_INTEGRATION_GROUP=integration/egress bash scripts/test-integration.sh coverage; then
  echo 'partial integration coverage passed' >&2
  exit 1
fi
ECLTEST_SHARD=coverage-owner ECLTEST_CONTAINER_LOCK=held bash scripts/test-integration.sh coverage --enable-coverage --builddir=dist-coverage --test-show-details=direct
rg -q '^cabal test ecluse-integration --enable-coverage --builddir=dist-coverage --test-show-details=direct --test-option=--report ' "$ECLTEST_SCRIPT_LOG"
[ "$(wc -l < "$ECLTEST_SCRIPT_LOG")" -eq 2 ]
echo 'integration script checks passed'
