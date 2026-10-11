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
  --audit-full)
    printf 'audit %s\n' "$*" >> "$ECLTEST_SCRIPT_LOG"
    printf 'audit instrumentation\n' > "${HPCTIXFILE:?audit must isolate instrumentation}"
    jq -e '.groups == ["all"]' "$3" >/dev/null
    ;;
  --catalogue-select)
    shift
    if [ "${1:-}" = --group ] && [ "${2:-}" = absent ]; then exit 19; fi
    if [ "${TEST_NEEDS_DOCKER:-}" = 1 ]; then
      printf '[{"groupPrerequisites":["docker"]}]\n'
    else
      printf '[{"groupPrerequisites":[]}]\n'
    fi
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
printf 'cabal %s\n' "$*" >> "$ECLTEST_SCRIPT_LOG"
[ "${ECLTEST_SHARD:?}" = coverage-owner ]
[ "$1" = test ]
report=""
previous=""
for argument in "$@"; do
  if [ "$previous" = --test-option=--report ]; then report="${argument#--test-option=}"; fi
  previous="$argument"
done
[ -n "$report" ]
if [ "${TEST_MISSING_REPORT:-}" != 1 ]; then
  if [ "${TEST_PARTIAL_REPORT:-}" = 1 ]; then
    echo '{"groups":["subset"]}' > "$report"
  else
    echo '{"groups":["all"]}' > "$report"
  fi
fi
printf 'coverage ticks\n' > "$ECLTEST_INTEGRATION_ARTIFACT_DIR/suite.tix"
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
ECLTEST_SHARD=coverage-owner ECLTEST_CONTAINER_LOCK=held TEST_NEEDS_DOCKER=1 bash scripts/test-integration.sh coverage --enable-coverage --builddir=dist-coverage --test-show-details=direct
rg -q '^cabal test ecluse-integration --enable-coverage --builddir=dist-coverage --test-show-details=direct --test-option=--report ' "$ECLTEST_SCRIPT_LOG"
[ "$(wc -l < "$ECLTEST_SCRIPT_LOG")" -eq 3 ]
rg -q '^audit --audit-full ' "$ECLTEST_SCRIPT_LOG"
[ "$(cat "$ECLTEST_INTEGRATION_ARTIFACT_DIR/suite.tix")" = 'coverage ticks' ]
for option in '--test-option=--group' '--test-options=--group integration/egress' '--test-option' \
  '--test-option=--catalogue-list' '--test-option=--report' '--test-wrapper=true' 'ecluse-unit' '--builddir=other' '--ghc-options=-fno-hpc' '--disable-coverage'; do
  before="$(wc -l < "$ECLTEST_SCRIPT_LOG")"
  if bash scripts/test-integration.sh coverage "$option"; then
    echo "coverage accepted $option" >&2
    exit 1
  fi
  if bash scripts/coverage.sh ecluse-integration "$option"; then
    echo "coverage built before rejecting $option" >&2
    exit 1
  fi
  [ "$(wc -l < "$ECLTEST_SCRIPT_LOG")" -eq "$before" ]
done
if ECLTEST_SHARD=coverage-owner TEST_PARTIAL_REPORT=1 bash scripts/test-integration.sh coverage --enable-coverage --builddir=dist-coverage; then
  echo 'partial execution report passed full coverage audit' >&2
  exit 1
fi
ECLTEST_SHARD=coverage-owner bash scripts/test-integration.sh coverage --enable-coverage --builddir=dist-coverage
if ECLTEST_SHARD=coverage-owner TEST_MISSING_REPORT=1 bash scripts/test-integration.sh coverage --enable-coverage --builddir=dist-coverage; then
  echo 'missing execution report reused a previous passing report' >&2
  exit 1
fi
[ "$(cat "$ECLTEST_INTEGRATION_ARTIFACT_DIR/suite.tix")" = 'coverage ticks' ]
echo 'integration script checks passed'
