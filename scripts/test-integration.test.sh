#!/usr/bin/env bash
# Exercise fixed integration execution, identities, and coverage audits without services.
set -euo pipefail
root="$(git rev-parse --show-toplevel)"
cd "$root"
mkdir -p scratchpad
fixture="$(mktemp -d scratchpad/integration-script.XXXXXXXXXX)"
trap 'rm -rf "$fixture"' EXIT
fixture="$(realpath "$fixture")"
export ECLTEST_INTEGRATION_ARTIFACT_DIR="$fixture/artifacts"
export ECLTEST_SCRIPT_LOG="$fixture/log"
export ECLTEST_SOURCE=test-source
export ECLTEST_ARCHITECTURE=test-architecture
export ECLTEST_RUN_ID=test-run
export PATH="$fixture:$PATH"
cat > "$fixture/runner" <<'RUNNER'
#!/usr/bin/env bash
set -euo pipefail
case "$(basename "$0")" in
  ecluse-integration-manifest)
    printf '{"schemaVersion":1}\n'
    printf 'instrumentation\n' > "${HPCTIXFILE:?metadata must isolate instrumentation}"
    ;;
  ecluse-integration-audit)
    printf 'audit %s\n' "$*" >> "$ECLTEST_SCRIPT_LOG"
    printf 'audit instrumentation\n' > "${HPCTIXFILE:?audit must isolate instrumentation}"
    jq -e '.groups == ["all"]' "$2" >/dev/null
    ;;
  *) exit 2 ;;
esac
RUNNER
cp "$fixture/runner" "$fixture/ecluse-integration-manifest"
cp "$fixture/runner" "$fixture/ecluse-integration-audit"
cat > "$fixture/docker" <<'DOCKER'
#!/usr/bin/env bash
printf 'unexpected Docker acquisition\n' >> "$ECLTEST_SCRIPT_LOG"
exit 23
DOCKER
cat > "$fixture/cabal" <<'CABAL'
#!/usr/bin/env bash
set -euo pipefail
case "$1" in
  build) exit 0 ;;
  list-bin) printf '%s/%s\n' "$(dirname "$0")" "$2"; exit ;;
  test) ;;
  *) exit 2 ;;
esac
printf 'cabal %s\n' "$*" >> "$ECLTEST_SCRIPT_LOG"
[ "${ECLTEST_SOURCE:?}" = test-source ]
[ "${ECLTEST_ARCHITECTURE:?}" = test-architecture ]
[ "${ECLTEST_RUN_ID:?}" = test-run ]
[ "${#ECLTEST_PLAN_DIGEST}" -eq 64 ]
report="${ECLTEST_REPORT_PATH:?}"
if [ "${TEST_MISSING_REPORT:-}" != 1 ]; then
  if [ "${TEST_PARTIAL_REPORT:-}" = 1 ]; then
    echo '{"groups":["subset"]}' > "$report"
  else
    echo '{"groups":["all"]}' > "$report"
  fi
fi
printf 'coverage ticks\n' > "$ECLTEST_INTEGRATION_ARTIFACT_DIR/suite.tix"
CABAL
chmod +x "$fixture/runner" "$fixture/docker" "$fixture/cabal" "$fixture/ecluse-integration-manifest" "$fixture/ecluse-integration-audit"

if bash scripts/test-integration.sh run --group absent; then
  echo 'integration accepted a selector argument' >&2
  exit 1
fi
[ ! -e "$ECLTEST_SCRIPT_LOG" ]
export ECLTEST_CONTAINER_LOCK=held
bash scripts/test-integration.sh
rg -q '^cabal test ecluse-integration --test-show-details=direct$' "$ECLTEST_SCRIPT_LOG"
bash scripts/test-integration.sh coverage --enable-coverage --builddir=dist-coverage --test-show-details=direct
rg -q '^cabal test ecluse-integration --enable-coverage --builddir=dist-coverage --test-show-details=direct$' "$ECLTEST_SCRIPT_LOG"
[ "$(wc -l < "$ECLTEST_SCRIPT_LOG")" -eq 4 ]
rg -q '^audit ' "$ECLTEST_SCRIPT_LOG"
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
