#!/usr/bin/env bash
# Check request identity, subject isolation, native outcomes and stale receipt removal.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/scripts" "$work/bench/corpus/npm" "$work/bench/corpus/pypi" "$work/acceptance"
cp "$here/performance-receipt.sh" "$work/scripts/"
cat > "$work/scripts/test-containers.sh" <<'STUB'
#!/usr/bin/env bash
printf 'local-run\n'
STUB
cat > "$work/bin/ghc" <<'STUB'
#!/usr/bin/env bash
printf '9.10.3\n'
STUB
cat > "$work/bin/cabal" <<'STUB'
#!/usr/bin/env bash
case "$1" in
  build) exit 0 ;;
  list-bin) printf '%s/bin/reporter\n' "$TEST_WORK" ;;
  *) exit 2 ;;
esac
STUB
cat > "$work/bin/reporter" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
case "$1" in
  performance-obligation)
    jq -n --arg kind "$2" --arg subject "$3" --slurpfile conditions "$4" \
      '{poKind:$kind,poSubject:$subject,poRows:[$subject],poConditions:$conditions[0]}'
    ;;
  performance-receipt)
    [ -s "$3" ]
    [ "$ECLTEST_SOURCE" = test-source ]
    [ "$ECLTEST_RUN_ID" = test-run-2 ]
    [ "$ECLTEST_ATTEMPT" = 2 ]
    jq -e --arg native "$3" '.requestIdentity.source == "test-source" and .requestIdentity.image == "not-applicable" and .requestIdentity.runId == "test-run-2" and .requestAttempt == "2" and (.requestArtifacts | index($native) != null)' "$2" >/dev/null
    jq -n --slurpfile request "$2" '{subject:$request[0].requestObligation.poSubject}' > "$4"
    [ "$(jq -r .requestOutcome "$2")" = passed ]
    ;;
  performance-audit)
    [ -s "$3" ]
    ;;
  *) exit 2 ;;
esac
STUB
chmod +x "$work/bin/"* "$work/scripts/"*
cat > "$work/bench/corpus/pins.json" <<'JSON'
{"captures":{"npm":{"sample":{"path":"npm/sample.json"}},"pypi":{"sample":{"path":"pypi/sample.json"}}}}
JSON
printf '{}\n' > "$work/bench/corpus/npm/sample.json"
printf '{}\n' > "$work/bench/corpus/pypi/sample.json"
printf '{}\n' > "$work/acceptance/criteria.json"
printf 'native report\n' > "$work/perf-allocation-report.md"
printf '{"outcome":"passed","observations":[],"conditions":{}}\n' > "$work/perf-allocation-observations.json"

export TEST_WORK="$work" PATH="$work/bin:$PATH"
export GITHUB_SHA=test-source GITHUB_RUN_ID=test-run GITHUB_RUN_ATTEMPT=2
export PERF_ARTIFACT_DIR="$work/receipts" PERF_NATIVE_OUTCOME=success
cd "$work"
bash scripts/performance-receipt.sh allocation
for subject in ecosystem-npm ecosystem-pypi; do
  [ -s "$PERF_ARTIFACT_DIR/$subject/obligation.json" ]
  [ -s "$PERF_ARTIFACT_DIR/$subject/request.json" ]
  [ -s "$PERF_ARTIFACT_DIR/$subject/receipt.json" ]
  jq -e --arg subject "$subject" '.requestObligation.poSubject == $subject and .requestRecordedConditions.mode == "captures" and .requestRecordedConditions.passes == "5"' "$PERF_ARTIFACT_DIR/$subject/request.json" >/dev/null
  digest="$(sha256sum "$PERF_ARTIFACT_DIR/$subject/obligation.json" | cut -d' ' -f1)"
  jq -e --arg digest "$digest" '.requestIdentity.planDigest == $digest' "$PERF_ARTIFACT_DIR/$subject/request.json" >/dev/null
done

printf 'benchmark,csv\n' > "$work/bench-results.csv"
printf 'All: OK\n' > "$work/bench-output.txt"
PERF_ARTIFACT_DIR="$work/benchmark-receipts" bash scripts/performance-receipt.sh benchmark
jq -e '.requestRecordedConditions.rts == "-T" and .requestRecordedConditions.stdev == "10" and .requestRecordedConditions.consoleLog == "bench-output.txt"' "$work/benchmark-receipts/ecosystem-npm/request.json" >/dev/null

printf 'live report\n' > "$work/perf-acceptance-report.md"
printf '{"outcome":"passed","observations":[],"conditions":{}}\n' > "$work/perf-acceptance-observations.json"
PERF_ARTIFACT_DIR="$work/live-receipts" bash scripts/performance-receipt.sh live-allocation
jq -e '.requestRecordedConditions.mode == "live" and .requestObligation.poKind == "live-allocation"' "$work/live-receipts/ecosystem-pypi/request.json" >/dev/null
printf '{"outcome":"unavailable","observations":[],"conditions":{}}\n' > "$work/perf-acceptance-observations.json"
PERF_ARTIFACT_DIR="$work/live-receipts" bash scripts/performance-receipt.sh live-allocation
jq -e '.scope == "diagnostic-only" and .reason == "live upstream unavailable"' "$work/live-receipts/diagnostic.json" >/dev/null
[ ! -e "$work/live-receipts/ecosystem-npm/receipt.json" ]
[ ! -e "$work/live-receipts/ecosystem-pypi/receipt.json" ]

mkdir -p "$work/bench/load"
printf '{}\n' > "$work/bench/load/floors.json"
printf 'native load report\n' > "$work/bench-load-results.md"
printf '{"outcome":"passed","observations":[],"conditions":{"floorEnforcement":"held","floorEnforcementReasons":""}}\n' > "$work/load-observations.json"
BENCH_LOAD_POD=2cpu-1gib BENCH_LOAD_DURATION_SECONDS=30 BENCH_LOAD_CONCURRENCY=100 \
  BENCH_LOAD_SERVE_MAX_IN_FLIGHT='' BENCH_LOAD_ISOLATE_OHA=1 PERF_ARTIFACT_DIR="$work/load-receipts" \
  bash scripts/performance-receipt.sh load-foundation
jq -e '.requestRecordedConditions.podShape == "2cpu-1gib" and .requestRecordedConditions.serveMaxInFlight == "automatic" and .requestRecordedConditions.affinity == "taskset -c 0 oha"' "$work/load-receipts/ecosystem-npm/request.json" >/dev/null
printf '{"outcome":"passed","observations":[],"conditions":{}}\n' > "$work/load-observations.json"
BENCH_LOAD_POD=2cpu-1gib PERF_ARTIFACT_DIR="$work/load-receipts" \
  bash scripts/performance-receipt.sh load-foundation
jq -e '.scope == "diagnostic-only" and .reason == "load floor enforcement missing"' "$work/load-receipts/diagnostic.json" >/dev/null
[ ! -e "$work/load-receipts/ecosystem-npm/receipt.json" ]
printf '{"outcome":"passed","observations":[],"conditions":{"floorEnforcement":"not-held","floorEnforcementReasons":"ChangedDuration, WrongRunner"}}\n' > "$work/load-observations.json"
BENCH_LOAD_POD=2cpu-1gib PERF_ARTIFACT_DIR="$work/load-receipts" \
  bash scripts/performance-receipt.sh load-foundation
jq -e '.scope == "diagnostic-only" and .reason == "load floors not held" and .floorEnforcementReasons == "ChangedDuration, WrongRunner" and .nativeOutcome == "passed"' "$work/load-receipts/diagnostic.json" >/dev/null
[ ! -e "$work/load-receipts/ecosystem-pypi/receipt.json" ]
printf '{"outcome":"passed","observations":[],"conditions":{"floorEnforcement":"held"}}\n' > "$work/load-observations.json"
BENCH_LOAD_POD=2cpu-1gib PERF_ARTIFACT_DIR="$work/load-receipts" \
  bash scripts/performance-receipt.sh load-foundation
jq -e '.scope == "diagnostic-only" and .reason == "load floor enforcement reasons missing"' "$work/load-receipts/diagnostic.json" >/dev/null
printf '{"outcome":"passed","observations":[],"conditions":{"floorEnforcement":"held","floorEnforcementReasons":"WrongRunner"}}\n' > "$work/load-observations.json"
BENCH_LOAD_POD=2cpu-1gib PERF_ARTIFACT_DIR="$work/load-receipts" \
  bash scripts/performance-receipt.sh load-foundation
jq -e '.scope == "diagnostic-only" and .reason == "contradictory load floor enforcement"' "$work/load-receipts/diagnostic.json" >/dev/null
printf '{"outcome":"passed","observations":[],"conditions":{"floorEnforcement":"held","floorEnforcementReasons":""}}\n' > "$work/load-observations.json"
BENCH_LOAD_POD=2cpu-1gib BENCH_LOAD_SCENARIOS='npm/merge-cold' \
  PERF_ARTIFACT_DIR="$work/load-receipts" bash scripts/performance-receipt.sh load-foundation
jq -e '.scope == "diagnostic-only" and .reason == "selected scenarios" and .selectedScenarios == "npm/merge-cold"' "$work/load-receipts/diagnostic.json" >/dev/null
[ ! -e "$work/load-receipts/ecosystem-npm/receipt.json" ]
[ ! -e "$work/load-receipts/ecosystem-pypi/receipt.json" ]
rm load-observations.json
BENCH_LOAD_POD=unlimited BENCH_LOAD_THRASH_LIMITS_MIB='2048,1536' \
  PERF_ARTIFACT_DIR="$work/thrash-receipts" bash scripts/performance-receipt.sh load-foundation
jq -e '.scope == "diagnostic-only" and .reason == "GC-thrash probe" and .nativeDigest == ""' "$work/thrash-receipts/diagnostic.json" >/dev/null
[ ! -e "$work/thrash-receipts/ecosystem-npm/receipt.json" ]

if PERF_NATIVE_OUTCOME=failure bash scripts/performance-receipt.sh allocation > "$work/failure.log" 2>&1; then
  echo 'unexpected success for failed native process' >&2
  exit 1
fi
[ -s "$PERF_ARTIFACT_DIR/ecosystem-npm/receipt.json" ]
[ -s "$PERF_ARTIFACT_DIR/ecosystem-pypi/receipt.json" ]
rm perf-allocation-observations.json
if bash scripts/performance-receipt.sh allocation > "$work/missing.log" 2>&1; then
  echo 'unexpected success without native observations' >&2
  exit 1
fi
[ ! -e "$PERF_ARTIFACT_DIR/ecosystem-npm/receipt.json" ]
[ ! -e "$PERF_ARTIFACT_DIR/ecosystem-pypi/receipt.json" ]
printf 'ok - performance requests bind both subjects and reject stale or failed native evidence\n'
