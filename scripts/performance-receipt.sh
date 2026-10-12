#!/usr/bin/env bash
# Bind native measurement output to independent, per-ecosystem obligations.
set -euo pipefail

kind="${1:-}"
[ "$#" -eq 1 ] || { echo 'performance receipt: expected one fixed kind' >&2; exit 2; }
case "$kind" in
  allocation)
    native=perf-allocation-observations.json
    artifacts=("$native" perf-allocation-report.md acceptance/criteria.json bench/corpus/pins.json)
    ;;
  live-allocation)
    native=perf-acceptance-observations.json
    artifacts=("$native" perf-acceptance-report.md acceptance/criteria.json bench/corpus/pins.json)
    ;;
  benchmark)
    native=bench-results.csv
    artifacts=("$native" bench-output.txt bench/corpus/pins.json)
    ;;
  load-foundation)
    native=load-observations.json
    artifacts=("$native" bench-load-results.md bench/load/floors.json bench/corpus/pins.json)
    ;;
  *) echo "performance receipt: unknown fixed kind: $kind" >&2; exit 2 ;;
esac

case "${PERF_NATIVE_OUTCOME:-}" in
  success) outcome=passed ;;
  failure) outcome=failed ;;
  cancelled|skipped) outcome=unavailable ;;
  *) echo 'performance receipt: set PERF_NATIVE_OUTCOME from the native step outcome' >&2; exit 2 ;;
esac

source_revision="${GITHUB_SHA:-$(git rev-parse HEAD)}"
attempt="${GITHUB_RUN_ATTEMPT:-1}"
run_id="${GITHUB_RUN_ID:-$(bash scripts/test-containers.sh run-id)}-$attempt"
case "$(uname -m)" in
  aarch64|arm64) architecture=arm64 ;;
  x86_64|amd64) architecture=amd64 ;;
  *) echo 'performance receipt: unsupported architecture' >&2; exit 2 ;;
esac
export ECLTEST_SOURCE="$source_revision" ECLTEST_ARCHITECTURE="$architecture"
export ECLTEST_RUN_ID="$run_id" ECLTEST_ATTEMPT="$attempt"
export ECLTEST_REQUIRED=1
root="${PERF_ARTIFACT_DIR:-scratchpad/performance/$kind/$run_id}"
mkdir -p "$root"
rm -f "$root/diagnostic.json"
for subject in ecosystem-npm ecosystem-pypi; do
  rm -f "$root/$subject/obligation.json" "$root/$subject/request.json" "$root/$subject/receipt.json"
done

diagnostic() {
  local reason="$1" report="$2" report_digest='' native_digest=''
  if [ "$outcome" = passed ] && [ ! -s "$report" ]; then
    echo "performance receipt: missing diagnostic report: $report" >&2
    return 1
  fi
  [ ! -s "$report" ] || report_digest="$(sha256sum "$report" | cut -d' ' -f1)"
  [ ! -s "$native" ] || native_digest="$(sha256sum "$native" | cut -d' ' -f1)"
  jq -n --arg reason "$reason" --arg source "$source_revision" --arg run "$run_id" \
    --arg attempt "$attempt" --arg architecture "$architecture" --arg pod "${BENCH_LOAD_POD:-}" \
    --arg selected "${BENCH_LOAD_SCENARIOS:-}" --arg outcome "$outcome" \
    --arg enforcement "${floor_status:-}" --arg enforcementReasons "${floor_reasons:-}" \
    --arg report "$report_digest" --arg nativeDigest "$native_digest" \
    '{scope:"diagnostic-only",reason:$reason,source:$source,runId:$run,attempt:$attempt,architecture:$architecture,podShape:$pod,selectedScenarios:$selected,nativeOutcome:$outcome,floorEnforcement:$enforcement,floorEnforcementReasons:$enforcementReasons,reportDigest:$report,nativeDigest:$nativeDigest}' \
    > "$root/diagnostic.json"
  echo "performance receipt: $reason is diagnostic only; no full receipt" >&2
}

selected_scenarios="${BENCH_LOAD_SCENARIOS:-}"
selected_nonspace="${selected_scenarios//[[:space:]]/}"
floor_status=''
floor_reasons=''
floor_reasons_present=false
if [ "$kind" = load-foundation ] && [ -s "$native" ]; then
  jq -e 'type == "object"' "$native" >/dev/null
  floor_status="$(jq -r '.conditions.floorEnforcement // "missing"' "$native")"
  floor_reasons="$(jq -r '.conditions.floorEnforcementReasons // ""' "$native")"
  floor_reasons_present="$(jq -r '.conditions | if type == "object" then has("floorEnforcementReasons") else false end' "$native")"
fi
if [ "$kind" = load-foundation ] && { [ -n "$selected_nonspace" ] || [ -n "${BENCH_LOAD_THRASH_LIMITS_MIB:-}" ]; }; then
  reason='selected scenarios'
  [ -z "${BENCH_LOAD_THRASH_LIMITS_MIB:-}" ] || reason='GC-thrash probe'
  diagnostic "$reason" bench-load-results.md
  exit 0
fi
if [ "$kind" = load-foundation ] && [ -s "$native" ]; then
  case "$floor_status" in
    held)
      if [ "$floor_reasons_present" != true ]; then
        diagnostic 'load floor enforcement reasons missing' bench-load-results.md
        exit 0
      fi
      if [ -n "$floor_reasons" ]; then
        diagnostic 'contradictory load floor enforcement' bench-load-results.md
        exit 0
      fi
      ;;
    not-held)
      diagnostic 'load floors not held' bench-load-results.md
      exit 0
      ;;
    *)
      diagnostic 'load floor enforcement missing' bench-load-results.md
      exit 0
      ;;
  esac
fi
if [ "$kind" = live-allocation ] && [ -s "$native" ] && jq -e '.outcome == "unavailable"' "$native" >/dev/null; then
  diagnostic 'live upstream unavailable' perf-acceptance-report.md
  exit 0
fi

compiler="ghc-$(ghc --numeric-version)"

case "$kind" in
  allocation|live-allocation)
    mode=captures
    [ "$kind" = live-allocation ] && mode=live
    jq -n --arg compiler "$compiler" --arg mode "$mode" \
      '{compiler:$compiler,optimisation:"-O1",profiling:"off",mode:$mode,passes:"5"}' > "$root/conditions.json"
    ;;
  benchmark)
    jq -n --arg compiler "$compiler" \
      '{compiler:$compiler,optimisation:"-O1",profiling:"off",rts:"-T",stdev:"10"}' > "$root/conditions.json"
    ;;
  load-foundation)
    pod="${BENCH_LOAD_POD:-}"
    duration="${BENCH_LOAD_DURATION_SECONDS:-}"
    concurrency="${BENCH_LOAD_CONCURRENCY:-}"
    admission="${BENCH_LOAD_SERVE_MAX_IN_FLIGHT:-automatic}"
    affinity=unrestricted
    [ "${BENCH_LOAD_ISOLATE_OHA:-}" != 1 ] || affinity='taskset -c 0 oha'
    jq -n --arg compiler "$compiler" --arg pod "$pod" --arg duration "$duration" \
      --arg concurrency "$concurrency" --arg admission "$admission" --arg affinity "$affinity" \
      '{compiler:$compiler,optimisation:"-O1",profiling:"off",podShape:$pod,durationSeconds:$duration,concurrency:$concurrency,serveMaxInFlight:$admission,affinity:$affinity}' > "$root/conditions.json"
    ;;
esac

cabal build exe:bench-report >&2
reporter="$(cabal list-bin exe:bench-report)"
overall=0
for ecosystem in npm pypi; do
  subject="ecosystem-$ecosystem"
  directory="$root/$subject"
  mkdir -p "$directory"
  if ! jq -e --arg ecosystem "$ecosystem" '.captures[$ecosystem] | type == "object" and length > 0' bench/corpus/pins.json >/dev/null; then
    echo "performance receipt: no pinned captures for $ecosystem" >&2
    overall=1
    continue
  fi
  mapfile -t captures < <(jq -r --arg ecosystem "$ecosystem" '.captures[$ecosystem][] .path' bench/corpus/pins.json)
  subject_artifacts=("${artifacts[@]}")
  for capture in "${captures[@]}"; do
    subject_artifacts+=("bench/corpus/$capture")
  done
  artifacts_json="$(printf '%s\n' "${subject_artifacts[@]}" | jq -R . | jq -s .)"
  recorded="$root/conditions.json"
  if [ "$kind" = benchmark ]; then
    jq '. + {consoleLog:"bench-output.txt"}' "$root/conditions.json" > "$directory/recorded-conditions.json"
    recorded="$directory/recorded-conditions.json"
  fi
  if ! "$reporter" performance-obligation "$kind" "$subject" "$root/conditions.json" > "$directory/obligation.json"; then
    overall=1
    continue
  fi
  plan_digest="$(sha256sum "$directory/obligation.json" | cut -d' ' -f1)"
  jq -n --slurpfile obligation "$directory/obligation.json" --slurpfile conditions "$recorded" \
    --arg source "$source_revision" --arg architecture "$architecture" --arg run "$run_id" \
    --arg digest "$plan_digest" --arg attempt "$attempt" --arg outcome "$outcome" \
    --argjson artifacts "$artifacts_json" \
    '{requestIdentity:{source:$source,image:"not-applicable",architecture:$architecture,planDigest:$digest,runId:$run},requestAttempt:$attempt,requestObligation:$obligation[0],requestArtifacts:$artifacts,requestOutcome:$outcome,requestRecordedConditions:$conditions[0]}' \
    > "$directory/request.json"
  if ! "$reporter" performance-receipt "$directory/request.json" "$native" "$directory/receipt.json"; then
    overall=1
  fi
  if [ -s "$directory/receipt.json" ]; then
    "$reporter" performance-audit "$directory/request.json" "$directory/receipt.json" || overall=1
  else
    echo "performance receipt: missing $subject receipt" >&2
    overall=1
  fi
done
exit "$overall"
