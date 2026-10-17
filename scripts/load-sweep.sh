#!/usr/bin/env bash
# Plan and run the jobs of a committed load sweep, bench/load/sweeps/<sweep>.json.
#   plan <sweep> [<selection>]  print jobs=<JSON list> for the workflow matrix. The selection names
#                               phases or jobs, comma-separated. Blank selects every phase not on request.
#   run <sweep> <job>           measure the job's arms in order, one directory each under
#                               sweep-results/<job>, then set them beside the job's anchors in
#                               comparison.md and comparison.json there.
# Needs jq, and for run, task and cabal (the dev shells).
set -euo pipefail

sweep_file() {
  case "$1" in
    '' | *[!a-z0-9-]*) echo "load-sweep.sh: a sweep name is lower-case letters, digits, and hyphens: $1" >&2; exit 2 ;;
  esac
  local file="bench/load/sweeps/$1.json"
  [ -f "$file" ] || { echo "load-sweep.sh: no sweep file $file" >&2; exit 2; }
  printf '%s\n' "$file"
}

plan() {
  local file
  file="$(sweep_file "$1")"
  jq -r --arg selection "${2:-}" '
    ($selection | split(",") | map(gsub("^\\s+|\\s+$"; "")) | map(select(. != ""))) as $names
    | [.phases[] | {phase, onRequest: (.onRequest // false), jobs: [.jobs[].job]}] as $phases
    | ($names - [$phases[].phase] - [$phases[].jobs[]]) as $unknown
    | if ($unknown | length) > 0 then error("the sweep has no phase or job named \($unknown | join(", "))") else . end
    | [$phases[] | . as $p | .jobs[]
        | select(if ($names | length) == 0 then ($p.onRequest | not) else (. as $job | $names | any(. == $p.phase or . == $job)) end)]
    | if length == 0 then error("the selection names no job") else . end
    | "jobs=\(tojson)"
  ' "$file"
}

# The environment of one arm: its shape, placement, and runtime settings, and what the sweep fixes.
# A dispatch's own scenario list narrows the arm's, for a short check of the sweep path.
arm_environment() {
  jq -r --arg arm "$2" --arg scenarios "${BENCH_LOAD_SCENARIOS:-}" '
    . as $sweep | .arms[$arm]
    | if . == null then error("the sweep has no arm \($arm)") else . end
    | "BENCH_LOAD_POD=\(.shape)",
      "BENCH_LOAD_PROXY_CPUS=\(.cpus // "")",
      "BENCH_LOAD_PROXY_CORES=\(.cores // "")",
      "BENCH_LOAD_PROXY_MAX_HEAP_BYTES=\(.maxHeapBytes // "")",
      "BENCH_LOAD_PROXY_ALLOC_AREA_MIB=\(.allocAreaMib // "")",
      "BENCH_LOAD_SERVE_MAX_IN_FLIGHT=\(.serveMaxInFlight // "")",
      "BENCH_LOAD_PROBE_RTT=0",
      "BENCH_LOAD_UPSTREAM_LATENCY_MS=\($sweep.upstreamLatencyMs)",
      "BENCH_LOAD_PASSES=\($sweep.passes)",
      "BENCH_LOAD_SCENARIOS=\(if $scenarios == "" then ($sweep.scenarios | join(",")) else $scenarios end)"
  ' "$1"
}

run() {
  local name="$1" job="$2" file
  file="$(sweep_file "$name")"
  # Command substitution, not a process substitution, so a failed read stops the run.
  local listed arms
  listed="$(jq -r --arg job "$job" '
    [.phases[].jobs[] | select(.job == $job)]
    | if length == 1 then .[0].arms[] else error("the sweep has no job \($job)") end
  ' "$file")"
  mapfile -t arms <<< "$listed"
  local out="sweep-results/$job" failed=0 slot=0 reports=()
  rm -rf "$out"
  mkdir -p "$out"
  for arm in "${arms[@]}"; do
    slot=$((slot + 1))
    local dir="$out/$slot-$arm" environment settings outcome=success
    mkdir -p "$dir"
    environment="$(arm_environment "$file" "$arm")"
    mapfile -t settings <<< "$environment"
    echo "load-sweep.sh: job $job, slot $slot: $arm"
    # Each arm's report stays in its directory. Only the comparison goes to the run summary.
    env -u GITHUB_STEP_SUMMARY "${settings[@]}" task performance:load-measure PERF_ARTIFACT_DIR="$dir/receipt" || outcome=failure
    env -u GITHUB_STEP_SUMMARY "${settings[@]}" PERF_NATIVE_OUTCOME="$outcome" task performance:load-receipt PERF_ARTIFACT_DIR="$dir/receipt" || failed=1
    [ "$outcome" = success ] || failed=1
    for produced in bench-load-results.md load-observations.json; do
      if [ -e "$produced" ]; then mv "$produced" "$dir/"; fi
    done
    reports+=("$dir/bench-load-results.md")
  done
  "$(cabal list-bin exe:bench-report)" sweep "$name" "$job" "$out" "${reports[@]}" || failed=1
  return "$failed"
}

case "${1:-}" in
  plan) [ "$#" -ge 2 ] && [ "$#" -le 3 ] || { echo 'usage: load-sweep.sh plan SWEEP [SELECTION]' >&2; exit 2; }; plan "$2" "${3:-}" ;;
  run) [ "$#" -eq 3 ] || { echo 'usage: load-sweep.sh run SWEEP JOB' >&2; exit 2; }; run "$2" "$3" ;;
  *) echo 'usage: load-sweep.sh plan SWEEP [SELECTION] | run SWEEP JOB' >&2; exit 2 ;;
esac
