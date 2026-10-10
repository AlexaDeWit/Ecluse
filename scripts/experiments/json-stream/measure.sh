#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
#
# SPDX-License-Identifier: MIT
set -euo pipefail

# shellcheck source-path=SCRIPTDIR
# shellcheck source=common.sh
source "$(dirname -- "${BASH_SOURCE[0]}")/common.sh"
unset GHCRTS
readonly fold_chunks=(1024 32768)
readonly full_chunk=65536
readonly calibration_passes=3
readonly maximum_passes=200000
readonly nanoseconds_per_second=1000000000
readonly rts_options=(+RTS -T -A4m -s -RTS)
check_inputs
readonly repeats=${BENCH_REPEATS:-5}
readonly seconds=${BENCH_SECONDS:-0.3}
[[ "$repeats" =~ ^[1-9][0-9]*$ ]] || fail 'BENCH_REPEATS must be positive'
[[ "$seconds" =~ ^[0-9]+([.][0-9]+)?$ ]] || fail 'BENCH_SECONDS must be numeric'
awk -v seconds="$seconds" 'BEGIN {exit !(seconds > 0)}' || fail 'BENCH_SECONDS must be positive'
mkdir -- "$artifact_dir/samples"
: > "$artifact_dir/workloads.tsv"
while IFS= read -r fixture; do
  printf 'full\t%s\t%s\naeson\t%s\t%s\n' "$fixture" "$full_chunk" "$fixture" "$full_chunk" >> "$artifact_dir/workloads.tsv"
  for chunk in "${fold_chunks[@]}"; do printf 'fold\t%s\t%s\n' "$fixture" "$chunk" >> "$artifact_dir/workloads.tsv"; done
done < <(jq -r '.[].path' "$artifact_dir/fixtures.json")

run_sample() {
  local variant=$1 kind=$2 fixture=$3 chunk=$4 count=$5 phase=$6 repetition=$7 row=$8
  local name binary stem status elapsed allocated live checksum good passes size extra
  local started finished loadavg
  local -a command
  case "$kind" in
    fold) name=token-fold ;;
    full) name=aeson-benchmark-jstream-parse ;;
    selected) name=aeson-benchmark-fastobj ;;
    aeson) name=aeson-benchmark-aeson-parse ;;
    *) fail "Unknown workload: $kind" ;;
  esac
  binary=$(binary_path "$variant" "$name")
  stem="$artifact_dir/samples/$phase.$repetition.$row.$variant"
  if [[ "$kind" == fold ]]; then
    command=("$binary" measure "$count" "$chunk" "$work_dir/original/benchmarks/json-data/$fixture" "${rts_options[@]}")
  else
    command=("$binary" "$chunk" "$count" "$work_dir/original/benchmarks/json-data/$fixture" "${rts_options[@]}")
  fi
  jq -nc --args '$ARGS.positional' -- "${command[@]}" > "$stem.command.json"
  started=$(date +%s%N)
  status=0
  "${command[@]}" > "$stem.stdout" 2> "$stem.stderr" || status=$?
  finished=$(date +%s%N)
  if [[ "$status" != 0 ]]; then
    jq -nc --arg variant "$variant" --arg kind "$kind" --arg fixture "$fixture" --argjson status "$status" \
      --rawfile stdout "$stem.stdout" --rawfile stderr "$stem.stderr" \
      '{variant:$variant,kind:$kind,fixture:$fixture,exit_code:$status,stdout:$stdout,stderr:$stderr}' >> "$artifact_dir/failures.jsonl"
    fail "Benchmark process failed: $variant/$kind/$fixture"
  fi
  checksum=null
  if [[ "$kind" == fold ]]; then
    IFS=, read -r passes size elapsed allocated live checksum extra < "$stem.stdout"
    [[ "$passes" == "$count" && "$size" == "$chunk" && -z "$extra" ]] || fail "Unexpected fold output: $stem.stdout"
    good=$count
  else
    read -r good elapsed < <(awk -v scale="$nanoseconds_per_second" '/ good, / {gsub("s", "", $3); printf "%s %.0f\n", $1, $3 * scale}' "$stem.stdout")
    allocated=$(awk '/bytes allocated in the heap/ {gsub(",", "", $1); print $1}' "$stem.stderr")
    live=$(awk '/bytes maximum residency/ {gsub(",", "", $1); print $1}' "$stem.stderr")
  fi
  for number in "$elapsed" "$allocated" "$live"; do [[ "$number" =~ ^[0-9]+$ ]] || fail "Malformed metric in $stem"; done
  loadavg=$(cat /proc/loadavg)
  jq -nc --arg variant "$variant" --arg kind "$kind" --arg fixture "$fixture" --arg phase "$phase" \
    --arg loadavg "$loadavg" --arg checksum "$checksum" --argjson chunk "$chunk" --argjson count "$count" \
    --argjson repetition "$repetition" --argjson good "$good" --argjson elapsed "$elapsed" \
    --argjson allocated "$allocated" --argjson live "$live" --argjson wall "$((finished - started))" \
    --slurpfile command "$stem.command.json" --rawfile stdout "$stem.stdout" --rawfile stderr "$stem.stderr" \
    '{variant:$variant,kind:$kind,fixture:$fixture,phase:$phase,repetition:$repetition,chunk:$chunk,count:$count,good:$good,elapsed_ns:$elapsed,ns_per_pass:($elapsed/$count),allocated_bytes:$allocated,allocated_per_pass:($allocated/$count),max_live_bytes:$live,checksum:$checksum,process_wall_ns:$wall,loadavg:$loadavg,command:$command[0],stdout:$stdout,stderr:$stderr}' \
    > "$stem.json"
  cat -- "$stem.json" >> "$artifact_dir/$phase.jsonl"
  [[ "$good" == "$count" ]] || fail "Failed parses: $variant/$kind/$fixture, $good of $count succeeded"
}

jq -nc --argjson repeats "$repeats" --argjson seconds "$seconds" --arg loadavg "$(cat /proc/loadavg)" \
  --arg started "$(date --utc --iso-8601=seconds)" '{repeats:$repeats,target_seconds:$seconds,loadavg:$loadavg,started_utc:$started}' \
  > "$artifact_dir/run.json"
awk '/Cpus_allowed_list/ {print}' /proc/self/status > "$artifact_dir/cpu-affinity.txt"
: > "$artifact_dir/calibration.jsonl"
: > "$artifact_dir/calibration.tsv"
: > "$artifact_dir/raw.jsonl"
row=0
while IFS=$'\t' read -r kind fixture chunk; do
  run_sample baseline "$kind" "$fixture" "$chunk" "$calibration_passes" calibration 0 "$row"
  count=$(jq -r --argjson seconds "$seconds" --argjson scale "$nanoseconds_per_second" --argjson maximum "$maximum_passes" \
    '($seconds*$scale/.ns_per_pass|round) | if . < 1 then 1 elif . > $maximum then $maximum else . end' \
    "$artifact_dir/samples/calibration.0.$row.baseline.json")
  printf '%s\t%s\t%s\t%s\n' "$kind" "$fixture" "$chunk" "$count" >> "$artifact_dir/calibration.tsv"
  row=$((row + 1))
done < "$artifact_dir/workloads.tsv"
for ((repetition=0; repetition<repeats; repetition++)); do
  row=0
  while IFS=$'\t' read -r kind fixture chunk count; do
    for ((position=0; position<${#variants[@]}; position++)); do
      variant=${variants[$(((position + repetition) % ${#variants[@]}))]}
      run_sample "$variant" "$kind" "$fixture" "$chunk" "$count" raw "$repetition" "$row"
    done
    row=$((row + 1))
  done < "$artifact_dir/calibration.tsv"
  printf 'Completed repetition %s/%s\n' "$((repetition + 1))" "$repeats"
done
check_inputs
