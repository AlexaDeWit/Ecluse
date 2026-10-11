#!/usr/bin/env bash
# Build first, then retain every sample from two balanced rotations on one runner.
set -euo pipefail

readonly main_revision=b795016660d171db729bc773a9a8c495e38d3294
readonly native_revision=854da92680eb61d5e2c4ac14fdd11549144c7cc6
readonly implementations=(main native)
readonly revisions=("$main_revision" "$native_revision")
readonly rotations=('main native' 'native main')
readonly expected_scenarios=npm/merge-cold,npm/heavy-private,pypi/index-cold,pypi/heavy-private
readonly paired_root="${PAIRED_LOAD_ROOT:-${RUNNER_TEMP:?RUNNER_TEMP or PAIRED_LOAD_ROOT is required}/paired-json-load}"
readonly sources="$paired_root/sources"
readonly binaries="$paired_root/binaries"
readonly results="$paired_root/results"

build_all() {
  mkdir -p "$sources" "$binaries" "$results/build"
  printf 'implementation,revision,binary_sha256\n' > "$results/implementations.csv"
  for index in "${!implementations[@]}"; do
    local implementation="${implementations[$index]}" revision="${revisions[$index]}"
    local source="$sources/$implementation" binary
    git diff --exit-code "$main_revision" "$revision" -- \
      bench/load bench/corpus config/default.yaml flake.nix flake.lock cabal.project cabal.project.freeze \
      > "$results/build/$implementation-input-diff.txt"
    git worktree add --detach "$source" "$revision"
    (
      cd "$source"
      cabal build -v0 exe:bench-load
      binary="$(cabal list-bin exe:bench-load)"
      cp "$binary" "$binaries/$implementation"
      if [[ "$implementation" == main ]]; then
        cabal build -v0 exe:bench-report
        cp "$(cabal list-bin exe:bench-report)" "$binaries/report"
      fi
      git ls-tree -r HEAD bench/load bench/corpus config/default.yaml flake.nix flake.lock \
        cabal.project cabal.project.freeze ecluse.cabal > "$results/build/$implementation-sources.txt"
      cp dist-newstyle/cache/plan.json "$results/build/$implementation-plan.json"
    ) 2>&1 | tee "$results/build/$implementation.log"
    printf '%s,%s,%s\n' "$implementation" "$revision" \
      "$(sha256sum "$binaries/$implementation" | cut -d ' ' -f 1)" >> "$results/implementations.csv"
  done
  {
    ghc --version
    cabal --version
    nix --version
    oha --version
    uname -a
    cat /proc/cpuinfo
  } > "$results/toolchain-and-runner.txt"
  sha256sum "$results"/build/*-plan.json > "$results/build-plan-hashes.txt"
  cp "$sources/main/bench/corpus/pins.json" "$results/corpus-pins.json"
  git rev-parse HEAD > "$results/measurement-commit.txt"
  sha256sum scripts/paired-json-load.sh > "$results/measurement-script.sha256"
  touch "$paired_root/builds-complete"
}

run_rotations() {
  [[ -f "$paired_root/builds-complete" ]]
  [[ "${BENCH_LOAD_CONCURRENCY:-}" == 100 && "${BENCH_LOAD_DURATION_SECONDS:-}" == 30 ]]
  [[ "${BENCH_LOAD_UPSTREAM_LATENCY_MS:-}" == 5 && "${BENCH_LOAD_PROBE_RTT:-}" == 0 ]]
  [[ "${BENCH_LOAD_SCENARIOS:-}" == "$expected_scenarios" ]]
  [[ -z "${BENCH_LOAD_SERVE_MAX_IN_FLIGHT:-}${BENCH_LOAD_PUBLIC_CONNECTIONS_PER_HOST:-}${BENCH_LOAD_PRIVATE_CONNECTIONS_PER_HOST:-}${BENCH_LOAD_THRASH_LIMITS_MIB:-}" ]]
  case "${BENCH_LOAD_POD:-}" in
    unlimited | 2cpu-1gib | 4cpu-1gib | 4cpu-2gib) ;;
    *) printf 'unsupported pod shape\n' >&2; return 1 ;;
  esac
  printf 'round,position,implementation,revision,exit_code,capture_exit_code\n' > "$results/samples.csv"
  env | LC_ALL=C sort | awk '/^(BENCH_LOAD_|GHCRTS=|GITHUB_EVENT_NAME=|RUNNER_ARCH=)/' > "$results/measurement-environment.txt"
  local failed=0 round position implementation directory status
  local -a order pipeline_status
  for round in "${!rotations[@]}"; do
    read -r -a order <<< "${rotations[$round]}"
    for position in "${!order[@]}"; do
      implementation="${order[$position]}"
      directory="$results/round-$((round + 1))/$((position + 1))-$implementation"
      mkdir -p "$directory"
      date -u +%FT%TZ > "$directory/started.txt"
      set +e
      (
        cd "$sources/$implementation"
        env -u GHCRTS GITHUB_STEP_SUMMARY="$directory/step-summary.md" \
          "$binaries/$implementation" +RTS -N3 -RTS
      ) 2>&1 | tee "$directory/bench-load-results.md"
      pipeline_status=("${PIPESTATUS[@]}")
      set -e
      status="${pipeline_status[0]}"
      printf '%s\n' "$status" > "$directory/exit-code.txt"
      printf '%s\n' "${pipeline_status[1]}" > "$directory/capture-exit-code.txt"
      date -u +%FT%TZ > "$directory/ended.txt"
      printf '%s,%s,%s,%s,%s,%s\n' "$((round + 1))" "$((position + 1))" "$implementation" \
        "$(git -C "$sources/$implementation" rev-parse HEAD)" "$status" "${pipeline_status[1]}" >> "$results/samples.csv"
      if ((status != 0 || pipeline_status[1] != 0)); then
        failed=1
      fi
    done
    local baseline="$results/round-$((round + 1))/baseline"
    mkdir -p "$baseline"
    for position in "${!order[@]}"; do
      if [[ "${order[$position]}" == main ]]; then
        directory="$results/round-$((round + 1))/$((position + 1))-main"
        cp "$directory/bench-load-results.md" "$baseline/bench-load-results.md"
        printf 'commit=%s\nrun=%s\ncreated=%s\n' "$main_revision" \
          "${GITHUB_SERVER_URL:-https://github.com}/${GITHUB_REPOSITORY:-AlexaDeWit/Ecluse}/actions/runs/${GITHUB_RUN_ID:-local-validation}" \
          "$(cat "$directory/started.txt")" > "$baseline/baseline.txt"
      fi
    done
    for position in "${!order[@]}"; do
      implementation="${order[$position]}"
      if [[ "$implementation" != main ]]; then
        directory="$results/round-$((round + 1))/$((position + 1))-$implementation"
        if ! "$binaries/report" against-main load "$baseline" "$directory/bench-load-results.md" \
          > "$directory/against-paired-main.md" 2> "$directory/comparison-stderr.txt"; then
          failed=1
        fi
      fi
    done
  done
  cat "$results/samples.csv"
  return "$failed"
}

case "${1:-}" in
  build) build_all ;;
  run) run_rotations ;;
  *) printf 'usage: paired-json-load.sh build|run\n' >&2; exit 2 ;;
esac
