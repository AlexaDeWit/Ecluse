#!/usr/bin/env bash
set -euo pipefail

paired_repo=$(git rev-parse --show-toplevel)
paired_root="$paired_repo/scratchpad/paired-arm-load"
paired_a=8f78594a1a3182258f0f660d73ab94f86d958dd3
paired_b=516dff4c5867a2337c61f7bc9bea280ec8a8d7e1
paired_cgroup=/sys/fs/cgroup/ecluse-bench-load

revision() {
  case "$1" in
    A) printf '%s\n' "$paired_a" ;;
    B) printf '%s\n' "$paired_b" ;;
    *) echo 'Expected revision A or B.' >&2; exit 2 ;;
  esac
}

environment() {
  env | LC_ALL=C sort | sed -n -E '/^(BENCH_LOAD_|BENCH_PATTERN_|GHCRTS=|PATH=|LD_LIBRARY_PATH=|LANG=|LC_|TZ=|CABAL_DIR=|IN_NIX_SHELL=|NIX_CFLAGS_COMPILE=|NIX_LDFLAGS=|NIX_GHC|GITHUB_ACTIONS=|GITHUB_EVENT_NAME=|GITHUB_SHA=|GITHUB_RUN_ID=|GITHUB_RUN_ATTEMPT=|RUNNER_ARCH=|RUNNER_OS=|ImageOS=|ImageVersion=)/p'
}

prepare() {
  [[ ! -e "$paired_root" ]]
  mkdir -p "$paired_root/provenance" "$paired_root/builds" "$paired_root/results"
  for side in A B; do
    git worktree add --detach "$paired_root/$side" "$(revision "$side")"
  done
  git rev-parse HEAD > "$paired_root/provenance/diagnostic-commit.txt"
  git diff HEAD > "$paired_root/provenance/diagnostic.patch"
  environment > "$paired_root/provenance/build-environment.txt"
  uname -a > "$paired_root/provenance/uname.txt"
  lscpu > "$paired_root/provenance/lscpu.txt"
  cat /proc/cpuinfo > "$paired_root/provenance/cpuinfo.txt"
  cat /proc/meminfo > "$paired_root/provenance/meminfo.txt"
  getconf CLK_TCK > "$paired_root/provenance/clock-ticks.txt"
  ghc --info > "$paired_root/provenance/ghc-info.txt"
  ghc-pkg list > "$paired_root/provenance/ghc-packages.txt"
  cabal --version > "$paired_root/provenance/cabal-version.txt"
  nix --version > "$paired_root/provenance/nix-version.txt"
  oha --version > "$paired_root/provenance/oha-version.txt"
  sha256sum "$(readlink -f "$(command -v oha)")" > "$paired_root/provenance/oha.sha256"
  cat > "$paired_root/provenance/limits.txt" <<'EOF'
Diagnostic only. A and B are unmodified native executables.
Direct children omit parent floors and preceding grid workloads.
With the sampler on, it reads actual cgroup members every two seconds outside the harness leaf.
With the sampler off, external process, affinity, pressure and mapped-file observations are unavailable.
Process CPU and cgroup CPU samples do not share the exact native RTS window.
Sampler rusage and its own process counters expose observer cost.
Native JSON retains proxy RTS CPU, GC CPU, allocation, GC counts and admission samples.
Unmodified binaries delete raw oha files and omit the starting native cgroup reading.
Timed response bytes, upstream request counts and exact cgroup CPU window deltas remain unverified.
The next attribution phase needs symmetric instrumentation if these limits block attribution.
No absent value is replaced with zero. Every native exit status is retained.
EOF
}

build() {
  local side=$1 tree destination status=0 binary
  tree="$paired_root/$side"
  destination="$paired_root/builds/$side"
  mkdir -p "$destination"
  [[ $(git -C "$tree" rev-parse HEAD) == "$(revision "$side")" ]]
  git -C "$tree" diff --exit-code HEAD
  git -C "$tree" status --porcelain > "$destination/source-status.txt"
  git -C "$tree" rev-parse HEAD > "$destination/revision.txt"
  date --iso-8601=ns > "$destination/started.txt"
  printf '%s\n' 'cabal build -v3 exe:bench-load' > "$destination/command.txt"
  (
    cd "$tree"
    sha256sum flake.nix flake.lock cabal.project cabal.project.freeze ecluse.cabal bench/corpus/pins.json bench/load/floors.json > "$destination/inputs.sha256"
    cabal build -v3 exe:bench-load
  ) > "$destination/stdout.txt" 2> "$destination/stderr.txt" || status=$?
  printf '%s\n' "$status" > "$destination/exit.txt"
  date --iso-8601=ns > "$destination/ended.txt"
  [[ $status == 0 ]] || return "$status"
  binary=$(cd "$tree" && cabal list-bin exe:bench-load)
  cp "$binary" "$destination/bench-load"
  printf '%s\n' "$binary" > "$destination/original-path.txt"
  cp "$tree/dist-newstyle/cache/plan.json" "$destination/plan.json"
  find "$tree/dist-newstyle" -type f \( -name '*.o' -o -name '*.a' -o -name '*.so' -o -name '*.hi' \) -print0 | sort -z | xargs -0 -r sha256sum > "$destination/objects.sha256"
  ldd "$destination/bench-load" > "$destination/ldd.txt"
  sha256sum "$destination/bench-load" > "$destination/binary.sha256"
}

seal() {
  for side in A B; do
    [[ $(cat "$paired_root/builds/$side/exit.txt") == 0 ]]
    sha256sum --check "$paired_root/builds/$side/binary.sha256"
  done
  cmp "$paired_root/A/flake.lock" "$paired_root/B/flake.lock"
  date --iso-8601=ns > "$paired_root/provenance/both-built.txt"
}

delegate() {
  sudo swapoff -a
  sudo mkdir -p "$paired_cgroup/harness"
  sudo chown -R "$(id -u):$(id -g)" "$paired_cgroup"
  printf '%s\n' '+cpu +memory +pids' | sudo tee "$paired_cgroup/cgroup.subtree_control" >/dev/null
}

join_harness() {
  local target_pid=$BASHPID membership
  printf 'pid=%s\n' "$target_pid" > "$1"
  printf '%s\n' "$target_pid" | sudo tee "$paired_cgroup/harness/cgroup.procs" >/dev/null
  membership=$(cat "/proc/$target_pid/cgroup")
  printf '%s\n' "$membership" >> "$1"
  if [[ $membership != "0::${paired_cgroup#/sys/fs/cgroup}/harness" ]]; then
    printf 'Unexpected cgroup for launcher PID %s: %s\n' "$target_pid" "$membership" >&2
    return 1
  fi
}

preflight() {
  [[ -z $(cat "$paired_cgroup/harness/cgroup.procs") ]]
  (join_harness "$paired_root/provenance/cgroup-preflight.txt")
  cat "$paired_root/provenance/cgroup-preflight.txt"
  [[ -z $(cat "$paired_cgroup/harness/cgroup.procs") ]]
}

settings() {
  local name shape=${1:-2cpu-1gib}
  case "$shape" in
    2cpu-1gib | 4cpu-2gib) ;;
    *) echo 'Expected pod shape 2cpu-1gib or 4cpu-2gib.' >&2; return 2 ;;
  esac
  while IFS='=' read -r name _; do
    case "$name" in
      BENCH_LOAD_* | BENCH_PATTERN_* | GHCRTS | ECLUSE_PERFORMANCE_OBSERVATIONS) unset "$name" ;;
    esac
  done < <(env)
  export NO_COLOR=true GHCRTS='-T -N3'
  export BENCH_LOAD_CGROUP="$paired_cgroup" BENCH_LOAD_POD="$shape"
  export BENCH_LOAD_PROBE_RTT=0 BENCH_LOAD_UPSTREAM_LATENCY_MS=5
  export BENCH_LOAD_DURATION_SECONDS=30 BENCH_LOAD_CONCURRENCY=100
  export BENCH_LOAD_PAYLOAD_BYTES=363520 BENCH_LOAD_CACHE_MAX_ENTRIES=3
  export BENCH_LOAD_WORKING_SET=64 BENCH_LOAD_ISOLATE_OHA=1
}

run_native() {
  local side=$1 label=$2 limit=$3 sampler_state=$4
  shift 4
  local destination="$paired_root/results/$label" child sampler status=0 sample_status=unavailable
  [[ $sampler_state == on || $sampler_state == off ]]
  [[ -f "$paired_root/provenance/both-built.txt" && ! -e "$destination" ]]
  [[ -w "$paired_cgroup/harness/cgroup.procs" ]]
  [[ -z $(cat "$paired_cgroup/harness/cgroup.procs") ]]
  [[ -z $(find "$paired_cgroup" -mindepth 1 -maxdepth 1 -type d -name 'proxy-*' -print -quit) ]]
  sha256sum --check "$paired_root/builds/$side/binary.sha256"
  mkdir -p "$destination"
  printf '%s\n' "$sampler_state" > "$destination/sampler-state.txt"
  if [[ $sampler_state == off ]]; then
    printf '%s\n' 'Unavailable: external process CPU, affinity, pressure, cgroup samples, mapped files and sampler rusage. The sampler is disabled. Native JSON and launcher membership remain available.' > "$destination/sampler-observations.txt"
  fi
  export ECLUSE_PERFORMANCE_OBSERVATIONS="$destination/native-observations.json"
  environment > "$destination/environment.txt"
  printf '%s\0' "$paired_root/builds/$side/bench-load" "$@" > "$destination/argv.nul"
  printf '%q ' "$paired_root/builds/$side/bench-load" "$@" > "$destination/command.txt"
  printf '\n' >> "$destination/command.txt"
  date --iso-8601=ns > "$destination/started.txt"
  (
    join_harness "$destination/launcher-membership.txt"
    cd "$paired_root/$side"
    exec timeout --signal=TERM --kill-after=30s "$limit" "$paired_root/builds/$side/bench-load" "$@"
  ) > "$destination/stdout.txt" 2> "$destination/stderr.txt" &
  child=$!
  printf '%s\n' "$child" > "$destination/timeout-pid.txt"
  if [[ $sampler_state == on ]]; then
    /usr/bin/time -v -o "$destination/sampler-rusage.txt" bash "$paired_repo/scripts/bench-load-sample.sh" "$paired_cgroup" "$destination" &
    sampler=$!
  fi
  wait "$child" || status=$?
  printf '%s\n' "$status" > "$destination/exit.txt"
  date --iso-8601=ns > "$destination/ended.txt"
  if [[ $sampler_state == on ]]; then
    touch "$destination/stop-sampler"
    sample_status=0
    wait "$sampler" || sample_status=$?
  fi
  printf '%s\n' "$sample_status" > "$destination/sampler-exit.txt"
  printf '%s: native exit %s, sampler exit %s\n' "$label" "$status" "$sample_status"
  [[ $status == 0 && ( $sampler_state == off || $sample_status == 0 ) ]]
}

children() {
  local scenario pass side ordinal failed=0
  local -a arguments
  settings
  for scenario in pypi/wheel-hot-path npm/tarball-ceiling; do
    for pass in loaded concurrency-one; do
      ordinal=0
      for side in A B B A B A A B; do
        ordinal=$((ordinal + 1))
        arguments=("$scenario")
        if [[ $pass == concurrency-one ]]; then arguments=(--service-time "$scenario"); fi
        bash "$paired_repo/scripts/bench-load-paired.sh" child "$side" "children/${scenario//\//-}/$pass/$ordinal-$side" 2cpu-1gib on "${arguments[@]}" || failed=1
      done
    done
  done
  return "$failed"
}

observer_plan() {
  local shape scenario block token side sampler_state ordinal
  printf 'shape\tscenario\tordinal\tside\tsampler\n'
  for shape in 2cpu-1gib 4cpu-2gib; do
    for scenario in pypi/wheel-hot-path npm/tarball-ceiling; do
      ordinal=0
      for block in 1 2; do
        for token in A:on B:on B:off A:off A:off B:off B:on A:on; do
          side=${token%:*}
          sampler_state=${token#*:}
          if [[ $block == 2 ]]; then
            if [[ $sampler_state == on ]]; then sampler_state=off; else sampler_state=on; fi
          fi
          ordinal=$((ordinal + 1))
          printf '%s\t%s\t%02d\t%s\t%s\n' "$shape" "$scenario" "$ordinal" "$side" "$sampler_state"
        done
      done
    done
  done
}

observer_control() {
  local output="$paired_root/results/observer-control" shape scenario ordinal side sampler_state status failed=0 label
  [[ ! -e "$output" ]]
  mkdir -p "$output"
  observer_plan > "$output/plan.tsv"
  printf 'shape\tscenario\tordinal\tside\tsampler\tdispatch_exit\n' > "$output/outcomes.tsv"
  while IFS=$'\t' read -r shape scenario ordinal side sampler_state; do
    [[ $shape != shape ]] || continue
    label="observer-control/$shape/${scenario//\//-}/$ordinal-$side-$sampler_state"
    status=0
    bash "$paired_repo/scripts/bench-load-paired.sh" child "$side" "$label" "$shape" "$sampler_state" "$scenario" || status=$?
    printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$shape" "$scenario" "$ordinal" "$side" "$sampler_state" "$status" >> "$output/outcomes.tsv"
    if [[ $status != 0 ]]; then failed=1; fi
  done < "$output/plan.tsv"
  return "$failed"
}

case "${1:-}" in
  prepare) prepare ;;
  build) build "${2:?Expected A or B}" ;;
  seal) seal ;;
  delegate) delegate ;;
  preflight) preflight ;;
  children) children ;;
  observer-plan) observer_plan ;;
  observer-control) observer_control ;;
  child)
    shift
    child_side=$1
    child_label=$2
    child_shape=$3
    child_sampler=$4
    shift 4
    settings "$child_shape"
    run_native "$child_side" "$child_label" 5m "$child_sampler" "$@"
    ;;
  grid)
    settings
    revision "${2:?Expected A or B}" >/dev/null
    [[ ${3:-} =~ ^[1-4]$ ]]
    run_native "$2" "grid/$3-$2" 75m on
    ;;
  *) echo 'usage: bench-load-paired.sh {prepare|build A/B|seal|delegate|preflight|children|observer-plan|observer-control|grid A/B 1-4}' >&2; exit 2 ;;
esac
