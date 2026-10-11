#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra DeWit
# SPDX-License-Identifier: MIT
set -euo pipefail

[[ "${GITHUB_ACTIONS:-}" == true && "${GITHUB_REF:-}" == refs/heads/measure/1635-fixed-rtt ]]
root="${GITHUB_WORKSPACE:?}"
cd "$root"
out="$root/fixed-rtt-results"
bin="${RUNNER_TEMP:?}/measure-1635-bin"
summary="${GITHUB_STEP_SUMMARY:?}"
build_flags=(--enable-optimization=1 -fdev-http-egress)
order=(pair1-candidate pair1-baseline pair2-baseline pair2-candidate pair3-candidate pair3-baseline)
mkdir -p "$out" "$bin"
unset GHCRTS

sha_for() {
  case "$1" in
    candidate) printf '%s\n' "$MEASURE_CANDIDATE_SHA" ;;
    baseline) printf '%s\n' "$MEASURE_BASELINE_SHA" ;;
    *) return 1 ;;
  esac
}

for run in "${order[@]}"; do
  mkdir "$out/$run"
  printf '%s\n' "$(sha_for "${run#*-}")" > "$out/$run/source-sha.txt"
  printf '%s\n' 'not run: setup or an earlier cleanup may fail' > "$out/$run/status.txt"
  printf '# %s: %s\n\nDiagnostic subset, not held to floors.\n\n' "$run" "$(sha_for "${run#*-}")" > "$out/$run/bench-load-results.md"
done
{
  printf '%s\n' '# PR 1635 fixed RTT diagnostic' ''
  printf '%s\n' 'Six subset reports. Not held to success floors. The full acceptance criteria still apply.' ''
  printf '%s\n' 'Exact total allocation is not emitted by the unchanged driver. Allocation/success is rounded.' ''
  printf 'Wrapper: %s\nCandidate: %s\nBaseline: %s\n' "$(git -C "$root" rev-parse HEAD)" "$MEASURE_CANDIDATE_SHA" "$MEASURE_BASELINE_SHA"
  printf 'Build flags: %s\n' "${build_flags[*]}"
  printf 'Run order: %s\n' "${order[*]}"
  printf '%s\n' 'Driver: +RTS -N3 -RTS. Children inherit -T -N3. Proxies receive -T and resolve their own RTS plan.'
  env | LC_ALL=C sort | awk '/^(BENCH_LOAD_|CABAL_DIR=|RUNNER_OS=|RUNNER_ARCH=|GITHUB_RUN_ID=|GITHUB_RUN_ATTEMPT=)/'
  printf '\nRoot settings:\n'
  sha256sum "$root"/{cabal.project,cabal.project.freeze,ecluse.cabal,flake.nix,flake.lock,Taskfile.yml}
  if [[ -f "$CABAL_DIR/config" ]]; then sha256sum "$CABAL_DIR/config"; fi
} | tee "$out/manifest.md" >> "$summary"
for run in "${order[@]}"; do
  cp "$out/manifest.md" "$out/$run/settings.md"
done

for source in "$root" "$root/candidate" "$root/baseline"; do
  for local_settings in cabal.project.local cabal.project.local.freeze; do
    [[ ! -e "$source/$local_settings" ]]
  done
done
for role in candidate baseline; do
  [[ "$(git -C "$root/$role" rev-parse HEAD)" == "$(sha_for "$role")" ]]
  git -C "$root/$role" diff --exit-code
  git -C "$root/$role" diff --cached --exit-code
  for file in cabal.project cabal.project.freeze flake.nix flake.lock Taskfile.yml; do
    cmp "$root/$file" "$root/$role/$file"
  done
done
for area in bench/load bench/corpus; do
  diff -qr "$root/candidate/$area" "$root/baseline/$area" >> "$out/source-comparison.txt"
  printf '%s unchanged between H and D\n' "$area" >> "$out/source-comparison.txt"
done
if [[ "${MEASURE_CABAL_CACHE_HIT:-}" != true ]]; then
  nix develop .#ci --command task update 2>&1 | tee "$out/index-update.log"
fi

build_role() {
  local role="$1" source="$root/$1" resolved
  mkdir "$out/$role-build"
  (
    cd "$source"
    printf 'Compiled source: %s\nSource directory: %s\n' "$(git rev-parse HEAD)" "$PWD"
    sha256sum cabal.project cabal.project.freeze ecluse.cabal flake.nix flake.lock
    nix develop .#ci --command ghc --numeric-version
    nix develop .#ci --command cabal --numeric-version
    nix develop .#ci --command cabal build -v0 exe:bench-load "${build_flags[@]}"
  ) 2>&1 | tee "$out/$role-build/build.log"
  resolved="$(cd "$source" && nix develop .#ci --command cabal list-bin -v0 exe:bench-load "${build_flags[@]}")"
  [[ -x "$resolved" ]]
  cp "$resolved" "$bin/$role-bench-load"
  cp "$source/dist-newstyle/cache/plan.json" "$out/$role-build/plan.json"
  cp "$source"/{cabal.project,cabal.project.freeze,ecluse.cabal} "$out/$role-build/"
  printf '%s\n' "$resolved" > "$out/$role-build/resolved-bin.txt"
  sha256sum "$bin/$role-bench-load" > "$out/$role-build/binary.sha256"
}
build_role candidate
build_role baseline

# The existing harness retires its cgroups. A remaining proxy must block the next pass.
verify_cleanup() {
  local group
  for group in "$BENCH_LOAD_CGROUP"/proxy-*; do
    [[ -d "$group" ]] || continue
    printf '%s\n' "$group"
    cat "$group/cgroup.procs" "$group/cgroup.events"
    [[ -z "$(cat "$group/cgroup.procs")" && "$(cat "$group/cgroup.events")" == *'populated 0'* ]] || return 1
  done
  printf '%s\n' 'No live proxy remains.'
}

sudo swapoff -a
sudo mkdir -p "$BENCH_LOAD_CGROUP/harness"
sudo chown -R "$(id -u):$(id -g)" "$BENCH_LOAD_CGROUP"
echo '+cpu +memory +pids' | sudo tee "$BENCH_LOAD_CGROUP/cgroup.subtree_control" >/dev/null
cat "$BENCH_LOAD_CGROUP/cgroup.subtree_control" > "$out/controllers.txt"
echo "$$" | sudo tee "$BENCH_LOAD_CGROUP/harness/cgroup.procs" >/dev/null
verify_cleanup > "$out/initial-cleanup.txt"

require_row() {
  local count
  count="$(grep -c -F -x -- "$2" "$1" || true)"
  printf '%s: %s occurrence(s), expected %s\n' "$2" "${count:-0}" "$3"
  [[ "$count" == "$3" ]]
}

verify_settings() {
  local report="$1" failed=0 row
  for row in \
    '| injected upstream latency | 5.0 ms |' \
    '| harness GHC capabilities (-N) | 3 |' \
    '| load | 100 connections x 30 s (a scenario may scale its own connections) |' \
    "| pod shape | 4cpu-1gib: the proxy's own cgroup, CPU-time quota via cpu.max, memory.max set, swap off |" \
    "| serve admission (requests) | 40 (the proxy's computed default) |" \
    '| private pool | computed by the proxy from its fd limit |' \
    '| public pool | computed by the proxy from its fd limit |'; do
    require_row "$report" "$row" 2 || failed=1
  done
  require_row "$report" '| heap ceiling (-M) / proxy GHC capabilities / allocation area per capability | 864.0 MiB / 4 / 32.0 MiB |' 5 || failed=1
  grep -Fq 'This run is not held to the success floors' "$report" || failed=1
  return "$failed"
}

failed=0
for run in "${order[@]}"; do
  role="${run#*-}"
  report="$out/$run/bench-load-results.md"
  printf '%s\n' 'running' > "$out/$run/status.txt"
  set +e
  (
    cd "$root/$role"
    export GITHUB_STEP_SUMMARY="$out/$run/summary.md"
    nix develop .#ci --command "$bin/$role-bench-load" +RTS -N3 -RTS
  ) 2>&1 | tee -a "$report"
  codes=("${PIPESTATUS[@]}")
  set -e
  printf 'Harness exit: %s\nCapture exit: %s\n' "${codes[0]}" "${codes[1]}" > "$out/$run/status.txt"
  if [[ "${codes[0]}" != 0 || "${codes[1]}" != 0 ]]; then failed=1; fi
  if ! verify_settings "$report" > "$out/$run/settings-check.txt"; then failed=1; fi
  cat "$report" "$out/$run/status.txt" "$out/$run/settings-check.txt" >> "$summary"
  if ! verify_cleanup > "$out/$run/cleanup.txt"; then
    printf '%s\n' 'Cleanup failed. Remaining passes stay unrun to prevent overlapping proxies.' | tee -a "$out/$run/status.txt" "$summary"
    failed=1
    break
  fi
done
exit "$failed"
