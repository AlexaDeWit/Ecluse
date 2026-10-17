#!/usr/bin/env bash
# Check the sweep runner's job selection and each arm's environment through `task test-scripts`.
set -euo pipefail

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/repo/bench/load/sweeps"
cp "$repo/bench/load/sweeps/capability-1634.json" "$work/repo/bench/load/sweeps/"
cp "$repo/scripts/load-sweep.sh" "$work/"
fail=0

# The measure task records the settings it ran with and leaves the files the harness writes.
cat > "$work/bin/task" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
case "$1" in
  performance:load-measure)
    printf '%s pod=%s cpus=%s cores=%s heap=%s area=%s admission=%s probe=%s latency=%s passes=%s scenarios=%s summary=%s\n' \
      "$2" "$BENCH_LOAD_POD" "$BENCH_LOAD_PROXY_CPUS" "$BENCH_LOAD_PROXY_CORES" "$BENCH_LOAD_PROXY_MAX_HEAP_BYTES" \
      "$BENCH_LOAD_PROXY_ALLOC_AREA_MIB" "$BENCH_LOAD_SERVE_MAX_IN_FLIGHT" "$BENCH_LOAD_PROBE_RTT" \
      "$BENCH_LOAD_UPSTREAM_LATENCY_MS" "$BENCH_LOAD_PASSES" "$BENCH_LOAD_SCENARIOS" "${GITHUB_STEP_SUMMARY:-unset}" >> "$TEST_TRACE"
    printf 'report of %s\n' "$BENCH_LOAD_POD" > bench-load-results.md
    printf '{}\n' > load-observations.json
    [ "$BENCH_LOAD_PROXY_CORES" != "${TEST_FAIL_CORES:-none}" ]
    ;;
  performance:load-receipt) printf 'receipt %s %s\n' "$2" "$PERF_NATIVE_OUTCOME" >> "$TEST_TRACE" ;;
  *) exit 2 ;;
esac
STUB
cat > "$work/bin/cabal" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
[ "$*" = 'list-bin exe:bench-report' ] || exit 2
printf '%s/bin/bench-report\n' "$TEST_WORK"
STUB
cat > "$work/bin/bench-report" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
printf 'report %s\n' "$*" >> "$TEST_TRACE"
printf 'comparison\n'
STUB
chmod +x "$work/bin/task" "$work/bin/cabal" "$work/bin/bench-report"

check() {
  if [ "$2" = "$3" ]; then
    printf 'ok   - %s\n' "$1"
  else
    printf 'FAIL - %s\nexpected: %s\n     got: %s\n' "$1" "$2" "$3"
    fail=1
  fi
}

sweep() {
  (cd "$work/repo" && PATH="$work/bin:$PATH" TEST_WORK="$work" TEST_TRACE="$work/trace" bash "$work/load-sweep.sh" "$@")
}

refuses() {
  if sweep "$@" > /dev/null 2>&1; then
    printf 'FAIL - refuses %s\n' "$*"
    fail=1
  else
    printf 'ok   - refuses %s\n' "$*"
  fi
}

check 'plans every phase not on request by default' \
  'jobs=["A1","A2","B1","B2","B3","B4","B5","B6","B7","B8","C1","C2","C3","C4","C5","D1","D2","D3","D4","E1","E2","E3","E4","E5"]' \
  "$(sweep plan capability-1634)"
check 'plans the named phases and jobs, in sweep order' 'jobs=["D2","F1","F2","F3"]' "$(sweep plan capability-1634 ' F, D2')"
refuses plan capability-1634 Z9
refuses plan Capability_1634
refuses run capability-1634 Z9

: > "$work/trace"
check 'runs a job to the end and fails when an arm failed' 1 "$(TEST_FAIL_CORES=4 GITHUB_STEP_SUMMARY=/dev/null sweep run capability-1634 D1 > /dev/null 2>&1; echo $?)"
check 'measures each arm in slot order with its own settings, outside the run summary' \
  "PERF_ARTIFACT_DIR=sweep-results/D1/1-anchor/receipt pod=1gib cpus=2-3 cores=2 heap= area= admission= probe=0 latency=5 passes=loaded scenarios=npm/merge-cold,pypi/index-cold,npm/heavy-private-25pct,pypi/heavy-private-25pct,npm/assembled-response-hit,pypi/assembled-response-hit,npm/heavy-private,pypi/heavy-private,npm/herd,pypi/herd,npm/warm-under-cold,pypi/warm-under-cold summary=unset
receipt PERF_ARTIFACT_DIR=sweep-results/D1/1-anchor/receipt success" \
  "$(head -2 "$work/trace")"
check 'pins the request-only arms with no memory limit' \
  'pod=unlimited cpus=2-3 cores=8 area= | pod=unlimited cpus=2-3 cores=4 area=32 | pod=unlimited cpus=2-3 cores=4 area=' \
  "$(grep '^PERF' "$work/trace" | sed -n '2,4p' | cut -d' ' -f2-4,6 | paste -sd'|' | sed 's/|/ | /g')"
check 'records the failed arm as a failed native outcome' 'receipt PERF_ARTIFACT_DIR=sweep-results/D1/3-d-n4-area32/receipt failure' "$(grep '3-d-n4-area32' "$work/trace" | grep '^receipt')"
check 'keeps each arm in its own directory and compares them in slot order' \
  "report sweep capability-1634 D1 sweep-results/D1/1-anchor/bench-load-results.md sweep-results/D1/2-d-n8/bench-load-results.md sweep-results/D1/3-d-n4-area32/bench-load-results.md sweep-results/D1/4-d-n4/bench-load-results.md sweep-results/D1/5-anchor/bench-load-results.md" \
  "$(grep '^report' "$work/trace")"
check 'leaves the arm report beside its observations' 'report of unlimited' "$(cat "$work/repo/sweep-results/D1/2-d-n8/bench-load-results.md")"
check 'narrows every arm to the scenarios a dispatch names' 'scenarios=npm/herd' \
  "$(: > "$work/trace"; BENCH_LOAD_SCENARIOS=npm/herd sweep run capability-1634 A1 > /dev/null 2>&1; grep -o 'scenarios=[^ ]*' "$work/trace" | sort -u)"

exit "$fail"
