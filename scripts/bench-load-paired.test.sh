#!/usr/bin/env bash
set -euo pipefail

paired_test_root=$(mktemp -d)
trap 'rm -rf -- "$paired_test_root"' EXIT
export PAIRED_TEST_ROOT="$paired_test_root"

# Exercise the real functions with stub commands and ordinary files instead of cgroups.
sed '/^case "${1:-}" in/,$d' scripts/bench-load-paired.sh > "$paired_test_root/check.sh"
cat >> "$paired_test_root/check.sh" <<'CHECK'
observer_plan > "$PAIRED_TEST_ROOT/plan.tsv"
awk -F '\t' '
  NR == 1 { next }
  {
    cell = $1 "/" $2
    treatment = cell "/" $4 "/" $5
    cells[cell]++
    counts[treatment]++
    positions[treatment] += $3
  }
  END {
    if (NR != 65 || length(cells) != 4 || length(counts) != 16) exit 1
    for (cell in cells) if (cells[cell] != 16) exit 1
    for (treatment in counts) if (counts[treatment] != 4 || positions[treatment] != 34) exit 1
  }
' "$PAIRED_TEST_ROOT/plan.tsv"

paired_root="$PAIRED_TEST_ROOT/artifacts"
paired_repo="$PAIRED_TEST_ROOT/repo"
paired_cgroup="$PAIRED_TEST_ROOT/cgroup"
mkdir -p "$paired_root/provenance" "$paired_root/builds/A" "$paired_root/A" "$paired_cgroup/harness" "$paired_repo/scripts"
touch "$paired_root/provenance/both-built.txt" "$paired_cgroup/harness/cgroup.procs"
cat > "$paired_root/builds/A/bench-load" <<'NATIVE'
#!/usr/bin/env bash
printf 'native stdout\n'
printf 'native stderr\n' >&2
exit "${PAIRED_STUB_EXIT:-0}"
NATIVE
chmod +x "$paired_root/builds/A/bench-load"
sha256sum "$paired_root/builds/A/bench-load" > "$paired_root/builds/A/binary.sha256"
cat > "$paired_repo/scripts/bench-load-sample.sh" <<'SAMPLER'
#!/usr/bin/env bash
set -euo pipefail
touch "$2/sampler-started"
while [[ ! -f "$2/stop-sampler" ]]; do sleep 0.01; done
SAMPLER
environment() { printf 'stub environment\n'; }
join_harness() { printf 'stub membership\n' > "$1"; }

run_native A on-row 5s on example
[[ -f "$paired_root/results/on-row/sampler-started" ]]
[[ $(cat "$paired_root/results/on-row/sampler-state.txt") == on ]]
[[ $(cat "$paired_root/results/on-row/sampler-exit.txt") == 0 ]]
run_native A off-row 5s off example
[[ ! -e "$paired_root/results/off-row/sampler-started" ]]
[[ ! -e "$paired_root/results/off-row/sampler-rusage.txt" ]]
[[ ! -e "$paired_root/results/off-row/stop-sampler" ]]
[[ $(cat "$paired_root/results/off-row/sampler-state.txt") == off ]]
[[ $(cat "$paired_root/results/off-row/sampler-exit.txt") == unavailable ]]
[[ -s "$paired_root/results/off-row/sampler-observations.txt" ]]
[[ $(cat "$paired_root/results/off-row/stdout.txt") == 'native stdout' ]]
[[ $(cat "$paired_root/results/off-row/stderr.txt") == 'native stderr' ]]
export PAIRED_STUB_EXIT=7
if run_native A failed-row 5s off example; then
  echo 'A failed native command passed.' >&2
  exit 1
fi
[[ $(cat "$paired_root/results/failed-row/exit.txt") == 7 ]]
cat > "$paired_repo/scripts/bench-load-paired.sh" <<'DISPATCH'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$*" >> "$PAIRED_TEST_ROOT/dispatched.txt"
if [[ $2 == B ]]; then exit 7; fi
DISPATCH
if observer_control; then
  echo 'The observer control ignored failed commands.' >&2
  exit 1
fi
[[ $(wc -l < "$PAIRED_TEST_ROOT/dispatched.txt") == 64 ]]
awk -F '\t' 'NR > 1 && (($4 == "A" && $6 != 0) || ($4 == "B" && $6 != 7)) { exit 1 } END { if (NR != 65) exit 1 }' "$paired_root/results/observer-control/outcomes.tsv"
printf 'ok: balanced plan, sampler on/off, native output and all failure statuses\n'
CHECK
bash "$paired_test_root/check.sh"
