#!/usr/bin/env bash
# Deterministic unit test for load-floors.sh. A stub `gh` on PATH writes each run's record and one
# artifact file, and a stub reporter records its arguments, so no network or build is needed.
# Run via `task test-scripts`.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
script="$here/load-floors.sh"

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir "$work/bin" "$work/repo"
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
# gh run view <id> --json <fields>   |   gh run download <id> --pattern <glob> --dir <dir>
printf '%s\n' "$*" >> "$STUB_DIR/gh-calls"
case "$1 $2" in
  "run view") printf '{"databaseId": %s, "url": "https://example.test/runs/%s", "headSha": "sha%s", "event": "workflow_dispatch"}\n' "$3" "$3" "$3" ;;
  "run download")
    mkdir -p "$7/bench-load-results-ARM64-2cpu-1gib-sha$3"
    printf '{}\n' > "$7/bench-load-results-ARM64-2cpu-1gib-sha$3/load-observations.json" ;;
  *) exit 1 ;;
esac
STUB
cat > "$work/bin/reporter" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$@" > "$STUB_DIR/reporter-args"
STUB
chmod +x "$work/bin/gh" "$work/bin/reporter"

fail=0
export STUB_DIR="$work" PATH="$work/bin:$PATH" LOAD_FLOORS_REPORTER="reporter"

expect_usage() {
  local name="$1"
  shift
  if (cd "$work/repo" && bash "$script" "$@" 2> /dev/null); then
    echo "FAIL: $name: exited 0" >&2
    fail=1
  elif [ "$?" -ne 2 ]; then
    echo "FAIL: $name: did not exit 2" >&2
    fail=1
  fi
}

expect_usage "no runs" 
expect_usage "two runs" 11,12
expect_usage "four runs" 11,12,13,14
expect_usage "a run that is not an ID" 11,12,abc
expect_usage "an empty run" 11,,13
expect_usage "three arguments" 11,12,13 npm/herd extra

# Three runs: each run's record and artifacts land in its own directory, and the reporter gets the
# floors file and that directory.
rm -f "$work/gh-calls" "$work/reporter-args"
(cd "$work/repo" && bash "$script" 11,12,13 2> /dev/null)
runs_dir="$(sed -n 3p "$work/reporter-args")"
[ "$(sed -n 1,2p "$work/reporter-args")" = "$(printf 'floors\nbench/load/floors.json')" ] || { echo "FAIL: three runs: reporter arguments $(cat "$work/reporter-args")" >&2; fail=1; }
[ "$(wc -l < "$work/reporter-args")" -eq 3 ] || { echo "FAIL: three runs: a scenario list was passed" >&2; fail=1; }
for id in 11 12 13; do
  grep -q "\"databaseId\": $id" "$work/repo/$runs_dir/$id/run.json" || { echo "FAIL: three runs: no record for $id" >&2; fail=1; }
  [ -f "$work/repo/$runs_dir/$id/bench-load-results-ARM64-2cpu-1gib-sha$id/load-observations.json" ] || { echo "FAIL: three runs: no artifact for $id" >&2; fail=1; }
  grep -qx "run download $id --pattern bench-load-results-\* --dir $runs_dir/$id" "$work/gh-calls" || { echo "FAIL: three runs: no download of $id's results" >&2; fail=1; }
done

# A scenario list reaches the reporter as one argument.
rm -f "$work/reporter-args"
(cd "$work/repo" && bash "$script" 11,12,13 npm/merge-cold,pypi/index-cold 2> /dev/null)
[ "$(sed -n 4p "$work/reporter-args")" = "npm/merge-cold,pypi/index-cold" ] || { echo "FAIL: scenario list: reporter arguments $(cat "$work/reporter-args")" >&2; fail=1; }

[ "$fail" -eq 0 ] && echo "load-floors.sh: all cases passed"
exit "$fail"
