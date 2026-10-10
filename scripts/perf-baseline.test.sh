#!/usr/bin/env bash
# Deterministic unit test for perf-baseline.sh. A stub `gh` on PATH serves the run list
# and each run's artifacts from JSON files, and writes one file for a download, so no
# network is needed. Every case but the usage error must exit 0: a missing baseline
# never fails a job. Run via `task test-scripts`.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
script="$here/perf-baseline.sh"

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir "$work/bin"
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
# gh api <path>   |   gh run download <id> --repo <repo> --name <name> --dir <dir>
case "$1" in
  api)
    printf '%s\n' "$2" >> "$STUB_DIR/requests"
    case "$2" in
      */workflows/*/runs\?*)
        [ -z "${STUB_LIST_FAIL:-}" ] || exit 1
        cat "$STUB_DIR/runs.json" ;;
      */runs/*/artifacts\?*)
        id="${2#*/actions/runs/}"
        cat "$STUB_DIR/artifacts-${id%%/*}.json" ;;
      *) exit 1 ;;
    esac ;;
  run)
    [ -z "${STUB_DOWNLOAD_FAIL:-}" ] || exit 1
    printf '%s %s\n' "$3" "$7" >> "$STUB_DIR/downloads"
    printf 'results of %s\n' "$7" > "$9/results.txt" ;;
  *) exit 1 ;;
esac
STUB
chmod +x "$work/bin/gh"

fail=0

# One run of the list. $1 id, $2 branch, $3 event, $4 head repository.
run_json() {
  printf '{"id": %s, "head_branch": "%s", "event": "%s", "head_sha": "sha%s", "html_url": "https://example.test/runs/%s", "created_at": "2026-01-0%sT00:00:00Z", "head_repository": {"full_name": "%s"}}' \
    "$1" "$2" "$3" "$1" "$1" "$1" "$4"
}

# The run list, newest first, from run_json arguments in groups of four.
runs() {
  local sep=""
  printf '{"workflow_runs": ['
  while [ "$#" -gt 0 ]; do
    printf '%s' "$sep"
    run_json "$1" "$2" "$3" "$4"
    sep=", "
    shift 4
  done
  printf ']}\n'
}

# One run's artifacts. $1 run id, then name and expired ("true" or "false") in pairs.
artifacts() {
  local id="$1" sep=""
  shift
  {
    printf '{"artifacts": ['
    while [ "$#" -gt 0 ]; do
      printf '%s{"name": "%s", "expired": %s}' "$sep" "$1" "$2"
      sep=", "
      shift 2
    done
    printf ']}\n'
  } > "$stub/artifacts-$id.json"
}

# Start a case with an empty stub directory, an empty output directory, and the default prefix.
begin() {
  stub="$work/stub"
  out="$work/out"
  prefix="results-ARM64-"
  rm -rf "$stub" "$out"
  mkdir "$stub"
}

# Run the script for bench.yml and $prefix and assert its outcome. $1 name, $2 the expected
# baseline.txt, $3 the expected download ("<run> <artifact>", or empty for none).
check() {
  local name="$1" want_record="$2" want_download="$3" rc=0 got_record got_download
  STUB_DIR="$stub" GITHUB_REPOSITORY=owner/repo PATH="$work/bin:$PATH" \
    bash "$script" bench.yml "$prefix" "$out" > /dev/null 2>&1 || rc=$?
  got_record="$(cat "$out/baseline.txt" 2>/dev/null || true)"
  got_download="$(cat "$stub/downloads" 2>/dev/null || true)"
  if [ "$rc" -eq 0 ] && [ "$got_record" = "$want_record" ] && [ "$got_download" = "$want_download" ]; then
    printf 'ok   - %s\n' "$name"
  else
    printf 'FAIL - %s (exit %s)\n  record:   %s\n  download: %s\n' "$name" "$rc" "$got_record" "$got_download"
    fail=1
  fi
}

record() {
  printf 'commit=sha%s\nrun=https://example.test/runs/%s\ncreated=2026-01-0%sT00:00:00Z' "$1" "$1" "$1"
}

begin
runs 3 main push owner/repo 2 main push owner/repo > "$stub/runs.json"
artifacts 3 results-ARM64-sha3 false
check "the newest successful run on main supplies the baseline" "$(record 3)" "3 results-ARM64-sha3"
if [ "$(cat "$out/results.txt" 2>/dev/null)" = "results of results-ARM64-sha3" ]; then
  printf 'ok   - the artifact lands beside the record\n'
else
  printf 'FAIL - the artifact lands beside the record\n'
  fail=1
fi
if grep -q 'workflows/bench.yml/runs?branch=main&status=success' "$stub/requests"; then
  printf 'ok   - the list asks for successful runs of the workflow on main\n'
else
  printf 'FAIL - the list asks for successful runs of the workflow on main\n'
  fail=1
fi

begin
runs 3 main workflow_dispatch owner/repo 2 main schedule owner/repo > "$stub/runs.json"
artifacts 3 results-X64-sha3 false other-ARM64-sha3 false
artifacts 2 results-X64-sha2 false results-ARM64-sha2 false
check "a run without the artifact gives way to the next older run" "$(record 2)" "2 results-ARM64-sha2"

begin
prefix="results-ARM64-2cpu-1gib-"
runs 3 main workflow_dispatch owner/repo 2 main schedule owner/repo > "$stub/runs.json"
artifacts 3 results-ARM64-thrash-sha3 false results-ARM64-4cpu-1gib-sha3 false
artifacts 2 results-ARM64-4cpu-1gib-sha2 false results-ARM64-2cpu-1gib-sha2 false
check "a pod shape takes the newest run that measured that shape" "$(record 2)" "2 results-ARM64-2cpu-1gib-sha2"

begin
runs 3 main push owner/repo 2 main push owner/repo > "$stub/runs.json"
artifacts 3 results-ARM64-sha3 true
artifacts 2 results-ARM64-sha2 false
check "an expired artifact gives way to the next older run" "$(record 2)" "2 results-ARM64-sha2"

begin
runs 4 main pull_request owner/repo 3 main push fork/repo 2 feature push owner/repo 1 main push owner/repo > "$stub/runs.json"
artifacts 4 results-ARM64-sha4 false
artifacts 3 results-ARM64-sha3 false
artifacts 2 results-ARM64-sha2 false
artifacts 1 results-ARM64-sha1 false
check "a pull request, a fork, and another branch never supply the baseline" "$(record 1)" "1 results-ARM64-sha1"

begin
runs 3 main push owner/repo > "$stub/runs.json"
artifacts 3 other-ARM64-sha3 false
check "no run with the artifact is no baseline" \
  "unavailable=None of the last 20 successful runs of bench.yml on main holds an artifact named results-ARM64-*." ""

begin
runs > "$stub/runs.json"
check "no successful run is no baseline" \
  "unavailable=No successful run of bench.yml on main was found." ""

begin
STUB_LIST_FAIL=1 check "a run list the token cannot read is no baseline" \
  "unavailable=The runs of bench.yml on main could not be listed." ""

begin
printf 'not json\n' > "$stub/runs.json"
check "a run list that does not parse is no baseline" \
  "unavailable=The list of the runs of bench.yml on main did not parse." ""

begin
runs 3 main push owner/repo > "$stub/runs.json"
artifacts 3 results-ARM64-sha3 false
STUB_DOWNLOAD_FAIL=1 check "a download the token cannot make is no baseline" \
  "unavailable=The artifact results-ARM64-sha3 of the run https://example.test/runs/3 could not be downloaded." ""

rc=0
GITHUB_REPOSITORY=owner/repo PATH="$work/bin:$PATH" bash "$script" bench.yml > /dev/null 2>&1 || rc=$?
if [ "$rc" -eq 2 ]; then
  printf 'ok   - a missing argument is a usage error\n'
else
  printf 'FAIL - a missing argument is a usage error (exit %s)\n' "$rc"
  fail=1
fi

exit "$fail"
