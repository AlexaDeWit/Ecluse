#!/usr/bin/env bash
# Write the success floors in bench/load/floors.json from three calibration runs of the Load test
# workflow. For each run, saves `gh run view`'s record and downloads its bench-load-results
# artifacts, then calls `bench-report floors`, which sets each selected cell that is not flagged
# report-only to the file's floorFraction of the median of its three first-attempt counts.
#   load-floors.sh <run-id>,<run-id>,<run-id> [<scenario>,...]
# A scenario list recalibrates only those scenarios' cells. The downloads stay in a new directory
# under scratchpad/load-floors/ for review. Needs gh, and cabal from the dev shell.
set -euo pipefail

usage() {
  echo "usage: load-floors.sh <run-id>,<run-id>,<run-id> [<scenario>,...]" >&2
  exit 2
}

[ "$#" -ge 1 ] && [ "$#" -le 2 ] || usage
IFS=, read -r -a runs <<< "$1"
[ "${#runs[@]}" -eq 3 ] || usage
for id in "${runs[@]}"; do
  case "$id" in '' | *[!0-9]*) usage ;; esac
done

mkdir -p scratchpad/load-floors
work="$(mktemp -d scratchpad/load-floors/runs.XXXXXX)"
for id in "${runs[@]}"; do
  mkdir "$work/$id"
  gh run view "$id" --json databaseId,url,headSha,event > "$work/$id/run.json"
  gh run download "$id" --pattern 'bench-load-results-*' --dir "$work/$id"
done
echo "load-floors.sh: the runs' records and artifacts are in $work" >&2

# The reporter is a command line, so a test can stand in for the build.
read -r -a reporter <<< "${LOAD_FLOORS_REPORTER:-cabal run -v0 exe:bench-report --}"
"${reporter[@]}" floors bench/load/floors.json "$work" ${2:+"$2"}
