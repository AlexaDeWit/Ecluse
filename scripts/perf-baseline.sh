#!/usr/bin/env bash
# Fetch the baseline of a performance comparison for the three benchmark workflows: the
# results of the most recent successful run of one workflow on main.
#
#   Usage: scripts/perf-baseline.sh <workflow-file> <artifact-prefix> <out-dir>
#
# <out-dir>/baseline.txt gets `commit=`, `run=`, and `created=` lines beside the artifact's
# files, or one `unavailable=<code>` line that bench-report renders. A missing baseline exits
# 0: only a usage error fails. Reads GITHUB_REPOSITORY and GH_TOKEN. Needs gh and jq.
set -euo pipefail

if [ "$#" -ne 3 ]; then
  echo "usage: perf-baseline.sh <workflow-file> <artifact-prefix> <out-dir>" >&2
  exit 2
fi
workflow="$1"
prefix="$2"
out="$3"
repo="${GITHUB_REPOSITORY:?GITHUB_REPOSITORY must name the repository}"

mkdir -p "$out"

# $1 is the code the record carries, and $2 the detail for this job's log.
none() {
  echo "perf-baseline: no baseline ($1). $2"
  printf 'unavailable=%s\n' "$1" > "$out/baseline.txt"
  exit 0
}

# One call lists the 100 newest successful runs, the most a page holds.
listing="$(gh api "repos/$repo/actions/workflows/$workflow/runs?branch=main&status=success&per_page=100" < /dev/null)" \
  || none runs-not-listed "The runs of $workflow on main could not be listed."

# The branch filter alone also matches a pull request from a fork's own main branch, so a
# run counts only when this repository started it on main by a push, a schedule, or a dispatch.
runs="$(printf '%s' "$listing" | jq -r --arg repo "$repo" '
  .workflow_runs[]
  | select(.head_branch == "main" and .head_repository.full_name == $repo)
  | select(.event | IN("push", "schedule", "workflow_dispatch"))
  | [.id, .head_sha, .html_url, .created_at]
  | @tsv')" || none run-list-not-parsed "The list of the runs of $workflow on main did not parse."
[ -n "$runs" ] || none no-successful-run "No successful run of $workflow on main was found."

# Newest first. A dispatched run can hold fewer artifacts than a scheduled one, so the
# search goes on past a run without this one.
while IFS=$'\t' read -r id sha url created; do
  artifacts="$(gh api "repos/$repo/actions/runs/$id/artifacts?per_page=100" < /dev/null)" || continue
  name="$(printf '%s' "$artifacts" | jq -r --arg prefix "$prefix" '
    [.artifacts[] | select(.expired | not) | .name | select(startswith($prefix))][0] // empty')" || continue
  [ -n "$name" ] || continue

  gh run download "$id" --repo "$repo" --name "$name" --dir "$out" < /dev/null \
    || none download-failed "The artifact $name of the run $url could not be downloaded."
  printf 'commit=%s\nrun=%s\ncreated=%s\n' "$sha" "$url" "$created" > "$out/baseline.txt"
  echo "perf-baseline: $name from $url"
  exit 0
done <<< "$runs"

none no-run-with-artifact "No listed run of $workflow on main holds an artifact named $prefix*."
