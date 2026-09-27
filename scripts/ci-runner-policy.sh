#!/usr/bin/env bash
# Check every workflow job's runner, for `task lint-workflows`. CI builds and tests on
# arm64, so a job runs on ubuntu-24.04-arm unless the allow-list below names its
# workflow, job, and runner with a reason. A `runs-on: ${{ matrix.<dim>[.<key>] }}` job
# is checked against every value its matrix gives that path. Anything the script cannot
# resolve fails. A job that calls a reusable workflow is checked in that workflow.
#
# Usage: scripts/ci-runner-policy.sh [workflow-dir]   (default .github/workflows)
set -euo pipefail

dir="${1:-.github/workflows}"
policy_runner="ubuntu-24.04-arm"

# <workflow file>:<job id>:<runner>|<reason>
allowed=(
  "scorecard.yml:analysis:ubuntu-latest|ossf/scorecard-action ships only a linux/amd64 image."
  "release-build.yml:build:ubuntu-latest|The amd64 release image builds natively on amd64."
  "ci.yml:release-dry-run-boot:ubuntu-latest|The amd64 release image starts on its own architecture."
  "release.yml:verify-version:ubuntu-latest|Builds and tests no code, and only a publishing run can prove a runner change."
  "release.yml:publish:ubuntu-latest|Builds and tests no code, and only a publishing run can prove a runner change."
)

# Emit "<job>\t<runner>" per runner a job can take, or "<job>\t?<reason>" when unresolved.
# Expects job ids at two spaces, job keys at four, and matrix dimensions at eight.
runners_of() {
  awk -v quotes="[\"']" '
    function unquote(v) {
      sub(/[[:space:]]+#.*$/, "", v)
      gsub(/^[[:space:]]+|[[:space:]]+$/, "", v)
      gsub("^" quotes "|" quotes "$", "", v)
      return v
    }
    # One matrix entry: its dimension, item number, and key ("" for a scalar item).
    function record(d, i, k, v) {
      v = unquote(v)
      if (v ~ /\$\{\{/) matrix_expr = 1
      if (!((d, i, k) in mval)) { mval[d, i, k] = v; nent++; ed[nent] = d; ei[nent] = i; ek[nent] = k }
    }
    function resolve(path,   n, seg, j, found, items, hits) {
      n = split(path, seg, ".")
      if (matrix_expr) { print job "\t?the matrix holds an expression"; return }
      found = 0
      if (n == 1) {
        for (j = 1; j <= nent; j++) {
          if ((ed[j] == seg[1] && ek[j] == "") || (ed[j] == "include" && ek[j] == seg[1])) {
            print job "\t" mval[ed[j], ei[j], ek[j]]; found = 1
          }
        }
      } else if (n == 2) {
        items = nitems[seg[1]] + 0; hits = 0
        for (j = 1; j <= nent; j++) {
          if (ed[j] == seg[1] && ek[j] == seg[2]) { print job "\t" mval[ed[j], ei[j], ek[j]]; hits++ }
        }
        if (hits > 0 && hits < items) print job "\t?an item of matrix." seg[1] " lacks " seg[2]
        found = hits > 0
      } else {
        print job "\t?unresolvable matrix path matrix." path
        return
      }
      if (!found) print job "\t?no matrix value for matrix." path
    }
    function flush(   path) {
      if (job == "" || reusable) { job = ""; return }
      if (runs_on == "") { print job "\t?no runs-on"; job = ""; return }
      if (runs_on ~ /^\$\{\{[[:space:]]*matrix\.[A-Za-z0-9_.-]+[[:space:]]*\}\}$/) {
        path = runs_on
        sub(/^\$\{\{[[:space:]]*matrix\./, "", path); sub(/[[:space:]]*\}\}$/, "", path)
        resolve(path)
      } else if (runs_on ~ /\$\{\{/) {
        print job "\t?unresolvable expression " runs_on
      } else {
        print job "\t" runs_on
      }
      job = ""
    }
    function start_job(line) {
      flush()
      job = line; sub(/^  /, "", job); sub(/:.*$/, "", job)
      seen_jobs = 1
      runs_on = ""; reusable = 0; in_strategy = 0; in_matrix = 0; matrix_expr = 0
      dim = ""; nent = 0; delete mval; delete nitems
    }
    /^[^[:space:]#]/ { flush(); in_jobs = ($0 ~ /^jobs:[[:space:]]*(#.*)?$/); next }
    !in_jobs || /^[[:space:]]*(#.*)?$/ { next }
    /^  [A-Za-z0-9_-]+:[[:space:]]*(#.*)?$/ { start_job($0); next }
    /^ [^ ]|^  [^ ]|^   [^ ]/ { flush(); print "(jobs)\t?unrecognised line under jobs: " $0; next }
    job == "" { next }
    /^    runs-on:/ {
      runs_on = $0; sub(/^    runs-on:/, "", runs_on); runs_on = unquote(runs_on)
      if (runs_on == "") runs_on = "?block"
      next
    }
    /^    uses:/ { reusable = 1; next }
    /^    strategy:/ { in_strategy = 1; in_matrix = 0; next }
    /^    [^ ]/ { in_strategy = 0; in_matrix = 0; next }
    !in_strategy { next }
    /\$\{\{/ { matrix_expr = 1 }
    /^      matrix:[[:space:]]*(#.*)?$/ { in_matrix = 1; next }
    /^      matrix:/ { in_matrix = 1; matrix_expr = 1; next }
    /^      [^ ]/ { in_matrix = 0; next }
    !in_matrix { next }
    /^        [A-Za-z0-9_-]+:/ {
      line = $0; sub(/^ +/, "", line)
      dim = line; sub(/:.*$/, "", dim)
      v = line; sub(/^[^:]*:/, "", v); v = unquote(v)
      if (v ~ /^\[.*\]$/) {
        v = substr(v, 2, length(v) - 2)
        n = split(v, parts, ",")
        for (j = 1; j <= n; j++) { nitems[dim]++; record(dim, nitems[dim], "", parts[j]) }
      } else if (v != "") {
        matrix_expr = 1
      }
      next
    }
    /^          - / {
      nitems[dim]++
      line = $0; sub(/^          - /, "", line)
      if (line ~ /^[A-Za-z0-9_-]+:/) {
        k = line; sub(/:.*$/, "", k)
        v = line; sub(/^[^:]*:/, "", v)
        record(dim, nitems[dim], k, v)
      } else {
        record(dim, nitems[dim], "", line)
      }
      next
    }
    /^            [A-Za-z0-9_-]+:/ {
      line = $0; sub(/^ +/, "", line)
      k = line; sub(/:.*$/, "", k)
      v = line; sub(/^[^:]*:/, "", v)
      record(dim, nitems[dim], k, v)
      next
    }
    END { flush(); if (!seen_jobs) print "(file)\t?no jobs found" }
  ' "$1"
}

verdict=0
shopt -s nullglob
paths=("$dir"/*.yml "$dir"/*.yaml)
if [ "${#paths[@]}" -eq 0 ]; then
  echo "FAILED  no workflow files in $dir"
  exit 1
fi
for path in "${paths[@]}"; do
  file="$(basename "$path")"
  runners="$(runners_of "$path")"
  while IFS=$'\t' read -r job runner; do
    [ -n "$job" ] || continue
    if [ "${runner#\?}" != "$runner" ]; then
      echo "FAILED  $file $job: ${runner#\?}"
      verdict=1
      continue
    fi
    if [ "$runner" = "$policy_runner" ]; then
      echo "ok      $file $job: $runner"
      continue
    fi
    reason=""
    for entry in "${allowed[@]}"; do
      if [ "${entry%%|*}" = "$file:$job:$runner" ]; then
        reason="${entry#*|}"
      fi
    done
    if [ -n "$reason" ]; then
      echo "ok      $file $job: $runner (allow-listed: $reason)"
    else
      echo "FAILED  $file $job: $runner is not $policy_runner and not allow-listed"
      verdict=1
    fi
  done <<< "$runners"
done

exit "$verdict"
