#!/usr/bin/env bash
# Check every workflow job's runner, for `task lint-workflows`. CI builds and tests on
# arm64, so a job runs on ubuntu-24.04-arm unless the allow-list below names its
# workflow, job, and runner with a reason. A `runs-on: ${{ matrix.<key> }}` job is
# checked against every value its strategy gives that key. A runner the script cannot
# resolve fails, and a job that calls a reusable workflow is checked in that workflow.
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

# Emit "<job>\t<runner>" for each runner a job can take, "<job>\t?<reason>" when the
# runner cannot be resolved, and nothing for a reusable-workflow call. Reads the block
# layout every workflow here uses: job ids at two spaces, job keys at four.
runners_of() {
  awk -v quotes="[\"']" '
    function unquote(v) {
      sub(/[[:space:]]+#.*$/, "", v)
      gsub(/^[[:space:]]+|[[:space:]]+$/, "", v)
      gsub("^" quotes "|" quotes "$", "", v)
      return v
    }
    function flush(   key, n, parts, i, found) {
      if (job == "" || reusable) { job = ""; return }
      if (runs_on == "") { print job "\t?no runs-on"; job = ""; return }
      if (runs_on ~ /^\$\{\{[[:space:]]*matrix\.[A-Za-z0-9_.-]+[[:space:]]*\}\}$/) {
        key = runs_on
        sub(/^\$\{\{[[:space:]]*/, "", key); sub(/[[:space:]]*\}\}$/, "", key)
        n = split(key, parts, ".")
        key = parts[n]
        found = 0
        for (i = 1; i <= nvals; i++) {
          if (vkey[i] == key) { print job "\t" vval[i]; found = 1 }
        }
        if (!found) print job "\t?no matrix value for " runs_on
      } else if (runs_on ~ /\$\{\{/) {
        print job "\t?unresolvable expression " runs_on
      } else {
        print job "\t" runs_on
      }
      job = ""
    }
    /^[^[:space:]#]/ { flush(); in_jobs = ($0 ~ /^jobs:[[:space:]]*$/); next }
    !in_jobs { next }
    /^  [A-Za-z0-9_-]+:[[:space:]]*$/ {
      flush()
      job = $0; sub(/^  /, "", job); sub(/:.*$/, "", job)
      runs_on = ""; reusable = 0; in_strategy = 0; nvals = 0
      next
    }
    job == "" { next }
    /^    runs-on:/ {
      runs_on = $0; sub(/^    runs-on:/, "", runs_on); runs_on = unquote(runs_on)
      if (runs_on == "") runs_on = "?block"
      next
    }
    /^    uses:/ { reusable = 1; next }
    /^    strategy:/ { in_strategy = 1; next }
    /^    [A-Za-z0-9_-]+:/ { in_strategy = 0; next }
    in_strategy && /^      [[:space:]]*(- )?[A-Za-z0-9_-]+:[[:space:]]*[^[:space:]]/ {
      line = $0
      sub(/^[[:space:]]*(- )?/, "", line)
      k = line; sub(/:.*$/, "", k)
      v = line; sub(/^[^:]*:/, "", v)
      nvals++; vkey[nvals] = k; vval[nvals] = unquote(v)
    }
    END { flush() }
  ' "$1"
}

verdict=0
shopt -s nullglob
for path in "$dir"/*.yml "$dir"/*.yaml; do
  file="$(basename "$path")"
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
  done < <(runners_of "$path")
done

exit "$verdict"
