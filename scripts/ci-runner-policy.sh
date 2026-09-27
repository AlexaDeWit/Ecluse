#!/usr/bin/env bash
# Check every workflow job's runner, for `task lint-workflows`. CI builds and tests on
# arm64, so a job runs on ubuntu-24.04-arm unless the allow-list below names its
# workflow, job, and runner with a reason. A `runs-on: ${{ matrix.<a>[.<b>] }}` job is
# checked against every value its matrix and its include entries give that path.
# Anything the script cannot resolve fails. A job that calls a local reusable workflow
# is checked in that workflow, and a remote one fails unless allow-listed.
#
# yq turns each workflow into JSON and jq decides. Needs yq-go and jq (the dev shells).
#
# Usage: scripts/ci-runner-policy.sh [workflow-dir]   (default .github/workflows)
set -euo pipefail

dir="${1:-.github/workflows}"
policy_runner="ubuntu-24.04-arm"

# <workflow file>:<job id>:<runner or remote workflow>|<reason>
allowed=(
  "scorecard.yml:analysis:ubuntu-latest|ossf/scorecard-action ships only a linux/amd64 image."
  "release-build.yml:build:ubuntu-latest|The amd64 release image builds natively on amd64."
  "ci.yml:release-dry-run-boot:ubuntu-latest|The amd64 release image starts on its own architecture."
  "release.yml:verify-version:ubuntu-latest|Builds and tests no code, and only a publishing run can prove a runner change."
  "release.yml:publish:ubuntu-latest|Builds and tests no code, and only a publishing run can prove a runner change."
)

# Emits one "<job>\t<kind>\t<value>" row per runner a job can take ("runner"), per local
# or remote workflow it calls ("local", "uses"), and per reason it cannot resolve ("error").
decide="$(cat <<'JQ'
def row($job; $kind; $value): [$job, $kind, $value] | @tsv;
def fail($job; $why): row($job; "error"; $why);
def holds_expr: any(.. ; type == "string" and test("\\$\\{\\{"));
def string_or_error($what): if type == "string" then . else {error: "\($what) is not a string"} end;
def path_name($a; $b): "matrix.\($a)" + (if $b == null then "" else ".\($b)" end);

# The values one matrix path takes in the dimension named $a, as strings or {error}.
def dimension_values($m; $a; $b):
  if ($m | has($a) | not) then empty
  elif ($m[$a] | type) != "array" then {error: "matrix.\($a) is not a list"}
  else $m[$a][]
    | if $b == null then string_or_error("an item of matrix.\($a)")
      elif type == "object" and has($b) then .[$b] | string_or_error(path_name($a; $b))
      else {error: "an item of matrix.\($a) lacks \($b)"}
      end
  end;

# An include entry can add a combination with any value, so each one must give the path.
def include_values($m; $a; $b):
  if ($m | has("include") | not) then empty
  elif ($m.include | type) != "array" then {error: "matrix.include is not a list"}
  else $m.include[]
    | if type != "object" then {error: "an include entry is not a mapping"}
      elif has($a) | not then {error: "an include entry lacks \(path_name($a; $b))"}
      elif $b == null then .[$a] | string_or_error("an include entry's \($a)")
      elif (.[$a] | type) == "object" and (.[$a] | has($b)) then .[$a][$b] | string_or_error("an include entry's \($a).\($b)")
      else {error: "an include entry lacks \(path_name($a; $b))"}
      end
  end;

def matrix_rows($job; $j; $a; $b):
  if ($j.strategy | type) != "object" or ($j.strategy.matrix | type) != "object" then
    fail($job; "runs-on reads \(path_name($a; $b)), but the job has no matrix mapping")
  else
    [dimension_values($j.strategy.matrix; $a; $b), include_values($j.strategy.matrix; $a; $b)] as $values
    | if ($values | length) == 0 then fail($job; "no matrix value for \(path_name($a; $b))")
      else $values[] | if type == "string" then row($job; "runner"; .) else fail($job; .error) end
      end
  end;

def runner_rows($job; $j; $spec):
  if ($spec | type) == "string" then
    if ($spec | test("\\$\\{\\{") | not) then row($job; "runner"; $spec)
    else
      ([$spec | capture("^\\$\\{\\{\\s*matrix\\.(?<a>[A-Za-z0-9_-]+)(\\.(?<b>[A-Za-z0-9_-]+))?\\s*\\}\\}$")] | first) as $p
      | if $p == null then fail($job; "unresolvable runs-on expression \($spec)")
        else matrix_rows($job; $j; $p.a; $p.b)
        end
    end
  elif ($spec | type) == "array" then
    if ($spec | length) == 0 then fail($job; "runs-on is an empty list")
    else $spec[] as $label | runner_rows($job; $j; $label)
    end
  elif ($spec | type) == "object" then
    if ($spec | has("group")) then fail($job; "runs-on names a runner group, which the check cannot resolve")
    elif ($spec | has("labels")) then runner_rows($job; $j; $spec.labels)
    else fail($job; "runs-on is a mapping without labels")
    end
  else fail($job; "runs-on is not a string, list, or mapping")
  end;

def job_rows($job; $j):
  if ($j | type) != "object" then fail($job; "the job is not a mapping")
  else
    ( if ($j | has("strategy") | not) then empty
      elif ($j.strategy | type) != "object" then fail($job; "the strategy is an expression")
      elif ($j.strategy | has("matrix") | not) then empty
      elif ($j.strategy.matrix | type) != "object" then fail($job; "the matrix is an expression")
      elif ($j.strategy.matrix | holds_expr) then fail($job; "the matrix holds an expression")
      else empty
      end ),
    ( if ($j | has("uses")) then
        if ($j.uses | type) == "string" and ($j.uses | startswith("./")) then row($job; "local"; $j.uses)
        else row($job; "uses"; $j.uses | tostring)
        end
      else empty
      end ),
    ( if ($j | has("runs-on")) then runner_rows($job; $j; $j["runs-on"])
      elif ($j | has("uses")) then empty
      else fail($job; "no runs-on")
      end )
  end;

if type != "object" or (.jobs | type) != "object" or (.jobs | length) == 0 then fail("(file)"; "no jobs found")
else .jobs | to_entries[] | job_rows(.key; .value)
end
JQ
)"

reason_for() {
  local key="$1" entry
  for entry in "${allowed[@]}"; do
    if [ "${entry%%|*}" = "$key" ]; then
      printf '%s\n' "${entry#*|}"
      return 0
    fi
  done
  return 1
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
  if ! json="$(yq -o=json 'explode(.)' "$path" 2>&1)"; then
    echo "FAILED  $file: yq cannot parse it: $json"
    verdict=1
    continue
  fi
  rows="$(printf '%s\n' "$json" | jq -r "$decide")"
  if [ -z "$rows" ]; then
    echo "FAILED  $file (file): no jobs found"
    verdict=1
    continue
  fi
  while IFS=$'\t' read -r job kind value; do
    case "$kind" in
      runner)
        if [ "$value" = "$policy_runner" ]; then
          echo "ok      $file $job: $value"
        elif reason="$(reason_for "$file:$job:$value")"; then
          echo "ok      $file $job: $value (allow-listed: $reason)"
        else
          echo "FAILED  $file $job: $value is not $policy_runner and not allow-listed"
          verdict=1
        fi
        ;;
      local)
        echo "ok      $file $job: calls $value, checked as its own workflow"
        ;;
      uses)
        if reason="$(reason_for "$file:$job:$value")"; then
          echo "ok      $file $job: calls $value (allow-listed: $reason)"
        else
          echo "FAILED  $file $job: calls remote workflow $value, which is not allow-listed"
          verdict=1
        fi
        ;;
      *)
        echo "FAILED  $file $job: $value"
        verdict=1
        ;;
    esac
  done <<< "$rows"
done

exit "$verdict"
