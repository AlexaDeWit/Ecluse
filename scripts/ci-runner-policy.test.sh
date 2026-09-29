#!/usr/bin/env bash
# Deterministic unit test for ci-runner-policy.sh over fixture workflows. The case that
# must never pass is a job that builds or tests off arm64 without an allow-list entry.
# Run via `task test-scripts`.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
script="$here/ci-runner-policy.sh"

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

fail=0
mkdir "$work/wf"

# Add a further workflow to the next check's directory. $1 the file name, then stdin.
fixture() {
  cat > "$work/wf/$1"
}

# Assert the verdict over file $3 (from stdin) and any fixture() files, then clear them.
# $1 name, $2 expected exit, $4 optional text the output must contain.
check() {
  local name="$1" want="$2" file="$3" text="${4:-}" got=0 out
  cat > "$work/wf/$file"
  out="$(bash "$script" "$work/wf" 2>&1)" || got=$?
  rm -rf "$work/wf" && mkdir "$work/wf"
  if [ "$got" = "$want" ] && { [ -z "$text" ] || grep -qF -- "$text" <<< "$out"; }; then
    printf 'ok   - %s\n' "$name"
  else
    printf 'FAIL - %s (want exit %s and "%s", got %s)\n' "$name" "$want" "$text" "$got"
    fail=1
  fi
}

check "a job on the arm64 runner passes" 0 ci.yml <<'YAML'
jobs:
  build:
    runs-on: ubuntu-26.04-arm # the policy runner
    steps: []
YAML

check "a quoted arm64 runner passes" 0 ci.yml <<'YAML'
jobs:
  build:
    runs-on: "ubuntu-26.04-arm"
YAML

check "a job on ubuntu-latest outside the allow-list fails" 1 ci.yml <<'YAML'
jobs:
  build:
    runs-on: ubuntu-latest
YAML

check "a pinned amd64 runner outside the allow-list fails" 1 ci.yml <<'YAML'
jobs:
  build:
    runs-on: ubuntu-26.04
YAML

check "one bad job among good ones fails" 1 ci.yml <<'YAML'
jobs:
  build:
    runs-on: ubuntu-26.04-arm
  docs:
    runs-on: ubuntu-latest
  gate:
    runs-on: ubuntu-26.04-arm
YAML

check "an allow-listed job passes" 0 scorecard.yml <<'YAML'
jobs:
  analysis:
    runs-on: ubuntu-26.04
YAML

check "an allow-listed job on another runner fails" 1 scorecard.yml <<'YAML'
jobs:
  analysis:
    runs-on: windows-latest
YAML

check "an allow-listed job on ubuntu-latest fails" 1 scorecard.yml <<'YAML'
jobs:
  analysis:
    runs-on: ubuntu-latest
YAML

check "a job on an earlier arm64 image fails" 1 ci.yml <<'YAML'
jobs:
  build:
    runs-on: ubuntu-24.04-arm
YAML

check "an allow-listed job id in another workflow fails" 1 ci.yml <<'YAML'
jobs:
  analysis:
    runs-on: ubuntu-26.04
YAML

check "a matrix runner with an allow-listed amd64 leg passes" 0 release-build.yml <<'YAML'
jobs:
  build:
    strategy:
      fail-fast: false
      matrix:
        platform:
          - arch: amd64
            runner: ubuntu-26.04
          - arch: arm64
            runner: ubuntu-26.04-arm
    runs-on: ${{ matrix.platform.runner }}
YAML

check "a matrix runner with an unlisted amd64 leg fails" 1 ci.yml <<'YAML'
jobs:
  boot:
    runs-on: ${{ matrix.runner }}
    strategy:
      matrix:
        include:
          - runner: ubuntu-26.04-arm
          - runner: ubuntu-26.04
YAML

check "a matrix runner with no value for its key fails" 1 ci.yml <<'YAML'
jobs:
  boot:
    runs-on: ${{ matrix.runner }}
    strategy:
      matrix:
        os: [ubuntu-26.04-arm]
YAML

check "a runner expression outside the matrix fails" 1 ci.yml <<'YAML'
jobs:
  build:
    runs-on: ${{ inputs.runner }}
YAML

check "a runs-on block fails" 1 ci.yml <<'YAML'
jobs:
  build:
    runs-on:
      group: large
YAML

check "a job without runs-on fails" 1 ci.yml <<'YAML'
jobs:
  build:
    steps: []
YAML

fixture release-build.yml <<'YAML'
jobs:
  build:
    runs-on: ubuntu-26.04-arm
YAML
check "a call to a local workflow this run checks passes" 0 ci.yml <<'YAML'
jobs:
  dry-run:
    uses: ./.github/workflows/release-build.yml
    with:
      retention-days: 1
YAML

check "a key named runs-on inside a step is not a job runner" 0 ci.yml <<'YAML'
on:
  push:
jobs:
  build:
    runs-on: ubuntu-26.04-arm
    steps:
      - name: Echo
        run: |
          echo "runs-on: ubuntu-latest"
YAML

check "a job id with a trailing comment is its own job" 1 ci.yml <<'YAML'
jobs:
  build:
    runs-on: ubuntu-latest
  arm: # the next runs-on belongs to this job, not to build
    runs-on: ubuntu-26.04-arm
YAML

check "a commented job id on the arm64 runner passes" 0 ci.yml <<'YAML'
jobs: # every job
  arm: # a note
    runs-on: ubuntu-26.04-arm # the policy runner
YAML

check "a flow-mapping job is checked like any other" 1 ci.yml <<'YAML'
jobs:
  build: { runs-on: ubuntu-latest }
YAML

check "a quoted job id is checked like any other" 1 ci.yml <<'YAML'
jobs:
  "build":
    runs-on: ubuntu-latest
YAML

check "a workflow with no jobs fails" 1 ci.yml <<'YAML'
on:
  push:
YAML

check "a matrix path resolves its own dimension, not another key of the same name" 1 ci.yml <<'YAML'
jobs:
  build:
    runs-on: ${{ matrix.platform.runner }}
    strategy:
      matrix:
        platform:
          - host: ubuntu-latest
        include:
          - runner: ubuntu-26.04-arm
YAML

check "a matrix item without the path's key fails" 1 ci.yml <<'YAML'
jobs:
  build:
    runs-on: ${{ matrix.platform.runner }}
    strategy:
      matrix:
        platform:
          - runner: ubuntu-26.04-arm
          - arch: amd64
YAML

check "a matrix holding an expression fails" 1 ci.yml <<'YAML'
jobs:
  build:
    runs-on: ${{ matrix.runner }}
    strategy:
      matrix:
        runner:
          - ubuntu-26.04-arm
        include: ${{ fromJSON(inputs.extra) }}
YAML

check "a matrix that is an expression fails" 1 ci.yml <<'YAML'
jobs:
  build:
    runs-on: ${{ matrix.runner }}
    strategy:
      matrix: ${{ fromJSON(needs.plan.outputs.matrix) }}
YAML

check "a scalar matrix list on the arm64 runner passes" 0 ci.yml <<'YAML'
jobs:
  build:
    runs-on: ${{ matrix.runner }}
    strategy:
      matrix:
        runner:
          - ubuntu-26.04-arm
YAML

check "a flow matrix list with an unlisted runner fails" 1 ci.yml <<'YAML'
jobs:
  build:
    runs-on: ${{ matrix.runner }}
    strategy:
      matrix:
        runner: [ubuntu-26.04-arm, ubuntu-latest]
YAML

check "an include entry that adds an unlisted runner fails" 1 ci.yml <<'YAML'
jobs:
  build:
    runs-on: ${{ matrix.runner }}
    strategy:
      matrix:
        runner:
        - ubuntu-latest
        include:
          - runner: ubuntu-26.04-arm
YAML

check "a flow-mapping include entry with an unlisted runner fails" 1 ci.yml <<'YAML'
jobs:
  build:
    runs-on: ${{ matrix.runner }}
    strategy:
      matrix:
        runner: [ubuntu-26.04-arm]
        include:
          - {runner: ubuntu-latest}
YAML

check "an include entry that sets a two-segment path fails when unlisted" 1 ci.yml <<'YAML'
jobs:
  build:
    runs-on: ${{ matrix.platform.runner }}
    strategy:
      matrix:
        platform:
          - runner: ubuntu-26.04-arm
        include:
          - platform:
              runner: ubuntu-latest
YAML

check "a nested key of the same name does not stand in for the path" 1 ci.yml <<'YAML'
jobs:
  build:
    runs-on: ${{ matrix.platform.runner }}
    strategy:
      matrix:
        platform:
        - arch: amd64
          runner: ubuntu-latest
          meta:
            runner: ubuntu-26.04-arm
YAML

check "a call to a remote reusable workflow fails" 1 ci.yml <<'YAML'
jobs:
  build:
    uses: someone/else/.github/workflows/build.yml@0123456789abcdef0123456789abcdef01234567
YAML

check "a runs-on list with an unlisted label fails" 1 ci.yml <<'YAML'
jobs:
  build:
    runs-on: [ubuntu-26.04-arm, ubuntu-latest]
YAML

check "a runs-on list of the arm64 runner passes" 0 ci.yml <<'YAML'
jobs:
  build:
    runs-on: [ubuntu-26.04-arm]
YAML

check "a runner group fails" 1 ci.yml <<'YAML'
jobs:
  build:
    runs-on:
      group: arm-runners
      labels: [ubuntu-26.04-arm]
YAML

check "a labels mapping with an unlisted label fails" 1 ci.yml <<'YAML'
jobs:
  build:
    runs-on:
      labels: ubuntu-latest
YAML

check "an anchored runner resolves through its alias" 1 ci.yml <<'YAML'
x-runner: &runner ubuntu-latest
jobs:
  build:
    runs-on: *runner
YAML

check "an anchored arm64 runner resolves through its alias" 0 ci.yml <<'YAML'
jobs:
  first:
    runs-on: &runner ubuntu-26.04-arm
  second:
    runs-on: *runner
YAML

check "a YAML merge key fails by name" 1 ci.yml "uses a YAML merge key" <<'YAML'
x-base: &base
  runs-on: ubuntu-26.04-arm
jobs:
  build:
    <<: *base
YAML

fixture z.yml <<'YAML'
jobs:
  build:
    runs-on: ubuntu-latest
YAML
check "a merge-key file does not stop the files after it" 1 ci.yml "FAILED  z.yml build" <<'YAML'
x-base: &base
  runs-on: ubuntu-26.04-arm
jobs:
  build:
    <<: *base
YAML

check "a dotfile workflow off arm64 fails" 1 .evil.yml "FAILED  .evil.yml build" <<'YAML'
jobs:
  build:
    runs-on: ubuntu-latest
YAML

check "an upper-case extension off arm64 fails" 1 build.YML "FAILED  build.YML build" <<'YAML'
jobs:
  build:
    runs-on: ubuntu-latest
YAML

fixture .callee.yml <<'YAML'
jobs:
  build:
    runs-on: ubuntu-latest
YAML
check "a local call to a dotfile callee off arm64 fails" 1 ci.yml "FAILED  .callee.yml build" <<'YAML'
jobs:
  call:
    uses: ./.github/workflows/.callee.yml
YAML

check "a local call to a missing workflow fails" 1 ci.yml "which is not a workflow file this run reads" <<'YAML'
jobs:
  call:
    uses: ./.github/workflows/absent.yml
YAML

fixture callee.yml <<'YAML'
jobs:
  build:
    runs-on: ubuntu-26.04-arm
YAML
check "a local call into a subdirectory fails" 1 ci.yml "which is not a workflow file this run reads" <<'YAML'
jobs:
  call:
    uses: ./.github/workflows/sub/callee.yml
YAML

fixture callee.YML <<'YAML'
jobs:
  build:
    runs-on: ubuntu-26.04-arm
YAML
check "a local call whose name differs in case from the file fails" 1 ci.yml "which is not a workflow file this run reads" <<'YAML'
jobs:
  call:
    uses: ./.github/workflows/callee.yml
YAML

check "a literal arm64 runner passes beside a matrix expression" 0 ci.yml <<'YAML'
jobs:
  build:
    runs-on: ubuntu-26.04-arm
    strategy:
      matrix: ${{ fromJSON(needs.plan.outputs.matrix) }}
YAML

check "a matrix path matches its dimension ignoring case" 1 ci.yml <<'YAML'
jobs:
  build:
    runs-on: ${{ MATRIX.Runner }}
    strategy:
      matrix:
        runner: [ubuntu-latest]
YAML

check "a matrix path in another case resolves to an arm64 runner" 0 ci.yml <<'YAML'
jobs:
  build:
    runs-on: ${{ matrix.Platform.Runner }}
    strategy:
      matrix:
        platform:
          - runner: ubuntu-26.04-arm
YAML

check "a workflow yq cannot parse fails" 1 ci.yml <<'YAML'
jobs:
  build:
    runs-on: [ubuntu-26.04-arm
YAML

check "an empty workflow file fails" 1 ci.yml < /dev/null

mkdir -p "$work/empty"
if bash "$script" "$work/empty" >/dev/null 2>&1; then
  printf 'FAIL - %s\n' "an empty workflow directory fails"
  fail=1
else
  printf 'ok   - %s\n' "an empty workflow directory fails"
fi

if bash "$script" "$work/missing" >/dev/null 2>&1; then
  printf 'FAIL - %s\n' "a missing workflow directory fails"
  fail=1
else
  printf 'ok   - %s\n' "a missing workflow directory fails"
fi

if bash "$script" "$here/../.github/workflows" >/dev/null 2>&1; then
  printf 'ok   - %s\n' "the repository's own workflows pass"
else
  printf 'FAIL - %s\n' "the repository's own workflows pass"
  fail=1
fi

exit "$fail"
