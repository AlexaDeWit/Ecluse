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

# Assert the verdict for one fixture workflow. $1 name, $2 expected exit, $3 the file
# name the fixture takes (the allow-list keys on it), then the workflow on stdin.
check() {
  local name="$1" want="$2" file="$3" got=0
  rm -rf "$work/wf" && mkdir "$work/wf"
  cat > "$work/wf/$file"
  bash "$script" "$work/wf" >/dev/null 2>&1 || got=$?
  if [ "$got" = "$want" ]; then
    printf 'ok   - %s\n' "$name"
  else
    printf 'FAIL - %s (want exit %s, got %s)\n' "$name" "$want" "$got"
    fail=1
  fi
}

check "a job on the arm64 runner passes" 0 ci.yml <<'YAML'
jobs:
  build:
    runs-on: ubuntu-24.04-arm # the policy runner
    steps: []
YAML

check "a quoted arm64 runner passes" 0 ci.yml <<'YAML'
jobs:
  build:
    runs-on: "ubuntu-24.04-arm"
YAML

check "a job on ubuntu-latest outside the allow-list fails" 1 ci.yml <<'YAML'
jobs:
  build:
    runs-on: ubuntu-latest
YAML

check "a pinned amd64 runner outside the allow-list fails" 1 ci.yml <<'YAML'
jobs:
  build:
    runs-on: ubuntu-24.04
YAML

check "one bad job among good ones fails" 1 ci.yml <<'YAML'
jobs:
  build:
    runs-on: ubuntu-24.04-arm
  docs:
    runs-on: ubuntu-latest
  gate:
    runs-on: ubuntu-24.04-arm
YAML

check "an allow-listed job passes" 0 scorecard.yml <<'YAML'
jobs:
  analysis:
    runs-on: ubuntu-latest
YAML

check "an allow-listed job on another runner fails" 1 scorecard.yml <<'YAML'
jobs:
  analysis:
    runs-on: windows-latest
YAML

check "an allow-listed job id in another workflow fails" 1 ci.yml <<'YAML'
jobs:
  analysis:
    runs-on: ubuntu-latest
YAML

check "a matrix runner with an allow-listed amd64 leg passes" 0 release-build.yml <<'YAML'
jobs:
  build:
    strategy:
      fail-fast: false
      matrix:
        platform:
          - arch: amd64
            runner: ubuntu-latest
          - arch: arm64
            runner: ubuntu-24.04-arm
    runs-on: ${{ matrix.platform.runner }}
YAML

check "a matrix runner with an unlisted amd64 leg fails" 1 ci.yml <<'YAML'
jobs:
  boot:
    runs-on: ${{ matrix.runner }}
    strategy:
      matrix:
        include:
          - runner: ubuntu-24.04-arm
          - runner: ubuntu-latest
YAML

check "a matrix runner with no value for its key fails" 1 ci.yml <<'YAML'
jobs:
  boot:
    runs-on: ${{ matrix.runner }}
    strategy:
      matrix:
        os: [ubuntu-24.04-arm]
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

check "a reusable-workflow call has no runner to check" 0 ci.yml <<'YAML'
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
    runs-on: ubuntu-24.04-arm
    steps:
      - name: Echo
        run: |
          echo "runs-on: ubuntu-latest"
YAML

if bash "$script" "$here/../.github/workflows" >/dev/null 2>&1; then
  printf 'ok   - %s\n' "the repository's own workflows pass"
else
  printf 'FAIL - %s\n' "the repository's own workflows pass"
  fail=1
fi

exit "$fail"
