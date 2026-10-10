#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
#
# SPDX-License-Identifier: MIT
set -euo pipefail

: "${PROFILE_REF:?Set the full source commit}"
: "${PROFILE_OUTPUT:?Set a fresh absolute artifact directory}"
: "${RUNNER_TEMP:?The CI runner supplies its temporary directory}"
[[ "$PROFILE_REF" =~ ^[0-9a-f]{40}$ ]]
[[ "$PROFILE_OUTPUT" == /* ]]
readonly packages=(numpy typescript)
readonly profile_seconds=60
readonly profiling_stdev=1e-12
readonly source_dir="$RUNNER_TEMP/json-profile-source"

mkdir -- "$PROFILE_OUTPUT"
git fetch --no-tags --depth=1 origin "$PROFILE_REF"
git worktree add --detach "$source_dir" "$PROFILE_REF"
cd -- "$source_dir"
git rev-parse HEAD > "$PROFILE_OUTPUT/source-commit.txt"
git ls-tree -r HEAD -- core/src/Ecluse/Core/Registry vendor/json-stream bench cabal.project cabal.project.freeze flake.nix flake.lock > "$PROFILE_OUTPUT/source-tree.txt"
lscpu > "$PROFILE_OUTPUT/cpu.txt"

for package in "${packages[@]}"; do
  options="-p \"(/$package/ && /full metadata projection/)\" --stdev $profiling_stdev --timeout ${profile_seconds}s"
  status=0
  env -u IN_NIX_SHELL nix develop .#ci --command task bench-profile "BENCH_PROFILE_OPTS=$options" \
    > "$PROFILE_OUTPUT/$package.log" 2>&1 || status=$?
  for suffix in prof svg; do
    if [[ -f "ecluse-bench.$suffix" ]]; then cp -- "ecluse-bench.$suffix" "$PROFILE_OUTPUT/$package.$suffix"; fi
  done
  if (( status != 0 )); then
    tail -n 80 "$PROFILE_OUTPUT/$package.log" >&2
    exit "$status"
  fi
  grep -Eq '^All 1 tests passed' "$PROFILE_OUTPUT/$package.log"
done

cp -- dist-bench-prof/cache/plan.json "$PROFILE_OUTPUT/build-plan.json"
