#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
# SPDX-License-Identifier: MIT
set -euo pipefail

[[ ${GITHUB_ACTIONS:-false} == true && ${SHARING_LEAD_REVIEWED:-false} == true ]] || {
  echo 'sharing coverage requires a hosted run after lead review' >&2
  exit 1
}
[[ ${GITHUB_REF:-} == refs/heads/measure/1633-sharing-coverage ]] || {
  echo 'sharing coverage requires the measurement branch' >&2
  exit 1
}
expected=${EXPECTED_REVIEWED_SHA:?missing expected reviewed SHA}
[[ $expected =~ ^[0-9a-f]{40}$ && $(git rev-parse HEAD) == "$expected" ]] || {
  echo 'measurement checkout does not match the reviewed SHA' >&2
  exit 1
}
base=4dd0514b30350cf5c36d07cf7b28179e9001e684
root=$(git rev-parse --show-toplevel)
[[ ${CABAL_DIR:-} == "$root/.cabal" && ! -L "$root/.cabal" ]] || {
  echo 'unexpected cabal cache location' >&2
  exit 1
}
git diff --exit-code HEAD --
git diff --cached --exit-code --
while IFS= read -r -d '' path; do
  [[ $path == .cabal/* ]] || {
    printf 'unexpected untracked source: %s\n' "$path" >&2
    exit 1
  }
done < <(git ls-files --others --exclude-standard -z)
assets="$root/scratchpad/1633-sharing-coverage"
allowed=(.github/workflows/bench.yml scratchpad/1633-sharing-coverage/Main.hs scratchpad/1633-sharing-coverage/coverage.patch scratchpad/1633-sharing-coverage/static.yml scripts/measure-1633-sharing-coverage.sh)
actual_diff=$(git diff --name-only "$base" "$expected" | LC_ALL=C sort)
expected_diff=$(printf '%s\n' "${allowed[@]}" | LC_ALL=C sort)
[[ $actual_diff == "$expected_diff" ]] || {
  echo 'measurement source differs outside the five reviewed temporary files' >&2
  exit 1
}
for asset in "${allowed[@]}"; do
  cmp "$root/$asset" <(git show "$expected:$asset")
done
output="$assets/host-results"
[[ ! -e $output ]] || {
  echo 'refusing to overwrite an earlier sharing run' >&2
  exit 1
}
mkdir -p "$output/source" "$output/control" "$output/observed" "$output/provenance"
finish_run() {
  local run_status=$?
  set +e
  for leg in control observed; do
    if [[ -f $output/source/dist-sharing-$leg/cache/plan.json ]]; then
      cp "$output/source/dist-sharing-$leg/cache/plan.json" "$output/provenance/$leg-plan.json"
    fi
  done
  printf '%s\n' "$run_status" > "$output/status.txt"
  find "$output/control" "$output/observed" -type f -name '*.json' -print0 | sort -z | xargs -0 -r sha256sum > "$output/provenance/partial-json-sha256.txt"
  exit "$run_status"
}
trap finish_run EXIT
printf '%s\n' 'Coverage only. No baseline allocation, timing, heap or throughput claim.' > "$output/limits.txt"

git rev-parse HEAD 'HEAD^{tree}' > "$output/provenance/measurement-head-tree.txt"
git rev-parse "$base" "$base^{tree}" > "$output/provenance/base-head-tree.txt"
git status --porcelain > "$output/provenance/original-status.txt"
printf '%s\n' "$expected" > "$output/provenance/expected-reviewed-sha.txt"
printf '%s\n' "$actual_diff" > "$output/provenance/allowed-source-diff.txt"
for asset in "${allowed[@]}"; do
  git show "$expected:$asset" | sha256sum
  sha256sum "$root/$asset"
done > "$output/provenance/reviewed-asset-sha256.txt"
cp "$assets/coverage.patch" "$assets/Main.hs" "$output/provenance/"
sha256sum "$assets/coverage.patch" "$assets/Main.hs" "$0" "$root/.github/workflows/bench.yml" > "$output/provenance/overlay-sha256.txt"
git ls-tree -r "$base" > "$output/provenance/base-source-blobs.txt"
git archive "$base" | tar -x -C "$output/source"
cd "$output/source"
git init --quiet
cmp "$root/flake.lock" flake.lock
cp "$assets/Main.hs" bench/Main.hs
cp ecluse.cabal cabal.project cabal.project.freeze flake.lock "$output/provenance/"
sha256sum bench/corpus/pins.json core/test/unit/fixtures/corpus-outputs.tsv > "$output/provenance/frozen-sha256.txt"
ghc --version > "$output/provenance/compiler.txt"
cabal --version > "$output/provenance/cabal.txt"
getconf _NPROCESSORS_ONLN > "$output/provenance/host-cpus.txt"
for setting in cpu.max memory.max; do
  if [[ -r /sys/fs/cgroup/$setting ]]; then
    cat "/sys/fs/cgroup/$setting" > "$output/provenance/$setting"
  fi
done

run_read() {
  local leg=$1
  local builddir="dist-sharing-$leg"
  local options=()
  if [[ $leg == observed ]]; then
    options+=(--ghc-options=-DSHARING_STATS)
  fi
  cabal build ecluse-bench --enable-benchmarks --builddir="$builddir" "${options[@]}" 2>&1 | tee "$output/$leg-build.log"
  cp "$builddir/cache/plan.json" "$output/provenance/$leg-plan.json"
  local executable
  executable=$(cabal list-bin ecluse-bench --enable-benchmarks --builddir="$builddir" "${options[@]}")
  sha256sum bench/Main.hs core/src/Ecluse/Core/Registry/Json/{Intern,Shape,Walk,Writer}.hs core/src/Ecluse/Core/Registry/Npm/{Metadata,VersionWriter,Reader,StreamingProjection}.hs core/src/Ecluse/Core/Registry/PyPI/{Metadata,FileWriter,Reader,StreamingProjection}.hs > "$output/provenance/$leg-compiled-source-sha256.txt"
  sha256sum "$executable" > "$output/provenance/$leg-executable-sha256.txt"
  "$executable" "$output/$leg" +RTS -N1 -RTS 2>&1 | tee "$output/$leg.log"
  sha256sum "$output/$leg/"*.json > "$output/provenance/$leg-input-output-sha256.txt"
}

run_read control
git apply --unidiff-zero --check "$assets/coverage.patch"
git apply --unidiff-zero "$assets/coverage.patch"
run_read observed
for result in "$output/control/"*-result.json; do
  diff -u "$result" "$output/observed/${result##*/}"
done
for source in "$output/control/"*.json; do
  [[ $source == *-result.json ]] && continue
  cmp "$source" "$output/observed/${source##*/}"
done
awk '/^BEGIN / { active=1; case_name=$2; source_name=$3 } /^END / { active=0 } /^SHARING / && active { sub(/^SHARING /, ""); printf "{\"case\":\"%s\",\"source\":\"%s\",\"statistics\":%s}\n", case_name, source_name, $0 }' "$output/observed.log" > "$output/sharing-rows.jsonl"
[[ $(wc -l < "$output/sharing-rows.jsonl") == 12 ]] || {
  echo 'expected 12 source-read statistics rows' >&2
  exit 1
}
