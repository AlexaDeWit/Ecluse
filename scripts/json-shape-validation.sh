#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
# SPDX-License-Identifier: MIT
set -euo pipefail

readonly main_ref=dc1bfb3571d33500a945a23a88eea935bcd80d23
readonly native_ref=3b074ce62e7a1584fe3b638f2375b79f443e0f72
readonly c_ref=a45f883a71a74ed10214ef235230c0268ac93914
readonly bench_precision=10
readonly bench_timeout=30s
readonly expected_bench_rows=2
readonly benchmark_pattern='(/full metadata projection/ || /full document/)'
readonly -a variants=(main native c)
readonly -a revisions=("$main_ref" "$native_ref" "$c_ref")
readonly -a rotations=('main native c' 'native c main' 'c main native')
readonly -a runtime_flags=(-N1 -T)
readonly -a overlay_paths=(
  ecluse.cabal bench/Main.hs
  bench/Ecluse/Bench/ShapeValidation.hs
  bench/Ecluse/Bench/ShapeValidation/Inputs.hs
  test/support/Ecluse/Test/Registry/Metadata/Fetch.hs
)

if [[ "${GITHUB_ACTIONS:-}" != true ]]; then
  echo 'Run timed shape validation in CI. Local compilation and semantic preflight are separate.' >&2
  exit 2
fi

shape_root=$(git rev-parse --show-toplevel)
shape_work=${ECLUSE_SHAPE_WORK:?ECLUSE_SHAPE_WORK must name a fresh CI directory}
if [[ -e "$shape_work" ]]; then
  echo "Refusing to reuse measurement directory: $shape_work" >&2
  exit 2
fi
mkdir -p "$shape_work/artifacts" "$shape_work/checkouts"
shape_artifacts="$shape_work/artifacts"
shape_inputs="$shape_artifacts/inputs"
git diff "$main_ref" HEAD -- "${overlay_paths[@]}" > "$shape_artifacts/harness.patch"
test -s "$shape_artifacts/harness.patch"
git rev-parse HEAD > "$shape_artifacts/harness-head.txt"
ghc --info > "$shape_artifacts/ghc-info.txt"
cabal --version > "$shape_artifacts/cabal-version.txt"
uname -a > "$shape_artifacts/runner.txt"
printf 'variant\trevision\n' > "$shape_artifacts/sources.tsv"

declare -A binaries
for revision in "${revisions[@]}"; do
  git -C "$shape_root" cat-file -e "$revision^{commit}"
done
for index in "${!variants[@]}"; do
  variant=${variants[$index]}
  revision=${revisions[$index]}
  checkout="$shape_work/checkouts/$variant"
  git -C "$shape_root" worktree add --detach "$checkout" "$revision"
  git -C "$checkout" apply --check "$shape_artifacts/harness.patch"
  git -C "$checkout" apply "$shape_artifacts/harness.patch"
  cmp "$shape_root/flake.lock" "$checkout/flake.lock"
  cmp "$shape_root/cabal.project.freeze" "$checkout/cabal.project.freeze"
  printf '%s\t%s\n' "$variant" "$(git -C "$checkout" rev-parse HEAD)" >> "$shape_artifacts/sources.tsv"
done
for variant in "${variants[@]}"; do
  checkout="$shape_work/checkouts/$variant"
  (
    cd "$checkout"
    cabal build ecluse-bench --enable-benchmarks
  ) > "$shape_artifacts/build-$variant.log" 2>&1
  binaries[$variant]=$(cd "$checkout" && cabal list-bin ecluse-bench)
  sha256sum "${binaries[$variant]}" >> "$shape_artifacts/binaries.sha256"
done

(
  cd "$shape_work/checkouts/main"
  ECLUSE_SHAPE_MODE=generate ECLUSE_SHAPE_DIR="$shape_inputs" \
    "${binaries[main]}" +RTS "${runtime_flags[@]}" -RTS
) > "$shape_artifacts/generation.log" 2>&1
sha256sum "$shape_inputs"/*.json > "$shape_artifacts/inputs.sha256"

for variant in "${variants[@]}"; do
  (
    cd "$shape_work/checkouts/$variant"
    export ECLUSE_SHAPE_MODE=preflight ECLUSE_SHAPE_DIR="$shape_inputs"
    export ECLUSE_SHAPE_OUTPUT="$shape_artifacts/preflight-$variant"
    unset ECLUSE_SHAPE_BASELINE
    if [[ "$variant" != main ]]; then
      export ECLUSE_SHAPE_BASELINE="$shape_artifacts/preflight-main"
    fi
    "${binaries[$variant]}" +RTS "${runtime_flags[@]}" -RTS
  ) > "$shape_artifacts/preflight-$variant.log" 2>&1
done
sha256sum --check "$shape_artifacts/inputs.sha256"

printf 'case\tround\tvariant\tstarted_utc\n' > "$shape_artifacts/order.tsv"
while IFS= read -r case_id; do
  if [[ ! "$case_id" =~ ^[a-zA-Z0-9_-]+$ ]]; then
    echo "Invalid generated case label: $case_id" >&2
    exit 2
  fi
  for round in "${!rotations[@]}"; do
    read -r -a order <<< "${rotations[$round]}"
    for variant in "${order[@]}"; do
      stem="$shape_artifacts/$case_id-round-$round-$variant"
      printf '%s\t%s\t%s\t%s\n' "$case_id" "$round" "$variant" "$(date -u +%FT%TZ)" >> "$shape_artifacts/order.tsv"
      (
        cd "$shape_work/checkouts/$variant"
        ECLUSE_SHAPE_MODE=bench ECLUSE_SHAPE_DIR="$shape_inputs" ECLUSE_SHAPE_CASE="$case_id" \
          "${binaries[$variant]}" -p "$benchmark_pattern" --stdev "$bench_precision" \
          --timeout "$bench_timeout" --csv "$stem.csv" +RTS "${runtime_flags[@]}" -RTS
      ) > "$stem.log" 2>&1
      awk -F, -v expected="$expected_bench_rows" '
        NR > 1 { if (NF != 6 || $2 <= 0) exit 1; rows++ }
        END { if (rows != expected) exit 1 }
      ' "$stem.csv"
    done
  done
done < "$shape_inputs/cases.txt"
sha256sum --check "$shape_artifacts/inputs.sha256"
printf '%s\n' 'Completed all fixed rotations. Results are measurements, with no throughput pass threshold.' > "$shape_artifacts/completion.txt"
cat "$shape_artifacts/completion.txt"
