#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
#
# SPDX-License-Identifier: MIT
set -euo pipefail

# shellcheck source-path=SCRIPTDIR
# shellcheck source=common.sh
source "$(dirname -- "${BASH_SOURCE[0]}")/common.sh"
: "${BENCH_REPO:?Set BENCH_REPO to the project checkout}"
: "${BENCH_BASELINE_SHA:?Set BENCH_BASELINE_SHA to the full baseline commit}"
: "${BENCH_CANDIDATE_SHA:?Set BENCH_CANDIDATE_SHA to the full candidate commit}"
readonly input_paths=(cabal.project cabal.project.freeze flake.nix flake.lock)
readonly upstream_url=https://github.com/ondrap/json-stream.git
readonly upstream_revision=537a43a775e64f50dc63c373193323de98619799
readonly input_copy_count=2
readonly candidate_fold_api=${BENCH_CANDIDATE_FOLD_API:-auto}
case "$candidate_fold_api" in
  auto|pure_cursor) ;;
  *) fail 'BENCH_CANDIDATE_FOLD_API must be auto or pure_cursor' ;;
esac

for revision in "$BENCH_BASELINE_SHA" "$BENCH_CANDIDATE_SHA"; do
  [[ "$revision" =~ ^[0-9a-f]{40}$ ]] || fail "Expected a full commit SHA: $revision"
  git -C "$BENCH_REPO" cat-file -e "$revision^{commit}"
done
if [[ "$candidate_fold_api" == pure_cursor ]]; then
  git -C "$BENCH_REPO" diff --quiet "$BENCH_BASELINE_SHA" "$BENCH_CANDIDATE_SHA" -- vendor/json-stream ||
    fail 'The pure_cursor comparison requires identical baseline and candidate vendor sources'
fi
mkdir -- "$BENCH_OUTPUT"
mkdir -p -- "$work_dir" "$artifact_dir/inputs" "$artifact_dir/logs" "$artifact_dir/harness"
cp -- "$bundle_dir"/*.sh "$bundle_dir"/*.jq "$bundle_dir/TokenFold.hs" \
  "$bundle_dir/Taskfile.yml" "$bundle_dir/fixtures.json" "$bundle_dir/LICENSE" "$artifact_dir/harness/"
for path in "${input_paths[@]}"; do
  git -C "$BENCH_REPO" show "$BENCH_BASELINE_SHA:$path" > "$artifact_dir/inputs/baseline.$path"
  git -C "$BENCH_REPO" show "$BENCH_CANDIDATE_SHA:$path" > "$artifact_dir/inputs/candidate.$path"
  cmp -- "$artifact_dir/inputs/baseline.$path" "$artifact_dir/inputs/candidate.$path" ||
    fail "Benchmark dependency/toolchain inputs differ: $path"
done
cmp -- "$BENCH_REPO/flake.lock" "$artifact_dir/inputs/candidate.flake.lock"
cmp -- "$BENCH_REPO/flake.nix" "$artifact_dir/inputs/candidate.flake.nix"
index_state=$(awk '$1 == "index-state:" {print $2}' "$artifact_dir/inputs/candidate.cabal.project")
[[ "$index_state" =~ ^[0-9TZ:-]+$ ]] || fail 'Expected one pinned Cabal index-state'

upstream_repo=${BENCH_UPSTREAM_REPO:-$work_dir/upstream}
if [[ -z ${BENCH_UPSTREAM_REPO:-} ]]; then
  git init --quiet "$upstream_repo"
  git -C "$upstream_repo" fetch --quiet --depth=1 "$upstream_url" "$upstream_revision"
fi
git -C "$upstream_repo" cat-file -e "$upstream_revision^{commit}"
jq -r '.[] | .sha256 + "  " + .path' "$bundle_dir/fixtures.json" > "$artifact_dir/fixtures.sha256"
cp -- "$bundle_dir/fixtures.json" "$artifact_dir/fixtures.json"
jq -n --argjson input_copy_count "$input_copy_count" '{workload:"adapted-token-fold", input_copy_count:$input_copy_count,
  operation:"BS.useAsCStringLen source BS.packCStringLen",
  included_in_time_and_allocation:true, applied_equally_to_all_variants:true,
  note:"The fold includes two fresh input copies per pass. Upstream benchmark executables keep their own input handling."}' \
  > "$artifact_dir/input-copy-policy.json"
: > "$artifact_dir/variants.jsonl"

for variant in "${variants[@]}"; do
  target="$work_dir/$variant"
  mkdir -- "$target"
  git -C "$upstream_repo" archive "$upstream_revision" | tar -xf - -C "$target"
  revision=$upstream_revision
  native=false
  owned=false
  if [[ "$variant" != original ]]; then
    if [[ "$variant" == baseline ]]; then revision=$BENCH_BASELINE_SHA; else revision=$BENCH_CANDIDATE_SHA; fi
    # Both paths were created by this invocation from the pinned upstream archive.
    rm -rf -- "$target/Data" "$target/c_lib"
    git -C "$BENCH_REPO" archive "$revision" vendor/json-stream |
      tar -xf - --strip-components=2 -C "$target"
    git -C "$BENCH_REPO" show "$revision:ecluse.cabal" > "$artifact_dir/inputs/$variant.ecluse.cabal"
  fi
  [[ ! -f "$target/Data/JsonStream/Lexer/Internal.hs" ]] || native=true
  [[ ! -f "$target/Data/JsonStream/TokenReader.hs" ]] || owned=true
  fold_api=token_parser
  [[ "$owned" != true ]] || fold_api=owned_reader
  if [[ "$variant" == candidate && "$candidate_fold_api" == pure_cursor ]]; then
    [[ "$native" == true ]] || fail 'The pure_cursor lane requires Lexer.Internal'
    fold_api=pure_cursor
    owned=false
  fi
  if [[ "$native" == true ]]; then
    awk '
      /^  (c-sources|includes|include-dirs|cc-options):/ {next}
      {print}
      /^    Data.JsonStream.Unescape$/ {
        print "    Data.JsonStream.Lexer.Internal"
        print "    Data.JsonStream.Number"
      }
    ' "$target/benchmarks/aeson-benchmarks.cabal" > "$target/benchmarks/package.tmp"
    mv -- "$target/benchmarks/package.tmp" "$target/benchmarks/aeson-benchmarks.cabal"
  elif [[ "$variant" != original ]]; then
    sed 's|^  c-sources:.*|  c-sources: ../c_lib/lexer.c|' \
      "$target/benchmarks/aeson-benchmarks.cabal" > "$target/benchmarks/package.tmp"
    mv -- "$target/benchmarks/package.tmp" "$target/benchmarks/aeson-benchmarks.cabal"
  fi
  mkdir -- "$target/harness"
  cp -- "$bundle_dir/TokenFold.hs" "$target/harness/TokenFold.hs"
  cp -- "$bundle_dir/LICENSE" "$target/harness/LICENSE"
  cat > "$target/harness/token-fold.cabal" <<'CABAL'
cabal-version: 3.0
name: token-fold
version: 0.1.0.0
build-type: Simple
license: BSD-3-Clause AND MIT
license-files: ../LICENSE LICENSE
executable token-fold
  main-is: TokenFold.hs
  hs-source-dirs: ., ..
  default-language: Haskell2010
  ghc-options: -O2 -Wall -rtsopts
  build-depends: base, aeson, bytestring, text, scientific, primitive, deepseq
  other-modules:
    Data.JsonStream.CLexType
    Data.JsonStream.CLexer
    Data.JsonStream.TokenParser
    Data.JsonStream.Unescape
CABAL
  if [[ "$owned" == true ]]; then
    printf '    Data.JsonStream.TokenReader\n' >> "$target/harness/token-fold.cabal"
  fi
  if [[ "$native" == true ]]; then
    printf '    Data.JsonStream.Lexer.Internal\n    Data.JsonStream.Number\n' >> "$target/harness/token-fold.cabal"
  else
    printf '  c-sources: ../c_lib/lexer.c\n  include-dirs: ../c_lib\n  cc-options: -O2 -Wall\n' >> "$target/harness/token-fold.cabal"
  fi
  if [[ "$owned" == true ]]; then
    printf '  cpp-options: -DOWNED_READER\n' >> "$target/harness/token-fold.cabal"
  elif [[ "$fold_api" == pure_cursor ]]; then
    printf '  cpp-options: -DPURE_CURSOR\n' >> "$target/harness/token-fold.cabal"
  fi
  for group in benchmarks harness; do
    printf 'packages: .\nindex-state: %s\noptimization: 2\ntests: False\nbenchmarks: False\n' \
      "$index_state" > "$target/$group/cabal.project"
    cp -- "$artifact_dir/inputs/candidate.cabal.project.freeze" "$target/$group/cabal.project.freeze"
  done
  for source in JStreamParse.hs JStreamParseObj.hs AesonParse.hs; do
    cmp -- "$work_dir/original/benchmarks/$source" "$target/benchmarks/$source"
  done
  hash_sources "$target" > "$artifact_dir/$variant.sources.sha256"
  jq -nc --arg variant "$variant" --arg revision "$revision" --arg fold_api "$fold_api" --argjson native "$native" \
    '{variant:$variant, revision:$revision, native_lexer:$native, owned_fold:($fold_api == "owned_reader"), fold_api:$fold_api}' >> "$artifact_dir/variants.jsonl"
done
jq -s . "$artifact_dir/variants.jsonl" > "$artifact_dir/variants.json"
check_inputs
printf '%s\n' 'Prepared all variants with identical pinned dependency and Nix inputs.'
