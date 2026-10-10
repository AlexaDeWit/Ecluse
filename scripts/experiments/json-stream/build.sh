#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
#
# SPDX-License-Identifier: MIT
set -euo pipefail

# shellcheck source-path=SCRIPTDIR
# shellcheck source=common.sh
source "$(dirname -- "${BASH_SOURCE[0]}")/common.sh"
check_inputs
readonly benchmark_binaries=(aeson-benchmark-jstream-parse aeson-benchmark-fastobj aeson-benchmark-aeson-parse)
build_options=(build all -j1)
[[ ${BENCH_OFFLINE:-0} != 1 ]] || build_options+=(--offline)
: > "$artifact_dir/binaries.jsonl"
{
  ghc --version
  cabal --version
  nix --version
  uname -a
  lscpu
} > "$artifact_dir/toolchain.txt"
for variant in "${variants[@]}"; do
  for group in benchmarks harness; do
    (cd -- "$work_dir/$variant/$group" && cabal "${build_options[@]}") \
      > "$artifact_dir/logs/build-$variant-$group.log" 2>&1 || {
        cat -- "$artifact_dir/logs/build-$variant-$group.log" >&2
        fail "Build failed: $variant/$group"
      }
    cp -- "$work_dir/$variant/$group/dist-newstyle/cache/plan.json" "$artifact_dir/$variant.$group.plan.json"
  done
  for name in "${benchmark_binaries[@]}" token-fold; do
    group=benchmarks
    [[ "$name" != token-fold ]] || group=harness
    binary=$(cd -- "$work_dir/$variant/$group" && cabal list-bin "exe:$name")
    digest=$(sha256sum "$binary")
    digest=${digest%% *}
    symbol=false
    nm "$binary" > "$artifact_dir/$variant.$name.symbols.txt"
    if awk '$NF == "lex_json" {found=1} END {exit !found}' "$artifact_dir/$variant.$name.symbols.txt"; then symbol=true; fi
    native=$(jq -r --arg variant "$variant" '.[] | select(.variant == $variant) | .native_lexer' "$artifact_dir/variants.json")
    if [[ "$native" == true && "$symbol" == true ]]; then fail "Unexpected C lexer symbol: $variant/$name"; fi
    if [[ "$native" == false && "$name" != aeson-benchmark-aeson-parse && "$symbol" != true ]]; then
      fail "Missing reference C lexer symbol: $variant/$name"
    fi
    jq -nc --arg variant "$variant" --arg name "$name" --arg path "$binary" --arg sha256 "$digest" \
      --argjson lex_json "$symbol" '{variant:$variant,name:$name,path:$path,sha256:$sha256,lex_json:$lex_json}' \
      >> "$artifact_dir/binaries.jsonl"
  done
done
jq -s . "$artifact_dir/binaries.jsonl" > "$artifact_dir/binaries.json"
for variant in "${variants[@]}"; do
  jq '{compiler: .["compiler-id"], arch, os, dependencies: ([.["install-plan"][] | {name: .["pkg-name"], version: .["pkg-version"]}] | unique | sort_by(.name))}' \
    "$artifact_dir/$variant.benchmarks.plan.json" > "$artifact_dir/$variant.dependencies.json"
  cmp -- "$artifact_dir/original.dependencies.json" "$artifact_dir/$variant.dependencies.json"
done
check_inputs
