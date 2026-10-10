#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
#
# SPDX-License-Identifier: MIT
set -euo pipefail
export LC_ALL=C

readonly variants=(original baseline candidate)

bundle_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
: "${BENCH_OUTPUT:?Set BENCH_OUTPUT to a fresh absolute output directory}"
case "$BENCH_OUTPUT" in
  /*) ;;
  *) printf '%s\n' 'BENCH_OUTPUT must be absolute' >&2; exit 1 ;;
esac
readonly work_dir="$BENCH_OUTPUT/work"
readonly artifact_dir="$BENCH_OUTPUT/artifacts"

fail() {
  printf '%s\n' "$*" >&2
  exit 1
}

hash_sources() {
  local root=$1
  (
    cd -- "$root"
    find Data c_lib benchmarks harness -type f ! -path '*/dist-newstyle/*' -print0 |
      sort -z | xargs -0 sha256sum
  )
}

check_inputs() {
  local variant
  cmp -- "$bundle_dir/TokenFold.hs" "$artifact_dir/harness/TokenFold.hs"
  for variant in "${variants[@]}"; do
    (cd -- "$work_dir/$variant" && sha256sum --quiet --check "$artifact_dir/$variant.sources.sha256")
  done
  (cd -- "$work_dir/original/benchmarks/json-data" && sha256sum --quiet --check "$artifact_dir/fixtures.sha256")
}

binary_path() {
  local variant=$1 name=$2
  jq -er --arg variant "$variant" --arg name "$name" \
    '.[] | select(.variant == $variant and .name == $name) | .path' "$artifact_dir/binaries.json"
}
