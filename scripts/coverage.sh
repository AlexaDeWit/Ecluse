#!/usr/bin/env bash
#
# Generate a coverage report for one test suite, in Codecov's native JSON format,
# for upload by the Codecov action. See CONTRIBUTING.md -> "Coverage".
#
# Why we drive HPC by hand instead of `hpc-codecov cabal:<suite>`: hpc-codecov
# 0.5 (our pinned GHC 9.6 set) builds a wrong .mix path for cabal's modern
# `extra-compilation-artifacts` layout (it doubles the package-key segment), so
# its `cabal:` auto-discovery fails. We locate the .tix and every .mix directory
# ourselves and pass them explicitly; extra .mix dirs are harmless because
# hpc-codecov only reads the ones the .tix references.
#
# Usage: scripts/coverage.sh [SUITE]   (default: ecluse-unit)
set -euo pipefail

suite="${1:-ecluse-unit}"
builddir="dist-coverage" # isolated from dist-newstyle so the normal `cabal
                         # build` cache isn't invalidated by -fhpc instrumentation
outdir="coverage"

# Instrumented build + run of just this suite.
cabal test "$suite" \
  --enable-coverage \
  --builddir="$builddir" \
  --test-show-details=direct

tix="$(find "$builddir" -type f -name "${suite}.tix" | head -n1)"
if [ -z "$tix" ]; then
  echo "coverage: no ${suite}.tix found under $builddir/ (did the suite run?)" >&2
  exit 1
fi

# Every HPC mix directory produced by the coverage build.
mix_args=()
while IFS= read -r d; do
  mix_args+=(-m "$d")
done < <(find "$builddir" -type d -path '*/hpc/vanilla/mix')

mkdir -p "$outdir"
out="$outdir/${suite}.json"

# -s .            resolve source paths relative to the repo root (./src/..., ./test/...)
# -x Main         drop the hspec-discover entry module (no real source)
# -x Paths_ecluse drop the cabal-generated path module
# -f codecov      Codecov's native JSON: leanest for Codecov to ingest (no conversion)
# Library-vs-test scoping is done by codecov.yml `ignore`, not here, so new
# spec modules need no change to this script.
hpc-codecov "${mix_args[@]}" \
  -s . \
  -x Main \
  -x Paths_ecluse \
  -f codecov \
  -o "$out" \
  "$tix"

echo "coverage: wrote $out"
