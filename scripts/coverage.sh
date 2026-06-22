#!/usr/bin/env bash
#
# Generate a coverage report for one test suite, in Codecov's native JSON format,
# for upload by the Codecov action. See CONTRIBUTING.md -> "Coverage".
#
# Why we drive HPC by hand instead of `hpc-codecov cabal:<suite>`: hpc-codecov's
# `cabal:` auto-discovery builds a wrong .mix path for cabal's modern
# `extra-compilation-artifacts` layout (it doubles the package-key segment), so
# it fails. We locate the .tix and every .mix directory ourselves and pass them
# explicitly; extra .mix dirs are harmless because hpc-codecov only reads the
# ones the .tix references. This manual path sidesteps the auto-discovery
# entirely, so it is robust across hpc-codecov versions (pinned via the GHC 9.10
# set in flake.lock; currently 0.6.x).
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

# Completeness guard. HPC only emits a module that was linked into the suite's
# .tix, so a library module the suite never imports is silently *absent* from the
# report (not reported as 0%), which quietly inflates the percentage. Fail loudly
# when a src/ module is missing, so a dropped module is caught here rather than
# hidden: the fix is a test that exercises it (so it links), or — for a module
# with genuinely nothing to cover yet — an entry in `unscoped` below. See
# CONTRIBUTING.md -> "Coverage".
#
# Intentionally unscoped: pure Handles/types with no executable logic yet. Each
# entry states why and when it returns, so this stays a reviewed decision and not
# a silent escape hatch. Paths use the ./src/... form the report emits.
unscoped=(
  # pure protocol Handle (#16): the RegistryClient record + error newtypes, no
  # logic. Remove when S06 (npm-wire) adds real fetch/parse code and tests that
  # link it.
  ./src/Ecluse/Registry.hs
)

expected="$(find src -name '*.hs' | sed 's#^#./#' | sort)"
if [ ${#unscoped[@]} -gt 0 ]; then
  expected="$(comm -23 <(echo "$expected") <(printf '%s\n' "${unscoped[@]}" | sort))"
fi
present="$(grep -oE '"\./src/[^"]+\.hs"' "$out" | tr -d '"' | sort -u || true)"
missing="$(comm -23 <(echo "$expected") <(echo "$present"))"
if [ -n "$missing" ]; then
  {
    echo "coverage: library modules absent from $out (not linked by $suite):"
    echo "$missing" | sed 's/^/  /'
    echo "Add a test that exercises the module, or list it in scripts/coverage.sh 'unscoped'."
  } >&2
  exit 1
fi

echo "coverage: wrote $out"
