#!/usr/bin/env bash
#
# Generate a coverage report for ONE test suite, in Codecov's native JSON format,
# for the Codecov action to upload. See CONTRIBUTING.md -> "Coverage".
#
# This is a PARTIAL view: a single tier is only one of the flags Codecov merges
# into the project total (unit ∪ integration). CI uses this per-tier form on
# purpose, because each tier uploads under its own flag. A developer reading one
# tier's number is under-counting every module the other tier exercises. For the
# merged picture that matches the Codecov dashboard, run the combined report,
# which needs Docker:
#   scripts/coverage-combined.sh   (task coverage)
# Each non-combined run prints this caveat. Set ECLTEST_COVERAGE_QUIET_PARTIAL=1
# to suppress it, as the combined script does when it drives this one per tier.
#
# Why we drive HPC by hand instead of `hpc-codecov cabal:<suite>`: the `cabal:`
# auto-discovery builds a wrong .mix path for cabal's modern
# `extra-compilation-artifacts` layout. It doubles the package-key segment, so
# it fails. We locate the .tix and every .mix directory ourselves and pass them
# explicitly. Extra .mix dirs are harmless, because hpc-codecov reads only the
# ones the .tix references. This manual path avoids the auto-discovery, so it
# holds across hpc-codecov versions (pinned via the GHC 9.10 set in flake.lock,
# currently 0.6.x).
#
# Usage: scripts/coverage.sh [SUITE]   (default: ecluse-unit)
set -euo pipefail

suite="${1:-ecluse-unit}"
shift || true
builddir="dist-coverage" # isolated so -fhpc instrumentation never invalidates
                         # the normal `cabal build` cache in dist-newstyle
outdir="coverage"

# When coverage-combined.sh drives this script per tier, it sets
# ECLTEST_COVERAGE_QUIET_PARTIAL to suppress the caveat below. It prints the
# merged total itself.
if [ -z "${ECLTEST_COVERAGE_QUIET_PARTIAL:-}" ]; then
  case "$suite" in
    ecluse-core-unit) other="ecluse-unit and ecluse-integration" ;;
    ecluse-unit) other="ecluse-core-unit and ecluse-integration" ;;
    ecluse-integration) other="ecluse-core-unit and ecluse-unit" ;;
    *) other="the other gating tiers" ;;
  esac
  {
    echo "coverage: PARTIAL VIEW — measuring '$suite' ONLY."
    echo "  Codecov merges this with '$other' into the project total, so this number"
    echo "  under-counts every module '$other' exercises. For the merged, Codecov-matching"
    echo "  picture run:  task coverage   (combined unit ∪ integration; needs Docker)."
  } >&2
fi

# Instrumented build + run of just this suite.
cabal test "$suite" \
  --enable-coverage \
  --builddir="$builddir" \
  --test-show-details=direct \
  "$@"

# The newest .tix is the one this run wrote. The cached builddir keeps the
# package directory of every version it ever built, so an older
# ecluse-<version>/ tree carries a stale .tix of the same name, and which one
# `find` lists first varies by filesystem.
tix="$(find "$builddir" -type f -name "${suite}.tix" -printf '%T@\t%p\n' | sort -n | tail -n1 | cut -f2-)"
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

# Exclude every module of the ecluse-test-support library. It is a first-class
# local library, so the instrumented build measures it. Its Ecluse.Test.* modules
# link into the suite, and would otherwise land in the uploaded report. It is
# shared test scaffolding, not the software under test. The exclusion list comes
# from test/support/, so a NEW Ecluse.Test.* module drops out here automatically.
# As a second, report-side line of defence, codecov.yml `ignore` also drops
# test/support/**.
support_exclude=()
while IFS= read -r module; do
  support_exclude+=(-x "$module")
done < <(find test/support -name '*.hs' | sed -e 's#^test/support/##' -e 's#/#.#g' -e 's#\.hs$##')

# -s .              resolve source paths relative to the repo root (./src/...)
# -x Main           drop the hspec-discover entry module (no real source)
# -x Paths_ecluse   drop the cabal-generated path module
# -x Ecluse.Test.*  drop the ecluse-test-support library (support_exclude, above)
# -f codecov        Codecov's native JSON: leanest for Codecov to ingest (no conversion)
# codecov.yml `ignore` does the rest of the library-vs-test scoping, so a new
# spec module needs no change to this script.
hpc-codecov "${mix_args[@]}" \
  -s . \
  -x Main \
  -x Paths_ecluse \
  "${support_exclude[@]}" \
  -f codecov \
  -o "$out" \
  "$tix"

# Completeness guard. HPC emits only a module the suite linked into its .tix. A
# library module the suite never imports is therefore silently *absent* from the
# report rather than reported as 0%, which quietly inflates the percentage. This
# guard fails loudly on a missing module instead of hiding it. The fix is a test
# that exercises the module, so that it links. For a module with genuinely nothing
# to cover yet, add an entry to the suite's `unscoped` list below. See
# CONTRIBUTING.md -> "Coverage".
#
# The whole-library expectation holds only for the two unit suites. Each must link
# every module in its own source tree:
#   ecluse-core-unit → must link every core/src/*.hs module.
#   ecluse-unit      → must link every src/*.hs module.
# A focused suite reports a subset legitimately: ecluse-integration links only the
# cloud-backed modules its emulator tests exercise. Codecov merges that partial
# view into the total under its own flag, so the guard would only produce false
# positives there. Skip it for a non-unit suite.
case "$suite" in
  ecluse-core-unit)
    src_dir="core/src"
    src_prefix="./core/src/"
    # Intentionally unscoped in ecluse-core-unit: pure types/handles with no
    # executable logic yet. Each entry states why and when it returns, so this
    # stays a reviewed decision and not a silent escape hatch.
    unscoped=(
      # pure re-export shim: the curated public surface only. The implementation
      # and its coverage live in Ecluse.Core.Credential.Refresh.Internal.
      ./core/src/Ecluse/Core/Credential/Refresh.hs
      # pure re-export shims
      ./core/src/Ecluse/Core/Security.hs
      ./core/src/Ecluse/Core/Server/Pipeline.hs
      ./core/src/Ecluse/Core/Worker.hs
      # pure protocol Handle: the RegistryClient record and the error newtypes,
      # no logic. Remove once the npm wire adapter adds real fetch and parse code
      # with tests that link it.
      ./core/src/Ecluse/Core/Registry.hs
      # test and dev-only loopback constructor, enabled by the dev-http-egress
      # flag. The integration tests cover it, and core-unit does not link it.
      ./core/src/Ecluse/Core/Security/Egress/DevHttp.hs
      # only the integration test suite covers servePublish
      # (test/integration/Ecluse/Server/PublishSpec.hs).
      ./core/src/Ecluse/Core/Server/Pipeline/Publish.hs
    )
    
    # Ecluse.WorkerSpec no longer exists, so drop the orphaned coverage records
    # the CI cache still holds for it.
    support_exclude+=(-x "Ecluse.WorkerSpec")
    ;;
  ecluse-unit)
    src_dir="src"
    src_prefix="./src/"
    # Intentionally unscoped in ecluse-unit: currently none. Add an entry only with
    # a stated reason, in the ./src/... form the report emits.
    unscoped=()
    ;;
  ecluse-runtime-unit)
    src_dir="runtime/src"
    src_prefix="./runtime/src/"
    # Runtime modules the runtime-unit suite intentionally does not link. Another
    # gating tier covers each one, merged into the Codecov total under its flag.
    unscoped=(
      # Ecluse.Runtime.Server and Ecluse.Runtime.Env exercise the shell's runServer
      # and runWorker, so ServerSpec and EnvSpec live in ecluse-unit, which links
      # the app library. The runtime-unit partition cannot link them. ecluse-unit
      # covers both.
      ./runtime/src/Ecluse/Runtime/Server.hs
      ./runtime/src/Ecluse/Runtime/Env.hs
      # The composed application (shell fixtures) exercises the middleware pieces
      # and health probes in that same ecluse-unit ServerSpec. The runtime-unit
      # partition links only the drain and halt siblings, whose specs drive them
      # directly.
      ./runtime/src/Ecluse/Runtime/Server/Middleware.hs
      # only the integration tier exercises the S3 export adapter
      # (test/integration/Ecluse/Pilot/S3ExportSpec.hs).
      ./runtime/src/Ecluse/Runtime/Pilot/Export.hs
    )
    ;;
  *)
    echo "coverage: wrote $out"
    exit 0
    ;;
esac

expected="$(find "$src_dir" -name '*.hs' | sed 's#^#./#' | sort)"
if [ ${#unscoped[@]} -gt 0 ]; then
  expected="$(comm -23 <(echo "$expected") <(printf '%s\n' "${unscoped[@]}" | sort))"
fi
present="$(grep -oE "\"${src_prefix}[^\"]+"'\.hs"' "$out" | tr -d '"' | sort -u || true)"
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
