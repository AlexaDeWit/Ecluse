#!/usr/bin/env bash
#
# Generate a coverage report for ONE test suite, in Codecov's native JSON format,
# for upload by the Codecov action. See CONTRIBUTING.md -> "Coverage".
#
# This is a PARTIAL view: a single tier is only one of the flags Codecov merges
# into the project total (unit ∪ integration). CI uses this per-tier form on
# purpose — each tier uploads under its own flag — but a developer reading a single
# tier's number is under-counting every module the other tier exercises. For the
# merged picture that matches the Codecov dashboard, run the combined report:
#   scripts/coverage-combined.sh   (task coverage)   — needs Docker.
# Each non-combined run prints this caveat (suppressed via
# ECLUSE_COVERAGE_QUIET_PARTIAL=1 when the combined script drives it internally).
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
shift || true
builddir="dist-coverage" # isolated from dist-newstyle so the normal `cabal
                         # build` cache isn't invalidated by -fhpc instrumentation
outdir="coverage"

# Loudly remind that a single tier is a PARTIAL view Codecov merges with the other
# tier — the divergence this machinery exists to prevent (a local single-tier read
# under-counts every module the other tier covers). coverage-combined.sh sets
# ECLUSE_COVERAGE_QUIET_PARTIAL when it drives this per tier, because it then prints
# the merged total itself.
if [ -z "${ECLUSE_COVERAGE_QUIET_PARTIAL:-}" ]; then
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

# Exclude every module of the ecluse-test-support library. It is a first-class
# local library, so the instrumented build measures it and its Ecluse.Test.*
# modules (linked into the suite) would otherwise land in the uploaded report —
# but it is shared test scaffolding, not the software under test. The exclusions
# are derived from test/support/ so a NEW Ecluse.Test.* module is dropped here
# automatically. codecov.yml `ignore` also drops test/support/** as a second,
# report-side line of defence.
support_exclude=()
while IFS= read -r module; do
  support_exclude+=(-x "$module")
done < <(find test/support -name '*.hs' | sed -e 's#^test/support/##' -e 's#/#.#g' -e 's#\.hs$##')

# -s .              resolve source paths relative to the repo root (./src/...)
# -x Main           drop the hspec-discover entry module (no real source)
# -x Paths_ecluse   drop the cabal-generated path module
# -x Ecluse.Test.*  drop the ecluse-test-support library (support_exclude, above)
# -f codecov        Codecov's native JSON: leanest for Codecov to ingest (no conversion)
# Library-vs-test scoping is otherwise done by codecov.yml `ignore`, so new spec
# modules need no change to this script.
hpc-codecov "${mix_args[@]}" \
  -s . \
  -x Main \
  -x Paths_ecluse \
  "${support_exclude[@]}" \
  -f codecov \
  -o "$out" \
  "$tix"

# Completeness guard. HPC only emits a module that was linked into the suite's
# .tix, so a library module the suite never imports is silently *absent* from the
# report (not reported as 0%), which quietly inflates the percentage. Fail loudly
# when a module is missing, so a dropped module is caught here rather than
# hidden: the fix is a test that exercises it (so it links), or — for a module
# with genuinely nothing to cover yet — an entry in the suite's `unscoped` list
# below. See CONTRIBUTING.md -> "Coverage".
#
# This whole-library expectation holds only for the two unit suites, each meant to
# link every module in its respective source tree:
#   ecluse-core-unit → every core/src/*.hs module must be linked
#   ecluse-unit      → every src/*.hs module must be linked
# A focused suite (e.g. ecluse-integration, which links only the cloud-backed
# modules its emulator tests exercise) legitimately reports a subset; its partial
# view is merged into the Codecov total under its own flag, so the guard would only
# produce false positives there. Skip it for non-unit suites.
case "$suite" in
  ecluse-core-unit)
    src_dir="core/src"
    src_prefix="./core/src/"
    # Intentionally unscoped in ecluse-core-unit: pure types/handles with no
    # executable logic yet. Each entry states why and when it returns, so this
    # stays a reviewed decision and not a silent escape hatch.
    unscoped=(
      # pure re-export shim: the curated public surface only; implementation and
      # coverage live in Ecluse.Core.Credential.Refresh.Internal.
      ./core/src/Ecluse/Core/Credential/Refresh.hs
      # pure re-export shims
      ./core/src/Ecluse/Core/Security.hs
      ./core/src/Ecluse/Core/Server/Pipeline.hs
      ./core/src/Ecluse/Core/Worker.hs
      # pure protocol Handle (#16): the RegistryClient record + error newtypes, no
      # logic. Remove when S06 (npm-wire) adds real fetch/parse code and tests that
      # link it.
      ./core/src/Ecluse/Core/Registry.hs
      # test/dev-only loopback constructor enabled by dev-http-egress flag.
      # tested by integration tests, not linked by core-unit tests.
      ./core/src/Ecluse/Core/Security/Egress/DevHttp.hs
      # servePublish is tested exclusively by the integration test suite
      # (test/integration/Ecluse/Server/PublishSpec.hs).
      ./core/src/Ecluse/Core/Server/Pipeline/Publish.hs
    )
    
    # Drop orphaned coverage records from the CI cache since WorkerSpec was split and deleted.
    support_exclude+=(-x "Ecluse.WorkerSpec")
    ;;
  ecluse-unit)
    src_dir="src"
    src_prefix="./src/"
    # Intentionally unscoped in ecluse-unit: pure types/handles with no executable
    # logic yet. Each entry states why and when it returns, so this stays a reviewed
    # decision and not a silent escape hatch. Paths use the ./src/... form the
    # report emits.
    unscoped=()
    ;;
  ecluse-runtime-unit)
    src_dir="runtime/src"
    src_prefix="./runtime/src/"
    # Runtime modules the runtime-unit suite intentionally does not link, each covered by
    # another gating tier and merged into the Codecov total under its flag. Each entry
    # states why, so this stays a reviewed decision, not a silent escape hatch.
    unscoped=(
      # Ecluse.Runtime.Server and Ecluse.Runtime.Env exercise the shell's runServer /
      # runWorker, so ServerSpec and EnvSpec live in ecluse-unit (which links the app
      # library); the runtime-unit partition cannot. They are covered there.
      ./runtime/src/Ecluse/Runtime/Server.hs
      ./runtime/src/Ecluse/Runtime/Env.hs
      # The middleware pieces and health probes are exercised through the composed
      # application (shell fixtures) in the same ecluse-unit ServerSpec; the
      # runtime-unit partition links only the drain and halt siblings, whose specs
      # drive them directly.
      ./runtime/src/Ecluse/Runtime/Server/Middleware.hs
      # The S3 export adapter is exercised only by the integration tier
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
