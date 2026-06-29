#!/usr/bin/env bash
#
# Generate the COMBINED core-unit + app-unit + integration coverage report — the
# picture Codecov shows. Codecov merges the per-tier flag uploads (core-unit ∪
# app-unit ∪ integration) into one project total; this reproduces that total
# locally so a local read agrees with the dashboard. See CONTRIBUTING.md ->
# "Coverage" and docs/testing.md.
#
# Why combine, and why `--union`: Codecov sums the flag reports, so we mirror
# that by ADDing the three suites' HPC .tix (hpc combine's default join). A
# library module one suite can't exercise (e.g. the SQS MirrorQueue backend, the
# worker's real fetch/publish path) is covered only by the tier that drives it, so
# without `--union` — whose default is the *intersection* of the module namespace
# — those modules would be dropped and the merged view would understate them
# exactly where it matters. `--union` keeps every module any suite touched. All
# three suites are built from the same dist-coverage, so the shared modules' .tix
# hashes match and combine cleanly.
#
# REQUIRES A DOCKER DAEMON — the integration tier drives ministack containers via
# testcontainers (see docs/testing.md -> "Integration tests"). For a fast,
# Docker-free loop use the single-tier path instead:
#   make coverage SUITE=ecluse-core-unit  (a PARTIAL view; run both unit tiers for
#   make coverage SUITE=ecluse-unit        the full local unit picture without Docker)
#
# Usage: scripts/coverage-combined.sh   (accepts cabal test flags like -fdev-http-egress)
set -euo pipefail

builddir="dist-coverage"
outdir="coverage"
out="$outdir/combined.json"

core_unit_tier="ecluse-core-unit"
unit_tier="ecluse-unit"
integration_tier="ecluse-integration"

# Fail early, and helpfully, when the integration tier can't run. The combined
# report is unit ∪ integration, so a missing Docker daemon makes it impossible;
# point at the Docker-free single-tier path rather than producing a half report.
if ! docker info >/dev/null 2>&1; then
  cat >&2 <<EOF
coverage: the combined report needs a running Docker daemon (the
  $integration_tier tier drives ministack containers via testcontainers).
  Docker is a host concern; the Nix shell ships the toolchain, not the daemon.

  No Docker? Use the fast, Docker-free single-tier view (a PARTIAL picture
  Codecov merges with the other tiers):

      make coverage SUITE=$core_unit_tier
      make coverage SUITE=$unit_tier
EOF
  exit 1
fi

# Produce each tier's own report. scripts/coverage.sh leaves the suite's
# instrumented .tix under dist-coverage/ (which we combine below) AND writes
# coverage/<suite>.json — the same per-flag JSON CI uploads — so a combined run
# also refreshes all per-tier reports for free. SUITE-scoped runs print their own
# partial-view caveat; suppress it here since this script *is* the merged view.
echo "coverage: building $core_unit_tier (instrumented) ..."
ECLUSE_COVERAGE_QUIET_PARTIAL=1 bash "$(dirname "$0")/coverage.sh" "$core_unit_tier" "$@"
echo "coverage: building $unit_tier (instrumented) ..."
ECLUSE_COVERAGE_QUIET_PARTIAL=1 bash "$(dirname "$0")/coverage.sh" "$unit_tier" "$@"
echo "coverage: building $integration_tier (instrumented, needs Docker) ..."
ECLUSE_COVERAGE_QUIET_PARTIAL=1 bash "$(dirname "$0")/coverage.sh" "$integration_tier" "$@"

core_unit_tix="$(find "$builddir" -type f -name "${core_unit_tier}.tix" | head -n1)"
unit_tix="$(find "$builddir" -type f -name "${unit_tier}.tix" | head -n1)"
integration_tix="$(find "$builddir" -type f -name "${integration_tier}.tix" | head -n1)"
if [ -z "$core_unit_tix" ] || [ -z "$unit_tix" ] || [ -z "$integration_tix" ]; then
  echo "coverage: missing a tier .tix under $builddir/ (did all three suites run?)" >&2
  exit 1
fi

# ADD all three tiers' tick counts (hpc combine's default join), unioning the
# module namespace so a module only one tier links is kept (see header).
mkdir -p "$outdir"
unit_combined_tix="$builddir/unit-combined.tix"
hpc combine --union --output="$unit_combined_tix" "$core_unit_tix" "$unit_tix"
combined_tix="$builddir/combined.tix"
hpc combine --union --output="$combined_tix" "$unit_combined_tix" "$integration_tix"

# Every HPC mix directory from the coverage build — both suites'. hpc-codecov only
# reads the ones the combined .tix references, so extra dirs are harmless (the same
# robustness argument as scripts/coverage.sh).
mix_args=()
while IFS= read -r d; do
  mix_args+=(-m "$d")
done < <(find "$builddir" -type d -path '*/hpc/vanilla/mix')

# Exclude the ecluse-test-support library's modules, derived from test/support/ so
# a new Ecluse.Test.* module is dropped automatically. It is shared test
# scaffolding, not the software under test, and must never enter the report (see
# scripts/coverage.sh for the full rationale; codecov.yml `ignore` backs it up).
support_exclude=()
while IFS= read -r module; do
  support_exclude+=(-x "$module")
done < <(find test/support -name '*.hs' | sed -e 's#^test/support/##' -e 's#/#.#g' -e 's#\.hs$##')

# Flags mirror scripts/coverage.sh exactly so the combined JSON is shaped like the
# per-tier ones Codecov already ingests. Library-vs-test scoping otherwise stays in
# codecov.yml `ignore`, so new spec modules need no change here.
hpc-codecov "${mix_args[@]}" \
  -s . \
  -x Main \
  -x Paths_ecluse \
  "${support_exclude[@]}" \
  -f codecov \
  -o "$out" \
  "$combined_tix"

echo "coverage: wrote $out (combined core-unit ∪ app-unit ∪ integration — matches Codecov's merged total)"
