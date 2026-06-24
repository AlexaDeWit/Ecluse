#!/usr/bin/env bash
#
# Generate the COMBINED unit+integration coverage report — the picture Codecov
# shows. Codecov merges the per-tier flag uploads (unit ∪ integration) into one
# project total; this reproduces that total locally so a local read agrees with
# the dashboard. See CONTRIBUTING.md -> "Coverage" and docs/testing.md.
#
# Why combine, and why `--union`: Codecov sums the two flag reports, so we mirror
# that by ADDing the two suites' HPC .tix (hpc combine's default join). A library
# module the unit suite can't exercise (the SQS MirrorQueue backend, the worker's
# real fetch/publish path) is covered only by the integration tier, so without
# `--union` — whose default is the *intersection* of the module namespace — those
# modules would be dropped and the merged view would understate them exactly where
# it matters. `--union` keeps every module either suite touched. The library is
# built once into dist-coverage and linked into both suites, so the shared
# modules' .tix hashes match and combine cleanly.
#
# REQUIRES A DOCKER DAEMON — the integration tier drives ministack containers via
# testcontainers (see docs/testing.md -> "Integration tests"). For a fast,
# Docker-free loop use the single-tier path instead:
#   make coverage SUITE=ecluse-unit   (a PARTIAL view; Codecov merges it with the
#                                       integration tier — it under-counts every
#                                       module the integration tier exercises).
#
# Usage: scripts/coverage-combined.sh   (no arguments)
set -euo pipefail

builddir="dist-coverage"
outdir="coverage"
out="$outdir/combined.json"

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
  Codecov merges with the integration tier):

      make coverage SUITE=$unit_tier
EOF
  exit 1
fi

# Produce each tier's own report. scripts/coverage.sh leaves the suite's
# instrumented .tix under dist-coverage/ (which we combine below) AND writes
# coverage/<suite>.json — the same per-flag JSON CI uploads — so a combined run
# also refreshes both per-tier reports for free. SUITE-scoped runs print their own
# partial-view caveat; suppress it here since this script *is* the merged view.
echo "coverage: building $unit_tier (instrumented) ..."
ECLUSE_COVERAGE_QUIET_PARTIAL=1 bash "$(dirname "$0")/coverage.sh" "$unit_tier"
echo "coverage: building $integration_tier (instrumented, needs Docker) ..."
ECLUSE_COVERAGE_QUIET_PARTIAL=1 bash "$(dirname "$0")/coverage.sh" "$integration_tier"

unit_tix="$(find "$builddir" -type f -name "${unit_tier}.tix" | head -n1)"
integration_tix="$(find "$builddir" -type f -name "${integration_tier}.tix" | head -n1)"
if [ -z "$unit_tix" ] || [ -z "$integration_tix" ]; then
  echo "coverage: missing a tier .tix under $builddir/ (did both suites run?)" >&2
  exit 1
fi

# ADD the two tiers' tick counts (hpc combine's default join), unioning the module
# namespace so a module only one tier links is kept (see header).
mkdir -p "$outdir"
combined_tix="$builddir/combined.tix"
hpc combine --union --output="$combined_tix" "$unit_tix" "$integration_tix"

# Every HPC mix directory from the coverage build — both suites'. hpc-codecov only
# reads the ones the combined .tix references, so extra dirs are harmless (the same
# robustness argument as scripts/coverage.sh).
mix_args=()
while IFS= read -r d; do
  mix_args+=(-m "$d")
done < <(find "$builddir" -type d -path '*/hpc/vanilla/mix')

# Flags mirror scripts/coverage.sh exactly so the combined JSON is shaped like the
# per-tier ones Codecov already ingests. Library-vs-test scoping stays in
# codecov.yml `ignore`, so new spec modules need no change here.
hpc-codecov "${mix_args[@]}" \
  -s . \
  -x Main \
  -x Paths_ecluse \
  -f codecov \
  -o "$out" \
  "$combined_tix"

echo "coverage: wrote $out (combined unit ∪ integration — matches Codecov's merged total)"
