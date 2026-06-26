#!/usr/bin/env bash
#
# Backfill artifact locations in a grype SARIF report so GitHub code scanning
# accepts the upload. Rewrites the file in place. See CONTRIBUTING.md ->
# "Vulnerability scanning".
#
# grype scanning an SBOM source (`grype sbom:...`) has no filesystem path for a
# finding -- the SBOM enumerates Nix store components, not files -- so it emits
# every result with an empty physicalLocation.artifactLocation.uri (""). GitHub
# code scanning rejects an empty URI ("locationFromSarifResult: expected artifact
# location") and refuses the whole file, but only once the scan has >=1 finding
# (an empty results array uploads fine). So the upload works until the closure
# grows its first CVE, then breaks silently on every run.
#
# Fix: point every locationless result at flake.lock -- a real repo path (so code
# scanning is satisfied) and the honest remediation target, since flake.lock pins
# the whole dependency closure and bumping it is how a closure CVE is cleared. A
# result that already carries a real path (should grype ever emit one) is left
# untouched.
#
# Usage: scripts/grype-sarif-locations.sh <sarif-file>
set -euo pipefail

sarif="${1:?usage: grype-sarif-locations.sh <sarif-file>}"

tmp="$(mktemp)"
trap 'rm -f "$tmp"' EXIT

jq '
  .runs[].results[]?.locations[]?.physicalLocation.artifactLocation.uri
    |= (if (. // "") == "" then "flake.lock" else . end)
' "$sarif" >"$tmp"

mv "$tmp" "$sarif"
