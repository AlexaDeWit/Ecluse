#!/usr/bin/env bash
#
# Normalise artifact locations in a SARIF report so GitHub code scanning
# accepts the upload: every location URI that is empty or absolute is pointed
# at the given repo-relative path. Rewrites the file in place.
#
# Why: code scanning maps findings onto repository files, so it rejects an
# empty URI (grype over an SBOM has no filesystem path for a finding) and
# cannot place an absolute file:// one (osv-scanner anchors results to the
# lockfile's absolute path). The rejection only triggers once a scan has >= 1
# finding, so without this rewrite the upload works until the first real
# finding, then breaks. The target path is the repo file whose bump clears the
# finding: flake.lock for the image closure, cabal.project.freeze for the
# Haskell closure. A result already carrying a repo-relative path is left
# untouched.
#
# Usage: scripts/sarif-locations.sh <sarif-file> <repo-relative-path>
set -euo pipefail

sarif="${1:?usage: sarif-locations.sh <sarif-file> <repo-relative-path>}"
target="${2:?usage: sarif-locations.sh <sarif-file> <repo-relative-path>}"

tmp="$(mktemp)"
trap 'rm -f "$tmp"' EXIT

jq --arg target "$target" '
  (.runs[]?.results[]?.locations[]?.physicalLocation.artifactLocation.uri,
   .runs[]?.artifacts[]?.location.uri)
    |= (if (. // "") == "" or startswith("file://") or startswith("/")
        then $target
        else .
        end)
' "$sarif" >"$tmp"

mv "$tmp" "$sarif"
