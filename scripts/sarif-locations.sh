#!/usr/bin/env bash
#
# Normalise artifact locations in a SARIF report so GitHub code scanning
# accepts the upload. Make every location URI inside the repository
# repo-relative, and point every other empty or absolute one at the given
# repo-relative path. Rewrites the file in place.
#
# Why: code scanning maps findings onto repository files. So it rejects an
# empty URI: grype over an SBOM has no filesystem path for a finding. It also
# cannot place an absolute file:// one, because osv-scanner anchors results to
# the lockfile's absolute path. The rejection only triggers once a scan has >= 1
# finding, so without this rewrite the upload works until the first real
# finding, then breaks. The target path is the repo file whose bump clears the
# finding: flake.lock for the image closure, cabal.project.freeze for the
# Haskell closure. This leaves a result that already carries a repo-relative
# path untouched.
#
# Usage: scripts/sarif-locations.sh <sarif-file> <repo-relative-path>
set -euo pipefail

sarif="${1:?usage: sarif-locations.sh <sarif-file> <repo-relative-path>}"
target="${2:?usage: sarif-locations.sh <sarif-file> <repo-relative-path>}"

root="$(git rev-parse --show-toplevel)"
tmp="$(mktemp)"
trap 'rm -f "$tmp"' EXIT

jq --arg target "$target" --arg root "$root/" '
  (.runs[]?.results[]?.locations[]?.physicalLocation.artifactLocation.uri,
   .runs[]?.artifacts[]?.location.uri)
    |= ((. // "") | ltrimstr("file://")
        | if startswith($root) then ltrimstr($root)
          elif . == "" or startswith("/") then $target
          else .
          end)
' "$sarif" >"$tmp"

mv "$tmp" "$sarif"
