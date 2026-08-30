# shellcheck shell=bash
#
# Parse a release version into the two encodings a release uses: the ecluse.cabal
# `version:` field (X.Y.Z, no prefix, no suffix) and the git tag (vX.Y.Z, suffix kept).
# Sourced by init-release.sh and verify-release.sh so both accept the same input.

# Sets release_tag and release_cabal_version, clearing both on a malformed version.
# shellcheck disable=SC2034 # both are outputs, read by the sourcing script
release_version_parse() {
  local input="${1:-}" num='(0|[1-9][0-9]*)'
  release_tag=""
  release_cabal_version=""
  [[ "$input" =~ ^v?$num\.$num\.$num(-rc\.$num)?$ ]] || return 1
  local ver="${input#v}"
  release_tag="v$ver"
  # A candidate for X.Y.Z carries cabal version X.Y.Z (VERSIONING.md).
  release_cabal_version="${ver%%-*}"
}
