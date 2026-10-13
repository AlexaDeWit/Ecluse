#!/usr/bin/env bash
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/release-version.sh
. "$here/release-version.sh"

release_tag="" release_cabal_version=""
tag="${INPUT_TAG:-${REF_NAME:-}}"
if ! release_version_parse "$tag"; then
  printf 'Invalid release version: %q\n' "$tag" >&2
  exit 1
fi
if [ "$release_cabal_version" != "$1" ]; then
  printf "Release base '%s' != ecluse.cabal version '%s'\n" "$release_cabal_version" "$1" >&2
  exit 1
fi
printf 'image-tag=%s\ngit-tag=%s\n' "${release_tag#v}" "$release_tag"
