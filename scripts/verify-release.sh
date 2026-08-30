#!/usr/bin/env bash
#
# Verify a published release the way an anonymous reader sees it: the digest pin in the
# release body, an anonymous fetch of that digest, the attestations, and both
# architectures. Uses `skopeo --no-creds` rather than logging docker out, so it proves
# the anonymous path without discarding the operator's stored credentials.
# See runbooks/release.md step 5.
#
# Usage: scripts/verify-release.sh v0.2.0
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/release-version.sh
. "$here/release-version.sh"

version="${1:-}"

die() {
  printf 'verify-release: %s\n' "$*" >&2
  exit 1
}

[ -n "$version" ] || die "pass a version, for example: task verify-release VERSION=v0.2.0"
release_version_parse "$version" ||
  die "VERSION '$version' is not X.Y.Z or X.Y.Z-rc.N (a leading 'v' is optional)"
tag="$release_tag"
repo="$(gh repo view --json nameWithOwner --jq .nameWithOwner)" || die "could not resolve the repository"

body="$(gh release view "$tag" --repo "$repo" --json body --jq .body)" ||
  die "no GitHub Release for $tag"

# The body pins IMAGE@sha256:... (scripts/release-notes.sh). That pin is what operators
# deploy, so it is the reference every later check reads, never the mutable tag.
# -m1 rather than a pipe to head: head can close the pipe first, and pipefail would
# then read grep's SIGPIPE as a failure to find the pin.
pinned="$(printf '%s' "$body" | grep -m1 -oE '[a-z0-9._/-]+@sha256:[0-9a-f]{64}')" ||
  die "the release body for $tag carries no digest pin"
echo "ok   digest pinned in the release body: $pinned"

index="$(skopeo inspect --no-creds --raw "docker://$pinned")" ||
  die "anonymous fetch of $pinned failed. The package may still be private"
echo "ok   anonymous fetch works"

arches="$(printf '%s' "$index" | jq -r '[.manifests[]?.platform | select(.os == "linux") | .architecture] | sort | join(",")')"
[ "$arches" = "amd64,arm64" ] || die "expected linux amd64 and arm64, found '${arches:-none}'"
echo "ok   both architectures present: $arches"

gh attestation verify "oci://$pinned" --repo "$repo" >/dev/null 2>&1 ||
  die "attestation verification failed for $pinned"
echo "ok   attestations verify"

printf '\n%s verified. The image carries attestations, not a cosign signature, so never announce it as signed.\n' "$tag"
