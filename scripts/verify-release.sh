#!/usr/bin/env bash
#
# Verify a published release: the digest pin in the release body, that the pin belongs
# to this project and to this version's tag, an unauthenticated fetch of it, both
# architectures, and the attestations. See runbooks/release.md step 5.
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

# gh is not in the flake devshell, so a clear refusal beats a bare "command not found".
for tool in git gh jq skopeo; do
  command -v "$tool" >/dev/null || die "$tool is not on PATH, and verify-release needs it"
done

[ -n "$version" ] || die "pass a version, for example: task verify-release VERSION=v0.2.0"
release_version_parse "$version" ||
  die "'$version' is not X.Y.Z or X.Y.Z-rc.N (a leading 'v' is optional)"
tag="$release_tag"

repo="$(git remote get-url origin | sed -E 's#^(git@github\.com:|https://github\.com/)##; s#\.git$##')"
[[ "$repo" == */* ]] || die "could not read an owner/name repository from origin"
# Matches IMAGE in .github/workflows/release.yml, which lowercases the repository.
image="ghcr.io/$(printf '%s' "$repo" | tr '[:upper:]' '[:lower:]')"

release="$(gh release view "$tag" --repo "$repo" --json body,isDraft)" || die "no GitHub Release for $tag"
[ "$(jq -r .isDraft <<<"$release")" = "false" ] ||
  die "the Release for $tag is still a draft, so no anonymous reader can see it"

# The body pins IMAGE@sha256:... (scripts/release-notes.sh). That pin is what operators
# deploy, so every later check reads it rather than the mutable tag.
body="$(jq -r .body <<<"$release")"
pinned="$(grep -m1 -oE "$image@sha256:[0-9a-f]{64}" <<<"$body")" ||
  die "the release body for $tag pins no $image digest. Step 6 replaces this body by hand, so check it kept the pin"
digest="${pinned#*@}"
echo "ok   digest pinned in the release body: $pinned"

# Ties the pin to the version's own image tag, so a body copied from an earlier release
# cannot verify that release's image and report this one as good.
tagged_hex="$(skopeo inspect --no-creds --raw "docker://$image:${tag#v}" 2>/dev/null | sha256sum | cut -d' ' -f1)" ||
  die "unauthenticated fetch of $image:${tag#v} failed. The package may still be private"
[ "sha256:$tagged_hex" = "$digest" ] ||
  die "$image:${tag#v} resolves to sha256:$tagged_hex, not the pinned $digest"
echo "ok   unauthenticated fetch works, and the tag resolves to the pinned digest"

index="$(skopeo inspect --no-creds --raw "docker://$pinned")" || die "unauthenticated fetch of $pinned failed"
for want in amd64 arm64; do
  jq -e --arg a "$want" 'any(.manifests[]?.platform; .os == "linux" and .architecture == $a)' <<<"$index" >/dev/null ||
    die "the index carries no linux/$want"
done
echo "ok   linux/amd64 and linux/arm64 both present"

gh attestation verify "oci://$pinned" --repo "$repo" >/dev/null 2>&1 ||
  die "attestation verification failed for $pinned"
echo "ok   attestations verify"

printf '\n%s verified. The image carries attestations, not a cosign signature, so never announce it as signed.\n' "$tag"
