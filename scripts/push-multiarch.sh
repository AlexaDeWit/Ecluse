#!/usr/bin/env bash
#
# Push the multi-arch image that assemble-multiarch.sh builds under one canonical
# immutable tag. A consumer pulls `<image>:<tag>` and the registry serves amd64 or arm64.
# The two platform images land as digest-addressed blobs the index references, not as
# named tags.
#
# Run by release.yml's publish job. Needs skopeo, regctl, and jq (the `.#ci` shell), plus
# an active ghcr.io login in ~/.docker/config.json, which regctl reads. See
# docs/architecture/release-supply-chain.md, "Multi-architecture image".
#
# Prints the resolved digests to stdout as `key=value` lines (index-digest, amd64-digest,
# arm64-digest) for $GITHUB_OUTPUT, and nothing else. Tool output goes to stderr.
#
# Usage: scripts/push-multiarch.sh <image> <tag> <amd64-archive> <arm64-archive>
set -euo pipefail

image="$1"
tag="$2"
amd64_archive="$3"
arm64_archive="$4"

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
workdir="$(mktemp -d)"
trap 'rm -rf "$workdir"' EXIT
layout="$workdir/layout"

bash "$here/assemble-multiarch.sh" "$layout" "$amd64_archive" "$arm64_archive"
regctl image copy "ocidir://${layout}:multi" "${image}:${tag}" >&2

# The attest steps need the index digest (what `gh attestation verify oci://IMAGE:TAG`
# resolves to) and each platform manifest's.
index_digest="$(regctl manifest head "${image}:${tag}")"
raw="$(regctl manifest get "${image}:${tag}" --format raw-body)"
amd64_digest="$(printf '%s' "$raw" | jq -r '.manifests[] | select(.platform.os == "linux" and .platform.architecture == "amd64") | .digest')"
arm64_digest="$(printf '%s' "$raw" | jq -r '.manifests[] | select(.platform.os == "linux" and .platform.architecture == "arm64") | .digest')"

for pair in "index:$index_digest" "amd64:$amd64_digest" "arm64:$arm64_digest"; do
  name="${pair%%:*}"
  digest="${pair#*:}"
  if [ -z "$digest" ] || [ "$digest" = "null" ]; then
    echo "error: could not resolve ${name} digest from the pushed index" >&2
    exit 1
  fi
  echo "${name}-digest=${digest}"
done
