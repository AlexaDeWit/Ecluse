#!/usr/bin/env bash
#
# Assemble the two per-arch Nix-built OCI archives into ONE multi-arch image index
# and push it under a single canonical immutable tag — so a consumer pulls
# `<image>:<tag>` and the registry serves amd64 or arm64 automatically, with no
# lingering per-arch tags. The two platform images land in the registry as
# digest-addressed blobs referenced by the index, not as named tags.
#
# The assembly is fully DAEMONLESS and rootless-safe: skopeo writes each archive
# into an on-disk OCI image layout (plain files — no container engine, no
# containers-storage, so no user namespace, which is what ubuntu-24.04's AppArmor
# blocks for /nix/store binaries), and `regctl` (regclient) builds the index from
# those layouts and copies it to the registry. No podman, no sudo.
#
# Run by release.yml after the matrix build; needs skopeo, regctl, jq (the
# `.#ci` shell) and an active Docker Hub login (release.yml's
# docker/login-action writes ~/.docker/config.json, which both skopeo and regctl
# read). See docs/architecture/release-supply-chain.md → "Multi-architecture image".
#
# Emits the resolved digests to stdout as `key=value` lines (index-digest,
# amd64-digest, arm64-digest) — and nothing else — so release.yml can append them
# to $GITHUB_OUTPUT for the attest steps. All tool chatter goes to stderr.
#
# Usage: scripts/push-multiarch.sh <image> <tag> <amd64-archive> <arm64-archive>
set -euo pipefail

image="$1"
tag="$2"
amd64_archive="$3"
arm64_archive="$4"

workdir="$(mktemp -d)"
trap 'rm -rf "$workdir"' EXIT
layout="$workdir/layout"

# Each per-arch archive (Nix builds them with tag=null) → an entry in a shared
# on-disk OCI image layout. Tool output to stderr so stdout stays clean for the
# digest lines.
skopeo copy "docker-archive:${amd64_archive}" "oci:${layout}:amd64" >&2
skopeo copy "docker-archive:${arm64_archive}" "oci:${layout}:arm64" >&2

# Build the multi-arch index from the two single-platform layout entries (regctl
# reads each entry's image config for its platform), then copy the index + both
# platform images to the registry under the single canonical tag.
regctl index create "ocidir://${layout}:multi" \
  --ref "ocidir://${layout}:amd64" \
  --ref "ocidir://${layout}:arm64" >&2
regctl image copy "ocidir://${layout}:multi" "${image}:${tag}" >&2

# Recover the digests for the attest steps: the index itself (what
# `gh attestation verify oci://IMAGE:TAG` resolves to) and each platform manifest.
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
