#!/usr/bin/env bash
#
# Assemble the two per-arch Nix-built OCI archives into ONE multi-arch manifest
# list and push it under a single canonical immutable tag — so a consumer pulls
# `<image>:<tag>` and the registry serves amd64 or arm64 automatically, with no
# lingering per-arch tags. The two platform images land in the registry as
# digest-addressed blobs referenced by the index, not as named tags. The list is
# built LOCALLY (podman) from the archives, so only the one canonical tag is
# pushed — unlike `docker buildx imagetools create`, which would need the platform
# images pre-pushed under their own (permanent, since tags are immutable) tags.
#
# Run by release.yml after the matrix build; needs skopeo, podman, jq (the
# `.#release` shell) and an active Docker Hub login (release.yml's
# docker/login-action writes ~/.docker/config.json, which podman/skopeo read).
# See docs/architecture/release-supply-chain.md → "Multi-architecture image".
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

# A deterministic, fuse-free, rootless-safe local store shared by skopeo and
# podman. vfs needs no overlay/fuse support (robust on any CI runner); the images
# are tiny (~23 MB) so its copy-up cost is irrelevant. CONTAINERS_STORAGE_CONF is
# honoured by skopeo and podman alike.
workdir="$(mktemp -d)"
trap 'rm -rf "$workdir"' EXIT
export CONTAINERS_STORAGE_CONF="$workdir/storage.conf"
cat >"$CONTAINERS_STORAGE_CONF" <<EOF
[storage]
driver = "vfs"
graphroot = "$workdir/graph"
runroot = "$workdir/run"
EOF

# Import each per-arch archive (Nix builds them with tag=null) under a known
# local name. Tool output to stderr so stdout stays clean for the digest lines.
skopeo copy "docker-archive:${amd64_archive}" "containers-storage:localhost/ecluse-stage:amd64" >&2
skopeo copy "docker-archive:${arm64_archive}" "containers-storage:localhost/ecluse-stage:arm64" >&2

# Build the index locally and push ONLY the canonical tag (--all pushes both
# platform images plus the index that references them).
list="ecluse-multiarch:${tag}"
podman manifest create "$list" >&2
podman manifest add "$list" containers-storage:localhost/ecluse-stage:amd64 >&2
podman manifest add "$list" containers-storage:localhost/ecluse-stage:arm64 >&2
podman manifest push --all "$list" "docker://${image}:${tag}" >&2

# Recover the digests from the pushed index for the attest steps: the index
# itself (what `gh attestation verify oci://IMAGE:TAG` resolves to) and each
# platform manifest it references.
raw="$(skopeo inspect --raw "docker://${image}:${tag}")"
index_digest="$(skopeo inspect "docker://${image}:${tag}" --format '{{.Digest}}')"
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
