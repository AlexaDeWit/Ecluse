#!/usr/bin/env bash
#
# Assemble the two per-arch Nix-built image archives into one multi-arch OCI index, the
# `multi` entry of an on-disk OCI layout. It pushes nothing: push-multiarch.sh copies the
# result to the registry, and ci.yml's release dry-run stops here.
#
# Daemonless and rootless: skopeo writes each archive into a layout of plain files, and
# regctl builds the index from those entries. No container engine and no user namespace,
# which Ubuntu's AppArmor policy blocks for /nix/store binaries.
#
# All output goes to stderr. Needs skopeo, regctl, and jq (the `.#ci` shell).
#
# Usage: scripts/assemble-multiarch.sh <layout-dir> <amd64-archive> <arm64-archive>
set -euo pipefail

layout="${1:?usage: assemble-multiarch.sh <layout-dir> <amd64-archive> <arm64-archive>}"
amd64_archive="${2:?usage: assemble-multiarch.sh <layout-dir> <amd64-archive> <arm64-archive>}"
arm64_archive="${3:?usage: assemble-multiarch.sh <layout-dir> <amd64-archive> <arm64-archive>}"

# An explicit trust policy, so the copy does not depend on the runner image shipping
# /etc/containers/policy.json. It admits the two local archives and nothing else.
policy="$(mktemp)"
trap 'rm -f "$policy"' EXIT
cat > "$policy" <<'JSON'
{
  "default": [{"type": "reject"}],
  "transports": {"docker-archive": {"": [{"type": "insecureAcceptAnything"}]}}
}
JSON

skopeo --policy "$policy" copy "docker-archive:${amd64_archive}" "oci:${layout}:amd64" >&2
skopeo --policy "$policy" copy "docker-archive:${arm64_archive}" "oci:${layout}:arm64" >&2

for arch in amd64 arm64; do
  config_arch="$(regctl image config "ocidir://${layout}:${arch}" --format '{{.Architecture}}')"
  if [ "$config_arch" != "$arch" ]; then
    echo "error: the ${arch} archive's image config names architecture '${config_arch}'" >&2
    exit 1
  fi
done

# regctl reads each entry's platform from its image config.
regctl index create "ocidir://${layout}:multi" \
  --ref "ocidir://${layout}:amd64" \
  --ref "ocidir://${layout}:arm64" >&2

platforms="$(regctl manifest get "ocidir://${layout}:multi" --format raw-body \
  | jq -r '[.manifests[].platform | "\(.os)/\(.architecture)"] | sort | join(" ")')"
if [ "$platforms" != "linux/amd64 linux/arm64" ]; then
  echo "error: the index lists '${platforms}', expected 'linux/amd64 linux/arm64'" >&2
  exit 1
fi
echo "assembled ${layout}:multi over ${platforms}" >&2
