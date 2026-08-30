#!/usr/bin/env bash
#
# Stage a release's attestation bundles and SBOMs under their published asset names,
# for `gh release create` to upload. GHCR already holds the same documents as OCI
# referrers, and GitHub's attestations API serves them too. A release asset is the
# copy pinned to the release object, and the only copy OpenSSF Scorecard's
# Signed-Releases check can see. Run by release.yml after the attest steps. See
# docs/architecture/release-supply-chain.md → "Supply-chain attestations".
#
# Every source path arrives through the environment, so release.yml's run script
# interpolates nothing:
#   PROVENANCE_INDEX, PROVENANCE_AMD64, PROVENANCE_ARM64  attest-build-provenance bundles
#   SBOM_BUNDLE_AMD64, SBOM_BUNDLE_ARM64                  attest-sbom bundles
#   SBOM_AMD64, SBOM_ARM64                                the SPDX documents themselves
#
# Emits the staged paths to stdout, one per line, and nothing else.
#
# Usage: scripts/release-assets.sh <git-tag> <staging-dir>
set -euo pipefail

tag="$1"
outdir="$2"
version="${tag#v}"

# `.sigstore.json` is the Sigstore bundle's own extension: each staged file is
# exactly what `gh attestation verify --bundle` reads.
assets=(
  "provenance.sigstore.json:PROVENANCE_INDEX"
  "amd64-provenance.sigstore.json:PROVENANCE_AMD64"
  "arm64-provenance.sigstore.json:PROVENANCE_ARM64"
  "amd64-sbom.sigstore.json:SBOM_BUNDLE_AMD64"
  "arm64-sbom.sigstore.json:SBOM_BUNDLE_ARM64"
  "amd64-sbom.spdx.json:SBOM_AMD64"
  "arm64-sbom.spdx.json:SBOM_ARM64"
)

# Check every input before copying any of it. An attest step that silently produced
# nothing would otherwise leave a half-staged directory, and a release body promising
# an attestation the release does not carry.
for entry in "${assets[@]}"; do
  var="${entry#*:}"
  src="${!var-}"
  if [ -z "$src" ]; then
    echo "error: \$${var} is unset; release.yml must pass every bundle and SBOM path" >&2
    exit 1
  fi
  if [ ! -s "$src" ]; then
    echo "error: \$${var} points at '${src}', which is missing or empty" >&2
    exit 1
  fi
done

mkdir -p "$outdir"

for entry in "${assets[@]}"; do
  suffix="${entry%%:*}"
  var="${entry#*:}"
  dest="${outdir}/ecluse-${version}-${suffix}"
  cp "${!var}" "$dest"
  echo "$dest"
done
