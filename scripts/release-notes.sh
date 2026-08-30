#!/usr/bin/env bash
#
# Build the GitHub Release body for a published image: the digest pin, the
# `gh attestation verify` recipe, the attestation assets that release.yml uploads
# alongside it (see release-assets.sh), and the auto-generated changelog (merged
# PRs since the previous tag). Run by release.yml. Needs `gh`. See CONTRIBUTING.md →
# "Releases".
#
# Usage: scripts/release-notes.sh <git-tag> <image> <digest>
set -euo pipefail

tag="$1"
image="$2"
digest="$3"
repo="${GITHUB_REPOSITORY:-AlexaDeWit/Ecluse}"
version="${tag#v}"

# Auto changelog (merged PRs since the previous tag). With no earlier tag the body can
# come back sparse or over-inclusive, so the runbook replaces it after publish.
changelog=$(gh api "repos/$repo/releases/generate-notes" -f tag_name="$tag" --jq '.body' 2>/dev/null || true)

cat <<EOF
## Image

Pin deployments **by digest** (immutable; the tag is just a convenience pointer):

\`\`\`
${image}@${digest}
\`\`\`

## Verify

This image carries keyless **provenance** and **SBOM** attestations, recorded in the
public Rekor transparency log. \`verify\` checks one predicate type per run, and
provenance is its default:

\`\`\`bash
gh attestation verify "oci://${image}@${digest}" --repo ${repo}
\`\`\`

The SBOM is attested per platform, never on the index digest above, so name a platform
digest (\`docker manifest inspect\` lists them) and the SPDX predicate type:

\`\`\`bash
gh attestation verify "oci://${image}@<platform-digest>" --repo ${repo} \\
  --predicate-type https://spdx.dev/Document/v2.3
\`\`\`

## Attestation assets

Both commands above read GitHub's attestations API. The same documents ship as assets on
this release, so you can verify a copy pinned to the release object, and read either SBOM
with no verify tool at all.

| Asset | Covers |
| --- | --- |
| \`ecluse-${version}-provenance.sigstore.json\` | SLSA provenance for the multi-arch index |
| \`ecluse-${version}-amd64-provenance.sigstore.json\` | SLSA provenance for the \`linux/amd64\` image |
| \`ecluse-${version}-arm64-provenance.sigstore.json\` | SLSA provenance for the \`linux/arm64\` image |
| \`ecluse-${version}-amd64-sbom.sigstore.json\` | SPDX attestation for the \`linux/amd64\` image |
| \`ecluse-${version}-arm64-sbom.sigstore.json\` | SPDX attestation for the \`linux/arm64\` image |
| \`ecluse-${version}-amd64-sbom.spdx.json\` | The \`linux/amd64\` SPDX document itself |
| \`ecluse-${version}-arm64-sbom.spdx.json\` | The \`linux/arm64\` SPDX document itself |

Point \`--bundle\` at a downloaded bundle to verify that copy instead of a fetched one:

\`\`\`bash
gh release download ${tag} -p 'ecluse-*-provenance.sigstore.json'
gh attestation verify "oci://${image}@${digest}" --repo ${repo} \\
  --bundle ecluse-${version}-provenance.sigstore.json
\`\`\`

A \`-sbom.sigstore.json\` bundle takes \`--predicate-type\` as well. The two
\`.spdx.json\` assets are the SPDX documents themselves, not bundles: read those
directly.

See [Verifying the image](https://github.com/${repo}#verifying-the-image) for the
full recipe (provenance, SBOM, and the reproducible rebuild).

${changelog}
EOF
