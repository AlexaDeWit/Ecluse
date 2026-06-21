#!/usr/bin/env bash
#
# Build the GitHub Release body for a published image: the digest pin, the
# `gh attestation verify` recipe, and the auto-generated changelog (merged PRs
# since the previous tag). Run by release.yml; needs `gh`. See CONTRIBUTING.md →
# "Releases & container image".
#
# Usage: scripts/release-notes.sh <git-tag> <image> <digest>
set -euo pipefail

tag="$1"
image="$2"
digest="$3"
repo="${GITHUB_REPOSITORY:-AlexaDeWit/Ecluse}"

# Auto changelog (merged PRs since the previous tag); empty on the first release.
changelog=$(gh api "repos/$repo/releases/generate-notes" -f tag_name="$tag" --jq '.body' 2>/dev/null || true)

cat <<EOF
## Image

Pin deployments **by digest** (immutable; the tag is just a convenience pointer):

\`\`\`
${image}@${digest}
\`\`\`

## Verify

This image carries keyless **provenance** and **SBOM** attestations — immutable
OCI referrers plus the public Rekor transparency log. Verify them by digest:

\`\`\`bash
gh attestation verify "oci://${image}@${digest}" --repo ${repo}
\`\`\`

See [Verifying the image](https://github.com/${repo}#verifying-the-image) for the
full recipe (provenance, SBOM, and the reproducible rebuild).

${changelog}
EOF
