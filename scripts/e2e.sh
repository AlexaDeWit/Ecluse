#!/usr/bin/env bash
# Build and run the end-to-end suite (S53) — the CI `e2e` job gates on this.
#
# Builds the real OCI image (.#dockerImage), loads it into the local docker
# daemon, and runs `ecluse-e2e` against it with ECLUSE_E2E_IMAGE pointing at the
# loaded tag. The suite stands up the whole system as containers on an RFC 5737
# TEST-NET docker network and drives it with the real npm CLI, so this needs a
# Docker daemon and the npm CLI. set -euo pipefail below means an image-build or
# test failure exits non-zero, so the gate sees it. See
# planning/slices/S53-e2e-ecosystem.md.
set -euo pipefail

echo "==> building the OCI image (.#dockerImage)"
img_archive="$(nix build .#dockerImage --no-link --print-out-paths)"

echo "==> loading the image into docker"
tag="$(docker load <"$img_archive" | sed -n 's/^Loaded image: //p' | head -1)"
if [ -z "$tag" ]; then
	echo "could not determine the loaded image tag from 'docker load'" >&2
	exit 1
fi
echo "==> image: $tag"

echo "==> running ecluse-e2e"
ECLUSE_E2E_IMAGE="$tag" cabal test ecluse-e2e --test-show-details=direct
