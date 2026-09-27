#!/usr/bin/env bash
# Load a docker-archive into the local Docker daemon and print the image reference it
# loaded. The Nix image builds with tag = null, so the reference carries a content-hash
# tag that only `docker load` reports.
#
# Usage: scripts/docker-load.sh <docker-archive>
set -euo pipefail

archive="${1:?usage: docker-load.sh <docker-archive>}"

loaded="$(docker load --input "$archive")"
ref="$(printf '%s\n' "$loaded" | awk -F'Loaded image: ' 'NF > 1 && ref == "" { ref = $2 } END { print ref }')"
if [ -z "$ref" ]; then
  echo "docker-load: could not determine the loaded image reference from 'docker load'" >&2
  exit 1
fi
printf '%s\n' "$ref"
