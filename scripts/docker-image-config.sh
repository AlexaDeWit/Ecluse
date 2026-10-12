#!/usr/bin/env bash
# Export the daemon image to compare its configuration with the prepared archive.
set -euo pipefail
root="${1:?artifact root}"
reference="${2:?image reference}"
actual="$(docker image inspect --format '{{.Id}}' "$reference")"
[[ "$actual" =~ ^sha256:[a-f0-9]{64}$ ]] || { echo "invalid loaded image ID: $actual" >&2; exit 1; }
directory="$(mktemp -d "$root/.image-check.XXXXXXXXXX")"
trap 'rm -rf "$directory"' EXIT
docker image save --output "$directory/image.tar" "$actual"
bash scripts/image-archive.sh configuration "$directory/image.tar" > "$root/loaded-config"
printf '%s\n' "$actual" > "$root/loaded-ref"
