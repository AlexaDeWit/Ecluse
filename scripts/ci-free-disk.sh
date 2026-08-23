#!/usr/bin/env bash
# Remove the runner's preinstalled toolchains, but only when the root filesystem is
# actually short of space. The removal costs about a minute and frees ~24 GB, while
# the current runner image starts with 87 GB free of 145 GB. The threshold clears the
# ~30 GB the heaviest job adds: the Nix store, the Docker data-root, the cabal store,
# and the e2e image.
set -euo pipefail

threshold_gb="${CI_FREE_DISK_THRESHOLD_GB:-40}"
avail_gb="$(df -BG --output=avail / | tail -n 1 | tr -dc '0-9')"

if [ "$avail_gb" -ge "$threshold_gb" ]; then
  echo "free-disk: / has ${avail_gb} GB free, at or above the ${threshold_gb} GB threshold. Nothing removed."
  exit 0
fi

echo "free-disk: / has ${avail_gb} GB free, below the ${threshold_gb} GB threshold. Removing preinstalled toolchains."
df -h /
sudo rm -rf \
  /usr/local/lib/android \
  /usr/share/dotnet \
  /usr/share/swift \
  /opt/ghc \
  /usr/local/.ghcup
df -h /
