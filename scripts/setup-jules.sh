#!/usr/bin/env bash
set -euo pipefail

# scripts/setup-jules.sh: bootstrap Nix in containerized environments (e.g. Jules).
#
# Derived from https://github.com/adrian-gierakowski/google-jules-nix
#
# LICENSE for the derived portion:
#
# Copyright 2026 Adrian Gierakowski
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the “Software”), to deal in the
# Software without restriction, including without limitation the rights to use,
# copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
# Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

# This script installs Nix, configures it for use within containers (disabling
# sandboxing and syscall filtering which often conflict with container runtimes),
# and pins nixpkgs to the project's canonical version.

NIXPKGS_COMMIT="${NIXPKGS_COMMIT:-e8210c649915deed7080033cdbabcc19e40bb899}"

echo "Starting Nix bootstrap for Écluse..."

# --- 1. Pre-installation configuration ---
# We pre-configure /etc/nix/nix.conf to ensure the installer succeeds in
# container environments (specifically disabling sandbox and filter-syscalls).
if [ ! -d /etc/nix ]; then
    sudo mkdir -p /etc/nix
fi

ensure_config() {
    local key="$1"
    local value="$2"
    if [ ! -f /etc/nix/nix.conf ]; then
        echo "$key = $value" | sudo tee -a /etc/nix/nix.conf > /dev/null
    elif grep -q "^$key =" /etc/nix/nix.conf; then
        sudo sed -i "s|^$key =.*|$key = $value|" /etc/nix/nix.conf
    else
        echo "$key = $value" | sudo tee -a /etc/nix/nix.conf > /dev/null
    fi
}

echo "Pre-configuring Nix for container compatibility..."
ensure_config "sandbox" "false"
ensure_config "filter-syscalls" "false"
ensure_config "experimental-features" "nix-command flakes"
ensure_config "connect-timeout" "10"
ensure_config "download-attempts" "5"
ensure_config "trusted-users" "root jules"

# --- 2. Install Nix if missing ---
NIX_WAS_INSTALLED=false
if ! command -v nix &> /dev/null && [ ! -e "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh" ]; then
    echo "Nix not found. Installing using Native Nix installer..."
    curl -L https://nixos.org/nix/install | sh -s -- --daemon --yes
    NIX_WAS_INSTALLED=true
fi

# Source nix-daemon for the current session
if [ -e "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh" ]; then
    # shellcheck source=/dev/null
    . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
fi

if ! command -v nix &> /dev/null; then
    echo "Error: Nix installation failed or not in PATH."
    exit 1
fi

if [ "$NIX_WAS_INSTALLED" = true ]; then
    echo "Re-applying container compatibility configuration after installation..."
    ensure_config "sandbox" "false"
    ensure_config "filter-syscalls" "false"
    ensure_config "experimental-features" "nix-command flakes"
    ensure_config "connect-timeout" "10"
    ensure_config "download-attempts" "5"
    ensure_config "trusted-users" "root jules"
fi

# --- 3. Daemon management and verification ---
# Ensure nix-daemon is running and accessible (robust against systemd failures in containers)
echo "Verifying nix-daemon connection..."
RESTART_DAEMON=false
if [ "$NIX_WAS_INSTALLED" = true ]; then
    RESTART_DAEMON=true
fi
SOCKET="/nix/var/nix/daemon-socket/socket"

if [ ! -S "$SOCKET" ]; then
    echo "Socket $SOCKET not found. Nix daemon might not be running correctly."
    RESTART_DAEMON=true
fi

if [ "$RESTART_DAEMON" = true ]; then
    if command -v systemctl &> /dev/null; then
         echo "Attempting restart via systemctl..."
         sudo systemctl restart nix-daemon || true
         sleep 2
    fi

    if [ ! -S "$SOCKET" ]; then
         echo "Socket still not found. Trying manual start..."
         if command -v systemctl &> /dev/null; then
            sudo systemctl stop nix-daemon nix-daemon.socket || true
         fi

         DAEMON_BIN="/nix/var/nix/profiles/default/bin/nix-daemon"
         if [ -x "$DAEMON_BIN" ]; then
             sudo "$DAEMON_BIN" --daemon 2>&1 | sudo tee /var/log/nix-daemon.log > /dev/null &
             sleep 2
             if [ -S "$SOCKET" ]; then
                 echo "nix-daemon started manually."
             else
                 echo "Failed to start nix-daemon manually. Check /var/log/nix-daemon.log"
             fi
         else
             echo "Error: nix-daemon binary not found at $DAEMON_BIN."
         fi
    fi
else
    echo "nix-daemon socket found."
fi

# --- 4. Pin nixpkgs in the registry ---
echo "Pinning nixpkgs to $NIXPKGS_COMMIT..."
nix --extra-experimental-features "nix-command flakes" registry pin nixpkgs "github:NixOS/nixpkgs/$NIXPKGS_COMMIT"

# --- 5. Pre-arm Nix shells ---
echo "Pre-warming Nix cache (realizing dev shells)..."
nix --extra-experimental-features "nix-command flakes" print-dev-env . > /dev/null
nix --extra-experimental-features "nix-command flakes" print-dev-env .#ci > /dev/null
nix --extra-experimental-features "nix-command flakes" print-dev-env .#mcp > /dev/null

echo "Nix bootstrap complete."
nix --version
