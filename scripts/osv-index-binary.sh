#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
# SPDX-License-Identifier: MIT
set -euo pipefail
source_dir="${1:?variant source directory}"
output="${2:?binary destination}"
root="$PWD"
cd "$source_dir"
binary="$(cabal list-bin exe:osv-index-experiment --enable-optimization=1 --disable-profiling)"
cp "$binary" "$root/$output"
