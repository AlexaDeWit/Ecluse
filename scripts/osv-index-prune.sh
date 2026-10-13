#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
# SPDX-License-Identifier: MIT
set -euo pipefail
sample="${1:?sample directory}"
case "$sample" in
  scratchpad/osv-index/samples/*/warm-*) exit 0 ;;
  scratchpad/osv-index/samples/*/[1-6]-[AB]) ;;
  *) exit 2 ;;
esac
if [ "$(cat "$sample/compare.status")" = 0 ]; then
  rm -rf -- "$sample/db"
fi
