#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
#
# SPDX-License-Identifier: MIT
set -euo pipefail

# shellcheck source-path=SCRIPTDIR
# shellcheck source=common.sh
source "$(dirname -- "${BASH_SOURCE[0]}")/common.sh"
if [[ ! -s "$artifact_dir/raw.jsonl" ]]; then fail 'No measured samples to summarise'; fi
jq -s -f "$bundle_dir/summary.jq" "$artifact_dir/raw.jsonl" > "$artifact_dir/summary.json"
printf '%s\n' 'Results are descriptive. No performance threshold applies.'
