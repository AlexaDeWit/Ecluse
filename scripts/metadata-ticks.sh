#!/usr/bin/env bash
# Metadata tools must not overwrite the instrumented suite's native tick file.
set -euo pipefail
mkdir -p scratchpad/integration-discovery
directory="$(mktemp -d scratchpad/integration-discovery/run.XXXXXXXXXX)"
trap 'rm -f "$directory/metadata.tix"; rmdir "$directory"' EXIT
export HPCTIXFILE="$directory/metadata.tix"
exec_status=0
"$@" || exec_status=$?
exit "$exec_status"
