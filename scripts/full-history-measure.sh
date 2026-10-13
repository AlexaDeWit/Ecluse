#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
# SPDX-License-Identifier: MIT
set -euo pipefail
repository="$1"
evidence="$2"
build="$3"
pass_directory="$4"
mode="$5"
test "${build#"$evidence"/}" != "$build"
test "${pass_directory#"$evidence"/}" != "$pass_directory"
case "$mode" in
  native|marker-only|record) ;;
  *) printf 'invalid full-history mode: %s\n' "$mode" >&2; exit 1 ;;
esac
executable="$(cat "$build/binary-path.txt")"
export ECLUSE_BATCH_MODE="$mode"
export ECLUSE_BATCH_OUTPUT="$pass_directory/batches.csv"
export ECLUSE_FULL_OUTPUT="$pass_directory"
printf '%s\n' "ECLUSE_BATCH_MODE=$mode" "ECLUSE_BATCH_OUTPUT=$ECLUSE_BATCH_OUTPUT" "ECLUSE_FULL_OUTPUT=$ECLUSE_FULL_OUTPUT" "GHCRTS=${GHCRTS:-}" timeout=1800 kill-after=30 > "$pass_directory/environment.txt"
printf '%s\n' "$executable" --csv "$pass_directory/native.csv" --stdev 10 +RTS -T -RTS > "$pass_directory/command.txt"
bash "$repository/scripts/process-capture.sh" "$pass_directory/native.status" "$pass_directory/native.log" timeout --signal=TERM --kill-after=30s 30m "$executable" --csv "$pass_directory/native.csv" --stdev 10 +RTS -T -RTS
bash "$repository/scripts/process-status.sh" "$pass_directory/native.status"
