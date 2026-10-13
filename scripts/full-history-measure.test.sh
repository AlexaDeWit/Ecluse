#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
# SPDX-License-Identifier: MIT
set -euo pipefail
repository="$(git rev-parse --show-toplevel)"
mkdir -p "$repository/scratchpad"
fixture="$(mktemp -d "$repository/scratchpad/full-history-argv.XXXXXX")"
trap 'rm -rf "$fixture"' EXIT
mkdir -p "$fixture/bin" "$fixture/build" "$fixture/pass"
printf '%s\n' "$fixture/benchmark with spaces" > "$fixture/build/binary-path.txt"
cat > "$fixture/bin/timeout" <<'SHIM'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$@" > "$FULL_ARGV_FIXTURE/actual-argv.txt"
printf '%s\n' "$ECLUSE_BATCH_MODE" "$ECLUSE_BATCH_OUTPUT" "$ECLUSE_FULL_OUTPUT" > "$FULL_ARGV_FIXTURE/actual-env.txt"
exit "${FULL_ARGV_STATUS:-0}"
SHIM
chmod +x "$fixture/bin/timeout"
for mode in native marker-only record; do
  env -u GHCRTS PATH="$fixture/bin:$PATH" FULL_ARGV_FIXTURE="$fixture" bash "$repository/scripts/full-history-measure.sh" "$repository" "$fixture" "$fixture/build" "$fixture/pass" "$mode"
  printf '%s\n' --signal=TERM --kill-after=30s 30m "$fixture/benchmark with spaces" --csv "$fixture/pass/native.csv" --stdev 10 +RTS -T -RTS > "$fixture/expected-argv.txt"
  cmp "$fixture/expected-argv.txt" "$fixture/actual-argv.txt"
  printf '%s\n' "$mode" "$fixture/pass/batches.csv" "$fixture/pass" > "$fixture/expected-env.txt"
  cmp "$fixture/expected-env.txt" "$fixture/actual-env.txt"
  test "$(cat "$fixture/pass/native.status")" = 0
done
if env -u GHCRTS PATH="$fixture/bin:$PATH" FULL_ARGV_FIXTURE="$fixture" FULL_ARGV_STATUS=124 bash "$repository/scripts/full-history-measure.sh" "$repository" "$fixture" "$fixture/build" "$fixture/pass" record; then
  printf 'failed native launch unexpectedly passed\n' >&2
  exit 1
fi
test "$(cat "$fixture/pass/native.status")" = 124
printf 'Full-history arguments, modes, external guard and failed status pass. No benchmark executed.\n'
