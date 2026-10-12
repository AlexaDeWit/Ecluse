#!/usr/bin/env bash
# Exercise explicit HPC conversion inputs without a compiler or an instrumented suite.
set -euo pipefail
repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/test/support/Support" "$work/test/contracts/Contract" \
  "$work/runtime/test-support/Runtime" "$work/dist-coverage/a/hpc/vanilla/mix"
export TEST_ARGS="$work/args"
cat > "$work/bin/hpc-codecov" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$@" > "$TEST_ARGS"
STUB
chmod +x "$work/bin/hpc-codecov"
export PATH="$work/bin:$PATH"
touch "$work/test/support/Support/Driver.hs" "$work/test/contracts/Contract/Plan.hs" \
  "$work/runtime/test-support/Runtime/Fixture.hs" "$work/input.tix"
cd "$work"
bash "$repo/scripts/coverage.sh" input.tix report/result.json
[ "$(tail -1 "$TEST_ARGS")" = input.tix ]
for value in dist-coverage/a/hpc/vanilla/mix report/result.json Support.Driver Contract.Plan Runtime.Fixture; do
  grep -Fxq "$value" "$TEST_ARGS"
done
printf 'ok - one HPC converter keeps explicit ticks and shared-support exclusions\n'
