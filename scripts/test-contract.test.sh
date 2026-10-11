#!/usr/bin/env bash
# Keep tier dispatch literal and reject unknown actions before invoking another script.
set -euo pipefail
root="$(git rev-parse --show-toplevel)"
cd "$root"
bash_path="$(command -v bash)"
mkdir -p scratchpad
fixture="$(mktemp -d scratchpad/contract-dispatch.XXXXXXXXXX)"
trap 'rm -rf "$fixture"' EXIT
fixture="$(realpath "$fixture")"
export ECLTEST_DISPATCH_LOG="$fixture/log"
cat > "$fixture/bash" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$ECLTEST_DISPATCH_LOG"
STUB
# An absolute interpreter avoids recursively finding this stub through PATH.
sed -i "1c#!$bash_path" "$fixture/bash"
chmod +x "$fixture/bash"
export PATH="$fixture:$PATH"
unset ECLTEST_CONTRACT_TIER
"$bash_path" scripts/test-contract.sh list
for tier in e2e integration; do
  for action in list plan; do
    ECLTEST_CONTRACT_TIER="$tier" "$bash_path" scripts/test-contract.sh "$action"
  done
done
cat > "$fixture/expected" <<'EXPECTED'
scripts/e2e.sh list
scripts/e2e.sh list
scripts/e2e.sh plan
scripts/test-integration.sh list
scripts/test-integration.sh plan
EXPECTED
cmp "$fixture/expected" "$ECLTEST_DISPATCH_LOG"
if ECLTEST_CONTRACT_TIER="integration'; touch $fixture/injected; #" "$bash_path" scripts/test-contract.sh list; then
  echo 'invalid tier passed' >&2
  exit 1
fi
if "$bash_path" scripts/test-contract.sh execute; then
  echo 'invalid action passed' >&2
  exit 1
fi
[ ! -e "$fixture/injected" ]
cmp "$fixture/expected" "$ECLTEST_DISPATCH_LOG"
echo 'contract dispatch checks passed'
