#!/usr/bin/env bash
# Require current tier evidence and pass its identity to the completion audit.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/e2e/reports" "$work/integration"
export TEST_WORK="$work" PATH="$work/bin:$PATH"
export ECLTEST_ARTIFACT_DIR="$work/e2e"
export ECLTEST_INTEGRATION_ARTIFACT_DIR="$work/integration"
export ECLTEST_SOURCE=test-source ECLTEST_RUN_ID=test-run ECLTEST_ARCHITECTURE=arm64

printf '{"image":"sha256:%064d"}\n' 0 > "$work/e2e/identity.json"
printf '{"plan":"e2e"}\n' > "$work/e2e/plan.json"
printf '{"subject":"npm"}\n' > "$work/e2e/reports/npm.json"
printf '{"plan":"integration"}\n' > "$work/integration/plan.json"
printf '{"subject":"integration"}\n' > "$work/integration/report.json"
cat > "$work/bin/cabal" <<'STUB'
#!/usr/bin/env bash
case "$1" in
  build) exit "${TEST_BUILD_STATUS:-0}" ;;
  list-bin) printf '%s/bin/audit\n' "$TEST_WORK" ;;
  *) exit 2 ;;
esac
STUB
cat > "$work/bin/audit" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
[ "$ECLTEST_REQUIRED" = 1 ]
[ "$ECLTEST_SOURCE" = test-source ]
[ "$ECLTEST_RUN_ID" = test-run ]
[ "$ECLTEST_ARCHITECTURE" = arm64 ]
[ "$ECLTEST_IMAGE_ID" = "sha256:$(printf '%064d' 0)" ]
[ "$#" -eq 4 ]
[ "$1" = "$TEST_WORK/integration/plan.json" ]
[ "$2" = "$TEST_WORK/integration/report.json" ]
[ "$3" = "$TEST_WORK/e2e/plan.json" ]
[ "$4" = "$TEST_WORK/e2e/reports/npm.json" ]
exit "${TEST_AUDIT_STATUS:-0}"
STUB
chmod +x "$work/bin/"*

reject() {
  if "$@" > "$work/rejected.log" 2>&1; then
    echo "unexpected success: $*" >&2
    exit 1
  fi
}

bash "$here/e2e-completion.sh" npm
reject env TEST_AUDIT_STATUS=7 bash "$here/e2e-completion.sh" npm
reject env TEST_BUILD_STATUS=9 bash "$here/e2e-completion.sh" npm
reject bash "$here/e2e-completion.sh" unknown
reject bash "$here/e2e-completion.sh" npm extra
mv "$work/integration/report.json" "$work/integration/held.json"
reject bash "$here/e2e-completion.sh" npm
mv "$work/integration/held.json" "$work/integration/report.json"
printf '{"image":"wrong"}\n' > "$work/e2e/identity.json"
reject bash "$here/e2e-completion.sh" npm
printf 'ok - ecosystem completion requires tier reports and matching identity\n'
