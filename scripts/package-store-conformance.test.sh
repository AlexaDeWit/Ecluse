#!/usr/bin/env bash
# Exercise report identity, missing evidence, and failed verdicts without a build.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/scripts"
cp "$here/package-store-conformance.sh" "$work/scripts/"
cat > "$work/bin/cabal" <<'STUB'
#!/usr/bin/env bash
case "$1" in
  build) exit "${TEST_BUILD_STATUS:-0}" ;;
  list-bin) printf '%s/bin/%s\n' "$TEST_WORK" "$2" ;;
  *) exit 2 ;;
esac
STUB
cat > "$work/bin/git" <<'STUB'
#!/usr/bin/env bash
printf 'test-source\n'
STUB
cat > "$work/scripts/test-containers.sh" <<'STUB'
#!/usr/bin/env bash
printf 'test-run\n'
STUB
cat > "$work/bin/subject" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
case "$(basename "$0")" in
  *-manifest) printf '{"subject":"%s"}\n' "${0##*/ecluse-conformance-}" ;;
  *-audit)
    [ "$#" -eq 2 ]
    [ "$(sha256sum "$1" | cut -d' ' -f1)" = "$ECLTEST_PLAN_DIGEST" ]
    jq -e --arg source "$ECLTEST_SOURCE" --arg run "$ECLTEST_RUN_ID" \
      '.source == $source and .run == $run and .passed == true' "$2" >/dev/null
    ;;
  ecluse-conformance-*)
    [ "$#" -eq 0 ]
    [ "$ECLTEST_REQUIRED" = 1 ]
    if [ "${TEST_SKIP_REPORT:-0}" != 1 ]; then
      jq -n --arg source "$ECLTEST_SOURCE" --arg run "$ECLTEST_RUN_ID" \
        --argjson passed "${TEST_PASSED:-true}" \
        '{source:$source,run:$run,passed:$passed}' > "$ECLTEST_REPORT_PATH"
    fi
    exit "${TEST_RUN_STATUS:-0}"
    ;;
  *) exit 2 ;;
esac
STUB
for subject in verdaccio codeartifact; do
  for suffix in '' -manifest -audit; do
    cp "$work/bin/subject" "$work/bin/ecluse-conformance-$subject$suffix"
  done
done
chmod +x "$work/bin/"* "$work/scripts/"*
export PATH="$work/bin:$PATH" TEST_WORK="$work"
cd "$work"

reject() {
  if "$@" > "$work/rejected.log" 2>&1; then
    echo "unexpected success: $*" >&2
    exit 1
  fi
}

bash scripts/package-store-conformance.sh verdaccio > "$work/verdaccio.log"
bash scripts/package-store-conformance.sh codeartifact > "$work/codeartifact.log"
for log in "$work/verdaccio.log" "$work/codeartifact.log"; do
  path="$(sed -n 's/^Package-store conformance artifacts: //p' "$log")"
  [ -s "$path/plan.json" ]
  [ -s "$path/report.json" ]
done
reject env TEST_SKIP_REPORT=1 bash scripts/package-store-conformance.sh verdaccio
reject env TEST_PASSED=false bash scripts/package-store-conformance.sh verdaccio
reject env TEST_RUN_STATUS=7 bash scripts/package-store-conformance.sh verdaccio
reject env TEST_BUILD_STATUS=9 bash scripts/package-store-conformance.sh verdaccio
reject bash scripts/package-store-conformance.sh unknown
reject bash scripts/package-store-conformance.sh verdaccio extra
printf 'ok - package-store contract reports and verdicts\n'
