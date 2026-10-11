#!/usr/bin/env bash
# Verify artifact identity at the boundary without a Haskell build or Docker daemon.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/archive"
export ECLTEST_ARTIFACT_DIR="$work/prepared" ECLTEST_SOURCE=test-source ECLTEST_RUN_ID=test-run
export TEST_WORK="$work"
printf '{"architecture":"arm64"}\n' > "$work/archive/config.json"
printf '[{"Config":"config.json"}]\n' > "$work/archive/manifest.json"
tar -cf "$work/image.tar" -C "$work/archive" config.json manifest.json
cat > "$work/bin/nix" <<'STUB'
#!/usr/bin/env bash
printf '%s/image.tar\n' "$TEST_WORK"
STUB
cat > "$work/bin/cabal" <<'STUB'
#!/usr/bin/env bash
case "$1" in
  build) exit "${TEST_BUILD_FAIL:-0}" ;;
  list-bin) printf '%s/bin/runner\n' "$TEST_WORK" ;;
  *) exit 2 ;;
esac
STUB
cat > "$work/bin/runner" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
case "$1" in
  --catalogue-plan) echo '{"schemaVersion":1,"partitions":["npm-serving","npm-writes","pypi","shared-system"]}' ;;
  --audit) test "$#" = 3 ;;
  --shard)
    test "$2" = pypi
    test "$3" = --report
    test "$ECLTEST_CONTAINER_LOCK" = held
    echo '{}' > "$4"
    echo 'runner evidence'
    exit "${TEST_RUN_STATUS:-0}"
    ;;
  *) exit 2 ;;
esac
STUB
cat > "$work/bin/docker" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
case "$1" in
  load) echo 'Loaded image: ecluse:test' ;;
  info|ps|network) ;;
  image) printf '%s\n' "${TEST_IMAGE_ID:-sha256:wrong}" ;;
  *) exit 2 ;;
esac
STUB
chmod +x "$work/bin/"*
export PATH="$work/bin:$PATH"
script="$here/e2e.sh"

reject() {
  if "$@" > "$work/rejected.log" 2>&1; then
    echo "unexpected success: $*" >&2
    exit 1
  fi
}

bash "$script" prepare
export TEST_IMAGE_ID
TEST_IMAGE_ID="$(jq -r .image "$ECLTEST_ARTIFACT_DIR/identity.json")"
bash "$script" load
[ "$(cat "$ECLTEST_ARTIFACT_DIR/image-ref")" = ecluse:test ]
ECLTEST_SHARD=pypi bash "$script" run
[ -f "$ECLTEST_ARTIFACT_DIR/reports/pypi.json" ]
grep -q 'runner evidence' "$ECLTEST_ARTIFACT_DIR/reports/pypi.log"
status=0
ECLTEST_SHARD=pypi TEST_RUN_STATUS=7 bash "$script" run || status=$?
[ "$status" = 7 ]
reject env TEST_BUILD_FAIL=1 bash "$script" plan
rm "$ECLTEST_ARTIFACT_DIR/reports/pypi.json"
reject env ECLTEST_SOURCE=other bash "$script" load
reject env ECLTEST_RUN_ID=other bash "$script" load
reject env TEST_IMAGE_ID=sha256:wrong bash "$script" load
cp "$ECLTEST_ARTIFACT_DIR/plan.json" "$work/plan.json"
printf '\n' >> "$ECLTEST_ARTIFACT_DIR/plan.json"
reject bash "$script" load
cp "$work/plan.json" "$ECLTEST_ARTIFACT_DIR/plan.json"
printf 'corrupt' >> "$ECLTEST_ARTIFACT_DIR/docker-image.tar"
reject bash "$script" load
reject bash "$script" audit
mkdir -p "$ECLTEST_ARTIFACT_DIR/reports"
echo '{}' > "$ECLTEST_ARTIFACT_DIR/reports/npm-serving.json"
bash "$script" audit
bash "$here/e2e-matrix.sh" "$work/plan.json" > "$work/matrix"
grep -q 'partitions=\["npm-serving","npm-writes","pypi","shared-system"\]' "$work/matrix"
for bad in '{"partitions":[]}' '{"partitions":["pypi","pypi"]}' '{"partitions":["../escape"]}' '{}'; do
  printf '%s\n' "$bad" > "$work/bad.json"
  reject bash "$here/e2e-matrix.sh" "$work/bad.json"
done
printf 'ok - exact source, run, image, archive, plan and nonempty report checks\n'
