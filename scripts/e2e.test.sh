#!/usr/bin/env bash
# Verify artifact identity at the boundary without a Haskell build or Docker daemon.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
work="$(mktemp -d)"
full_roots=()
full_pids=()
trap 'rm -rf "$work" "${full_roots[@]}"' EXIT
mkdir -p "$work/bin" "$work/archive"
export ECLTEST_ARTIFACT_DIR="$work/prepared" ECLTEST_SOURCE=test-source ECLTEST_RUN_ID=test-run
export TEST_WORK="$work"
printf '{"architecture":"arm64"}\n' > "$work/archive/config.json"
printf '[{"Config":"config.json"}]\n' > "$work/archive/manifest.json"
tar -cf "$work/image.tar" -C "$work/archive" config.json manifest.json
cat > "$work/bin/nix" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$ECLTEST_ARTIFACT_DIR" >> "$TEST_WORK/preparations"
printf '%s/image.tar\n' "$TEST_WORK"
STUB
cat > "$work/bin/cabal" <<'STUB'
#!/usr/bin/env bash
case "$1" in
  build) exit "${TEST_BUILD_FAIL:-0}" ;;
  list-bin) printf '%s/bin/%s\n' "$TEST_WORK" "$2" ;;
  *) exit 2 ;;
esac
STUB
cat > "$work/bin/runner" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
case "$(basename "$0")" in
  ecluse-e2e-manifest) echo '{"schemaVersion":1,"partitions":["npm-serving","npm-writes","pypi","shared-system"]}' ;;
  ecluse-e2e-audit)
    shift
    for report in "$@"; do jq -e '.passed == true' "$report" >/dev/null; done
    ;;
  ecluse-e2e-*)
    test "$#" = 0
    test "$ECLTEST_CONTAINER_LOCK" = held
    echo '{"passed":true}' > "$ECLTEST_REPORT_PATH"
    if [ "${TEST_BLOCK_RUN:-}" = 1 ]; then
      touch "$TEST_WORK/running"
      while [ ! -f "$TEST_WORK/release-run" ]; do sleep 0.01; done
    fi
    echo 'runner evidence'
    exit "${TEST_RUN_STATUS:-0}"
    ;;
  *) exit 2 ;;
esac
STUB
for component in manifest audit npm-serving npm-writes pypi shared-system; do
  cp "$work/bin/runner" "$work/bin/ecluse-e2e-$component"
done
cat > "$work/bin/docker" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
case "$1" in
  load) echo 'Loaded image: ecluse:test' ;;
  info) exit "${TEST_DOCKER_STATUS:-0}" ;;
  ps|network) ;;
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
bash "$script" pypi
[ -f "$ECLTEST_ARTIFACT_DIR/reports/pypi.json" ]
grep -q 'runner evidence' "$ECLTEST_ARTIFACT_DIR/reports/pypi.log"
bash "$script" audit
reject env TEST_DOCKER_STATUS=23 bash "$script" pypi
[ ! -e "$ECLTEST_ARTIFACT_DIR/reports/pypi.json" ]
reject bash "$script" audit
bash "$script" pypi
mv "$ECLTEST_ARTIFACT_DIR/reports/pypi.json" "$ECLTEST_ARTIFACT_DIR/reports/all.json"
reject env TEST_DOCKER_STATUS=23 bash "$script" pypi
[ ! -e "$ECLTEST_ARTIFACT_DIR/reports/pypi.json" ]
[ ! -e "$ECLTEST_ARTIFACT_DIR/reports/all.json" ]
reject bash "$script" audit
find "$ECLTEST_ARTIFACT_DIR/reports/attempts" -name 'previous-*.json' | grep -q .
bash "$script" pypi
reject env TEST_BUILD_FAIL=1 bash "$script" pypi
[ ! -e "$ECLTEST_ARTIFACT_DIR/reports/pypi.json" ]
reject bash "$script" audit
bash "$script" pypi
status=0
TEST_RUN_STATUS=7 bash "$script" pypi || status=$?
[ "$status" = 7 ]
[ ! -e "$ECLTEST_ARTIFACT_DIR/reports/pypi.json" ]
reject bash "$script" audit
bash "$script" pypi
reject bash "$script" pypi --group forbidden
[ ! -e "$ECLTEST_ARTIFACT_DIR/reports/pypi.json" ]
reject bash "$script" audit
reject env ECLTEST_SOURCE=other bash "$script" load
reject env ECLTEST_RUN_ID=other bash "$script" load
reject env TEST_IMAGE_ID=sha256:wrong bash "$script" load
reject env ECLTEST_ARTIFACT_DIR="$work/build-failure" TEST_BUILD_FAIL=1 bash "$script" prepare
reject env -u ECLTEST_ARTIFACT_DIR bash "$script" prepare

wait_for() {
  local attempt
  for ((attempt = 0; attempt < 200; attempt++)); do
    [ -f "$1" ] && return 0
    sleep 0.01
  done
  return 1
}
TEST_BLOCK_RUN=1 bash "$script" pypi > "$work/blocked-run.log" 2>&1 &
run_pid=$!
wait_for "$work/running"
before="$(sha256sum "$ECLTEST_ARTIFACT_DIR/plan.json" "$ECLTEST_ARTIFACT_DIR/identity.json")"
bash "$script" prepare > "$work/overlap.log" 2>&1 &
prepare_pid=$!
sleep 0.1
kill -0 "$prepare_pid"
[ "$before" = "$(sha256sum "$ECLTEST_ARTIFACT_DIR/plan.json" "$ECLTEST_ARTIFACT_DIR/identity.json")" ]
touch "$work/release-run"
wait "$run_pid"
status=0
wait "$prepare_pid" || status=$?
[ "$status" -ne 0 ]
[ "$before" = "$(sha256sum "$ECLTEST_ARTIFACT_DIR/plan.json" "$ECLTEST_ARTIFACT_DIR/identity.json")" ]
bash "$script" audit

ECLTEST_ARTIFACT_DIR="$work/shared-preparation" bash "$script" prepare > "$work/prepare-1.log" 2>&1 &
first_prepare=$!
ECLTEST_ARTIFACT_DIR="$work/shared-preparation" bash "$script" prepare > "$work/prepare-2.log" 2>&1 &
second_prepare=$!
first_status=0
second_status=0
wait "$first_prepare" || first_status=$?
wait "$second_prepare" || second_status=$?
[ "$((first_status + second_status))" -eq 1 ]
[ "$(grep -Fxc "$work/shared-preparation" "$work/preparations")" -eq 1 ]

for number in 1 2; do
  env -u ECLTEST_ARTIFACT_DIR bash "$script" full > "$work/full-$number.log" 2>&1 &
  full_pids[number]=$!
done
for number in 1 2; do
  wait "${full_pids[number]}"
  full_roots+=("$(sed -n 's/^E2E artifacts: //p' "$work/full-$number.log")")
done
[ "${full_roots[0]}" != "${full_roots[1]}" ]
for directory in "${full_roots[@]}"; do
  for component in npm-serving npm-writes pypi shared-system; do
    [ -s "$directory/reports/$component.json" ]
  done
  ECLTEST_ARTIFACT_DIR="$directory" bash "$script" audit
done

cp "$ECLTEST_ARTIFACT_DIR/plan.json" "$work/plan.json"
printf '\n' >> "$ECLTEST_ARTIFACT_DIR/plan.json"
reject bash "$script" load
cp "$work/plan.json" "$ECLTEST_ARTIFACT_DIR/plan.json"
printf 'corrupt' >> "$ECLTEST_ARTIFACT_DIR/docker-image.tar"
reject bash "$script" load
printf 'ok - immutable inputs, current-attempt evidence, overlap and execution identities\n'
