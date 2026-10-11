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
  list-bin) printf '%s/bin/runner\n' "$TEST_WORK" ;;
  *) exit 2 ;;
esac
STUB
cat > "$work/bin/runner" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
case "$1" in
  --catalogue-plan) echo '{"schemaVersion":1,"partitions":["npm-serving","npm-writes","pypi","shared-system"]}' ;;
  --audit|--audit-full)
    test "$#" = 3
    jq -e '.passed == true' "$3" >/dev/null
    ;;
  --shard)
    test "$2" = pypi
    test "$3" = --report
    test "$ECLTEST_CONTAINER_LOCK" = held
    echo '{"passed":true}' > "$4"
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
ECLTEST_SHARD=pypi bash "$script" run
[ -f "$ECLTEST_ARTIFACT_DIR/reports/pypi.json" ]
grep -q 'runner evidence' "$ECLTEST_ARTIFACT_DIR/reports/pypi.log"
bash "$script" audit
reject env ECLTEST_SHARD=pypi TEST_DOCKER_STATUS=23 bash "$script" run
[ ! -e "$ECLTEST_ARTIFACT_DIR/reports/pypi.json" ]
reject bash "$script" audit
ECLTEST_SHARD=pypi bash "$script" run
mv "$ECLTEST_ARTIFACT_DIR/reports/pypi.json" "$ECLTEST_ARTIFACT_DIR/reports/all.json"
bash "$script" audit
reject env ECLTEST_SHARD=pypi TEST_DOCKER_STATUS=23 bash "$script" run
[ ! -e "$ECLTEST_ARTIFACT_DIR/reports/pypi.json" ]
[ ! -e "$ECLTEST_ARTIFACT_DIR/reports/all.json" ]
reject bash "$script" audit
find "$ECLTEST_ARTIFACT_DIR/reports/attempts" -name 'previous-*.json' | grep -q .
ECLTEST_SHARD=pypi bash "$script" run
reject env ECLTEST_SHARD=pypi TEST_BUILD_FAIL=1 bash "$script" run
[ ! -e "$ECLTEST_ARTIFACT_DIR/reports/pypi.json" ]
reject bash "$script" audit
ECLTEST_SHARD=pypi bash "$script" run
status=0
ECLTEST_SHARD=pypi TEST_RUN_STATUS=7 bash "$script" run || status=$?
[ "$status" = 7 ]
[ ! -e "$ECLTEST_ARTIFACT_DIR/reports/pypi.json" ]
reject bash "$script" audit
reject env ECLTEST_SOURCE=other bash "$script" load
reject env ECLTEST_RUN_ID=other bash "$script" load
reject env TEST_IMAGE_ID=sha256:wrong bash "$script" load
reject env TEST_BUILD_FAIL=1 bash "$script" plan
reject env -u ECLTEST_ARTIFACT_DIR bash "$script" prepare

wait_for() {
  local attempt
  for ((attempt = 0; attempt < 200; attempt++)); do
    [ -f "$1" ] && return 0
    sleep 0.01
  done
  return 1
}
ECLTEST_SHARD=pypi TEST_BLOCK_RUN=1 bash "$script" run > "$work/blocked-run.log" 2>&1 &
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
  env -u ECLTEST_ARTIFACT_DIR ECLTEST_SHARD=pypi bash "$script" full > "$work/full-$number.log" 2>&1 &
  full_pids[number]=$!
done
for number in 1 2; do
  wait "${full_pids[number]}"
  full_roots+=("$(sed -n 's/^E2E artifacts: //p' "$work/full-$number.log")")
done
[ "${full_roots[0]}" != "${full_roots[1]}" ]
for directory in "${full_roots[@]}"; do
  [ -s "$directory/reports/pypi.json" ]
  ECLTEST_ARTIFACT_DIR="$directory" bash "$script" audit
done

cp "$ECLTEST_ARTIFACT_DIR/plan.json" "$work/plan.json"
printf '\n' >> "$ECLTEST_ARTIFACT_DIR/plan.json"
reject bash "$script" load
cp "$work/plan.json" "$ECLTEST_ARTIFACT_DIR/plan.json"
printf 'corrupt' >> "$ECLTEST_ARTIFACT_DIR/docker-image.tar"
reject bash "$script" load
env -u GITHUB_OUTPUT bash "$here/e2e-matrix.sh" "$work/plan.json" > "$work/matrix"
GITHUB_OUTPUT="$work/github-output" bash "$here/e2e-matrix.sh" "$work/plan.json" > "$work/github-stdout"
[ ! -s "$work/github-stdout" ]
cmp "$work/matrix" "$work/github-output"
grep -q 'partitions=\["npm-serving","npm-writes","pypi","shared-system"\]' "$work/matrix"
for bad in '{"partitions":[]}' '{"partitions":["pypi","pypi"]}' '{"partitions":["../escape"]}' '{}'; do
  printf '%s\n' "$bad" > "$work/bad.json"
  reject env -u GITHUB_OUTPUT bash "$here/e2e-matrix.sh" "$work/bad.json"
done
printf 'ok - immutable inputs, current-attempt evidence, overlap and execution identities\n'
