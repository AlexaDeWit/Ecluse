#!/usr/bin/env bash
# Check the real Task graph with stand-in processes and observable files.
set -euo pipefail
repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
work="$(mktemp -d)"
trap 'status=$?; if [ "$status" -ne 0 ]; then cat "$work/task.log" >&2; fi; rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/archive"
cp "$repo/Taskfile.yml" "$work/"
cp -r "$repo/taskfiles" "$repo/scripts" "$work/"
cd "$work"
export TEST_WORK="$work" TEST_TRACE="$work/trace"
export ECLTEST_SOURCE=test-source ECLTEST_RUN_ID=test-run ECLTEST_ARCHITECTURE=arm64
export PATH="$work/bin:$PATH"
printf '{"architecture":"arm64"}\n' > archive/config.json
printf '[{"Config":"config.json"}]\n' > archive/manifest.json
tar -cf image.tar -C archive config.json manifest.json
export TEST_CONFIG_ID
TEST_CONFIG_ID="sha256:$(sha256sum archive/config.json | cut -d' ' -f1)"
cat > bin/cabal <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
case "$1" in
  build)
    printf 'build:%s\n' "$2" >> "$TEST_TRACE"
    [ "${TEST_FAIL_BUILD:-}" != "$2" ] || exit 9
    if [ -n "${TEST_OVERLAP:-}" ] && [ "$2" = ecluse-e2e-manifest ]; then
      touch "$TEST_WORK/plan-started"
      timeout 5 bash -c 'until [ -e "$TEST_WORK/image-started" ]; do sleep 0.01; done'
    fi
    ;;
  list-bin) printf '%s/bin/%s\n' "$TEST_WORK" "${2#exe:}" ;;
  run|bench)
    printf 'measure\n' >> "$TEST_TRACE"
    test -s "$PERF_ARTIFACT_DIR/conditions.json"
    test ! -e "$PERF_ARTIFACT_DIR/ecosystem-npm/receipt.json"
    test ! -e "$PERF_ARTIFACT_DIR/ecosystem-pypi/receipt.json"
    printf 'native observations\n' > "${ECLUSE_PERFORMANCE_OBSERVATIONS:-bench-results.csv}"
    printf 'native report\n'
    exit "${TEST_MEASURE_STATUS:-0}"
    ;;
  test)
    printf 'integration\n' >> "$TEST_TRACE"
    [ -z "${HPCTIXFILE:-}" ]
    [ "${#ECLTEST_PLAN_DIGEST}" -eq 64 ]
    [ "$ECLTEST_CONTAINER_LOCK" = held ]
    if [ "${TEST_MISSING_REPORT:-}" != integration ]; then
      printf '%s\n' "${TEST_INTEGRATION_REPORT:-passed}" > "$ECLTEST_REPORT_PATH"
    fi
    mkdir -p dist-coverage/hpc/vanilla/mix
    printf 'native ticks\n' > dist-coverage/ecluse-integration.tix
    exit "${TEST_INTEGRATION_STATUS:-0}"
    ;;
  *) exit 2 ;;
esac
STUB
cat > bin/git <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
case "$*" in
  'rev-parse HEAD') printf 'fallback-source\n' ;;
  'rev-parse --show-toplevel') printf '%s\n' "$TEST_WORK" ;;
  *) exit 2 ;;
esac
STUB
cat > bin/uname <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
[ "$*" = -m ]
printf 'fallback-architecture\n'
STUB
cat > bin/nix <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
printf 'image-build\n' >> "$TEST_TRACE"
if [ -n "${TEST_OVERLAP:-}" ]; then
  touch "$TEST_WORK/image-started"
  timeout 5 bash -c 'until [ -e "$TEST_WORK/plan-started" ]; do sleep 0.01; done'
fi
[ -z "${TEST_FAIL_IMAGE:-}" ] || { echo 'image build failed' >&2; exit 8; }
printf '%s/image.tar\n' "$TEST_WORK"
STUB
cat > bin/docker <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
case "$1" in
  info) exit "${TEST_DOCKER_STATUS:-0}" ;;
  load) printf 'load\n' >> "$TEST_TRACE"; echo 'Loaded image: ecluse:test' ;;
  ps) [ "${TEST_CLEANUP_FAIL:-0}" = 0 ] || echo container ;;
  network) ;;
  rm) exit 1 ;;
  image)
    case "$2" in
      inspect) printf '%s\n' "${TEST_DAEMON_ID:-$TEST_CONFIG_ID}" ;;
      save) cp "${TEST_DAEMON_ARCHIVE:-$TEST_WORK/image.tar}" "$4" ;;
      *) exit 2 ;;
    esac
    ;;
  *) exit 2 ;;
esac
STUB
cat > bin/ecluse-artifact <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
[ "$ECLTEST_SOURCE" = "${TEST_EXPECT_SOURCE:-test-source}" ]
[ "$ECLTEST_ARCHITECTURE" = "${TEST_EXPECT_ARCHITECTURE:-arm64}" ]
[ "$ECLTEST_REQUIRED" = 1 ]
operation="$1"
root="$2"
case "$operation" in
  prepare-begin)
    [ ! -e "$root/plan.json" ] && [ ! -e "$root/identity.json" ]
    mkdir "$root/.preparation"
    ;;
  prepare-finish)
    test -s "$root/plan.json"
    test -s "$root/docker-image.tar"
    printf '%s\n' "$3" > "$root/identity.json"
    ;;
  verify|verify-plan)
    test -s "$root/identity.json"
    if [ "$operation" = verify ]; then test -s "$root/docker-image.tar"; fi
    if [ "$#" -eq 3 ]; then [ "$3" = "$(cat "$root/identity.json")" ]; fi
    printf 'export ECLTEST_IMAGE_ID=%q\n' "$(cat "$root/identity.json")" > "$root/identity.env"
    ;;
  attempt-begin) printf 'running\n' > "$4/status" ;;
  attempt-finish)
    printf '%s\n' "$5" > "$4/status"
    if [ -s "$4/report.json" ]; then
      cp "$4/report.json" "$root/reports/$3.json"
    elif [ "$5" = 0 ]; then
      exit 1
    fi
    exit "$5"
    ;;
  attempt-continue)
    [ "$(cat "$4/status")" -le 1 ]
    [ "$(cat "$4/cleanup-status")" = 0 ]
    cmp "$4/report.json" "$root/reports/$3.json"
    case "$(cat "$4/report.json")" in passed|failed) ;; *) exit 1 ;; esac
    ;;
  *) exit 2 ;;
esac
STUB
cat > bin/runner <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
[ "$ECLTEST_SOURCE" = "${TEST_EXPECT_SOURCE:-test-source}" ]
[ "$ECLTEST_ARCHITECTURE" = "${TEST_EXPECT_ARCHITECTURE:-arm64}" ]
[ "$ECLTEST_REQUIRED" = 1 ]
component="$(basename "$0")"
case "$component" in
  *-manifest)
    if [ "$component" = ecluse-integration-manifest ]; then
      printf 'metadata ticks\n' > "${HPCTIXFILE:?}"
    fi
    printf 'plan\n'
    ;;
  ecluse-e2e-audit)
    printf 'audit\n' >> "$TEST_TRACE"
    [ "$#" = 6 ]
    shift
    for report in "$@"; do test -s "$report"; done
    for report in "$@"; do [ "$(cat "$report")" = passed ]; done
    ;;
  ecluse-completion-*-audit)
    [ "$#" = 8 ]
    subject="${component#ecluse-completion-}"
    subject="${subject%-audit}"
    [ "$8" = "$ECLTEST_ARTIFACT_DIR/reports/$subject.json" ]
    for report in "$@"; do test -s "$report"; done
    ;;
  *-audit)
    printf 'audit:%s\n' "$component" >> "$TEST_TRACE"
    if [ "$component" = ecluse-integration-audit ]; then
      printf 'audit ticks\n' > "${HPCTIXFILE:?}"
    fi
    [ "$(cat "$2")" = passed ]
    ;;
  ecluse-e2e-*)
    subject="${component#ecluse-e2e-}"
    printf 'run:%s\n' "$subject" >> "$TEST_TRACE"
    [ "$#" = 0 ]
    [ "$ECLTEST_CONTAINER_LOCK" = held ]
    [ "$ECLTEST_IMAGE_ID" = "$TEST_CONFIG_ID" ]
    if [ "${TEST_MISSING_REPORT:-}" != "$subject" ]; then
      value=passed
      [ "${TEST_FAILED_SUBJECT:-}" != "$subject" ] || value=failed
      [ "${TEST_SETUP_SUBJECT:-}" != "$subject" ] || value=setup-failed
      printf '%s\n' "$value" > "$ECLTEST_REPORT_PATH"
    fi
    if [ "${TEST_BLOCK_SUBJECT:-}" = "$subject" ]; then
      touch "$TEST_WORK/running"
      while [ ! -e "$TEST_WORK/release" ]; do sleep 0.01; done
    fi
    [ "${TEST_CANCEL_SUBJECT:-}" != "$subject" ] || exit 143
    [ "${TEST_SETUP_SUBJECT:-}" != "$subject" ] || exit 1
    [ "${TEST_FAILED_SUBJECT:-}" != "$subject" ] || exit "${TEST_FAILURE_STATUS:-1}"
    ;;
  ecluse-conformance-*)
    printf 'backend:%s\n' "$component" >> "$TEST_TRACE"
    if [ "${TEST_MISSING_BACKEND:-0}" = 0 ]; then
      printf '%s\n' "${TEST_BACKEND_REPORT:-passed}" > "$ECLTEST_REPORT_PATH"
    fi
    exit "${TEST_BACKEND_STATUS:-0}"
    ;;
  *) exit 2 ;;
esac
STUB
for name in ecluse-e2e-manifest ecluse-e2e-audit ecluse-e2e-npm ecluse-e2e-pypi ecluse-e2e-cargo ecluse-e2e-rubygems ecluse-e2e-shared-system \
  ecluse-completion-npm-audit ecluse-completion-pypi-audit ecluse-completion-cargo-audit ecluse-completion-rubygems-audit ecluse-integration-manifest ecluse-integration-audit \
  ecluse-conformance-verdaccio ecluse-conformance-verdaccio-manifest ecluse-conformance-verdaccio-audit \
  ecluse-conformance-codeartifact ecluse-conformance-codeartifact-manifest ecluse-conformance-codeartifact-audit; do
  cp bin/runner "bin/$name"
done
cat > bin/hpc-codecov <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
while [ "$1" != -o ]; do shift; done
printf '{}\n' > "$2"
STUB
cat > bin/sudo <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
case "$1" in
  swapoff) printf 'swapoff\n' >> "$TEST_TRACE" ;;
  mkdir|chown|tee) exec "$@" ;;
  *) exit 2 ;;
esac
STUB
cat > bin/bench-report <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
case "$1" in
  conditions)
    printf 'conditions\n' >> "$TEST_TRACE"
    path="$2"
    shift 2
    printf '%s\n' "$@" > "$path"
    ;;
  receipt-*)
    printf 'receipt\n' >> "$TEST_TRACE"
    test -s "$3"
    for ecosystem in npm pypi cargo rubygems; do
      mkdir -p "$2/ecosystem-$ecosystem"
      printf '%s\n' "$PERF_NATIVE_OUTCOME" > "$2/ecosystem-$ecosystem/receipt.json"
    done
    [ "$PERF_NATIVE_OUTCOME" = success ]
    ;;
  *) exit 2 ;;
esac
STUB
chmod +x bin/*

run_task() {
  # Nested Task colour resets must not prefix diagnostics checked as complete lines.
  NO_COLOR=1 task --exit-code --silent "$@" > "$work/task.log" 2>&1
}

reject() {
  local status=0
  "$@" || status=$?
  if [ "$status" = 0 ]; then
    echo "unexpected success: $*" >&2
    cat "$work/task.log" >&2
    exit 1
  fi
}

wait_for() {
  local attempt
  for ((attempt = 0; attempt < 500; attempt++)); do
    [ -f "$1" ] && return 0
    sleep 0.01
  done
  cat "$work/task.log" >&2
  return 1
}

export ECLTEST_ARTIFACT_DIR="$work/prepared"
run_task test-e2e-prepare
run_task test-e2e-load
TEST_DAEMON_ID="sha256:$(printf manifest | sha256sum | cut -d' ' -f1)" run_task test-e2e-load
[ "$(cat "$ECLTEST_ARTIFACT_DIR/image-ref")" != "$TEST_CONFIG_ID" ]
run_task test-e2e-pypi
mkdir wrong-archive
printf '{}\n' > wrong-archive/config.json
cp archive/manifest.json wrong-archive/manifest.json
tar -cf wrong-image.tar -C wrong-archive config.json manifest.json
TEST_DAEMON_ARCHIVE="$work/wrong-image.tar" reject run_task test-e2e-load
TEST_DAEMON_ARCHIVE="$work/wrong-image.tar" reject run_task test-e2e-pypi
[ ! -e "$ECLTEST_ARTIFACT_DIR/reports/pypi.json" ]
[ -z "$(find "$ECLTEST_ARTIFACT_DIR" -name '.image-check.*' -print -quit)" ]
run_task test-e2e-load
run_task test-e2e-pypi
[ "$(cat "$ECLTEST_ARTIFACT_DIR/reports/pypi.json")" = passed ]
status=0
TEST_FAILED_SUBJECT=pypi TEST_FAILURE_STATUS=7 run_task test-e2e-pypi || status=$?
[ "$status" = 7 ]
[ "$(cat "$ECLTEST_ARTIFACT_DIR/reports/pypi.json")" = failed ]
for failing in docker artifact-build subject-build arguments missing; do
  run_task test-e2e-pypi
  case "$failing" in
    docker) TEST_DOCKER_STATUS=23 reject run_task test-e2e-pypi ;;
    artifact-build) TEST_FAIL_BUILD=exe:ecluse-artifact reject run_task test-e2e-pypi ;;
    subject-build) TEST_FAIL_BUILD=ecluse-e2e-pypi reject run_task test-e2e-pypi ;;
    arguments) reject run_task test-e2e-pypi -- --group forbidden ;;
    missing) TEST_MISSING_REPORT=pypi reject run_task test-e2e-pypi ;;
  esac
  [ ! -e "$ECLTEST_ARTIFACT_DIR/reports/pypi.json" ]
done

ECLTEST_ARTIFACT_DIR="$work/bootstrap-failure" TEST_FAIL_BUILD=exe:ecluse-artifact reject run_task test-e2e-prepare
ECLTEST_ARTIFACT_DIR="$work/bootstrap-failure" reject run_task test-e2e-prepare
ECLTEST_ARTIFACT_DIR="$work/overlap" TEST_OVERLAP=1 run_task test-e2e-prepare
test -s "$work/overlap/identity.json"
status=0
ECLTEST_ARTIFACT_DIR="$work/image-failure" TEST_FAIL_IMAGE=1 run_task test-e2e-prepare || status=$?
[ "$status" = 8 ]
grep -Fxq 'image build failed' "$work/task.log"
[ ! -e "$work/image-failure/archive-path" ]
[ ! -e "$work/image-failure/docker-image.tar" ]
[ ! -e "$work/image-failure/identity.json" ]
status=0
ECLTEST_ARTIFACT_DIR="$work/plan-failure" TEST_FAIL_BUILD=ecluse-e2e-manifest TEST_FAIL_IMAGE=1 run_task test-e2e-prepare || status=$?
[ "$status" = 9 ]
grep -Fxq 'image build failed' "$work/task.log"
: > "$TEST_TRACE"
ECLTEST_ARTIFACT_DIR="$work/full-failed" TEST_FAILED_SUBJECT=pypi reject run_task test-e2e
for subject in npm pypi cargo rubygems shared-system; do test -s "$work/full-failed/reports/$subject.json"; done
[ "$(cat "$work/full-failed/reports/pypi.json")" = failed ]
[ "$(cat "$work/full-failed/pypi.status")" = 1 ]
[ "$(grep -E '^(run:|audit$)' "$TEST_TRACE")" = "$(printf 'run:npm\nrun:pypi\nrun:cargo\nrun:rubygems\nrun:shared-system\naudit')" ]
for reason in setup cancel missing cleanup; do
  : > "$TEST_TRACE"
  case "$reason" in
    setup) ECLTEST_ARTIFACT_DIR="$work/full-$reason" TEST_SETUP_SUBJECT=pypi reject run_task test-e2e ;;
    cancel) ECLTEST_ARTIFACT_DIR="$work/full-$reason" TEST_CANCEL_SUBJECT=pypi reject run_task test-e2e ;;
    missing) ECLTEST_ARTIFACT_DIR="$work/full-$reason" TEST_MISSING_REPORT=pypi reject run_task test-e2e ;;
    cleanup) ECLTEST_ARTIFACT_DIR="$work/full-$reason" TEST_CLEANUP_FAIL=1 TEST_FAILED_SUBJECT=npm reject run_task test-e2e ;;
  esac
  ! grep -q '^run:cargo\|^run:rubygems\|^run:shared-system\|^audit$' "$TEST_TRACE"
done

TEST_BLOCK_SUBJECT=pypi run_task test-e2e-pypi &
first=$!
wait_for "$work/running"
prepared_digest="$(sha256sum "$ECLTEST_ARTIFACT_DIR/plan.json" "$ECLTEST_ARTIFACT_DIR/identity.json")"
run_task test-e2e-prepare &
second=$!
sleep 0.1
kill -0 "$second"
touch "$work/release"
wait "$first"
reject wait "$second"
[ "$prepared_digest" = "$(sha256sum "$ECLTEST_ARTIFACT_DIR/plan.json" "$ECLTEST_ARTIFACT_DIR/identity.json")" ]
[ "$(cat "$ECLTEST_ARTIFACT_DIR/reports/pypi.json")" = passed ]

unset ECLTEST_ARTIFACT_DIR
run_task test-e2e > "$work/full-1.log" 2>&1 &
first=$!
run_task test-e2e > "$work/full-2.log" 2>&1 &
second=$!
wait "$first"
wait "$second"
[ "$(find scratchpad/e2e -mindepth 1 -maxdepth 1 -type d | wc -l)" = 2 ]
[ "$(find scratchpad/e2e -path '*/reports/shared-system.json' | wc -l)" = 2 ]
for directory in scratchpad/e2e/run.*; do
  rm "$directory/docker-image.tar"
  ECLTEST_ARTIFACT_DIR="$directory" run_task test-e2e-audit
done

export ECLTEST_INTEGRATION_ARTIFACT_DIR="$work/integration"
run_task test-integration
run_task coverage-integration
[ "$(cat dist-coverage/ecluse-integration.tix)" = 'native ticks' ]
[ -z "$(find scratchpad/integration-discovery -mindepth 1 -print -quit)" ]
for option in '--test-option=--group' '--test-options=--group integration/egress' '--test-option' \
  '--test-option=--catalogue-list' '--test-option=--report' '--test-wrapper=true' 'ecluse-unit' \
  '--builddir=other' '--ghc-options=-fno-hpc' '--disable-coverage'; do
  before="$(wc -l < "$TEST_TRACE")"
  reject run_task coverage-integration "COVERAGE_FLAGS=$option"
  [ "$(wc -l < "$TEST_TRACE")" = "$before" ]
done
TEST_INTEGRATION_REPORT=failed TEST_INTEGRATION_STATUS=7 reject run_task coverage-integration
[ "$(cat dist-coverage/ecluse-integration.tix)" = 'native ticks' ]
TEST_MISSING_REPORT=integration reject run_task coverage-integration

run_task test-conformance-verdaccio
run_task test-conformance-codeartifact
TEST_MISSING_BACKEND=1 reject run_task test-conformance-verdaccio
TEST_BACKEND_REPORT=failed reject run_task test-conformance-verdaccio
status=0
TEST_BACKEND_STATUS=7 run_task test-conformance-verdaccio || status=$?
[ "$status" = 7 ]
TEST_FAIL_BUILD=ecluse-conformance-verdaccio-manifest reject run_task test-conformance-verdaccio
export ECLTEST_ARTIFACT_DIR="$work/prepared"
export ECLTEST_CORE_UNIT_ARTIFACT_DIR="$work/core" ECLTEST_RUNTIME_UNIT_ARTIFACT_DIR="$work/runtime"
mkdir -p "$work/core" "$work/runtime"
for tier in core runtime; do
  printf 'plan\n' > "$work/$tier/unit-plan.json"
  printf 'passed\n' > "$work/$tier/unit-report.json"
done
run_task test-integration
for subject in npm pypi cargo rubygems shared-system; do
  : > "$TEST_TRACE"
  ECLTEST_COMPONENT=ambient-component ECLTEST_SHARD=ambient-shard run_task "test-e2e-$subject"
  [ "$(grep '^run:' "$TEST_TRACE")" = "run:$subject" ]
  [ "$(cat "$work/prepared/reports/$subject.json")" = passed ]
done
for subject in npm pypi cargo rubygems; do
  run_task "test-e2e-$subject-completion"
  for path in "$work/core/unit-report.json" "$work/runtime/unit-report.json" "$work/integration/report.json" "$work/prepared/reports/$subject.json"; do
    mv "$path" "$path.saved"
    reject run_task "test-e2e-$subject-completion"
    mv "$path.saved" "$path"
  done
done

for target in test-e2e-prepare test-e2e; do
  ECLTEST_ARTIFACT_DIR="$work/rejected-$target" reject run_task "$target" -- --group forbidden
  [ ! -e "$work/rejected-$target/.bootstrap" ]
done
for target in test-e2e-load test-e2e-npm test-e2e-pypi test-e2e-cargo test-e2e-rubygems test-e2e-shared-system \
  test-e2e-audit test-e2e-npm-completion test-e2e-pypi-completion test-e2e-cargo-completion test-e2e-rubygems-completion \
  test-conformance-verdaccio test-conformance-codeartifact test-integration; do
  reject run_task "$target" -- --group forbidden
done
(
  unset ECLTEST_SOURCE ECLTEST_ARCHITECTURE
  export TEST_EXPECT_SOURCE=fallback-source TEST_EXPECT_ARCHITECTURE=fallback-architecture
  export ECLTEST_ARTIFACT_DIR="$work/fallback-identity"
  run_task test-e2e-prepare
  run_task test-e2e-load
  run_task test-e2e-npm
  run_task test-conformance-codeartifact
  run_task test-integration
)

export PERF_ARTIFACT_DIR="$work/performance" BENCH_LOAD_CGROUP="$work/cgroup"
export BENCH_LOAD_ISOLATE_OHA=1 BENCH_LOAD_DURATION_SECONDS=30 BENCH_LOAD_CONCURRENCY=100
run_task performance:load-build
run_task performance:load-cgroup
[ "$(cat "$(dirname "$BENCH_LOAD_CGROUP")/cgroup.subtree_control")" = '+cpuset' ]
[ "$(cat "$BENCH_LOAD_CGROUP/cgroup.subtree_control")" = '+cpuset +cpu +memory +pids' ]
for kind in allocation live-allocation benchmark load; do
  for ecosystem in npm pypi cargo rubygems; do
    mkdir -p "$PERF_ARTIFACT_DIR/ecosystem-$ecosystem"
    touch "$PERF_ARTIFACT_DIR/ecosystem-$ecosystem/receipt.json"
  done
  : > "$TEST_TRACE"
  status=0
  TEST_MEASURE_STATUS=7 run_task "performance:$kind-measure" || status=$?
  [ "$status" = 7 ]
  [ "$(cat "$PERF_ARTIFACT_DIR/native-status")" = 7 ]
  [ "$(grep -E '^(conditions|measure)$' "$TEST_TRACE")" = "$(printf 'conditions\nmeasure')" ]
  PERF_NATIVE_OUTCOME=failure reject run_task "performance:$kind-receipt"
  for ecosystem in npm pypi cargo rubygems; do
    [ "$(cat "$PERF_ARTIFACT_DIR/ecosystem-$ecosystem/receipt.json")" = failure ]
  done
done
test -s "$BENCH_LOAD_CGROUP/harness/cgroup.procs"
grep -Fxq 'affinity=taskset -c 0 oha' "$PERF_ARTIFACT_DIR/conditions.json"
grep -Fxq 'loadPasses=every' "$PERF_ARTIFACT_DIR/conditions.json"
grep -Fxq 'proxyCpus=' "$PERF_ARTIFACT_DIR/conditions.json"
TEST_FAIL_BUILD=exe:bench-report PERF_NATIVE_OUTCOME=failure reject run_task performance:load-receipt
for ecosystem in npm pypi cargo rubygems; do
  [ ! -e "$PERF_ARTIFACT_DIR/ecosystem-$ecosystem/receipt.json" ]
done
printf 'ok - Task sequence, image build overlap, current failures, aggregate evidence, locks, cancellation, coverage and fixed backend contracts\n'
