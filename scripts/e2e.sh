#!/usr/bin/env bash
# Prepare immutable inputs, run fixed subjects, and audit their reports.
set -euo pipefail

root="${ECLTEST_ARTIFACT_DIR:-}"
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

fail() {
  echo "e2e: $*" >&2
  exit 1
}

lock_artifacts() {
  [ -n "$root" ] || fail "set ECLTEST_ARTIFACT_DIR for staged commands, or use task test-e2e"
  mkdir -p "$root"
  export ECLTEST_ARTIFACT_DIR="$root"
  exec 8>"$root/.lock"
  flock 8
}

architecture() {
  case "$(uname -m)" in
    aarch64|arm64) echo arm64 ;;
    x86_64) echo amd64 ;;
    *) echo 'e2e: unsupported architecture' >&2; return 1 ;;
  esac
}

runner() {
  cabal build "$1" >&2 || return
  cabal list-bin "$1"
}

identity() {
  local expected_source expected_run digest
  expected_source="${ECLTEST_SOURCE:-$(git rev-parse HEAD)}"
  expected_run="${ECLTEST_RUN_ID:-}"
  jq -e --arg source "$expected_source" --arg arch "$(architecture)" --arg run "$expected_run" \
    '.schemaVersion == 1 and .source == $source and .architecture == $arch
     and (.runId | type == "string" and length > 0)
     and ($run == "" or .runId == $run)
     and (.image | test("^sha256:[a-f0-9]{64}$"))
     and (.archiveDigest | test("^[a-f0-9]{64}$"))
     and (.planDigest | test("^[a-f0-9]{64}$"))' "$root/identity.json" >/dev/null || fail "execution identity does not match the source, architecture or run"
  digest="$(sha256sum "$root/plan.json" | cut -d' ' -f1)"
  [ "$digest" = "$(jq -r .planDigest "$root/identity.json")" ] || fail "plan digest mismatch"
  export ECLTEST_SOURCE="$expected_source"
  export ECLTEST_ARCHITECTURE
  ECLTEST_ARCHITECTURE="$(architecture)"
  export ECLTEST_IMAGE_ID
  ECLTEST_IMAGE_ID="$(jq -r .image "$root/identity.json")"
  export ECLTEST_PLAN_DIGEST="$digest"
  export ECLTEST_RUN_ID
  ECLTEST_RUN_ID="$(jq -r .runId "$root/identity.json")"
}

archive_config_digest() {
  local config digest
  config="$(tar -xOf "$1" manifest.json | jq -er 'if length == 1 then .[0].Config else error("expected one image") end')" || return
  digest="$(tar -xOf "$1" "$config" | sha256sum | cut -d' ' -f1)" || return
  printf 'sha256:%s\n' "$digest"
}

# Docker image IDs identify configurations or manifests, depending on the image store.
verified_image_ref() (
  local reference="$1" actual actual_config
  actual="$(docker image inspect --format '{{.Id}}' "$reference")" || return
  [[ "$actual" =~ ^sha256:[a-f0-9]{64}$ ]] || fail "invalid loaded image ID: $actual"
  check_directory="$(mktemp -d "$root/.image-check.XXXXXXXXXX")" || return
  trap 'rm -rf "$check_directory"' EXIT
  docker image save --output "$check_directory/image.tar" "$actual" || return
  actual_config="$(archive_config_digest "$check_directory/image.tar")" || return
  [ "$actual_config" = "$ECLTEST_IMAGE_ID" ] \
    || fail "loaded image configuration mismatch: expected=$ECLTEST_IMAGE_ID actual=$actual_config daemon-id=$actual reference=$reference"
  printf '%s\n' "$actual"
)

prepare() {
  local binary archive image
  [ ! -e "$root/plan.json" ] && [ ! -e "$root/identity.json" ] && [ ! -e "$root/docker-image.tar" ] \
    || fail "prepared inputs already exist, choose a new ECLTEST_ARTIFACT_DIR"
  mkdir "$root/.preparation" || fail "preparation already attempted, choose a new ECLTEST_ARTIFACT_DIR"
  binary="$(runner ecluse-e2e-manifest)"
  "$binary" > "$root/plan.json"
  archive="$(nix build --print-build-logs --option max-silent-time 1200 .#dockerImage --no-link --print-out-paths)"
  cp "$archive" "$root/docker-image.tar"
  image="$(archive_config_digest "$archive")"
  jq -n --arg source "${ECLTEST_SOURCE:-$(git rev-parse HEAD)}" --arg image "$image" \
    --arg architecture "$(architecture)" --arg planDigest "$(sha256sum "$root/plan.json" | cut -d' ' -f1)" \
    --arg archiveDigest "$(sha256sum "$archive" | cut -d' ' -f1)" \
    --arg runId "${ECLTEST_RUN_ID:-$(bash "$here/test-containers.sh" run-id)}" \
    '{schemaVersion:1,source:$source,image:$image,architecture:$architecture,planDigest:$planDigest,archiveDigest:$archiveDigest,runId:$runId}' > "$root/identity.json"
}

load_image() {
  local tag digest
  identity
  digest="$(sha256sum "$root/docker-image.tar" | cut -d' ' -f1)"
  [ "$digest" = "$(jq -r .archiveDigest "$root/identity.json")" ] || fail "archive digest mismatch"
  tag="$(bash "$here/docker-load.sh" "$root/docker-image.tar")"
  verified_image_ref "$tag" > "$root/image-ref"
}

run_component() {
  local binary component="$1"
  identity
  export ECLTEST_E2E_IMAGE
  ECLTEST_E2E_IMAGE="$(verified_image_ref "$(cat "$root/image-ref")")"
  binary="$(runner "$component")"
  exec "$binary"
}

run_attempt() {
  local shard="$1" component="$2" attempt status previous
  local -a superseded
  shift 2
  export ECLTEST_SHARD="$shard"
  [[ "$shard" =~ ^[a-z0-9][a-z0-9-]*$ ]] || fail "invalid shard name"
  mkdir -p "$root/reports/attempts"
  attempt="$(mktemp -d "$root/reports/attempts/$shard.XXXXXXXXXX")"
  superseded=("$root/reports/$shard.json" "$root/reports/all.json")
  for previous in "${superseded[@]}"; do
    if [ -e "$previous" ]; then
      mv "$previous" "$attempt/previous-$(basename "$previous")"
    fi
  done
  printf 'running\n' > "$attempt/status"
  export ECLTEST_REPORT_PATH="$attempt/report.json"
  set +e
  (
    set -e
    [ "$#" -eq 0 ] || fail "fixed subjects accept no arguments"
    identity
    bash "$here/test-containers.sh" run bash "$here/e2e.sh" owned-run "$component"
  ) 2>&1 | tee "$attempt/run.log" "$root/reports/$shard.log"
  status=$?
  set -e
  printf '%s\n' "$status" > "$attempt/status"
  if [ ! -s "$ECLTEST_REPORT_PATH" ]; then
    [ "$status" -eq 0 ] || return "$status"
    printf 'missing-report\n' > "$attempt/status"
    fail "runner completed without a current execution report"
  fi
  cp "$ECLTEST_REPORT_PATH" "$root/reports/$shard.json.tmp"
  mv "$root/reports/$shard.json.tmp" "$root/reports/$shard.json"
  return "$status"
}

audit() {
  local binary
  local -a reports
  identity
  shopt -s nullglob
  reports=("$root"/reports/*.json)
  [ "${#reports[@]}" -gt 0 ] || { echo 'e2e: no subject reports' >&2; return 1; }
  binary="$(runner ecluse-e2e-audit)"
  "$binary" "$root/plan.json" "${reports[@]}"
}

diagnostics() {
  mkdir -p "$root/reports"
  printf 'source=%s\nrun=%s\ncomponent=%s\njob-status=%s\n' \
    "${ECLTEST_SOURCE:-unknown}" "${ECLTEST_RUN_ID:-unknown}" \
    "${ECLTEST_COMPONENT:-unknown}" "${ECLTEST_JOB_STATUS:-unknown}" \
    > "$root/reports/${ECLTEST_COMPONENT:-unknown}-job.log"
}

case "${1:-}" in
  prepare) lock_artifacts; prepare ;;
  load) lock_artifacts; load_image ;;
  npm) lock_artifacts; run_attempt npm ecluse-e2e-npm "${@:2}" ;;
  pypi) lock_artifacts; run_attempt pypi ecluse-e2e-pypi "${@:2}" ;;
  shared-system) lock_artifacts; run_attempt shared-system ecluse-e2e-shared-system "${@:2}" ;;
  full)
    shift
    [ "$#" -eq 0 ] || fail "full execution accepts no arguments"
    if [ -z "$root" ]; then
      mkdir -p scratchpad/e2e
      root="$(mktemp -d scratchpad/e2e/run.XXXXXXXXXX)"
    fi
    printf 'E2E artifacts: %s\n' "$root"
    lock_artifacts
    prepare
    load_image
    run_attempt npm ecluse-e2e-npm
    run_attempt pypi ecluse-e2e-pypi
    run_attempt shared-system ecluse-e2e-shared-system
    audit
    ;;
  owned-run)
    shift
    [ "${ECLTEST_CONTAINER_LOCK:-}" = held ] || fail "missing container lifetime"
    case "${1:-}" in
      ecluse-e2e-npm|ecluse-e2e-pypi|ecluse-e2e-shared-system) ;;
      *) fail "unknown fixed component" ;;
    esac
    run_component "$1"
    ;;
  audit) lock_artifacts; audit ;;
  diagnostics) lock_artifacts; diagnostics ;;
  *) echo "usage: $0 {prepare|load|npm|pypi|shared-system|full|audit|diagnostics}" >&2; exit 2 ;;
esac
