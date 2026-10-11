#!/usr/bin/env bash
# Prepare immutable inputs, load an image, run a selection, or audit shard reports.
set -euo pipefail

root="${ECLTEST_ARTIFACT_DIR:-scratchpad/e2e}"
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

fail() {
  echo "e2e: $*" >&2
  exit 1
}

architecture() {
  case "$(uname -m)" in
    aarch64|arm64) echo arm64 ;;
    x86_64) echo amd64 ;;
    *) echo 'e2e: unsupported architecture' >&2; return 1 ;;
  esac
}

runner() {
  cabal build ecluse-e2e >&2 || return
  cabal list-bin ecluse-e2e
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

prepare() {
  local binary archive config image
  mkdir -p "$root"
  binary="$(runner)"
  "$binary" --catalogue-plan > "$root/plan.json"
  archive="$(nix build --print-build-logs --option max-silent-time 1200 .#dockerImage --no-link --print-out-paths)"
  cp "$archive" "$root/docker-image.tar"
  config="$(tar -xOf "$archive" manifest.json | jq -er 'if length == 1 then .[0].Config else error("expected one image") end')"
  image="sha256:$(tar -xOf "$archive" "$config" | sha256sum | cut -d' ' -f1)"
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
  [ "$(docker image inspect --format '{{.Id}}' "$tag")" = "$ECLTEST_IMAGE_ID" ] || fail "loaded image identity mismatch"
  printf '%s\n' "$tag" > "$root/image-ref"
}

run_selection() {
  local binary shard="${ECLTEST_SHARD:-all}"
  identity
  export ECLTEST_E2E_IMAGE
  ECLTEST_E2E_IMAGE="$(cat "$root/image-ref")"
  [ "$(docker image inspect --format '{{.Id}}' "$ECLTEST_E2E_IMAGE")" = "$ECLTEST_IMAGE_ID" ] || fail "loaded image identity mismatch"
  binary="$(runner)"
  mkdir -p "$root/reports"
  if [ "$shard" = all ]; then
    exec "$binary" --report "$root/reports/all.json" "$@"
  else
    exec "$binary" --shard "$shard" --report "$root/reports/$shard.json" "$@"
  fi
}

audit() {
  local binary
  local -a reports
  identity
  shopt -s nullglob
  reports=("$root"/reports/*.json)
  [ "${#reports[@]}" -gt 0 ] || { echo 'e2e: no shard reports' >&2; return 1; }
  binary="$(runner)"
  "$binary" --audit "$root/plan.json" "${reports[@]}"
}

diagnostics() {
  mkdir -p "$root/reports"
  printf 'source=%s\nrun=%s\nshard=%s\njob-status=%s\n' \
    "${ECLTEST_SOURCE:-unknown}" "${ECLTEST_RUN_ID:-unknown}" \
    "${ECLTEST_SHARD:-all}" "${ECLTEST_JOB_STATUS:-unknown}" \
    > "$root/reports/${ECLTEST_SHARD:-all}-job.log"
}

case "${1:-}" in
  prepare) prepare ;;
  load) load_image ;;
  run)
    shift
    mkdir -p "$root/reports"
    (
      identity
      bash "$here/test-containers.sh" run bash "$here/e2e.sh" owned-run "$@"
    ) 2>&1 | tee "$root/reports/${ECLTEST_SHARD:-all}.log"
    ;;
  owned-run)
    shift
    [ "${ECLTEST_CONTAINER_LOCK:-}" = held ] || fail "missing container lifetime"
    run_selection "$@"
    ;;
  audit) audit ;;
  diagnostics) diagnostics ;;
  list) binary="$(runner)"; "$binary" --catalogue-list ;;
  plan) binary="$(runner)"; "$binary" --catalogue-plan ;;
  *) echo "usage: $0 {prepare|load|run [SELECTION...]|audit|list|plan}" >&2; exit 2 ;;
esac
