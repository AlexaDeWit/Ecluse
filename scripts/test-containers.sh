#!/usr/bin/env bash
# Serialise planes on one daemon and reap only the current invocation's objects.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$here/process-group.sh"

readonly LABEL_KEY=com.ecluse.test
readonly SCOPE_KEY=com.ecluse.test.scope
readonly RUN_KEY=com.ecluse.test.run
readonly SHARD_KEY=com.ecluse.test.shard

scope_id() {
  local root
  root="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
  printf 'ecl-%s\n' "$(printf '%s' "$root" | sha1sum | cut -c1-12)"
}

run_id() {
  local token
  token="$(mktemp -d /tmp/ecluse-run.XXXXXXXXXXXX)"
  basename "$token"
  rmdir "$token"
}

# A single lock covers all contexts: two Docker contexts can point at the same daemon.
lock_daemon() {
  local lock_dir="/tmp/ecluse-test-daemon-${UID}"
  mkdir -p "$lock_dir"
  exec 9>"$lock_dir/lock"
  flock 9
}

reap() {
  local mode="${1:-}" ids status=0
  local -a filters=(--filter "label=$LABEL_KEY")
  case "$mode" in
    invocation)
      filters+=(--filter "label=$SCOPE_KEY=${ECLTEST_SCOPE:?}" --filter "label=$RUN_KEY=${ECLTEST_RUN_ID:?}" --filter "label=$SHARD_KEY=${ECLTEST_SHARD:?}")
      ;;
    stale) filters+=(--filter "label=$SCOPE_KEY=$(scope_id)") ;;
    --all) ;;
    *) echo "test-containers: unknown reap mode '$mode'" >&2; return 2 ;;
  esac
  ids="$(docker ps -aq "${filters[@]}")" || status=1
  if [ -n "$ids" ]; then
    printf '%s\n' "$ids" | xargs -r docker rm -f || status=1
  fi
  ids="$(docker network ls -q "${filters[@]}")" || status=1
  if [ -n "$ids" ]; then
    printf '%s\n' "$ids" | xargs -r -n1 docker network rm || status=1
  fi
  if [ "$mode" = --all ]; then
    ids="$(docker images -q --filter "label=$LABEL_KEY")" || status=1
    if [ -n "$ids" ]; then
      printf '%s\n' "$ids" | xargs -r docker rmi -f || status=1
    fi
  fi
  return "$status"
}

finish() {
  local status=$? cleanup_status=0
  trap - EXIT INT TERM
  if ! reap invocation; then
    cleanup_status=1
    echo 'test-containers: cleanup failed' >&2
    [ "$status" -ne 0 ] || status=1
  fi
  if [ -n "${ECLTEST_CLEANUP_STATUS_PATH:-}" ]; then
    printf '%s\n' "$cleanup_status" > "$ECLTEST_CLEANUP_STATUS_PATH"
  fi
  exit "$status"
}

run() {
  [ "$#" -gt 0 ] || return 2
  lock_daemon
  export ECLTEST_SCOPE
  ECLTEST_SCOPE="$(scope_id)"
  export ECLTEST_RUN_ID="${ECLTEST_RUN_ID:-$(run_id)}"
  export ECLTEST_SHARD="${ECLTEST_SHARD:-all}"
  export ECLTEST_CONTAINER_LOCK=held
  docker info >/dev/null
  trap finish EXIT
  run_process_group "$@"
}

case "${1:-}" in
  scope) scope_id ;;
  run-id) run_id ;;
  run) shift; run "$@" ;;
  reap)
    lock_daemon
    docker info >/dev/null
    reap "${2:-stale}"
    ;;
  list)
    docker ps -a --filter "label=$LABEL_KEY" \
      --format 'table {{.ID}}\t{{.Label "com.ecluse.test.scope"}}\t{{.Label "com.ecluse.test.run"}}\t{{.Label "com.ecluse.test.shard"}}\t{{.Names}}\t{{.Status}}'
    ;;
  *) echo "usage: $0 {scope|run COMMAND...|list|reap [--all]}" >&2; exit 2 ;;
esac
