#!/usr/bin/env bash
#
# Reap the Docker containers, networks, and (only in --all mode) images that
# Écluse's integration and end-to-end suites create, so a killed or interrupted
# run does not leave a pile of orphans behind and slowly fill the machine. See
# AGENTS.md -> "Build and tooling" and docs/testing.md -> "Tests and Docker".
#
# Every test container is stamped with two labels (see the shared writer,
# Ecluse.Test.Containers, and test/e2e/Ecluse/E2E/Harness/Docker.hs):
#
#   com.ecluse.test        = e2e | integration   # "this is an Écluse test object"
#   com.ecluse.test.scope  = <per-worktree id>    # who owns it
#
# The scope is what makes concurrency safe. `reap` (the default) removes only
# THIS worktree's objects, so it can never touch a sibling worktree's or a
# parallel agent's live containers. `reap --all` ignores the scope and removes
# every Écluse test object on the daemon: the "prune everything, nothing else is
# running" button.
#
# Usage:
#   scripts/test-containers.sh scope        # print this worktree's scope id
#   scripts/test-containers.sh list         # list Écluse test containers by scope
#   scripts/test-containers.sh reap         # remove THIS worktree's test objects
#   scripts/test-containers.sh reap --all   # remove ALL Écluse test objects
set -euo pipefail

readonly LABEL_KEY="com.ecluse.test"
readonly SCOPE_KEY="com.ecluse.test.scope"

# The reaping scope for the current worktree. Honours ECLUSE_TEST_SCOPE when the
# caller (the `task test-*` targets) has pinned it; otherwise derives a stable id
# from the worktree root path, so each checkout owns a distinct scope.
scope_id() {
  if [ -n "${ECLUSE_TEST_SCOPE:-}" ]; then
    printf '%s\n' "$ECLUSE_TEST_SCOPE"
    return
  fi
  local root
  root="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
  printf 'ecl-%s\n' "$(printf '%s' "$root" | sha1sum | cut -c1-12)"
}

# True when a Docker daemon is reachable. Everything downstream degrades to a
# no-op (exit 0) when it is not, so a `defer` reap never masks a suite result and
# `task test-clean` on a daemonless box is harmless.
docker_ready() {
  command -v docker >/dev/null 2>&1 && docker info >/dev/null 2>&1
}

# `rm -f` every container id on stdin (no-op on empty input).
rm_containers() {
  xargs -r docker rm -f >/dev/null 2>&1 || true
}

# Remove every network id on stdin, one at a time so one in-use network does not
# strand the rest.
rm_networks() {
  xargs -r -n1 docker network rm >/dev/null 2>&1 || true
}

# Force-remove every image id on stdin.
rm_images() {
  xargs -r docker rmi -f >/dev/null 2>&1 || true
}

reap() {
  local mode="${1:-}"
  if ! docker_ready; then
    echo "test-containers: no reachable Docker daemon; nothing to reap" >&2
    return 0
  fi

  local -a filters
  if [ "$mode" = "--all" ]; then
    filters=(--filter "label=$LABEL_KEY")
    echo "test-containers: reaping ALL Écluse test containers/networks/images (every worktree)" >&2
  elif [ -z "$mode" ]; then
    local scope
    scope="$(scope_id)"
    filters=(--filter "label=$LABEL_KEY" --filter "label=$SCOPE_KEY=$scope")
    echo "test-containers: reaping test objects for scope '$scope'" >&2
  else
    echo "test-containers: unknown reap mode '$mode' (expected nothing or --all)" >&2
    return 2
  fi

  docker ps -aq "${filters[@]}" | rm_containers
  docker network ls -q "${filters[@]}" | rm_networks
  # Images carry only the coarse label: a Dockerfile LABEL is static, so it has no
  # per-worktree scope. Prune them only in --all mode, where nothing else is meant
  # to be running and so nothing else can be relying on the shared build image.
  if [ "$mode" = "--all" ]; then
    docker images -q --filter "label=$LABEL_KEY" | rm_images
  fi
}

list() {
  if ! docker_ready; then
    echo "test-containers: no reachable Docker daemon" >&2
    return 0
  fi
  docker ps -a --filter "label=$LABEL_KEY" \
    --format 'table {{.ID}}\t{{.Label "com.ecluse.test"}}\t{{.Label "com.ecluse.test.scope"}}\t{{.Names}}\t{{.Status}}'
}

main() {
  local cmd="${1:-}"
  case "$cmd" in
    scope) scope_id ;;
    list) list ;;
    reap) reap "${2:-}" ;;
    *)
      echo "usage: $0 {scope|list|reap [--all]}" >&2
      exit 2
      ;;
  esac
}

main "$@"
