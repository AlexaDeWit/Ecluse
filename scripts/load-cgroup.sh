#!/usr/bin/env bash
# Delegate the measurement cgroup or enter its harness leaf before the native process.
set -euo pipefail
case "${1:-}" in
  prepare)
    sudo swapoff -a
    sudo mkdir -p "${BENCH_LOAD_CGROUP:?}/harness"
    sudo chown -R "$(id -u):$(id -g)" "$BENCH_LOAD_CGROUP"
    echo '+cpu +memory +pids' | sudo tee "$BENCH_LOAD_CGROUP/cgroup.subtree_control" >/dev/null
    ;;
  enter)
    shift
    echo "$$" | sudo tee "${BENCH_LOAD_CGROUP:?}/harness/cgroup.procs" >/dev/null
    exec "$@"
    ;;
  *) echo 'usage: load-cgroup.sh prepare | enter COMMAND...' >&2; exit 2 ;;
esac
