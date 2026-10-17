#!/usr/bin/env bash
# Delegate the measurement cgroup or enter its harness leaf before the native process.
set -euo pipefail
case "${1:-}" in
  prepare)
    sudo swapoff -a
    sudo mkdir -p "${BENCH_LOAD_CGROUP:?}/harness"
    # A child may enable only what its parent enables, so cpuset, which places a proxy, starts at the root.
    echo '+cpuset' | sudo tee /sys/fs/cgroup/cgroup.subtree_control >/dev/null
    echo '+cpuset +cpu +memory +pids' | sudo tee "$BENCH_LOAD_CGROUP/cgroup.subtree_control" >/dev/null
    # After the controllers, so the harness leaf's own controller files are the harness's to write.
    sudo chown -R "$(id -u):$(id -g)" "$BENCH_LOAD_CGROUP"
    ;;
  enter)
    shift
    echo "$$" | sudo tee "${BENCH_LOAD_CGROUP:?}/harness/cgroup.procs" >/dev/null
    exec "$@"
    ;;
  *) echo 'usage: load-cgroup.sh prepare | enter COMMAND...' >&2; exit 2 ;;
esac
