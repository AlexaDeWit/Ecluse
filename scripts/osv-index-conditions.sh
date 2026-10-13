#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
# SPDX-License-Identifier: MIT
set -euo pipefail
out="${1:?conditions output}"
{
  git rev-parse HEAD
  ghc --version
  cabal --version
  uname -sm
  lscpu
  cat /proc/meminfo
  for setting in /sys/fs/cgroup/cpu.max /sys/fs/cgroup/memory.max; do
    if [ -r "$setting" ]; then
      printf '%s=' "${setting##*/}"
      cat "$setting"
    fi
  done
  cat /proc/self/status | grep -E 'Cpus_allowed_list|Mems_allowed_list'
  stat -f -c 'filesystem=%T blockBytes=%S' .
  printf 'run=%s attempt=%s architecture=%s\n' "${GITHUB_RUN_ID:?}" "${GITHUB_RUN_ATTEMPT:?}" "${RUNNER_ARCH:?}"
  printf 'optimisation=-O1 profiling=off rts=-N -A64m -n4m -T --disable-delayed-os-memory-return\n'
} > "$out"
