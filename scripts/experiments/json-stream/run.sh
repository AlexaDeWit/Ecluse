#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
#
# SPDX-License-Identifier: MIT
set -euo pipefail

bench_script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
case "${1:-all}" in
  prepare|build|measure|verify|report)
    bash "$bench_script_dir/${1}.sh"
    ;;
  all)
    bash "$bench_script_dir/prepare.sh"
    bash "$bench_script_dir/build.sh"
    bash "$bench_script_dir/verify.sh"
    bash "$bench_script_dir/measure.sh"
    bash "$bench_script_dir/report.sh"
    ;;
  *) printf '%s\n' 'Usage: run.sh [all|prepare|build|verify|measure|report]' >&2; exit 1 ;;
esac
