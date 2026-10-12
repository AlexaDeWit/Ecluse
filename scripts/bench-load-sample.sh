#!/usr/bin/env bash
set -euo pipefail

[[ $# == 2 ]] || { echo 'usage: bench-load-sample.sh CGROUP OUTPUT' >&2; exit 2; }
sample_root=$1
sample_output=$2
mkdir -p "$sample_output/processes" "$sample_output/cgroups"
declare -A sample_seen=()

capture_file() {
  local path=$1
  printf '\nfile %s\n' "$path"
  if [[ -r "$path" ]]; then cat "$path" || printf 'read failed\n'; else printf 'missing\n'; fi
}

capture_process() {
  local pid=$1 directory="$sample_output/processes/$1" field task
  [[ -d /proc/$pid ]] || return 0
  mkdir -p "$directory"
  {
    printf '\nsample %s\n' "$sample_now"
    for field in stat status cgroup io schedstat; do capture_file "/proc/$pid/$field"; done
  } >> "$directory/samples.txt"
  if [[ -z ${sample_seen[$pid]:-} ]]; then
    sample_seen[$pid]=1
    readlink "/proc/$pid/exe" > "$directory/exe.txt" || printf 'missing\n' > "$directory/exe.txt"
    tr '\0' '\n' < "/proc/$pid/cmdline" > "$directory/argv.txt" || true
    capture_file "/proc/$pid/maps" > "$directory/maps.txt"
    for task in /proc/"$pid"/task/*; do
      capture_file "$task/status"
    done > "$directory/task-status.txt"
  fi
}

capture_cgroup() {
  local directory=$1 label=$2 field pid members
  [[ -d "$directory" ]] || return 0
  {
    printf '\nsample %s\n' "$sample_now"
    for field in cgroup.procs cpu.max cpu.stat cpu.pressure cpuset.cpus.effective memory.current memory.max memory.events memory.swap.max pids.current pids.max; do
      capture_file "$directory/$field"
    done
  } >> "$sample_output/cgroups/$label.txt"
  if ! members=$(cat "$directory/cgroup.procs" 2>/dev/null); then
    printf 'Process membership disappeared after this sample.\n' >> "$sample_output/cgroups/$label.txt"
    return 0
  fi
  while read -r pid; do
    [[ $pid =~ ^[0-9]+$ ]] && capture_process "$pid"
  done <<< "$members"
  return 0
}

capture_file "/proc/$$/stat" > "$sample_output/sampler-start.txt"
while :; do
  sample_now=$(date --iso-8601=ns)
  capture_cgroup "$sample_root/harness" harness
  for sample_directory in "$sample_root"/proxy-*; do
    capture_cgroup "$sample_directory" "${sample_directory##*/}"
  done
  {
    printf '\nsample %s\n' "$sample_now"
    for sample_file in /proc/loadavg /proc/pressure/cpu /proc/pressure/io /proc/pressure/memory /sys/fs/cgroup/cpu.stat /sys/fs/cgroup/cpu.pressure /sys/fs/cgroup/cpu.max /sys/fs/cgroup/cpuset.cpus.effective; do capture_file "$sample_file"; done
  } >> "$sample_output/host-samples.txt"
  [[ ! -f "$sample_output/stop-sampler" ]] || break
  sleep 2
done
capture_file "/proc/$$/stat" > "$sample_output/sampler-end.txt"

# Hash mapped files after measurement, so reads do not compete with timed traffic.
find "$sample_output/processes" -name maps.txt -exec awk '$6 ~ /^\// {print $6}' {} + | LC_ALL=C sort -u > "$sample_output/mapped-paths.txt"
while IFS= read -r sample_path; do
  if [[ -f "$sample_path" ]]; then
    sha256sum "$sample_path"
  else
    printf 'missing %s\n' "$sample_path"
  fi
done < "$sample_output/mapped-paths.txt" > "$sample_output/mapped-files.sha256"
