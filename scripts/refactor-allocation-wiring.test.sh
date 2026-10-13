#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
# SPDX-License-Identifier: MIT

# Check resolved diagnostic commands through the repository Taskfile without running their actions.
set -euo pipefail

repository_root="$(git rev-parse --show-toplevel)"
cd "$repository_root"
source scripts/refactor-wiring-support.sh
diagnostic_root="$repository_root/scratchpad/refactor-allocation"
evidence_root="$diagnostic_root/evidence"
mkdir -p "$repository_root/scratchpad"
report_root="$(mktemp -d "$repository_root/scratchpad/refactor-allocation-wiring.XXXXXX")"
baseline=690a4cd522f3146fc04ae42d6abac684a44a422c
candidate=ab5f4af6e55b90ae79392f3d10a9505295f1239f
summary_count=0
: > "$report_root/expected-snapshots.tsv"
printf 'task\tsource\tsourceDirectory\tbuildDirectory\tpassDirectory\tdestination\n' > "$report_root/resolved-paths.tsv"

verify_summary() {
  local file="$1" side="$2" variant="$3" pass_name="$4" destination="$5"
  local source='' source_directory='' build_directory='' pass_directory=''
  local commands_file="$file.commands"
  expect_var "$file" ALLOC_DIAG_ROOT "$diagnostic_root" || return 1
  expect_var "$file" ALLOC_DIAG_EVIDENCE "$evidence_root" || return 1
  expect_var "$file" ALLOC_DIAG_BASELINE "$baseline" || return 1
  expect_var "$file" ALLOC_DIAG_CANDIDATE "$candidate" || return 1
  if [ -n "$side" ]; then
    source="$baseline"
    [ "$side" != B ] || source="$candidate"
    source_directory="$diagnostic_root/source-$variant-$side"
    build_directory="$evidence_root/build-$variant-$side"
    pass_directory="$evidence_root/$pass_name"
    expect_var "$file" ALLOC_DIAG_SOURCE "$source" || return 1
    expect_var "$file" ALLOC_DIAG_SOURCE_DIR "$source_directory" || return 1
    expect_var "$file" ALLOC_DIAG_BUILD_DIR "$build_directory" || return 1
    expect_var "$file" ALLOC_DIAG_PASS_DIR "$pass_directory" || return 1
  fi
  if [ -n "$destination" ]; then
    expect_var "$file" ALLOC_DIAG_DEST "$destination" || return 1
  fi
  awk '/^commands:$/ {commands=1; next} commands {print}' "$file" > "$commands_file"
  [ -s "$commands_file" ] || fail "no resolved commands: $file" || return 1
  if grep -Eq '\{\{|<no value>' "$commands_file"; then
    fail "unresolved template in commands: $file"
    return 1
  fi
  verify_paths "$commands_file" "$diagnostic_root" "$repository_root" || return 1
  printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$(basename "$file" .txt)" "$source" "$source_directory" "$build_directory" "$pass_directory" "$destination" >> "$report_root/resolved-paths.tsv"
}

summarise() {
  local label="$1" action="$2" side="$3" variant="$4" pass_name="$5" mode="$6" destination="$7"
  local file="$report_root/$label.txt"
  local -a args=(--summary "performance:allocation-diagnostics:$action")
  if [ -n "$side" ]; then
    args+=("ALLOC_DIAG_SIDE=$side" "ALLOC_DIAG_VARIANT=$variant" "ALLOC_DIAG_PASS=$pass_name" "ALLOC_DIAG_MODE=$mode")
  fi
  [ -z "$destination" ] || args+=("ALLOC_DIAG_DEST=$destination")
  # Unrelated namespaces must not change any diagnostic path or fixed source.
  args+=(EVIDENCE=/collision BASELINE=collision CANDIDATE=collision COMPARISON_ROOT=/collision SOURCE=collision SOURCE_DIR=/collision BUILD_DIR=/collision PASS_DIR=/collision DEST=/collision SIDE=collision VARIANT=collision PASS=collision MODE=collision)
  task "${args[@]}" > "$file"
  verify_summary "$file" "$side" "$variant" "$pass_name" "$destination"
  summary_count=$((summary_count + 1))
}

task --summary performance:allocation-diagnostics:run > "$report_root/default-run.txt"
verify_summary "$report_root/default-run.txt" '' '' '' ''
summary_count=$((summary_count + 1))
summarise run run '' '' '' '' ''
summarise dependency dependency '' '' '' '' ''
for variant in C I; do
  for side in A B; do
    summarise "prepare-$variant-$side" prepare "$side" "$variant" '' '' ''
    summarise "build-$variant-$side" build "$side" "$variant" '' '' ''
    summarise "snapshot-$variant-$side" snapshot "$side" "$variant" '' '' "$evidence_root/build-$variant-$side/identity"
    grep -Fq "performance:allocation-diagnostics:build ALLOC_DIAG_SIDE=$side ALLOC_DIAG_VARIANT=$variant" "$report_root/prepare-$variant-$side.txt" || fail "build call drops its context"
  done
done
while read -r side variant pass_name mode; do
  summarise "record-$pass_name" record-pass "$side" "$variant" "$pass_name" "$mode" ''
  summarise "measure-$pass_name" measure "$side" "$variant" "$pass_name" "$mode" ''
  source="$baseline"
  [ "$side" != B ] || source="$candidate"
  for phase in before after; do
    printf '%s\t%s\n' "$pass_name/$phase" "$source" >> "$report_root/expected-snapshots.tsv"
    summarise "snapshot-$pass_name-$phase" snapshot "$side" "$variant" "$pass_name" "$mode" "$evidence_root/$pass_name/$phase"
  done
  grep -Fq "performance:allocation-diagnostics:measure ALLOC_DIAG_SIDE=$side ALLOC_DIAG_VARIANT=$variant ALLOC_DIAG_PASS=$pass_name ALLOC_DIAG_MODE=$mode" "$report_root/record-$pass_name.txt" || fail "measurement call drops its context"
  grep -Fqx "        vars: {ALLOC_DIAG_SIDE: $side, ALLOC_DIAG_VARIANT: $variant, ALLOC_DIAG_PASS: $pass_name, ALLOC_DIAG_MODE: $mode}" taskfiles/refactor-allocation.yml || fail "run call differs for $pass_name"
done <<'SCHEDULE'
A C C-A1 native
B C C-B1 native
B C C-B2 native
A C C-A2 native
A I I-A1 record
B I I-B1 record
B I I-B2 record
A I I-A2 record
A I O-A-native empty-native
A I O-A-record empty-record
B I O-B-native empty-native
B I O-B-record empty-record
SCHEDULE

[ "$(grep -Fc ' - Task: performance:allocation-diagnostics:prepare' "$report_root/run.txt")" = 4 ] || fail 'build schedule differs'
[ "$(grep -Fc ' - Task: performance:allocation-diagnostics:record-pass' "$report_root/run.txt")" = 12 ] || fail 'pass schedule differs'
[ "$(grep -Fc "    dir: '{{.ALLOC_DIAG_SOURCE_DIR}}'" taskfiles/refactor-allocation.yml)" = 3 ] || fail 'source working directories differ'
grep -Fqx "        vars: {ALLOC_DIAG_SIDE: '{{.ALLOC_DIAG_SIDE}}', ALLOC_DIAG_VARIANT: '{{.ALLOC_DIAG_VARIANT}}', ALLOC_DIAG_PASS: '{{.ALLOC_DIAG_PASS}}', ALLOC_DIAG_MODE: '{{.ALLOC_DIAG_MODE}}', ALLOC_DIAG_DEST: '{{.ALLOC_DIAG_BUILD_DIR}}/identity'}" taskfiles/refactor-allocation.yml || fail 'build snapshot destination differs'
for phase in before after; do
  grep -Fqx "        vars: {ALLOC_DIAG_SIDE: '{{.ALLOC_DIAG_SIDE}}', ALLOC_DIAG_VARIANT: '{{.ALLOC_DIAG_VARIANT}}', ALLOC_DIAG_PASS: '{{.ALLOC_DIAG_PASS}}', ALLOC_DIAG_MODE: '{{.ALLOC_DIAG_MODE}}', ALLOC_DIAG_DEST: '{{.ALLOC_DIAG_PASS_DIR}}/$phase'}" taskfiles/refactor-allocation.yml || fail 'pass snapshot destination differs'
done
grep -Fqx '      - test "$(uname -m)" = aarch64' taskfiles/refactor-allocation.yml || fail 'ARM64 guard differs'

REFRACTOR_CANDIDATE=sibling-control task --summary performance:comparison:run > "$report_root/comparison.txt"
expect_var "$report_root/comparison.txt" CANDIDATE sibling-control
expect_var "$report_root/comparison.txt" EVIDENCE "$repository_root/scratchpad/refactor-comparison/evidence"

# Task previews stay in a copied include tree.
preview="$report_root/repository"
wiring_preview "$repository_root" "$report_root" performance:allocation-diagnostics:run refs/heads/measure/refactor-allocation-diagnostics
test ! -e "$report_root/blocked-command.log" || fail 'a target command executed during the preview'
test ! -e "$preview/scratchpad/refactor-allocation/evidence" || fail 'a diagnostic artifact was written during the preview'
verify_paths "$report_root/included-run-dry.txt" "$preview/scratchpad/refactor-allocation" "$preview"
if grep -Eq 'source--|source-[CI]-/|/evidence/(before|after)' "$report_root/included-run-dry.txt"; then
  fail 'nested task context was lost'
fi
[ "$(grep -Fc 'performance:allocation-diagnostics:snapshot] mkdir' "$report_root/included-run-dry.txt")" = 24 ] || fail 'nested snapshot count differs'
awk -F "'" -v prefix="$preview/scratchpad/refactor-allocation/evidence/" '
  /performance:allocation-diagnostics:snapshot] mkdir/ {destination=$2; sub(prefix, "", destination)}
  /test.*git rev-parse HEAD.*=/ {print destination "\t" $2}
' "$report_root/included-run-dry.txt" > "$report_root/actual-snapshots.tsv"
cmp "$report_root/expected-snapshots.tsv" "$report_root/actual-snapshots.tsv" || fail 'nested snapshot identities or destinations differ'


# Change only resolved command paths, leaving the expected variables intact.
awk '/^commands:$/ {commands=1} commands {sub(/mkdir -p .*\/evidence\/dependency/, "mkdir -p '\''/evidence/dependency")} {print}' "$report_root/default-run.txt" > "$report_root/escaped-command.txt"
if verify_summary "$report_root/escaped-command.txt" '' '' '' '' > "$report_root/negative.log" 2>&1; then
  fail 'escaped-path negative control passed'
fi
grep -Fq 'resolved path escapes diagnostic root: /evidence/dependency' "$report_root/negative.log" || fail 'negative control failed for an unrelated reason'
printf 'Validated %s included diagnostic summaries, the sibling namespace and the complete included dry-run. No target command executed.\nResolved paths: %s/resolved-paths.tsv\n' "$summary_count" "$report_root"
