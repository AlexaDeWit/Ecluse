#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
# SPDX-License-Identifier: MIT

# Check resolved diagnostic commands through the repository Taskfile without running their actions.
set -euo pipefail

case "${1:-}" in
  focused)
    namespace=ALLOC_DIAG_
    diagnostic_directory=refactor-allocation
    diagnostic_target=performance:allocation-diagnostics
    diagnostic_branch=measure/refactor-allocation-diagnostics
    pass_count=12
    ;;
  full-history)
    namespace=FULL_DIAG_
    diagnostic_directory=refactor-full-history
    diagnostic_target=performance:full-history-diagnostic
    diagnostic_branch=measure/refactor-full-history
    pass_count=8
    ;;
  *) printf 'unknown fixed wiring recipe\n' >&2; exit 1 ;;
esac
repository_root="$(git rev-parse --show-toplevel)"
cd "$repository_root"
diagnostic_root="$repository_root/scratchpad/${diagnostic_directory}"
evidence_root="$diagnostic_root/evidence"
mkdir -p "$repository_root/scratchpad"
report_root="$(mktemp -d "$repository_root/scratchpad/${diagnostic_directory}-wiring.XXXXXX")"
baseline=690a4cd522f3146fc04ae42d6abac684a44a422c
candidate=ab5f4af6e55b90ae79392f3d10a9505295f1239f
summary_count=0
: > "$report_root/expected-snapshots.tsv"
printf 'task\tsource\tsourceDirectory\tbuildDirectory\tpassDirectory\tdestination\n' > "$report_root/resolved-paths.tsv"

fail() {
  printf '%s\n' "$*" >&2
  return 1
}

expect_var() {
  local file="$1" name="$2" expected="$3"
  grep -Fqx "  $name: \"$expected\"" "$file" || fail "resolved variable differs: $name in $file"
}

verify_summary() {
  local file="$1" side="$2" variant="$3" pass_name="$4" destination="$5"
  local source='' source_directory='' build_directory='' pass_directory=''
  local commands_file="$file.commands"
  expect_var "$file" ${namespace}ROOT "$diagnostic_root" || return 1
  expect_var "$file" ${namespace}EVIDENCE "$evidence_root" || return 1
  expect_var "$file" ${namespace}BASELINE "$baseline" || return 1
  expect_var "$file" ${namespace}CANDIDATE "$candidate" || return 1
  if [ -n "$side" ]; then
    source="$baseline"
    [ "$side" != B ] || source="$candidate"
    source_directory="$diagnostic_root/source-$variant-$side"
    build_directory="$evidence_root/build-$variant-$side"
    pass_directory="$evidence_root/$pass_name"
    expect_var "$file" ${namespace}SOURCE "$source" || return 1
    expect_var "$file" ${namespace}SOURCE_DIR "$source_directory" || return 1
    expect_var "$file" ${namespace}BUILD_DIR "$build_directory" || return 1
    expect_var "$file" ${namespace}PASS_DIR "$pass_directory" || return 1
  fi
  if [ -n "$destination" ]; then
    expect_var "$file" ${namespace}DEST "$destination" || return 1
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

verify_paths() {
  local commands_file="$1" expected_root="$2" expected_repository="$3" quoted path normalised
  local quoted_pattern="['\"]/[^'\"]*['\"]"
  local -a quoted_paths=()
  mapfile -t quoted_paths < <(grep -oE "$quoted_pattern" "$commands_file")
  [ "${#quoted_paths[@]}" -gt 0 ] || fail "no resolved paths: $commands_file" || return 1
  for quoted in "${quoted_paths[@]}"; do
    path="${quoted:1:${#quoted}-2}"
    normalised="$(realpath -m -- "$path")"
    case "$normalised" in
      "$expected_root"|"$expected_root"/*|"$expected_repository") ;;
      "$expected_repository/scripts/process-capture.sh"|"$expected_repository/scripts/process-status.sh"|"$expected_repository/scripts/full-history-measure.sh") ;;
      *) fail "resolved path escapes diagnostic root: $path"; return 1 ;;
    esac
  done
}

summarise() {
  local label="$1" action="$2" side="$3" variant="$4" pass_name="$5" mode="$6" destination="$7"
  local file="$report_root/$label.txt"
  local -a args=(--summary "${diagnostic_target}:$action")
  if [ -n "$side" ]; then
    args+=("${namespace}SIDE=$side" "${namespace}VARIANT=$variant" "${namespace}PASS=$pass_name" "${namespace}MODE=$mode")
  fi
  [ -z "$destination" ] || args+=("${namespace}DEST=$destination")
  # Unrelated namespaces must not change any diagnostic path or fixed source.
  args+=(EVIDENCE=/collision BASELINE=collision CANDIDATE=collision COMPARISON_ROOT=/collision SOURCE=collision SOURCE_DIR=/collision BUILD_DIR=/collision PASS_DIR=/collision DEST=/collision SIDE=collision VARIANT=collision PASS=collision MODE=collision)
  task "${args[@]}" > "$file"
  verify_summary "$file" "$side" "$variant" "$pass_name" "$destination"
  summary_count=$((summary_count + 1))
}

if [ "$namespace" = FULL_DIAG_ ]; then
  awk '
    /task: prepare/ {builds++}
    /bench-report" full-history-plans/ {if (builds != 4 || launched) exit 1; guarded=1}
    /task: record-pass/ {if (!guarded) exit 1; launched=1}
    END {if (!guarded || !launched) exit 1}
  ' taskfiles/refactor-full-history.yml || fail 'full plan guard does not precede every measured launch'
fi
task --summary ${diagnostic_target}:run > "$report_root/default-run.txt"
verify_summary "$report_root/default-run.txt" '' '' '' ''
summary_count=$((summary_count + 1))
summarise run run '' '' '' '' ''
summarise dependency dependency '' '' '' '' ''
for variant in C I; do
  for side in A B; do
    summarise "prepare-$variant-$side" prepare "$side" "$variant" '' '' ''
    summarise "build-$variant-$side" build "$side" "$variant" '' '' ''
    summarise "snapshot-$variant-$side" snapshot "$side" "$variant" '' '' "$evidence_root/build-$variant-$side/identity"
    grep -Fq "${diagnostic_target}:build ${namespace}SIDE=$side ${namespace}VARIANT=$variant" "$report_root/prepare-$variant-$side.txt" || fail "build call drops its context"
  done
done
if [ "$namespace" = FULL_DIAG_ ]; then
  cat > "$report_root/schedule.txt" <<'SCHEDULE'
A C C-A1 native
B C C-B1 native
B I M-B1 marker-only
A I M-A1 marker-only
A I I-A1 record
B I I-B1 record
B I I-B2 record
A I I-A2 record
SCHEDULE
else
  cat > "$report_root/schedule.txt" <<'SCHEDULE'
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
fi
while read -r side variant pass_name mode; do
  summarise "record-$pass_name" record-pass "$side" "$variant" "$pass_name" "$mode" ''
  summarise "measure-$pass_name" measure "$side" "$variant" "$pass_name" "$mode" ''
  source="$baseline"
  [ "$side" != B ] || source="$candidate"
  for phase in before after; do
    printf '%s\t%s\n' "$pass_name/$phase" "$source" >> "$report_root/expected-snapshots.tsv"
    summarise "snapshot-$pass_name-$phase" snapshot "$side" "$variant" "$pass_name" "$mode" "$evidence_root/$pass_name/$phase"
  done
  grep -Fq "${diagnostic_target}:measure ${namespace}SIDE=$side ${namespace}VARIANT=$variant ${namespace}PASS=$pass_name ${namespace}MODE=$mode" "$report_root/record-$pass_name.txt" || fail "measurement call drops its context"
  grep -Fqx "        vars: {${namespace}SIDE: $side, ${namespace}VARIANT: $variant, ${namespace}PASS: $pass_name, ${namespace}MODE: $mode}" taskfiles/${diagnostic_directory}.yml || fail "run call differs for $pass_name"
  if [ "$namespace" = FULL_DIAG_ ]; then
  grep -Fq "scripts/full-history-measure.sh' '$repository_root' '$evidence_root' '$evidence_root/build-$variant-$side' '$evidence_root/$pass_name' '$mode'" "$report_root/measure-$pass_name.txt" || fail "measurement helper drops its context"
  if grep -Fq " -p " "$report_root/measure-$pass_name.txt"; then fail "full measurement selects a prefix"; fi
  fi
done < "$report_root/schedule.txt"

[ "$(grep -Fc " - Task: ${diagnostic_target}:prepare" "$report_root/run.txt")" = 4 ] || fail 'build schedule differs'
[ "$(grep -Fc " - Task: ${diagnostic_target}:record-pass" "$report_root/run.txt")" = "$pass_count" ] || fail 'pass schedule differs'
[ "$(grep -Fc "    dir: '{{.${namespace}SOURCE_DIR}}'" "taskfiles/$diagnostic_directory.yml")" = 3 ] || fail 'source working directories differ'
grep -Fqx "        vars: {${namespace}SIDE: '{{.${namespace}SIDE}}', ${namespace}VARIANT: '{{.${namespace}VARIANT}}', ${namespace}PASS: '{{.${namespace}PASS}}', ${namespace}MODE: '{{.${namespace}MODE}}', ${namespace}DEST: '{{.${namespace}BUILD_DIR}}/identity'}" "taskfiles/$diagnostic_directory.yml" || fail 'build snapshot destination differs'
for phase in before after; do
  grep -Fqx "        vars: {${namespace}SIDE: '{{.${namespace}SIDE}}', ${namespace}VARIANT: '{{.${namespace}VARIANT}}', ${namespace}PASS: '{{.${namespace}PASS}}', ${namespace}MODE: '{{.${namespace}MODE}}', ${namespace}DEST: '{{.${namespace}PASS_DIR}}/$phase'}" "taskfiles/$diagnostic_directory.yml" || fail 'pass snapshot destination differs'
done
grep -Fqx '      - test "$(uname -m)" = aarch64' "taskfiles/$diagnostic_directory.yml" || fail 'ARM64 guard differs'

REFRACTOR_CANDIDATE=sibling-control task --summary performance:comparison:run > "$report_root/comparison.txt"
expect_var "$report_root/comparison.txt" CANDIDATE sibling-control
expect_var "$report_root/comparison.txt" EVIDENCE "$repository_root/scratchpad/refactor-comparison/evidence"

# Task dry-run creates working directories. Confine them to a copied include tree, never a product worktree.
preview="$report_root/repository"
mkdir -p "$preview" "$report_root/bin"
cp Taskfile.yml "$preview/Taskfile.yml"
cp -a taskfiles "$preview/taskfiles"
cat > "$report_root/bin/uname" <<'PLATFORM'
#!/usr/bin/env bash
set -euo pipefail
test "${1:-}" = -m
printf 'aarch64\n'
PLATFORM
cat > "$report_root/bin/blocked" <<'BLOCKED'
#!/usr/bin/env bash
printf '%s\n' "$0" >> "$WIRING_BLOCK_LOG"
exit 125
BLOCKED
chmod +x "$report_root/bin/uname" "$report_root/bin/blocked"
for executable in mkdir git cabal ghc curl cp tar patch timeout gzip lscpu sha256sum find bash; do
  ln -s blocked "$report_root/bin/$executable"
done
# These shims use the real Bash directly because Bash itself must be blocked as a target command.
sed -i "1c#!$(command -v bash)" "$report_root/bin/uname" "$report_root/bin/blocked"
env -u GHCRTS PATH="$report_root/bin:$PATH" WIRING_BLOCK_LOG="$report_root/blocked-command.log" GITHUB_REF=refs/heads/${diagnostic_branch} \
  task --dir "$preview" --dry ${diagnostic_target}:run > "$report_root/included-run-dry.txt" 2>&1
test ! -e "$report_root/blocked-command.log" || fail 'a target command executed during the preview'
test ! -e "$preview/scratchpad/${diagnostic_directory}/evidence" || fail 'a diagnostic artifact was written during the preview'
verify_paths "$report_root/included-run-dry.txt" "$preview/scratchpad/${diagnostic_directory}" "$preview"
if grep -Eq 'source--|source-[CI]-/|/evidence/(before|after)' "$report_root/included-run-dry.txt"; then
  fail 'nested task context was lost'
fi
[ "$(grep -Fc "${diagnostic_target}:snapshot] mkdir" "$report_root/included-run-dry.txt")" = "$((pass_count * 2))" ] || fail 'nested snapshot count differs'
awk -F "'" -v prefix="$preview/scratchpad/$diagnostic_directory/evidence/" -v target="$diagnostic_target" '
  $0 ~ target ":snapshot] mkdir" {destination=$2; sub(prefix, "", destination)}
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

grep -Fqx '      - test "$GITHUB_REF" = refs/heads/measure/refactor-allocation-diagnostics' taskfiles/refactor-allocation.yml || fail 'focused branch guard differs'
grep -Fq "args=(--csv '{{.ALLOC_DIAG_PASS_DIR}}/native.csv' --stdev 10 -p" taskfiles/refactor-allocation.yml || fail 'focused selection differs'
grep -Fq 'timeout --signal=TERM --kill-after=30s 10m' taskfiles/refactor-allocation.yml || fail 'focused process guard differs'
