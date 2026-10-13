#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
# SPDX-License-Identifier: MIT

# Inspect exact child arguments and nested snapshots through the real Task include tree.
set -euo pipefail
repository_root="$(git rev-parse --show-toplevel)"
cd "$repository_root"
source scripts/refactor-wiring-support.sh
mkdir -p scratchpad
report_root="$(mktemp -d "$repository_root/scratchpad/refactor-residency-wiring.XXXXXX")"
diagnostic_root="$repository_root/scratchpad/refactor-residency-diagnostic"
evidence_root="$diagnostic_root/evidence"
baseline=690a4cd522f3146fc04ae42d6abac684a44a422c
candidate=ab5f4af6e55b90ae79392f3d10a9505295f1239f
poison=(EVIDENCE=/collision BASELINE=collision CANDIDATE=collision COMPARISON_ROOT=/collision SOURCE=collision SOURCE_DIR=/collision BUILD_DIR=/collision PASS_DIR=/collision DEST=/collision SIDE=collision VARIANT=collision PASS=collision MODE=collision ALLOC_DIAG_ROOT=/collision ALLOC_DIAG_EVIDENCE=/collision)
summary_count=0
: > "$report_root/expected-snapshots.tsv"

summary() {
  local label="$1" action="$2" side="$3" pass_name="$4" probe="$5" destination="$6"
  local file="$report_root/$label.txt" source="$baseline"
  local -a args=(--summary "performance:residency-diagnostics:$action" "${poison[@]}")
  [ -z "$side" ] || args+=("RES_DIAG_SIDE=$side")
  [ -z "$pass_name" ] || args+=("RES_DIAG_PASS=$pass_name")
  [ -z "$probe" ] || args+=("RES_DIAG_PROBE=$probe")
  [ -z "$destination" ] || args+=("RES_DIAG_DEST=$destination")
  task "${args[@]}" > "$file"
  expect_var "$file" RES_DIAG_ROOT "$diagnostic_root"
  expect_var "$file" RES_DIAG_EVIDENCE "$evidence_root"
  expect_var "$file" RES_DIAG_BASELINE "$baseline"
  expect_var "$file" RES_DIAG_CANDIDATE "$candidate"
  if [ -n "$side" ]; then
    [ "$side" != B ] || source="$candidate"
    expect_var "$file" RES_DIAG_SOURCE "$source"
    expect_var "$file" RES_DIAG_SOURCE_DIR "$diagnostic_root/source-$side"
    expect_var "$file" RES_DIAG_HELPER_DIR "$diagnostic_root/helper-$side"
    expect_var "$file" RES_DIAG_BUILD_DIR "$evidence_root/build-$side"
    expect_var "$file" RES_DIAG_HELPER_BUILD "$evidence_root/helper-build-$side"
  fi
  awk '/^commands:$/ {commands=1; next} commands {print}' "$file" > "$file.commands"
  if grep -Eq '\{\{|<no value>|/collision' "$file.commands"; then fail "unresolved or contaminated commands: $label"; fi
  verify_paths "$file.commands" "$diagnostic_root" "$repository_root"
  summary_count=$((summary_count + 1))
}

summary run run '' '' '' ''
for side in A B; do
  summary "prepare-native-$side" prepare-native "$side" '' '' ''
  summary "prepare-helper-$side" prepare-helper "$side" '' '' ''
  summary "build-helper-$side" build-helper "$side" '' '' ''
  grep -Fq "performance:comparison:prepare SIDE=$side COMPARISON_ROOT='$diagnostic_root' EVIDENCE='$evidence_root' BASELINE='$baseline' CANDIDATE='$candidate'" "$report_root/prepare-native-$side.txt" || fail 'native preparation drops its paths'
  grep -Fq "performance:residency-diagnostics:build-helper RES_DIAG_SIDE=$side" "$report_root/prepare-helper-$side.txt" || fail 'helper build drops its side'
  task --summary performance:comparison:prepare "SIDE=$side" "COMPARISON_ROOT=$diagnostic_root" "EVIDENCE=$evidence_root" "BASELINE=$baseline" "CANDIDATE=$candidate" > "$report_root/native-prepare-$side.txt"
  grep -Fq "performance:comparison:build SIDE=$side COMPARISON_ROOT='$diagnostic_root' EVIDENCE='$evidence_root' BASELINE='$baseline' CANDIDATE='$candidate'" "$report_root/native-prepare-$side.txt" || fail 'shared native build drops its paths'
done
for pass_name in A1 B1 A2 B2; do
  side="${pass_name:0:1}"
  source="$baseline"
  [ "$side" != B ] || source="$candidate"
  for probe in s3-heavy webpack-shared lodash-publish; do
    summary "record-$pass_name-$probe" record-child "$side" "$pass_name" "$probe" ''
    summary "measure-$pass_name-$probe" measure "$side" "$pass_name" "$probe" ''
    for phase in before after; do
      destination="$evidence_root/child-$pass_name-$probe/$phase"
      printf '%s\t%s\n' "child-$pass_name-$probe/$phase" "$source" >> "$report_root/expected-snapshots.tsv"
      summary "snapshot-$pass_name-$probe-$phase" snapshot "$side" "$pass_name" "$probe" "$destination"
      grep -Fq "IDENTITY_DIR='$destination'" "$report_root/snapshot-$pass_name-$probe-$phase.txt" || fail 'snapshot destination differs'
      task --summary performance:comparison:identity "SIDE=$side" "SOURCE=$source" "SOURCE_DIR=$diagnostic_root/source-$side" "IDENTITY_DIR=$destination" "COMPARISON_ROOT=$diagnostic_root" "EVIDENCE=$evidence_root" "BASELINE=$baseline" "CANDIDATE=$candidate" > "$report_root/native-identity-$pass_name-$probe-$phase.txt"
      expect_var "$report_root/native-identity-$pass_name-$probe-$phase.txt" SOURCE "$source"
      expect_var "$report_root/native-identity-$pass_name-$probe-$phase.txt" SOURCE_DIR "$diagnostic_root/source-$side"
      expect_var "$report_root/native-identity-$pass_name-$probe-$phase.txt" IDENTITY_DIR "$destination"
    done
    if [ "$probe" = webpack-shared ]; then
      grep -Fq 'args=(--metadata-probe Shared '\''bench/corpus/npm/webpack.full.json'\'' +RTS -T -N1 -RTS)' "$report_root/measure-$pass_name-$probe.txt" || fail 'Shared argv differs'
      if grep -Fq -- '-A128k' "$report_root/measure-$pass_name-$probe.txt"; then fail 'Shared inherits merge RTS'; fi
    else
      summary "generate-$pass_name-$probe" generate "$side" "$pass_name" "$probe" ''
      for phase in before after; do
        grep -Fq "cmp '$evidence_root/helper-build-$side/executable.sha256' '$evidence_root/generate-$pass_name-$probe/helper-$phase.sha256'" "$report_root/generate-$pass_name-$probe.txt" || fail 'live helper hash check is missing'
      done
      grep -Fq 'args=(--metadata-merge-probe "${inputs[@]}" +RTS -F1 -A128k -RTS' "$report_root/measure-$pass_name-$probe.txt" || fail 'merge argv differs'
    fi
  done
done

build_gate="$(awk '/residency-diagnostic-builds/ {print NR; exit}' "$report_root/run.txt.commands")"
first_helper="$(awk '/Task: performance:residency-diagnostics:generate/ {print NR; exit}' "$report_root/run.txt.commands")"
preparation_gate="$(awk '/residency-diagnostic-inputs/ {print NR; exit}' "$report_root/run.txt.commands")"
first_child="$(awk '/Task: performance:residency-diagnostics:record-child/ {print NR; exit}' "$report_root/run.txt.commands")"
[ -n "$build_gate" ] && [ -n "$first_helper" ] && [ "$build_gate" -lt "$first_helper" ] || fail 'build preflight does not precede generation'
[ -n "$preparation_gate" ] && [ -n "$first_child" ] && [ "$preparation_gate" -lt "$first_child" ] || fail 'preparation preflight does not precede measurements'
grep -Fq -- "--stderr '$evidence_root/child-A1-s3-heavy/native.stderr' timeout" "$report_root/measure-A1-s3-heavy.txt" || fail 'native stderr is not separate'

[ "$(grep -Fc ' - Task: performance:residency-diagnostics:record-child' "$report_root/run.txt")" = 12 ] || fail 'child schedule differs'
[ "$(grep -Fc ' - Task: performance:residency-diagnostics:generate' "$report_root/run.txt")" = 8 ] || fail 'helper schedule differs'
REFRACTOR_CANDIDATE=sibling-control task --summary performance:comparison:run > "$report_root/comparison.txt"
expect_var "$report_root/comparison.txt" CANDIDATE sibling-control
expect_var "$report_root/comparison.txt" EVIDENCE "$repository_root/scratchpad/refactor-comparison/evidence"
task --summary performance:allocation-diagnostics:run > "$report_root/allocation.txt"
expect_var "$report_root/allocation.txt" ALLOC_DIAG_ROOT "$repository_root/scratchpad/refactor-allocation"

wiring_preview "$repository_root" "$report_root" performance:residency-diagnostics:run refs/heads/measure/refactor-residency-diagnostic
preview="$report_root/repository"
test ! -e "$preview/scratchpad/refactor-residency-diagnostic/evidence" || fail 'preview wrote diagnostic evidence'
verify_paths "$report_root/included-run-dry.txt" "$preview/scratchpad/refactor-residency-diagnostic" "$preview"
[ "$(grep -Fc 'performance:residency-diagnostics:snapshot] task --exit-code performance:comparison:identity' "$report_root/included-run-dry.txt")" = 24 ] || fail 'nested snapshot count differs'
awk -F "'" -v prefix="$preview/scratchpad/refactor-residency-diagnostic/evidence/" '
  /residency-diagnostics:snapshot] task.*performance:comparison:identity/ {source=$2; destination=$6; sub(prefix, "", destination); print destination "\t" source}
' "$report_root/included-run-dry.txt" > "$report_root/actual-snapshots.tsv"
cmp "$report_root/expected-snapshots.tsv" "$report_root/actual-snapshots.tsv" || fail 'nested snapshots differ'

native_report="$report_root/native-build"
mkdir -p "$native_report"
native_root="$native_report/repository/scratchpad/refactor-residency-diagnostic"
wiring_preview "$repository_root" "$native_report" performance:comparison:build refs/heads/measure/refactor-residency-diagnostic \
  SIDE=B "COMPARISON_ROOT=$native_root" "EVIDENCE=$native_root/evidence" "BASELINE=$baseline" "CANDIDATE=$candidate"
verify_paths "$native_report/included-run-dry.txt" "$native_root" "$native_report/repository"
grep -Fq "performance:comparison:verify-source] test \"\$(git rev-parse HEAD)\" = '$candidate'" "$native_report/included-run-dry.txt" || fail 'nested native source identity differs'
grep -Fq "HEAD HEAD^{tree} > '$native_root/evidence/build-B/source.txt'" "$native_report/included-run-dry.txt" || fail 'nested native build snapshot differs'

# A changed command path must fail even when declared variables remain intact.
sed "s|mkdir -p '$evidence_root/overlay'|mkdir -p '/evidence/overlay'|" "$report_root/run.txt.commands" > "$report_root/escaped.commands"
if verify_paths "$report_root/escaped.commands" "$diagnostic_root" "$repository_root" > "$report_root/negative.log" 2>&1; then fail 'escaped command accepted'; fi
grep -Fq 'resolved path escapes diagnostic root: /evidence/overlay' "$report_root/negative.log" || fail 'path control failed for an unrelated reason'
printf 'Validated %s residency summaries, sibling namespaces, exact argv, eight helpers, twelve children and twenty-four nested snapshots. No target command executed.\nEvidence: %s\n' "$summary_count" "$report_root"
