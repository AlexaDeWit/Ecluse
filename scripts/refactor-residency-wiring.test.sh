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
# Execute the real measure target with a recorder in the contained include tree.
mkdir -p "$preview/scripts"
cp "$repository_root/scripts/"{process-capture,process-group}.sh "$preview/scripts/"
execution_root="$report_root/execution"
execution_source="$execution_root/source-A"
mkdir -p "$execution_source/bench/corpus/npm" "$execution_root/evidence/build-A"
recorder="$execution_root/native recorder"
cat > "$recorder" <<'RECORDER'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\0' "$@" > "$RES_DIAG_CONTROL_ARGV_LOG"
printf '{"control":"argv"}\n'
printf 'native control stderr\n' >&2
RECORDER
chmod +x "$recorder"
printf '%s\n' "$recorder" > "$execution_root/evidence/build-A/binary-path.txt"
for probe in s3-heavy webpack-shared lodash-publish; do
  corpus=bench/corpus/npm/webpack.full.json
  [ "$probe" != s3-heavy ] || corpus=bench/corpus/npm/aws-sdk-client-s3.full.json
  [ "$probe" != lodash-publish ] || corpus=bench/corpus/npm/lodash.full.json
  printf 'original public bytes\n' > "$execution_source/$corpus"
  child_directory="$execution_root/evidence/child-A1-$probe"
  generation_directory="$execution_root/evidence/generate-A1-$probe"
  mkdir -p "$child_directory" "$generation_directory/live"
  private="$generation_directory/live/private input \$literal \"quoted\" *.json"
  printf 'original private bytes\n' > "$private"
  printf '%s\n' "$private" "$corpus" > "$generation_directory/returned.txt"
  args=(--metadata-probe Shared "$corpus" +RTS -T -N1 -RTS)
  if [ "$probe" != webpack-shared ]; then
    args=(--metadata-merge-probe "$private" "$corpus" +RTS -F1 -A128k -RTS "$corpus" +RTS -T -N1 -RTS)
  fi
  printf '%s\0' "${args[@]}" > "$child_directory/expected.argv"
  printf '%s\n' "$recorder" "${args[@]}" > "$child_directory/expected.command"
  RES_DIAG_CONTROL_ARGV_LOG="$child_directory/observed.argv" task --dir "$preview" \
    performance:residency-diagnostics:measure "RES_DIAG_ROOT=$execution_root" RES_DIAG_SIDE=A RES_DIAG_PASS=A1 "RES_DIAG_PROBE=$probe" \
    > "$child_directory/task.log" 2>&1
  cmp "$child_directory/expected.argv" "$child_directory/observed.argv" || fail 'executed native argv differs'
  cmp "$child_directory/expected.command" "$child_directory/command.txt" || fail 'recorded native command differs'
  cmp "$child_directory/inputs-before.sha256" "$child_directory/inputs-after.sha256" || fail 'executed input bytes changed'
  [ "$(cat "$child_directory/native.status")" = 0 ] || fail 'recorder status differs'
  [ "$(cat "$child_directory/native.json")" = '{"control":"argv"}' ] || fail 'stdout identity differs'
  [ "$(cat "$child_directory/native.stderr")" = 'native control stderr' ] || fail 'stderr identity differs'
done

# A malformed returned list must fail before the recorder starts.
rejected_child="$execution_root/evidence/child-A2-s3-heavy"
rejected_generation="$execution_root/evidence/generate-A2-s3-heavy"
mkdir -p "$rejected_child" "$rejected_generation"
printf '%s\n' "$private" > "$rejected_generation/returned.txt"
if RES_DIAG_CONTROL_ARGV_LOG="$rejected_child/observed.argv" task --dir "$preview" \
  performance:residency-diagnostics:measure "RES_DIAG_ROOT=$execution_root" RES_DIAG_SIDE=A RES_DIAG_PASS=A2 RES_DIAG_PROBE=s3-heavy \
  > "$rejected_child/task.log" 2>&1; then fail 'a merge input with one path passed'; fi
test ! -e "$rejected_child/observed.argv" || fail 'the recorder ran before input count qualification'
test ! -e "$rejected_child/native.status" || fail 'capture started before input count qualification'

printf 'Validated %s residency summaries, sibling namespaces, exact argv, eight helpers, twelve children and twenty-four nested snapshots. The preview executes no target command. Three contained argv recorders pass, and one malformed-input control stops before capture.\nEvidence: %s\n' "$summary_count" "$report_root"
