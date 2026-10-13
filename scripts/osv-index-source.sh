#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
# SPDX-License-Identifier: MIT
set -euo pipefail
out="${1:?fresh source directory}"
mkdir "$out"
git ls-files > "$out/tracked.txt"
git archive HEAD > "$out/harness.tar"
for variant in A B; do
  mkdir "$out/$variant"
  tar -xf "$out/harness.tar" -C "$out/$variant"
done
source_file=core/src/Ecluse/Core/Osv/Compile.hs
needle='    execute_ conn "CREATE UNIQUE INDEX uq_ranges_segment ON package_vulnerability_ranges(package_name, cve_id, introduced_version, fixed_version, last_affected_version)"'
test "$(grep -Fxc "$needle" "$out/B/$source_file")" -eq 1
sed '/CREATE UNIQUE INDEX uq_ranges_segment ON package_vulnerability_ranges/d' "$out/B/$source_file" > "$out/Compile.hs"
mv "$out/Compile.hs" "$out/B/$source_file"
status=0
diff -u --label "A/$source_file" --label "B/$source_file" "$out/A/$source_file" "$out/B/$source_file" > "$out/source-diff.patch" || status=$?
test "$status" -eq 1
rm "$out/harness.tar"
