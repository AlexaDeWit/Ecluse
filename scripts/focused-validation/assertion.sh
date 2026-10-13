#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
#
# SPDX-License-Identifier: MIT

# Require executed Hspec assertions, preserving the original process status and log.
set -euo pipefail
directory="${1:?result directory}"
expected="${2:?pass or assertion text}"
kind="${3:-expectation}"
[[ "$kind" == expectation || "$kind" == reader-field ]]
status="$(cat "$directory/status")"
if [[ "$expected" == pass ]]; then
  test "$status" -eq 0
  grep -Eq '^[1-9][0-9]* examples?, 0 failures?$' "$directory/run.log"
  if [[ "$kind" == reader-field ]]; then
    grep -Fx '1 example, 0 failures' "$directory/run.log"
    grep -Eq '^[[:space:]]*passed 2000 tests\.$' "$directory/run.log"
  fi
else
  test "$status" -eq 1
  grep -Eq '^[1-9][0-9]* examples?, 1 failure$' "$directory/run.log"
  sed -n '/^Failures:/,/^Randomized with seed/p' "$directory/run.log" > "$directory/assertion.txt"
  grep -F -- "$expected" "$directory/assertion.txt"
  if [[ "$kind" == reader-field ]]; then
    grep -Fx '1 example, 1 failure' "$directory/run.log"
    grep -Eq 'failed after [1-9][0-9]* tests?([ ,.]|$)' "$directory/assertion.txt"
    grep -F 'focused-emitted-field-mismatch' "$directory/assertion.txt"
  else
    grep -Eq 'expected:|predicate failed on:' "$directory/assertion.txt"
  fi
fi
if grep -Eq '[1-9][0-9]* pending|PENDING:|reference oracles unavailable' "$directory/run.log"; then
  exit 1
fi
