#!/usr/bin/env bash
# Require executed Hspec assertions, preserving the original process status and log.
set -euo pipefail
directory="${1:?result directory}"
expected="${2:?pass or assertion text}"
status="$(cat "$directory/status")"
if [[ "$expected" == pass ]]; then
  test "$status" -eq 0
  grep -Eq '^[1-9][0-9]* examples?, 0 failures?$' "$directory/run.log"
else
  test "$status" -eq 1
  grep -Eq '^[1-9][0-9]* examples?, 1 failure$' "$directory/run.log"
  sed -n '/^Failures:/,/^Randomized with seed/p' "$directory/run.log" > "$directory/assertion.txt"
  grep -F -- "$expected" "$directory/assertion.txt"
  grep -Eq 'expected:|predicate failed on:' "$directory/assertion.txt"
fi
if grep -Eq '[1-9][0-9]* pending|PENDING:|reference oracles unavailable' "$directory/run.log"; then
  exit 1
fi
