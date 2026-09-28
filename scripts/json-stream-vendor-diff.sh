#!/usr/bin/env bash
# Diff vendor/json-stream against the json-stream commit it was taken from. Each vendored file
# carries an added SPDX header, which the comparison strips first. Exits 1 when a file differs.
set -euo pipefail

repository=https://github.com/AlexaDeWit/json-stream.git
commit=520e25758baa5b2665b45eee71ecf8e6a9759868
root="$(git rev-parse --show-toplevel)"
vendored="$root/vendor/json-stream"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

git -C "$work" init -q upstream
git -C "$work/upstream" fetch -q --depth 1 "$repository" "$commit"
git -C "$work/upstream" checkout -q FETCH_HEAD

# Drop the leading SPDX comment block and the blank line after it, keeping every other byte.
strip_header() {
  local lines
  lines="$(awk '/^(--|\/\/)( SPDX-.*)?$/ { next } /^$/ { print NR; exit } { print 0; exit }' "$1")"
  tail -n "+$((${lines:-0} + 1))" "$1"
}

status=0
while IFS= read -r -d '' file; do
  relative="${file#"$vendored"/}"
  [[ "$relative" == README.md ]] && continue
  if ! diff -u --label "upstream/$relative" --label "vendored/$relative" \
    "$work/upstream/$relative" <(strip_header "$file"); then
    status=1
  fi
done < <(find "$vendored" -type f -print0 | sort -z)
exit "$status"
