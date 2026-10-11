#!/usr/bin/env bash
# Pin the historical C reference and compare the maintained fork with its recorded upstream.
set -euo pipefail

tree=test/fuzz/json-lexer/reference
pins=test/fuzz/json-lexer/reference.sha256
fork=json-stream
upstream=https://github.com/ondrap/json-stream.git
cd "$(git rev-parse --show-toplevel)"

# git's index entries catch an added, removed or renamed path, a mode change and a symlink.
digests() {
  git ls-files --stage -- "$tree"
  git ls-files --stage -z -- "$tree" | while IFS= read -r -d '' entry; do
    case "$entry" in
      100644\ * | 100755\ *) sha256sum -- "${entry#*$'\t'}" ;;
    esac
  done
}

case "${1:?expected check, pin or diff}" in
  check)
    if ! diff -u --label "$pins" --label "$tree (as it is)" "$pins" <(digests); then
      echo "$tree no longer matches $pins. After an intended reference change, run: task json-reference-pin" >&2
      exit 1
    fi
    ;;
  pin)
    digests >"$pins"
    ;;
  diff)
    commit="$(grep -oE '`[0-9a-f]{40}`' "$fork/README.md" | head -n 1 | tr -d '`')"
    [[ -n "$commit" ]] || { echo "no upstream commit in $fork/README.md" >&2; exit 1; }
    work="$(mktemp -d)"
    trap 'rm -rf "$work"' EXIT
    git -C "$work" init -q
    git -C "$work" fetch -q --depth 1 "$upstream" "$commit"
    git -C "$work" checkout -q FETCH_HEAD
    status=0
    while IFS= read -r -d '' file; do
      case "$file" in
        "$fork"/src/*) relative="${file#"$fork"/src/}" ;;
        "$tree"/*) relative="c_lib/${file#"$tree"/}" ;;
        *) continue ;;
      esac
      [[ -f "$work/$relative" ]] || continue
      diff -u --label "upstream/$relative" --label "$file" "$work/$relative" "$file" || status=1
    done < <(find "$fork/src" "$tree" -type f -print0 | LC_ALL=C sort -z)
    version="$(sed -n 's/^version:[[:space:]]*//p' "$work/json-stream.cabal")"
    if ! grep -qx "constraints: any.json-stream ==$version" "$fork/upstream.freeze"; then
      echo "$fork/upstream.freeze does not name json-stream $version, upstream's version at $commit" >&2
      status=1
    fi
    exit "$status"
    ;;
  *)
    echo "expected check, pin or diff" >&2
    exit 2
    ;;
esac
