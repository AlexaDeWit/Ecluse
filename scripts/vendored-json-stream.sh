#!/usr/bin/env bash
# Guard the vendored json-stream tree.
#   check  compares the tree with vendor/json-stream.sha256, offline: each tracked path with its
#          mode and blob, then the SHA-256 of each regular file.
#   pin    rewrites vendor/json-stream.sha256 from the tree, after an intended change.
#   diff   fetches the upstream commit the tree's README names, diffs every upstream file the tree
#          holds apart from the README, and checks that json-stream.freeze names upstream's
#          version at that commit. It exits 1 while any file differs, as three do by design.
set -euo pipefail

tree=vendor/json-stream
pins=vendor/json-stream.sha256
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
      echo "vendor/json-stream no longer matches $pins. After an intended change, run: task vendor-pin" >&2
      exit 1
    fi
    ;;
  pin)
    digests >"$pins"
    ;;
  diff)
    commit="$(grep -oE '`[0-9a-f]{40}`' "$tree/README.md" | head -n 1 | tr -d '`')"
    [[ -n "$commit" ]] || { echo "no upstream commit in $tree/README.md" >&2; exit 1; }
    work="$(mktemp -d)"
    trap 'rm -rf "$work"' EXIT
    git -C "$work" init -q
    git -C "$work" fetch -q --depth 1 "$upstream" "$commit"
    git -C "$work" checkout -q FETCH_HEAD
    status=0
    while IFS= read -r -d '' file; do
      relative="${file#"$tree"/}"
      [[ "$relative" != README.md && -f "$work/$relative" ]] || continue
      diff -u --label "upstream/$relative" --label "vendored/$relative" "$work/$relative" "$file" || status=1
    done < <(find "$tree" -type f -print0 | LC_ALL=C sort -z)
    version="$(sed -n 's/^version:[[:space:]]*//p' "$work/json-stream.cabal")"
    if ! grep -qx "constraints: any.json-stream ==$version" "$tree/json-stream.freeze"; then
      echo "$tree/json-stream.freeze does not name json-stream $version, upstream's version at $commit" >&2
      status=1
    fi
    exit "$status"
    ;;
  *)
    echo "expected check, pin or diff" >&2
    exit 2
    ;;
esac
