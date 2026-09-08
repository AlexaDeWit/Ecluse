#!/usr/bin/env bash
# Exercise index discovery and the optional desktop opener without building docs.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
docs_tmp="$(mktemp -d)"
trap 'rm -rf "$docs_tmp"' EXIT
bash_bin="$(command -v bash)"
mkdir -p "$docs_tmp/bin"
ln -s "$(command -v find)" "$docs_tmp/bin/find"
cd "$docs_tmp"

for directory in missing empty; do
  if [ "$directory" = empty ]; then
    mkdir dist-newstyle
  fi
  status=0
  timeout 5 env PATH="$docs_tmp/bin" "$bash_bin" "$here/docs-open.sh" >output 2>error || status=$?
  [ "$status" = 1 ] || { echo "FAIL: $directory build directory must fail with exit 1"; exit 1; }
  [[ "$(<error)" == *'docs: no Haddock index found under dist-newstyle/ (did Haddock run?)'* ]] || {
    echo "FAIL: $directory build directory must name the missing Haddock index"
    exit 1
  }
done

html='dist-newstyle/build with spaces/doc/html/ecluse/index.html'
mkdir -p "${html%/*}"
touch "$html"
timeout 5 env PATH="$docs_tmp/bin" "$bash_bin" "$here/docs-open.sh" >output
[ "$(<output)" = "Haddock: $html" ] || { echo 'FAIL: index without opener'; exit 1; }

xdg-open() {
  printf '%s\n' "$@" >opened
}
export -f xdg-open
timeout 5 env PATH="$docs_tmp/bin" "$bash_bin" -c 'source "$1"; wait' bash "$here/docs-open.sh" >output
[ "$(<opened)" = "$html" ] || { echo 'FAIL: opener must receive the index path'; exit 1; }
[ "$(<output)" = "Haddock: $html" ] || { echo 'FAIL: index with opener'; exit 1; }
echo 'ok: Haddock discovery and optional opener'
