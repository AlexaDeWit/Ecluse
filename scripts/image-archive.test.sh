#!/usr/bin/env bash
# Deterministic unit test for image-archive.sh. Each fixture is a small docker-archive in
# the layout the Nix image build writes: store layers with absolute member names, a top
# layer of ./bin links, and one directory per layer named for its digest. The cases that
# matter most are the refusals: a second Écluse program, and two archives that differ.
# Run via `task test-scripts`.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
script="$here/image-archive.sh"

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

fail=0
hash=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
ecluse="nix/store/$hash-ecluse-9.9.9"

# Write a file into a tree. $1 tree, $2 path, $3 content (the path when omitted).
put() {
  mkdir -p "$1/$(dirname "$2")"
  printf '%s' "${3:-$2}" > "$1/$2"
}

# The image as it ships: one Écluse program, its /bin link, and the linked libraries
# with the programs their store paths carry.
shipped_image() {
  put "$1/store" "$ecluse/bin/ecluse"
  put "$1/store" "nix/store/$hash-glibc-2.42-67/lib/libc.so.6"
  put "$1/store" "nix/store/$hash-glibc-2.42-67/libexec/getconf/POSIX_V7_LP64_OFF64"
  put "$1/store" "nix/store/$hash-numactl-2.0.18/lib/libnuma.so.1"
  put "$1/store" "nix/store/$hash-numactl-2.0.18/bin/numactl"
  put "$1/store" "nix/store/$hash-zstd-1.5.7/bin/pzstd"
  put "$1/store" "nix/store/$hash-nss-cacert-3.126/etc/ssl/certs/ca-bundle.crt"
  mkdir -p "$1/root/bin"
  ln -s "/$ecluse/bin/ecluse" "$1/root/bin/ecluse"
}

# Pack a tree into a gzip docker-archive. $1 archive, $2 tree, $3 member mtime (0 when omitted).
pack() {
  local stage="$work/stage" fixed=(--sort=name --owner=0 --group=0 --numeric-owner) layer
  rm -rf "$stage"
  mkdir -p "$stage/image"
  tar -C "$2/store" -cPf "$stage/store.tar" "${fixed[@]}" --mtime="@${3:-0}" \
    --transform 's#^nix#/nix#' nix
  tar -C "$2/root" -cf "$stage/root.tar" "${fixed[@]}" --mtime="@${3:-0}" ./bin
  for layer in store root; do
    mkdir "$stage/image/$(sha256sum "$stage/$layer.tar" | cut -d' ' -f1)"
    mv "$stage/$layer.tar" "$stage/image/$(sha256sum "$stage/$layer.tar" | cut -d' ' -f1)/layer.tar"
  done
  printf '{}' > "$stage/image/config.json"
  printf '[]' > "$stage/image/manifest.json"
  (cd "$stage/image" && tar -czf "$1" "${fixed[@]}" --mtime=@0 -- *)
}

# Assert one run. $1 name, $2 expected exit, $3 a pattern its output must hold ("" for
# none), then the script's arguments.
check() {
  local name="$1" want="$2" pattern="$3" got=0 out
  shift 3
  out="$(bash "$script" "$@" 2>&1)" || got=$?
  if [ "$got" != "$want" ]; then
    printf 'FAIL - %s (want exit %s, got %s)\n' "$name" "$want" "$got"
    fail=1
  elif [ -n "$pattern" ] && ! grep -Eq -- "$pattern" <<< "$out"; then
    printf 'FAIL - %s (output lacks %s)\n' "$name" "$pattern"
    fail=1
  else
    printf 'ok   - %s\n' "$name"
  fi
}

shipped_image "$work/shipped"
pack "$work/shipped.tar" "$work/shipped"
check "passes an image whose only Écluse program is bin/ecluse" 0 \
  "^ok +/$ecluse/bin/ecluse$" executables "$work/shipped.tar"

shipped_image "$work/tool"
put "$work/tool/store" "$ecluse/bin/site-gen"
pack "$work/tool.tar" "$work/tool"
check "fails on a second program in the ecluse store path" 1 \
  "^FAILED +/$ecluse/bin/site-gen:" executables "$work/tool.tar"

shipped_image "$work/link"
ln -s "/$ecluse/bin/site-gen" "$work/link/root/bin/site-gen"
pack "$work/link.tar" "$work/link"
check "fails on a second link in /bin" 1 \
  "^FAILED +/bin/site-gen:" executables "$work/link.tar"

shipped_image "$work/shell"
put "$work/shell/store" "nix/store/$hash-busybox-1.36.1/bin/sh"
pack "$work/shell.tar" "$work/shell"
check "fails on a program from a package that is not a listed library" 1 \
  "^FAILED +/nix/store/$hash-busybox-1.36.1/bin/sh: busybox-1.36.1 " executables "$work/shell.tar"

shipped_image "$work/nested"
put "$work/nested/store" "$ecluse/lib/helpers/libexec/probe"
pack "$work/nested.tar" "$work/nested"
check "fails on an Écluse program in a nested libexec directory" 1 \
  "^FAILED +/$ecluse/lib/helpers/libexec/probe:" executables "$work/nested.tar"

shipped_image "$work/absent"
rm "$work/absent/store/$ecluse/bin/ecluse"
pack "$work/absent.tar" "$work/absent"
check "fails when no store path holds bin/ecluse" 1 \
  "^FAILED +0 store paths hold bin/ecluse" executables "$work/absent.tar"

printf 'not an archive' > "$work/garbage.tar"
check "refuses a file that is not an archive" 2 "" executables "$work/garbage.tar"

tar -cf "$work/empty.tar" -T /dev/null
check "refuses an archive without layers" 2 "" executables "$work/empty.tar"

check "refuses an unknown command" 2 "^usage:" inspect "$work/shipped.tar"

cp "$work/shipped.tar" "$work/copy.tar"
check "passes two archives with the same bytes" 0 \
  "^ok +the two builds gave the same archive$" compare "$work/shipped.tar" "$work/copy.tar"

# Same size, different bytes: only a content digest can tell the two files apart.
shipped_image "$work/rebuilt"
put "$work/rebuilt/store" "$ecluse/bin/ecluse" "${ecluse//e/E}/bin/ecluse"
pack "$work/rebuilt.tar" "$work/rebuilt"
check "fails on a file whose bytes differ, and names it" 1 \
  "^> [0-9a-f]{64}  [0-9]+  /$ecluse/bin/ecluse$" compare "$work/shipped.tar" "$work/rebuilt.tar"

pack "$work/later.tar" "$work/shipped" 1
check "fails on archives that differ only in a member's timestamp" 1 \
  "^> .*/layer\.tar$" compare "$work/shipped.tar" "$work/later.tar"

gzip -dc "$work/shipped.tar" > "$work/plain.tar"
check "fails on the same members under different compression" 1 \
  "^None: " compare "$work/shipped.tar" "$work/plain.tar"

check "refuses a missing archive" 2 "cannot read" compare "$work/shipped.tar" "$work/missing.tar"

exit "$fail"
