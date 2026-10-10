#!/usr/bin/env bash
# Checks on a release image's docker-archive, for release-build.yml and ci.yml.
#
#   executables <archive>     Fail unless bin/ecluse is the image's only program, apart
#                             from those the linked libraries below carry. A program is
#                             a file or link under a bin, sbin, or libexec directory.
#   compare <first> <second>  Fail unless the two archives are the same bytes, and name
#                             the archive members and the files that differ.
#
# Exit 1 on a failed check, 2 on a usage error or an unreadable archive. Needs GNU tar.
set -euo pipefail

# Libraries the binary links whose store paths carry programs of their own.
# <package name>|<why the image holds it>
linked=(
  "glibc|the C library, which carries its getconf helpers"
  "numactl|libnuma for GHC's runtime, which carries the numactl tools"
  "zstd|libzstd for libdw on amd64, which carries pzstd"
)

usage() {
  echo "usage: image-archive.sh executables <archive> | compare <first> <second>" >&2
  exit 2
}

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

# Unpack an archive's top level (layer tars, config, manifest) into $2.
unpack() {
  mkdir -p "$2"
  if ! tar -xf "$1" -C "$2" || ! compgen -G "$2/*/layer.tar" > /dev/null; then
    echo "image-archive: $1 is not a docker-archive with layers" >&2
    exit 2
  fi
}

# The layers of an unpacked image. A layer's directory is the digest of its tar.
layers() {
  local layer
  for layer in "$1"/*/layer.tar; do
    layer="${layer%/layer.tar}"
    printf '%s\n' "${layer##*/}"
  done | LC_ALL=C sort
}

# Every non-directory member of an unpacked image's layers, without a leading ./ or /.
members() {
  local layer
  layers "$1" | while IFS= read -r layer; do
    tar -tPf "$1/$layer/layer.tar"
  done | sed -E 's#^\.?/##' | { grep -v '/$' || true; } | LC_ALL=C sort -u
}

linked_reason() {
  local entry
  for entry in "${linked[@]}"; do
    if [[ "$1" =~ ^"${entry%%|*}"-[0-9] ]]; then
      printf '%s\n' "${entry#*|}"
      return 0
    fi
  done
  return 1
}

executables() {
  local image="$work/image" verdict=0 shipped=0 path package inside reason
  local store_path='^nix/store/[a-z0-9]{32}-([^/]+)/(.+)$'
  unpack "$1" "$image"
  while IFS= read -r path; do
    if [ "$path" = "bin/ecluse" ]; then
      echo "ok      /$path"
    elif [[ ! "$path" =~ $store_path ]]; then
      echo "FAILED  /$path: a program outside the store that is not /bin/ecluse"
      verdict=1
    else
      package="${BASH_REMATCH[1]}"
      inside="${BASH_REMATCH[2]}"
      if [[ "$package" =~ ^ecluse-[0-9] ]] && [ "$inside" = "bin/ecluse" ]; then
        echo "ok      /$path"
        shipped=$((shipped + 1))
      elif [[ "$package" =~ ^ecluse-[0-9] ]]; then
        echo "FAILED  /$path: the image ships bin/ecluse and no other Écluse program"
        verdict=1
      elif reason="$(linked_reason "$package")"; then
        echo "ok      /$path ($reason)"
      else
        echo "FAILED  /$path: $package is not a library listed in image-archive.sh"
        verdict=1
      fi
    fi
  done < <(members "$image" | { grep -E '(^|/)(bin|sbin|libexec)/' || true; })
  if [ "$shipped" != 1 ]; then
    echo "FAILED  $shipped store paths hold bin/ecluse, and the image needs exactly one"
    verdict=1
  fi
  return "$verdict"
}

# "<sha256>  <bytes>  <path>" for each regular file in the layers named on stdin, from
# unpacked image $1. --to-command pipes each file and writes nothing to disk.
contents() {
  local layer
  while IFS= read -r layer; do
    tar -xPf "$1/$layer/layer.tar" \
      --to-command='printf "%s  %s  %s\n" "$(sha256sum | cut -d" " -f1)" "$TAR_SIZE" "$TAR_FILENAME"'
  done | LC_ALL=C sort -k 3
}

# Print the lines two files do not share. Status 1 when there are any.
differ() {
  local status=0
  diff "$1" "$2" || status=$?
  [ "$status" -le 1 ] || exit 2
  return "$status"
}

compare() {
  local archive side first second found=0
  for archive in "$1" "$2"; do
    if [ ! -r "$archive" ]; then
      echo "image-archive: cannot read $archive" >&2
      exit 2
    fi
  done
  first="$(sha256sum "$1" | cut -d' ' -f1)"
  second="$(sha256sum "$2" | cut -d' ' -f1)"
  echo "first   $first  $1"
  echo "second  $second  $2"
  if [ "$first" = "$second" ]; then
    echo "ok      the two builds gave the same archive"
    return 0
  fi
  echo "FAILED  the two builds gave different archives (first build <, second build >)"

  unpack "$1" "$work/first"
  unpack "$2" "$work/second"
  tar -tvf "$1" > "$work/first.members"
  tar -tvf "$2" > "$work/second.members"
  LC_ALL=C comm -23 <(layers "$work/first") <(layers "$work/second") > "$work/first.layers"
  LC_ALL=C comm -13 <(layers "$work/first") <(layers "$work/second") > "$work/second.layers"
  for side in first second; do
    contents "$work/$side" < "$work/$side.layers" > "$work/$side.contents"
  done

  echo "Archive members that differ:"
  differ "$work/first.members" "$work/second.members" || found=1
  echo "Files that differ in those layers (sha256, bytes, path):"
  differ "$work/first.contents" "$work/second.contents" || found=1
  if [ "$found" = 0 ]; then
    echo "None: the members and their files match, so the archives differ in their framing."
  fi
  return 1
}

case "${1:-}" in
  executables)
    [ "$#" = 2 ] || usage
    executables "$2"
    ;;
  compare)
    [ "$#" = 3 ] || usage
    compare "$2" "$3"
    ;;
  *) usage ;;
esac
