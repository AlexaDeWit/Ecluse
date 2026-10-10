#!/usr/bin/env bash
# Checks on a release image's docker-archive, for release-build.yml and ci.yml.
#   executables <archive>     Fail unless bin/ecluse, in one store path, is the only
#                             Écluse program. Other packages' store paths are not read.
#   compare <first> <second>  Fail unless the two archives are the same bytes, and name
#                             the archive members and the files that differ.
# Exit 1 on a failed check, 2 on a usage error or an unreadable archive. Needs GNU tar.
set -euo pipefail

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
# The loop's own status is its last tar's, so each tar failure ends it at once.
members() {
  local layer
  layers "$1" | while IFS= read -r layer; do
    tar -tPf "$1/$layer/layer.tar" || exit 2
  done | sed -E 's#^\.?/##' | { grep -v '/$' || true; } | LC_ALL=C sort -u
}

# A program is a file or link under a bin, sbin, or libexec directory. The check reads
# the image's root and each ecluse-<version> store path, and no other package's.
executables() {
  local image="$work/image" verdict=0 shipped=0 path package inside
  local store_path='^nix/store/[a-z0-9]{32}-([^/]+)/(.+)$'
  unpack "$1" "$image"
  # Through files, never a process substitution: bash discards that one's status, and a
  # layer tar cannot list must stop the check.
  if ! members "$image" > "$work/members"; then
    echo "image-archive: cannot list a layer of $1" >&2
    exit 2
  fi
  grep -E '(^|/)(bin|sbin|libexec)/' "$work/members" > "$work/programs" || [ "$?" = 1 ] || exit 2
  while IFS= read -r path; do
    if [ "$path" = "bin/ecluse" ]; then
      echo "ok      /$path"
    elif [[ ! "$path" =~ $store_path ]]; then
      echo "FAILED  /$path: a program outside the store that is not /bin/ecluse"
      verdict=1
    else
      package="${BASH_REMATCH[1]}"
      inside="${BASH_REMATCH[2]}"
      if [[ ! "$package" =~ ^ecluse-[0-9][0-9.]*$ ]]; then
        continue
      elif [ "$inside" = "bin/ecluse" ]; then
        echo "ok      /$path"
        shipped=$((shipped + 1))
      else
        echo "FAILED  /$path: the image ships bin/ecluse and no other Écluse program"
        verdict=1
      fi
    fi
  done < "$work/programs"
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
      --to-command='printf "%s  %s  %s\n" "$(sha256sum | cut -d" " -f1)" "$TAR_SIZE" "$TAR_FILENAME"' ||
      exit 2
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
