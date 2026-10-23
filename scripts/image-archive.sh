#!/usr/bin/env bash
# Checks on a release image's docker-archive, for release-build.yml and ci.yml.
#   executables <archive>     Fail unless bin/ecluse, in one store path, is the only
#                             Écluse program. Other packages' store paths are not read.
#   trust-store <archive>     Fail unless SSL_CERT_FILE names a CA bundle in the image, and
#                             every bundle holds plain certificates and no distrusted root.
#   compare <first> <second>  Fail unless the two archives are the same bytes, and name
#                             the archive members and the files that differ.
#   configuration <archive>   Print the digest of the image configuration.
# Exit 1 on a failed check, 2 on a usage error or an unreadable archive. Needs GNU tar, and
# jq and openssl for trust-store.
set -euo pipefail

usage() {
  echo "usage: image-archive.sh executables <archive> | trust-store <archive> | compare <first> <second> | configuration <archive>" >&2
  exit 2
}

# SHA-256 fingerprints of the roots no image bundle may hold: the roots that eMudhra's
# compromised intermediate CAs chain to, and the other two emSign roots Mozilla removed.
declare -A distrusted=(
  [D7A7A0FB5D7E2731D771E9484EBCDEF71D5F0C3E0A2948782BC83EE0EA699EF4]="AAA Certificate Services"
  [86A1ECBA089C4A8D3BBE2734C612BA341D813E043CF9E8A862CD5C57A36BBE6B]="emSign ECC Root CA - G3"
  [7DD78D5F4F13459A83DFF9ABBBA62EDBAF6F2D102BF257FD712F4D9F2746ED8D]="emSign Root TLS CA - G3"
  [125609AA301DA0A249B97A8239CB6A34216F44DCAC9F3954B14292F2E8C8608F]="emSign Root CA - C1"
  [BC4D809B15189D78DB3E1D8CF4F9726A795DA1643CA5F1358E1DDB0EDC0D7EB3]="emSign ECC Root CA - C3"
)

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

# Check one CA bundle $2, shown as /$1. The TLS stack takes every PEM block as a trust anchor
# and ignores OpenSSL trust rules, so a TRUSTED CERTIFICATE block counts as trusted.
bundle() {
  local line kind="" pem="" count=0 ruled=0 verdict=0 fingerprint root
  local -A reported=()
  while IFS= read -r line || [ -n "$line" ]; do
    # Drop trailing blanks and a CRLF's carriage return, which would end up in the block kind.
    line="${line%"${line##*[![:space:]]}"}"
    case "$line" in
      "-----BEGIN "*)
        kind="${line#-----BEGIN }"
        kind="${kind%-----}"
        pem="$line"
        ;;
      "-----END "*)
        pem+=$'\n'"$line"
        count=$((count + 1))
        [ "$kind" = CERTIFICATE ] || ruled=$((ruled + 1))
        if ! fingerprint="$(openssl x509 -noout -fingerprint -sha256 <<< "$pem" 2> /dev/null)"; then
          echo "FAILED  /$1: block $count, a $kind, is no readable certificate"
          verdict=1
        else
          fingerprint="${fingerprint#*=}"
          fingerprint="${fingerprint//:/}"
          root="${distrusted[$fingerprint]:-}"
          # A trust-rule bundle holds a root twice, as a plain and as a trusted block.
          if [ -n "$root" ] && [ -z "${reported[$root]:-}" ]; then
            reported[$root]=1
            echo "FAILED  /$1: holds the distrusted root $root"
            verdict=1
          fi
        fi
        kind=""
        ;;
      *) [ -z "$kind" ] || pem+=$'\n'"$line" ;;
    esac
  done < "$2"
  if [ "$ruled" != 0 ]; then
    echo "FAILED  /$1: $ruled blocks carry OpenSSL trust rules, which the TLS stack ignores"
    verdict=1
  fi
  if [ "$count" = 0 ]; then
    echo "FAILED  /$1: holds no certificate, and the check reads every regular file directly" \
      "in an etc/ssl/certs directory as a CA bundle"
    verdict=1
  elif [ "$verdict" = 0 ]; then
    echo "ok      /$1: $count certificates, none distrusted"
  fi
  return "$verdict"
}

# Every regular file directly in an etc/ssl/certs directory, never in a subdirectory, counts
# as a bundle. The TLS stack reads the one SSL_CERT_FILE names, or the files in /etc/ssl/certs
# when SSL_CERT_FILE is unset, and other TLS clients read hashed files such as 14bc7599.0. So
# a file there that holds no certificate fails too.
trust_store() {
  local image="$work/image" config cert_file layer name path verdict=0 named=0
  unpack "$1" "$image"
  config="$(jq -er 'if length == 1 then .[0].Config else error("expected one image") end' \
    "$image/manifest.json")" || exit 2
  cert_file="$(jq -er '[.config.Env // [] | .[] | select(startswith("SSL_CERT_FILE="))
    | ltrimstr("SSL_CERT_FILE=")] | last // ""' "$image/$config")" || exit 2
  cert_file="${cert_file#/}"
  layers "$image" > "$work/layers"
  while IFS= read -r layer; do
    tar -tPf "$image/$layer/layer.tar" > "$work/names" || exit 2
    grep -E '(^|/)etc/ssl/certs/[^/]+$' "$work/names" > "$work/bundles" || [ "$?" = 1 ] || exit 2
    while IFS= read -r name; do
      # A link extracts as nothing, and the file it points to is a member of its own.
      tar -xOPf "$image/$layer/layer.tar" "$name" > "$work/bundle" || exit 2
      [ -s "$work/bundle" ] || continue
      path="$(sed -E 's#^\.?/##' <<< "$name")"
      bundle "$path" "$work/bundle" || verdict=1
      [ "$path" != "$cert_file" ] || named=1
    done < "$work/bundles"
  done < "$work/layers"
  if [ -z "$cert_file" ]; then
    echo "FAILED  the image configuration sets no SSL_CERT_FILE"
    verdict=1
  elif [ "$named" = 0 ]; then
    echo "FAILED  SSL_CERT_FILE names /$cert_file, which is no CA bundle in the image"
    verdict=1
  else
    echo "ok      SSL_CERT_FILE names /$cert_file"
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

# The configuration digest remains stable across classic and containerd image stores.
configuration() {
  local config digest
  config="$(tar -xOf "$1" manifest.json | jq -er 'if length == 1 then .[0].Config else error("expected one image") end')"
  digest="$(tar -xOf "$1" "$config" | sha256sum | cut -d' ' -f1)"
  printf 'sha256:%s\n' "$digest"
}

case "${1:-}" in
  configuration)
    [ "$#" = 2 ] || usage
    configuration "$2"
    ;;
  executables)
    [ "$#" = 2 ] || usage
    executables "$2"
    ;;
  trust-store)
    [ "$#" = 2 ] || usage
    trust_store "$2"
    ;;
  compare)
    [ "$#" = 3 ] || usage
    compare "$2" "$3"
    ;;
  *) usage ;;
esac
