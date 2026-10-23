#!/usr/bin/env bash
# Deterministic unit test for image-archive.sh. Each fixture is a small docker-archive in
# the layout the Nix image build writes: store layers with absolute member names, a top
# layer of ./bin and ./etc links, and one directory per layer named for its digest. The
# cases that matter most are the refusals: a second Écluse program, a CA bundle that
# trusts a distrusted root, and two archives that differ. Run via `task test-scripts`.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
script="$here/image-archive.sh"

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

fail=0
hash=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
ecluse="nix/store/$hash-ecluse-9.9.9"
bundle="nix/store/$hash-tls-ca-bundle/etc/ssl/certs/ca-bundle.crt"

# A throwaway root for the bundles, and emSign ECC Root CA - G3, a root no bundle may hold.
openssl req -x509 -newkey ec -pkeyopt ec_paramgen_curve:P-256 -nodes -subj /CN=fixture \
  -days 1 -keyout "$work/fixture.key" -out "$work/fixture.pem" 2> /dev/null
cat > "$work/distrusted.pem" <<'EOF'
-----BEGIN CERTIFICATE-----
MIICTjCCAdOgAwIBAgIKPPYHqWhwDtqLhDAKBggqhkjOPQQDAzBrMQswCQYDVQQG
EwJJTjETMBEGA1UECxMKZW1TaWduIFBLSTElMCMGA1UEChMcZU11ZGhyYSBUZWNo
bm9sb2dpZXMgTGltaXRlZDEgMB4GA1UEAxMXZW1TaWduIEVDQyBSb290IENBIC0g
RzMwHhcNMTgwMjE4MTgzMDAwWhcNNDMwMjE4MTgzMDAwWjBrMQswCQYDVQQGEwJJ
TjETMBEGA1UECxMKZW1TaWduIFBLSTElMCMGA1UEChMcZU11ZGhyYSBUZWNobm9s
b2dpZXMgTGltaXRlZDEgMB4GA1UEAxMXZW1TaWduIEVDQyBSb290IENBIC0gRzMw
djAQBgcqhkjOPQIBBgUrgQQAIgNiAAQjpQy4LRL1KPOxst3iAhKAnjlfSU2fySU0
WXTsuwYc58Byr+iuL+FBVIcUqEqy6HyC5ltqtdyzdc6LBtCGI79G1Y4PPwT01xyS
fvalY8L1X44uT6EYGQIrMgqCZH0Wk9GjQjBAMB0GA1UdDgQWBBR8XQKEE9TMipuB
zhccLikenEhjQjAOBgNVHQ8BAf8EBAMCAQYwDwYDVR0TAQH/BAUwAwEB/zAKBggq
hkjOPQQDAwNpADBmAjEAvvNhzwIQHWSVB7gYboiFBS+DCBeQyh+KTOgNG3qxrdWB
CUfvO6wIBHxcmbHtRwfSAjEAnbpV/KlK6O3t5nYBQnvI+GDZjVGLVTv7jHvrZQnD
+JbNR6iC8hZVdyR+EhCVBCyj
-----END CERTIFICATE-----
EOF

# Write a file into a tree. $1 tree, $2 path, $3 content (the path when omitted).
put() {
  mkdir -p "$1/$(dirname "$2")"
  printf '%s' "${3:-$2}" > "$1/$2"
}

# The configuration that names $2 as SSL_CERT_FILE, for the image tree $1.
cert_file() {
  printf '{"config":{"Env":["SSL_CERT_FILE=/%s"]}}' "$2" > "$1/config.json"
}

# The image as it ships: one Écluse program, its /bin link, the linked libraries with the
# programs their store paths carry, and the CA bundle SSL_CERT_FILE names, linked from /etc.
shipped_image() {
  put "$1/store" "$ecluse/bin/ecluse"
  put "$1/store" "nix/store/$hash-glibc-2.42-67/lib/libc.so.6"
  put "$1/store" "nix/store/$hash-glibc-2.42-67/libexec/getconf/POSIX_V7_LP64_OFF64"
  put "$1/store" "nix/store/$hash-numactl-2.0.18/lib/libnuma.so.1"
  put "$1/store" "nix/store/$hash-numactl-2.0.18/bin/numactl"
  put "$1/store" "nix/store/$hash-zstd-1.5.7/bin/pzstd"
  mkdir -p "$(dirname "$1/store/$bundle")" "$1/root/bin" "$1/root/etc/ssl/certs"
  cp "$work/fixture.pem" "$1/store/$bundle"
  ln -s "/$ecluse/bin/ecluse" "$1/root/bin/ecluse"
  ln -s "/$bundle" "$1/root/etc/ssl/certs/ca-bundle.crt"
  cert_file "$1" "$bundle"
}

# Pack a tree into a gzip docker-archive. $1 archive, $2 tree, $3 member mtime (0 when omitted).
pack() {
  local stage="$work/stage" fixed=(--sort=name --owner=0 --group=0 --numeric-owner) layer
  rm -rf "$stage"
  mkdir -p "$stage/image"
  tar -C "$2/store" -cPf "$stage/store.tar" "${fixed[@]}" --mtime="@${3:-0}" \
    --transform 's#^nix#/nix#' nix
  tar -C "$2/root" -cf "$stage/root.tar" "${fixed[@]}" --mtime="@${3:-0}" ./bin ./etc
  for layer in store root; do
    mkdir "$stage/image/$(sha256sum "$stage/$layer.tar" | cut -d' ' -f1)"
    mv "$stage/$layer.tar" "$stage/image/$(sha256sum "$stage/$layer.tar" | cut -d' ' -f1)/layer.tar"
  done
  cp "$2/config.json" "$stage/image/config.json"
  printf '[{"Config":"config.json"}]' > "$stage/image/manifest.json"
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

# A library package's store path is not read, whatever programs Nix puts in it.
shipped_image "$work/library"
put "$work/library/store" "nix/store/$hash-numactl-2.0.18/bin/numad"
pack "$work/library.tar" "$work/library"
check "passes a program that a library package carries in its own store path" 0 \
  "^ok +/$ecluse/bin/ecluse$" executables "$work/library.tar"

shipped_image "$work/sbin"
put "$work/sbin/store" "$ecluse/sbin/ecluse-admin"
pack "$work/sbin.tar" "$work/sbin"
check "fails on an Écluse program under sbin" 1 \
  "^FAILED +/$ecluse/sbin/ecluse-admin:" executables "$work/sbin.tar"

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

shipped_image "$work/twice"
put "$work/twice/store" "nix/store/${hash//a/b}-ecluse-9.9.9/bin/ecluse"
pack "$work/twice.tar" "$work/twice"
check "fails when two store paths hold bin/ecluse" 1 \
  "^FAILED +2 store paths hold bin/ecluse" executables "$work/twice.tar"

check "passes a CA bundle of plain certificates that SSL_CERT_FILE names" 0 \
  "^ok +SSL_CERT_FILE names /$bundle$" trust-store "$work/shipped.tar"

shipped_image "$work/rooted"
cat "$work/fixture.pem" "$work/distrusted.pem" > "$work/rooted/store/$bundle"
pack "$work/rooted.tar" "$work/rooted"
check "fails on a bundle that holds a distrusted root" 1 \
  "^FAILED +/$bundle: holds the distrusted root emSign ECC Root CA - G3$" \
  trust-store "$work/rooted.tar"

# OpenSSL would refuse this root for a server, and the TLS stack still trusts it.
shipped_image "$work/rejected"
openssl x509 -in "$work/distrusted.pem" -addreject serverAuth -trustout \
  >> "$work/rejected/store/$bundle"
pack "$work/rejected.tar" "$work/rejected"
check "fails on a distrusted root that trust rules reject" 1 \
  "^FAILED +/$bundle: holds the distrusted root emSign ECC Root CA - G3$" \
  trust-store "$work/rejected.tar"

shipped_image "$work/ruled"
openssl x509 -in "$work/fixture.pem" -addtrust serverAuth -trustout > "$work/ruled/store/$bundle"
pack "$work/ruled.tar" "$work/ruled"
check "fails on a bundle that carries trust rules" 1 \
  "^FAILED +/$bundle: 1 blocks carry OpenSSL trust rules" trust-store "$work/ruled.tar"

# The bundle the default paths read when SSL_CERT_FILE is unset counts as much.
shipped_image "$work/second"
put "$work/second/store" "nix/store/$hash-nss-cacert-3.126/etc/ssl/certs/ca-bundle.crt" \
  "$(cat "$work/distrusted.pem")"
pack "$work/second.tar" "$work/second"
check "fails on a second bundle that holds a distrusted root" 1 \
  "^FAILED +/nix/store/$hash-nss-cacert-3.126/etc/ssl/certs/ca-bundle.crt: holds the distrusted" \
  trust-store "$work/second.tar"

shipped_image "$work/text"
put "$work/text/store" "$bundle" "no certificates here"
pack "$work/text.tar" "$work/text"
check "fails on a bundle that holds no certificate" 1 \
  "^FAILED +/$bundle: holds no certificate$" trust-store "$work/text.tar"

shipped_image "$work/unset"
printf '{"config":{}}' > "$work/unset/config.json"
pack "$work/unset.tar" "$work/unset"
check "fails when the configuration sets no SSL_CERT_FILE" 1 \
  "^FAILED +the image configuration sets no SSL_CERT_FILE$" trust-store "$work/unset.tar"

shipped_image "$work/elsewhere"
cert_file "$work/elsewhere" "etc/ssl/certs/ca-bundle.crt"
pack "$work/elsewhere.tar" "$work/elsewhere"
check "fails when SSL_CERT_FILE names no regular file in the image" 1 \
  "^FAILED +SSL_CERT_FILE names /etc/ssl/certs/ca-bundle.crt, which is no CA bundle" \
  trust-store "$work/elsewhere.tar"

# The unreadable layer sorts first, so the layers after it list without error.
mkdir -p "$work/torn/0000"
tar -xzf "$work/shipped.tar" -C "$work/torn"
printf 'not a tar' > "$work/torn/0000/layer.tar"
(cd "$work/torn" && tar -czf "$work/torn.tar" -- *)
check "refuses an archive with a layer it cannot list" 2 \
  "cannot list a layer" executables "$work/torn.tar"

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
