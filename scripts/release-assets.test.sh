#!/usr/bin/env bash
# Deterministic unit test for release-assets.sh. The asset names are a published
# interface: the release body, the README, and the runbook all quote them, and a
# consumer's `--bundle` command names one. The refusals matter as much, because a
# release that advertises an attestation it does not carry is worse than no asset at
# all. Run via `task test-scripts`.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
script="$here/release-assets.sh"

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

# A version no release will ever carry, so nothing here reads as coupled to the
# current one. Each source file's content names itself, which is what lets the
# per-asset comparison below tell two same-shaped bundles apart.
version=9.9.9
src="$work/src"
mkdir -p "$src"
for f in prov-index prov-amd64 prov-arm64 sbom-amd64 sbom-arm64 spdx-amd64 spdx-arm64; do
  printf '{"file":"%s"}\n' "$f" >"$src/$f.json"
done

complete=(
  "PROVENANCE_INDEX=$src/prov-index.json"
  "PROVENANCE_AMD64=$src/prov-amd64.json"
  "PROVENANCE_ARM64=$src/prov-arm64.json"
  "SBOM_BUNDLE_AMD64=$src/sbom-amd64.json"
  "SBOM_BUNDLE_ARM64=$src/sbom-arm64.json"
  "SBOM_AMD64=$src/spdx-amd64.json"
  "SBOM_ARM64=$src/spdx-arm64.json"
)

# Asset basename -> the source file it must carry.
declare -A want=(
  ["ecluse-$version-provenance.sigstore.json"]=prov-index
  ["ecluse-$version-amd64-provenance.sigstore.json"]=prov-amd64
  ["ecluse-$version-arm64-provenance.sigstore.json"]=prov-arm64
  ["ecluse-$version-amd64-sbom.sigstore.json"]=sbom-amd64
  ["ecluse-$version-arm64-sbom.sigstore.json"]=sbom-arm64
  ["ecluse-$version-amd64-sbom.spdx.json"]=spdx-amd64
  ["ecluse-$version-arm64-sbom.spdx.json"]=spdx-arm64
)
expected_names="$(printf '%s\n' "${!want[@]}" | LC_ALL=C sort)"

fail=0

report() {
  if [ "$1" = 0 ]; then
    printf 'ok   - %s\n' "$2"
  else
    printf 'FAIL - %s\n' "$2"
    fail=1
  fi
}

rc=0
env "${complete[@]}" bash "$script" "v$version" "$work/tagged" >"$work/tagged.txt" || rc=1
[ "$(LC_ALL=C ls -1 "$work/tagged" | sort)" = "$expected_names" ] || rc=1
report "$rc" "stages all seven assets under their published names"

rc=0
[ "$(wc -l <"$work/tagged.txt")" = 7 ] || rc=1
[ "$(head -1 "$work/tagged.txt")" = "$work/tagged/ecluse-$version-provenance.sigstore.json" ] || rc=1
report "$rc" "prints one staged path per line, for gh release create"

# Names alone cannot catch a swapped pair: exchanging the two platforms' bundles keeps
# every name intact and ships the arm64 attestation as the amd64 one.
rc=0
for name in "${!want[@]}"; do
  cmp -s "$src/${want[$name]}.json" "$work/tagged/$name" || rc=1
done
report "$rc" "gives each asset the source its name claims"

# release.yml passes the stripped tag on a rehearsal dispatch and the `v`-prefixed ref
# on a real tag. Both must name the assets the same way.
rc=0
env "${complete[@]}" bash "$script" "$version" "$work/bare" >/dev/null || rc=1
[ "$(LC_ALL=C ls -1 "$work/bare" | sort)" = "$expected_names" ] || rc=1
report "$rc" "names assets the same with or without the tag's v prefix"

# The release body advertises these names. A rename here that misses release-notes.sh
# publishes a body listing assets the release does not carry. Stub `gh` so the notes
# render offline: the changelog call is guarded and degrades to empty.
mkdir -p "$work/stub"
printf '#!/usr/bin/env bash\nexit 1\n' >"$work/stub/gh"
chmod +x "$work/stub/gh"
rc=0
notes="$(PATH="$work/stub:$PATH" GITHUB_REPOSITORY=owner/repo \
  bash "$here/release-notes.sh" "v$version" ghcr.io/owner/img sha256:0 2>/dev/null)" || rc=1
for name in "${!want[@]}"; do
  case "$notes" in *"$name"*) ;; *) rc=1 ;; esac
done
report "$rc" "the release body names every asset that gets staged"

rc=0
if env "${complete[@]:1}" bash "$script" "v$version" "$work/incomplete" >/dev/null 2>&1; then
  rc=1
fi
[ ! -d "$work/incomplete" ] || rc=1
report "$rc" "refuses an unset bundle path and stages nothing"

: >"$src/empty.json"
rc=0
if env "${complete[@]}" "PROVENANCE_ARM64=$src/empty.json" \
  bash "$script" "v$version" "$work/empty" >/dev/null 2>&1; then
  rc=1
fi
[ ! -d "$work/empty" ] || rc=1
report "$rc" "refuses an empty bundle and stages nothing"

exit "$fail"
