#!/usr/bin/env bash
# Check the public Pages entry points for the metadata search engines and link
# previews use. The sitemap is the authoritative page list: every URL it names
# must exist in the assembled site and carry a title, a description, and the
# matching canonical and Open Graph URLs. This runs against the assembled site,
# not the source templates.
set -euo pipefail

site="${1:-_site}"
domain="https://ecluse-proxy.com/"

fail() {
  printf 'site metadata: %s\n' "$1" >&2
  exit 1
}

[[ -f "$site/robots.txt" ]] || fail "missing robots.txt"
[[ -f "$site/sitemap.xml" ]] || fail "missing sitemap.xml"
grep -q "Sitemap: ${domain}sitemap.xml" "$site/robots.txt" \
  || fail "robots.txt does not name the sitemap"

mapfile -t urls < <(grep -o '<loc>[^<]*</loc>' "$site/sitemap.xml" | sed -e 's|<loc>||' -e 's|</loc>||')
[[ ${#urls[@]} -gt 0 ]] || fail "sitemap.xml lists no URLs"

for url in "${urls[@]}"; do
  path="${url#"$domain"}"
  [[ "$url" != "$path" ]] || fail "$url is not under the canonical domain"
  file="$site/$path"
  if [[ -z "$path" || "$path" == */ ]]; then
    file="${file}index.html"
  fi
  [[ -f "$file" ]] || fail "$url names no file in the assembled site (expected $file)"
  grep -q '<title>[^<][^<]*</title>' "$file" || fail "$url has no title"
  grep -q '<meta name="description" content="[^\"]' "$file" || fail "$url has no description"
  grep -q "<link rel=\"canonical\" href=\"$url\"" "$file" || fail "$url canonical link does not match"
  grep -q "<meta property=\"og:url\" content=\"$url\"" "$file" || fail "$url Open Graph URL does not match"
done

if grep -q 'alexadewit.github.io/Ecluse' "$site"/*.html "$site/api/index.html" "$site/api/openapi/index.html"; then
  fail "obsolete GitHub Pages URL remains"
fi

printf 'site metadata: all checks passed\n'
