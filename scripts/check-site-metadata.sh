#!/usr/bin/env bash
# Check the public Pages entry points for the metadata search engines and link
# previews use. This runs against the assembled site, not the source templates.
set -euo pipefail

site="${1:-_site}"
pages=(index.html motivation.html alternatives.html usage.html ai-disclosure.html threat-model.html)

fail() {
  printf 'site metadata: %s\n' "$1" >&2
  exit 1
}

for page in "${pages[@]}"; do
  file="$site/$page"
  [[ -f "$file" ]] || fail "missing $page"
  grep -q '<title>[^<][^<]*</title>' "$file" || fail "$page has no title"
  grep -q '<meta name="description" content="[^\"]' "$file" || fail "$page has no description"
  grep -q '<link rel="canonical" href="https://ecluse-proxy.com/' "$file" || fail "$page has no canonical URL"
  grep -q '<meta property="og:url" content="https://ecluse-proxy.com/' "$file" || fail "$page has no Open Graph URL"
done

grep -R -q 'alexadewit.github.io/Ecluse' "${pages[@]/#/$site/}" && fail "obsolete GitHub Pages URL remains" || true
[[ -f "$site/robots.txt" ]] || fail "missing robots.txt"
[[ -f "$site/sitemap.xml" ]] || fail "missing sitemap.xml"
grep -q 'Sitemap: https://ecluse-proxy.com/sitemap.xml' "$site/robots.txt" || fail "robots.txt does not name the sitemap"

for page in "${pages[@]}"; do
  if [[ "$page" == index.html ]]; then
    url='https://ecluse-proxy.com/'
  else
    url="https://ecluse-proxy.com/$page"
  fi
  grep -q "<loc>$url</loc>" "$site/sitemap.xml" || fail "$url is absent from sitemap.xml"
done

printf 'site metadata: all checks passed\n'
