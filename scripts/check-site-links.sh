#!/usr/bin/env bash
# Check the assembled site's link graph for correctness, against five failure modes:
#   1. a hand-authored page carries a relative href/src that resolves to nothing,
#      or an internal anchor its target page does not define;
#   2. a page the sitemap advertises is unreachable by following internal links
#      from the home page (an orphan — readers and crawlers both arrive there);
#   3. a top-level page exists but the sitemap does not advertise it;
#   4. a GitHub blob link (the web/links.lua fallback) names a repo path that no
#      longer exists in this checkout;
#   5. a GitHub blob link into a Markdown file carries an anchor no heading in
#      that file generates.
# The Haddock trees under api/ecluse and api/ecluse-core are machine-generated
# and already gated by the docs-site leaky-link sweep, so their interiors are
# exempt from resolution here (their hub links still count for reachability).
set -euo pipefail

site="${1:-_site}"
repo="${2:-$PWD}"
domain="https://ecluse-proxy.com/"
blob="https://github.com/AlexaDeWit/Ecluse/blob/main/"

fail() {
  printf 'site links: %s\n' "$1" >&2
  exit 1
}

[[ -d "$site" ]] || fail "missing site directory $site"
site_abs="$(realpath "$site")"

# The hand-authored entry points: every top-level page plus the two API hubs.
pages=("$site_abs"/*.html "$site_abs/api/index.html" "$site_abs/api/openapi/index.html")

hrefs_of() { # file -> href/src attribute values, one per line
  grep -o -E '(href|src)="[^"]*"' "$1" | sed -e 's/^[a-z]*="//' -e 's/"$//'
}

is_external() { # true for targets that are not site-relative paths
  case "$1" in
    http://* | https://* | mailto:* | data:* | '#'* | /*) return 0 ;;
    *) return 1 ;;
  esac
}

exempt() { # true for machine-generated interiors gated elsewhere
  case "$1" in
    "$site_abs"/api/ecluse/* | "$site_abs"/api/ecluse-core/* | "$site_abs"/api/ecluse-runtime/*) return 0 ;;
    *) return 1 ;;
  esac
}

resolve() { # containing-dir target -> normalised absolute path (dirs -> index.html)
  local abs
  abs="$(realpath -m "$1/$2")"
  if [[ -d "$abs" || "$2" == */ ]]; then
    abs="$abs/index.html"
  fi
  printf '%s\n' "$abs"
}

has_id() { # file fragment -> success if the page defines the anchor
  grep -q -E "(id|name)=\"$2\"" "$1"
}

# GitHub's heading slugger, closely enough for this repository's headings:
# strip Markdown emphasis/code markers, lowercase, drop everything that is not
# a word character, space, or hyphen, then turn spaces into hyphens (without
# squeezing runs — "a & b" slugs to "a--b").
md_slugs() { # markdown-file -> one heading slug per line
  grep -E '^#{1,6} ' "$1" \
    | sed -E -e 's/^#{1,6} +//' -e 's/[`*_]//g' -e 's/\[([^]]*)\]\([^)]*\)/\1/g' \
    | tr '[:upper:]' '[:lower:]' \
    | sed -E -e 's/[^a-z0-9 -]//g' -e 's/ /-/g'
}

md_anchor_ok() { # markdown-file fragment -> success if a heading generates it
  local frag="$2" base
  base="$(printf '%s\n' "$frag" | sed -E 's/-[0-9]+$//')" # duplicate headings get -N
  md_slugs "$1" | grep -q -x -F -e "$frag" -e "$base"
}

# --- 1. every link on a hand-authored page resolves (file and anchor) --------
for page in "${pages[@]}"; do
  [[ -f "$page" ]] || fail "expected page is missing: ${page#"$site_abs"/}"
  dir="$(dirname "$page")"
  while IFS= read -r target; do
    frag=""
    if [[ "$target" == *'#'* ]]; then frag="${target#*#}"; fi

    # GitHub blob links are minted by web/links.lua from in-repo paths: the
    # path must exist in this checkout, and a Markdown anchor must match a
    # heading, or the link 404s (or lands nowhere) the moment it is clicked.
    if [[ "$target" == "$blob"* ]]; then
      rel="${target#"$blob"}"
      rel="${rel%%#*}"
      rel="${rel%/}"
      [[ -e "$repo/$rel" ]] || fail "${page#"$site_abs"/} links a missing repo path: $rel"
      if [[ -n "$frag" && "$rel" == *.md && "$frag" != L* ]]; then
        md_anchor_ok "$repo/$rel" "$frag" \
          || fail "${page#"$site_abs"/} links $rel#$frag but no heading generates that anchor"
      fi
      continue
    fi

    if is_external "$target"; then continue; fi
    target="${target%%#*}"
    if [[ -z "$target" ]]; then continue; fi
    abs="$(resolve "$dir" "$target")"
    if exempt "$abs"; then continue; fi
    [[ "$abs" == "$site_abs"/* ]] || fail "${page#"$site_abs"/} links outside the site: $target"
    [[ -f "$abs" ]] || fail "${page#"$site_abs"/} has a dangling link: $target"
    if [[ -n "$frag" && "$abs" == *.html ]]; then
      has_id "$abs" "$frag" \
        || fail "${page#"$site_abs"/} links ${target}#${frag} but the page defines no such anchor"
    fi
  done < <(hrefs_of "$page")
done

# --- 2. every sitemap page is reachable from the home page -------------------
declare -A seen
queue=("$site_abs/index.html")
seen["$site_abs/index.html"]=1
while [[ ${#queue[@]} -gt 0 ]]; do
  current="${queue[0]}"
  queue=("${queue[@]:1}")
  dir="$(dirname "$current")"
  while IFS= read -r target; do
    if is_external "$target"; then continue; fi
    target="${target%%#*}"
    if [[ -z "$target" ]]; then continue; fi
    abs="$(resolve "$dir" "$target")"
    if [[ -n "${seen[$abs]:-}" || ! -f "$abs" ]]; then continue; fi
    seen["$abs"]=1
    # Walk on through hand-authored HTML only; the Haddock interiors are huge
    # and their hubs are the reachability targets that matter.
    if [[ "$abs" == *.html ]] && ! exempt "$abs"; then
      queue+=("$abs")
    fi
  done < <(hrefs_of "$current")
done

mapfile -t urls < <(grep -o '<loc>[^<]*</loc>' "$site_abs/sitemap.xml" | sed -e 's|<loc>||' -e 's|</loc>||')
[[ ${#urls[@]} -gt 0 ]] || fail "sitemap.xml lists no URLs"
for url in "${urls[@]}"; do
  path="${url#"$domain"}"
  file="$site_abs/$path"
  if [[ -z "$path" || "$path" == */ ]]; then
    file="${file}index.html"
  fi
  [[ -n "${seen[$file]:-}" ]] || fail "$url is not reachable from the home page (orphaned)"
done

# --- 3. every top-level page is advertised in the sitemap --------------------
for page in "$site_abs"/*.html; do
  name="$(basename "$page")"
  url="$domain$name"
  if [[ "$name" == index.html ]]; then url="$domain"; fi
  grep -q "<loc>$url</loc>" "$site_abs/sitemap.xml" || fail "$name is absent from sitemap.xml"
done

printf 'site links: all checks passed\n'
