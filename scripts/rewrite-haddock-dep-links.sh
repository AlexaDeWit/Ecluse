#!/usr/bin/env bash
#
# Rewrite Haddock dependency cross-reference links to canonical Hackage URLs.
#
# `cabal haddock --haddock-hyperlink-source` resolves identifiers from other
# packages against the LOCAL build tree, so the published site ends up with links
# no reader can follow -- and one shape that ships a build-host path into public
# HTML:
#
#   * GHC boot libraries (base, time, containers, ...) link through pkgroot:
#       ${pkgroot}/../../../../<nixhash>-ghc-<ver>-doc/share/doc/ghc/html/libraries/<pkg>-<ver>-<hash>/<page>
#     -- unresolvable off the build host, and it embeds the Nix store hash.
#   * Other dependencies link through an absolute cabal-store path:
#       file://<...>/store/ghc-<ver>/<pkg>-<ver>-<hash>/share/doc/html/<page>
#     -- unresolvable, and on CI it embeds the runner's home path
#     (/home/runner/...) in the published HTML.
#
# Both are dead links; the second is also an information-hygiene lapse. This
# rewrites each to the canonical Hackage page for the same identifier
#   https://hackage.haskell.org/package/<pkg>-<ver>/docs/<page>
# which resolves and carries no local path. The store/unit-id <hash> suffix on
# the package directory (a 64-char cabal fingerprint, or GHC's short boot-lib
# abbreviation) is dropped, since it is not part of the Hackage package id.
#
# Output-only: it edits the generated HTML and the JSON search index, never the
# published module set or the /api layout. See the `docs-site` Makefile target.
#
# Usage:
#   rewrite-haddock-dep-links.sh <dir>      rewrite *.html and *.json under <dir> in place
#   rewrite-haddock-dep-links.sh --filter   rewrite a single stream, stdin -> stdout (tests)
set -euo pipefail

# A dependency package directory is "<name>-<version>[-<hash>]". <name> may itself
# contain hyphens (e.g. amazonka-core), so the version is pinned as the first
# trailing "-<digits-and-dots>" group and the optional store/unit-id <hash> after
# it is consumed but not re-emitted.

# GHC boot libraries: <prefix>/share/doc/ghc/html/libraries/<name>-<ver>[-<hash>]/<page>.
# The whole <prefix> (pkgroot/.. or a store path) and its Nix hash are dropped.
boot='s#[^"]*/share/doc/ghc/html/libraries/([^/"]+-[0-9][0-9.]*)(-[0-9a-f]+)?/([^"]+)#https://hackage.haskell.org/package/\1/docs/\3#g'

# cabal-store dependencies: <prefix>/store/<ghc>/<name>-<ver>-<hash>/share/doc/html/<page>.
store='s#[^"]*/store/[^/"]+/([^/"]+-[0-9][0-9.]*)(-[0-9a-f]+)?/share/doc/html/([^"]+)#https://hackage.haskell.org/package/\1/docs/\3#g'

usage() {
  echo "usage: $0 <dir> | --filter" >&2
  exit 2
}

[ "$#" -eq 1 ] || usage

case "$1" in
  --filter)
    exec sed -E -e "$boot" -e "$store"
    ;;
  -*)
    usage
    ;;
  *)
    dir="$1"
    [ -d "$dir" ] || {
      echo "$0: not a directory: $dir" >&2
      exit 1
    }
    # Rewrite every generated page and the JSON search index in place. Other file
    # types (CSS, JS bundles, images) never carry these links.
    find "$dir" -type f \( -name '*.html' -o -name '*.json' \) -print0 |
      xargs -0 -r sed -E -i -e "$boot" -e "$store"
    ;;
esac
