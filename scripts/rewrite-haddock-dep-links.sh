#!/usr/bin/env bash
#
# Rewrite Haddock dependency cross-reference links to canonical Hackage URLs.
#
# `cabal haddock --haddock-hyperlink-source` resolves identifiers from other
# packages against the LOCAL build tree. The published site then carries links no
# reader can follow, and one shape ships a build-host path into public HTML:
#
#   * GHC boot libraries (base, time, containers, ...) link through pkgroot:
#       ${pkgroot}/../../../../<nixhash>-ghc-<ver>-doc/share/doc/ghc/html/libraries/<pkg>-<ver>-<hash>/<page>
#     This is unresolvable off the build host, and it embeds the Nix store hash.
#   * Other dependencies link through an absolute cabal-store path:
#       file://<...>/store/ghc-<ver>/<pkg>-<ver>-<hash>/share/doc/html/<page>
#     This is unresolvable, and on CI it embeds the runner's home path
#     (/home/runner/...) in the published HTML.
#
# Both are dead links. The second is also an information-hygiene lapse. This
# script rewrites each to the canonical Hackage page for the same identifier
#   https://hackage.haskell.org/package/<pkg>-<ver>/docs/<page>
# which resolves and carries no local path. It drops the store or unit-id <hash>
# suffix on the package directory: a 64-char cabal fingerprint, or GHC's short
# boot-lib abbreviation. That suffix is not part of the Hackage package id.
#
# Output-only: it edits the generated HTML and the JSON search index, never the
# published module set or the /api layout. See the `docs-site` Makefile target.
#
# Usage:
#   rewrite-haddock-dep-links.sh <dir>      rewrite *.html and *.json under <dir> in place
#   rewrite-haddock-dep-links.sh --filter   rewrite a single stream, stdin -> stdout (tests)
set -euo pipefail

# A dependency package directory is "<name>-<version>[-<hash>]". <name> may itself
# contain hyphens, such as amazonka-core. So the expression pins the version as
# the first trailing "-<digits-and-dots>" group, then consumes the store or
# unit-id <hash> after it without re-emitting it.
#
# The <hash> match must be MANDATORY, not optional. GNU sed is POSIX
# leftmost-longest, so it maximises the version group first. An all-digit
# abbreviated hash, which GHC emits (process-1.6.26.1-2190), would then fold INTO
# the version through "[0-9][0-9.]*" and ship a 404 Hackage link. A mandatory
# trailing "-<hash>/" forces that "-<digits>" into the hash group instead. A
# separate fallback expression then handles the legacy unhashed GHC layout. It
# runs AFTER the hashed one, so it only ever sees genuinely hash-less links. A
# rewritten URL no longer contains /libraries/ or /store/, so it cannot re-match.

# GHC boot libraries, hashed dir: <prefix>/share/doc/ghc/html/libraries/<name>-<ver>-<hash>/<page>.
# This drops the whole <prefix> (pkgroot/.. or a store path) and its Nix hash. It
# drops the version too, to produce canonical, stable Hackage URLs.
boot_h='s#[^"]*/share/doc/ghc/html/libraries/([^/"]+)-[0-9][0-9.]*-[0-9a-f]+/([^"]+)#https://hackage.haskell.org/package/\1/docs/\2#g'

# GHC boot libraries, legacy unhashed dir (older GHC layouts): no -<hash> segment.
boot_n='s#[^"]*/share/doc/ghc/html/libraries/([^/"]+)-[0-9][0-9.]*/([^"]+)#https://hackage.haskell.org/package/\1/docs/\2#g'

# cabal-store dependencies: <prefix>/store/<ghc>/<name>-<ver>-<hash>/share/doc/html/<page>.
# A store unit-id dir always carries its <hash>, so there is no unhashed fallback.
store_h='s#[^"]*/store/[^/"]+/([^/"]+)-[0-9][0-9.]*-[0-9a-f]+/share/doc/html/([^"]+)#https://hackage.haskell.org/package/\1/docs/\2#g'

# Generic Nix store documentation: /nix/store/<hash>-<name>-<ver>-doc/share/doc/html/<page>.
# Covers non-boot dependencies when built via Nix.
nix_h='s#[^"]*/nix/store/[0-9a-z]+-([^/"]+)-[0-9][0-9.]*-doc/share/doc/html/([^"]+)#https://hackage.haskell.org/package/\1/docs/\2#g'

usage() {
  echo "usage: $0 <dir> | --filter" >&2
  exit 2
}

[ "$#" -eq 1 ] || usage

case "$1" in
  --filter)
    exec sed -E -e "$boot_h" -e "$boot_n" -e "$store_h" -e "$nix_h"
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
      xargs -0 -r sed -E -i -e "$boot_h" -e "$boot_n" -e "$store_h" -e "$nix_h"
    ;;
esac
