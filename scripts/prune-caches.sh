#!/usr/bin/env bash
# Select GitHub Actions cache ids to delete — for .github/workflows/cache-cleanup.yml.
#
# Reads cache rows on stdin, one TSV row per cache:
#
#     id<TAB>ref<TAB>key<TAB>created_at
#
# (as produced by `gh api .../actions/caches --jq '... | @tsv'`), and writes the ids to
# delete to stdout, one per line. The workflow feeds those ids to `gh api -X DELETE`.
#
# Retention policy (matches the workflow's documented intent):
#
#   * ref != refs/heads/main          -> delete. PRs restore caches but never save (see
#                                        the setup-toolchain action), so any non-main
#                                        entry is a straggler — e.g. left by a deleted
#                                        branch.
#   * on main, beyond the newest N     -> delete. Once a dependency epoch advances
#     per key prefix                     (flake.lock / cabal.project[.freeze] change),
#                                        the previous epoch's immutable-keyed entries are
#                                        dead weight.
#
# Prefix = the key with a trailing `-<16+ hex>` (a hashFiles digest) stripped, so every
# epoch of one logical cache groups together. N defaults to 2 (current + one fallback for
# in-flight runs / quick rollback); override via KEEP_PER_PREFIX.
#
# Bash + awk/sort (no interval regexes) so it runs on the plain runner without the Nix
# shell. Try it against a sample:
#
#   printf 'id\tref\tkey\tcreated\n' | KEEP_PER_PREFIX=2 scripts/prune-caches.sh
set -euo pipefail

keep="${KEEP_PER_PREFIX:-2}"
rows="$(cat)"

# Off-main rows are stragglers — delete them all (emitted in input order).
printf '%s\n' "$rows" | awk -F'\t' '$1 != "" && $2 != "refs/heads/main" { print $1 }'

# On-main rows: keep the newest $keep per key prefix, delete the rest. Emit
# "prefix<TAB>created<TAB>id", sort by prefix then created (id breaks created ties)
# descending, then drop everything past the newest $keep of each prefix.
printf '%s\n' "$rows" | awk -F'\t' '
  $1 == "" || $2 != "refs/heads/main" { next }
  {
    key = $3
    if (key ~ /-[0-9a-f]+$/) {                       # ends with -<hex...>?
      seg = key; sub(/.*-/, "", seg)                 # the final dash-delimited segment
      if (length(seg) >= 16) sub(/-[0-9a-f]+$/, "", key)   # a digest: strip it
    }
    print key "\t" $4 "\t" $1
  }' \
  | LC_ALL=C sort -t$'\t' -k1,1 -k2,2r -k3,3r \
  | awk -F'\t' -v keep="$keep" '
      $1 != prev { prev = $1; n = 0 }
      { if (++n > keep) print $3 }'
