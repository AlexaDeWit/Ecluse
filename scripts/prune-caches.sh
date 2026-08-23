#!/usr/bin/env bash
# Select GitHub Actions cache ids to delete, for .github/workflows/cache-cleanup.yml.
#
# Reads one TSV row per cache on stdin (as produced by `gh api .../actions/caches`):
#
#     id<TAB>ref<TAB>key<TAB>created_at<TAB>size_in_bytes
#
# and writes the ids to delete on stdout, one per line, with a reasoned line per id on
# stderr. Three arms, emitted most urgent first so a capped sweep still does the work
# that matters: off-main stragglers, superseded epochs, then unwritten keys.
#
# Prefix = the key with a trailing `-<16+ hex>` (a hashFiles digest) stripped, so every
# epoch of one logical cache groups together. KEEP_PER_PREFIX (default 2) is the current
# epoch plus one fallback for in-flight runs. The heavy single-epoch caches keep only
# KEEP_DOCS (default 1): the `nix-*` store closures and the Pages `-docs-` doc variants
# are ~1 GB each, change only on a dependency bump, and have one writer each.
#
# Bash + awk/sort (no interval regexes) so it runs on the plain runner without the Nix
# shell. Try it against a sample:
#
#   printf 'id\tref\tkey\tcreated\t123\n' | KEEP_PER_PREFIX=2 scripts/prune-caches.sh
set -euo pipefail

# The cache key prefixes this repo's workflows write. A new cache anywhere in .github
# must be added here, or this sweep reaps its entries and names them in the log.
allowed_prefixes='nix-|cabal-store-|dist-|determinatesystem-nix-installer-'

keep="${KEEP_PER_PREFIX:-2}"
keep_docs="${KEEP_DOCS:-1}"
rows="$(cat)"

# Each arm emits "id<TAB>key<TAB>size<TAB>reason".
selected="$(
  printf '%s\n' "$rows" | awk -F'\t' '
    $1 != "" && $2 != "refs/heads/main" { print $1 "\t" $3 "\t" $5 "\toff-main straggler" }'

  printf '%s\n' "$rows" | awk -F'\t' '
      $1 == "" || $2 != "refs/heads/main" { next }
      {
        key = $3
        prefix = key
        if (prefix ~ /-[0-9a-f]+$/) {
          seg = prefix; sub(/.*-/, "", seg)
          if (length(seg) >= 16) sub(/-[0-9a-f]+$/, "", prefix)
        }
        print prefix "\t" $4 "\t" $1 "\t" key "\t" $5
      }' \
    | LC_ALL=C sort -t$'\t' -k1,1 -k2,2r -k3,3r \
    | awk -F'\t' -v keep="$keep" -v keep_docs="$keep_docs" '
        $1 != prev { prev = $1; n = 0 }
        {
          k = ($1 ~ /^nix-/ || $1 ~ /-docs-/) ? keep_docs : keep
          if (++n > k) print $3 "\t" $4 "\t" $5 "\tsuperseded epoch of " $1
        }'

  printf '%s\n' "$rows" | awk -F'\t' -v allowed="^($allowed_prefixes)" '
    $1 == "" || $2 != "refs/heads/main" { next }
    $3 !~ allowed { print $1 "\t" $3 "\t" $5 "\tno workflow writes this key" }'
)"

printf '%s\n' "$selected" | awk -F'\t' '
  $1 == "" || seen[$1]++ { next }
  { printf "prune: %8.1f MB  %s  (%s)\n", $3 / 1048576, $2, $4 > "/dev/stderr"
    print $1 }'
