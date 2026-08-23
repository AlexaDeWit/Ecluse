#!/usr/bin/env bash
# Select GitHub Actions cache ids to delete, for .github/workflows/cache-cleanup.yml.
# Reads one TSV row per cache on stdin, as `gh api .../actions/caches` produces it:
#
#     id<TAB>ref<TAB>key<TAB>created_at<TAB>size_in_bytes
#
# Writes the ids on stdout, most urgent first, and a reasoned line per id on stderr.
# awk and sort only, so it runs on the plain runner. Try it against a sample:
#   printf 'id\tref\tkey\tcreated\t123\n' | KEEP_PER_PREFIX=2 scripts/prune-caches.sh
set -euo pipefail

# The cache key prefixes this repo's workflows write. A new cache anywhere in .github
# must be added here, or this sweep reaps its entries and names them in the log.
allowed_prefixes='nix-|cabal-store-|dist-|determinatesystem-nix-installer-'

keep="${KEEP_PER_PREFIX:-2}"
keep_docs="${KEEP_DOCS:-1}"
rows="$(cat)"

# Each arm emits "id<TAB>key<TAB>size<TAB>reason", most urgent first: off-main
# stragglers, then superseded epochs, then keys no workflow writes.
selected="$(
  printf '%s\n' "$rows" | awk -F'\t' '
    $1 != "" && $2 != "refs/heads/main" { print $1 "\t" $3 "\t" $5 "\toff-main straggler" }'

  printf '%s\n' "$rows" | awk -F'\t' '
      $1 == "" || $2 != "refs/heads/main" { next }
      {
        key = $3
        # Group every epoch of one logical cache: strip a trailing hashFiles digest.
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
