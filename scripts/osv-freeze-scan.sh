#!/usr/bin/env bash
#
# Scan the pinned Haskell closure for known advisories: query OSV.dev with the
# exact pin of every package in cabal.project.freeze, the committed projection
# of the Nix package set, so findings describe the closure the shipped image is
# built from (HSEC advisories are exported to OSV). Writes osv-freeze.json (the
# filtered findings) and osv-freeze.md (the Markdown report), and prints the
# report. Advisories listed in scripts/osv-freeze-ignore.txt are accepted risk
# and filtered out. Report-only, like grype: the exit code reflects scan
# execution, not findings; the daily security.yml run keeps a tracking issue in
# sync instead. See docs/architecture/release-supply-chain.md.
set -euo pipefail

freeze="cabal.project.freeze"
ignore_file="scripts/osv-freeze-ignore.txt"

batch=$(grep -oE 'any\.[A-Za-z0-9-]+ ==[0-9.]+' "$freeze" \
  | sed 's/any\.//; s/ ==/ /' \
  | jq -R 'split(" ") | {package: {name: .[0], ecosystem: "Hackage"}, version: .[1]}' \
  | jq -s '{queries: .}')

results=$(curl --fail --silent --show-error --max-time 120 \
  -X POST -d "$batch" https://api.osv.dev/v1/querybatch)

ignored=$({ grep -vE '^[[:space:]]*(#|$)' "$ignore_file" || true; } | jq -R . | jq -s .)

jq --argjson queries "$(jq '.queries' <<<"$batch")" --argjson ignored "$ignored" '
  [ .results
    | to_entries[]
    | select(.value.vulns != null)
    | { package: $queries[.key].package.name,
        version: $queries[.key].version,
        advisories: [ .value.vulns[].id
                      | select(. as $id | $ignored | index($id) | not) ] }
    | select((.advisories | length) > 0)
  ]' <<<"$results" >osv-freeze.json

{
  echo "## OSV/HSEC scan of the pinned Haskell closure"
  echo ""
  n=$(jq 'length' osv-freeze.json)
  if [ "$n" -eq 0 ]; then
    echo "No known advisories affect the pins in \`$freeze\`."
  else
    echo "| Package | Pinned | Advisories |"
    echo "|---|---|---|"
    jq -r '.[] | "| \(.package) | \(.version) | \(.advisories
      | map("[\(.)](https://osv.dev/vulnerability/\(.))") | join(", ")) |"' \
      osv-freeze.json
  fi
  if [ "$(jq 'length' <<<"$ignored")" -gt 0 ]; then
    echo ""
    echo "_Accepted advisories (filtered per \`$ignore_file\`): $(jq -r 'join(", ")' <<<"$ignored")._"
  fi
} | tee osv-freeze.md
