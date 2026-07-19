#!/usr/bin/env bash
#
# Keep one tracking issue per scanner in sync with its latest scan: open or
# update the issue when findings are present, close it when clean. Run by the
# daily scheduled jobs in security.yml; needs `gh` + `issues: write`. See
# CONTRIBUTING.md → "Vulnerability scanning".
#
#   usage: vuln-issue.sh [grype|osv]
set -euo pipefail

scanner="${1:-grype}"
case "$scanner" in
  grype)
    label="security:vuln-scan"
    label_desc="Automated grype scan findings"
    n=$(python3 -c 'import json;print(len(json.load(open("grype.json")).get("matches",[])))' 2>/dev/null || echo 0)
    title="Vulnerability scan: $n finding(s) in the image closure"
    report() { bash scripts/vuln-report.sh grype.json; }
    remediation="Remediation is a \`flake.lock\` bump; Renovate opens these weekly. This issue is refreshed by the daily scan and closed automatically when the closure is clean."
    clean_msg="✅ Latest grype scan found no known CVEs in the image closure. Closing."
    ;;
  osv)
    label="security:hsec-scan"
    label_desc="Automated OSV/HSEC findings over cabal.project.freeze"
    n=$(jq 'length' osv-freeze.json 2>/dev/null || echo 0)
    title="HSEC scan: $n package(s) in the Haskell closure carry advisories"
    report() { cat osv-freeze.md; }
    remediation="Remediation is flake-side (a \`flake.lock\` refresh or an overlay pin in flake.nix) followed by \`task freeze\`, never a hand-edit of the generated freeze. Accepted advisories are recorded in \`scripts/osv-freeze-ignore.txt\`. This issue is refreshed by the daily scan and closed automatically when the closure is clean."
    clean_msg="✅ Latest OSV scan found no advisories against the pinned Haskell closure. Closing."
    ;;
  *)
    echo "usage: $0 [grype|osv]" >&2
    exit 2
    ;;
esac

# Idempotent: ensure the label exists before we filter/create on it.
gh label create "$label" --color B60205 --description "$label_desc" 2>/dev/null || true
existing=$(gh issue list --label "$label" --state open --json number --jq '.[0].number // empty')

if [ "$n" -gt 0 ]; then
  body=$(mktemp)
  {
    report
    echo ""
    echo "_Scan: ${GITHUB_SERVER_URL}/${GITHUB_REPOSITORY}/actions/runs/${GITHUB_RUN_ID} · $(date -u +%Y-%m-%dT%H:%MZ)._"
    echo "_${remediation}_"
  } >"$body"
  if [ -n "$existing" ]; then
    gh issue edit "$existing" --title "$title" --body-file "$body"
    echo "updated #$existing ($n findings)"
  else
    gh issue create --title "$title" --label "$label" --body-file "$body"
    echo "opened tracking issue ($n findings)"
  fi
else
  if [ -n "$existing" ]; then
    gh issue close "$existing" --comment "$clean_msg"
    echo "closed #$existing (clean)"
  else
    echo "clean; no open issue"
  fi
fi
