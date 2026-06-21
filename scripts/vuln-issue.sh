#!/usr/bin/env bash
#
# Keep a single tracking issue in sync with the latest grype scan: open or update
# it when CVEs are present, close it when the closure is clean. Run by the daily
# scheduled scan in security.yml; needs `gh` + `issues: write`. See CONTRIBUTING.md
# → "Vulnerability scanning".
set -euo pipefail

LABEL="security:vuln-scan"
n=$(python3 -c 'import json;print(len(json.load(open("grype.json")).get("matches",[])))' 2>/dev/null || echo 0)

# Idempotent: ensure the label exists before we filter/create on it.
gh label create "$LABEL" --color B60205 --description "Automated grype scan findings" 2>/dev/null || true
existing=$(gh issue list --label "$LABEL" --state open --json number --jq '.[0].number // empty')

if [ "$n" -gt 0 ]; then
  body=$(mktemp)
  {
    bash scripts/vuln-report.sh grype.json
    echo ""
    echo "_Scan: ${GITHUB_SERVER_URL}/${GITHUB_REPOSITORY}/actions/runs/${GITHUB_RUN_ID} · $(date -u +%Y-%m-%dT%H:%MZ)._"
    echo "_Remediation is a \`flake.lock\` bump — Dependabot opens these weekly. This issue is refreshed by the daily scan and closed automatically when the closure is clean._"
  } >"$body"
  title="Vulnerability scan: $n finding(s) in the image closure"
  if [ -n "$existing" ]; then
    gh issue edit "$existing" --title "$title" --body-file "$body"
    echo "updated #$existing ($n findings)"
  else
    gh issue create --title "$title" --label "$LABEL" --body-file "$body"
    echo "opened tracking issue ($n findings)"
  fi
else
  if [ -n "$existing" ]; then
    gh issue close "$existing" --comment "✅ Latest grype scan found no known CVEs in the image closure. Closing."
    echo "closed #$existing (clean)"
  else
    echo "clean; no open issue"
  fi
fi
