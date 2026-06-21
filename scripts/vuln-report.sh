#!/usr/bin/env bash
#
# Render a grype JSON report as Markdown: a severity count table plus the
# Critical/High findings with their fixed-in versions. Used for the CI job
# summary and as the tracking-issue body. See CONTRIBUTING.md → "Vulnerability
# scanning".
#
# Usage: scripts/vuln-report.sh [grype.json]
set -euo pipefail

python3 - "${1:-grype.json}" <<'PY'
import json, sys, collections

try:
    matches = json.load(open(sys.argv[1])).get("matches", [])
except Exception:
    print("_No grype report found (scan may have failed)._")
    sys.exit(0)

order = ["Critical", "High", "Medium", "Low", "Negligible", "Unknown"]
by = collections.Counter((m["vulnerability"].get("severity") or "Unknown") for m in matches)
present = [s for s in order if by.get(s)]

print(f"### Vulnerability scan (grype) — {len(matches)} finding(s)\n")
if matches:
    print("| " + " | ".join(present) + " |")
    print("|" + "|".join("---" for _ in present) + "|")
    print("| " + " | ".join(str(by[s]) for s in present) + " |\n")
else:
    print("No known CVEs in the image closure. ✅\n")

hi = [m for m in matches if (m["vulnerability"].get("severity") in ("Critical", "High"))]
if hi:
    print("**Critical / High:**\n")
    print("| Severity | ID | Package | Version | Fixed in |")
    print("|---|---|---|---|---|")
    for m in sorted(hi, key=lambda m: order.index(m["vulnerability"].get("severity") or "Unknown")):
        v, a = m["vulnerability"], m["artifact"]
        fix = ", ".join((v.get("fix") or {}).get("versions") or []) or "—"
        print(f"| {v.get('severity')} | {v.get('id')} | {a.get('name')} | {a.get('version')} | {fix} |")
PY
