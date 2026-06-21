#!/usr/bin/env bash
#
# Emit a SLSA v1.0 provenance *predicate* — the body that `cosign attest --type
# slsaprovenance1` wraps into a signed in-toto Statement bound to the image
# digest. Fields come from the GitHub Actions run context (the cryptographic
# proof of "who built it" is the keyless signing identity, not this body; this
# records the structured how/where). See CONTRIBUTING.md → "Supply-chain
# attestations".
#
# Usage: scripts/gen-provenance.sh [OUT]   (default: provenance.predicate.json)
set -euo pipefail

out="${1:-provenance.predicate.json}"

python3 - "$out" <<'PY'
import json, os, sys, datetime

server = os.environ.get("GITHUB_SERVER_URL", "https://github.com")
repo   = os.environ.get("GITHUB_REPOSITORY", "")
sha    = os.environ.get("GITHUB_SHA", "")
ref    = os.environ.get("GITHUB_REF", "")
run_id = os.environ.get("GITHUB_RUN_ID", "")
wf_ref = os.environ.get("GITHUB_WORKFLOW_REF", "")
tag    = os.environ.get("TAG", "")
now    = datetime.datetime.now(datetime.timezone.utc).isoformat()

predicate = {
    "buildDefinition": {
        "buildType": f"{server}/{repo}/release-nix-image/v1",
        "externalParameters": {
            "workflowRef": wf_ref,
            "imageTag": tag,
        },
        "internalParameters": {
            "runnerEnvironment": "github-hosted",
        },
        # The source the image was built from, with its commit digest.
        "resolvedDependencies": [
            {
                "uri": f"git+{server}/{repo}@{ref}",
                "digest": {"gitCommit": sha},
            }
        ],
    },
    "runDetails": {
        # Informational; the verified builder identity is the signing cert.
        "builder": {"id": f"{server}/{repo}/.github/workflows/release.yml@{ref}"},
        "metadata": {
            "invocationId": f"{server}/{repo}/actions/runs/{run_id}",
            "startedOn": now,
        },
    },
}

with open(sys.argv[1], "w") as f:
    json.dump(predicate, f, indent=2)
print(f"provenance: wrote {sys.argv[1]}")
PY
