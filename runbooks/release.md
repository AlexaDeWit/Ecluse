# Release runbook

The step-by-step procedure for cutting an Écluse release. The maintainer runs it; every step
needs repository administrator access. The design behind the pipeline, and why each control
exists, live in
[Release and supply-chain operations](../docs/architecture/release-supply-chain.md). This file
says only what to do and in what order.

The whole pipeline is one workflow, `.github/workflows/release.yml`, triggered by a `v*` tag
push. The maintainer's manual acts are: the pre-flight checks, the tag, one deployment
approval, the package-visibility check, and the verification.

## 1. Pre-flight

Run before touching a tag.

```bash
# main is green at the commit to tag (merged does not mean gate-green)
gh run list --branch main --limit 4

# the version the release will assert; the tag must match it
env -u IN_NIX_SHELL nix develop .#ci --command task version

# the tag must not exist anywhere
git ls-remote --tags origin
```

`task init-release` re-checks all three and refuses on any of them, so `DRY_RUN=1 task
init-release VERSION=vX.Y.Z` rehearses this whole section without creating anything.

Scope must be final before the tag: the `Tag Integrity` ruleset blocks tag deletion and update
for everyone, administrators included, so a pushed tag cannot be moved. Decide what merges
first.

## 2. Cut and push the tag

```bash
env -u IN_NIX_SHELL nix develop --command task init-release VERSION=v0.2.0
```

It refuses unless the version matches `ecluse.cabal`, the working tree is clean, `HEAD` is
`origin/main`, the `CI gate` check is green on that commit, and the tag exists neither locally
nor on origin. It prints that commit, then cuts the signed tag pinned to it, verifies the
signature, and asks you to type the tag back before it pushes. `DRY_RUN=1` stops after the
checks, creating nothing.

The tag is pinned to the commit the checks cleared, so `HEAD` moving under you cannot retarget
it. The ruleset refuses an unsigned tag, and after the push neither the tag nor its target can
change. That confirmation is the last point where stopping costs nothing.

## 3. What fires, and where it stops

The tag push runs `verify-version` (tag matches `ecluse.cabal`), then the two native `build`
jobs (amd64 and arm64, image plus SBOM each). The `publish` job then STOPS at the `release`
environment gate: a 72-hour wait timer with self-review prevented. That pause is a designed
control against stolen-credential tag pushes, not a misconfiguration.

**Release the gate with the admin bypass on the pending deployment.** Open the run page when
`publish` shows Waiting; an administrator sees the bypass on the pending deployment. Never zero
the wait timer to get past it. If the bypass button does not appear, the fallback is to set the
timer to 0 in the environment settings, run the release, and restore it the same day; the
restore is mandatory, or the architecture document's description of the gate becomes false.

After approval, `publish` runs alone: it assembles and pushes one multi-arch index, attaches
three provenance attestations (index, amd64, arm64) and two SBOM attestations, and creates the
GitHub Release with the digest pinned in the body. The Release also carries the five attestation
bundles and both SPDX documents as assets. The only credential anywhere is the
ephemeral `GITHUB_TOKEN`.

## 4. The package, on a first push to a namespace

A GHCR package created by a workflow push starts private, and a user namespace cannot preset
visibility before the first push. So on the first release (or any release that creates a new
package): once the push step completes, open the package's settings, set visibility to Public,
confirm the package is linked to the repository, and confirm immutable tags is on (the README
and the architecture document both promise one immutable tag per version). Until the flip,
every published install and verify command fails for anonymous readers.

## 5. Verify

```bash
env -u IN_NIX_SHELL nix develop --command task verify-release VERSION=v<version>
```

It reads the digest pin out of the release body, checks the pin belongs to this project and that
the version's own image tag resolves to it, fetches it, checks the index carries both amd64 and
arm64, checks the Release carries all seven assets, and verifies the provenance.

Authenticated as the owner, a registry read passes even against a private package, which hides
exactly the failure an anonymous reader would hit. So the registry reads run through `skopeo
--no-creds`, which sends no credential. That tests the reader's path without discarding your
stored login, which `docker logout` would. The `gh` calls still run on your token, so the script
checks outright that the Release is not a draft rather than inferring it.

`gh attestation verify` checks one predicate type per run and defaults to SLSA provenance, so
the script never reaches the SBOM attestations. Those are attested per platform, so one needs a
platform digest. Verifying against a downloaded bundle is also manual:

```bash
gh release download v<version> -p 'ecluse-*-provenance.sigstore.json'
gh attestation verify "oci://ghcr.io/alexadewit/ecluse@sha256:<digest>" \
  --repo AlexaDeWit/Ecluse --bundle ecluse-<version>-provenance.sigstore.json

gh attestation verify "oci://ghcr.io/alexadewit/ecluse@sha256:<platform-digest>" \
  --repo AlexaDeWit/Ecluse --predicate-type https://spdx.dev/Document/v2.3
```

Announcement wording: the image carries attestations, verified with `gh attestation verify`.
There is no cosign signature, by design, so never write "signed image".

## 6. Release notes

The workflow generates the body from the pull requests merged since the previous tag. Replace it
only when you want different wording:

```bash
gh release edit v<version> --notes-file <handwritten.md>
```

Keep the digest pin and the asset list at the top. `task verify-release` reads the pin out of the
body, so a replacement that drops it fails verification.

## 7. Rollback

The tag is immutable to everyone (the ruleset's bypass list is empty, and rulesets give
administrators no implicit bypass). The GitHub Release object is separately deletable, which is
the usable retraction path.

| Failure | Action |
| --- | --- |
| Build fails before publish | Nothing published. Fix on `main`; cut the next patch version. The tag stays where it is. |
| Wrong commit tagged, not pushed | `git tag -d`, re-tag. This is why step 2 verifies first. |
| Wrong commit tagged and pushed | Ship the next patch version, or deliberately edit the ruleset. |
| Published image is broken | The image tag is immutable too. Ship the next patch version; delete or mark the Release. |
| Release body wrong | `gh release edit --notes-file`. Safe and repeatable. |

## 8. Rehearsal

`workflow_dispatch` on `release.yml` runs the whole pipeline except the Release creation, which
is gated to tag pushes. It exercises the cold builds, the gate, the push, all five attestations,
and the asset staging, without cutting a release. Only the upload itself goes unexercised:

```bash
gh workflow run release.yml -f tag=<never-pushed-rc-tag>
```

Use a tag string never pushed before: GHCR's immutable tags refuse reuse. Note that a PUSHED
`v*-rc.*` tag does cut a prerelease Release; only the dispatch path skips it.
