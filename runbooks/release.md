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

Scope must be final before the tag: the `Tag Integrity` ruleset blocks tag deletion and update
for everyone, administrators included, so a pushed tag cannot be moved. Decide what merges
first.

## 2. Cut and push the tag

```bash
env -u IN_NIX_SHELL nix develop --command task tag   # signed tag, not pushed
git tag -v "v$(env -u IN_NIX_SHELL nix develop .#ci --command task version)"
git rev-parse --verify "v0.1.0^{commit}"             # the commit you intend
git push origin v0.1.0                               # the point of no return
```

`task tag` signs the tag; the ruleset refuses an unsigned one. Verify the signature and the
target commit before the push, because after the push neither can change.

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
GitHub Release with the digest pinned in the body. The only credential anywhere is the
ephemeral `GITHUB_TOKEN`.

## 4. The package, on a first push to a namespace

A GHCR package created by a workflow push starts private, and a user namespace cannot preset
visibility before the first push. So on the first release (or any release that creates a new
package): once the push step completes, open the package's settings, set visibility to Public,
confirm the package is linked to the repository, and confirm immutable tags is on (the README
and the architecture document both promise one immutable tag per version). Until the flip,
every published install and verify command fails for anonymous readers.

## 5. Verify

Run the first three from a logged-out context (`docker logout ghcr.io`, or a machine that never
authenticated): as the owner they pass even against a private package, which hides exactly the
failure a reader would hit.

```bash
gh release view v0.1.0                                        # digest pin present
docker pull ghcr.io/alexadewit/ecluse@sha256:<digest>          # anonymous pull works
gh attestation verify "oci://ghcr.io/alexadewit/ecluse@sha256:<digest>" \
  --repo AlexaDeWit/Ecluse                                     # attestations verify
docker manifest inspect ghcr.io/alexadewit/ecluse:0.1.0 | grep architecture
# expect both amd64 and arm64
```

Announcement wording: the image carries attestations, verified with `gh attestation verify`.
There is no cosign signature, by design, so never write "signed image".

## 6. Release notes

With no earlier tag, the generated body can come back sparse or over-inclusive, so do not rely on
it for the first release. `gh release create` succeeds either way, so publish and then replace the
body, keeping the digest and the verify recipe at the top:

```bash
gh release edit v0.1.0 --notes-file <handwritten.md>
```

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
is gated to tag pushes. It exercises the cold builds, the gate, the push, and all five
attestations without cutting a release:

```bash
gh workflow run release.yml -f tag=<never-pushed-rc-tag>
```

Use a tag string never pushed before: GHCR's immutable tags refuse reuse. Note that a PUSHED
`v*-rc.*` tag does cut a prerelease Release; only the dispatch path skips it.
