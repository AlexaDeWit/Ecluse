# Release and supply-chain operations

> Part of the [Écluse architecture overview](../architecture.md).

How Écluse is built into a container image, published, attested, and scanned. The
contributor-facing summary and the `task` targets live in
[`CONTRIBUTING.md`](../../CONTRIBUTING.md); this document is the operational detail behind
them. The consumer-side verify recipe is in the
[README](../../README.md#verifying-the-image).

## Releases and container image

Écluse ships as a lean OCI image built by Nix (`dockerTools.buildLayeredImage`, see
[`flake.nix`](../../flake.nix)), not a Dockerfile. The image is the binary's runtime closure
plus CA certificates and nothing else: no shell, no package manager. It runs non-root (uid
65532) and is bit-for-bit reproducible. Build it locally with `task docker-build`, which
writes `./result`, a `docker-archive`.

> The image is roughly 23 MB. A residual chunk (`curl` / `openssl` / `krb5`) rides in via the
> GHC runtime's `libdw` (elfutils) backtrace support, not application code; excising it needs
> a static-musl build, with its own TLS caveats, and is a deliberate later trim rather than a
> launch blocker.

Publishing is a separate, tag-triggered workflow
([`release.yml`](../../.github/workflows/release.yml)), never part of the PR `gate`. A
`vX.Y.Z` tag must match `ecluse.cabal`'s `version:` field or the release fails fast at a
verify-version step. On a match it builds the image natively for `linux/amd64` and
`linux/arm64` (see [Multi-architecture image](#multi-architecture-image)), assembles them
into one multi-arch index under a single immutable tag, attaches keyless provenance and SBOM
attestations, and publishes a GitHub Release carrying the image digest, the
`gh attestation verify` recipe, and the generated changelog. A pre-release tag (`vX.Y.Z-rc.N`)
is flagged as a prerelease.

**Immutable tags, no `latest`.** The target repo, `ghcr.io/alexadewit/ecluse`, enforces
immutable tags, so every push is a fresh, never-reused tag: the release publishes
`ecluse:X.Y.Z` and nothing else, a single canonical multi-arch tag (an OCI index) that serves
amd64 or arm64 automatically. There is no moving pointer, so pin deployments by digest
(`ghcr.io/alexadewit/ecluse@sha256:…`, the index digest), which is the stronger posture
regardless. Each version's digest is published in its GitHub Release.

### Publishing the capability manifest

The OpenAPI [capability manifest](web-layer.md#capability-manifest) is regenerated at publish time, never committed. `task docs-site` (and `task site`) run the `openapi-gen` executable, which walks the route records to write `openapi.json`, then render it into a static Redoc page under `./_site` for GitHub Pages. The Redoc bundle is vendored and hash-pinned (the `mermaidJs` `fetchurl` pattern), so the site build needs no Node. Output is deterministic (pinned key ordering, fixed base URLs), so a regeneration is a reviewable diff. There is no `GET /openapi.json` route on the running proxy.

## Multi-architecture image

`ecluse:X.Y.Z` is an OCI index over a `linux/amd64` and a `linux/arm64` image, so a consumer
pulls one tag and the registry serves the right architecture.

Each architecture is built natively, not cross-compiled: a matrix `build` job runs the Nix
image build on its own runner, amd64 on `ubuntu-latest` and arm64 on the free public-repo
`ubuntu-24.04-arm` runner, so GHC compiles natively and each per-arch image stays
reproducible, sidestepping GHC cross-compilation (fragile with Template Haskell). The build
legs are credential-free, each uploading only its image archive and per-arch SBOM as a
workflow artifact.

A single privileged `publish` job assembles the index from the two archives
([`push-multiarch.sh`](../../scripts/push-multiarch.sh)) and pushes only the one canonical
tag. The assembly is daemonless: `skopeo` writes each archive into an on-disk OCI layout and
[`regctl`](https://regclient.org) builds the index and copies it, index plus both platform
images, to the registry. Daemonless OCI layouts leave no per-arch tags behind (immutable tags
could never be reused or deleted) and sidestep the rootless-container user-namespace limits
that would otherwise fail the build from `/nix/store`. Centralising the push in one job also
keeps the registry credential in the protected `release` environment, off the build legs.

## Supply-chain attestations

Each release attaches keyless (Sigstore / OIDC, no stored key) attestations to the image by
digest, recorded in the public Rekor transparency log and stored as immutable OCI referrers,
so they cannot be tampered with and coexist with the repo's immutable tags. They are produced
in CI by GitHub's [attest-actions](https://github.com/actions/attest-build-provenance).

Because the image is multi-arch, the attestations are per platform plus the index: provenance
is attested on the index digest (what `gh attestation verify oci://…:X.Y.Z` resolves to) and
on each platform digest, so a consumer pinning one architecture can verify it too. The SBOM is
attested per platform, since each arch has its own C closure, binding it to that platform's
digest rather than the index.

- **Provenance** (`actions/attest-build-provenance`). SLSA provenance from the run context:
  source repo and commit, the release workflow, and the run. The "who built it" guarantee is
  the keyless signing identity, the release workflow's OIDC cert.
- **SBOM** (`actions/attest-sbom`, content from `task sbom`). Generated with
  [`sbomnix`](https://github.com/tiiuae/sbomnix) from the Nix closure of the exact binary the
  image ships (`.#ecluse-bin`), not a scan of the image, which could not see the
  statically-linked Haskell libraries. It lists the real contents (the `ecluse` binary, whose
  Haskell dependencies are statically linked over a dynamic glibc, plus that glibc, zlib, and
  the curl / openssl / krb5 chunk from the GHC runtime) with no dynamic-build noise to trip
  CVE scanners, and is independently derivable because the image is reproducible.

The attest-actions are used rather than cosign because cosign stores attestations under a
single mutable `.att` tag, which the repo's immutable tags forbid; each attestation is instead
its own immutable referrer. A separate image signature is unnecessary: the provenance
attestation already binds the digest to the builder identity. Consumers verify by digest with
`gh attestation verify` (see the [README](../../README.md#verifying-the-image)).

**Authentication.** Écluse is published to GHCR with no long-lived static credentials, using
the ephemeral, repository-scoped `GITHUB_TOKEN` (`packages: write`), which exists only for the
job's duration and is constrained to this repository. The keyless attestations above (via
GitHub OIDC, `id-token: write` + `attestations: write`) offset the static-token weakness. The
full build-push-attest chain runs on a `vX.Y.Z` tag or a `workflow_dispatch`, gated by the
`release` environment's required reviewer.

## Vulnerability scanning and dependency freshness

Two arms keep the image's dependency closure honest: detection and freshness.

**Detection, `grype` (the authority).** `task scan` builds the sbomnix SBOM of the application
closure into `sbom/`, runs `grype`, and saves the severity-rated findings in `grype.json`.
`task scan-vulnix` is a secondary [vulnix](https://github.com/flyingcircusio/vulnix)
cross-check, broader and Nix-patch-aware but un-graded, so not the authority. A naive closure
scan with distro-advisory matchers reports around a thousand mostly-irrelevant CVEs; the
grype-over-SBOM view is the curated one. Both scanners come from the single pinned nixpkgs
(26.05).

The [`security.yml`](../../.github/workflows/security.yml) workflow is report-only and never
gates a PR, since the closure is fixed by a `flake.lock` bump, not an in-PR change. On a PR it
runs only when the flake changes; on a daily schedule it scans `main` and opens or updates a
single tracking issue (label `security:vuln-scan`) when grype reports CVEs, closing it when
clean, so CVEs disclosed after a release still surface.

**Freshness, Renovate.** [`renovate.json5`](../../.github/renovate.json5) runs one bot across
every ecosystem the repo has: flake inputs, GitHub Actions, and Hackage cabal dependencies.
For Nix it refreshes `flake.lock` weekly, so the C-library closure picks up upstream security
fixes; the gate validates each bump and the scan re-runs on it. Fixing a finding is usually
just merging the Renovate PR. HSEC advisories (the Haskell Security Response Team database)
are exported to [OSV.dev](https://osv.dev), and Renovate raises a fix-PR through
`osvVulnerabilityAlerts` when one affects a cabal dep, because the default GitHub Advisory
Database has no Hackage ecosystem. To cover the full resolved install plan, a custom regex
manager parses `cabal.project.freeze`; GHC-boot libraries are left to move with the compiler
and are covered by grype's scan of the runtime closure.

## Posture scoring, OpenSSF Scorecard

[`scorecard.yml`](../../.github/workflows/scorecard.yml) runs OpenSSF Scorecard weekly and on
branch-protection changes. It grades the repository's supply-chain posture, branch protection,
pinned dependencies, signed and attested releases, SAST, token permissions, and dangerous
workflow patterns, uploads findings to the Security tab, and publishes the score that backs
the README badge. It is report-only, never gating a PR. For a supply-chain policy proxy this
is dogfooding: the same hygiene it proxies for, measured on itself.
