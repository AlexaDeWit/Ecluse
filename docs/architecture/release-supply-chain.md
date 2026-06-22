# Release & Supply-Chain Operations

> Part of the [Écluse architecture overview](../architecture.md).

How Écluse is built into a container image, published, attested, and scanned. The
contributor-facing summary and the `make` targets live in
[`../../CONTRIBUTING.md`](../../CONTRIBUTING.md); this document is the operational
detail behind them. The consumer-side verify recipe is in the
[README](../../README.md#verifying-the-image).

## Releases & container image

Écluse ships as a lean OCI image built **by Nix**
(`dockerTools.buildLayeredImage`, see [`flake.nix`](../../flake.nix)), not a
Dockerfile. The image is the stripped binary's runtime closure plus CA
certificates and nothing else — no shell, no package manager, runs **non-root**
(uid 65532), and is **bit-for-bit reproducible** (a fitting property for a
supply-chain tool). Build it locally with `make docker-build` (→ `./result`, a
`docker-archive`).

> The image is ~23 MB. A residual chunk (`curl`/`openssl`/`krb5`) rides in via
> the GHC runtime's `libdw` (elfutils) backtrace support, not our code; excising
> it needs a static-musl build (with its own TLS caveats) and is a deliberate
> later trim, not a launch blocker.

Publishing is a separate, tag-triggered workflow
([`.github/workflows/release.yml`](../../.github/workflows/release.yml)) — **not**
part of the PR `gate`. Pushing a `vX.Y.Z` tag builds the image, pushes it
(`make docker-push`), attaches keyless provenance + SBOM attestations as immutable
OCI referrers (the GitHub attest-actions; SBOM content from `make sbom`), and
publishes a **GitHub Release** carrying the image digest, the `gh attestation
verify` recipe, and the auto-generated changelog
([`scripts/release-notes.sh`](../../scripts/release-notes.sh)). A pre-release tag
(`vX.Y.Z-rc.N`) is flagged as a prerelease; an `rc` smoke test via
`workflow_dispatch` publishes the image but no Release.

**Immutable tags — no `latest`.** The target repo
([`alexadewit/ecluse`](https://hub.docker.com/r/alexadewit/ecluse)) enforces
immutable tags, so every push is a fresh, never-reused tag: the release publishes
`ecluse:X.Y.Z` (from the git tag) and nothing else. There is deliberately no
moving pointer — **pin deployments by digest** (`alexadewit/ecluse@sha256:…`),
which is the stronger supply-chain posture regardless; the digest for each version
is published in its GitHub Release.

## Supply-chain attestations

Each release attaches two **keyless** (Sigstore/OIDC, no stored key) attestations
to the image, bound to its **digest**, recorded in the public **Rekor**
transparency log, and stored as **immutable OCI referrers** — each a
content-addressed, write-once artifact that is never updated, so it can't be
tampered with and it coexists with the repo's immutable tags. Both are produced
in CI by GitHub's [attest-actions](https://github.com/actions/attest-build-provenance):

- **Provenance** (`actions/attest-build-provenance`). SLSA provenance generated
  from the run context — the source repo + commit, the release workflow, and the
  run (the *how/where*). The cryptographic "who built it" guarantee is the
  keyless signing identity (the release workflow's OIDC cert).
- **SBOM** (`actions/attest-sbom`, content from `make sbom`). Generated with
  [`sbomnix`](https://github.com/tiiuae/sbomnix) from the **Nix closure of the
  exact binary the image ships** (`.#ecluse-bin`, stripped/static) — not a scan
  of the image, which couldn't see the statically-linked Haskell deps. So it
  lists the real contents (~23 components: the `ecluse` binary plus its C closure
  — glibc, zlib, and the curl/openssl/krb5 chunk that rides in via the GHC
  runtime's `libdw`) with no dynamic-build noise to trip CVE scanners. The
  Haskell deps are compiled into the `ecluse` component; they are pinned by
  `flake.lock` and, because the image is bit-for-bit reproducible, independently
  derivable.

> **Why the GitHub attest-actions, not cosign?** cosign stores attestations under
> a single mutable `.att` tag (a second attestation must *update* it), which the
> repo's immutable tags forbid — and cosign has no referrer mode for attestations
> at any version (only for signatures). The attest-actions store each attestation
> as its own immutable referrer, so the storage is immutable too. A separate
> image signature is unnecessary: the provenance attestation already binds the
> digest to the builder identity.

Consumers verify by digest with `gh attestation verify`; the recipe lives in the
[README](../../README.md#verifying-the-image).

**Authentication (Docker Hub).** Docker Hub has no OIDC keyless login, so the push
needs a long-lived token — kept as weak and contained as possible:

- **Per-repo token scoping is not available on a personal account** — only
  account-wide access tokens (choose the *Read & Write* permission level; `Delete`
  is not needed for immutable-tag pushes). True per-repository scoping requires an
  **Organization Access Token**, which needs a Docker org on a paid plan. The
  pragmatic mitigation without paying: put the image under a dedicated **machine
  account** that can reach *only* this repo, so its account-wide token is
  effectively repo-scoped.
- Store it as `DOCKERHUB_USERNAME` / `DOCKERHUB_TOKEN` on a **protected `release`
  GitHub Environment** (required reviewers), so only an approved release job can
  read it. The token is fed via `--password-stdin`, never argv or `echo`.
- Each image carries **keyless provenance + SBOM attestations** via GitHub OIDC
  (`id-token: write` + `attestations: write`) — immutable OCI referrers + the
  Rekor log, no stored key — giving verifiable provenance and contents that
  offset the static-token weakness. Verify with `gh attestation verify` (see
  [README](../../README.md#verifying-the-image)).

> The `release` environment and its `DOCKERHUB_*` secrets are configured, so a
> `vX.Y.Z` tag (or a `workflow_dispatch`) runs the full build → push → attest
> chain, gated by the environment's required reviewer.

## Vulnerability scanning & dependency freshness

Two arms keep the image's dependency closure honest over time — **detection** and
**freshness**.

**Detection — `grype` (the authority).** `make scan` builds the sbomnix SBOM of
`.#ecluse-bin` (the exact shipped binary) and scans it with
[grype](https://github.com/anchore/grype) against its maintained DB →
severity-rated, low-noise findings in `grype.json` (plus a table). `make
scan-vulnix` is a secondary [vulnix](https://github.com/flyingcircusio/vulnix)
cross-check — more comprehensive and Nix-patch-aware, but un-graded, so *not* the
authority. (A naive closure scan with distro-advisory matchers reports ~1000
mostly-irrelevant CVEs — ancient or Debian/Ubuntu advisories that don't apply to
a Nix build; grype-over-SBOM is the curated view.) Note: the pinned nixpkgs'
`vulnix` (1.10.1) is broken against NVD's retired 1.1 feeds, so a working `vulnix`
comes from a second, newer nixpkgs input used *only* for that tool.

The [`security.yml`](../../.github/workflows/security.yml) workflow is
**report-only** — it never gates a PR, because the closure is fixed by a
`flake.lock` bump, not an in-PR change. On a PR it runs only when the flake
changes; on a **daily schedule** it scans `main` and opens/updates a single
tracking issue (label `security:vuln-scan`) when grype reports CVEs, closing it
when clean — so CVEs disclosed *after* a release still surface.

**Freshness — Renovate.** [`renovate.json5`](../../.github/renovate.json5) runs one
bot across every ecosystem the repo has — the **flake inputs** (`nix`), **GitHub
Actions**, and **Haskell/Hackage** cabal deps (which Dependabot has no manager
for — the reason we migrated). For Nix it refreshes `flake.lock` weekly (the
branch-tracked inputs carry no version to bump), so the C-library closure picks
up upstream security fixes; the gate validates each bump and the scan re-runs on
it. This is the remediation arm — fixing a finding is usually just merging the
Renovate PR.

**Haskell advisories** (`cabal-audit` / the HSEC database) are a deferred
follow-up: `cabal-audit` is marked broken in the pinned nixpkgs, and the
statically-linked Haskell deps are a lower-risk surface than the C libs.
