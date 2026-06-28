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
certificates and nothing else, no shell, no package manager, runs **non-root**
(uid 65532), and is **bit-for-bit reproducible** (a fitting property for a
supply-chain tool). Build it locally with `make docker-build` (→ `./result`, a
`docker-archive`).

> The image is ~23 MB. A residual chunk (`curl`/`openssl`/`krb5`) rides in via
> the GHC runtime's `libdw` (elfutils) backtrace support, not our code; excising
> it needs a static-musl build (with its own TLS caveats) and is a deliberate
> later trim, not a launch blocker.

Publishing is a separate, tag-triggered workflow
([`.github/workflows/release.yml`](../../.github/workflows/release.yml)), **not**
part of the PR `gate`. Pushing a `vX.Y.Z` tag builds the image **natively for both
`linux/amd64` and `linux/arm64`** (see [Multi-architecture image](#multi-architecture-image)),
assembles the two builds into a single multi-arch manifest list pushed under one
immutable tag, attaches keyless provenance + SBOM attestations as immutable OCI
referrers (the GitHub attest-actions; SBOM content from `make sbom`), and publishes
a **GitHub Release** carrying the image digest, the `gh attestation verify` recipe,
and the auto-generated changelog
([`scripts/release-notes.sh`](../../scripts/release-notes.sh)). A pre-release tag
(`vX.Y.Z-rc.N`) is flagged as a prerelease; an `rc` smoke test via
`workflow_dispatch` publishes the image but no Release. (`make docker-build` /
`make docker-push` remain the **single-arch, host-architecture** path for local
builds and manual pushes; the cross-arch assembly is CI-only.)

**Immutable tags, no `latest`.** The target repo
([`alexadewit/ecluse`](https://hub.docker.com/r/alexadewit/ecluse)) enforces
immutable tags, so every push is a fresh, never-reused tag: the release publishes
`ecluse:X.Y.Z` (from the git tag) and nothing else, a **single canonical
multi-arch tag** (an OCI index) that serves amd64 or arm64 automatically, with no
per-arch tags. There is deliberately no moving pointer, **pin deployments by
digest** (`alexadewit/ecluse@sha256:…`, the index digest), which is the stronger
supply-chain posture regardless; the digest for each version is published in its
GitHub Release.

## Multi-architecture image

The image is published as a **single multi-arch tag**, `ecluse:X.Y.Z` is an OCI
manifest list (index) over a `linux/amd64` and a `linux/arm64` image, so a
consumer pulls the one tag and the registry serves the right architecture. Many
consumers are migrating clouds to arm64 while others still need amd64; one tag
covers both.

Each architecture is **built natively, not cross-compiled.** A matrix `build` job
runs the Nix image build on its own runner, amd64 on `ubuntu-latest`, arm64 on
GitHub's free public-repo `ubuntu-24.04-arm` runner, so GHC compiles natively on
each target and the per-arch image stays bit-for-bit reproducible. This sidesteps
GHC cross-compilation (fragile with Template Haskell) entirely. The build legs are
**credential-free**: each only uploads its image archive and per-arch SBOM as a
workflow artifact.

A single privileged `publish` job then assembles the index **locally** from the
two archives ([`scripts/push-multiarch.sh`](../../scripts/push-multiarch.sh)) and
pushes **only the one canonical tag**. The assembly is **daemonless**: `skopeo`
writes each archive into an on-disk **OCI image layout** (plain files), and
[`regctl`](https://regclient.org) (regclient) builds the index from those layouts
and copies it, index plus both platform images, as digest-addressed blobs, to the
registry under the single tag. Two design choices are deliberate:

- **Local assembly, not registry-side.** Building the list from registry
  references (e.g. `docker buildx imagetools create`) would require the platform
  images to be pre-pushed under their own tags, which, because the repo's tags are
  immutable and the push token has no delete scope, would persist forever. Local
  assembly leaves **no per-arch tags behind**.
- **Daemonless (OCI layouts + regctl), not a container engine.** A rootless
  container engine (podman/buildah) needs a local `containers-storage`, whose
  **user namespace ubuntu-24.04's AppArmor denies** for binaries running from
  `/nix/store`, the publish job would fail with `unshare(...): Operation not
  permitted`. OCI layouts are just files and `regctl` is pure-Go over HTTP, so the
  whole path avoids user namespaces (and any `sudo`/AppArmor workaround) entirely.

Centralising the push in one job also keeps the registry credential in exactly one
place (the protected `release` environment), off the matrix build legs, a small
blast-radius win on top of the attestation posture below.

## Supply-chain attestations

Each release attaches **keyless** (Sigstore/OIDC, no stored key) attestations to
the image by **digest**, recorded in the public **Rekor** transparency log and
stored as **immutable OCI referrers**, each a content-addressed, write-once
artifact that is never updated, so it can't be tampered with and it coexists with
the repo's immutable tags. They are produced in CI by GitHub's
[attest-actions](https://github.com/actions/attest-build-provenance).

Because the image is [multi-arch](#multi-architecture-image), the attestations are
**per platform plus the index**: **provenance** is attested on the index digest
(what `gh attestation verify oci://…:X.Y.Z` resolves to) *and* on each platform
digest (so a consumer pinning a single architecture by digest can verify it too);
the **SBOM** is attested **per platform**, each arch has its own C closure, so its
SBOM binds to that platform's digest, never to the index, which has no closure of
its own.

- **Provenance** (`actions/attest-build-provenance`). SLSA provenance generated
  from the run context, the source repo + commit, the release workflow, and the
  run (the *how/where*). The cryptographic "who built it" guarantee is the
  keyless signing identity (the release workflow's OIDC cert).
- **SBOM** (`actions/attest-sbom`, content from `make sbom`). Generated with
  [`sbomnix`](https://github.com/tiiuae/sbomnix) from the **Nix closure of the
  exact binary the image ships** (`.#ecluse-bin`, stripped/static), not a scan
  of the image, which couldn't see the statically-linked Haskell deps. So it
  lists the real contents (~23 components: the `ecluse` binary plus its C closure
 , glibc, zlib, and the curl/openssl/krb5 chunk that rides in via the GHC
  runtime's `libdw`) with no dynamic-build noise to trip CVE scanners. The
  Haskell deps are compiled into the `ecluse` component; they are pinned by
  `flake.lock` and, because the image is bit-for-bit reproducible, independently
  derivable.

> **Why the GitHub attest-actions, not cosign?** cosign stores attestations under
> a single mutable `.att` tag (a second attestation must *update* it), which the
> repo's immutable tags forbid, and cosign has no referrer mode for attestations
> at any version (only for signatures). The attest-actions store each attestation
> as its own immutable referrer, so the storage is immutable too. A separate
> image signature is unnecessary: the provenance attestation already binds the
> digest to the builder identity.

Consumers verify by digest with `gh attestation verify`; the recipe lives in the
[README](../../README.md#verifying-the-image).

**Authentication (Docker Hub).** Docker Hub has no OIDC keyless login, so the push
needs a long-lived token, kept as weak and contained as possible:

- **Per-repo token scoping is not available on a personal account**, only
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
  (`id-token: write` + `attestations: write`), immutable OCI referrers + the
  Rekor log, no stored key, giving verifiable provenance and contents that
  offset the static-token weakness. Verify with `gh attestation verify` (see
  [README](../../README.md#verifying-the-image)).

> The `release` environment and its `DOCKERHUB_*` secrets are configured, so a
> `vX.Y.Z` tag (or a `workflow_dispatch`) runs the full build → push → attest
> chain, gated by the environment's required reviewer.

## Vulnerability scanning & dependency freshness

Two arms keep the image's dependency closure honest over time, **detection** and
**freshness**.

**Detection, `grype` (the authority).** `make scan` builds the sbomnix SBOM of
`.#ecluse-bin` (the exact shipped binary) and scans it with
[grype](https://github.com/anchore/grype) against its maintained DB →
severity-rated, low-noise findings in `grype.json` (plus a table). `make
scan-vulnix` is a secondary [vulnix](https://github.com/flyingcircusio/vulnix)
cross-check, more comprehensive and Nix-patch-aware, but un-graded, so *not* the
authority. (A naive closure scan with distro-advisory matchers reports ~1000
mostly-irrelevant CVEs, ancient or Debian/Ubuntu advisories that don't apply to
a Nix build; grype-over-SBOM is the curated view.) Both scanners come from the
single pinned nixpkgs (26.05). (On the older 24.11 base that pin's `vulnix`
(1.10.1) was broken against NVD's retired feeds, which once forced a second,
newer nixpkgs input used *only* for `vulnix`; 26.05 ships a working one, so that
extra input is gone.)

The [`security.yml`](../../.github/workflows/security.yml) workflow is
**report-only**, it never gates a PR, because the closure is fixed by a
`flake.lock` bump, not an in-PR change. On a PR it runs only when the flake
changes; on a **daily schedule** it scans `main` and opens/updates a single
tracking issue (label `security:vuln-scan`) when grype reports CVEs, closing it
when clean, so CVEs disclosed *after* a release still surface.

**Freshness, Renovate.** [`renovate.json5`](../../.github/renovate.json5) runs one
bot across every ecosystem the repo has, the **flake inputs** (`nix`), **GitHub
Actions**, and **Haskell/Hackage** cabal deps (which Dependabot has no manager
for, the reason we migrated). For Nix it refreshes `flake.lock` weekly (the
branch-tracked inputs carry no version to bump), so the C-library closure picks
up upstream security fixes; the gate validates each bump and the scan re-runs on
it. This is the remediation arm, fixing a finding is usually just merging the
Renovate PR.

**Haskell advisories, Renovate's OSV alerting.** HSEC advisories (the
[Haskell Security Response Team](https://github.com/haskell/security-advisories)
database) are exported to [OSV.dev](https://osv.dev), and Renovate maps the
`hackage` datasource it extracts from `ecluse.cabal` to the OSV `Hackage`
ecosystem, so `osvVulnerabilityAlerts: true` (set in
[`renovate.json5`](../../.github/renovate.json5)) raises a fix-PR when an advisory
affects one of our cabal deps. It must be the OSV-based opt-in: the default
platform `vulnerabilityAlerts` (the GitHub Advisory Database) has no Hackage
ecosystem, so the OSV flag is what brings HSEC coverage. This alerts on the
**declared** cabal deps; a deeper audit of the full resolved install plan,transitive deps and GHC-boot libraries (`base`, `process`, …), with SARIF output, via `cabal-audit`, remains an optional future enhancement. (`cabal-audit` and
the modern `hsec-tools` 0.5.x build cleanly on the current GHC 9.10 toolchain;
the earlier "broken in nixpkgs" blocker applied to the pre-26.05 / GHC-9.6 set.)
It stays deferred because the statically-linked Haskell deps are a lower-risk
surface than the C libs grype already watches.

## Posture scoring, OpenSSF Scorecard

[`scorecard.yml`](../../.github/workflows/scorecard.yml) runs **OpenSSF Scorecard**
weekly (and on branch-protection changes / pushes to `main`). It grades the
repository's supply-chain posture, branch protection, pinned dependencies,
signed/attested releases, SAST, token permissions, and dangerous workflow
patterns, uploads findings to the Security tab (code scanning), and publishes a
public score that backs the README badge. It is **report-only**: it never gates a
PR. For a tool whose purpose is supply-chain resilience, this is dogfooding, the
same hygiene we proxy for, measured on ourselves.
