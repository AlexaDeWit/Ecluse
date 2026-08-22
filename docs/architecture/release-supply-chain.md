# Release and supply-chain operations

> Part of the [Écluse architecture overview](../architecture.md).

How Écluse is built into a container image, published, attested, and scanned. This document is the
operational detail behind [`CONTRIBUTING.md`](../../CONTRIBUTING.md), which holds the
contributor-facing summary and the `task` targets. The consumer-side verify recipe is in the
[README](../../README.md#verifying-the-image).

## Releases and container image

Écluse ships as an OCI image that Nix builds (`dockerTools.buildLayeredImage`, see
[`flake.nix`](../../flake.nix)), not a Dockerfile. The image is the binary's runtime closure plus CA
certificates and nothing else: no shell, no package manager. It runs non-root (uid 65532) and is
bit-for-bit reproducible. Build it locally with `task docker-build`, which writes `./result`, a
`docker-archive`.

Publishing is a separate, tag-triggered workflow
([`release.yml`](../../.github/workflows/release.yml)), never part of the PR `gate`. A `vX.Y.Z` tag
must match `ecluse.cabal`'s `version:` field, or the release fails fast at a verify-version step. On
a match the workflow builds the image natively for `linux/amd64` and `linux/arm64` (see
[Multi-architecture image](#multi-architecture-image)).

The workflow assembles the two into one multi-arch index and pushes it to GitHub Container Registry
under a single immutable tag. It attaches keyless provenance and SBOM attestations. It then
publishes a GitHub Release carrying the image digest, the `gh attestation verify` recipe, and the
generated changelog. A pre-release tag (`vX.Y.Z-rc.N`) publishes as a prerelease. GHCR is the only
registry Écluse publishes to.

**Immutable tags, no `latest`.** The target repo, `ghcr.io/alexadewit/ecluse`, enforces immutable
tags, so every push is a fresh, never-reused tag. The release publishes `ecluse:X.Y.Z` and nothing
else: one canonical multi-arch tag (an OCI index) that serves amd64 or arm64 automatically. There is
no moving pointer, so pin deployments by digest (`ghcr.io/alexadewit/ecluse@sha256:…`, the index
digest), which is the stronger posture in any case. Each GitHub Release carries its version's digest.

### The release environment

The `publish` job runs in the GitHub Environment `release`. That environment, not the workflow file,
gates a release. To reproduce this publishing posture in a fork or a rebuilt repository, recreate
three protection rules on it.

- **A required reviewer, `AlexaDeWit`, with self-review prevented.** A tag push builds and then
  stops. The `publish` job waits for a human approval before it reaches the registry or mints an
  attestation. The environment prevents self-review, so the account that pushed the tag cannot
  approve its own deployment. Repository administrators can bypass the protection rules, which
  keeps a single-maintainer release from deadlocking.
- **A wait timer of 4320 minutes (72 hours).** A publish waits on that timer as well as on the
  approval. A tag pushed with a stolen credential sits in a long, visible window. An operator can
  notice it and cancel it before the publish reaches the registry.
- **A deployment branch policy** that admits only the `main` branch and the `v*` tag pattern.
  Nobody can dispatch a publish from another branch.

The environment carries no secrets and no variables, and needs none. The only credential a publish
uses is the ephemeral `GITHUB_TOKEN` that GitHub issues to the job. There is no registry password to
store and nothing to rotate.

## Multi-architecture image

`ecluse:X.Y.Z` is an OCI index over a `linux/amd64` and a `linux/arm64` image, so a consumer pulls
one tag and the registry serves the right architecture. Each architecture builds natively on its
own runner, and one publish job assembles the index
([`push-multiarch.sh`](../../scripts/push-multiarch.sh)).

## Supply-chain attestations

Each release attaches keyless attestations (Sigstore / OIDC, no stored key) to the image by digest.
The public Rekor transparency log records them, and the registry stores each as an immutable OCI
referrer. So nobody can tamper with them, and they coexist with the repo's immutable tags. GitHub's
[attest-actions](https://github.com/actions/attest-build-provenance) produce them in CI.

The image is multi-arch, so the attestations cover each platform plus the index. The release attests
provenance on the index digest (what `gh attestation verify oci://…:X.Y.Z` resolves to) and on each
platform digest. A consumer who pins one architecture can therefore verify it too. The release
attests the SBOM per platform, because each arch has its own C closure. That binds the SBOM to that
platform's digest rather than the index.

- **Provenance** (`actions/attest-build-provenance`). SLSA provenance from the run context: source
  repo and commit, the release workflow, and the run. The "who built it" guarantee is the keyless
  signing identity, the release workflow's OIDC cert.
- **SBOM** (`actions/attest-sbom`, content from `task sbom`).
  [`sbomnix`](https://github.com/tiiuae/sbomnix) generates it from the Nix closure of the exact
  binary the image ships (`.#ecluse-bin`), never from a scan of the image. Such a scan could not see
  the statically-linked Haskell libraries. It lists the real contents: the `ecluse` binary, whose
  Haskell dependencies link statically over a dynamic glibc, plus the platform runtime libraries.
  It carries no dynamic-build noise to trip CVE scanners, and anyone can derive it again because the
  image is reproducible.

The release uses the attest-actions rather than cosign, because cosign stores attestations under a
single mutable `.att` tag, which the repo's immutable tags forbid. Each attestation is instead its
own immutable referrer. A separate image signature is unnecessary: the provenance attestation already
binds the digest to the builder identity. Consumers verify by digest with `gh attestation verify`
(see the [README](../../README.md#verifying-the-image)).

**Authentication.** A publish holds no long-lived registry credential. It authenticates to GHCR with
the ephemeral, repository-scoped `GITHUB_TOKEN` (`packages: write`), which lives only for the job's
duration and reaches no other repository. It signs the attestations through GitHub OIDC
(`id-token: write` plus `attestations: write`), with no stored key. The full build-push-attest chain
runs on a `vX.Y.Z` tag or a `workflow_dispatch`, behind
[the release environment](#the-release-environment).

## Vulnerability scanning and dependency freshness

Three arms keep the shipped closure honest: C-closure detection, Haskell-closure detection, and
freshness.

**Detection, `grype` (the C-closure authority).** `task scan` builds the sbomnix SBOM of the
application closure into `sbom/` and runs `grype`. It writes the severity-rated findings as
`grype.sarif`, with a table in the log. `task scan-vulnix` is a secondary
[vulnix](https://github.com/flyingcircusio/vulnix) cross-check: broader and Nix-patch-aware, but
un-graded, so not the authority. A naive closure scan with distro-advisory matchers reports about a
thousand mostly-irrelevant CVEs. The grype-over-SBOM view is the curated one. Both scanners come from
the single pinned nixpkgs (26.05).

The [`security.yml`](../../.github/workflows/security.yml) workflow is report-only and never gates a
PR, because a `flake.lock` bump fixes the closure, not an in-PR change. Both of its jobs upload SARIF
to GitHub code scanning (categories `grype` and `osv-hsec`). Triage therefore happens in the Security
tab alongside Semgrep and Scorecard, so the issue tracker holds only human-filed work. An alert
closes itself once a later scan no longer reports it. On a PR the workflow runs only when the
dependency plan changes. On a daily schedule it scans `main`, so CVEs disclosed after a release still
surface.

**Freshness, Renovate.** [`renovate.json5`](../../.github/renovate.json5) runs one bot across the
ecosystems the repo automates: flake inputs, GitHub Actions, and Hackage cabal dependencies.
Renovate's `nix` manager is beta and off by default, so the config enables it explicitly. Without
that opt-in the weekly refresh does not run at all.

A version-based update also waits seven days from its publication before Renovate proposes it
(`minimumReleaseAge`). Renovate withholds it outright rather than raise a PR that reports itself as
pending. A release yanked shortly after it ships therefore never reaches a branch here. A fix PR
raised from a vulnerability alert skips that wait, because for a known-vulnerable dependency the
delay is the greater risk.

The weekly `flake.lock` refresh is the single freshness lever. The flake pins the package set that
supplies both the image's C-library closure and every Haskell dependency, and `cabal.project.freeze`
is *generated* from that set (`task freeze`). The `freeze-sync` flake check fails CI whenever the
committed freeze drifts. That refresh sits outside the quarantine. A lock bump carries no publication
dates to age out, so the flake's branch inputs move on the weekly schedule alone. The gate validates
each bump and the scan re-runs on it. Fixing a finding is usually merging the Renovate PR, plus one
`task freeze` commit when Haskell versions moved.

**Detection, OSV/HSEC (the Haskell-closure authority).** HSEC advisories (the Haskell Security
Response Team database) are exported to [OSV.dev](https://osv.dev). The default GitHub Advisory
Database has no Hackage ecosystem and never sees them. The `osv-freeze` job in
[`security.yml`](../../.github/workflows/security.yml) runs
[osv-scanner](https://google.github.io/osv-scanner/), from the pinned nixpkgs like every other scan
tool, over every exact pin in `cabal.project.freeze` (`task scan-osv` locally). The freeze mirrors
the Nix set, so a match describes exactly the closure the shipped image is built from, statically
linked Haskell libraries included. No scan of the image itself can see those libraries. Findings
upload as SARIF under the `osv-hsec` code-scanning category.

The scans report every finding. The repo hardcodes no ignore list, and acceptance or dismissal
happens in GitHub's security surfaces. Detection is not remediation. The fix for a Haskell advisory
is a flake-side bump (`flake.lock` or an overlay pin), then `task freeze`. Never hand-edit the
generated freeze. Renovate's experimental `osvVulnerabilityAlerts` stays enabled as an uncredited
second net. It has raised nothing against pinned packages so far, which is why the scheduled scan,
whose runs are observable, is the arm of record.

## Posture scoring, OpenSSF Scorecard

[`scorecard.yml`](../../.github/workflows/scorecard.yml) runs OpenSSF Scorecard weekly and on
branch-protection changes. It grades the repository's supply-chain posture: branch protection, pinned
dependencies, signed and attested releases, SAST, token permissions, and dangerous workflow patterns.
It uploads findings to the Security tab and publishes the score that backs the README badge. It is
report-only and never gates a PR. For a supply-chain policy proxy this is dogfooding: the same
hygiene it proxies for, measured on itself.
