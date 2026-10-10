# Release and supply-chain operations

> Part of the [Écluse architecture overview](../architecture.md).

How Écluse is built into a container image, published, attested, and scanned. This document is the
operational detail behind [`CONTRIBUTING.md`](../../CONTRIBUTING.md), which holds the
contributor-facing summary and the `task` targets. The consumer-side verify recipe is in the
[README](../../README.md#verifying-the-image).

## Releases and container image

Écluse ships as an OCI image that Nix builds (`dockerTools.buildLayeredImage`, see
[`flake.nix`](../../flake.nix)), not a Dockerfile. The image holds the `ecluse` executable, its
runtime closure, and CA certificates: no shell, no package manager, and none of the repository's
development tools. It runs non-root (uid 65532). The flake's lock file pins its inputs. Build it
locally with `task docker-build`, which writes `./result`, a `docker-archive`.

`ecluse` is the only program of its own that the image holds. Library packages in the runtime
closure keep the programs that nixpkgs puts in their own store paths, such as glibc's `getconf`
helpers and the `numactl` tools beside libnuma.

Every image build runs [`image-archive.sh`](../../scripts/image-archive.sh), which refuses a
redundant Écluse program. A program is a file or link under a `bin`, `sbin`, or `libexec`
directory. The image must hold `bin/ecluse` in exactly one `ecluse-<version>` store path. That
store path must hold no other program, and the image's root must hold none but its `/bin/ecluse`
link. The check does not read the store paths of other packages.

Publishing is a separate, tag-triggered workflow
([`release.yml`](../../.github/workflows/release.yml)), never part of the PR `gate`. A `vX.Y.Z` tag
must match `ecluse.cabal`'s `version:` field, or the release fails fast at a verify-version step. On
a match the workflow builds the image natively for `linux/amd64` and `linux/arm64` (see
[Multi-architecture image](#multi-architecture-image)).

The build is the reusable workflow
[`release-build.yml`](../../.github/workflows/release-build.yml), and CI's release dry-run runs the
same definition. Every commit on `main`, and every pull request that can change the build, builds
both images, assembles the multi-arch index, and starts each image on its own architecture, without
a registry login or a push. A release is therefore never the first build of an architecture.

The workflow assembles the two into one multi-arch index and pushes it to GitHub Container Registry
under a single immutable tag. It attaches keyless provenance and SBOM attestations. It then
publishes a GitHub Release carrying the image digest, the `gh attestation verify` recipe, the
generated changelog, and every attestation and SBOM as a downloadable asset. A pre-release tag
(`vX.Y.Z-rc.N`) publishes as a prerelease. GHCR is the only registry Écluse publishes to.

**Two builds of one commit.** The CI run that holds the release dry-run builds each image a second
time. The two builds run on separate runners and share no cache and no Nix store. The
`release-compare` job then compares the SHA-256 of the two archives for each architecture. A pass
shows that those two builds of that commit gave the same image archive. It is evidence and not a
guarantee for every build, because GHC 9.10 does not promise deterministic object code. GHC 9.10
orders its object code differently from build to build when it compiles modules in parallel, so the
flake compiles the image's `ecluse` binary and each Haskell library it builds from source one module
at a time. A release's own build is a third build, and no job compares it with the other two. The
job reports its result and does not gate a merge.

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

The workflow retains both image archives and both SBOMs for seven days from each upload. The
dry-run keeps its copies for one day.
That covers the three-day wait and a nominal four-day approval margin.
Uneven build completion, runner queues, and publish setup consume part of that margin.
Before approval, check the run's artifact expiry times and leave enough time for `publish` to download all four inputs.
Keep retention and the environment wait timer aligned if either changes.

## Multi-architecture image

`ecluse:X.Y.Z` is an OCI index over a `linux/amd64` and a `linux/arm64` image, so a consumer pulls
one tag and the registry serves the right architecture. Each architecture builds natively on its
own runner. [`assemble-multiarch.sh`](../../scripts/assemble-multiarch.sh) assembles the index and
checks that it lists exactly those two platforms, and the publish job pushes it
([`push-multiarch.sh`](../../scripts/push-multiarch.sh)). The release dry-run runs the same assembly
and stops before the push.

**The image builds take no cache.** Both build jobs install Nix with the plain installer and restore
nothing from the GitHub Actions cache, unlike the rest of CI, which sets up through
[`setup-toolchain`](../../.github/actions/setup-toolchain/action.yml). The Actions cache is
repository-scoped, writable by any run with `main` scope, and carries no signature over its
contents. A Nix store restored from it arrives already realised, which bypasses the substituter
signature check that would otherwise reject a tampered store path, so a poisoned cache entry ships a
poisoned image under an honest-looking provenance attestation. Building cold from
`cache.nixos.org`, whose signatures Nix verifies against the trusted public key, and from the tagged
source is what keeps the attestation worth something. A release pays for that in build minutes,
which is affordable on a path that runs a few times a year.

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
  It carries no dynamic-build noise to trip CVE scanners, and anyone can derive it again from the
  same commit, because the flake's lock file pins that closure.

**Each attestation has two homes.** It goes to GHCR as an OCI referrer, and onto the GitHub Release
as an asset ([`release-assets.sh`](../../scripts/release-assets.sh) stages them). `gh attestation
verify` reads neither by default: it queries GitHub's attestations API, and takes the referrer only
under `--bundle-from-oci`. The asset is the copy pinned to the release object, fetchable over plain
HTTPS, and `--bundle` verifies it against the same digest. It is also the only copy OpenSSF
Scorecard can see (see [Posture scoring](#posture-scoring-openssf-scorecard)). The release carries
each platform's SPDX document unwrapped as well, because the attested copy only comes out through a
verify tool. Asset names are `ecluse-<version>-provenance.sigstore.json` for the index, then
`ecluse-<version>-<arch>-provenance.sigstore.json`, `ecluse-<version>-<arch>-sbom.sigstore.json`,
and `ecluse-<version>-<arch>-sbom.spdx.json`.

`gh attestation verify` checks one predicate type per run and defaults to SLSA provenance, so the
SBOM needs `--predicate-type https://spdx.dev/Document/v2.3` and a platform digest. The index digest
carries no SBOM attestation.

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

## Vulnerability scanning and dependency updates

Three arms keep the shipped closure honest: C-closure detection, Haskell-closure detection, and
dependency updates.

**Detection, `grype` (the C-closure authority).** `task scan` builds the sbomnix SBOM of the
application closure into `sbom/` and runs `grype` over it (`task scan-sbom`). It writes the
severity-rated findings as `grype.sarif`, with a table in the log. `task scan-vulnix` is a secondary
[vulnix](https://github.com/flyingcircusio/vulnix) cross-check: broader and Nix-patch-aware, but
un-graded, so not the authority. A naive closure scan with distro-advisory matchers reports about a
thousand mostly-irrelevant CVEs. The grype-over-SBOM view is the curated one. Both scanners come from
the single pinned nixpkgs (26.05).

The grype scan and the OSV/HSEC scan below are report-only and never gate a PR, because a
`flake.lock` bump fixes the closure, not an in-PR change. The `grype` job in
[`ci.yml`](../../.github/workflows/ci.yml) runs in the nightly run and on a manual dispatch. It
scans each architecture's CycloneDX SBOM from that run's release dry-run, because each architecture
has its own C closure. The `osv-freeze` job in [`security.yml`](../../.github/workflows/security.yml)
runs daily on `main` and on a PR that changes the dependency plan. The daily runs mean CVEs
disclosed after a release still surface. Both jobs upload SARIF to GitHub code scanning, under the
categories `grype` (arm64), `grype-amd64`, and `osv-hsec`. Triage happens in the Security tab
alongside Semgrep and Scorecard, so the issue tracker holds only human-filed work. An alert closes
itself once a later scan no longer reports it.

**Dependency updates, Renovate.** [`renovate.json5`](../../.github/renovate.json5) runs one bot across the
ecosystems the repo automates: flake inputs, GitHub Actions, and Hackage cabal dependencies.
Renovate's `nix` manager is beta and off by default, so the config enables it explicitly. Without
that opt-in the weekly refresh does not run at all.

A version-based update also waits seven days from its publication before Renovate proposes it
(`minimumReleaseAge`). Renovate withholds it outright rather than raise a PR that reports itself as
pending. A release yanked shortly after it ships therefore never reaches a branch here. A fix PR
raised from a vulnerability alert skips that wait, because for a known-vulnerable dependency the
delay is the greater risk.

The weekly `flake.lock` refresh is the single dependency-update lever. The flake pins the package set that
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

Its `Signed-Releases` check reads the assets on a GitHub Release. It never looks at a registry, so
attestations that live only as OCI referrers are invisible to it. That is one reason each release
carries its bundles as assets too (see [Supply-chain attestations](#supply-chain-attestations)).
