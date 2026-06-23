# Écluse

[![OpenSSF Scorecard](https://api.scorecard.dev/projects/github.com/AlexaDeWit/Ecluse/badge)](https://scorecard.dev/viewer/?uri=github.com/AlexaDeWit/Ecluse)
[![OpenSSF Best Practices](https://www.bestpractices.dev/projects/13335/badge)](https://www.bestpractices.dev/projects/13335)
[![codecov](https://codecov.io/gh/AlexaDeWit/Ecluse/branch/main/graph/badge.svg?token=1TWB5HBQ0S)](https://codecov.io/gh/AlexaDeWit/Ecluse)

![Écluse — a supply-chain resilience proxy for package registries](docs/social-preview.png)

A supply-chain resilience proxy for package registries, written in Haskell. The name —
**Écluse** (*Quebec French:* "ayy-cluze", [e.klyz]) — is French for a canal lock: the
controlled passage every dependency clears before it reaches your build.

**New here?** [**Why Écluse?** (`MOTIVATION.md`)](MOTIVATION.md) is the *why* — the problem,
why the off-the-shelf options didn't fit, and the reasoning behind the design; a fair guide
to other tools in this space is in [`ALTERNATIVES.md`](ALTERNATIVES.md).

> **Status: pre-launch — under active development; no GA release yet.** The
> functional core and the npm **packument** path are in place; the **tarball**
> path, the **mirror worker**, and the **cloud backends** are still landing, so
> Écluse is not yet a proxy you can put in front of a build. Pre-release candidates
> are published and attested, but expect breaking changes before `v0.1.0`. What has
> actually shipped is tracked slice by slice in the
> [delivery plan](planning/delivery-plan.md) (the per-slice `status:` is the source
> of truth); [`USAGE.md`](USAGE.md) is the deployment **contract**, not a claim
> that every capability is wired today.

**API documentation:** [Haddock for the library](https://alexadewit.github.io/Ecluse/), auto-published from `main`.

## Overview

`ecluse` sits between your development environment (or CI) and the npm
registry, enforcing a configurable resilience policy before any package reaches a
build. It proxies requests through a private upstream first, falls back to the
public npm registry with rules applied, and mirrors approved packages
asynchronously — without hosting packages itself.

See [`docs/architecture.md`](docs/architecture.md) for the full design:
three-registry model, deny-by-default rules engine, mirror queue, and
configuration reference.

## Using Écluse

**Deploying or operating Écluse? Start with the [Operator Manual
(`USAGE.md`)](USAGE.md).** It is the consumer-facing reference: configuration
(environment variables and the config document), connecting your clients, the
**network-egress safety** you are responsible for, the rule policy, and the
health/observability endpoints — surfaced out of the internal architecture docs so
operators find them. The `docs/architecture/` documents remain the home for the
*why* behind each setting.

## Verifying the image

> **Pre-release.** No GA release is cut yet — no
> [GitHub Releases](https://github.com/AlexaDeWit/Ecluse/releases) and no `v0.1.0`
> tag. Pre-release candidates (e.g. `0.1.0-rc.2`) are published to Docker Hub and
> **already carry the attestations below**, so this recipe works against an RC today
> (expect breaking changes before `v0.1.0`). It is the verification contract every
> released image will meet.

Every published image carries **provenance** and **SBOM** attestations — keyless
(Sigstore), recorded in the public Rekor transparency log, and stored as
**immutable OCI referrers** on the image (write-once; they can't be overwritten).
Once a release is cut, each version's digest will be published in its
[GitHub Release](https://github.com/AlexaDeWit/Ecluse/releases); until then, pin a
published tag by digest. Verify by **digest** with the GitHub CLI:

```bash
IMAGE=alexadewit/ecluse@sha256:…   # pin by digest

# Verify every attestation (provenance + SBOM) against the release identity + Rekor:
gh attestation verify "oci://$IMAGE" --repo AlexaDeWit/Ecluse

# …or just one, by predicate type:
gh attestation verify "oci://$IMAGE" --repo AlexaDeWit/Ecluse \
  --predicate-type https://slsa.dev/provenance/v1
```

`gh attestation verify` checks each attestation's signature against the release
workflow's identity and the Rekor log, and that its subject matches the digest
you pulled. Add `--format json` to extract the documents (e.g. the SPDX SBOM).

Strongest of all, the image is **bit-for-bit reproducible** — rather than trust
anyone, rebuild it from the pinned source and compare to what you pulled (pin a
release tag once one is cut; until then, a branch or commit ref):

```bash
nix build github:AlexaDeWit/Ecluse/<ref>#dockerImage   # → ./result (a docker-archive)
```

See [Release & Supply-Chain Operations](docs/architecture/release-supply-chain.md#supply-chain-attestations)
for how the attestations are produced.

## Development

**[Nix](https://nixos.org/) with flakes is a hard dependency** — the whole
toolchain (GHC 9.10, Cabal, fourmolu, hlint, Semgrep) comes from the pinned dev
shell. Get productive in three commands:

```bash
nix develop        # enter the dev shell (direnv does this automatically)
make build         # build the library, executable, and tests
make check         # everything the CI gate runs
```

Full setup, the `make` workflow, reproducible/hermetic builds, and dependency
locking are in **[Getting Started](docs/getting-started.md)**. The contribution
process — conventions, DCO sign-off, and the AI-assistance policy — is in
[`CONTRIBUTING.md`](CONTRIBUTING.md); all participation is governed by our
[Code of Conduct](CODE_OF_CONDUCT.md).

## Project Structure

| Path        | Purpose                                                                                                                  |
| ----------- | ------------------------------------------------------------------------------------------------------------------------ |
| `app/`      | Executable entry point — thin wiring only                                                                                |
| `src/`      | Library — all business logic                                                                                             |
| `test/`     | Unit and integration tests                                                                                               |
| `docs/`     | Architecture and design documents                                                                                        |
| `flake.nix` | Nix dev shell (GHC 9.10, cabal, HLS, ghcid) **and** the package build (`nix build`) + hermetic checks (`nix flake check`) |
