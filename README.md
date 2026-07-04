# Écluse

[![OpenSSF Scorecard](https://api.scorecard.dev/projects/github.com/AlexaDeWit/Ecluse/badge)](https://scorecard.dev/viewer/?uri=github.com/AlexaDeWit/Ecluse)
[![OpenSSF Best Practices](https://www.bestpractices.dev/projects/13335/badge)](https://www.bestpractices.dev/projects/13335)
[![codecov](https://codecov.io/gh/AlexaDeWit/Ecluse/branch/main/graph/badge.svg?token=1TWB5HBQ0S)](https://codecov.io/gh/AlexaDeWit/Ecluse)

![Écluse: a supply-chain policy proxy for package registries](docs/social-preview.png)

A supply-chain policy proxy for package registries, written in Haskell. The name
**Écluse** (*Quebec French:* "ayy-cluze", [e.klyz]) is French for a canal lock: the
controlled passage every dependency clears before it reaches your build.

New here? Start with [Why Écluse? (`MOTIVATION.md`)](MOTIVATION.md), the *why*: the problem,
why the off-the-shelf options didn't fit, and the reasoning behind the design. A fair guide
to the other tools in this space is in [`ALTERNATIVES.md`](ALTERNATIVES.md).

Built with AI: I've leaned on an LLM heavily for the implementation during this
bootstrapping phase, behind a documented review process and a strict CI gate. See
[`AI-DISCLOSURE.md`](AI-DISCLOSURE.md) for what's mine, what the AI did, and how to verify
it rather than trust it.

> **Status: pre-launch, under active development, no GA release yet.** The functional core
> and the npm packument, tarball, and publish paths run today, and an AWS-backed deployment
> (an SQS mirror queue with a demand-driven worker, writing under a container-role
> credential) is wired end to end. The GCP backends and the deployment runbook are still to
> come. Pre-release candidates are published and attested, but expect breaking changes
> before `v0.1.0`. [`USAGE.md`](USAGE.md) is the deployment contract: what is wired today.

API documentation: [Haddock for the library](https://alexadewit.github.io/Ecluse/api/), auto-published from `main`.

## Overview

`ecluse` sits between your build (or CI) and the upstream registry, and applies a
deny-by-default policy before any package reaches a build. It proxies requests through a
private upstream first, falls back to the public registry with rules applied, and mirrors
approved packages asynchronously, without hosting packages itself. npm is the first
supported ecosystem; the core is registry-agnostic, and PyPI and RubyGems are on the
roadmap. The serve path is capacity-bounded: metadata-bearing requests are admitted up to a
configurable process-wide limit, and excess load is shed promptly instead of building an
unbounded queue.

See [`docs/architecture.md`](docs/architecture.md) for the full design: the four-role registry
model, the deny-by-default rules engine, the mirror queue, and the configuration reference.

The system's threat model (OWASP Threat Dragon, STRIDE) is the single source of truth for
its risks, published as a readable
[register](https://alexadewit.github.io/Ecluse/threat-model.html) generated from
[`threat-modelling/ecluse.json`](threat-modelling/ecluse.json) on every build. Record threats
in the model, not in prose.

## Using Écluse

Deploying or operating Écluse? Start with the [operator manual (`USAGE.md`)](USAGE.md). It's
the consumer-facing reference: configuration (environment variables and the config document),
connecting your clients, the network-egress safety you're responsible for, the rule policy,
and the health and observability endpoints. The [`docs/architecture/`](docs/architecture.md)
documents are the *why* behind each setting.

## Verifying the image

> **Pre-release.** No GA release is cut yet. Pre-release candidates (e.g. `0.1.0-rc.2`) are
> published to Docker Hub and already carry the attestations below, so this recipe works
> against an RC today. Expect breaking changes before `v0.1.0`.

Each published tag is a single multi-arch image (`linux/amd64` + `linux/arm64`). Every image
carries keyless (Sigstore) provenance and SBOM attestations, recorded in the public Rekor
transparency log. Once a release is cut its digest is published in the
[GitHub Release](https://github.com/AlexaDeWit/Ecluse/releases); until then, pin a tag by
digest. Verify by digest with the GitHub CLI:

```bash
IMAGE=ghcr.io/alexadewit/ecluse@sha256:…   # pin by digest

# Verify every attestation (provenance + SBOM) against the release identity + Rekor:
gh attestation verify "oci://$IMAGE" --repo AlexaDeWit/Ecluse

# …or just one, by predicate type:
gh attestation verify "oci://$IMAGE" --repo AlexaDeWit/Ecluse \
  --predicate-type https://slsa.dev/provenance/v1
```

`gh attestation verify` checks each attestation's signature against the release workflow's
identity and the Rekor log, and that its subject matches the digest you pulled. Add
`--format json` to extract the documents (e.g. the SPDX SBOM).

The strongest check is reproducibility: the image is bit-for-bit reproducible, so rather
than trust anyone, rebuild it from pinned source and compare it to what you pulled. Pin a
release tag once one is cut; until then, use a branch or commit ref:

```bash
nix build github:AlexaDeWit/Ecluse/<ref>#dockerImage   # → ./result (a docker-archive)
```

See [Release & Supply-Chain Operations](docs/architecture/release-supply-chain.md#supply-chain-attestations)
for how the attestations are produced.

## Versioning

Écluse follows [semantic versioning](https://semver.org): `MAJOR.MINOR.PATCH` against the
operator-facing contract (the `PROXY_*` configuration and the proxy's behaviour), not the
Haskell module API. The version lives in one place, `ecluse.cabal`'s `version:` field, and the
image tag, git tag, and GitHub Release all derive from it. While the version is `0.y.z` the
contract is not yet stable, so pin an exact version (by digest) and expect breaking changes in
any release. [`VERSIONING.md`](VERSIONING.md) is the full policy: what each number means, how
release candidates work, and how a release is cut.

## Development

[Nix](https://nixos.org/) with flakes is a hard dependency: the whole toolchain (GHC 9.10,
Cabal, fourmolu, hlint, Semgrep) comes from the pinned dev shell. Get productive in three
commands:

```bash
nix develop        # enter the dev shell (direnv does this automatically)
task build         # build the library, executable, and tests
task check         # fast pre-push checks (a subset of the gate)
task gate          # the full CI-gate mirror (adds the Docker integration + Haddock tiers)
```

Full setup, the `task` workflow, reproducible and hermetic builds, and dependency locking
are in [Getting Started](docs/getting-started.md). The contribution process (conventions, DCO
sign-off, and the AI-assistance policy) is in [`CONTRIBUTING.md`](CONTRIBUTING.md); all
participation is governed by the [Code of Conduct](CODE_OF_CONDUCT.md).

## Project structure

| Path        | Purpose                                                                                                                  |
| ----------- | ------------------------------------------------------------------------------------------------------------------------ |
| `core/`     | `ecluse-core` library: the pure, ecosystem-agnostic capability core (`Ecluse.Core.*`)                                    |
| `src/`      | `ecluse` library: the application shell that composes the core into a running proxy (`Ecluse.*`)                         |
| `app/`      | Executable entry point, thin wiring only                                                                                  |
| `test/`     | Unit and integration tests                                                                                               |
| `docs/`     | Architecture and design documents                                                                                        |
| `flake.nix` | Nix dev shell (GHC 9.10, cabal, HLS, ghcid) and the package build (`nix build`) plus hermetic checks (`nix flake check`) |
