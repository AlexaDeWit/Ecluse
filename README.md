# Écluse

[![OpenSSF Scorecard](https://api.scorecard.dev/projects/github.com/AlexaDeWit/Ecluse/badge)](https://scorecard.dev/viewer/?uri=github.com/AlexaDeWit/Ecluse)
[![OpenSSF Best Practices](https://www.bestpractices.dev/projects/13335/badge)](https://www.bestpractices.dev/projects/13335)
[![codecov](https://codecov.io/gh/AlexaDeWit/Ecluse/branch/main/graph/badge.svg?token=1TWB5HBQ0S)](https://codecov.io/gh/AlexaDeWit/Ecluse)

![Écluse: a supply-chain policy proxy for package registries](docs/social-preview.png)

A supply-chain policy proxy for package registries, written in Haskell. The name
**Écluse** (*Quebec French:* "ayy-cluze", [e.klyz]) is French for a canal lock: the
controlled passage every dependency clears before it reaches your build.

Start with [Why Écluse? (`MOTIVATION.md`)](MOTIVATION.md) for the problem and the design
reasoning, and [`ALTERNATIVES.md`](ALTERNATIVES.md) for other tools in this space.
[Verifying the image](#verifying-the-image) covers how to verify a release's build
provenance (its keyless provenance and SBOM attestations, and the bit-for-bit
reproducible rebuild) rather than trusting it.

> **Status: pre-launch, no GA release yet.** The npm packument, tarball, and publish paths
> run today, and an AWS-backed deployment (SQS mirror queue, demand-driven worker, writing
> under a container-role credential) is wired end to end. The GCP backends and the
> deployment runbook are still to come. Release candidates are published and attested;
> expect breaking changes before `v0.1.0`. [`USAGE.md`](USAGE.md) is the deployment
> contract.

[Haddock API docs](https://ecluse-proxy.com/api/) auto-publish from `main`.

## Overview

`ecluse` sits between your build (or CI) and the upstream registry and applies a
deny-by-default policy before any package is served. It checks a private upstream first,
falls back to the public registry with rules applied, and mirrors approved packages
asynchronously, without hosting packages itself. The serve path is capacity-bounded:
metadata requests are admitted up to a process-wide limit, and excess load is shed rather
than queued. npm is the first supported ecosystem; the core is registry-agnostic, with PyPI
and RubyGems on the roadmap.

[`docs/architecture.md`](docs/architecture.md) has the full design: the four-role registry
model, the rules engine, the mirror queue, and the configuration reference. The threat model
(OWASP Threat Dragon, STRIDE) is generated into a readable
[register](https://ecluse-proxy.com/threat-model.html) from
[`threat-modelling/ecluse.json`](threat-modelling/ecluse.json).

## Using Écluse

[`USAGE.md`](USAGE.md) is the operator manual: configuration, connecting your clients, the
network-egress safety you're responsible for, the rule policy, and the health and
observability endpoints. The [`docs/architecture/`](docs/architecture.md) documents are the
*why* behind each setting.

## Verifying the image

> **Pre-release.** No GA release is cut yet. Release candidates (e.g. `0.1.0-rc.2`) are
> published to Docker Hub and already carry the attestations below.

Each tag is a single multi-arch image (`linux/amd64` + `linux/arm64`) carrying keyless
(Sigstore) provenance and SBOM attestations in the public Rekor log. A cut release's digest
is published in the [GitHub Release](https://github.com/AlexaDeWit/Ecluse/releases); until
then, pin by digest. Verify with the GitHub CLI:

```bash
IMAGE=ghcr.io/alexadewit/ecluse@sha256:…   # pin by digest

# Verify every attestation (provenance + SBOM) against the release identity + Rekor:
gh attestation verify "oci://$IMAGE" --repo AlexaDeWit/Ecluse

# …or just one, by predicate type:
gh attestation verify "oci://$IMAGE" --repo AlexaDeWit/Ecluse \
  --predicate-type https://slsa.dev/provenance/v1
```

This checks each signature against the release workflow's identity and the Rekor log, and
that the subject matches your digest. Add `--format json` to extract the documents.

Stronger still, the image is bit-for-bit reproducible: rebuild it from pinned source and
compare, rather than trust anyone.

```bash
nix build github:AlexaDeWit/Ecluse/<ref>#dockerImage   # → ./result (a docker-archive)
```

See [Release & Supply-Chain Operations](docs/architecture/release-supply-chain.md#supply-chain-attestations).

## Versioning

Écluse follows [semantic versioning](https://semver.org) against the operator-facing
contract (the `PROXY_*` configuration and the proxy's behaviour), not the Haskell module
API. The version lives in `ecluse.cabal`'s `version:` field; the image, git, and release
tags derive from it. While it's `0.y.z` the contract is unstable: pin an exact version by
digest and expect breaking changes. [`VERSIONING.md`](VERSIONING.md) is the full policy.

## Development

[Nix](https://nixos.org/) with flakes is a hard dependency: the whole toolchain (GHC 9.10,
Cabal, fourmolu, hlint, Semgrep) comes from the pinned dev shell.

```bash
nix develop        # enter the dev shell (direnv does this automatically)
task build         # build the library, executable, and tests
task check         # fast pre-push checks (a subset of the gate)
task gate          # the full CI-gate mirror (adds the Docker integration + Haddock tiers)
```

[Getting Started](docs/getting-started.md) covers full setup, the `task` workflow, and
dependency locking. [`CONTRIBUTING.md`](CONTRIBUTING.md) covers the contribution process and
DCO sign-off; participation is governed by the [Code of Conduct](CODE_OF_CONDUCT.md).

## Project structure

| Path        | Purpose                                                                                                                  |
| ----------- | ------------------------------------------------------------------------------------------------------------------------ |
| `core/`     | `ecluse-core` library: the pure, ecosystem-agnostic capability core (`Ecluse.Core.*`)                                    |
| `runtime/`  | `ecluse-runtime` library: the effectful edge — OTel SDK, warp, scribes, and cloud adapters (`Ecluse.Runtime.*`)          |
| `src/`      | `ecluse` library: the composition shell that assembles and runs the tiers (`Ecluse.*`)                                  |
| `app/`      | Executable entry point, thin wiring only                                                                                  |
| `test/`     | Unit and integration tests                                                                                               |
| `docs/`     | Architecture and design documents                                                                                        |
| `flake.nix` | Nix dev shell (GHC 9.10, cabal, HLS, ghcid) and the package build (`nix build`) plus hermetic checks (`nix flake check`) |
