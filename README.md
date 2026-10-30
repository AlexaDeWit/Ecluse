# Écluse

[![OpenSSF Scorecard](https://api.scorecard.dev/projects/github.com/AlexaDeWit/Ecluse/badge)](https://scorecard.dev/viewer/?uri=github.com/AlexaDeWit/Ecluse)
[![OpenSSF Best Practices](https://www.bestpractices.dev/projects/13335/badge)](https://www.bestpractices.dev/projects/13335)
[![codecov](https://codecov.io/gh/AlexaDeWit/Ecluse/branch/main/graph/badge.svg?token=1TWB5HBQ0S)](https://codecov.io/gh/AlexaDeWit/Ecluse)

![Écluse: control which packages reach your builds](docs/branding/banner.png)

Écluse is a self-hosted package policy proxy for npm, PyPI, Cargo, and RubyGems. A central platform,
DevOps, or IT team runs it for the organisation's developer teams and CI.
Clients use the company registry address while Écluse decides which public package versions
they can install.

The name **Écluse** (*Quebec French:* "ayy-cluze", [e.klyz]) is French for a canal lock.

**Status: generally available, pre-1.0.0.** Configuration can change before `v1.0.0`.
The [operator manual](https://ecluse-proxy.com/docs/) defines the supported deployment contract.

## Overview

Écluse applies policy before public package versions enter a build or a configured mirror.
Registries hold the package archives. Écluse does not host them.

- **New public releases wait seven days by default.** Age does not prove a package is harmless.
  The delay reduces exposure to releases removed during that window.
- **Advisory checks are optional.** Configuring an advisory source activates known-malicious
  denial and early admission for qualifying exact security fixes. Higher-ranked rules, evidence
  freshness, and remaining advisories still affect the result.
- **npm denies install scripts and dependencies fetched from outside the registry by default.**
  The second covers git sources, remote tarball URLs and unsupported schemes such as `jsr:`.
  These admission rules also apply without an advisory source. Scored CVSS and EPSS denies are
  separate opt-in rules.
- **Private packages use the trusted path.** They bypass public admission policy, while caller
  authorisation and applicable integrity and completion checks still apply.
- **Mirroring keeps admitted public versions in your registry.** A complete copy can remain
  available during a public outage or after upstream removal.
- **Cleanup removes stored versions denied at revocation.** It requires a supported backend,
  independent authority and consent for each target, and stays within a deletion cap.

The [security model](https://ecluse-proxy.com/docs/security/) explains the limits and links to
the rendered threat model. Network access control remains the operating team's responsibility.
Pointing clients at Écluse alone does not prevent direct downloads around it.

## Using Écluse

Choose the guide for your task:

| Task | Start here |
| --- | --- |
| Decide whether Écluse fits | [Understand Écluse](https://ecluse-proxy.com/docs/understand/) |
| Deploy it for your organisation | [Set up Écluse](https://ecluse-proxy.com/docs/setup/) |
| Embed configuration in a derived image | [Package your configuration](https://ecluse-proxy.com/docs/configure/) |
| Investigate a request or policy interaction | [Detailed behaviour](https://ecluse-proxy.com/docs/behaviour/) |
| Review security assumptions and controls | [Rendered threat model](https://ecluse-proxy.com/docs/threat-model/) |
| Read or extend the code | [Architecture](docs/architecture.md) and [API reference](https://ecluse-proxy.com/api/) |

Images publish to `ghcr.io/alexadewit/ecluse`, with an immutable tag for each version and no
`latest` tag. Pin production deployments by digest and verify the base image before building
your configured derivative.

## Verifying the image

> **Releases.** The release workflow publishes every version to GitHub Container Registry
> (`ghcr.io/alexadewit/ecluse`).

Each tag is a single multi-arch image (`linux/amd64` + `linux/arm64`) carrying keyless
(Sigstore) provenance and SBOM attestations in the public Rekor log. Each
[GitHub Release](https://github.com/AlexaDeWit/Ecluse/releases) publishes the digest for its
version and attaches those attestations as assets. Verify with the GitHub CLI:

```bash
IMAGE=ghcr.io/alexadewit/ecluse@sha256:…   # pin by digest, the index

# Provenance on the index. `verify` checks one predicate type per run, and this is
# its default, so the flag is optional:
gh attestation verify "oci://$IMAGE" --repo AlexaDeWit/Ecluse \
  --predicate-type https://slsa.dev/provenance/v1

# The SBOM is attested per platform, never on the index, so name a platform digest
# (`docker manifest inspect` lists them) and the SPDX predicate type:
gh attestation verify "oci://ghcr.io/alexadewit/ecluse@<platform-digest>" \
  --repo AlexaDeWit/Ecluse --predicate-type https://spdx.dev/Document/v2.3
```

The command checks the signature against the release workflow's identity and the Rekor
log, and confirms the subject matches your digest. Add `--format json` to extract the
documents.

Both commands read GitHub's attestations API. Each release also attaches the same
documents as assets, so you can verify a copy pinned to the release object instead:

```bash
gh release download v<version> -p 'ecluse-*-provenance.sigstore.json'
gh attestation verify "oci://$IMAGE" --repo AlexaDeWit/Ecluse \
  --bundle ecluse-<version>-provenance.sigstore.json
```

A `…-sbom.sigstore.json` bundle needs `--predicate-type` as well. The two
`ecluse-<version>-<arch>-sbom.spdx.json` assets are the SPDX documents themselves, not
bundles: read those directly, and pass `--bundle` the matching `…-sbom.sigstore.json` to
check the signature over one.

Nix builds the image from pinned source, so you can also rebuild it yourself.

```bash
nix build github:AlexaDeWit/Ecluse/<ref>#dockerImage   # → ./result (a docker-archive)
```

See [Release and supply-chain operations](docs/architecture/release-supply-chain.md#supply-chain-attestations).

## Versioning

The version lives in `ecluse.cabal`'s `version:` field, and the image, git, and release tags
derive from it. [`VERSIONING.md`](VERSIONING.md) is the policy: what the numbers promise, and
what `0.y.z` does not.

## Development

[Nix](https://nixos.org/) with flakes is a hard dependency: the whole toolchain (GHC 9.10,
Cabal, fourmolu, hlint, Semgrep) comes from the pinned dev shell.

```bash
nix develop        # enter the dev shell (direnv does this automatically)
task build         # build the library, executable, and tests
task check         # pre-push checks: build, coverage, static checks, weeder, stan (needs Docker)
task gate          # task check, then the integration suite and the Haddock build
```

`task check` runs the unit and integration suites under coverage, so it needs a running Docker
daemon and starts the integration containers. `task gate` is not the whole CI gate. It omits the
end-to-end tier, the allocation budgets, the release dry-run, and the two dependency-lock checks
that `task nix-check` runs.

[Getting Started](docs/getting-started.md) covers full setup, the `task` workflow, and
dependency locking. [`CONTRIBUTING.md`](CONTRIBUTING.md) covers the contribution process and
DCO sign-off. The [Code of Conduct](CODE_OF_CONDUCT.md) governs participation, and
[`GOVERNANCE.md`](GOVERNANCE.md) says who decides.

## Project structure

| Path        | Purpose                                                                                                                  |
| ----------- | ------------------------------------------------------------------------------------------------------------------------ |
| `core/`     | `ecluse-core` library: the ecosystem-agnostic domain and capability core (`Ecluse.Core.*`)                                    |
| `runtime/`  | `ecluse-runtime` library: the effectful edge (OTel SDK, warp, scribes, and cloud adapters, `Ecluse.Runtime.*`)           |
| `src/`      | `ecluse` library: the composition shell that assembles and runs the tiers (`Ecluse.*`)                                  |
| `app/`      | Executable entry point, thin wiring only                                                                                  |
| `test/`     | Unit, integration, E2E and package-store conformance tests, with shared contracts under `test/contracts/`                |
| `report/`   | Pure Markdown formatting shared by reports and generated site pages |
| `taskfiles/` | Task orchestration for tests, coverage, and performance measurements                                                      |
| `json-stream/` | Maintained json-stream fork: the private lexer and parser library, retaining upstream BSD-3-Clause attribution |
| `vendor/` | Attributed native decoder sources with bounded resource lifetime support |
| `examples/` | Operator starting points, including the Datadog Terraform dashboard example |
| `config/`   | The embedded defaults (`default.yaml`), the schema guidepost operator configs override                                    |
| `runbooks/` | Maintainer procedures run step by step (releases)                                                                        |
| `docs/`     | Contributor guides, testing contracts, and architecture decisions                                                                                        |
| `threat-modelling/` | The Saerskriven model that owns system diagrams, threats, controls, and assumptions. The site build generates its views on demand. |
| `web/`      | The documentation site (Zola): content, templates, and styles. `web/content/docs/` holds the operator manual              |
| `flake.nix` | Nix dev shell (GHC 9.10, cabal, HLS, ghcid) and the package build (`nix build`) plus hermetic checks (`nix flake check`) |
