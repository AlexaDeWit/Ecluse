# Versioning

Écluse follows [semantic versioning](https://semver.org) (semver): `MAJOR.MINOR.PATCH`. It
is an application shipped as a container image, not a Haskell library that other packages take
dependency bounds against, so it does not follow the Haskell Package Versioning Policy (PVP).
The version is a release identity for operators, and semver is the scheme they expect.

## One value, one source of truth

The version lives in exactly one place: the `version:` field of [`ecluse.cabal`](ecluse.cabal).
Everything downstream derives from it, so the published image tag, the git tag, and the GitHub
Release can never disagree:

- `make version` prints it (reading `cabal info`, cabal's own parser of the package).
- `make tag` cuts a signed `vX.Y.Z` git tag from it, so the tag cannot be mistyped.
- the release workflow asserts the pushed tag matches it before it builds anything, and fails
  the release on any drift.

The mechanics are in [Release & Supply-Chain Operations](docs/architecture/release-supply-chain.md).

## What the numbers mean

I version against the **operator-facing contract**, not the Haskell source. That contract is
the configuration interface (the `PROXY_*` environment variables and the configuration
document), the proxy's request and serve behaviour (what it admits, denies, and mirrors), and
the container interface. The Haskell modules are internal to the application and carry no API
stability promise.

- **MAJOR**: a breaking change to that contract. A removed or renamed environment variable, a
  default that changes what gets admitted or denied, an incompatible configuration document, or
  a behaviour an operator relied on being removed.
- **MINOR**: a backward-compatible addition. A new ecosystem adapter, a new configuration option
  that defaults to today's behaviour, an opt-in feature.
- **PATCH**: a backward-compatible fix. A bug fix, a security patch, or performance work that
  leaves the contract unchanged.

## Before 1.0

Écluse is pre-MVP, and while the version is `0.y.z` the contract above is **not yet stable**.
Any release may change behaviour or configuration, including in a minor bump. Pin an exact
version, and prefer pinning [by digest](README.md#verifying-the-image); do not assume two `0.y`
releases are compatible. The first release that commits to the contract is `1.0.0`.

## Release candidates

A release candidate is tagged `vX.Y.Z-rc.N` (for example `v0.1.0-rc.2`). It is published with
the same provenance and SBOM attestations as a final release, flagged as a prerelease, and cuts
no GitHub Release. The tag-match guard compares only the base version, so a candidate for
`X.Y.Z` carries the `ecluse.cabal` version `X.Y.Z`.

## Cutting a release

1. Bump `version:` in [`ecluse.cabal`](ecluse.cabal) in a pull request, following the rules above.
2. Once it merges, run `make tag`. It creates the signed `vX.Y.Z` tag from the cabal version. It
   does not push, because cutting a release is a deliberate step.
3. Push the tag: `git push origin vX.Y.Z`. The release workflow re-asserts that the tag matches
   the cabal version, builds the multi-arch image, attaches the provenance and SBOM
   attestations, and publishes the GitHub Release.

For the release pipeline and the attestation contract, see
[Release & Supply-Chain Operations](docs/architecture/release-supply-chain.md) and the CI notes
in [`AGENTS.md`](AGENTS.md).
