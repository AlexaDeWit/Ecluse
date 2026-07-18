# Versioning

Écluse follows [semantic versioning](https://semver.org): `MAJOR.MINOR.PATCH`. It's an application
shipped as a container image, not a Haskell library other packages bound against, so it doesn't
follow the Haskell Package Versioning Policy (PVP). The version is a release identity for operators.

## One value, one source of truth

The version lives in one place: the `version:` field of [`ecluse.cabal`](ecluse.cabal). Everything
downstream derives from it, so the image tag, git tag, and GitHub Release can't disagree:

- `task version` prints it (via `cabal info`).
- `task tag` cuts a signed `vX.Y.Z` git tag from it, so it can't be mistyped.
- the release workflow asserts the pushed tag matches before building, and fails on any drift.

The mechanics are in [Release & Supply-Chain Operations](docs/architecture/release-supply-chain.md).

## What the numbers mean

Écluse versions against the **operator-facing contract**, not the Haskell source: the configuration
interface (the `ECLUSE_*` env vars and the config document), the proxy's request and serve behaviour
(what it admits, denies, mirrors), and the container interface. The Haskell modules are internal and
carry no API stability promise.

- **MAJOR**: a breaking change to that contract. A removed or renamed environment variable, a
  default that changes what gets admitted or denied, an incompatible configuration document, or a
  behaviour an operator relied on being removed.
- **MINOR**: a backward-compatible addition. A new ecosystem adapter, a new configuration option
  that defaults to today's behaviour, an opt-in feature.
- **PATCH**: a backward-compatible fix. A bug fix, a security patch, or performance work that leaves
  the contract unchanged.

## Before 1.0

While the version is `0.y.z` the contract above is not yet stable: any release may change behaviour
or configuration, including a minor bump. Pin an exact version, preferably
[by digest](README.md#verifying-the-image), and don't assume two `0.y` releases are compatible.
`1.0.0` is the first release that commits to the contract.

## Release candidates

A release candidate is tagged `vX.Y.Z-rc.N` (e.g. `v0.1.0-rc.2`), published with the same provenance
and SBOM attestations as a final release, flagged as a prerelease, and cuts no GitHub Release. The
tag-match guard compares only the base version, so a candidate for `X.Y.Z` carries `ecluse.cabal`
version `X.Y.Z`.

## Cutting a release

1. Bump `version:` in [`ecluse.cabal`](ecluse.cabal) in a pull request, following the rules above.
2. Once it merges, run `task tag`: it creates the signed `vX.Y.Z` tag from the cabal version but
   doesn't push, since cutting a release is deliberate.
3. Push the tag (`git push origin vX.Y.Z`). The release workflow re-asserts the tag matches the cabal
   version, builds the multi-arch image, attaches the provenance and SBOM attestations, and publishes
   the GitHub Release.

For the pipeline and attestation contract, see
[Release & Supply-Chain Operations](docs/architecture/release-supply-chain.md) and the CI notes in
[`AGENTS.md`](AGENTS.md).
