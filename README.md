# Écluse

A supply-chain resilience proxy for package registries, written in Haskell. The
name is French for a canal lock — the controlled passage every dependency clears
before it reaches your build.

## Overview

`ecluse` sits between your development environment (or CI) and the npm
registry, enforcing a configurable resilience policy before any package reaches a
build. It proxies requests through a private upstream first, falls back to the
public npm registry with rules applied, and mirrors approved packages
asynchronously — without hosting packages itself.

See [`docs/architecture.md`](docs/architecture.md) for the full design:
three-registry model, deny-by-default rules engine, mirror queue, and
configuration reference.

## Development

See [`CONTRIBUTING.md`](CONTRIBUTING.md) for the full contributor guide —
codebase conventions, testing strategy, and CI / repository requirements. A
quick start follows.

### Prerequisites

[Nix](https://nixos.org/) with flakes enabled provides all build tooling via the
dev shell. A running Docker daemon is required only for the integration test
suite (which spins up ephemeral containers via `testcontainers` / `ministack`);
unit and property tests need nothing beyond the dev shell.

### Getting started

```bash
# Enter the dev shell (direnv will do this automatically if configured)
nix develop

# Build
cabal build all

# Run the fast unit tests (see CONTRIBUTING.md for the integration/smoke suites)
cabal test ecluse-unit

# Run the proxy
cabal run ecluse
```

### Continuous integration

Every push and pull request runs [`.github/workflows/ci.yml`](.github/workflows/ci.yml):
build, unit and integration tests, format & lint, and Semgrep static analysis,
all feeding a single `gate` job (the one required status check). The smoke suite
(live-registry checks) also runs but is allowed to fail and does not gate. CI
uses the same Nix dev shell as local development (pinned by `flake.lock`), so it
validates against the exact same toolchain. See [`CONTRIBUTING.md`](CONTRIBUTING.md)
for details.

## Project Structure

| Path | Purpose |
|------|---------|
| `app/` | Executable entry point — thin wiring only |
| `src/` | Library — all business logic |
| `test/` | Unit and integration tests |
| `docs/` | Architecture decision records and design documents |
| `flake.nix` | Nix dev environment (GHC 9.6, cabal, HLS, ghcid) |
