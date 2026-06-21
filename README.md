# Écluse

![Écluse — a supply-chain resilience proxy for package registries](docs/social-preview.png)

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

Every task runs through `make`, and the tools come from the Nix dev shell
automatically — each target wraps itself in `nix develop` when you're not already
inside it, so this works straight from a bare terminal:

```bash
make build      # build library, executable, and tests
make test       # fast, gating unit suite
make check      # build + test + format + lint + sast (what the CI gate runs)
make run        # run the proxy
make help       # list every target
```

Prefer an interactive shell? `nix develop` (or direnv) drops you in, and the same
`make` targets then run the tools directly. For a hermetic, reproducible
build/checks — sandboxed, what you'd ship — use `make nix-build` and
`make nix-check`.

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
| `flake.nix` | Nix dev shell (GHC 9.6, cabal, HLS, ghcid) **and** the package build (`nix build`) + hermetic checks (`nix flake check`) |
