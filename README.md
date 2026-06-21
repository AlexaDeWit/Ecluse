# npm-secure-proxy

A defense-in-depth proxy for the npm registry, written in Haskell.

## Overview

`npm-secure-proxy` sits between your development environment (or CI) and the npm
registry, enforcing a configurable security policy before any package reaches a
build. It proxies requests through a private upstream first, falls back to the
public npm registry with rules applied, and mirrors approved packages
asynchronously — without hosting packages itself.

See [`docs/architecture.md`](docs/architecture.md) for the full design:
three-registry model, deny-by-default rules engine, mirror queue, and
configuration reference.

## Development

### Prerequisites

[Nix](https://nixos.org/) with flakes enabled. All tooling is provided by the Nix dev shell.

### Getting started

```bash
# Enter the dev shell (direnv will do this automatically if configured)
nix develop

# Build
cabal build all

# Run tests
cabal test

# Run the proxy
cabal run npm-secure-proxy
```

## Project Structure

| Path | Purpose |
|------|---------|
| `app/` | Executable entry point — thin wiring only |
| `src/` | Library — all business logic |
| `test/` | Unit and integration tests |
| `docs/` | Architecture decision records and design documents |
| `flake.nix` | Nix dev environment (GHC 9.6, cabal, HLS, ghcid) |
