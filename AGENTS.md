# Agent Instructions

## Documentation Policy

- **Always read `README.md` before starting any task.** It describes the current architecture, key design decisions, and module responsibilities.
- **Read architecture documents** in `docs/` (if present) before making structural changes to the codebase.
- **Keep documentation up to date.** Any change that affects behavior, architecture, public interfaces, or configuration must be reflected in `README.md` and any relevant `docs/` file in the same PR/commit. Do not defer documentation updates.
- When adding a new module or significantly changing an existing one, update the architecture section of `README.md` to describe its role.

## Project Structure

```
app/       — executable entry point (Main.hs only; keep thin)
src/       — library code (all business logic lives here)
test/      — unit and integration tests (mirror src/ module structure)
docs/      — architecture and design documents
```

## Code Conventions

- **Follow [`STYLE.md`](STYLE.md) for all Haskell coding style** — documentation/Haddock requirements, naming, function design, totality, imports, and the compiler-flag set. It is written to be followed directly by agents; read it before writing or changing code. The points below are the structural essentials only.
- Separate concerns: application wiring in `app/`, logic in `src/`, tests in `test/`.
- Tests mirror the library module hierarchy (e.g. `src/Foo/Bar.hs` → `test/Foo/BarSpec.hs`).
- Keep `app/Main.hs` thin — it should only parse config and call into the library.
- **Keep modules fit-to-purpose with idiomatic namespacing.** Organize vertically (a type lives with the functions on it), one `Ecluse.<Area>` namespace per area, and split a `.Types` module only when it earns it. The full principles are in [`STYLE.md`](STYLE.md) → "Module organization"; the current concrete layout is in [`CONTRIBUTING.md`](CONTRIBUTING.md) → "Codebase Layout".

## Build & Tooling

- **Nix (with flakes) is a hard dependency.** All tooling comes from the dev shell (`nix develop`, or `direnv`), pinned by `flake.lock` (GHC 9.6); there is no supported system-level GHC/Cabal.
- Run every task through **`make`** — the unified runner shared with CI (`make build`, `make test`, `make check`, `make sast`, …; `make help` for the list), which wraps Cabal/fourmolu/hlint/Semgrep from the dev shell. Run `make` **inside** `nix develop` (it auto-wraps in `nix develop --command` otherwise, but re-enters the shell per target). Cabal (not Stack) remains the underlying build tool.
- **Haskell search & exploration tools are in the dev shell** — use them to confirm signatures and discover functions instead of guessing:
  - `hoogle` — search by name or type signature. Run `hoogle generate` once (downloads the Hackage database; needs network) to build the index, then e.g. `hoogle 'Text -> ByteString'`, `hoogle Data.Map.insert`, or `hoogle --info <name>` for docs; `hoogle server --local --port 8080` gives a browsable UI.
  - `cabal-plan` — inspect the resolved build plan and dependency versions (`cabal-plan list-bins`, `cabal-plan dot`); run after a `make build`.
  - `haskell-language-server` and `ghcid` — live type/error feedback; Haddock for the project builds via `cabal haddock`.
- The dependency set and rationale (relude as the prelude via cabal mixins, aeson, amazonka, warp/wai, http-client-tls, katip, envparse, cache, hedgehog) live in [`docs/architecture.md`](docs/architecture.md) → "Technology Stack"; the **testing strategy** (pure `hspec`+`hedgehog` tests; integration tests via `testcontainers` + `ministack` over Docker) lives in [`CONTRIBUTING.md`](CONTRIBUTING.md). Read them before adding dependencies or tests.

## CI & Security

- CI is a **single unified workflow** (`.github/workflows/ci.yml`): the build/test, format/lint, and Semgrep jobs all feed a terminal **`gate`** job. Only `gate` is marked `Required` in branch protection — wire any new check in as a `gate` dependency, never as another required check. See [`CONTRIBUTING.md`](CONTRIBUTING.md) → "Continuous Integration".
- **Pin every GitHub Action to a full commit SHA** (never a tag/branch), with the version in a trailing comment. Dependabot bumps them.
- Keep workflows injection-free — never interpolate untrusted `${{ github.event.* }}` / `${{ github.head_ref }}` values directly into `run:` shell blocks; pass them via `env:` or intermediate files instead.
- **Semgrep** runs `--config auto`, failing on ERROR/WARNING findings, from the Nix dev shell, so CI and local use the same pinned binary.
- **Before pushing, verify Semgrep is clean locally:** inside `nix develop`, `make sast` (Semgrep `--config auto`, failing on ERROR/WARNING) must report zero findings. Do not push with outstanding findings.
- **Semgrep ignores require the repo owner's approval.** Do not add `.semgrepignore` entries or `nosemgrep` comments unilaterally.
- Commits are GPG-signed; keep history verifiable.
