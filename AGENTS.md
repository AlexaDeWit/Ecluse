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
docs/      — architecture decision records and design documents
```

## Code Conventions

- Separate concerns: application wiring in `app/`, logic in `src/`, tests in `test/`.
- Tests mirror the library module hierarchy (e.g. `src/Foo/Bar.hs` → `test/Foo/BarSpec.hs`).
- Keep `app/Main.hs` thin — it should only parse config and call into the library.

## Build & Tooling

- All tooling comes from the Nix dev shell — run `nix develop` (or rely on `direnv`) before building. Do not assume a system-level GHC/Cabal.
- Build with **Cabal** (`cabal build all`, `cabal test`), not Stack: Nix provides reproducibility and `flake.lock` pins nixpkgs (GHC 9.6).
- The dependency set and rationale (relude as the prelude via cabal mixins, aeson, amazonka, warp/wai, http-client-tls, katip, envparse, cache, hedgehog) and the **testing strategy** (pure `hspec`+`hedgehog` tests; integration tests via `testcontainers` + `ministack` over Docker) live in [`docs/architecture.md`](docs/architecture.md). Read it before adding dependencies or tests.

## CI & Security

- Every push runs the build + test workflow and **Semgrep** (`--config auto`, failing on ERROR/WARNING findings). Keep workflows injection-free — never interpolate untrusted `${{ github.event.* }}` / `${{ github.head_ref }}` values directly into `run:` shell blocks; pass them via `env:` or intermediate files instead.
- **Before pushing, verify Semgrep is clean locally:** `semgrep scan --config auto --severity ERROR --severity WARNING --error .` must report zero findings. Do not push with outstanding findings.
- **Semgrep ignores require the repo owner's approval.** Do not add `.semgrepignore` entries or `nosemgrep` comments unilaterally.
- Commits are GPG-signed; keep history verifiable.
