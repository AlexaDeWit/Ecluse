# Getting Started

Everything you need to set up a development environment and run the inner
loop. For the contribution *process* (conventions, sign-off, AI policy), see
[`CONTRIBUTING.md`](../CONTRIBUTING.md); for the test tiers and coverage, see
[Testing Strategy](testing.md).

## Local development

**Nix (with flakes) is a hard dependency.** The whole toolchain — GHC 9.10, Cabal,
fourmolu, hlint, Semgrep — comes from the dev shell, pinned by `flake.lock`; there
is no supported system-level build. Enter the shell with `nix develop` (or let
`direnv` do it), then run everything through **`make`**, the single entry point
shared by local development and CI.

Run `make` **inside** the dev shell. Targets also work from a bare terminal (each
wraps itself in `nix develop --command`), but that re-enters the shell per target,
so reserve it for one-offs.

| Task | Command |
|------|---------|
| Build | `make build` |
| Test (fast loop) | `make test` |
| Format (write) | `make format` |
| Lint | `make lint` |
| Static analysis (SAST) | `make sast` |
| Coverage (unit → Codecov JSON) | `make coverage` |
| Everything the gate runs | `make check` |

Run `make help` for the full list (the integration/smoke suites, `nix-build`,
`nix-check`, …). The underlying commands live in the [`Makefile`](../Makefile), so
local and CI never drift.

**Before you push,** run `make check` — it must be clean: build (warnings are
errors via `-Werror`; see [`STYLE.md`](../STYLE.md) → "Compiler flags"), the unit
suite, the doctest examples, `fourmolu --mode check`, `hlint`, and Semgrep (zero
findings).
Add `make test-integration` (needs Docker) for the other gating suite. The CI
`gate` enforces the same set, so a clean local run predicts a green gate. The
smoke suite (`make test-smoke`) is allowed to fail and never gates (see [Testing Strategy](testing.md)).

### Reproducible build & checks (Nix)

The `make build` / `make test` targets above wrap `cabal`, the incremental inner
loop. For a reproducible, **hermetic** build and check run — sandboxed, with
every dependency pinned by `flake.lock` — use the Nix outputs (also exposed as
make targets):

| Task | Command |
|------|---------|
| Build the `ecluse` binary | `make nix-build` (`nix build`) → `./result/bin/ecluse` |
| Run the hermetic checks | `make nix-check` (`nix flake check`) |

`nix flake check` builds the package and runs the pure tier: the `ecluse-unit`
suite (`checks.unit`), `fourmolu --mode check` (`checks.format`), `hlint`
(`checks.lint`), and `cabal check` (`checks.cabal-check`). Deliberately
**excluded** — they cannot run in a hermetic
sandbox: `ecluse-integration` (needs a Docker daemon), `ecluse-smoke` (live
network), and Semgrep (`--config auto` fetches rules over the network). Those
stay dev-shell / CI steps.

> **Flakes only see git-tracked files.** `git add` new sources before
> `nix build` / `nix flake check`, or they are invisible to the build — and a
> build that references them (e.g. via the cabal file) will fail on the missing
> modules.

Reach for Nix to reproduce CI exactly or to produce the release artifact; reach
for `cabal` for day-to-day iteration (Nix rebuilds the whole package on any
change, so it is poor for edit-compile cycles).

### Dependency locking

Two build paths means two locks, one per resolver, pinned **independently**:

| Path | Resolver | Lock |
|------|----------|------|
| Nix / hermetic build (the **shipped** artifact) | nixpkgs GHC 9.10 set | `flake.lock` |
| `cabal` (dev shell + the CI gate) | Hackage | `index-state:` in `cabal.project` + `cabal.project.freeze` |

`callCabal2nix` does **not** read `cabal.project` / `cabal.project.freeze`, so the
two are genuinely separate locks. The `index-state` caps the Hackage snapshot the
solver may see, and the freeze pins exact versions — so `cabal build` / `cabal test`
(and therefore the gate) resolve a **reproducible** plan: a fresh Hackage upload can
no longer flip the gate with no source change. Today's caret bounds keep the frozen
versions close to the nixpkgs ones, so the gate tests roughly what ships.

Move the pins **deliberately**:

- **cabal path** — `make freeze`: advances `index-state` to the latest index and
  rewrites `cabal.project.freeze`. Commit both. (Renovate widens the *bounds* in
  `ecluse.cabal`; moving the *pinned versions* is this manual, reviewed step.)
- **Nix path** — `nix flake update`, or Renovate's weekly `flake.lock` refresh.

A flake bump that shifts the nixpkgs package set is a good prompt to `make freeze`,
so both paths keep tracking the same versions.

---

## Codebase Layout

The *principles* of module organization and namespacing — vertical organization
(types live with their functions), one `Ecluse.<Area>` namespace per area, and
when a `.Types` split is justified — live in [`STYLE.md`](../STYLE.md) → "Module
organization". This section records the *current* concrete layout and the one
project-specific pattern below.

- **Handles are records of functions, selected at one composition root.** A
  swappable backend — registry protocol, mirror queue, credential provider — is
  modelled as a record whose fields are functions (the *Handle pattern*), built by
  a per-backend smart constructor (e.g. `newSqsQueue :: SqsConfig -> IO
  MirrorQueue`). Adding a backend means adding a constructor behind the *existing*
  record and wiring it into the single, config-driven composition root — never
  smearing SDK or provider selection across call sites. See
  [Cloud Backends → Handles](architecture/cloud-backends.md#handles-records-of-functions).

For the **current module list**, read the module index of the
[published Haddock](https://alexadewit.github.io/Ecluse/api/) — each module's one-line
summary is its header — and the root [`Ecluse`](../src/Ecluse.hs) module's "How the
code is organized" synopsis for the narrative grouping. Both live with the code
and update with it, so they cannot drift; this guide deliberately does not
duplicate the list here.

Tests mirror this hierarchy within each suite's source dir (e.g. the unit specs
for `Ecluse.Rules` and `Ecluse.Version` are `test/unit/Ecluse/RulesSpec.hs` and
`test/unit/Ecluse/VersionSpec.hs`; version ordering additionally has a
differential suite, `Ecluse.VersionOrderingSpec`, against reference oracles).

