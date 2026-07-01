# Getting Started

Everything you need to set up a development environment and run the inner loop. For the
contribution *process* (conventions, sign-off, AI policy), see
[`CONTRIBUTING.md`](../CONTRIBUTING.md); for the test tiers and coverage, see
[Testing Strategy](testing.md).

## Local development

**Nix (with flakes) is a hard dependency.** The whole toolchain (GHC 9.10, Cabal, fourmolu,
hlint, Semgrep) comes from the dev shell, pinned by `flake.lock`; there's no supported
system-level build. Enter the shell with `nix develop` (or let `direnv` do it), then run
everything through **`task`**, the single entry point shared by local development and CI.

Run `task` **inside** the dev shell.
wraps itself in `nix develop --command`), but that re-enters the shell per target, so keep it
for one-offs.

| Task | Command |
|------|---------|
| Build | `task build` |
| Test (fast loop) | `task test` |
| Format (write) | `task format` |
| Lint | `task lint` |
| Static analysis (SAST) | `task sast` |
| Coverage (combined, matches Codecov; needs Docker) | `task coverage` |
| Coverage (fast, unit-only, Docker-free; partial) | `task coverage-unit` |
| Pre-push checks (fast) | `task check` |
| Full CI-gate mirror (needs Docker) | `task gate` |

Run `task --list` for the full list (the integration/smoke suites, `nix-build`, `nix-check`,
…). The underlying commands live in the [`Taskfile.yml`](../Taskfile.yml), so local and CI never
drift.

**Before you push,** run `task check`, and it has to be clean: build (warnings are errors via
`-Werror`), units (with strict assertions), doctest over the Haddock `>>>` examples,
`fourmolu --mode check`, `hlint`, and Semgrep (zero findings). `task check` is the fast
subset; `task gate` additionally runs the Docker-bound integration suite and the Haddock
build, the two tiers the gate has that `task check` doesn't, for a faithful end-to-end
representation. (Performance tests are not a push requirement; they are verified
server-side.) Add `task test-integration` (needs Docker) for the other gating suite. The CI
gate expects all those to pass. It also runs a live-registry smoke suite, but that suite
(`task test-smoke`) is allowed to fail and never gates (see [Testing Strategy](testing.md)).

### Reproducible build & checks (Nix)

The `task build` / `task test` targets above wrap `cabal`, the incremental inner loop. For a
slow hermetic build using the exact closure from the Nix store (GHC and all C libraries from
`flake.lock`), use the Nix outputs (also exposed as task targets):

| Task | Command |
|------|---------|
| Build the `ecluse` binary | `task nix-build` (`nix build`) → `./result/bin/ecluse` |
| Run the hermetic checks | `task nix-check` (`nix flake check`) |

`nix flake check` builds the package and runs the pure tier: the `ecluse-unit` suite
(`checks.unit`), `fourmolu --mode check` (`checks.format`), `hlint` (`checks.lint`),
`cabal check` (`checks.cabal-check`), and the library Haddock (`checks.docs`). Three things
are deliberately **excluded**, because they can't run in a hermetic sandbox: `ecluse-integration`
(needs a Docker daemon), `ecluse-smoke` (live network), and Semgrep (`--config auto` fetches
rules over the network). Those stay dev-shell / CI steps.

> **Flakes only see git-tracked files.** `git add` new sources before `nix build` /
> `nix flake check`, or they're invisible to the build, and a build that references them
> (e.g. via the cabal file) will fail on the missing modules.

Reach for Nix to reproduce CI exactly or to produce the release artifact; reach for `cabal`
for day-to-day iteration (Nix rebuilds the whole package on any change, so it's poor for
edit-compile cycles).

### Dependency locking

Two build paths means two locks, one per resolver, pinned **independently**:

| Path | Resolver | Lock |
|------|----------|------|
| Nix / hermetic build (the **shipped** artifact) | nixpkgs GHC 9.10 set | `flake.lock` |
| `cabal` (dev shell + the CI gate) | Hackage | `index-state:` in `cabal.project` + `cabal.project.freeze` |

`callCabal2nix` does **not** read `cabal.project` / `cabal.project.freeze`, so the two are
genuinely separate locks. The `index-state` caps the Hackage snapshot the solver may see, and
the freeze pins exact versions, so `cabal build` / `cabal test` (and therefore the gate)
resolve a **reproducible** plan: a fresh Hackage upload can no longer flip the gate with no
source change. Today's caret bounds keep the frozen versions close to the nixpkgs ones, so
the gate tests roughly what ships.

Move the pins **deliberately**:

- **cabal path.** `task freeze` advances `index-state` to the latest index and rewrites
  `cabal.project.freeze`. Commit both. (Renovate widens the *bounds* in `ecluse.cabal`;
  moving the *pinned versions* is this manual, reviewed step.)
- **Nix path.** `nix flake update`, or Renovate's weekly `flake.lock` refresh.

A flake bump that shifts the nixpkgs package set is a good prompt to `task freeze`, so both
paths keep tracking the same versions.

---

## Codebase Layout

The *principles* of module organisation and namespacing (vertical organisation, where types
live with their functions, one `Ecluse.<Area>` namespace per area, and when a `.Types` split
is justified) live in [`STYLE.md`](../STYLE.md) → "Module organization". This section records
the *current* concrete layout and the one project-specific pattern below.

- **Two libraries behind one `ecluse.cabal`.** The pure capability core is the `ecluse-core`
  library (`core/src`, modules under `Ecluse.Core.*`); the application shell that composes it
  into a running proxy, config, the `Env` composition root, logging, the WAI `Application`, and
  the telemetry SDK/OTLP wiring, is the `ecluse` library (`src`, modules under `Ecluse.*`), with
  `app/Main.hs` as the executable. The boundary is build-enforced: the core's unit suite cannot
  depend on the app library. See
  [architecture → Codebase decomposition](architecture.md#codebase-decomposition).

- **Handles are records of functions, selected at one composition root.** A swappable backend
  (registry protocol, mirror queue, credential provider) is modelled as a record whose fields
  are functions (the *Handle pattern*), built by a per-backend smart constructor (e.g.
  `newSqsQueue :: SqsConfig -> IO MirrorQueue`). Adding a backend means adding a constructor
  behind the *existing* record and wiring it into the single, config-driven composition root,
  never smearing SDK or provider selection across call sites. See
  [Cloud Backends → Handles](architecture/cloud-backends.md#handles-records-of-functions).

For the **current module list**, read the module index of the
[published Haddock](https://alexadewit.github.io/Ecluse/api/) (each module's one-line summary
is its header) and the root [`Ecluse`](../src/Ecluse.hs) module's "How the code is organised"
synopsis for the narrative grouping. Both live with the code and update with it, so they can't
drift; this guide deliberately doesn't duplicate the list here.

Tests mirror this hierarchy within each suite's source dir (e.g. the core unit specs for
`Ecluse.Core.Rules` and `Ecluse.Core.Version` are `core/test/unit/Ecluse/RulesSpec.hs` and
`core/test/unit/Ecluse/VersionSpec.hs`; version ordering additionally has a differential suite,
`Ecluse.VersionOrderingSpec`, against reference oracles).
