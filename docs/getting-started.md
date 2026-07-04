# Getting started

Everything you need to set up a development environment and run the inner loop. For the
contribution *process* (conventions, sign-off, AI policy), see
[`CONTRIBUTING.md`](../CONTRIBUTING.md); for the test tiers and coverage, see
[Testing Strategy](testing.md).

## Local development

**Nix (with flakes) is a hard dependency.** The whole toolchain (GHC 9.10, Cabal, fourmolu,
hlint, Semgrep) comes from the dev shell, pinned by `flake.lock`; there's no supported
system-level build. Enter the shell with `nix develop` (or let `direnv` do it), then run
everything through **`task`**, the single entry point shared by local development and CI.

Run `task` inside the dev shell. Running it from outside works too (it wraps itself in
`nix develop --command`), but that re-enters the shell per target, so keep it for one-offs.

| Task | Command |
|------|---------|
| Build | `task build` |
| Test (fast loop) | `task test` |
| Format (write) | `task format` |
| Lint | `task lint` |
| Static analysis (SAST) | `task sast` |
| Coverage (combined, matches Codecov; needs Docker) | `task coverage` |
| Coverage (fast, unit-only, Docker-free; partial) | `task coverage-unit` |
| Pre-push checks (subset of the gate) | `task check` |
| Full CI-gate mirror (needs Docker) | `task gate` |

Run `task --list` for the full list (the integration/smoke suites, `nix-build`, `nix-check`,
…). The underlying commands live in the [`Taskfile.yml`](../Taskfile.yml), so local and CI never
drift.

**Before you push,** run `task check` clean: build (`-Werror`), units (strict assertions),
doctest over the Haddock `>>>` examples, `fourmolu --mode check`, `hlint`, and Semgrep (zero
findings). It's a full `-Werror` build of every component, so on a cold checkout or under
contention it runs 10+ minutes. `task gate` adds the Docker-bound integration suite and the
Haddock build, the two tiers `task check` lacks (`task test-integration` runs the integration
suite alone). Performance tests aren't a push requirement; they're verified server-side. The CI
gate expects all those to pass, plus a live-registry smoke suite (`task test-smoke`) that's
allowed to fail and never gates (see [Testing Strategy](testing.md)).

### Reproducible build & checks (Nix)

The `task build` / `task test` targets wrap `cabal`, the incremental inner loop. For a slow
hermetic build from the exact Nix-store closure (GHC and C libraries from `flake.lock`), use the
Nix outputs:

| Task | Command |
|------|---------|
| Build the `ecluse` binary | `task nix-build` (`nix build`) → `./result/bin/ecluse` |
| Run the hermetic checks | `task nix-check` (`nix flake check`) |

`nix flake check` builds the package and runs the pure tier: the `ecluse-unit` suite
(`checks.unit`), `fourmolu --mode check` (`checks.format`), `hlint` (`checks.lint`), `cabal check`
(`checks.cabal-check`), and the library Haddock (`checks.docs`). Three things are excluded (they
can't run in a hermetic sandbox): `ecluse-integration` (needs Docker), `ecluse-smoke` (live
network), and Semgrep (`--config auto` fetches rules over the network). Those stay dev-shell / CI
steps.

> **Flakes only see git-tracked files.** `git add` new sources before `nix build` /
> `nix flake check`, or they're invisible to the build, and a build that references them
> (e.g. via the cabal file) will fail on the missing modules.

Reach for Nix to reproduce CI exactly or produce the release artifact; reach for `cabal` for
day-to-day iteration (Nix rebuilds the whole package on any change).

### Dependency locking

Two build paths means two locks, one per resolver, pinned **independently**:

| Path | Resolver | Lock |
|------|----------|------|
| Nix / hermetic build (the **shipped** artifact) | nixpkgs GHC 9.10 set | `flake.lock` |
| `cabal` (dev shell + the CI gate) | Hackage | `index-state:` in `cabal.project` + `cabal.project.freeze` |

`callCabal2nix` doesn't read `cabal.project` / `cabal.project.freeze`, so the two are genuinely
separate locks. The `index-state` caps the Hackage snapshot the solver sees and the freeze pins
exact versions, so `cabal build` / `cabal test` (and the gate) resolve a reproducible plan: a fresh
Hackage upload can't flip the gate with no source change. Today's caret bounds keep the frozen
versions close to the nixpkgs ones, so the gate tests roughly what ships.

Move the pins deliberately:

- **cabal path.** `task freeze` advances `index-state` and rewrites `cabal.project.freeze`; commit
  both. (Renovate widens the *bounds* in `ecluse.cabal`; moving the *pinned versions* is this
  manual step.)
- **Nix path.** `nix flake update`, or Renovate's weekly `flake.lock` refresh.

A flake bump that shifts the nixpkgs set is a good prompt to `task freeze`, so both paths track the
same versions.

---

## Codebase layout

The *principles* of module organisation (vertical organisation, types with their functions, one
`Ecluse.<Area>` namespace per area, when a `.Types` split is justified) live in
[`STYLE.md`](../STYLE.md) → "Module organization". This section records the current layout and one
project-specific pattern.

- **Two libraries behind one `ecluse.cabal`.** The pure capability core is `ecluse-core` (`core/src`,
  `Ecluse.Core.*`); the application shell that composes it into a running proxy (config, the `Env`
  composition root, logging, the WAI `Application`, telemetry) is `ecluse` (`src`, `Ecluse.*`), with
  `app/Main.hs` as the executable. The boundary is build-enforced: the core's unit suite can't depend
  on the app library. See
  [architecture → Codebase decomposition](architecture.md#codebase-decomposition).

- **Handles are records of functions, selected at one composition root.** A swappable backend
  (registry protocol, mirror queue, credential provider) is a record whose fields are functions (the
  *Handle pattern*), built by a per-backend smart constructor (e.g.
  `newSqsQueue :: SqsConfig -> IO MirrorQueue`). Adding a backend means adding a constructor behind
  the *existing* record and wiring it into the single composition root, not smearing provider
  selection across call sites. See
  [Cloud Backends → Handles](architecture/cloud-backends.md#handles-records-of-functions).

For the current module list, read the [published Haddock](https://ecluse-proxy.com/api/)
module index and the root [`Ecluse`](../src/Ecluse.hs) module's "How the code is organised"
synopsis. Both live with the code, so this guide doesn't duplicate the list.

Tests mirror this hierarchy within each suite's source dir (e.g. `Ecluse.Core.Rules` →
`core/test/unit/Ecluse/RulesSpec.hs`; version ordering also has a differential suite,
`Ecluse.VersionOrderingSpec`, against reference oracles).
