# Getting started

Everything you need to set up a development environment and run the inner loop. For the
contribution *process* (conventions, sign-off, AI policy), see
[`CONTRIBUTING.md`](../CONTRIBUTING.md); for the test tiers and coverage, see
[Testing Strategy](testing.md).

## Local development

**Nix (with flakes) is a hard dependency.** The whole toolchain (GHC 9.10, Cabal, fourmolu, hlint,
Semgrep) comes from the dev shell, pinned by `flake.lock`; there's no supported system-level build.
Enter the shell with `nix develop` (or let `direnv` do it), then run everything through **`task`**,
the entry point shared by local development and CI. Running it from outside the shell works but
re-enters per target, so keep that for one-offs.

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

Run `task --list` for the full set. The underlying commands live in
[`Taskfile.yml`](../Taskfile.yml), so local and CI never drift.

**Before you push,** run `task check` clean: build (`-Werror`), units, doctest over the Haddock
`>>>` examples, `fourmolu --mode check`, `hlint`, Semgrep (zero findings), and the two analysis
tiers `weeder` (dead code) and `stan`, each failing `check` on any finding. It's a full `-Werror`
build of every component plus two `-fwrite-ide-info` builds, so cold it runs well over 10 minutes.
`task gate` adds the Docker-bound integration suite and the Haddock build, the two tiers `check`
lacks. The CI gate expects all of that, plus end-to-end (`task test-e2e`) and a live-registry smoke
suite (`task test-smoke`): e2e gates on every PR but is kept out of local `check` and `gate` for its
weight; smoke is allowed to fail and never gates. Performance tests run server-side, not at push. See
[Testing Strategy](testing.md).

### Reproducible build and checks (Nix)

`task build` / `task test` wrap `cabal` for the incremental inner loop. For a hermetic build from the
exact Nix-store closure (GHC and C libraries from `flake.lock`), use the Nix outputs:

| Task | Command |
|------|---------|
| Build the `ecluse` binary | `task nix-build` (`nix build`) → `./result/bin/ecluse` |
| Evaluate the flake, build the Haddock check | `task nix-check` (`nix flake check`) |

`nix flake check` builds the one flake check, `checks.docs`: the library Haddock, so a broken doc
comment fails; it's the check the CI `docs` job builds. The tests, formatting, and linting are
**not** flake checks; they run through `task` against the incremental cabal build, which is what gates
in CI. Three couldn't be flake checks even in principle: `ecluse-integration` (needs Docker),
`ecluse-smoke` (live network), and Semgrep (`--config auto` fetches rules over the network).

> **Flakes only see git-tracked files.** `git add` new sources before `nix build` /
> `nix flake check`, or they're invisible and a build that references them (via the cabal file)
> fails on the missing modules.

### Dependency locking

Two build paths, two locks, pinned **independently**:

| Path | Resolver | Lock |
|------|----------|------|
| Nix / hermetic build (the **shipped** artifact) | nixpkgs GHC 9.10 set | `flake.lock` |
| `cabal` (dev shell + the CI gate) | Hackage | `index-state:` in `cabal.project` + `cabal.project.freeze` |

`callCabal2nix` doesn't read `cabal.project` / `.freeze`, so the two are genuinely separate locks. The
`index-state` caps the Hackage snapshot and the freeze pins exact versions, so a fresh Hackage upload
can't flip the gate with no source change. Move the pins deliberately: on the cabal path,
`task freeze` advances `index-state` and rewrites `cabal.project.freeze` (commit both); on the Nix
path, `nix flake update` or Renovate's weekly `flake.lock` refresh. Renovate widens the *bounds* in
`ecluse.cabal`, but moving the *pinned versions* is the manual `task freeze` step.

---

## Codebase layout

The *principles* of module organisation (types with their functions, one `Ecluse.<Area>` namespace
per area, when a `.Types` split is justified) live in [`docs/style.md`](style.md) →
"Module organisation". This section records the current layout and one project-specific pattern.

- **Two libraries behind one `ecluse.cabal`.** The pure capability core is `ecluse-core` (`core/src`,
  `Ecluse.Core.*`); the application shell that composes it into a running proxy (config, the `Env`
  composition root, logging, the WAI app, telemetry) is `ecluse` (`src`, `Ecluse.*`), with
  `app/Main.hs` the executable. The boundary is build-enforced: the core's unit suite can't depend on
  the app library. See
  [architecture → Codebase decomposition](architecture.md#codebase-decomposition).
- **Handles are records of functions, selected at one composition root.** A swappable backend
  (registry protocol, mirror queue, credential provider) is a record whose fields are functions (the
  *Handle pattern*), built by a per-backend smart constructor (e.g.
  `newSqsQueue :: SqsConfig -> IO MirrorQueue`). Adding a backend means a constructor behind the
  *existing* record, wired into the single composition root, not provider selection smeared across
  call sites. See
  [Cloud Backends → Handles](architecture/cloud-backends.md#handles-records-of-functions).

For the module list, read the [published Haddock](https://ecluse-proxy.com/api/) module index and the
root [`Ecluse`](../src/Ecluse.hs) synopsis. Tests mirror this hierarchy (e.g.
`Ecluse.Core.Rules` → `core/test/unit/Ecluse/RulesSpec.hs`).
