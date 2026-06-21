# Contributing

This guide covers how we work on **Écluse** (package `ecluse`): local development, codebase
conventions, testing, and CI / repository requirements. For the systems design
— the three-registry model, rules engine, the seams (registry, queue, credential
provider) and cloud backends, and configuration — see
[`docs/architecture.md`](docs/architecture.md). Haskell coding style —
formatting, Haddock, naming, totality, and the compiler-flag set — has its own
reference in [`STYLE.md`](STYLE.md). Agent-specific instructions live in
[`AGENTS.md`](AGENTS.md).

## Local development

**Nix (with flakes) is a hard dependency.** The whole toolchain — GHC 9.6, Cabal,
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
| Everything the gate runs | `make check` |

Run `make help` for the full list (the integration/smoke suites, `nix-build`,
`nix-check`, …). The underlying commands live in the [`Makefile`](Makefile), so
local and CI never drift.

**Before you push,** run `make check` — it must be clean: build (warnings are
errors via `-Werror`; see [`STYLE.md`](STYLE.md) → "Compiler flags"), the unit
suite, `fourmolu --mode check`, `hlint`, and Semgrep (zero findings).
Add `make test-integration` (needs Docker) for the other gating suite. The CI
`gate` enforces the same set, so a clean local run predicts a green gate. The
smoke suite (`make test-smoke`) is allowed to fail and never gates (see Testing
Strategy).

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
suite (`checks.unit`), `fourmolu --mode check` (`checks.format`), and `hlint`
(`checks.lint`). Deliberately **excluded** — they cannot run in a hermetic
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

---

## Codebase Layout

Modules are fit-to-purpose and follow idiomatic Haskell structure: each area of
the application gets its own namespace directly under `Ecluse`, and types
are split from implementation where that split earns its keep.

- **One namespace per area.** Each concern lives under its own
  `Ecluse.<Area>` namespace (`Rules` today; `Registry`, `Queue`, `Credential`,
  `Config`, `Server`, … later) rather than being appended to a grab-bag module.
- **Types split from implementation — when it helps.** Where an area carries
  non-trivial logic, its data types live in a `.Types` leaf module and the
  functions live in the sibling module (e.g. `Ecluse.Rules.Types` +
  `Ecluse.Rules`). Where an area is essentially a cohesive set of types
  with their constructors and renderers (the package model), a single module is
  clearer than a forced split.
- **Seams are records of functions, selected at one composition root.** A
  swappable backend — registry protocol, mirror queue, credential provider — is
  modelled as a record whose fields are functions (the *Handle pattern*), built by
  a per-backend smart constructor (e.g. `newSqsQueue :: SqsConfig -> IO
  MirrorQueue`). Adding a backend means adding a constructor behind the *existing*
  record and wiring it into the single, config-driven composition root — never
  smearing SDK or provider selection across call sites. See
  [Cloud Backends → Seams](docs/architecture.md#seams-records-of-functions).

Current layout:

| Module | Holds |
|--------|-------|
| `Ecluse.Package` | The ecosystem-agnostic package vocabulary: `Scope`, `PackageName`, `Version`, `Dist`, `Maintainer`, `PackageDetails`, with their smart constructors and renderers. |
| `Ecluse.Rules.Types` | Rule data types: `Rule`, `EvalContext`, `RuleOutcome`, `Decision`. |
| `Ecluse.Rules` | Rule evaluation and decision rendering: `evalRule`, `evalRules`, `renderDecision`, `renderDuration`. |

Tests mirror this hierarchy within each suite's source dir (e.g. the unit spec
for `Ecluse.Rules` is `test/unit/Ecluse/RulesSpec.hs`).

---

## Testing Strategy

The suite is split into three Cabal components by cost and determinism. The first
two gate merges; the third is allowed to fail by design.

### Unit tests — `ecluse-unit` (gating)

Pure, fast, deterministic `hspec` + `hedgehog` tests covering all pure logic: the
rules engine, parsers, and configuration. No IO, no Docker; they run on every
push and locally in milliseconds. The rules engine is exercised with properties
(deny-by-default invariants, deny-precedence over allows, per-rule predicates).
The credential-provider refresh/cache/expiry policy is unit-tested here too, with
an injected clock and a fake `mintToken`, so only the per-cloud token call itself
is left to integration (see below). Proxy request-lifecycle tests run against an
in-process WAI stub standing in for the private/public upstreams, so the full
fetch → parse → rules → mirror path can be asserted without a network. Run:
`cabal test ecluse-unit`.

### Integration tests — `ecluse-integration` (gating)

Exercise cloud-backed code — the `MirrorQueue` and `CredentialProvider` seams —
against a **real emulator per cloud**, all driven by `testcontainers` (a generic
container manager, not an AWS-specific one):

- **AWS** — a **ministack** container (a lightweight LocalStack alternative);
  `amazonka` is pointed at `http://<container>:4566` with throwaway credentials.
- **GCP** — Google's official **Pub/Sub emulator** container; the client is
  pointed at it via `PUBSUB_EMULATOR_HOST` (the emulator ignores auth). GCP's
  backend — and this test — land once the client-viability spike clears (see
  architecture's [Cloud Backends](docs/architecture.md#cloud-backends)).

Both are hermetic and reproducible — no real cloud account or credentials. They
require a running Docker daemon: CI's `ubuntu-latest` provides one; locally,
install Docker (Nix provides the toolchain but not the daemon, a host concern).
Run: `cabal test ecluse-integration` (or `make test-integration`).

> **Token-mint caveat.** No emulator covers the managed-registry token APIs
> (CodeArtifact's `GetAuthorizationToken`, or GCP's OAuth2 token endpoint). That
> is by design: the only un-emulable part is the per-cloud `mintToken` leaf of the
> `CredentialProvider`, so it is mocked at that seam here, while the generic
> refresh/cache/expiry policy around it is unit-tested with an injected clock; the
> *real* cloud mint runs end-to-end only in the (non-gating) smoke tier. The
> managed registry's npm protocol is just HTTPS+JSON, so it is covered once —
> against a real npm-speaking registry (e.g. Verdaccio) or an in-process WAI stub
> through the `RegistryClient` seam — a deliberate benefit of keeping protocol,
> queue, and credentials as separate seams.

### Smoke tests — `ecluse-smoke` (allowed to fail, non-gating)

Make **live** calls to public registries (npm, PyPI) to confirm our JSON
decoding and protocol handling match reality. Because they depend on
uncontrolled external services, they are **expected to fail occasionally by
design** and never block a merge — the CI `gate` does not depend on them. Treat a
smoke failure as a prompt to investigate (did the upstream protocol drift, or is
it just flakiness?), not an automatic blocker. Run: `cabal test ecluse-smoke`.

This tier is also where the one un-emulable cloud surface is checked end-to-end:
the real per-cloud token *mint* (`CredentialProvider`'s `mintToken`) against the
live cloud. Like the registry calls it needs real external access (here, cloud
credentials), so it is allowed to fail and stays isolated to one small function
per cloud — an accepted residual risk, consistent with the rest of this tier.

**References:** [testcontainers](https://hackage.haskell.org/package/testcontainers) (Haskell, GHC 9.6-compatible) · [ministack](https://github.com/ministackorg/ministack) (local AWS emulator, image `ministackorg/ministack`, port 4566) · [Pub/Sub emulator](https://cloud.google.com/pubsub/docs/emulator) (local GCP emulator, default port 8085).

---

## Continuous Integration

CI is a **single unified workflow graph** ([`.github/workflows/ci.yml`](.github/workflows/ci.yml)).
Every check is a job in that one graph — build & test, format & lint, and Semgrep
static analysis — and they all feed a terminal **gate** job that succeeds only
when every upstream job has succeeded.

- **One required check.** Branch-protection rulesets mark only the `gate` job as
  `Required`. Adding or removing checks never means editing the ruleset: a new
  job simply becomes another dependency of the gate.
- **SHA-pinned actions.** Every `uses:` reference is pinned to a full commit SHA
  (never a tag or branch), with the human-readable version in a trailing
  comment. Dependabot keeps the SHAs current. This stops a re-tagged or
  compromised action from silently entering CI — directly relevant for a
  supply-chain security tool.
- **Semgrep via Nix.** Semgrep runs from the pinned Nix dev shell
  (`nix develop --command semgrep ...`), exactly as developers run it locally,
  rather than from a third-party container image — one fewer unpinned
  supply-chain input.

---

## Repository requirements

- **Workflows stay injection-free.** Never interpolate untrusted
  `${{ github.event.* }}` / `${{ github.head_ref }}` values directly into `run:`
  shell blocks; pass them via `env:` or intermediate files instead.
- **Semgrep ignores require the repo owner's approval.** Do not add
  `.semgrepignore` entries or `nosemgrep` comments unilaterally.
- **Use [Conventional Commits](https://www.conventionalcommits.org/).** Write
  commit subjects as `type(scope): summary`, where `type` is one of `feat`,
  `fix`, `docs`, `chore`, `ci`, `refactor`, `test`, `build`, or `perf` (scope
  optional). Keep the summary short and imperative; put detail in the body.
- **Commits are GPG-signed.** Keep history verifiable.
