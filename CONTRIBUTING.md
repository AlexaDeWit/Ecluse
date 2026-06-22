# Contributing

This guide covers how we work on **Écluse** (package `ecluse`): local development, codebase
conventions, testing, and CI / repository requirements. For the systems design
— the three-registry model, rules engine, the handles (registry, queue, credential
provider) and cloud backends, and configuration — see
[`docs/architecture.md`](docs/architecture.md). Haskell coding style —
formatting, naming, totality, and the compiler-flag set — has its own reference
in [`STYLE.md`](STYLE.md), and the documentation/Haddock conventions are in
[`HADDOCK.md`](HADDOCK.md). Agent-specific instructions live in
[`AGENTS.md`](AGENTS.md).

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
`nix-check`, …). The underlying commands live in the [`Makefile`](Makefile), so
local and CI never drift.

**Before you push,** run `make check` — it must be clean: build (warnings are
errors via `-Werror`; see [`STYLE.md`](STYLE.md) → "Compiler flags"), the unit
suite, the doctest examples, `fourmolu --mode check`, `hlint`, and Semgrep (zero
findings).
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
when a `.Types` split is justified — live in [`STYLE.md`](STYLE.md) → "Module
organization". This section records the *current* concrete layout and the one
project-specific pattern below.

- **Handles are records of functions, selected at one composition root.** A
  swappable backend — registry protocol, mirror queue, credential provider — is
  modelled as a record whose fields are functions (the *Handle pattern*), built by
  a per-backend smart constructor (e.g. `newSqsQueue :: SqsConfig -> IO
  MirrorQueue`). Adding a backend means adding a constructor behind the *existing*
  record and wiring it into the single, config-driven composition root — never
  smearing SDK or provider selection across call sites. See
  [Cloud Backends → Handles](docs/architecture/cloud-backends.md#handles-records-of-functions).

For the **current module list**, read the module index of the
[published Haddock](https://alexadewit.github.io/Ecluse/) — each module's one-line
summary is its header — and the root [`Ecluse`](src/Ecluse.hs) module's "How the
code is organized" synopsis for the narrative grouping. Both live with the code
and update with it, so they cannot drift; this guide deliberately does not
duplicate the list here.

Tests mirror this hierarchy within each suite's source dir (e.g. the unit specs
for `Ecluse.Rules` and `Ecluse.Version` are `test/unit/Ecluse/RulesSpec.hs` and
`test/unit/Ecluse/VersionSpec.hs`; version ordering additionally has a
differential suite, `Ecluse.VersionOrderingSpec`, against reference oracles).

---

## Testing Strategy

The suite is split into three Cabal components by **what external collaborator the
code under test needs** — and so by determinism: **unit** (none — pure logic or
in-process doubles), **integration** (an *emulable* service, via a container),
**smoke** (an *un-emulable* live service). The first two are hermetic and **gate**
merges; the third makes live calls and is **allowed to fail by design**. One rule
spans all three — see *What gates, and what doesn't* below — so read that before
choosing a new test's tier.

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

Exercise cloud-backed code — the `MirrorQueue` and `CredentialProvider` handles —
against a **real emulator per cloud**, all driven by `testcontainers` (a generic
container manager, not an AWS-specific one):

- **AWS** — a **ministack** container (a lightweight LocalStack alternative);
  `amazonka` is pointed at `http://<container>:4566` with throwaway credentials.
- **GCP** — Google's official **Pub/Sub emulator** container; the client is
  pointed at it via `PUBSUB_EMULATOR_HOST` (the emulator ignores auth). GCP's
  backend — and this test — land once the client-viability spike clears (see
  architecture's [Cloud Backends](docs/architecture/cloud-backends.md#cloud-backends)).

Both are hermetic and reproducible — no real cloud account or credentials. They
require a running Docker daemon: CI's `ubuntu-latest` provides one; locally,
install Docker (Nix provides the toolchain but not the daemon, a host concern).
Run: `cabal test ecluse-integration` (or `make test-integration`).

> **Token-mint caveat.** No emulator covers the managed-registry token APIs
> (CodeArtifact's `GetAuthorizationToken`, or GCP's OAuth2 token endpoint). That
> is by design: the only un-emulable part is the per-cloud `mintToken` leaf of the
> `CredentialProvider`, so it is mocked at that handle here, while the generic
> refresh/cache/expiry policy around it is unit-tested with an injected clock; the
> *real* cloud mint runs end-to-end only in the (non-gating) smoke tier. The
> managed registry's npm protocol is just HTTPS+JSON, so it is covered once —
> against a real npm-speaking registry (e.g. Verdaccio) or an in-process WAI stub
> through the `RegistryClient` handle — a deliberate benefit of keeping protocol,
> queue, and credentials as separate handles.

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

### What gates, and what doesn't

Two things about the split are easy to get backwards, so state them plainly:

- **The integration tier is not "the tier for thorough tests."** What sends a test
  to integration is a collaborator that can only be a *real* (emulated) service — a
  cloud queue, a token mint — not how comprehensive the test is. A cross-component
  test that needs no live external is a **unit** test even when it wires the whole
  pipeline together: the proxy request-lifecycle (fetch → parse → rules → mirror)
  runs against an in-process WAI stub and lives in `ecluse-unit`. Determinism and
  breadth are orthogonal to the tier — put a test wherever its subject can be
  exercised *deterministically*, which is the unit tier far more often than not.

- **The smoke tier is a drift _detector_, never a correctness _guarantee_.** Because
  it depends on uncontrolled external services it cannot gate, so nothing we rely on
  for correctness may live *only* there. Every load-bearing behaviour owes a
  **deterministic, gating mirror** in the unit or integration tier; a smoke test,
  where one exists, only confirms the deterministic model still matches the live
  world. Two cases already follow this shape and are the template to copy:
  - **Version ordering** is gated offline against a committed fixture
    (`Ecluse.VersionOrderingSpec` vs `test/unit/fixtures/version-ordering.txt`); the
    smoke suite (`Ecluse.VersionOraclesSpec`) *additionally* regenerates that fixture
    from the live oracles and runs a generative differential — a check *on* the
    fixture, not the only check.
  - **Credential acquisition** has its refresh/cache/expiry policy unit-tested with
    an injected clock and a fake mint; only the one un-emulable leaf — the real
    per-cloud token mint — runs live in smoke.

  So a slice never discharges an acceptance criterion with a smoke test alone. The
  lone standing case where a behaviour can *only* be observed against a live service
  (the token mint) is an explicitly-accepted residual risk, called out in the slice
  — not the default.

### Coverage — Codecov (gating)

Coverage is measured per **gating** suite and reported to
[Codecov](https://about.codecov.io/). Generation is local and tool-agnostic —
Codecov is only the consumer, so the reporter can be swapped without touching the
build. `make coverage` builds a suite instrumented (HPC, in an isolated
`dist-coverage/` so the normal build cache is untouched), then converts the
`.tix`/`.mix` output to Codecov's native JSON with
[`hpc-codecov`](https://hackage.haskell.org/package/hpc-codecov) — the leanest
format for Codecov to ingest. See [`scripts/coverage.sh`](scripts/coverage.sh).

- **Per-suite flags.** Each tier uploads under its own Codecov *flag*, so one
  combined gate spans the suites while each stays visible. `unit` uploads today;
  `integration` is wired identically (commented in `ci.yml`) and turns on when
  that suite gains real cases. The **smoke** tier is excluded — non-gating and
  network-bound, like everywhere else.
- **Tokenless upload.** CI uploads via GitHub **OIDC** (`use_oidc: true`), so
  there is no `CODECOV_TOKEN` secret to store or leak — the same keyless posture
  as the release image signing.
- **What's measured.** Library code only; `app/` (the thin entry point) and
  `test/` are ignored (see [`codecov.yml`](codecov.yml)).

The gate itself is Codecov's two commit statuses: `codecov/project` (no
regression versus the PR base, within a 1% threshold) and `codecov/patch`
(new/changed lines ≥ 95% — the coverage standard for all newly introduced work;
the project status stays on no-regression so the baseline ratchets up as
well-covered changes land). Both knobs live in [`codecov.yml`](codecov.yml).

**References:** [testcontainers](https://hackage.haskell.org/package/testcontainers) (Haskell, GHC 9.10-compatible) · [ministack](https://github.com/ministackorg/ministack) (local AWS emulator, image `ministackorg/ministack`, port 4566) · [Pub/Sub emulator](https://cloud.google.com/pubsub/docs/emulator) (local GCP emulator, default port 8085).

---

## Continuous Integration

Every push and PR runs the single unified workflow
([`.github/workflows/ci.yml`](.github/workflows/ci.yml)): build & test, integration,
a **Haddock** gate scoped to our own library (`make docs-check` — documents only
`lib:ecluse`, skipping dependency docs and source links; the full source-linked
site is published from `main` via Pages), and a combined **static-checks** job —
fourmolu, hlint, `cabal check`,
Semgrep, and **workflow-lint** (`make lint-workflows` = actionlint + zizmor, which
audit the Actions workflows themselves for correctness and security — template
injection, credential persistence, excessive permissions). The four non-Haskell
checks share one job (they don't build, so a single toolchain setup serves them
all) and each still runs even if a sibling fails, so one run reports them all. All
feed one terminal **`gate`** job — the only required status check (plus Codecov's
server-side `codecov/project` / `codecov/patch`; see
[Coverage](#coverage--codecov-gating)). Local `make check` runs the same set, so a
clean local run predicts a green gate. The design rationale — least-privilege
token, the one-required-check rule, SHA-pinned shared setup, the lean
`nix develop .#ci` shell and Nix-store cache, and Semgrep-via-Nix — is in
[`AGENTS.md`](AGENTS.md) → "CI & Security".

---

## Releases, attestations & vulnerability scanning

Écluse ships as a lean, reproducible OCI image built by Nix (`make docker-build`),
published by a tag-triggered workflow that attaches keyless SLSA provenance + SBOM
attestations and a GitHub Release pinning the digest. Image CVEs are scanned
report-only (`make scan` — grype over the SBOM) and dependency freshness is kept
by Renovate refreshing `flake.lock` (and bumping the GitHub Actions and Haskell
dependencies).

The full operational detail — image contents, the publish/attest chain, Docker
Hub token handling, and the scanning/freshness arms — is in
[Release & Supply-Chain Operations](docs/architecture/release-supply-chain.md).
Consumers verify an image with `gh attestation verify`; the recipe is in the
[README](README.md#verifying-the-image).

---

## AI-assisted contributions

AI-assisted work is welcome, but the bar does not change: **you are the author,
you must understand and be able to explain every line, and the contribution must
be worth more than the time it takes to review.** Low-effort, unreviewed AI
output ("slop") will be closed.

- **Disclose non-trivial AI use.** Editor autocomplete needs no disclosure;
  AI-generated or substantially AI-shaped code, prose, or commits do. Add an
  `Assisted-by:` git trailer naming the tool — e.g.
  `Assisted-by: Claude (Anthropic)` — and mention it in the PR description. This
  records a tool that *helped*; you remain the sole author, so it is **not**
  `Co-authored-by:`.
- **Verify before you file.** Never open an issue — and especially never a
  vulnerability report — that an AI produced and you have not reproduced and
  confirmed yourself (see [`SECURITY.md`](SECURITY.md)).

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
- **Disclose AI assistance.** Mark non-trivial AI-assisted commits with an
  `Assisted-by:` trailer — see [AI-assisted contributions](#ai-assisted-contributions).
