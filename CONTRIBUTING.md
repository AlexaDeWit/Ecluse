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
| Coverage (unit → Codecov JSON) | `make coverage` |
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

The *principles* of module organization and namespacing — vertical organization
(types live with their functions), one `Ecluse.<Area>` namespace per area, and
when a `.Types` split is justified — live in [`STYLE.md`](STYLE.md) → "Module
organization". This section records the *current* concrete layout and the one
project-specific pattern below.

- **Seams are records of functions, selected at one composition root.** A
  swappable backend — registry protocol, mirror queue, credential provider — is
  modelled as a record whose fields are functions (the *Handle pattern*), built by
  a per-backend smart constructor (e.g. `newSqsQueue :: SqsConfig -> IO
  MirrorQueue`). Adding a backend means adding a constructor behind the *existing*
  record and wiring it into the single, config-driven composition root — never
  smearing SDK or provider selection across call sites. See
  [Cloud Backends → Seams](docs/architecture/cloud-backends.md#seams-records-of-functions).

Current layout:

| Module | Holds |
|--------|-------|
| `Ecluse.Ecosystem` | The ecosystem tag (`Ecosystem`), shared by the package vocabulary, the version engine, and (later) the registry adapters. It lives alone to break the `Package`↔`Version` import cycle (STYLE.md §4.3). |
| `Ecluse.Version` | Version identity and ordering: `Version`, `VersionKey`, `mkVersion` / `compareVersions` / `parseVersionKey`, and the (private) per-ecosystem parsers — semver, PEP 440, `Gem::Version`. |
| `Ecluse.Package` | The ecosystem-agnostic package vocabulary: `Scope`, `PackageName`, the normalised signals (`CodeExecSignal`, `Trust`, `Availability`), `Artifact`, `Person`, `Dependency`, and `PackageDetails` (which embeds a `Version`), with their smart constructors and renderers. |
| `Ecluse.Rules.Types` | Rule data types: `Rule`, `EvalContext`, `RuleOutcome`, `Decision`. |
| `Ecluse.Rules` | Rule evaluation and decision rendering: `evalRule`, `evalRules`, `renderDecision`, `renderDuration`. |

Tests mirror this hierarchy within each suite's source dir (e.g. the unit specs
for `Ecluse.Rules` and `Ecluse.Version` are `test/unit/Ecluse/RulesSpec.hs` and
`test/unit/Ecluse/VersionSpec.hs`; version ordering additionally has a
differential suite, `Ecluse.VersionOrderingSpec`, against reference oracles).

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
  architecture's [Cloud Backends](docs/architecture/cloud-backends.md#cloud-backends)).

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
(new/changed lines ≥ 80%). Both knobs live in [`codecov.yml`](codecov.yml).

**References:** [testcontainers](https://hackage.haskell.org/package/testcontainers) (Haskell, GHC 9.6-compatible) · [ministack](https://github.com/ministackorg/ministack) (local AWS emulator, image `ministackorg/ministack`, port 4566) · [Pub/Sub emulator](https://cloud.google.com/pubsub/docs/emulator) (local GCP emulator, default port 8085).

---

## Continuous Integration

CI is a **single unified workflow graph** ([`.github/workflows/ci.yml`](.github/workflows/ci.yml)).
Every check is a job in that one graph — build & test, format & lint, and Semgrep
static analysis — and they all feed a terminal **gate** job that succeeds only
when every upstream job has succeeded.

- **Triggers & least-privilege token.** CI runs on pushes to `main`, on every
  PR, and on manual dispatch — a feature-branch push no longer double-runs
  alongside its PR. The workflow's default `GITHUB_TOKEN` is `contents: read`;
  only the build/test job widens it (`id-token: write`) for the tokenless
  Codecov upload.
- **One required check, with one documented exception.** Branch-protection
  rulesets mark only the `gate` job as `Required`; adding or removing an
  *in-workflow* check never means editing the ruleset — a new job simply becomes
  another dependency of the gate. The lone exception is **Codecov**: its
  `project`/`patch` verdicts are computed server-side and surface as their own
  `codecov/project` and `codecov/patch` commit statuses, which cannot be
  expressed as jobs in this graph, so those two are additionally marked
  `Required` (see "Coverage"). Every check that *can* be a job still routes
  through `gate`.
- **Shared, SHA-pinned setup.** Toolchain setup — install Nix, restore the Nix
  store and cabal caches — lives once in the
  [`setup-toolchain`](.github/actions/setup-toolchain/action.yml) composite
  action, so every job shares one definition and the setup actions are pinned
  (and Dependabot-bumped) in one place. Every `uses:` is a full commit SHA with
  the version in a trailing comment; a re-tagged or compromised action can't
  silently enter CI — directly relevant for a supply-chain tool.
- **Lean CI shell + Nix-store cache.** CI enters a slimmed `nix develop .#ci`
  shell (GHC, cabal, the formatter/linter, Semgrep, the version oracles — no IDE
  or release tooling), not the full developer shell. The realized Nix store is
  cached across runs ([`cache-nix-action`](https://github.com/nix-community/cache-nix-action),
  keyed on `flake.nix`/`flake.lock`), so warm runs restore the toolchain from
  the GitHub cache instead of `cache.nixos.org` — faster, and immune to that
  substituter's transient flakiness. `nix.conf` retry knobs (`connect-timeout`,
  `download-attempts`) harden the cold path.
- **Semgrep via Nix.** Semgrep runs from the pinned Nix dev shell
  (`nix develop --command semgrep ...`), exactly as developers run it locally,
  rather than from a third-party container image — one fewer unpinned
  supply-chain input.

---

## Releases & container image

Écluse ships as a lean OCI image built **by Nix**
(`dockerTools.buildLayeredImage`, see [`flake.nix`](flake.nix)), not a Dockerfile.
The image is the stripped binary's runtime closure plus CA certificates and
nothing else — no shell, no package manager, runs **non-root** (uid 65532), and
is **bit-for-bit reproducible** (a fitting property for a supply-chain tool).
Build it locally with `make docker-build` (→ `./result`, a `docker-archive`).

> The image is ~23 MB. A residual chunk (`curl`/`openssl`/`krb5`) rides in via
> the GHC runtime's `libdw` (elfutils) backtrace support, not our code; excising
> it needs a static-musl build (with its own TLS caveats) and is a deliberate
> later trim, not a launch blocker.

Publishing is a separate, tag-triggered workflow
([`.github/workflows/release.yml`](.github/workflows/release.yml)) — **not** part
of the PR `gate`. Pushing a `vX.Y.Z` tag builds the image, pushes it
(`make docker-push`), and attaches keyless provenance + SBOM attestations as
immutable OCI referrers (the GitHub attest-actions; SBOM content from `make sbom`).

**Immutable tags — no `latest`.** The target repo
([`alexadewit/ecluse`](https://hub.docker.com/r/alexadewit/ecluse)) enforces
immutable tags, so every push is a fresh, never-reused tag: the release publishes
`ecluse:X.Y.Z` (from the git tag) and nothing else. There is deliberately no
moving pointer — **pin deployments by digest** (`alexadewit/ecluse@sha256:…`),
which is the stronger supply-chain posture regardless.

### Supply-chain attestations

Each release attaches two **keyless** (Sigstore/OIDC, no stored key) attestations
to the image, bound to its **digest**, recorded in the public **Rekor**
transparency log, and stored as **immutable OCI referrers** — each a
content-addressed, write-once artifact that is never updated, so it can't be
tampered with and it coexists with the repo's immutable tags. Both are produced
in CI by GitHub's [attest-actions](https://github.com/actions/attest-build-provenance):

- **Provenance** (`actions/attest-build-provenance`). SLSA provenance generated
  from the run context — the source repo + commit, the release workflow, and the
  run (the *how/where*). The cryptographic "who built it" guarantee is the
  keyless signing identity (the release workflow's OIDC cert).
- **SBOM** (`actions/attest-sbom`, content from `make sbom`). Generated with
  [`sbomnix`](https://github.com/tiiuae/sbomnix) from the **Nix closure of the
  exact binary the image ships** (`.#ecluse-bin`, stripped/static) — not a scan
  of the image, which couldn't see the statically-linked Haskell deps. So it
  lists the real contents (~23 components: the `ecluse` binary plus its C closure
  — glibc, zlib, and the curl/openssl/krb5 chunk that rides in via the GHC
  runtime's `libdw`) with no dynamic-build noise to trip CVE scanners. The
  Haskell deps are compiled into the `ecluse` component; they are pinned by
  `flake.lock` and, because the image is bit-for-bit reproducible, independently
  derivable.

> **Why the GitHub attest-actions, not cosign?** cosign stores attestations under
> a single mutable `.att` tag (a second attestation must *update* it), which the
> repo's immutable tags forbid — and cosign has no referrer mode for attestations
> at any version (only for signatures). The attest-actions store each attestation
> as its own immutable referrer, so the storage is immutable too. A separate
> image signature is unnecessary: the provenance attestation already binds the
> digest to the builder identity.

Consumers verify by digest with `gh attestation verify`; the recipe lives in the
[README](README.md#verifying-the-image).

**Authentication (Docker Hub).** Docker Hub has no OIDC keyless login, so the push
needs a long-lived token — kept as weak and contained as possible:

- **Per-repo token scoping is not available on a personal account** — only
  account-wide access tokens (choose the *Read & Write* permission level; `Delete`
  is not needed for immutable-tag pushes). True per-repository scoping requires an
  **Organization Access Token**, which needs a Docker org on a paid plan. The
  pragmatic mitigation without paying: put the image under a dedicated **machine
  account** that can reach *only* this repo, so its account-wide token is
  effectively repo-scoped.
- Store it as `DOCKERHUB_USERNAME` / `DOCKERHUB_TOKEN` on a **protected `release`
  GitHub Environment** (required reviewers), so only an approved release job can
  read it. The token is fed via `--password-stdin`, never argv or `echo`.
- Each image carries **keyless provenance + SBOM attestations** via GitHub OIDC
  (`id-token: write` + `attestations: write`) — immutable OCI referrers + the
  Rekor log, no stored key — giving verifiable provenance and contents that
  offset the static-token weakness. Verify with `gh attestation verify` (see
  [README](README.md#verifying-the-image)).

> The `release` environment and its `DOCKERHUB_*` secrets are configured, so a
> `vX.Y.Z` tag (or a `workflow_dispatch`) runs the full build → push → attest
> chain, gated by the environment's required reviewer.

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
