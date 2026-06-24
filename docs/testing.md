# Testing Strategy

The suite is split into three Cabal components by **what external collaborator the
code under test needs** — and so by determinism: **unit** (none — pure logic or
in-process doubles), **integration** (an *emulable* service, via a container),
**smoke** (an *un-emulable* live service). The first two are hermetic and **gate**
merges; the third makes live calls and is **allowed to fail by design**. One rule
spans all three — see *What gates, and what doesn't* below — so read that before
choosing a new test's tier.

## Unit tests — `ecluse-unit` (gating)

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

## Integration tests — `ecluse-integration` (gating)

Exercise cloud-backed code — the `MirrorQueue` and `CredentialProvider` handles —
against a **real emulator per cloud**, all driven by `testcontainers` (a generic
container manager, not an AWS-specific one):

- **AWS** — a **ministack** container (a lightweight LocalStack alternative);
  `amazonka` is pointed at `http://<container>:4566` with throwaway credentials.
- **GCP** — Google's official **Pub/Sub emulator** container; the client is
  pointed at it via `PUBSUB_EMULATOR_HOST` (the emulator ignores auth). GCP's
  backend — and this test — land once the client-viability spike clears (see
  architecture's [Cloud Backends](architecture/cloud-backends.md#cloud-backends)).

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

## Smoke tests — `ecluse-smoke` (allowed to fail, non-gating)

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

## What gates, and what doesn't

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

## Coverage — Codecov (gating)

Coverage is measured per **gating** suite and reported to
[Codecov](https://about.codecov.io/). Generation is local and tool-agnostic —
Codecov is only the consumer, so the reporter can be swapped without touching the
build. `make coverage` builds a suite instrumented (HPC, in an isolated
`dist-coverage/` so the normal build cache is untouched), then converts the
`.tix`/`.mix` output to Codecov's native JSON with
[`hpc-codecov`](https://hackage.haskell.org/package/hpc-codecov) — the leanest
format for Codecov to ingest. See [`scripts/coverage.sh`](../scripts/coverage.sh).

- **Per-suite flags.** Each tier uploads under its own Codecov *flag*, so one
  combined gate spans the suites while each stays visible. Both the `unit` and
  `integration` tiers upload, each under its own flag, and Codecov waits for both
  (`notify.after_n_builds: 2` in `codecov.yml`) before computing the combined
  total — so a partial upload can't fire a transient "coverage decreased" status.
  The **smoke** tier is excluded — non-gating and network-bound, like everywhere
  else.
- **Tokenless upload.** CI uploads via GitHub **OIDC** (`use_oidc: true`), so
  there is no `CODECOV_TOKEN` secret to store or leak — the same keyless posture
  as the release image signing.
- **What's measured.** Library code only; `app/` (the thin entry point) and
  `test/` are ignored (see [`codecov.yml`](../codecov.yml)).

The gate itself is Codecov's two commit statuses: `codecov/project` (no
regression versus the PR base, within a 1% threshold) and `codecov/patch`
(new/changed lines ≥ 85% — a floor that verifies behaviour, not a number to chase;
~95% is a long-term aspiration, not a per-PR gate. The project status stays on
no-regression so the baseline still ratchets up as well-covered changes land).
Both knobs live in [`codecov.yml`](../codecov.yml).

### Coverage gates behaviour, not boilerplate

The 85% patch bar guards **hand-written logic and branches** — where regressions
hide. It is a floor for verifying behaviour, not a number to chase (~95% is a
long-term goal, not a per-PR gate), and three rules keep it honest.

**Don't test a derived instance for its laws.** A `deriving stock (Eq, Show, Ord)`
is lawful by construction; a test that merely exercises it catches a GHC bug, not
ours. A `partial` or uncovered derived line is an **accepted partial** — note it in
the PR and move on. (Likewise a genuinely-unreachable branch behind the
[STYLE §10](../STYLE.md) `error`-escape-hatch: total by construction, accepted.)

**No coverage theater.** Never add a test whose only purpose is to colour a line —
`show x \`shouldSatisfy\` (not . null)` pins nothing and trains the wrong reflex.
This is the *no tautological assertions* bar applied to coverage: a test states a
behaviour or it should not exist. Colouring a derived line green is worse than
leaving it `partial`.

**But guard a derivation that encodes a load-bearing decision.** A derived instance
is lawful, yet the *specific* behaviour it picks can be wrong — and the choice rides
on declaration structure, invisible to the compiler and to
`-Wmissing-deriving-strategies`, so a later "cosmetic" refactor can silently move a
contract. Three axes, one discriminator:

- **Order — `Ord`/`Enum`/`Bounded`.** Derived order is lexicographic by *field*
  (products) or *constructor* (sums) declaration order; reordering them silently
  changes it. When the domain depends on the *specific* order (severity, priority,
  ranges), pin it with a `sort`/`compare` assertion, or route ordering through a
  function. `Version` and `PrecededRule` deliberately do **not** derive `Ord` —
  their order is law-bearing — and `compareVersions` is property-tested against the
  differential oracles above.
- **Equality membership — `Eq`/`Ord`.** Derived equality folds in *every* field;
  sometimes one must be excluded. `PackageName` hand-writes `Eq`/`Ord` over a
  canonical key so the display form never affects identity — a derived instance
  there would be a latent bug.
- **Wire contract — `ToJSON`/`FromJSON`.** A derived JSON instance couples the
  external wire shape to the record's structure, so a field rename or re-nesting is a
  silent breaking change for clients we cannot recompile. For an **owned,
  externally-observable** response, guard it — a golden snapshot of the encoded bytes
  plus a schema round-trip (the
  [OpenAPI contract-drift controls](architecture/api-surface.md#contract-drift-controls):
  `validateToJSON`, the golden manifest), and decode-against-real-fixtures on the
  input side. Better
  still, design the coupling away: the served packument relays the **raw upstream
  `Value`**, edited in place
  ([decision vs served surface](architecture/registry-model.md#decision-surface-vs-served-surface)),
  and the config decoders are hand-written and strict — neither drifts with a refactor.

The discriminator is always: **does an external party or a domain rule depend on the
*specific* shape, or only that *some* instance exists?** An `Ord` used only as a
`Map`/`Set` key needs no test — any consistent order works; the same `Ord` used to
sort by severity does. And prefer designing the coupling away over testing a fragile
derivation — a test on a still-fragile derivation is the weaker guard.

**References:** [testcontainers](https://hackage.haskell.org/package/testcontainers) (Haskell, GHC 9.10-compatible) · [ministack](https://github.com/ministackorg/ministack) (local AWS emulator, image `ministackorg/ministack`, port 4566) · [Pub/Sub emulator](https://cloud.google.com/pubsub/docs/emulator) (local GCP emulator, default port 8085).

