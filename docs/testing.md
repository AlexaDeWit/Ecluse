# Testing Strategy

The suite is split into three Cabal components by **what external collaborator the code under
test needs**, and so by determinism: **unit** (none: pure logic or in-process doubles),
**integration** (an *emulable* service, via a container), **smoke** (an *un-emulable* live
service). The first two are hermetic and **gate** merges; the third makes live calls and is
**allowed to fail by design**. One rule spans all three (see *What gates, and what doesn't*
below), so read that before choosing a new test's tier.

## Unit tests: `ecluse-unit` (gating)

Pure, fast, deterministic `hspec` + `hedgehog` tests covering all pure logic: the rules
engine, parsers, and configuration. No IO, no Docker; they run on every push and locally in
milliseconds. The rules engine is exercised with properties (deny-by-default invariants,
deny-precedence over allows, per-rule predicates). The credential-provider
refresh/cache/expiry policy is unit-tested here too, with an injected clock and a fake
`mintToken`, so only the per-cloud token call itself is left to integration (see below).
Proxy request-lifecycle tests run against an in-process WAI stub standing in for the
private/public upstreams, so the full fetch → parse → rules → mirror path can be asserted
without a network. Run: `cabal test ecluse-unit`.

The unit tier is three Cabal suites, split by which library a spec may link and enforced by
each suite's `build-depends`: **`ecluse-core-unit`** covers `Ecluse.Core.*` (it depends on
`ecluse-core` only); **`ecluse-runtime-unit`** covers the `Ecluse.Runtime.*` capabilities
that need no application library (the cloud adapters, the telemetry SDK wiring, and logging;
it depends on `ecluse-runtime`); and **`ecluse-unit`** covers the composition shell and the
app-tier specs that exercise it (it depends on the `ecluse` app library, so it can also drive
runtime handles through `runServer`/`runWorker`, which is why the `Ecluse.Runtime.Server` and
`Ecluse.Runtime.Env` specs live here rather than in `ecluse-runtime-unit`). Each tests its
tree in isolation, mirrored under `core/test/unit`, `runtime/test/unit`, and `test/unit`. Run
all three with `cabal test ecluse-core-unit ecluse-runtime-unit ecluse-unit`.

## Integration tests: `ecluse-integration` (gating)

Exercise cloud-backed code (the `MirrorQueue` and `CredentialProvider` handles) against a
**real emulator per cloud**, all driven by `testcontainers` (a generic container manager, not
an AWS-specific one):

- **AWS**: a **ministack** container (a lightweight LocalStack alternative); `amazonka` is
  pointed at `http://<container>:4566` with throwaway credentials.
- **GCP**: Google's official **Pub/Sub emulator** container; the client is pointed at it via
  `PUBSUB_EMULATOR_HOST` (the emulator ignores auth). GCP's backend, and this test, land once
  the client-viability spike clears (see architecture's
  [Cloud Backends](architecture/cloud-backends.md#cloud-backends)).

Both are hermetic and reproducible: no real cloud account or credentials. They require a
running Docker daemon: CI's `ubuntu-latest` provides one; locally, install Docker (Nix
provides the toolchain but not the daemon, a host concern). Run: `cabal test
ecluse-integration` (or `task test-integration`).

## Residency gate: `ecluse-residency` (gating)

S54's bounded-memory streaming gate: streams a 1 MiB and a 100 MiB artifact through the
tarball relay (both the trusted private-hit leg and the gated public leg) and asserts peak
live bytes are **invariant in artifact size** within a fixed margin, the correctness
counterpart to the load bench's inform-only residency trend. It is its own suite, not an
`ecluse-integration` spec, because the measurement demands process isolation (live-bytes
sampling must not share a heap with hundreds of other examples) and the RTS statistics flag
(`-with-rtsopts=-T`) baked into its `ghc-options`. It runs outside coverage for the same
reason. No Docker needed (loopback WAI stubs only). Run: `cabal test ecluse-residency` (or
`task test-residency`); `task check` includes it via `cabal-checks`.

> **Token-mint caveat.** No emulator covers the managed-registry token APIs (CodeArtifact's
> `GetAuthorizationToken`, or GCP's OAuth2 token endpoint). That's by design: the only
> un-emulable part is the per-cloud `mintToken` leaf of the `CredentialProvider`, so it's
> mocked at that handle here, while the generic refresh/cache/expiry policy around it is
> unit-tested with an injected clock; the *real* cloud mint runs end-to-end only in the
> (non-gating) smoke tier. The managed registry's npm protocol is just HTTPS+JSON, so it's
> covered once, against a real npm-speaking registry (e.g. Verdaccio) or an in-process WAI
> stub through the bundle's publish capability, a deliberate benefit of keeping protocol,
> queue, and credentials as separate handles.

## Smoke tests: `ecluse-smoke` (allowed to fail, non-gating)

Make **live** calls to public registries (npm, PyPI) to confirm our JSON decoding and
protocol handling match reality. Because they depend on uncontrolled external services,
they're **expected to fail occasionally by design** and never block a merge: the CI `gate`
doesn't depend on them. Treat a smoke failure as a prompt to investigate (did the upstream
protocol drift, or is it just flakiness?), not an automatic blocker. Run: `cabal test
ecluse-smoke`.

This tier is also where the one un-emulable cloud surface is checked end-to-end: the real
per-cloud token *mint* (`CredentialProvider`'s `mintToken`) against the live cloud. Like the
registry calls it needs real external access (here, cloud credentials), so it's allowed to
fail and stays isolated to one small function per cloud, an accepted residual risk,
consistent with the rest of this tier.

## End-to-end tests: `ecluse-e2e` (gating)

The only tier that assembles the **whole system through the real composition root** and
drives it with the **real `npm` CLI**. It runs the actual published OCI image
(`nix build .#dockerImage`), an nginx public-upstream stub, and a Verdaccio private
upstream + mirror target as containers on a docker network, then asserts client- and
mirror-observable outcomes: an allow-listed package installs; a rules-denied one is blocked
and never mirrored; an installed package round-trips server→worker to the private mirror; a
tampered artifact fails the integrity gate and never publishes. It is the tier that catches
**composition-root and cross-component** regressions nothing else does, its first run found
that the path-relative `dist.tarball` rewrite was not installable by `npm` (now fixed by
`ECLUSE_SERVER__PUBLIC_URL`).

It **gates**, it runs as its own parallel job that the CI `gate` depends on. Although it is
far heavier than the rest of the gate (an image build, multiple containers, the npm CLI), it
is **hermetic**, the nginx + Verdaccio upstreams are local, so unlike the live-registry smoke
tier it has no external dependency to flake on, and has been reliably green, which is what
makes gating on it safe. It runs on every PR and nightly. Because of its weight it is kept out
of the local `task gate` and `task check` targets (run `task test-e2e` on demand).

The egress guard refuses internal addresses on the public path. To ensure the tests run correctly, the containers run on an RFC 5737 documentation subnet (`203.0.113.0/24`), which the guard treats as external. This approach avoids any production escape hatches, allowing the real default-build image to run unmodified. 

Run `task test-e2e` to build the image, load it, and run the suite. This command requires a Docker daemon and the npm CLI. It skips tests (marking every case as `pending`) when `ECLUSE_E2E_IMAGE` is unset.

Because this is the only tier that runs the **real `npm` CLI** against real packages, it is
also where an upstream lifecycle script (`preinstall`/`install`/`postinstall`/`prepare`/…)
could execute arbitrary code inside our own CI. For a supply-chain-security tool that is
unacceptable, so the harness disables lifecycle scripts for **every** npm child it spawns by
setting `npm_config_ignore_scripts` in the child environment (the committed root `.npmrc`
carries the same `ignore-scripts=true` for in-repo npm and Renovate, but cannot reach the
harness's throwaway projects outside the repo tree). A gating case installs a probe project
whose own `postinstall` would write a sentinel and asserts the sentinel never appears, so the
guard cannot silently rot. `ignore-scripts` skips lifecycle scripts only; it does not change
install/fetch/lockfile behaviour, so the resilience scenarios are unaffected. The version
oracles (`test/oracles`) need no such guard: their `node_modules` is materialised by Nix from
the lockfile (`importNpmLock`), a pure materialisation that runs no npm CLI and no scripts.

## OSV advisory fixtures

Advisory-shaped test data has one source of truth: the committed OSV JSON files under
`test/fixtures/osv/` (`v1/`, plus the `v2/` delta). Everything a suite consumes is derived
from them at test time; an `osv.db` is **never** committed as a binary, so a fixture cannot
drift from the artifact contract (`Ecluse.Core.Osv.Schema`).

- `Ecluse.Test.Osv` (`ecluse-test-support`) assembles the osv.dev-shaped zip in memory
  (`osvCorpusZip`) and hand-builds *hostile* artifacts (a wrong schema epoch, a view
  shadowing the ranges table), the tampered files the real compiler must never be able to
  produce, for the reader's rejection tests.
- `Ecluse.Test.OsvDb` (`ecluse-test-support`) compiles the corpus into a real artifact
  through the OSV producer's actual pipeline (`withFixtureOsvDb`). The compiler
  (`Ecluse.Core.Osv.Compile`) lives in `ecluse-core` and takes no live telemetry handle, so
  this helper is ecosystem-agnostic and the `ecluse-core-unit` partition can link it: the
  OSV compile specs exercise the real artifact from the core tier, while a rule-evaluation
  spec can still test against a pure fake lookup.
- The corpus is versioned: `CorpusV2` adds an advisory for a package `CorpusV1` leaves
  clean, so shadow-swap tests can observe both an ETag change and a rule-outcome flip.
- `Ecluse.Test.OsvSpec` pins each version's compiled rows exactly; editing the corpus means
  updating the pin in the same PR, deliberately.

## Tests and Docker

The integration (`ecluse-integration`) and end-to-end (`ecluse-e2e`) tiers are the only ones
that start Docker containers: integration through `testcontainers` (ministack, the OTLP
collector), e2e through the raw `docker` harness (`test/e2e/Ecluse/E2E/Harness/Docker.hs`,
the proxy image plus its nginx/Verdaccio/ministack data plane). Both stamp every container
they create with two labels:

- `com.ecluse.test` = `integration` | `e2e`, marking it as an Écluse test container; and
- `com.ecluse.test.scope` = a **per-worktree** id (from `ECLUSE_TEST_SCOPE`, set from
  `scripts/test-containers.sh scope` by every container-running target: `task test-integration`,
  `task test-e2e`, and the `coverage` tier that `task check` runs).

Under a normal exit both harnesses tear their own containers down (a `bracket` in the e2e
harness, `withContainers` in the integration tier), and the `docker run`s carry `--rm` so a
container that crashes on its own is removed too. The gap is a **hard kill** (SIGKILL, an
OOM, or an agent/CI harness killing a timed-out command), which runs no cleanup and leaves
the whole topology behind. Repeated across a battery of runs that is how a machine ends up
with hundreds of orphaned containers.

Two reaping commands close that gap, both driven by `scripts/test-containers.sh`:

- **`task test-clean`** removes only **this worktree's** test containers, networks (and, in
  `--all` mode, build images), keyed on the `com.ecluse.test.scope` label. Because it is
  scoped it is safe to run while other agents or worktrees have suites running; it cannot
  touch theirs. `task test-integration`, `task test-e2e`, and the `coverage` tier on the
  `task check` path run this scoped reap automatically before the suite (sweeping this
  worktree's strays from a previous killed run) and again on exit.
- **`task test-clean-all`** removes **every** Écluse test container/network/image on the
  daemon regardless of scope. Reach for it only when you know no other suite is running; it
  will remove a parallel worktree's live containers.

Inspect what is currently lingering with `docker ps --filter label=com.ecluse.test` or
`bash scripts/test-containers.sh list` (which groups them by scope). The label writer is
`Ecluse.Test.Containers`, kept in lock-step with the reaper's label spelling.

**Every image the test tiers pull is fully digest-pinned (`name@sha256:...`); a mutable tag
is never pulled.** Écluse is a supply-chain-security tool, so this is an invariant, not a
convenience: a tag can be re-pointed to a poisoned image between one pull and the next, while
a `@sha256:` digest is immutable and content-addressed, so the bytes are verified on every
pull.

The invariant is a *type*, not a scan. A pulled image is a `PinnedImageRef`
(`Ecluse.Test.Container.Image`) whose constructor is hidden, reached only through the
validating `mkPinnedImageRef`, which accepts `name@sha256:<64 lowercase hex>` and rejects a
bare tag. Every `docker run` / `docker build FROM` site takes an `ImageRef`, either
`PinnedExternal` for an image pulled from a registry (which must be pinned) or `LocallyBuilt`
for the product image the run builds itself (never pulled, so never pinned), and renders it to
a string only at the call to `docker`. Because a pull site accepts only a validated
`PinnedImageRef`, an unpinned pull is unrepresentable there rather than something to detect
after the fact; each harness resolves its raw literals through `mkPinnedImageRef` at startup
and fails loudly on an invalid one, so an unpinned literal aborts the suite (in CI) before it
pulls anything. The pins live at the harness sites that name each image
(`test/e2e/.../Harness/Docker.hs` for the e2e data plane; `test/integration/.../Ministack.hs`
and the telemetry specs for the integration tier), each with an adjacent comment recording the
human-readable version the digest resolves from, so a reader (and Renovate) can still see what
it is.

Both tiers pull those pinned images (ministack, the OTLP collector, nginx, Verdaccio) from
Docker Hub at run time. On the shared, unauthenticated GitHub-hosted runners that pool is
heavily throttled, and an intermittent auth-token timeout on a single pull inside a suite's
setup hook is enough to redden the whole gating job. To absorb that blip, the CI e2e and
integration jobs warm those images into the local cache before the suite runs, via
`scripts/docker-prepull.sh`, which pulls each reference with bounded exponential-backoff
retries; the harness's own `docker run` / `docker build FROM` then reuse the cached image. It
is best-effort (a still-failing pull only warns and lets the suite try again), and the
references it is given mirror the harness digest pins verbatim, so it warms the exact image
the suite will pull. The pre-pull list is a cache-warming convenience, not the trust boundary:
that is the typed harness pull above, so a stray tag in the pre-pull refs would be a cache miss,
never a way to slip an unpinned image past the suite.

## What gates, and what doesn't

Two things about the split are easy to get backwards, so I'll state them plainly:

- **The integration tier is not "the tier for thorough tests."** What sends a test to
  integration is a collaborator that can only be a *real* (emulated) service, a cloud queue,
  a token mint, not how broad the test is. A cross-component test that needs no live
  external is a **unit** test even when it wires the whole pipeline together: the proxy
  request-lifecycle (fetch → parse → rules → mirror) runs against an in-process WAI stub and
  lives in `ecluse-unit`. Determinism and breadth are orthogonal to the tier: put a test
  wherever its subject can be exercised *deterministically*, which is the unit tier far more
  often than not.

- **The smoke tier is a drift _detector_, never a correctness _guarantee_.** Because it
  depends on uncontrolled external services it can't gate, so nothing we rely on for
  correctness may live *only* there. Every load-bearing behaviour owes a **deterministic,
  gating mirror** in the unit or integration tier; a smoke test, where one exists, only
  confirms the deterministic model still matches the live world. Two cases already follow this
  shape and are the template to copy:
  - **Version ordering** is gated offline against a committed fixture
    (`Ecluse.VersionOrderingSpec` vs `core/test/unit/fixtures/version-ordering.txt`); the smoke
    suite (`Ecluse.VersionOraclesSpec`) *additionally* regenerates that fixture from the live
    oracles and runs a generative differential, a check *on* the fixture, not the only check.
  - **Credential acquisition** has its refresh/cache/expiry policy unit-tested with an
    injected clock and a fake mint; only the one un-emulable leaf, the real per-cloud token
    mint, runs live in smoke.

  So a slice never discharges an acceptance criterion with a smoke test alone. The lone
  standing case where a behaviour can *only* be observed against a live service (the token
  mint) is an explicitly-accepted residual risk, called out in the slice, not the default.

Beyond the test tiers, two static-analysis jobs gate as well. **`weeder`** reports library
code not reachable from the application entry point (`Ecluse.run`); **`stan`** runs HIE-based
partial-function and potential-bug analysis at the floor set in `.stan.toml`. Each runs as its
own parallel job that the CI `gate` depends on, and any finding above its floor fails the job
and blocks the merge. Among the always-on jobs, only `smoke` (live registries) stays
non-gating.

## Coverage: Codecov (gating)

Coverage is measured per **gating** suite and reported to
[Codecov](https://about.codecov.io/). Generation is local and tool-agnostic: Codecov is only
the consumer, so the reporter can be swapped without touching the build. Generation builds a
suite instrumented (HPC, in an isolated `dist-coverage/` so the normal build cache is
untouched), then converts the `.tix`/`.mix` output to Codecov's native JSON with
[`hpc-codecov`](https://hackage.haskell.org/package/hpc-codecov), the leanest format for
Codecov to ingest. See [`scripts/coverage.sh`](../scripts/coverage.sh) (one tier) and
[`scripts/coverage-combined.sh`](../scripts/coverage-combined.sh) (the merged view).

- **Codecov is the merged authority; `task coverage` reproduces it.** Codecov merges the
  per-tier flag uploads into one project total (unit ∪ integration), so a single tier's number
  *under-counts* every module the other tier exercises, the SQS `MirrorQueue` backend and the
  worker's real fetch/publish path are covered only by the integration tier. The canonical
  local command, **`task coverage`**, reproduces Codecov's merged total: it runs both gating
  unit suites plus the `ecluse-integration` suite (which hits the SQS mirror queues). The
  local `task coverage` therefore **agrees with the dashboard**. Because it runs the integration
  tier it **needs a running Docker daemon** (the ministack containers, exactly like the suite
  itself); with no daemon it fails with a clear message pointing at the fast path below. For a
  quick, Docker-free loop, **`task coverage-unit`** (or `task coverage SUITE=ecluse-unit`)
  measures the unit tier only and **loudly prints that it is a partial view** Codecov merges
  with the integration tier, so a single-tier read is never mistaken for the whole picture.
- **Per-suite flags (what CI uploads).** Each tier uploads under its own Codecov *flag*, so one
  combined gate spans the suites while each stays visible. CI runs the per-tier form
  (`task coverage SUITE=ecluse-unit` and `task coverage SUITE=ecluse-integration`) so each flag
  gets its own JSON; both the `unit` and `integration` tiers upload, and Codecov waits for both
  (`notify.after_n_builds: 2` in `codecov.yml`) before computing the combined total, so a
  partial upload can't fire a transient "coverage decreased" status. The **smoke** tier is
  excluded: non-gating and network-bound, like everywhere else.
- **Reporting divergence vs. real gaps.** This combined command exists to remove a *reporting*
  confusion, a local single-tier read disagreeing with the merged dashboard, not to paper
  over real coverage gaps. If the **merged** report still shows a module's error arms red (e.g.
  `Worker.hs`'s fail-closed integrity-mismatch branch), that is a *genuine* uncovered path the
  tests owe, not a reporting artifact: it is fixed by a test, not by this tooling. Keep the two
  distinct.
- **Tokenless upload.** CI uploads via GitHub **OIDC** (`use_oidc: true`), so there's no
  `CODECOV_TOKEN` secret to store or leak, the same keyless posture as the release image
  signing.
- **What's measured.** Library code only; `app/` (the thin entry point) and `test/` are
  ignored (see [`codecov.yml`](../codecov.yml)). The shared `ecluse-test-support` library
  (`test/support/`, the `Ecluse.Test.*` modules) is excluded end to end: it is a first-class
  cabal component, so the instrumented build measures it, but it is test scaffolding rather than
  software under test. Generation drops every `Ecluse.Test.*` module from the HPC report (the
  `hpc-codecov` exclusions in [`scripts/coverage.sh`](../scripts/coverage.sh) and
  [`scripts/coverage-combined.sh`](../scripts/coverage-combined.sh), derived from `test/support/`
  so a new module is dropped automatically), and `codecov.yml` `ignore` lists `test/support/**`
  as a second, report-side line of defence.

The gate itself is Codecov's two commit statuses: `codecov/project` (no regression versus the
PR base, within a 1% threshold) and `codecov/patch` (new/changed lines ≥ 85%, a floor that
verifies behaviour, not a number to chase; ~95% is a long-term aspiration, not a per-PR gate.
The project status stays on no-regression so the baseline still ratchets up as well-covered
changes land). Both knobs live in [`codecov.yml`](../codecov.yml).

### Coverage gates behaviour, not boilerplate

The 85% patch bar guards **hand-written logic and branches**, where regressions hide. It's a
floor for verifying behaviour, not a number to chase (~95% is a long-term goal, not a per-PR
gate), and three rules keep it honest.

**Don't test a derived instance for its laws.** A `deriving stock (Eq, Show, Ord)` is lawful
by construction; a test that merely exercises it catches a GHC bug, not ours. A `partial` or
uncovered derived line is an **accepted partial**: note it in the PR and move on. (Likewise a
genuinely-unreachable branch behind the [STYLE §10](../STYLE.md) `error`-escape-hatch: total
by construction, accepted.)

**No coverage theater.** Never add a test whose only purpose is to colour a line:
`show x \`shouldSatisfy\` (not . null)` pins nothing and trains the wrong reflex. This is the
*no tautological assertions* bar applied to coverage: a test states a behaviour or it
shouldn't exist. Colouring a derived line green is worse than leaving it `partial`.

**But guard a derivation that encodes a load-bearing decision.** A derived instance is
lawful, yet the *specific* behaviour it picks can be wrong, and the choice rides on
declaration structure, invisible to the compiler and to `-Wmissing-deriving-strategies`, so a
later "cosmetic" refactor can silently move a contract. Three axes, one discriminator:

- **Order (`Ord`/`Enum`/`Bounded`).** Derived order is lexicographic by *field* (products) or
  *constructor* (sums) declaration order; reordering them silently changes it. When the domain
  depends on the *specific* order (severity, priority, ranges), pin it with a `sort`/`compare`
  assertion, or route ordering through a function. `Version` and `PrecededRule` deliberately do
  **not** derive `Ord`, because their order is law-bearing, and `compareVersions` is
  property-tested against the differential oracles above.
- **Equality membership (`Eq`/`Ord`).** Derived equality folds in *every* field; sometimes one
  must be excluded. `PackageName` hand-writes `Eq`/`Ord` over a canonical key so the display
  form never affects identity; a derived instance there would be a latent bug.
- **Wire contract (`ToJSON`/`FromJSON`).** A derived JSON instance couples the external wire
  shape to the record's structure, so a field rename or re-nesting is a silent breaking change
  for clients we can't recompile. For an **owned, externally-observable** response, guard it:
  derive the documented schema from the *same* codec as the wire instances so the two cannot
  diverge (the [OpenAPI capability manifest](architecture/api-surface.md#how-its-built-and-published)
  takes this `autodocodec` route, its assembly covered by ordinary unit tests), and
  decode-against-real-fixtures on the input side.
  Better still, design the coupling away: the served packument relays the **raw upstream
  `Value`**, edited in place
  ([decision vs served surface](architecture/registry-model.md#decision-surface-vs-served-surface)),
  and the config decoders are hand-written and strict, so neither drifts with a refactor.

The discriminator is always: **does an external party or a domain rule depend on the
*specific* shape, or only that *some* instance exists?** An `Ord` used only as a `Map`/`Set`
key needs no test (any consistent order works); the same `Ord` used to sort by severity does.
And prefer designing the coupling away over testing a fragile derivation: a test on a
still-fragile derivation is the weaker guard.

**References:** [testcontainers](https://hackage.haskell.org/package/testcontainers) (Haskell, GHC 9.10-compatible) · [ministack](https://github.com/ministackorg/ministack) (local AWS emulator, image `ministackorg/ministack`, port 4566) · [Pub/Sub emulator](https://cloud.google.com/pubsub/docs/emulator) (local GCP emulator, default port 8085).
