# Testing strategy

What sends a test to a tier is **the external collaborator the code under test needs**,
and so how deterministic it can be. Three kinds:

- **unit** needs no collaborator (pure logic or in-process doubles),
- **integration** needs an *emulable* service, reached through a container,
- **smoke** needs an *un-emulable* live service.

The first two are hermetic and **gate** merges; smoke makes live calls and is **allowed to fail by
design**. Two further gating tiers, residency and end-to-end, sit alongside them, for seven `cabal`
test-suites in all. One rule spans every tier (see *What gates, and what doesn't*), so read that
before choosing a new test's home.

## Unit tests: `ecluse-core-unit`, `ecluse-runtime-unit`, `ecluse-unit` (gating)

Pure, fast, deterministic `hspec` + `hedgehog` tests over all pure logic: the rules engine, parsers,
and configuration. No IO, no Docker; they run on every push in milliseconds. The rules engine is
exercised with properties (deny-by-default, deny-precedence over allows, per-rule predicates). The
credential provider's refresh/cache/expiry policy is unit-tested here with an injected clock and a
fake `mintToken`; the real mint runs only in smoke (see that caveat below). Proxy request-lifecycle
tests run against an in-process WAI stub, so the full fetch → parse → rules → mirror path is asserted
without a network.

The tier is three suites, split by which library a spec may link and enforced by each
suite's `build-depends`:

- **`ecluse-core-unit`** covers `Ecluse.Core.*` (depends on `ecluse-core` only).
- **`ecluse-runtime-unit`** covers the `Ecluse.Runtime.*` capabilities that need no
  application library: the cloud adapters, the telemetry SDK wiring, and logging.
- **`ecluse-unit`** covers the composition shell and the app-tier specs. It depends on
  the `ecluse` app library, so it can drive runtime handles through
  `runServer`/`runWorker`, which is why the `Ecluse.Runtime.Server` and
  `Ecluse.Runtime.Env` specs live here.

Each tests its tree in isolation, mirrored under `core/test/unit`, `runtime/test/unit`,
and `test/unit`. Run all three: `cabal test ecluse-core-unit ecluse-runtime-unit
ecluse-unit`.

## Integration tests: `ecluse-integration` (gating)

Exercise cloud-backed code (the `MirrorQueue` and `CredentialProvider` handles) against a real
emulator, driven by `testcontainers`. The AWS backend runs against a **ministack** container (a
lightweight LocalStack alternative); `amazonka` is pointed at `http://<container>:4566` with throwaway
credentials. The telemetry specs run a real OTLP **Collector** container the same way. Both are
hermetic: no real cloud account or credentials.

They require a running Docker daemon: CI's `ubuntu-latest` provides one; locally, install Docker (Nix
ships the toolchain, not the daemon). Run: `cabal test ecluse-integration` (or
`task test-integration`).

> **Token-mint caveat.** No emulator covers the managed-registry token API (CodeArtifact's
> `GetAuthorizationToken`). The only un-emulable part is the `mintToken` leaf of the
> `CredentialProvider`, so it's mocked at that handle here, while the generic refresh/cache/expiry
> policy around it is unit-tested with an injected clock. The real mint runs end-to-end only in the
> non-gating smoke tier.

## Residency gate: `ecluse-residency` (gating)

The bounded-memory streaming gate streams a 1 MiB and a 100 MiB artifact through the tarball relay
(both the trusted private-hit leg and the gated public leg) and asserts peak live bytes are
**invariant in artifact size** within a fixed margin. It is its own suite, not an
`ecluse-integration` spec, because the measurement needs process isolation and the RTS statistics
flag (`-with-rtsopts=-T`) in its `ghc-options`; it runs outside coverage too. No Docker (loopback WAI
stubs only). Run: `cabal test ecluse-residency` (or `task test-residency`); `task check` includes it
via `cabal-checks`.

## Smoke tests: `ecluse-smoke` (allowed to fail, non-gating)

Make **live** calls to public registries (npm today) to confirm our JSON decoding and protocol
handling match reality. Because they depend on uncontrolled external services, they're expected to
fail occasionally and never block a merge: the CI `gate` doesn't depend on them. Treat a failure as a
prompt to investigate (protocol drift or flakiness?), not a blocker. Run: `cabal test ecluse-smoke`.

This tier is also where the one un-emulable cloud surface runs end-to-end: the real token *mint*
(`CredentialProvider`'s `mintToken`) against the live cloud. It needs real external access, so it's
allowed to fail and stays isolated to one small function, an accepted residual risk.

## End-to-end tests: `ecluse-e2e` (gating)

The only tier that assembles the whole system through the real composition root and drives it with
the real `npm` CLI. It runs the actual published OCI image (`nix build .#dockerImage`), an nginx
public-upstream stub, and a Verdaccio private upstream + mirror target as containers on a Docker
network, then asserts client- and mirror-observable outcomes: an allow-listed package installs; a
rules-denied one is blocked and never mirrored; an installed package round-trips server → worker to
the private mirror; a tampered artifact fails the integrity gate and never publishes. It catches
composition-root and cross-component regressions nothing else does. A served `dist.tarball` is
rewritten to an absolute installable URL under `ECLUSE_SERVER__PUBLIC_URL` (the path-relative form
isn't installable by `npm`).

It **gates** as its own parallel job the CI `gate` depends on. Although far heavier than the rest of
the gate (an image build, multiple containers, the npm CLI), it is hermetic: the nginx and Verdaccio
upstreams are local, so unlike smoke it has no external dependency to flake on, which makes gating
safe. For its weight it is kept out of the local `task gate` and `task check`; run `task test-e2e` on
demand to build the image, load it, and run the suite. It needs a Docker daemon and the npm CLI, and
skips every case as `pending` when `ECLUSE_E2E_IMAGE` is unset.

The egress guard refuses internal addresses on the public path. So the containers run on
the RFC 5737 documentation subnet `203.0.113.0/24`, which the guard treats as external:
the real default-build image runs unmodified, with no production escape hatch.

Because it runs the real `npm` CLI against real packages, it's also where an upstream
lifecycle script (`preinstall`/`install`/`postinstall`/`prepare`) could execute arbitrary
code inside our own CI. So the harness sets `npm_config_ignore_scripts` for **every** npm child it
spawns; the committed root `.npmrc` carries the same `ignore-scripts=true` for in-repo npm and
Renovate but can't reach the throwaway projects outside the repo tree, hence the env var. A gating
case installs a probe whose `postinstall` would write a sentinel and asserts it never appears, so the
guard can't silently rot. `ignore-scripts` skips lifecycle scripts only, so the resilience scenarios
are unaffected.

## OSV advisory fixtures

Advisory-shaped test data has one source of truth: the committed OSV JSON under `test/fixtures/osv/`
(`v1/`, plus the `v2/` delta). Everything a suite consumes is derived from them at test time; an
`osv.db` is **never** committed as a binary, so a fixture can't drift from the artifact contract
(`Ecluse.Core.Osv.Schema`). Helpers in `ecluse-test-support` assemble the osv.dev-shaped zip (plus
*hostile* artifacts for rejection tests) and compile the corpus through the real OSV pipeline
(`Ecluse.Core.Osv.Compile`, in `ecluse-core`, so `ecluse-core-unit` can link it). The corpus is
versioned so shadow-swap tests observe an ETag change and a rule-outcome flip, and
`Ecluse.Test.OsvSpec` pins each version's rows exactly, so editing it updates the pin in the same PR.

## Tests and Docker

The integration and end-to-end tiers are the only ones that start Docker containers: integration
through `testcontainers` (ministack, the OTLP collector), e2e through the raw `docker` harness (the
proxy image plus its nginx/Verdaccio data plane). Both stamp every container with two labels:
`com.ecluse.test` = `integration` | `e2e`, and `com.ecluse.test.scope` = a **per-worktree** id (from
`ECLUSE_TEST_SCOPE`, set by every container-running target: `task test-integration`, `task test-e2e`,
and the `coverage` tier `task check` runs).

Both harnesses tear their own containers down on a normal exit, and the `docker run`s carry `--rm`.
The gap is a **hard kill** (SIGKILL, OOM, a timed-out command), which runs no cleanup and leaves the
topology behind. Two reaping commands close it, both driven by `scripts/test-containers.sh`:

- **`task test-clean`** removes only *this worktree's* test containers and networks (keyed on
  `com.ecluse.test.scope`), so it is safe to run while other worktrees have suites running; the
  container-running targets run it automatically before and after the suite.
- **`task test-clean-all`** removes *every* Écluse test container/network/image on the
  daemon regardless of scope. Reach for it only when no other suite is running.

Inspect what is lingering with `docker ps --filter label=com.ecluse.test`. The label writer
is `Ecluse.Test.Containers`, kept in lock-step with the reaper.

**Every image the test tiers pull is fully digest-pinned (`name@sha256:...`); a mutable tag is never
pulled**, because a tag can be re-pointed to a poisoned image between pulls while a digest is
immutable. It's enforced as a *type*: a pull site accepts only a validated `PinnedImageRef`
(`Ecluse.Test.Container.Image`), so an unpinned pull is unrepresentable and aborts the suite before
pulling. The pins live at the harness sites naming each image (`test/e2e/…/Harness/Docker.hs`;
`test/integration/…/Ministack.hs` and the telemetry specs), each with a comment recording the
human-readable version. To absorb Docker Hub throttling on the shared runners, the CI jobs warm those
exact references first via `scripts/docker-prepull.sh`; the `ci.yml` comments own that rationale.

## What gates, and what doesn't

Two things are easy to get backwards:

- **The integration tier is not "the tier for thorough tests."** What sends a test to integration is
  a collaborator that can only be a *real* (emulated) service, not how broad the test is. A
  cross-component test that needs no live external is a **unit** test even when it wires the whole
  pipeline: the proxy request-lifecycle runs against an in-process WAI stub in `ecluse-unit`. Put a
  test wherever its subject can be exercised *deterministically*.
- **The smoke tier is a drift *detector*, never a correctness *guarantee*.** Because it depends on
  uncontrolled external services it can't gate, so nothing we rely on for correctness may live *only*
  there. Every load-bearing behaviour owes a deterministic, gating mirror in the unit or integration
  tier; a smoke test only confirms the model still matches the live world. Version ordering is the
  template: gated offline against a committed fixture, with the smoke suite additionally regenerating
  that fixture from the live oracles as a differential check.

Beyond the test tiers, two static-analysis jobs gate. **`weeder`** reports library code not reachable
from the entry point (`Ecluse.run`); **`stan`** runs HIE-based partial-function and bug analysis at
the floor in `.stan.toml`. Each is its own parallel job the CI `gate` depends on, and a finding above
its floor blocks the merge. Among the always-on jobs, only `smoke` is non-gating.

## Coverage: Codecov (gating)

Coverage is measured per **gating** suite and reported to
[Codecov](https://about.codecov.io/). Generation is local and tool-agnostic: a suite is
built instrumented (HPC, in an isolated `dist-coverage/` so the normal build cache is
untouched), then `hpc-codecov` converts the `.tix`/`.mix` output to Codecov's native JSON.
`scripts/coverage.sh` produces one tier; the merged view is assembled inline by the
Taskfile `coverage` task.

**Codecov is the merged authority; `task coverage` reproduces it.** Codecov merges the per-flag
uploads into one project total, so a single tier's number *under-counts* the modules the others
exercise (the SQS `MirrorQueue` and the worker's fetch/publish path are covered only by integration).
**`task coverage`** runs the three instrumented unit suites plus `ecluse-integration`,
`hpc combine --union`s them into `coverage/combined.json`, and so agrees with the dashboard; because
it runs the integration tier it **needs a Docker daemon** (with none it fails, pointing at the fast
path). For a quick, Docker-free loop, **`task coverage-unit`** (default `SUITE=ecluse-unit`, or
another suite) measures one tier and loudly prints that it is a partial view.

**What CI uploads.** The build-test job runs `task cabal-checks` (which runs `task coverage`). As a
byproduct that writes four per-suite JSONs: `ecluse-core-unit`, `ecluse-runtime-unit`, and
`ecluse-unit` (all under the Codecov flag `unit`), and `ecluse-integration` (flag `integration`),
each uploaded under its flag. Codecov waits for all four (`notify.after_n_builds: 4` in
[`codecov.yml`](../codecov.yml)) before computing the total, so a partial upload can't fire a
transient "coverage decreased" status. The **smoke** and **e2e** tiers upload nothing: they aren't
built with HPC, so a line exercised only by them reads as uncovered. Don't reason "the e2e test
covers it"; a path that needs coverage needs a unit or integration test.

The combined command removes a *reporting* confusion (a local single-tier read disagreeing with the
merged dashboard); it doesn't paper over gaps. If the *merged* report still shows a module's error
arms red (e.g. `Worker.hs`'s fail-closed integrity-mismatch branch), that is a genuine uncovered
path a test owes.

The gate is Codecov's two commit statuses, both in [`codecov.yml`](../codecov.yml): `codecov/project`
(no regression versus the PR base, within a 1% threshold) and `codecov/patch` (new/changed lines ≥
85%, a floor that verifies behaviour rather than a number to chase). Uploads use GitHub **OIDC**
(`use_oidc: true`), so there's no `CODECOV_TOKEN` to leak. Library code only is measured; `app/**`,
`bench/**`, and `test/**` are excluded, and every `Ecluse.Test.*` module of `ecluse-test-support` is
dropped from the HPC report too. Which derived instances the 85% patch bar treats as accepted
partials is decided in [`STYLE.md`](../STYLE.md) → "Data types and deriving".

**References:** [testcontainers](https://hackage.haskell.org/package/testcontainers) ·
[ministack](https://github.com/ministackorg/ministack) (local AWS emulator, image
`ministackorg/ministack`, port 4566).
