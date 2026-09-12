# Testing strategy

Where a test belongs, what each tier gates, and how CI measures coverage. One thing decides a
test's tier: **the external collaborator the code under test needs**, and so how deterministic the
test can be. Three kinds:

- **unit** needs no collaborator (pure logic or in-process doubles),
- **integration** needs an *emulable* service, reached through a container,
- **smoke** needs an *un-emulable* live service.

The first two are hermetic and **gate** merges. Smoke makes live calls and is **allowed to fail by
design**. Two further gating tiers, residency and end-to-end, sit alongside them, for seven `cabal`
test-suites in all. One rule spans every tier (see *What gates, and what doesn't*), so read that
before you choose a new test's home.

## Unit tests: `ecluse-core-unit`, `ecluse-runtime-unit`, `ecluse-unit` (gating)

Pure, fast, deterministic `hspec` and `hedgehog` tests over all pure logic: the rules engine,
parsers, and configuration. No IO, no Docker. They run on every push in milliseconds. Properties
exercise the rules engine: deny-by-default, deny-precedence over allows, and per-rule predicates.
This tier tests the credential provider's refresh, cache, and expiry policy with an injected clock
and a fake `mintToken`. The real mint runs only in smoke (see that caveat below).
Proxy request-lifecycle tests run against an in-process WAI stub, so they assert the full
fetch → parse → rules → mirror path without a network.

The tier is three suites, split by which library a spec may link. Each suite's `build-depends`
enforces the split:

- **`ecluse-core-unit`** covers `Ecluse.Core.*` (depends on `ecluse-core` only).
- **`ecluse-runtime-unit`** covers the `Ecluse.Runtime.*` capabilities that need no
  application library: the cloud adapters, the telemetry SDK wiring, and logging.
- **`ecluse-unit`** covers the composition shell and the app-tier specs. It depends on
  the `ecluse` app library, so it can drive runtime handles through
  `runServer`/`runWorker`, which is why the `Ecluse.Runtime.Server` and
  `Ecluse.Runtime.Env` specs live here.

Each tests its tree in isolation, mirrored under `core/test/unit`, `runtime/test/unit`, and
`test/unit`. A spec module is the tested module's full name with `Spec` appended, and its file sits
at the tested module's path under the suite's source directory, library prefix included, so
`Ecluse.Core.Cve.Slot` is tested by `core/test/unit/Ecluse/Core/Cve/SlotSpec.hs`, module
`Ecluse.Core.Cve.SlotSpec`. The prefix stays in the name, so a spec states which library it tests
without the reader knowing which suite holds it. A suite outside the unit tier puts its tier token
before `Spec`: `IntegrationSpec`, `E2ESpec`, `SmokeSpec`, `ResidencySpec`. The integration spec for
`Ecluse.Core.Server.Pipeline.Tarball` is therefore
`Ecluse.Core.Server.Pipeline.TarballIntegrationSpec`, and the unit spec beside it keeps the bare
`Spec`. Run all three: `cabal test ecluse-core-unit ecluse-runtime-unit ecluse-unit`.

## Integration tests: `ecluse-integration` (gating)

Exercise cloud-backed code (the `MirrorQueue` and `CredentialProvider` handles) against a real
emulator, driven by `testcontainers`. The AWS backend runs against a **ministack** container, a
lightweight LocalStack alternative, with `amazonka` pointed at `http://<container>:4566` and
throwaway credentials. The telemetry specs run a real OTLP **Collector** container the same way. Both
are hermetic: no real cloud account, no real credentials.

The tier needs a running Docker daemon. CI's `ubuntu-latest` provides one. Locally, install Docker:
Nix ships the toolchain, not the daemon. Run: `cabal test ecluse-integration` (or
`task test-integration`).

> **Token-mint caveat.** No emulator covers the managed-registry token API (CodeArtifact's
> `GetAuthorizationToken`). The only un-emulable part is the `mintToken` leaf of the
> `CredentialProvider`, so this tier mocks it at that handle. The unit tier covers the generic
> refresh, cache, and expiry policy around it with an injected clock. The real mint runs end-to-end
> only in the non-gating smoke tier.

## Residency gate: `ecluse-residency` (gating)

The bounded-memory streaming gate streams a 1 MiB and a 100 MiB artifact through the tarball relay.
It covers both the trusted private-hit leg and the gated public leg. It asserts that peak live bytes
stay invariant in artifact size within a fixed margin. It is its own suite, not an
`ecluse-integration` spec, because the measurement needs process isolation and the RTS statistics
flag (`-with-rtsopts=-T`) in its `ghc-options`. It runs outside coverage too. No Docker, loopback
WAI stubs only. Run: `cabal test ecluse-residency` (or `task test-residency`). `task check` includes
it via `cabal-checks`.

## Smoke tests: `ecluse-smoke` (allowed to fail, non-gating)

Make live calls to public registries (npm today) to confirm our JSON decoding and protocol handling
match reality. They depend on uncontrolled external services, so an occasional failure is normal
and never blocks a merge. The CI `gate` does not depend on them. Treat a failure as a
prompt to investigate (protocol drift or flakiness?), not a blocker. Run: `cabal test ecluse-smoke`.

This tier is also where the one un-emulable cloud surface runs end-to-end: the real token *mint*
(`CredentialProvider`'s `mintToken`) against the live cloud. It needs real external access, so it is
allowed to fail and stays isolated to one small function, an accepted residual risk.

The telemetry Datadog check lives here too. With Datadog API credentials in the environment, it emits
a uniquely stamped span and metric through the real export path. It then polls the Datadog API until
they appear. It is secret-gated (skipped without credentials) and non-gating, so a Datadog outage or
ingestion lag never blocks a merge. It is the only telemetry check that reaches the Datadog SaaS.

The hermetic span and metric assertions run in `ecluse-integration`. A request drives an in-process
Écluse, and a real Collector container asserts the spans and metrics arrived. The unit tier covers
config parsing, the denial span-attribute mapping, the JSONL scribe, and the metric-label guard.

## End-to-end tests: `ecluse-e2e` (gating)

The only tier that assembles the whole system through the real composition root and drives it with
the real `npm` and `pip` clients. It runs the published OCI image (`nix build .#dockerImage`), an
nginx public-upstream stub, a Verdaccio private upstream and mirror target, and a ministack emulator
for the mirror queue and the advisory store, as containers on a Docker network. It then asserts
client- and mirror-observable outcomes:

- an allow-listed package installs,
- Écluse blocks a rules-denied package and never mirrors it,
- an installed package round-trips server → worker to the private mirror,
- mirroring an older version after a newer one leaves the mirror's `dist-tags.latest` alone,
- a tampered artifact fails the integrity gate and never publishes,
- `pip` installs a wheel from a `pypi` mount in hash-checking mode, pinned to the sha256 the
  served Simple index advertised, so the installed bytes are the advertised ones.

The Dredger cases seed Verdaccio through the proxy and mirror worker, then run the same
image with an identity deny and no advisory database. They cover `--once`, `--dry-run`,
consent refusal, first-party protection, and listing preservation after the final version
is deleted. `Ecluse.DredgerE2ESpec` also drives `listPackagesIn` against the store and compares
complete package-version snapshots with the audit records and cycle counts. Its first-party
fixture holds two versions. These cases do not verify behaviour against a real CodeArtifact repository.

One further Dredger scenario connects advisory compilation to revocation. Pilot compiles the `v1`
corpus through the product image and uploads it to the emulated advisory store. The proxy syncs it,
and `npm` installs both versions of the corpus fixture the `v2` delta condemns, which the worker
mirrors. Pilot then publishes the `v2` generation, the proxy swaps it in, and a candidate-mode
Dredger cycle deletes the affected version under a named `DenyIfCve`. The scenario then confirms the
next install of that version fails, its artifact request is refused with `403` and re-mirrors
nothing, and the stated fix still installs from the store.

It catches composition-root and cross-component regressions nothing else does. The mount rewrites a
served `dist.tarball` to an absolute installable URL under `ECLUSE_SERVER__PUBLIC_URL`, because
`npm` cannot install the path-relative form.

It gates as its own parallel job the CI `gate` depends on. It is far heavier than the rest of the
gate: an image build, multiple containers, and the npm CLI. But it is hermetic. The nginx and
Verdaccio upstreams are local, so unlike smoke it has no external dependency to flake on, which
makes gating safe. Its weight keeps it out of the local `task gate` and `task check`. Run
`task test-e2e` on demand to build the image, load it, and run the suite. It needs a Docker daemon
and the `npm` and `pip` clients, both from the dev shell, and skips every case as `pending` when
`ECLTEST_E2E_IMAGE` is unset.

The egress guard refuses internal addresses on the public path. So the containers run on
the RFC 5737 documentation subnet `203.0.113.0/24`, which the guard treats as external. The real
default-build image runs unmodified, with no production escape hatch.

This tier runs the real `npm` CLI against real packages, so an upstream lifecycle script
(`preinstall`/`install`/`postinstall`/`prepare`) could execute arbitrary code inside our own CI. The
harness therefore sets `npm_config_ignore_scripts` for every npm child it spawns. The committed
root `.npmrc` carries the same `ignore-scripts=true` for in-repo npm and Renovate. That file cannot
reach the throwaway projects outside the repo tree, hence the env var. A gating case installs a
probe whose `postinstall` would write a sentinel, and asserts the sentinel never appears, so the
guard cannot rot silently. `ignore-scripts` skips lifecycle scripts only, so it leaves the
resilience scenarios alone.

The pip case carries the same prohibition. A source distribution runs its own build backend on
install, so the harness passes `--only-binary=:all:` and installs a wheel alone, and it points
`PIP_CONFIG_FILE` at `/dev/null` because `--isolated` still reads the global and site config files.

## Benchmarks (non-gating)

Use the benchmark tier to assess cost alongside the seven Cabal test suites. None of its
three workflows gates a merge or belongs in branch protection as a required check.
The workflow YAML owns schedules and run options.
Each workflow puts its report in the GitHub run summary and uploads the listed files on that run's page.
Reports support manual comparisons only. No workflow stores a cross-run baseline or consumes another run's results.

| Workflow | Measurement | Downloadable report files |
|---|---|---|
| [Work per request](../.github/workflows/bench.yml) | Time and allocations for the benchmark groups over committed and synthetic corpora | `bench-results.csv`, `bench-output.txt` |
| [Performance acceptance](../.github/workflows/perf-acceptance.yml) | Full-document and selective-decode overhead on live registry documents against reviewed budgets | `perf-acceptance-report.md` |
| [Load](../.github/workflows/bench-load.yml) | npm and PyPI throughput and latency through the composed proxy, with separate ecosystem sections and baseline sources | `bench-load-results.md` |

Read a red result according to its measurement:

- Work-per-request benchmarks fail on build errors, harness crashes, or failed complexity assertions. They do not compare performance against regression thresholds.
- Performance acceptance fails on an overhead budget breach. An unavailable live registry produces an unavailable result, not a breach.
  Its report separates upstream time from Écluse overhead. A breach needs a human decision about a code regression or a budget revision.
- Load benchmarks use `oha` against the composed proxy. They fail when the harness cannot boot, `oha` cannot run, or a scenario serves nothing.
  Fixture preflights also fail on an unexpected status, index shape, or wheel body.
  Throughput and latency have no regression threshold. Shared-runner noise and the load run's cost make it unsuitable as a per-PR signal.

Budget values and calibration belong in [acceptance/criteria.json](../acceptance/criteria.json).
Corpus pins and capture policy belong in [bench/corpus/pins.json](../bench/corpus/pins.json).

## Onboarding an ecosystem

An ecosystem counts as onboarded when it supplies each item below for its supported operations.
Register new modules in [ecluse.cabal](../ecluse.cabal) and the applicable harness entry point.
`<Ecosystem>` denotes the module component, such as `Npm` or `PyPI`, and `<ecosystem>` denotes the corpus directory name.
The pending links identify work needed to bring existing ecosystems up to this bar.

| Obligation | Expected file or module pattern | Worked examples and current gaps |
|---|---|---|
| Unit contracts, gating in `ecluse-core-unit` | Mirror `Ecluse.Core.Registry.<Ecosystem>.*` with `core/test/unit/Ecluse/Core/Registry/<Ecosystem>/*Spec.hs`, including the adapter contracts | [npm](../core/test/unit/Ecluse/Core/Registry/Npm/) and [PyPI](../core/test/unit/Ecluse/Core/Registry/PyPI/). The shared [adapter spec](../core/test/unit/Ecluse/Core/Registry/AdapterSpec.hs) pins ecosystem dispatch. |
| Adapter integration, gating in `ecluse-integration` | `test/integration/Ecluse/Core/Registry/<Ecosystem>/AdapterIntegrationSpec.hs` for metadata and artifact routes against local upstreams | [PyPI adapter](../test/integration/Ecluse/Core/Registry/PyPI/AdapterIntegrationSpec.hs). Existing npm coverage lives in [PipelineIntegrationSpec](../test/integration/Ecluse/Core/Server/PipelineIntegrationSpec.hs) and its [pipeline specs](../test/integration/Ecluse/Core/Server/Pipeline/), without a separate adapter module. |
| At least one real-client install, gating in `ecluse-e2e` | `test/e2e/Ecluse/E2E/<Ecosystem>/InstallE2ESpec.hs`, with fixtures under `test/e2e/Ecluse/E2E/Fixtures/<Ecosystem>.hs` | npm and pip installs currently share [E2ESpec.hs](../test/e2e/Ecluse/E2ESpec.hs), using [npm](../test/e2e/Ecluse/E2E/Fixtures/Npm.hs) and [PyPI](../test/e2e/Ecluse/E2E/Fixtures/PyPI.hs) fixtures. [#1304](https://github.com/AlexaDeWit/Ecluse/issues/1304) supplies the per-ecosystem spec layout. |
| Work-per-request instance and corpus | Register an `EcosystemBench` in [Ecluse.Test.EcosystemBench](../test/support/Ecluse/Test/EcosystemBench.hs), with frozen bytes under `bench/corpus/<ecosystem>/`, pins in `bench/corpus/pins.json`, and a synthetic byte generator | npm and PyPI run every metadata group through the shared record. [PyPI captures](../bench/corpus/pypi/) use the shipped PEP 691 Simple JSON format. Generator checks cover decoding, projection, selective reads, and artifact URL rewriting. New instances require no changes to the benchmark groups or report renderer. |
| Performance acceptance budgets | Ecosystem budgets in `acceptance/criteria.json`, consumed by `acceptance/app/Main.hs` using the benchmark corpus | The [driver](../acceptance/app/Main.hs) measures live npm packuments and PyPI PEP 691 Simple JSON documents for the shared corpus identities. Each ecosystem has its own report section. [Criteria](../acceptance/criteria.json) record the budgets and calibration evidence. Frozen capture bytes are not acceptance measurements. |
| Load fixture | `bench/load/Ecluse/BenchLoad/<Ecosystem>.hs` exporting an `UpstreamFixture`, registered in `bench/load/Main.hs` | [npm](../bench/load/Ecluse/BenchLoad/Npm.hs) and [PyPI](../bench/load/Ecluse/BenchLoad/PyPI.hs) run metadata, artifact, and cache scenarios through shared proxy wiring. PyPI checks PEP 691 indices and wheel bodies before load, and uses a labelled configured baseline. Its eviction cache stays below the actual corpus working set. PyPI worker mirroring waits for [#765](https://github.com/AlexaDeWit/Ecluse/issues/765). |

The shared residency gate remains in
[`test/residency/Ecluse/Core/Server/Pipeline/TarballResidencySpec.hs`](../test/residency/Ecluse/Core/Server/Pipeline/TarballResidencySpec.hs).
Extend its cases if an ecosystem adds a distinct artifact relay path.
Live protocol checks use `test/smoke/Ecluse/Core/Registry/<Ecosystem>SmokeSpec.hs`, following
the [npm example](../test/smoke/Ecluse/Core/Registry/NpmSmokeSpec.hs). PyPI has no corresponding protocol smoke module yet.
Smoke coverage never replaces a gating case.

## OSV advisory fixtures

Advisory-shaped test data has one source of truth: the committed OSV JSON under `test/fixtures/osv/`
(`v1/`, plus the `v2/` delta). A suite derives everything it consumes from those files at test time.
No `osv.db` is ever committed as a binary, so a fixture cannot drift from the artifact contract
(`Ecluse.Core.Osv.Schema`). Helpers in `ecluse-test-support` assemble the osv.dev-shaped zip, plus
*hostile* artifacts for rejection tests. They compile the corpus through the real OSV pipeline
(`Ecluse.Core.Osv.Compile`, in `ecluse-core`, so `ecluse-core-unit` can link it). The corpus carries
versions, so shadow-swap tests observe an ETag change and a rule-outcome flip.
`Ecluse.Test.OsvSpec` pins each version's rows exactly, so editing the corpus updates the pin in the
same PR.

## Tests and Docker

The integration and end-to-end tiers are the only ones that start Docker containers. Integration goes
through `testcontainers` (ministack, the OTLP collector). The e2e tier goes through the raw `docker`
harness: the proxy image plus its nginx/Verdaccio data plane. Both stamp every container with two
labels: `com.ecluse.test` = `integration` | `e2e`, and `com.ecluse.test.scope` = a **per-worktree**
id. That id comes from `ECLTEST_SCOPE`, which every container-running target sets:
`task test-integration`, `task test-e2e`, and the `coverage` tier `task check` runs.

Every harness and CI variable uses the `ECLTEST_` prefix, never `ECLUSE_`. The config loader claims
the whole `ECLUSE_` prefix and aborts the boot on any variable under it that is not a config key, so
a harness variable on that prefix would stop the very proxy the tests are booting.

Both harnesses tear their own containers down on a normal exit, and the `docker run`s carry `--rm`.
The gap is a **hard kill** (SIGKILL, OOM, a timed-out command), which runs no cleanup and leaves the
topology behind. Two reaping commands close it, both driven by `scripts/test-containers.sh`:

- **`task test-clean`** removes only *this worktree's* test containers and networks (keyed on
  `com.ecluse.test.scope`), so it is safe to run while other worktrees have suites running. The
  container-running targets run it automatically before and after the suite.
- **`task test-clean-all`** removes *every* Écluse test container/network/image on the
  daemon regardless of scope. Reach for it only when no other suite is running.

Inspect what is lingering with `docker ps --filter label=com.ecluse.test`. The label writer
is `Ecluse.Test.Containers`, kept in lock-step with the reaper.

**Every image the test tiers pull is fully digest-pinned (`name@sha256:...`), and a mutable tag is
never pulled.** A tag can be re-pointed to a poisoned image between pulls, while a digest is
immutable. A *type* enforces it: a pull site accepts only a validated `PinnedImageRef`
(`Ecluse.Test.Container.Image`), so an unpinned pull is unrepresentable and aborts the suite before
pulling. Every pin lives in that same module beside the validator, and a harness names the pin
rather than the digest. To absorb Docker Hub throttling on the shared runners, the CI jobs warm those
exact references first through `scripts/docker-prepull.sh`. The `ci.yml` comments own that rationale.

## What gates, and what doesn't

Two things are easy to get backwards:

- **The integration tier is not "the tier for thorough tests."** A test goes to integration because
  its collaborator can only be a *real* (emulated) service, not because the test is broad. A
  cross-component test that needs no live external service is a **unit** test, even when it wires the
  whole pipeline. The proxy request-lifecycle runs against an in-process WAI stub in `ecluse-unit`.
  Put a test wherever its subject runs *deterministically*.
- **The smoke tier is a drift *detector*, never a correctness *guarantee*.** It depends on
  uncontrolled external services, so it cannot gate, and nothing we rely on for correctness may live
  *only* there. Every load-bearing behaviour owes a deterministic, gating mirror in the unit or
  integration tier. A smoke test only confirms the model still matches the live world. Version
  ordering is the template. The gate checks it offline against a committed fixture, and the smoke
  suite also regenerates that fixture from the live oracles as a differential check.

Beyond the test tiers, two static-analysis jobs gate. **`weeder`** reports library code not reachable
from the entry point (`Ecluse.run`). **`stan`** runs HIE-based partial-function and bug analysis at
the floor in `.stan.toml`. Each is its own parallel job the CI `gate` depends on, and a finding above
its floor blocks the merge. Among the always-on jobs, only `smoke` is non-gating.

A PR that edits documentation only skips the Haskell jobs. The `changes` job classifies it
against an allow-list of documentation paths in
[`scripts/ci-classify-change.sh`](../scripts/ci-classify-change.sh), which fails closed: an
unlisted path runs everything. The static checks run on every PR either way, because the site
build reads the very files such a PR edits, and it fails on a broken internal link or anchor.
The `gate` job accepts a skipped job from that filter and from nothing else, so a job that
silently never ran still fails the gate. Such a PR uploads no coverage, so the required
`codecov/project` status stays pending by design, and the repo owner merges it by
administrator bypass.

The Haddock job wraps its flake checks with `scripts/ci-build-diagnostics.sh`. On Linux,
the wrapper observes output bytes through two `tee` processes and their `/proc` IO counters.
Output silence produces process, memory and disk snapshots on stderr. Snapshots exclude
command arguments and environment variables. Missing diagnostics do not change the build result.
Cancellation stops the command's process group after a two-second grace period.

| Variable | Default | Meaning |
|---|---|---|
| `CI_BUILD_QUIET_SECONDS` | `120` | Seconds without output before a snapshot, also the minimum interval between snapshots |
| `CI_BUILD_POLL_SECONDS` | `1` | Seconds between output checks |
| `CI_BUILD_MAX_SNAPSHOTS` | `20` | Maximum snapshots per invocation |

Each diagnostic command has a five-second deadline and a 201-line output limit.
`task test-scripts` checks output streaming, exit status, snapshot limits and cancellation without a Haskell build.

## Coverage: Codecov (gating)

CI measures coverage per gating suite and reports it to [Codecov](https://about.codecov.io/).
Generation is local and tool-agnostic. A suite is built instrumented: HPC, in an isolated
`dist-coverage/` that leaves the normal build cache alone. Then `hpc-codecov` converts the
`.tix`/`.mix` output to Codecov's native JSON. `scripts/coverage.sh` produces one tier. The Taskfile
`coverage` task assembles the merged view inline.

**Codecov is the merged authority, and `task coverage` reproduces it.** Codecov merges the per-flag
uploads into one project total. A single tier's number therefore *under-counts* the modules the
others exercise: only integration covers the SQS `MirrorQueue` and the worker's fetch/publish path.
`task coverage` runs the three instrumented unit suites plus `ecluse-integration` and
`hpc combine --union`s them into `coverage/combined.json`, so it agrees with the dashboard. It runs
the integration tier, so it needs a Docker daemon. Without one it fails and points at the fast path.
For a quick, Docker-free loop, `task coverage-unit` (default `SUITE=ecluse-unit`, or another suite)
measures one tier and prints loudly that it is a partial view.

**What CI uploads.** The build-test job runs `task cabal-checks`, which runs `task coverage`. That
writes four per-suite JSONs as a byproduct: `ecluse-core-unit`, `ecluse-runtime-unit`, and
`ecluse-unit` (all under the Codecov flag `unit`), and `ecluse-integration` (flag `integration`). CI
uploads each under its flag. Codecov waits for all four (`notify.after_n_builds: 4` in
[`codecov.yml`](../codecov.yml)) before it computes the total, so a partial upload cannot fire a
transient "coverage decreased" status. The smoke and e2e tiers upload nothing: they are not built
with HPC, so a line only they exercise reads as uncovered. Never reason "the e2e test covers it". A
path that needs coverage needs a unit or integration test.

The combined command removes a *reporting* confusion, a local single-tier read that disagrees with
the merged dashboard. It does not paper over gaps. If the *merged* report still shows a module's
error arms red (e.g. `Worker.hs`'s fail-closed integrity-mismatch branch), that is a genuine
uncovered path a test owes.

The gate is Codecov's two commit statuses, both in [`codecov.yml`](../codecov.yml).
`codecov/project` allows no regression versus the PR base, within a 1% threshold. `codecov/patch`
requires new and changed lines at ≥ 85%, a floor that verifies behaviour rather than a number to
chase. Uploads use GitHub OIDC (`use_oidc: true`), so there is no `CODECOV_TOKEN` to leak. Coverage
measures library code only. It excludes `app/**`, `bench/**`, and `test/**`, and drops every
`Ecluse.Test.*` module of `ecluse-test-support` from the HPC report too.
[`docs/style.md`](style.md) → "Data types and deriving" decides which derived instances the 85%
patch bar treats as accepted partials.

**References:** [testcontainers](https://hackage.haskell.org/package/testcontainers) ·
[ministack](https://github.com/ministackorg/ministack) (local AWS emulator, image
`ministackorg/ministack`, port 4566).

## Style

Tests are documentation too, so keep them as readable as the code.

- **Structure with `hspec`**: `describe` per function/area, `it` with a full-sentence
  expectation.

  ```haskell
  describe "evalRule" $ do
      it "AllowScope allows a matching scope" $
          evalRule ctx (AllowScope (mkScope "myorg")) (pkg (Just "myorg") 0)
              `shouldSatisfy` isAllow
  ```

- **Name fixtures and helpers, and give them signatures** (`now :: UTCTime`,
  `pkg :: Maybe Text -> Integer -> RuleEvidence`). A small builder that fills defaults and
  exposes only the axis under test keeps each case to one line.
- **Add small predicate/extractor helpers** (`isAllow`, `approvedBy`) instead of inlining
  pattern matches in assertions.
- **Express invariants as `hedgehog` properties** under `describe "properties"` with `forAll`
  and `(===)`: an invariant that must hold for *every* input (order-independence, a round-trip
  law) belongs here.
- **Share cross-suite helpers through `ecluse-test-support`** (`test/support/`). A helper more
  than one suite needs lives there, never copied per suite. Its modules mirror the main-library
  namespace, so a helper for `Ecluse.X` lives in `Ecluse.Test.X`: the digest fixtures and
  `unsafeHash` for `Ecluse.Core.Package` live in `Ecluse.Test.Package`. Cross-cutting helpers
  live in `Ecluse.Test.Support`. A helper only one suite uses stays local.
