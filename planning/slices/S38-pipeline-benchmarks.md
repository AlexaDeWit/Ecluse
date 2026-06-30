---
id: S38
title: Layer B, throughput & latency under load, with the mandatory traffic scenarios (informational)
milestone: M9, Benchmarking & load testing
status: merged
depends-on: [S37]
test-tier: [bench]
arch-refs:
  - docs/architecture/performance.md
  - docs/architecture.md#request-lifecycle
  - docs/architecture/web-layer.md#web-layer
  - docs/architecture/registry-model.md#packument-merge-across-upstreams
  - docs/architecture/rules-engine.md#applying-verdicts-to-a-packument
  - docs/architecture/cloud-backends.md#mirror-queue
pr: 410
---

# S38, Layer B: throughput & latency under load, with the mandatory traffic scenarios (informational)

> Milestone **M9** · depends on: [S37](S37-benchmark-harness.md) (the harness + CI plumbing); the request pipeline (S14/S15/S33/S19) is merged · tier: bench

**Goal.** Add **Layer B, throughput & latency under concurrency** to the harness: the
*host-sensitive* layer that answers "does the proxy keep up with traffic?" by driving
the **real `Application` on Warp/localhost over stub upstreams and the in-memory handle
doubles**, with **configurable injected upstream latency + payload size**, under a real
concurrent load generator (`oha`). Hermetic and deterministic in shape (no network, no
Docker). Like S37 it is **inform-only and never gates** (decision **D1**); it carries
**no SLO**, it characterises and trends, it never passes or fails on a number.

**Why both layers.** Allocations (S37, Layer A) are the *leading indicator* of the p99
this slice measures, GC pauses are tail latency for an inline proxy. Layer A localises
regressions deterministically; Layer B shows the throughput/latency shape under real
concurrency. Two sides of one coin.

**Acceptance criteria.**
- [x] **Load harness.** The real composed `Application` booted on localhost Warp
  (`testWithApplication`) over in-process stub upstreams and the in-memory
  `MirrorQueue` / `CredentialProvider` doubles (`newInMemoryQueue`, `staticProvider`),
  with **injectable per-upstream latency + payload size**, driven by `oha` from the
  `.#bench` dev shell; `make bench-load`. A separate `bench-load` **executable** (not a
  `tasty-bench` component).  _web-layer.md#web-layer · architecture.md#request-lifecycle_
- [x] **Mandatory traffic scenarios** (the real-world shapes that earn the confidence,  architect-specified):
  1. **`merge-cold`**, public download path with the private + public packument MERGE
     in the loop: `GET /{pkg}` fanning to both upstreams → merge → rule-filter →
     URL-rewrite → ETag → re-serialise, with the public metadata cache disabled (TTL 0).
     The public leg is single-flight, so concurrent misses coalesce onto one in-flight
     fetch+decode (the ~40 ms decode is amortised under load, not per-request); each
     request pays the live private fetch, the merge, the rule sweep, and the re-serialise.
     The expensive headline path.
    , _registry-model.md#packument-merge-across-upstreams · rules-engine.md_
  2. **`cached-public-hit`**, the cheap, common high-throughput path: the same `GET`
     with the anonymous public origin served from the warm metadata cache (no public
     fetch or decode).  _web-layer.md (metadata cache)_
  3. **`worker-mirroring`**, the fetch → verify → publish → ack loop, driven in-process
     (no HTTP surface).  _cloud-backends.md#mirror-queue_
- [x] **Metrics captured per scenario:** throughput; latency distribution
  **p50/p90/p99/p99.9**; **peak residency**; GC-pause stats; **and work-normalised
  per-request counters** (allocations/request), the host-independent signal that stays
  meaningful on a shared runner. (Caveat, disclosed in the report and performance.md: the
  allocations/request figure is measured over the whole bench process, so for the HTTP
  scenarios it folds in the in-process stub upstreams' allocations, a consistent
  over-count for trending, not a pure proxy per-request cost, not directly comparable to
  Layer A. Peak residency is a process high-water mark that also spans the warm-up.)
- [x] **Inform-only flow (D1/D2/D3).** Results render to **stdout and the run summary**
  and upload as a **per-run downloadable artifact**; **no `gate` wiring**, never fails on
  a regression. There is **no cross-run baseline and no PR-comparison comment**, both
  deliberately dropped (a durable store / a comment would need write access this project
  does not take on); comparison is by hand, and allocations/request are machine-independent
  so an eyeballed delta is reliable. The CI job runs on **`workflow_dispatch` + a nightly
  `schedule`**, never per-PR (shared-runner throughput is too noisy for a per-PR signal).

**File scope (as built).**
- `bench/load/`, the load harness: `Main.hs` (the driver / per-scenario child),
  `Ecluse/BenchLoad/Harness.hs` (the ecosystem-agnostic core), `Ecluse/BenchLoad/Oha.hs`
  (the `oha` driver), `Ecluse/BenchLoad/Npm.hs` (the npm fixture). Canned payloads are
  generated in the npm fixture.
- `ecluse.cabal`, a `bench-load` `executable` (deps: the `ecluse` app lib, `warp`,
  `wai`, `http-client`, `http-types`, `typed-process`, `aeson`, `crypton`, `memory`,
  `katip`, the in-memory doubles via `ecluse-test-support`); `-threaded -rtsopts
  "-with-rtsopts=-T -N"`. Kept out of the library closure.
- `flake.nix`, `oha` added to `devShells.bench`.
- `Makefile`, a `bench-load` target (via `$(NIX_BENCH)`).
- `.github/workflows/bench-load.yml`, the inform-only load job (`workflow_dispatch` +
  nightly `schedule`), off `gate`, first-party SHA-pinned actions, renders to the run
  summary, uploads this run's results.
- `docs/architecture/performance.md`, the Layer B section filled in.

**Test tier.** Bench, informational, non-gating. The harness self-checks its wiring (a
scenario that serves nothing, or a worker job that never publishes, is a thrown literal
failure), but it is not part of the gate.

**As-built notes / deviations.**
- **Ecosystem abstraction (architect-specified, beyond the issue).** The harness is split
  into a reusable **structure** (the `oha` driver, RTS capture, scenario runner, report
  rendering) and a per-ecosystem **interface**, an `UpstreamFixture` (the Handle pattern:
  an ecosystem + its `Scenario`s, each carrying only its ecosystem-specific setup/teardown
  in `scenarioBoot`). npm is the first and only instance; adding PyPI/RubyGems is "write
  the fixture + register its scenarios", not "rewrite the harness". Documented in
  performance.md.
- **The "private-only cache hit" became `cached-public-hit`.** The default `passthrough`
  posture caches only the anonymous **public** origin; the trusted private origin is the
  per-client authority and is fetched per request, never cached. So a literal
  "private-only cache hit" is not a shape the proxy has. The faithful realisation of the
  issue's cheap, *no-public-fetch* path is the same `GET` with the public origin served
  warm from cache while the live private leg merges in. The two packument scenarios differ
  in cache TTL (cold = TTL 0; hit = long TTL + warm-up), but note `merge-cold` is not a
  strict per-request worst case: the public leg's `resolveMetadata` is single-flight, so
  even at TTL 0 concurrent misses coalesce onto one in-flight fetch+decode (the ~40 ms
  decode amortised under load), which narrows the contrast with `cached-public-hit` (both
  amortise the public fetch, one via the cache, one via single-flight). This is a
  spec-vs-architecture reconciliation surfaced for review.
- **Per-scenario process isolation.** Peak residency is a process-wide RTS high-water
  mark, so the driver re-execs the binary once per scenario (each prints its report as a
  single JSON line) and aggregates, keeping each scenario's residency its own.
- **`oha` 1.14.0 invocation.** The JSON flag is `--output-format json` (not `--json`);
  the report carries `summary.requestsPerSec`, `latencyPercentiles.{p50, p90, p99, p99.9}`,
  and the status/error distributions.
- **Load-knob defaults:** `-c 50`, `-z 30s`, 5 ms injected upstream latency, ~256 KiB
  payload; each overridable via `BENCH_LOAD_*` environment variables (the CI dispatch
  exposes duration + concurrency as inputs, passed through the environment).
- **`perf` instructions/request deferred** (open decision 3): GitHub runners often block
  `perf_event_open`, so CPU-instruction counts are not captured for now.
- **Hermeticity.** All upstreams are loopback `warp` stubs; the proxy and worker fetch
  them over plain (no-TLS, unguarded) managers with `127.0.0.1` opted into the
  internal-range allowance, exactly as the integration suite does, no external socket, no
  Docker. Throughput/latency absolutes are **noisy on the shared runner (D2)**, read the
  trend coarsely, use the work-normalised per-request counters as the steady signal, and
  take trustworthy absolutes from **local deep-dives**. **Never wired into `gate`.**
