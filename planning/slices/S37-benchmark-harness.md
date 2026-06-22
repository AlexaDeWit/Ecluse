---
id: S37
title: Benchmark harness + pure-core micro-benchmarks (informational)
milestone: M9 — Performance benchmarking
status: not-started
depends-on: []
test-tier: [bench]
arch-refs:
  - docs/architecture/technology-stack.md#key-decisions
  - docs/architecture/observability.md
  - docs/architecture/rules-engine.md#applying-verdicts-to-a-packument
  - docs/architecture/registry-model.md#packument-merge-across-upstreams
  - docs/architecture/security.md
pr: null
---

# S37 — Benchmark harness + pure-core micro-benchmarks (informational)

> Milestone **M9** · depends on: — (runs against the already-merged pure core) · tier: bench

**Goal.** Stand up the benchmarking harness and the first micro-benchmarks over
the **pure core already on `main`** (version ordering, rules, the npm wire
decoders / projection, the router, the security guards), wired into CI as an
**informational, non-gating** trend compared against prior baselines. Benchmarks
**inform; they never gate** — no `gate` dependency, no build-failing
`--fail-if-slower` — so a change may knowingly trade performance for correctness
and still merge; the trend just records it. Because every target is already
merged, this slice lands before the MVP and **de-risks the perf-CI pipeline
early** — a natural companion to the inter-wave
[quality & alignment pass](../orchestration-strategy.md#inter-wave-quality--alignment-pass),
which can then *measure* the regressions it currently eyeballs.

**Acceptance criteria.**
- [ ] **`ecluse-bench` component.** A cabal `benchmark` stanza (`import: shared` +
  the relude mixin, like the test suites) using
  [`tasty-bench`](https://hackage.haskell.org/package/tasty-bench) — featherweight
  (one module, only `tasty`), GHC 9.6, CPU-time by default — builds `-Werror`-clean
  and runs via `make bench` from the Nix shell. `criterion` is **rejected** (50+
  transitive deps) per the lean-dependency posture. — _technology-stack.md#key-decisions_
- [ ] **Micro-benches over the pure hot paths**, each on a realistic input (reuse
  `test/unit/fixtures/npm/*.json` — e.g. `core-js`, `webpack-cli`): npm wire decode +
  projection (`Registry.Npm.Wire` / `.Project`); `Rules.evalRules` scaled across
  version counts; `Version.compareVersions` / `parseVersionKey`;
  `Server.Route.classify`; the `Security` bounded-read / nesting-depth guards.
  — _rules-engine.md#applying-verdicts-to-a-packument · registry-model.md · security.md_
- [ ] **Time *and* allocations captured** (`+RTS -T`). The dashboard tracks
  **allocations** as the stable, machine-independent signal and CPU time as the
  noisier one — documented as such, and **neither gates**. (GC pauses drive an
  inline proxy's tail latency — _observability.md_.)
- [ ] **Informational CI only.** A non-gating workflow on its own lifecycle (like
  `security.yml` / `pages.yml`, *not* a `gate` dependency) runs the benches and
  publishes to a
  [`github-action-benchmark`](https://github.com/benchmark-action/github-action-benchmark)
  trend dashboard with **`fail-on-alert: false`** — it *comments* on a threshold
  regression but never fails the build.
- [ ] **No Pages contention.** Trend data is stored on a **dedicated branch** by
  `github-action-benchmark` (a Node action — **accepted**, SHA-pinned,
  Dependabot-bumped), kept off the Haddock GitHub-Pages artifact deploy
  (single-concurrency group) so the two publish paths never contend.
- [ ] **Strategy documented in the same PR** — `docs/architecture/performance.md`
  (or a CONTRIBUTING "Performance benchmarking" section): what is measured,
  allocations-vs-time, **never-gates**, how to read the dashboard, and how to run
  `make bench` locally.

**File scope.**
- `bench/Main.hs` (+ `bench/Ecluse/*Bench.hs`) — the `tasty-bench` suite.
- `ecluse.cabal` — `benchmark ecluse-bench` stanza (adds `tasty-bench` + `tasty`,
  benchmark-component-only; never in the library's dependency closure).
- `Makefile` — `bench` target (`$(NIX) cabal bench …`, RTS `-T`).
- `flake.nix` — `tasty-bench` in the package set / dev shell if not already present.
- `scripts/bench-to-json.{sh,hs}` — `tasty-bench` CSV → `customSmallerIsBetter` JSON.
- `.github/workflows/bench.yml` — the informational, non-gating workflow.
- `docs/architecture/performance.md` — the strategy doc (the home of the
  *informational, never-gates* and *Node-accepted, own-branch* decisions).

**Test tier.** Bench — informational; **not** a gating suite and **not** wired into
`gate`. A tiny unit check may cover the CSV→JSON shim if it grows logic.

**Notes / risks.** Confirm `tasty-bench` (+ `tasty`) is in the pinned package set;
**escalate** if absent (a real toolchain dependency). Keep the benchmark component
out of the library's dependency closure. RTS hygiene: a larger nursery (`-A32m`)
and `-fproc-alignment=64` cut GC / layout noise; supply inputs via `env`, not
top-level thunks. This track is **off the launch critical path** — pull it in
opportunistically. **Never wire any of it into `gate`.**
