---
id: S37
title: Benchmark harness + Layer A work-per-request benches + PR-run CI (informational)
milestone: M9 — Benchmarking & load testing
status: merged
depends-on: []
test-tier: [bench]
arch-refs:
  - docs/architecture/performance.md
  - docs/architecture/technology-stack.md#key-decisions
  - docs/architecture/observability.md
  - docs/architecture/rules-engine.md#applying-verdicts-to-a-packument
  - docs/architecture/registry-model.md#packument-merge-across-upstreams
pr: 385
---

# S37 — Benchmark harness + Layer A work-per-request benches + PR-run CI (informational)

> Milestone **M9** · depends on: — (runs against the already-merged pure core) · tier: bench

**Goal.** Stand up the benchmarking harness and the **work-per-request** layer over the
pure `ecluse-core` already on `main`, plus the informational CI plumbing the whole
milestone hangs off. M9 is **one capability, two layers** (see
[performance.md](../../docs/architecture/performance.md)): this slice is **Layer A —
work per request**, the *deterministic, machine-independent* signal (allocations /
instructions). [S38](S38-pipeline-benchmarks.md) adds **Layer B — throughput &
latency under load**. Because every target here is already merged, S37 lands early and
**de-risks the perf-CI pipeline** before Layer B builds on it.

**Posture (the whole milestone).** **Informational; never gates.** Not a `gate.needs`
dependency. The workflow's *only* red state is a **literal benchmark failure** (build
error, harness crash, non-zero exit) — it **never** computes a perf-regression fail
(no `fail-on-alert`, no `--fail-if-slower`). A slow result is data, not a failure. A
change may knowingly trade performance for correctness and still merge.

**Acceptance criteria.**
- [x] **`ecluse-bench` component.** A cabal `benchmark` stanza (`import: shared` + the
  relude mixin, like the test suites) using
  [`tasty-bench`](https://hackage.haskell.org/package/tasty-bench) (one module; only
  `tasty`), `-Werror`-clean, run via `make bench` from the Nix shell. `criterion` is
  rejected (50+ transitive deps) per the lean-dependency posture. The component is
  kept **out of the library's dependency closure**. — _technology-stack.md#key-decisions_
- [x] **Micro-benches over the pure hot paths**, each on a realistic input from the
  corpus below: npm wire decode + projection (`Registry.Npm.Wire` / `.Project`);
  `Rules.evalRules` scaled across version counts; packument `Package.Merge`; the npm
  URL-rewrite + ETag / re-serialise; `Version.compareVersions` / `parseVersionKey`;
  `Server.Route.classify`; the `Security` bounded-read / nesting-depth guards.
  — _rules-engine.md · registry-model.md · security.md_
- [x] **Time *and* allocations captured** (`+RTS -T`). **Allocations are the tracked,
  machine-independent signal**; CPU time is recorded but informational — documented as
  such. (GC pressure drives an inline proxy's tail latency — _observability.md_.)
- [x] **Complexity assertions via `tasty-bench-fit`** on the version-count-scaled
  paths (merge, rules-over-versions, the serve filter): flag worse-than-linear. This
  is the CI-stable guard against the accidentally-quadratic class
  ([#373](https://github.com/AlexaDeWit/Ecluse/issues/373) /
  [#374](https://github.com/AlexaDeWit/Ecluse/issues/374) /
  [#299](https://github.com/AlexaDeWit/Ecluse/issues/299)).
- [x] **Realistic corpus.** Reuse `core/test/unit/fixtures/npm/*.json` (incl. the real
  large `express.full.json`) and add a **synthetic ~100k-version packument generator**
  to stress scaling. Bench inputs are supplied via `env`, never top-level thunks.
- [x] **Informational CI workflow.** `.github/workflows/bench.yml` on **`pull_request`
  + `workflow_dispatch`** (D3), **not** a `gate` dependency, runs `make bench` in a lean
  `.#bench` dev shell, and renders the results to the run summary. **First-party
  SHA-pinned actions only** (no third-party Node action). Superseded PR runs are
  cancelled; a dispatch is never cancelled.
- [x] **Per-run results, no cross-run baseline.** Each run uploads its own results
  (`actions/upload-artifact`, SHA-pinned, `bench-results-<sha>`) — downloadable from
  that run, **not** a cross-run baseline (an artifact is scoped to its run, and a
  durable cross-run store would need write permissions we deliberately do not take on).
  Comparison is by hand (allocations are machine-independent). _No before/after PR
  comment: it would need that same write access — out of scope (D2/D3)._
- [x] **`make bench-profile`** — a profiling build → flamegraph target, so a
  regression localises to a cost centre.
- [x] **Strategy documented in the same PR** — `docs/architecture/performance.md`
  authored here: the two-layer model, allocations-vs-time, **never-gates-except-on-
  failure**, the per-run-results flow (no cross-run baseline), the consistency posture
  (D2), and how to run `make bench` / `make bench-profile` locally.

**File scope.**
- `bench/Main.hs` (+ `bench/Ecluse/Core/*Bench.hs`) — the `tasty-bench` suite.
- `bench/fixtures/` — the synthetic large-packument generator (if not derivable from
  the existing npm fixtures).
- `ecluse.cabal` — `benchmark ecluse-bench` stanza (`tasty-bench`, `tasty`,
  `tasty-bench-fit`, `ecluse:ecluse-core`, `ecluse:ecluse-test-support`); never in the
  library closure.
- `cabal.project.freeze` — regenerated via `make freeze` to pin the new deps; possibly
  `benchmarks: True` in `cabal.project` (mirror of the existing `tests: True`).
- `flake.nix` — a lean `devShells.bench` (`ciInputs ++ [ tasty tooling ]`, cf
  `.#weeder` / `.#stan`).
- `Makefile` — `bench` (RTS `-T`) and `bench-profile` targets.
- `.github/workflows/bench.yml` — the informational, non-gating, `workflow_dispatch`
  workflow.
- `docs/architecture/performance.md` — the strategy doc (home of the *inform-only*,
  *never-gates-except-on-failure*, *D1/D2/D3* decisions).

**Test tier.** Bench — informational; **not** gating, **not** wired into `gate`. A
tiny unit check may cover any results-formatting shim if it grows logic.

**Notes / decisions.**
- Tooling confirmed present in the pin (nixpkgs 26.05 / ghc910, 2026-06-27):
  `tasty-bench` 0.4.1, `tasty` 1.5.4, `tasty-bench-fit` 0.1.1.
- **D1** — no SLO; inform-only (this milestone never asserts a pass/fail throughput).
- **D2** — measure on the **shared public** GitHub-hosted runner; trustworthy
  absolutes come from **local deep-dives**. Self-hosted rejected (PR-code supply-chain
  risk); larger hosted runners deferred (need a paid org plan).
- **D3** — runs on `pull_request` + `workflow_dispatch`. No cross-run baseline and no
  before/after PR comment (each would need a durable cross-run store / write access this
  project deliberately avoids); comparison is by hand.
- RTS hygiene: a larger nursery (`-A32m`) and `-fproc-alignment=64` cut GC / layout
  noise. **Never wire any of this into `gate`.**
- **As-built (rebased onto the settled engine).** Rule evaluation became `IO` (the
  `PreparedRule` engine, #381 → #394): the rule-sweep and serve benches `prepare` rules
  once and measure the `IO` action (`whnfAppIO`), with an `IO` complexity-fit variant
  (`notWorseThanLinearIO`). `RouteBench` threads the HTTP method (the S52 publish path,
  #379, made `classify` method-aware and added the `Publish` route). The bench stanza
  adds `http-types`; `cabal.project` carries a narrow `allow-newer: regression-simple:base`
  (a stale `base < 4.20` cap on a `tasty-bench-fit` transitive, the cabal-path analogue
  of the Nix set's jailbreak). `codecov.yml` ignores `bench/` (not application code).
