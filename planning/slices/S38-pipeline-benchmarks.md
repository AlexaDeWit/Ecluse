---
id: S38
title: End-to-end pipeline benchmarks (in-process, informational)
milestone: M9 — Performance benchmarking
status: not-started
depends-on: [S14, S33, S37]
test-tier: [bench]
arch-refs:
  - docs/architecture.md#request-lifecycle
  - docs/architecture/web-layer.md#web-layer
  - docs/architecture/registry-model.md#packument-merge-across-upstreams
  - docs/architecture/rules-engine.md#applying-verdicts-to-a-packument
pr: null
---

# S38 — End-to-end pipeline benchmarks (in-process, informational)

> Milestone **M9** · depends on: [S14](S14-packument-path.md), [S33](S33-packument-merge.md), [S37](S37-benchmark-harness.md) · tier: bench

**Goal.** Extend the harness from pure functions to the **whole request pipeline
through the WAI `Application`, over the in-memory handle doubles** — hermetic and
deterministic (no network, no Docker), exactly the payoff of the
record-of-functions handles. Benchmarks the packument path end to end:
decode → rule-filter → cross-upstream merge → re-serialise → ETag. Like S37 it is
**informational and non-gating** — it feeds the same trend and wires into no `gate`.

**Acceptance criteria.**
- [ ] **End-to-end packument benchmark.** Drive the `Application` over in-memory
  `RegistryClient` / `MirrorQueue` / `CredentialProvider` doubles
  (`newInMemoryQueue`, `staticProvider`, an in-memory registry) with a realistic
  packument, measuring the full parse → filter → merge → re-serialise → ETag path.
  — _architecture.md#request-lifecycle · web-layer.md#web-layer_
- [ ] **Merge at scale.** `mergePackuments` (S33) over large / asymmetric upstream
  inputs; track scaling and flag worse-than-linear (optionally `tasty-bench-fit`).
  — _registry-model.md#packument-merge-across-upstreams_
- [ ] **Re-serialise + ETag** over the *filtered* body measured (the proxy computes
  its own validators over its own document, not upstream's).
  — _rules-engine.md#applying-verdicts-to-a-packument_
- [ ] Results feed the **informational** dashboard from S37; **no gate** wiring,
  `fail-on-alert: false`.

**File scope.**
- `bench/Ecluse/PipelineBench.hs`, `bench/Ecluse/MergeBench.hs` — additive to the
  S37 suite.
- `ecluse.cabal` — benchmark-component deps (`wai`; reuse the library's in-memory
  doubles).
- (Reuses S37's `.github/workflows/bench.yml`; no new workflow.)

**Test tier.** Bench — informational, non-gating.

**Notes / risks.** Reuse the existing in-memory handle doubles so the pipeline bench
opens no sockets. Keep bench inputs in fixtures (extend `test/unit/fixtures/npm/`,
or a `bench/fixtures/` if large). Blocked on the walking skeleton (S14) and the
pure merge (S33) existing. **Never wire into `gate`.**
