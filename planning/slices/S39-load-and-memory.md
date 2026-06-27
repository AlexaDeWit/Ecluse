---
id: S39
title: Macro load + bounded-memory observations (SUPERSEDED — folded into S38)
milestone: M9 — Benchmarking & load testing
status: superseded
depends-on: [S37]
test-tier: [bench]
arch-refs:
  - docs/architecture/performance.md
pr: null
---

# S39 — Macro load + bounded-memory observations (SUPERSEDED)

> **Superseded** by the M9 re-cut (architect-approved 2026-06-27). Not a separate
> deliverable.

The original S39 split "macro load" from "pipeline benchmarks" (S38) and bundled a
bounded-memory claim. The revised M9 design treats load and pipeline benchmarking as
**two sides of one coin**, so:

- **The load / throughput / latency / peak-residency characterisation moves into
  [S38](S38-pipeline-benchmarks.md)** — a single Layer-B harness (real Warp over the
  in-memory doubles, driven by `oha`), covering the mandatory traffic scenarios. There
  is no separate macro-load slice.

- **The bounded-memory streaming property is lifted *out* of M9** as a **gating
  correctness test**, not a benchmark. "Residency stays bounded for any artifact size"
  is a deterministic assertion that belongs in the tarball-path test tier
  (depends on [S15](S15-tarball-path.md)), distinct from the inform-only perf trend —
  M9 never gates. **Flagged to the architect** to place as its own correctness
  issue/slice; not committed here.

See [`docs/architecture/performance.md`](../../docs/architecture/performance.md) and
the [delivery plan → M9](../delivery-plan.md) for the revised milestone.
