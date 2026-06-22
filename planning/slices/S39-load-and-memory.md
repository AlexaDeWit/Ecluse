---
id: S39
title: Macro load + bounded-memory streaming observations (scheduled, informational)
milestone: M9 — Performance benchmarking
status: not-started
depends-on: [S15, S37]
test-tier: [bench]
arch-refs:
  - docs/architecture/web-layer.md#web-layer
  - docs/architecture/observability.md
  - docs/architecture/security.md
  - docs/architecture/cloud-backends.md#mirror-queue
pr: null
---

# S39 — Macro load + bounded-memory streaming observations (scheduled, informational)

> Milestone **M9** · depends on: [S15](S15-tarball-path.md), [S37](S37-benchmark-harness.md) · tier: bench

**Goal.** Characterise the **running server** under load and confirm the
**bounded-memory streaming** property the architecture claims. Warp on localhost
over in-memory doubles, driven by an external load tool, capturing throughput and
**tail latency (p50/p99)** — the metric that matters for an inline proxy, and the
one GC pauses drive — plus **peak residency** while streaming a large artifact.
Host-sensitive, so it runs on a **schedule / manual dispatch**, never per-PR, and
(like the rest of M9) **informs, never gates**.

**Acceptance criteria.**
- [ ] **Load harness.** Warp on localhost over in-memory doubles, driven by `oha`
  (or `wrk2`) from the Nix shell; capture throughput + p50/p99 latency for the
  packument and tarball paths. — _web-layer.md#web-layer · observability.md (GC → tail latency)_
- [ ] **Bounded-memory streaming observation.** Stream a large artifact through the
  tarball path and record **peak residency**, confirming it stays bounded
  regardless of artifact size (constant-memory streaming + the `maxBodyBytes`
  metadata cap are architectural claims). — _web-layer.md#web-layer · security.md_
- [ ] **Mirror-enqueue overhead** on the hot path measured — must be negligible
  (best-effort, never blocks the response). — _cloud-backends.md#mirror-queue_
- [ ] Runs on **schedule / `workflow_dispatch`** (not per-PR), publishes to the
  informational trend; **never gates**.

**File scope.**
- `bench/load/` — load scenarios + the `oha` / `wrk2` driver script.
- `Makefile` — `bench-load` target.
- `flake.nix` — `oha` (or `wrk2`) in the dev shell.
- `.github/workflows/bench.yml` — add the scheduled, non-gating load job.

**Test tier.** Bench — informational, non-gating, scheduled.

**Notes / risks.** Load numbers are host-sensitive: trend them relative to the same
runner class, never gate. Confirm `oha` / `wrk2` in nixpkgs; **escalate** if absent.
S20 (AWS composition) makes a run more production-shaped, but the in-memory doubles
suffice to characterise the proxy itself. **Architect call (flagged, not baked):**
the bounded-memory invariant could *alternatively* be promoted to a **gating
correctness property** in S13 / S15's own test tier (residency stays bounded for any
artifact size) — a correctness assertion, distinct from a perf benchmark, that would
gate as a normal test; left to those slices / the architect rather than imposed here.
