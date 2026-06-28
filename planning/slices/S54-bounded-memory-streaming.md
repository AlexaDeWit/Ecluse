---
id: S54
title: Bounded-memory streaming — residency-invariant gating correctness test
milestone: M8 — Release hardening
status: not-started
depends-on: [S15]
test-tier: [I]
arch-refs:
  - docs/architecture/web-layer.md#web-layer
  - docs/architecture/security.md
pr: null
---

# S54 — Bounded-memory streaming: residency-invariant gating correctness test

> Milestone **M8** · depends on: [S15](S15-tarball-path.md) (the tarball stream) · tier: integration · **last, lowest criticality**

**Goal.** Turn the architecture's **constant-memory streaming** claim — a tarball of
any size flows through the proxy without buffering, and the `maxBodyBytes` metadata cap
bounds decoded packument reads — from a stated property into a **gating correctness
test**. This is the *correctness* counterpart to the inform-only peak-residency
*observation* in [S38](S38-pipeline-benchmarks.md): M9 watches the trend and never
gates; this asserts the invariant and does. Split out of the M9 re-cut (2026-06-27) so
a gating test never lived inside the inform-only perf milestone.

**The load-bearing design point.** A naive "peak residency < N MB" assertion is
machine-dependent and flaky — exactly why this was kept informational in M9. The test
must instead assert the **invariant, not an absolute**: residency is *independent of
artifact size*. Stream a small artifact and a very large one through the same path,
measure peak live bytes for each (`GHC.Stats` after a `performGC`, RTS `-T`), and assert
the **delta stays within a small constant margin**, i.e. memory does not scale with the
body. That is deterministic enough to gate. **If a non-flaky gating assertion proves
infeasible in practice, do not force a flaky gate — keep it informational and escalate.**

**Acceptance criteria.**
- [ ] **Tarball residency invariant (gating).** Streaming a large artifact (e.g. 100 MB)
  through the tarball path costs essentially the same peak residency as a small one
  (e.g. 1 MB), within a fixed margin independent of body size — proving constant-memory
  passthrough, not size-proportional buffering. — _web-layer.md#web-layer_
- [ ] **Metadata cap holds (gating).** A packument response that decompresses past
  `maxBodyBytes` is bounded/refused at the cap rather than read unbounded — the
  decoded-bytes bound, not the on-the-wire bound. — _web-layer.md · security.md_
- [ ] **Deterministic, not flaky.** The assertions key on the size-invariance / cap, with
  a `performGC` + `GHC.Stats` measurement and a generous margin; they pass repeatably on
  the shared CI runner. Gates as a normal test in its tier.

**File scope.**
- `test/integration/Ecluse/Server/<…>Spec.hs` (or the existing tarball/streaming spec) —
  the residency-invariant + cap assertions; a synthetic large-body source helper.
- Possibly `core/test/unit/…` if a unit-level variant over the streaming combinator
  proves the invariant more robustly than the full integration path.

**Test tier.** Integration (gating) — the realistic streaming path; a unit-level variant
is acceptable if it asserts the invariant more deterministically.

**Notes / risks.** Residency assertions are the classic flaky-test trap; the invariant
framing (size-independence) + GC + margin is the mitigation. Depends only on the merged
tarball path (S15); the `maxBodyBytes` cap is from S13. Lowest priority in the queue —
pull it in at a quiet point, after the launch-critical and observability tails.
