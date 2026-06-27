---
id: S38
title: Layer B — throughput & latency under load, with the mandatory traffic scenarios (informational)
milestone: M9 — Benchmarking & load testing
status: not-started
depends-on: [S37]
test-tier: [bench]
arch-refs:
  - docs/architecture/performance.md
  - docs/architecture.md#request-lifecycle
  - docs/architecture/web-layer.md#web-layer
  - docs/architecture/registry-model.md#packument-merge-across-upstreams
  - docs/architecture/rules-engine.md#applying-verdicts-to-a-packument
  - docs/architecture/cloud-backends.md#mirror-queue
pr: null
---

# S38 — Layer B: throughput & latency under load, with the mandatory traffic scenarios (informational)

> Milestone **M9** · depends on: [S37](S37-benchmark-harness.md) (the harness + CI plumbing); the request pipeline (S14/S15/S33/S19) is merged · tier: bench

**Goal.** Add **Layer B — throughput & latency under concurrency** to the harness: the
*host-sensitive* layer that answers "does the proxy keep up with traffic?" by driving
the **real `Application` on Warp/localhost over the in-memory handle doubles**, with
**configurable injected upstream latency + payload size**, under a real concurrent load
generator. Hermetic and deterministic in shape (no network, no Docker). Like S37 it is
**informational and never gates** (see [performance.md](../../docs/architecture/performance.md));
per **D1** it carries **no SLO** — it characterises and trends, it never passes or fails
on a number.

**Why both layers.** Allocations (S37, Layer A) are the *leading indicator* of the p99
this slice measures — GC pauses are tail latency for an inline proxy. Layer A localises
regressions deterministically; Layer B shows the throughput/latency shape under real
concurrency. Two sides of one coin.

**Acceptance criteria.**
- [ ] **Load harness.** Warp on localhost over the in-memory `RegistryClient` /
  `MirrorQueue` / `CredentialProvider` doubles (`newInMemoryQueue`, `staticProvider`,
  an in-memory registry), with **injectable per-upstream latency + payload size**,
  driven by `oha` from the `.#bench` dev shell; `make bench-load`.
  — _web-layer.md#web-layer · architecture.md#request-lifecycle_
- [ ] **Mandatory traffic scenarios** (the real-world shapes that earn the confidence —
  architect-specified):
  1. **Public download path with private + public packument MERGE in the loop** —
     `GET /{pkg}` fanning to both upstreams → merge → rule-filter → URL-rewrite → ETag
     → re-serialise. The expensive headline path.
     — _registry-model.md#packument-merge-across-upstreams · rules-engine.md_
  2. **Private-only cache hit** — the cheap, common high-throughput path (served from
     the metadata cache / private-only, no public fetch). — _web-layer.md (metadata cache)_
  3. **Worker mirroring process** — the fetch → verify → publish → ack loop.
     — _cloud-backends.md#mirror-queue_
- [ ] **Metrics captured per scenario:** throughput; latency distribution
  **p50/p90/p99/p99.9**; **peak residency**; GC-pause stats; **and work-normalized
  per-request counters** (allocations/request; optionally `perf stat` instructions/
  request) — the host-independent signal that stays meaningful on a shared runner.
- [ ] **Feeds the same informational flow as S37** — results to the run summary + the
  `main` baseline artifact; **no `gate` wiring**, never fails on a regression.

**File scope.**
- `bench/Ecluse/Core/PipelineBench.hs` (or `bench/load/`) — the scenario drivers,
  additive to the S37 suite; the `oha` invocation + result-parsing script under
  `bench/load/` or `scripts/`.
- `ecluse.cabal` — benchmark-component deps (`wai`, `warp`, the app `ecluse` library
  for the composed `Application`; reuse the in-memory doubles).
- `flake.nix` — `oha` added to the `.#bench` dev shell.
- `Makefile` — `bench-load` target.
- `.github/workflows/bench.yml` — add the Layer-B job (still `workflow_dispatch`,
  still non-gating).

**Test tier.** Bench — informational, non-gating.

**Notes / risks.** Reuse the existing in-memory doubles so the harness opens no
external sockets (only localhost Warp ↔ `oha`). `oha` 1.14.0 / `wrk2` 4.0.0 are both
in the pin (2026-06-27). Throughput/latency absolutes are **noisy on the shared runner
(D2)** — read the trend coarsely (big moves are real) and use the work-normalized
per-request counters as the steady signal; trustworthy absolutes come from **local
deep-dives**. **Never wire into `gate`.**
