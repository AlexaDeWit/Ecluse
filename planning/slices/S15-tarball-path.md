---
id: S15
title: Tarball path + demand-driven mirror enqueue
milestone: M3, Request pipeline
status: merged
depends-on: [S14]
test-tier: [unit]
arch-refs:
  - docs/architecture.md#request-lifecycle
  - docs/architecture/cloud-backends.md#mirror-queue
  - docs/architecture/web-layer.md#streaming-and-resource-lifetime
  - docs/architecture/web-layer.md#error-model
pr: null
---

# S15, Tarball path + demand-driven mirror enqueue

> Milestone **M3** · depends on: [S14](S14-packument-path.md) · tier: unit

**Goal.** The artifact request path: a private hit streams unfiltered; a private
miss gates *that one version* against the public upstream and, on admit, streams it
**and enqueues a demand-driven mirror job**, serving the client immediately.

**Acceptance criteria.**
- [ ] **Private hit** → stream the tarball unfiltered (already vetted), bounded
  memory (S13 streaming).  _architecture.md#request-lifecycle_
- [ ] **Private miss** → fetch the version's metadata from public, run the rules for
  that single version; on **admit**, stream from public **and** enqueue a
  `MirrorJob` (mirror target URL, package, version, artifact location); on reject,
  the serve error model (403/503/500).  _architecture.md#request-lifecycle, web-layer.md#error-model_
- [ ] **Enqueue is best-effort and non-blocking**: the artifact is served first; an
  enqueue failure is logged/metered and **never** fails the client response.   _cloud-backends.md#mirror-queue_
- [ ] **Demand-driven**: a job is enqueued only when an artifact is *accepted on the
  tarball path*, not when a packument is filtered.  _cloud-backends.md#mirror-queue_
- [ ] Lockfile installs (`npm ci`) hitting tarball URLs with no preceding packument
  request are gated correctly on this path alone.

**File scope.**
- `src/Ecluse/Server/Pipeline.hs`, add the tarball handler (additive to S14).
- `src/Ecluse/Server.hs`, wire the tarball route (replace the S12 stub).
- `test/unit/Ecluse/Server/PipelineSpec.hs`, extend: private-hit stream, private-miss gate+stream+enqueue (assert the in-memory queue received the job), reject mapping, enqueue-failure-doesn't-fail-serve.

**Test tier.** Unit, `hspec-wai` + in-process upstream + the S02 in-memory queue
double (assert the enqueued job); no network, no cloud.

**Notes / risks.** Enqueue uses the `MirrorQueue` handle (in-memory double here; real
SQS in S18, the consuming worker in S19). Keep the serve-then-enqueue ordering, the
client must never wait on the queue. The integrity hash travels in the job for the
worker to verify (S19); this slice does **not** verify on the serve path (it relies
on the client's `dist.integrity`). The tarball handler is added on the
post-base-hardening **`ReaderT RequestCtx IO`** hot path
(base-hardening D6), reading its mount deps from
`RequestCtx`, not the plain-`IO`-taking-`Env` shape S14 first shipped.
