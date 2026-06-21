---
id: S19
title: Mirror worker (fetch → verify → publish → ack)
milestone: M4 — AWS cloud backends & worker
status: not-started
depends-on: [S08, S16, S18]
test-tier: [unit, integration]
arch-refs:
  - docs/architecture/cloud-backends.md#mirror-queue
  - docs/architecture/cloud-backends.md#process-model
  - docs/architecture/web-layer.md#streaming-and-resource-lifetime
pr: null
---

# S19 — Mirror worker (fetch → verify → publish → ack)

> Milestone **M4** · depends on: [S08](S08-npm-data-plane.md), [S16](S16-credential-wrapper.md), [S18](S18-sqs-queue.md) · tier: unit, integration

**Goal.** The consume loop that turns enqueued jobs into mirrored packages:
`runWorker` receives a job, fetches the artifact from the public upstream, **verifies
its bytes against the version's integrity hash**, publishes to the mirror target via
`publishArtifact` (token from the `CredentialProvider`), and acks. Runs in-process as
a supervised concurrent thread, split-ready.

**Acceptance criteria.**
- [ ] `runWorker :: Env -> IO ()`: `receive` → fetch artifact → **verify
  `dist.integrity`** → `publishArtifact` (bearer from `CredentialProvider`) → `ack`.
  A **hash mismatch fails the job (no publish)** and alarms — a tampered artifact
  never enters the private upstream. — _cloud-backends.md#mirror-queue_
- [ ] **Idempotent publish**: a redelivered job whose version already exists is
  treated as success (S08's 409-is-success). — _cloud-backends.md#mirror-queue_
- [ ] **No re-running rules** in the worker (gated at serve time); retry-is-don't-ack
  on transient failure; long publishes may `extendVisibility`. — _cloud-backends.md#mirror-queue_
- [ ] **Health/heartbeat**: the worker exposes a consume-loop heartbeat /
  last-successful-poll distinct from the server's HTTP readiness, feeding `/livez`
  (S12). — _cloud-backends.md#process-model_
- [ ] Composition root runs `runServer` + `runWorker` **concurrently** (async /
  unliftio), each a self-contained entry over the shared `Env` (split-ready). —
  _cloud-backends.md#process-model_

**File fence.**
- `src/Ecluse/Worker.hs` — `runWorker`, the consume loop, integrity verification, heartbeat.
- `src/Ecluse/Env.hs` — worker heartbeat handle (additive).
- `src/Ecluse.hs` — run server+worker concurrently (additive to S12).
- `test/unit/Ecluse/WorkerSpec.hs` — loop logic with in-memory queue + WAI-stub upstream + fake publish: verify-fail→no-publish→no-ack, idempotent-success, heartbeat.
- `test/integration/Ecluse/WorkerSpec.hs` — end-to-end against ministack queue + a WAI/Verdaccio mirror stub.

**Test tier.** Unit (loop logic, gating) + integration (real queue emulator + stub
registry, gating).

**Notes / risks.** **Integrity verification is the security crux** — a corrupt/
tampered artifact must never publish. Verify by streaming (don't buffer a whole large
tarball in memory). The npm publish protocol is the same regardless of cloud (managed
registry = npm endpoint + token), so there is no per-cloud publish path. Worker
liveness must surface a stall (single-process health reflects a stalled worker today;
a future standalone binary keeps its own probe).
