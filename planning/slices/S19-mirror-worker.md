---
id: S19
title: Mirror worker (fetch → verify → publish → ack)
milestone: M4 — AWS cloud backends & worker
status: merged
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
- [ ] **Wire the publish-side `RegistryClient`** — the deferred tail of the
  per-ecosystem composition-root wiring (D5; the *serve* side landed in
  [#144](https://github.com/AlexaDeWit/Ecluse/pull/144)). Resolve the configured
  publish client **per ecosystem** (the mirror-target endpoint paired with the global
  `CredentialProvider`) at the composition root, replacing the refusing
  `Env.envRegistry` placeholder (`unconfiguredRegistry`). `envRegistry` is currently
  consumed by nothing — the serve path builds its own per-leg clients, so this slice
  is its first real consumer and retires/repurposes the single global slot. —
  _cloud-backends.md#process-model_
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

**File scope.**
- `src/Ecluse/Worker.hs` — `runWorker`, the consume loop, integrity verification, heartbeat.
- `src/Ecluse/Env.hs` — worker heartbeat handle (additive).
- `src/Ecluse.hs` — run server+worker concurrently (additive to S12); resolve the
  per-ecosystem publish `RegistryClient` from config and retire the single global
  `envRegistry` placeholder.
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

**Review points (carried from the S18 review).** The queue handle exposes only a
one-shot long-poll `receive` (a batch, or `[]` after `sqsWaitSeconds`); the
*continuous* loop, concurrency, and supervision are this slice's responsibility.
Decide and document each here when building:

- **Loop robustness — the loop must not die on a single bad iteration.** Wrap each
  iteration so a transient `receive`/fetch/publish error or an undecodable body is
  caught, logged/metered, and backed-off — then the loop continues. ("Retry-is-don't-
  ack" is *job-level* semantics; it does not protect the loop itself — an escaping
  exception would kill the worker thread.)
- **Supervision posture.** `runServices` holds the worker via `concurrently_`
  (`runServer` ‖ `runWorker`), so it is a GC root and a crash *propagates* (fail-stop,
  taking the process down). Decide: self-recover from transients (catch-and-continue,
  above) and reserve fail-stop for genuinely fatal (e.g. config) errors; the heartbeat
  AC surfaces a stalled/dead loop. (GHC note: a long-poll `receive` parks in the IO
  manager, not on an MVar/STM, so the deadlock detector never culls it and GC never
  collects the running thread — provided it stays held by the supervisor, never
  fire-and-forget. So the long-running `receive` IO is safe; the only real liveness
  risk is an unhandled exception, addressed above.)
- **Graceful shutdown.** Bracket the loop so process shutdown tears it down cleanly;
  in-flight un-acked messages simply redeliver (safe — idempotent publish covers it).
- **Batch concurrency.** `receive` returns up to `sqsBatchSize` (≤10). Decide
  sequential vs bounded-concurrent processing of a batch (throughput vs the per-message
  visibility budget).
- **Ack-within-visibility discipline.** Ack on success; on a long publish call
  `extendVisibility` *before* the `sqsVisibilityTimeout` (30s) lapses; on failure let
  it redeliver (don't ack). Make the per-job processing-time vs visibility-budget
  relationship explicit.
- **Long-poll vs HTTP timeout (cross-ref S18).** The `receive` request relies on the
  HTTP response timeout exceeding the long-poll window (`sqsWaitSeconds`, 20s). S18's
  `receiveRequest` should pin an explicit `responseTimeout > sqsWaitSeconds` (or confirm
  amazonka's default exceeds it) so a client-side timeout never cuts a long-poll short.

**Deferred composition wiring (from [#144](https://github.com/AlexaDeWit/Ecluse/pull/144)).**
The per-ecosystem composition root wired the *serve* side (each mount's
`PackumentDeps`); the *publish* side — a configured `RegistryClient` per ecosystem —
was deliberately left to this slice, the worker being its only consumer. Today
`Env.envRegistry` is a single global refusing placeholder, so this slice resolves it
from config and retires the single global slot — the deferred D5 tail of the
per-ecosystem composition-root work.
