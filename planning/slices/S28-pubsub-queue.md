---
id: S28
title: Pub/Sub MirrorQueue backend
milestone: M7, GCP backends
status: not-started
depends-on: [S02, S27]
test-tier: [integration]
arch-refs:
  - docs/architecture/cloud-backends.md#queue-abstraction
  - docs/architecture/cloud-backends.md#service-mapping
pr: null
---

# S28, Pub/Sub `MirrorQueue` backend

> Milestone **M7** · depends on: [S02](S02-handle-interfaces.md), [S27](S27-gcp-spike.md) · tier: integration

**Goal.** The GCP queue backend behind the existing `MirrorQueue` handle, implemented
per the S27 decision: `Publish` / `Pull`(+ack-deadline) / `Acknowledge`, verified
against the Pub/Sub emulator.

**Acceptance criteria.**
- [ ] `newPubSubQueue :: PubSubConfig -> IO MirrorQueue` implements the handle
  (`enqueue`/`receive`/`ack`/`extendVisibility`), `ReceiptHandle` carrying the
  Pub/Sub `ackId` (opaque).  _cloud-backends.md#queue-abstraction_
- [ ] Provider differences (ack deadline vs visibility timeout, batch limits,
  dead-letter wiring) stay behind the handle; the worker (S19) is unchanged.   _cloud-backends.md#queue-abstraction, #service-mapping_
- [ ] Integration test against the Pub/Sub emulator: enqueue→receive→ack;
  no-ack→redeliver.  _cloud-backends.md#testing_

**File scope.**
- `src/Ecluse/Queue/PubSub.hs`, `newPubSubQueue` (client per S27 decision).
- `ecluse.cabal`, add the client dep decided in S27.
- `test/integration/Ecluse/PubSubQueueSpec.hs`, emulator round-trip.

**Test tier.** Integration, Pub/Sub emulator, hermetic, gating.

**Notes / risks.** The worker and the whole proxy core are unchanged, this is purely
an additive backend behind the handle (the design's whole point). Reuse the S19 worker
as-is. Keep the client surface minimal per the S27 hand-roll-vs-SDK outcome.
