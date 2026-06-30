---
id: S18
title: SQS MirrorQueue backend
milestone: M4, AWS cloud backends & worker
status: merged
depends-on: [S02]
test-tier: [integration]
arch-refs:
  - docs/architecture/cloud-backends.md#queue-abstraction
  - docs/architecture/cloud-backends.md#service-mapping
  - CONTRIBUTING.md#integration-tests--ecluse-integration-gating
pr: null
---

# S18, SQS `MirrorQueue` backend

> Milestone **M4** · depends on: [S02](S02-handle-interfaces.md) · tier: integration

**Goal.** The AWS queue backend behind the `MirrorQueue` handle:
`SendMessage`/`ReceiveMessage`(+visibility timeout)/`DeleteMessage` over
`amazonka-sqs`, verified against a `ministack` container.

**Acceptance criteria.**
- [ ] `newSqsQueue :: SqsConfig -> IO MirrorQueue` implements
  `enqueue`/`receive`(one long-poll, `[]` on timeout)/`ack`/`extendVisibility`, with
  `ReceiptHandle` carrying the SQS receipt handle (opaque).  _cloud-backends.md#queue-abstraction_
- [ ] **Retry-is-don't-ack**: a failed job is simply not acked; the visibility
  timeout redelivers; the SQS-native DLQ (max-receive-count) catches persistent
  failures. No explicit `nack`.  _cloud-backends.md#queue-abstraction_
- [ ] Batch size, long-poll window, visibility timeout are config (sane defaults).   _cloud-backends.md#queue-abstraction_
- [ ] Integration test against **ministack** (image `ministackorg/ministack`, port
  4566) via `testcontainers`: enqueue→receive→ack round-trip; no-ack→redeliver.   _CONTRIBUTING.md#integration-tests--ecluse-integration-gating_

**File scope.**
- `src/Ecluse/Queue/Sqs.hs`, `newSqsQueue`.
- `ecluse.cabal`, add `amazonka-sqs` (and `testcontainers` to the integration suite).
- `test/integration/Ecluse/MirrorQueueSpec.hs`, fill the existing stub with the ministack round-trip.

**Test tier.** Integration, real emulator, hermetic, gating (needs Docker). Enable
the integration Codecov flag (commented in `ci.yml`) once this lands with real cases.

**Notes / risks.** This is the first real integration suite, wiring it green also
flips on the integration coverage flag. Keep provider differences (vs Pub/Sub, S28)
behind the handle; `ReceiptHandle` opacity is what lets both clouds share the worker
(S19). Coordinate the amazonka footprint with S17.
