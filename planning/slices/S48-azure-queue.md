---
id: S48
title: Azure MirrorQueue backend
milestone: M10 — Azure backends
status: not-started
depends-on: [S02, S47]
test-tier: [integration]
arch-refs:
  - docs/architecture/cloud-backends.md#queue-abstraction
  - docs/architecture/cloud-backends.md#azure-backends-designed-for-furthest-out
pr: null
---

# S48 — Azure `MirrorQueue` backend

> Milestone **M10** · depends on: [S02](S02-handle-interfaces.md), [S47](S47-azure-spike.md) · tier: integration

**Goal.** Implement the `MirrorQueue` handle for Azure using the product + client the
[S47](S47-azure-spike.md) spike chose — `newServiceBusQueue` (Service Bus, REST) or
`newAzureStorageQueue` (Storage Queues, REST). Purely additive behind the existing
handle; the worker and proxy core are unchanged.

**Acceptance criteria.**
- [ ] `enqueue` / `receive` (long-poll) / `ack` / `extendVisibility` implemented over
  the chosen product, mapping onto its primitives (Service Bus: send / peek-lock /
  complete / renew-lock; or Storage Queues: put / get+visibility-timeout / delete /
  update-visibility). — _cloud-backends.md#queue-abstraction_
- [ ] **Dead-letter** honoured: native (Service Bus `MaxDeliveryCount`) or the
  poison-queue-by-`DequeueCount` shim (Storage Queues), per S47. — _cloud-backends.md#queue-abstraction_
- [ ] `ReceiptHandle` stays **opaque** (lock token / pop-receipt); retry is
  "don't ack"; redelivery-safe via idempotent publish. — _cloud-backends.md#mirror-queue_
- [ ] Integration test exercises the handle against Azurite (Option B) or a real
  namespace in smoke (Option A), per S47's decision. — _cloud-backends.md#testing_

**File scope.**
- `src/Ecluse/Queue/Azure.hs` — the smart constructor + REST client behind `MirrorQueue`.
- `test/integration/Ecluse/Queue/AzureSpec.hs` — handle round-trip against the emulator/namespace.
- `ecluse.cabal` — register the module (+ any client dep, likely none beyond `http-client`/`aeson`).

**Test tier.** Integration — `MirrorQueue` handle verified per the S47 path.

**Notes / risks.** Honour the 5-min Service Bus lock cap (renew proactively) if
Option A. Keep all Azure specifics behind the handle — nothing downstream learns which
cloud it holds. If S47 left residual caveats (e.g. REST-only smoke coverage), carry
them here and **escalate** anything material.
