---
id: S47
title: Azure queue de-risking spike (decision gate)
milestone: M10, Azure backends
status: not-started
depends-on: [S18]
test-tier: [integration]
arch-refs:
  - docs/architecture/cloud-backends.md#azure-backends-designed-for-furthest-out
  - docs/architecture/cloud-backends.md#haskell-client-maturity--a-design-risk-to-retire-early
  - docs/architecture/technology-stack.md
pr: null
issue: 594
---

# S47, Azure queue de-risking spike (decision gate)

> Milestone **M10** · depends on: [S18](S18-sqs-queue.md) · tier: integration · **furthest-out, after AWS *and* GCP launch**

**Goal.** Retire Azure's named queue risk before committing a backend. It is sharper
than GCP's: the only Haskell Service Bus package is deprecated (2014), there is no
AMQP 1.0 Haskell client, and the **official Service Bus emulator serves AMQP only**
(messaging on 5672; the HTTP port is management-only), so a hand-rolled **REST**
Service Bus client **cannot** be tested against the emulator. The spike picks the
queue product by which one a Haskell client can actually drive *and* test:

- **Option A, Service Bus over REST.** Best semantic fit (peek-lock → `receive`,
  renew-lock → `extendVisibility`, complete → `ack`, native dead-letter); but no
  emulator coverage for REST messaging, so integration testing falls to a real
  namespace in the (non-gating) smoke tier only.
- **Option B, Storage Queues over REST + Azurite.** `Azurite` (official storage
  emulator) serves Queues over REST, so the client *is* `testcontainers`-testable;
  cost: **no native dead-letter** (emulate via a poison-queue keyed on `DequeueCount`)
  and a thinner feature set.

**Acceptance criteria.**
- [ ] A round-trip `enqueue → receive → ack` proven for **one** option: Option B
  against Azurite via `testcontainers`, **or** Option A against a real Service Bus
  namespace (smoke-only), with evidence.  _cloud-backends.md#azure-backends-designed-for-furthest-out_
- [ ] A written **decision** recorded (in this slice file + the PR): Service Bus
  (REST, smoke-tested) vs Storage Queues (REST, Azurite-tested), with the
  dead-letter and testability trade-offs that decided it.  _cloud-backends.md#haskell-client-maturity, technology-stack.md_
- [ ] Confirm the **credential** and **registry** arms are *not* part of this risk:
  the Entra token (S49) and Azure Artifacts publish (npm protocol, unchanged) are
  plain HTTPS+JSON.  _cloud-backends.md#azure-backends-designed-for-furthest-out_

**File scope.**
- `test/integration/Ecluse/AzureQueueSpikeSpec.hs`, the spike (explicitly-marked exploratory suite).
- `ecluse.cabal`, add the candidate dep(s) for the chosen path (likely none, hand-rolled REST over `http-client`+`aeson`).
- this slice file, record the outcome decision.

**Test tier.** Integration, Azurite round-trip (Option B) or a smoke round-trip (Option A).

**Notes / risks.** **Decision gate, not production code**, its output is the queue
product + client choice S48 implements. The emulator/REST mismatch is real and
load-bearing: if neither option clears the testability bar cleanly, **escalate**, it
changes Azure's whole queue strategy. Service Bus's lock duration **caps at 5 min**
(vs SQS 12h / Pub/Sub 10min), so under Option A `extendVisibility`/renew-lock becomes
load-bearing for large publishes, not optional. AWS (and even GCP) carry less risk
here, which is why Azure is furthest-out.
