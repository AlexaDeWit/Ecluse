---
id: S27
title: Pub/Sub de-risking spike (decision gate)
milestone: M7, GCP backends
status: not-started
depends-on: [S18]
test-tier: [integration]
arch-refs:
  - docs/architecture/cloud-backends.md#haskell-client-maturity--a-design-risk-to-retire-early
  - docs/architecture/technology-stack.md
pr: null
---

# S27, Pub/Sub de-risking spike (decision gate)

> Milestone **M7** · depends on: [S18](S18-sqs-queue.md) · tier: integration · **scheduled after AWS launch (S20)**

**Goal.** Retire the one named GCP design risk before committing the Pub/Sub backend:
stand up Google's Pub/Sub emulator via `testcontainers` and prove **one** client path
can `publish → pull → ack` against it. This experiment resolves both client-maturity
(`gogol` vs a hand-rolled REST client) and emulator (gRPC-first) compatibility.

**Acceptance criteria.**
- [ ] Pub/Sub emulator container started via `testcontainers`; the client points at
  it via `PUBSUB_EMULATOR_HOST` (auth ignored).  _cloud-backends.md#haskell-client-maturity_
- [ ] One client path demonstrably does `publish → pull → ack` end to end.
- [ ] A written **decision** recorded (in this slice file + the PR): `gogol` or a
  thin hand-rolled REST client over `http-client`+`aeson`, with the evidence
  (coverage, emulator compatibility).  _cloud-backends.md#haskell-client-maturity, technology-stack.md_

**File scope.**
- `test/integration/Ecluse/PubSubSpikeSpec.hs`, the spike (may live as an explicitly-marked exploratory suite).
- `ecluse.cabal`, add the candidate client dep (`gogol-pubsub` or none if hand-rolled).
- this slice file, record the outcome decision.

**Test tier.** Integration, emulator round-trip.

**Notes / risks.** This is a **decision gate**, not production code, its output is a
resolved client choice that S28 implements. If the emulator/client question can't be
resolved cleanly, **escalate** (it changes the GCP dependency strategy). The hedge
that fits the project philosophy (hand-roll the small REST surface) is favoured *if*
the emulator serves those REST calls. AWS carries no equivalent risk, which is why it
shipped first.
