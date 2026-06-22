---
id: S30
title: GCP composition wiring
milestone: M7 — GCP backends
status: not-started
depends-on: [S03, S28, S29]
test-tier: [integration]
arch-refs:
  - docs/architecture/cloud-backends.md#cloud-backends
  - docs/architecture/configuration.md#configuration
  - docs/architecture/configuration.md#outbound-registry-credentials
pr: null
---

# S30 — GCP composition wiring

> Milestone **M7** · depends on: [S03](S03-config-loader.md), [S28](S28-pubsub-queue.md), [S29](S29-adc-credential.md) · tier: integration

**Goal.** Light up the GCP arms of the config-driven composition root:
`MIRROR_QUEUE_PROVIDER=pubsub` → `newPubSubQueue`; ADC credential leaf for an
Artifact Registry mirror target — replacing the "not yet built" routes left honest in
S20.

**Acceptance criteria.**
- [ ] Composition root selects the GCP backends from config
  (`GOOGLE_CLOUD_PROJECT`, `pubsub`, ADC). — _configuration.md#configuration, #outbound-registry-credentials_
- [ ] The proxy core, rules, web layer, worker, and CVE subsystem are **unchanged** —
  GCP is purely additive behind the two handles. — _cloud-backends.md#cloud-backends_
- [ ] End-to-end integration test on GCP backends (Pub/Sub emulator + stub npm
  registry) mirrors the AWS end-to-end (S20). — _cloud-backends.md#testing_
- [ ] Config docs updated (the GCP rows already exist; mark them functional).

**File fence.**
- `src/Ecluse.hs` / `src/Ecluse/Env.hs` — GCP backend selection (additive to S20).
- `test/integration/Ecluse/GcpEndToEndSpec.hs` — full GCP-backed path.
- `README.md` / `docs/` — mark GCP functional.

**Test tier.** Integration — GCP composition end-to-end against emulators/stubs.

**Notes / risks.** This closes the "AWS and GCP are both first-class" goal. The whole
point of the handle design is that this slice touches only the composition root + a new
test — no core changes. If S27/S28 surfaced client limitations, reflect any residual
caveats here and **escalate** material ones.
