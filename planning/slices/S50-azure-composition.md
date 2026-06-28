---
id: S50
title: Azure composition wiring
milestone: M10, Azure backends
status: not-started
depends-on: [S03, S48, S49]
test-tier: [integration]
arch-refs:
  - docs/architecture/cloud-backends.md#cloud-backends
  - docs/architecture/cloud-backends.md#azure-backends-designed-for-furthest-out
  - docs/architecture/configuration.md#configuration
pr: null
---

# S50, Azure composition wiring

> Milestone **M10** · depends on: [S03](S03-config-loader.md), [S48](S48-azure-queue.md), [S49](S49-entra-credential.md) · tier: integration

**Goal.** Light up the Azure arms of the config-driven composition root:
`MIRROR_QUEUE_PROVIDER=servicebus` (or `azurequeue`) → the S48 backend; the Entra
credential leaf (S49) for an **Azure Artifacts** mirror target. The third backend
behind the same two handles, closing "AWS, GCP, **and Azure**."

**Acceptance criteria.**
- [ ] Composition root selects the Azure backends from config (the Azure region /
  tenant / namespace scoping, the chosen queue provider, the Entra leaf).,  _configuration.md#configuration_
- [ ] The proxy core, rules, web layer, worker, and CVE subsystem are **unchanged**,  Azure is purely additive behind the two handles., _cloud-backends.md#cloud-backends_
- [ ] Azure Artifacts is reached through the **unchanged** npm `RegistryClient` (npm
  protocol over HTTPS + Entra bearer); no per-cloud publish path is added.,  _cloud-backends.md#azure-backends-designed-for-furthest-out_
- [ ] End-to-end integration test on Azure backends (Azurite or namespace per S47 +
  stub npm registry) mirrors the AWS/GCP end-to-end., _cloud-backends.md#testing_
- [ ] Config + README rows for Azure added and marked functional.

**File scope.**
- `src/Ecluse.hs` / `src/Ecluse/Env.hs`, Azure backend selection (additive).
- `test/integration/Ecluse/AzureEndToEndSpec.hs`, full Azure-backed path.
- `README.md` / `docs/` / `USAGE.md`, document the Azure backend.

**Test tier.** Integration, Azure composition end-to-end against the S47 path + stubs.

**Notes / risks.** This is the **furthest-out** slice in the plan, lowest priority,
after AWS and GCP. The handle design means it touches only the composition root + a
new test, no core changes. Carry forward any residual caveats from S47/S48 (e.g.
Service Bus REST having only smoke coverage) and **escalate** material ones.
