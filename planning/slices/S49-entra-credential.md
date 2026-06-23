---
id: S49
title: Entra ID credential leaf
milestone: M10 — Azure backends
status: not-started
depends-on: [S16]
test-tier: [smoke]
arch-refs:
  - docs/architecture/cloud-backends.md#credential-provider
  - docs/architecture/cloud-backends.md#azure-backends-designed-for-furthest-out
pr: null
---

# S49 — Entra ID credential leaf

> Milestone **M10** · depends on: [S16](S16-credential-wrapper.md) · tier: smoke

**Goal.** Supply the per-cloud `mintToken` leaf for Azure, behind the generic
[`CredentialProvider`](../../docs/architecture/cloud-backends.md#credential-provider)
wrapper already built in S16. This is the **low-risk** Azure arm — a Microsoft Entra
ID (Azure AD) bearer token over plain HTTPS+JSON, no SDK, ~the size of the ADC leaf.

**Acceptance criteria.**
- [ ] `mintToken` acquires an Entra access token via **Managed Identity (IMDS)** —
  `GET http://169.254.169.254/metadata/identity/oauth2/token?api-version=2018-02-01&resource={res}`
  with header `Metadata: true` — parsing `access_token` + `expires_on` into
  `AuthToken`. — _cloud-backends.md#credential-provider_
- [ ] Alternative acquisition via **Workload Identity Federation** (AKS): read the
  projected SA token (audience `api://AzureADTokenExchange`), exchange it at the Entra
  token endpoint. — _cloud-backends.md#azure-backends-designed-for-furthest-out_
- [ ] `resource` for an Azure Artifacts mirror target is the Azure DevOps app ID
  `499b84ac-1321-427f-aa17-267ca6975798`; TTL (~1h) flows through the existing
  refresh-off-`expiresAt` wrapper (no new refresh policy). — _cloud-backends.md#credential-provider_
- [ ] The leaf is the only un-unit-testable surface; the wrapper around it stays
  deterministic (injected clock + fake mint), per the existing pattern. — _cloud-backends.md#testing_

**File scope.**
- `src/Ecluse/Credential/Entra.hs` — the `mintToken` leaf (IMDS + federation paths).
- `test/smoke/Ecluse/Credential/EntraSpec.hs` — real Entra mint (non-gating smoke).
- `ecluse.cabal` — register the module.

**Test tier.** Smoke — real token mint runs end-to-end only in the non-gating tier,
exactly like the CodeArtifact / ADC leaves.

**Notes / risks.** No SDK and no emulator needed — token acquisition is HTTPS+JSON, so
this arm carries none of the queue risk (S47). Keep the leaf tiny; all refresh / cache
/ single-flight / circuit-breaker policy already lives in the S16 wrapper.
