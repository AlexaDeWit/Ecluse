---
id: S29
title: Artifact Registry / ADC credential leaf
milestone: M7, GCP backends
status: not-started
depends-on: [S16]
test-tier: [smoke]
arch-refs:
  - docs/architecture/cloud-backends.md#credential-provider
  - docs/architecture/cloud-backends.md#service-mapping
pr: null
---

# S29, Artifact Registry / ADC credential leaf

> Milestone **M7** · depends on: [S16](S16-credential-wrapper.md) · tier: smoke

**Goal.** The GCP per-cloud credential leaf: mint an OAuth2 access token via
Application Default Credentials (ADC, TTL ~1h), wrapped by the S16 generic policy,the GCP analogue of the CodeArtifact leaf (S17).

**Acceptance criteria.**
- [ ] `newAdcProvider :: ... -> IO CredentialProvider` mints an OAuth2 access token
  from ADC, returning an `AuthToken` with the real `expiresAt` (~1h) so the S16
  wrapper refreshes off the token's own lifetime, the wide TTL spread vs
  CodeArtifact is exactly why the wrapper keys on `expiresAt`., _cloud-backends.md#credential-provider_
- [ ] ~10 lines of cloud-specific code; all policy stays in S16., _cloud-backends.md#credential-provider_
- [ ] Real mint exercised in **smoke** only (no emulator for the OAuth2 token
  endpoint)., _cloud-backends.md#testing_

**File scope.**
- `src/Ecluse/Credential/Adc.hs`, the leaf + smart constructor.
- `ecluse.cabal`, add the ADC/OAuth2 client dep (per S27's gogol-vs-hand-rolled outcome, reused if applicable).
- `test/smoke/Ecluse/Credential/AdcSpec.hs`, real mint against a sandbox project (secret-gated, non-gating).

**Test tier.** Smoke, un-emulable token mint; allowed to fail, secret-gated.

**Notes / risks.** Mirrors S17 exactly in shape, the handle split is what makes GCP an
additive leaf, not a structural change. Keep it tiny.
