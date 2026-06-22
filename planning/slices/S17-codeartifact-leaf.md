---
id: S17
title: CodeArtifact mintToken leaf
milestone: M4 — AWS cloud backends & worker
status: not-started
depends-on: [S16]
test-tier: [smoke]
arch-refs:
  - docs/architecture/cloud-backends.md#credential-provider
  - docs/architecture/cloud-backends.md#service-mapping
  - docs/architecture/web-layer.md#control-plane-vs-data-plane
pr: null
---

# S17 — CodeArtifact `mintToken` leaf

> Milestone **M4** · depends on: [S16](S16-credential-wrapper.md) · tier: smoke

**Goal.** The AWS per-cloud leaf: mint a CodeArtifact bearer token via
`GetAuthorizationToken` (control plane, `amazonka`), wrapped by the S16 generic
policy. ~10 lines of cloud-specific code behind the generic wrapper.

**Acceptance criteria.**
- [ ] `newCodeArtifactProvider :: ... -> IO CredentialProvider` mints via
  `amazonka-codeartifact` `GetAuthorizationToken` (with STS/instance-role identity),
  returning an `AuthToken` with the real `expiresAt` (TTL up to 12h) so the S16
  wrapper refreshes off the token's own lifetime. — _cloud-backends.md#credential-provider, #service-mapping_
- [ ] Control plane only (`amazonka`); the data plane that *uses* the token stays
  `http-client` (S08). — _web-layer.md#control-plane-vs-data-plane_
- [ ] The real mint is exercised in the **smoke** tier only (no emulator covers
  `GetAuthorizationToken`); the policy around it is already unit-tested (S16). —
  _cloud-backends.md#testing_

**File scope.**
- `src/Ecluse/Credential/CodeArtifact.hs` — the leaf + smart constructor.
- `ecluse.cabal` — add `amazonka`, `amazonka-codeartifact`, `amazonka-sts`.
- `test/smoke/Ecluse/Credential/CodeArtifactSpec.hs` — real mint against a sandbox account (secret-gated, non-gating).

**Test tier.** Smoke — the one un-emulable surface; allowed to fail, secret-gated.

**Notes / risks.** This is the only genuinely un-testable-in-CI part of credentials
(accepted residual risk, like the live-registry oracles). Keep the leaf tiny —
all policy lives in S16. Coordinate the amazonka dependency footprint (split
packages) with S18, which also pulls amazonka.
