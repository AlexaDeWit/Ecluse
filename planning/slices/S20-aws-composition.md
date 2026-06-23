---
id: S20
title: AWS composition root + config wiring (launch-ready)
milestone: M4 — AWS cloud backends & worker
status: not-started
depends-on: [S03, S15, S17, S18, S19]
test-tier: [integration]
arch-refs:
  - docs/architecture/cloud-backends.md#handles-records-of-functions
  - docs/architecture/configuration.md#outbound-registry-credentials
  - docs/architecture/cloud-backends.md#service-mapping
  - docs/architecture/access-model.md#credential-strategies-per-mount
pr: null
---

# S20 — AWS composition root + config wiring (**launch-ready**)

> Milestone **M4** · depends on: [S03](S03-config-loader.md), [S15](S15-tarball-path.md), [S17](S17-codeartifact-leaf.md), [S18](S18-sqs-queue.md), [S19](S19-mirror-worker.md) · tier: integration

**Goal.** Tie the AWS backends into the single, config-driven composition root: read
the configured providers, call the matching smart constructors, build the real `Env`,
and run server + worker. This is the slice that makes Écluse a deployable AWS-backed
npm proxy.

**Acceptance criteria.**
- [ ] The composition root reads config (S03) and selects backends —
  `MIRROR_QUEUE_PROVIDER=sqs` → `newSqsQueue`; mirror-target credential →
  `newCodeArtifactProvider` or `static` (`MIRROR_TARGET_TOKEN`) — storing the
  resulting handle records in `Env`. Nothing downstream knows which backend it holds.
  (The generic `ecosystem → RegistryClient + classifier + bindingPrefix → MountBinding`
  resolution is delivered by the **base-hardening track** — [D5](../design-queue.md);
  S20 layers AWS backend selection on top of it.) —
  _cloud-backends.md#handles-records-of-functions, configuration.md#outbound-registry-credentials_
- [ ] **Credential providers are process-global; mounts reference them**
  ([base-hardening D4](../design-queue.md)): the composition root builds the
  provider(s) **once** (a single container task role in the common case) and each
  mount *names* which it draws on — always a mirror-target write provider; under the
  default `passthrough`, reads forward the client token / are anonymous (no Écluse
  read credential); the `service` / `delegated-cache` read provider is wired by
  **S44**. A mount referencing an uninitialized provider **fails fast at boot**. —
  _access-model.md, cloud-backends.md#credential-provider, configuration.md#validation-fail-fast-reject-the-unknown_
- [ ] End-to-end integration test: a request through an in-process Écluse with a
  ministack queue + a stub npm registry exercises packument-filter, tarball-gate,
  enqueue, and worker fetch→verify→publish. — _cloud-backends.md#testing_
- [ ] `make nix-build` produces the runnable binary; the OCI image runs it (the
  release wiring already exists).

**File scope.**
- `src/Ecluse.hs` / `src/Ecluse/Env.hs` — config-driven `newEnv` (backend selection).
- `app/Main.hs` — parse config → build Env → run (still thin).
- `test/integration/Ecluse/AwsEndToEndSpec.hs` — the full AWS-backed path.
- `README.md` — mark the AWS npm proxy as functional (deployment detail is S32).

**Test tier.** Integration — the composition root exercised end-to-end against
emulators/stubs.

**Notes / risks.** **This is the AWS launch gate** — when it merges, the core
product works on AWS. Backend selection must be the *only* place SDK choice lives
(no smearing). The GCP arms of the selection enum exist in config (S03) but route to
"not yet built" until M7; keep that honest (a clear error, not a silent fallback).
After this, M5 (CVE), M6 (observability), and M8 (provenance/docs) layer on; M7 (GCP)
is scheduled.
