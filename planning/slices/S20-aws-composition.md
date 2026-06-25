---
id: S20
title: AWS composition root + config wiring (launch-ready)
milestone: M4 — AWS cloud backends & worker
status: in-progress
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
  resolution is delivered by the **base-hardening track** (D5);
  S20 layers AWS backend selection on top of it.) —
  _cloud-backends.md#handles-records-of-functions, configuration.md#outbound-registry-credentials_
- [ ] **Credential providers are process-global; mounts reference them**
  (base-hardening D4): the composition root builds the
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

## As-built notes

The D4/D5 base-hardening track had already landed most of the composition root
(`Ecluse.Composition` + `Ecluse.Config`): config-driven mounts, the process-global
credential providers (the `static` leaf), boot-time fail-fast aggregation
(`planMounts` / `planPublishTargets`), and the publish-side `RegistryClient`
resolution. S20 layered the remaining AWS backend selection on top of that, rather
than reimplementing it:

- **Mirror-queue selection (AC1 / AC3).** `run` previously hard-wired the
  `newInMemoryQueue` test double. It now selects the backend from config through a
  pure `Ecluse.Composition.planMirrorQueue :: EnvConfig -> Either [BootError] SqsConfig`:
  `MIRROR_QUEUE_PROVIDER=sqs` → an `SqsConfig` (from `MIRROR_QUEUE_URL` + `AWS_REGION`)
  that `run` hands to `newSqsQueue`; `pubsub` (the GCP arm) → a clear
  `QueueProviderUnavailable` boot error (no silent fallback); `sqs` with no
  `AWS_REGION` → a `QueueRegionMissing` boot error. The single `newSqsQueue` call lives
  in `run`; `planMirrorQueue` is the single place the not-built decision lives.
- **Static credential path is the AWS launch path.** A deployment with
  `MIRROR_TARGET_TOKEN` (static write credential) + an SQS queue + `AWS_REGION` is a
  fully working AWS-backed npm proxy today — that is the launch gate this slice
  closes. A mount that names an uninitialized provider still fails fast at boot
  (the D4 check), unchanged.
- **End-to-end integration test (AC4).** `test/integration/Ecluse/AwsEndToEndSpec.hs`
  drives an in-process Écluse (the real `Ecluse.Server.application` + the real
  `Ecluse.Worker.workerLoop`) over a **real SQS queue** (a `ministack` container) and
  WAI npm stubs: a packument request is filtered by the rules, a tarball request is
  gated and enqueues a real SQS job, and the worker fetches → verifies the integrity
  digest → publishes it to the mirror-target stub.

### Deferred / escalated (do not mark merged without the architect's call)

1. **CodeArtifact auto-mint wiring (`newCodeArtifactProvider`) is NOT wired.** The
   leaf exists (S17), but `GetAuthorizationToken` needs a CodeArtifact **domain**
   (and optionally owner / token-duration), and the design-of-record config surface
   (`docs/architecture/configuration.md`, `USAGE.md`) defines **no** production key
   for it — only the smoke-tier `ECLUSE_SMOKE_CODEARTIFACT_*` vars exist. Wiring it
   would mean inventing operator-facing config keys (or deriving the domain from
   `MIRROR_TARGET_URL`), which is an architect-level config-surface decision.
   `codeartifact` therefore stays the honest boot failure D4/D5 already implements.
   AWS launch ships on `static`; CodeArtifact auto-mint is a follow-up once the
   config surface is decided.
2. **`run` no longer has an in-memory queue path.** `MIRROR_QUEUE_PROVIDER` defaults
   to `sqs`, so the production entry now requires a reachable SQS endpoint (and
   `AWS_REGION`). There is **no** config key to point production `run` at an SQS
   emulator (the `SqsEndpoint` override is test-only), so the `test/e2e` harness —
   which deliberately ran the real image over the in-memory queue with a dummy
   `MIRROR_QUEUE_URL` — needs a queue strategy on rebase (a `ministack` queue
   container, or a decision on the emulator-endpoint config).
