---
id: S20
title: AWS composition root + config wiring (launch-ready)
milestone: M4, AWS cloud backends & worker
status: merged
depends-on: [S03, S15, S17, S18, S19]
test-tier: [integration]
arch-refs:
  - docs/architecture/cloud-backends.md#handles-records-of-functions
  - docs/architecture/configuration.md#outbound-registry-credentials
  - docs/architecture/cloud-backends.md#service-mapping
  - docs/architecture/access-model.md#credential-strategies-per-mount
pr: 292
---

# S20, AWS composition root + config wiring (**launch-ready**)

> Milestone **M4** · depends on: [S03](S03-config-loader.md), [S15](S15-tarball-path.md), [S17](S17-codeartifact-leaf.md), [S18](S18-sqs-queue.md), [S19](S19-mirror-worker.md) · tier: integration

**Goal.** Tie the AWS backends into the single, config-driven composition root: read
the configured providers, call the matching smart constructors, build the real `Env`,
and run server + worker. This is the slice that makes Écluse a deployable AWS-backed
npm proxy.

**Acceptance criteria.**
- [ ] The composition root reads config (S03) and selects backends,  `ECLUSE_QUEUE_BACKEND=sqs` → `newSqsQueue`; mirror-target credential →
  `newCodeArtifactProvider` or `static` (`ECLUSE_MIRROR_TARGET_TOKEN`), storing the
  resulting handle records in `Env`. Nothing downstream knows which backend it holds.
  (The generic `ecosystem → RegistryClient + classifier + bindingPrefix → MountBinding`
  resolution is delivered by the **base-hardening track** (D5);
  S20 layers AWS backend selection on top of it.),  _cloud-backends.md#handles-records-of-functions, configuration.md#outbound-registry-credentials_
- [ ] **Credential providers are process-global; mounts reference them**
  (base-hardening D4): the composition root builds the
  provider(s) **once** (a single container task role in the common case) and each
  mount *names* which it draws on, always a mirror-target write provider; under the
  default `passthrough`, reads forward the client token / are anonymous (no Écluse
  read credential); the `service` read provider is wired by
  **S44**. A mount referencing an uninitialized provider **fails fast at boot**.,  _access-model.md, cloud-backends.md#credential-provider, configuration.md#validation-fail-fast-reject-the-unknown_
- [ ] End-to-end integration test: a request through an in-process Écluse with a
  ministack queue + a stub npm registry exercises packument-filter, tarball-gate,
  enqueue, and worker fetch→verify→publish., _cloud-backends.md#testing_
- [ ] `make nix-build` produces the runnable binary; the OCI image runs it (the
  release wiring already exists).

**File scope.**
- `src/Ecluse.hs` / `src/Ecluse/Env.hs`, config-driven `newEnv` (backend selection).
- `app/Main.hs`, parse config → build Env → run (still thin).
- `test/integration/Ecluse/AwsEndToEndSpec.hs`, the full AWS-backed path.
- `README.md`, mark the AWS npm proxy as functional (deployment detail is S32).

**Test tier.** Integration, the composition root exercised end-to-end against
emulators/stubs.

**Notes / risks.** **This is the AWS launch gate**, when it merges, the core
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
  `ECLUSE_QUEUE_BACKEND=sqs` → an `SqsConfig` (from `ECLUSE_QUEUE_URL` + `AWS_REGION`)
  that `run` hands to `newSqsQueue`; `pubsub` (the GCP arm) → a clear
  `QueueProviderUnavailable` boot error (no silent fallback); `sqs` with no
  `AWS_REGION` → a `QueueRegionMissing` boot error. The single `newSqsQueue` call lives
  in `run`; `planMirrorQueue` is the single place the not-built decision lives.
- **Mirror-target credential provider selection (AC1 / AC2).** The write provider is
  selected by `ECLUSE_MIRROR_TARGET_CREDENTIAL_PROVIDER` (default `static`) through a pure
  `Ecluse.Composition.planMirrorCredential`: `static` uses `ECLUSE_MIRROR_TARGET_TOKEN`;
  `codeartifact` resolves a `CodeArtifactConfig` (`resolveCodeArtifactConfig`) and
  builds the generic refresh/cache wrapper around the `newCodeArtifactProvider` mint
  leaf (which mints once eagerly, so a misconfigured identity fails loudly at boot);
  the GCP `gcp-artifact-registry` arm is a fail-loud "not built" boot error. AWS
  credentials are the ambient container/task role (amazonka's chain), never an Écluse
  key. CodeArtifact inputs resolve **(a) from explicit `MIRROR_TARGET_CODEARTIFACT_*`
  keys, else (b) parsed from the mirror-target host** `{domain}-{owner}.d.codeartifact.{region}.amazonaws.com`;
  region precedence is explicit key → host (its authoritative region) → `AWS_REGION`.
  The owner must be a 12-digit AWS account id (a host whose tail after the last hyphen
  is not one is not a CodeArtifact endpoint, so it never mis-parses; an explicit
  non-account-id owner is rejected). A required input that resolves by neither route is
  a fail-loud boot error naming the exact key. The boot-time mint failure is caught and
  rendered through the aggregated boot block (legible transient-vs-permanent) while
  keeping the eager-mint fail-fast posture; the SQS endpoint-override secret key is
  carried as a redacted `Secret` end to end.
- **Mirror-target URL fold.** `ECLUSE_MIRROR_TARGET` is now optional and folds onto
  `ECLUSE_PRIVATE_UPSTREAM` when unset (one registry, read and written). The write
  **credential** does not fold, it stays the explicit provider above. The read-side
  (`PRIVATE_UPSTREAM_*`, S44) and publish-target (`PUBLICATION_TARGET_*`, S52) providers
  will follow the same prefixed-provider pattern when those slices land.
- **SQS endpoint override (AWS-SDK-standard).** `planMirrorQueue` honours
  `AWS_ENDPOINT_URL_SQS` (else `AWS_ENDPOINT_URL`), parsing it into the backend's
  `SqsEndpoint` (signed with `AWS_ACCESS_KEY_ID`/`AWS_SECRET_ACCESS_KEY`) so the
  **released image** can target a local `ministack` SQS with no test-only code path
  (preserving ship==test for the e2e tier); a malformed override is a fail-loud boot
  error; unset ⇒ normal AWS resolution.
- **Static credential path is the AWS launch path.** A deployment with
  `ECLUSE_MIRROR_TARGET_TOKEN` (static write credential) + an SQS queue + `AWS_REGION` is a
  fully working AWS-backed npm proxy. A mount that names an uninitialized provider
  still fails fast at boot (the D4 check), unchanged.
- **End-to-end integration test (AC4).** `test/integration/Ecluse/AwsEndToEndSpec.hs`
  drives an in-process Écluse (the real `Ecluse.Server.application` + the real
  `Ecluse.Worker.workerLoop`) over a **real SQS queue** built through the
  config-driven composition root (`planMirrorQueue` → `newSqsQueue`, driven by the
  `AWS_ENDPOINT_URL_SQS` prod key against a `ministack` container, no test-only code path)
  and WAI npm stubs: a packument request is filtered by the rules, a tarball request is
  gated and enqueues a real SQS job, and the worker fetches → verifies the integrity
  digest → publishes it to the mirror-target stub.

### Notes for the e2e rebase

`run` no longer has an in-memory queue path: `ECLUSE_QUEUE_BACKEND` defaults to `sqs`,
so the production entry requires a reachable SQS endpoint and `AWS_REGION`. The
`test/e2e` harness (which ran the real image over the in-memory queue with a dummy
`ECLUSE_QUEUE_URL`) now points the image at a `ministack` SQS via the new
`AWS_ENDPOINT_URL_SQS` key, the e2e agent adds the `ministack` queue container and
sets the var (that harness change is out of this slice's scope).
