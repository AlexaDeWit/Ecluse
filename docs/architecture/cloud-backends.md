# Cloud backends and mirroring

> Part of the [Écluse architecture overview](../architecture.md).

How Écluse mirrors approved public packages, and the backends that carry the queue and each
mount's store.

## Mirror queue

Mirroring is demand-driven. An admitted artifact GET enqueues a version trigger with an optional
filename. Schema version 2 carries no source URL. The worker resolves current paired metadata and
derives the complete inventory and download locations from that read. A queued job does not freeze
the earlier serve-time policy decision. Packument requests filter but never enqueue.

New workers also decode version 1 jobs. They validate the legacy filename and URL, retain the
filename as a demand hint, and discard the URL. Neither legacy field reduces version membership.

Deploy dual-reading workers and the new read visibility support first, stop old workers, then
deploy version 2 producers and enable PyPI mirroring. Old workers must not consume version 2 jobs.
For rollback, stop producers and drain or quarantine version 2 messages before reverting workers.

The worker checks the current first-party declaration before touching a public source. It reads
the target inventory, then tests any durable expected inventory against current target identities.
A complete managed version needs no public fetch or new admission. A filename or version alone
cannot prove PyPI, Cargo, or RubyGems completion. npm can prove its atomic single-artifact version from validated
native package, version, filename and digest facts, so completed npm retries also avoid public reads.

For new files, the worker resolves the supported source inventory and applies current policy,
integrity floors and location checks. It persists expected identities and paired source fields
before uploading anything. Each missing distribution is fetched once, verified against its
strongest supported source digest, and prepared for the native writer. PyPI preparation extracts
bounded archive Core Metadata from those verified bytes. It neither executes archive content nor
changes distribution bytes. The worker stores its independently computed SHA-256 before upload.

One ordinary successful pass reads the target twice, before and after the uploads. The final
read precedes validation of the latest durable record and every declared retained companion.
Only full filename, identity and companion proof acknowledges completion. Duplicate-file replies
are write outcomes, not completion evidence. Conflicting target bytes refuse rather than trigger
repeated publication. Existing receipt, retry and terminal-fault rules still govern failures.

### RubyGems managed source data

RubyGems mirrors the complete supported platform inventory for each admitted raw version.
Durable state binds original dependency rows, publication times, platform identities, and validated compact specifications to verified archive digests.
The worker captures compact specifications during mirroring, not policy evaluation.
It retains no `.gem` bodies in mirror state. Current target proof and all required companions precede completion.
A target timestamp or duplicate upload response cannot establish an original-source copy.
Missing or corrupt state cannot prove faithful mirror reads during a public outage.

CodeArtifact maps RubyGems maintenance to format `ruby`, without a namespace.
Physical deletion uses the existing independent role grants, first-party protection, consent, and caps.
A client yank request is neither a publication operation nor proof of physical deletion.
Backend response fixtures do not establish live AWS service or IAM behaviour.

### Durable PyPI read data

A project record retains each physical filename's first admitted source entry once. Separate
fragment coordinates prevent later admissions from colliding with earlier array positions.
Conditional updates union new identities and refuse conflicting bytes or declarations. A source
URL change does not change immutable byte identity. After an original-location fetch failure, the
worker can use a freshly admitted location with the same recorded file identity. It verifies the
bytes against the retained identity. Native archive upload metadata stays transient.

The project envelope follows the latest successfully committed source admission. File verification
and companion updates leave it unchanged. Fully managed listings restore that envelope. Listings
with unmanaged private versions retain the existing private-envelope authority.

The independently configured S3-compatible store must provide strong reads and conditional
creation and replacement. It retains expected files, supported consumer fields, verified digest
evidence and declared companion bytes. Its permissions and retention requirements are in the
operator manual. Worker deletion remains outside this interface.

Physical uploads become visible incrementally. Écluse checks caller-authorised private metadata
before one conditional project-state read. It combines private locations with retained source
fields only when current target identities prove the full expected view. An incomplete version
needs a validated full public alternative or answers unavailable. Unknown or malformed state
cannot fall back to current public metadata alone, which may omit historically expected files.
Conclusive state absence keeps unmanaged private handling. First-party names bypass this process.

Project listings keep public release discovery. Complete retained versions remain usable during
a public outage. Requests for another recorded wheel or companion use the authorised private or
retained path without public retrieval, policy re-evaluation or enqueue. These guarantees apply
through Écluse, not to direct reads from the underlying registry.

The mirror worker exists only when a mount mirrors. A deployment with no mirror target starts no
worker and builds no queue. The configured queue URL picks the backend: SQS for durability, or a
bounded in-memory queue with a boot warning when unset. The keys are in
[the operator manual](https://ecluse-proxy.com/docs/).

By default the worker runs inside the `ecluse proxy` process as a supervised concurrent thread,
which is what the in-memory queue requires: its jobs never leave the process that enqueued them.
Over a durable queue the two halves can be split, so the front door scales on request rate and
the worker fleet on queue depth. `ecluse proxy --no-worker` keeps the producer half and
`ecluse mirror` runs the consumer half, both over the same composition root, so a worker's
policy re-evaluation cannot diverge from the serve decision. Écluse refuses either split role at
boot over the in-memory queue, because every job would be stranded.

The consume-loop heartbeat backs the liveness surface of whichever process runs the worker,
distinct from HTTP readiness. `/livez` reports it as a pass-or-fail status plus the instant of
the last successful poll, so an orchestrator can judge staleness itself. A process that runs no
worker reports no poll and stays live on its listener alone.

[Mirror trigger, verification, upload and completion proof](https://ecluse-proxy.com/docs/threat-model/#mirror-writes-and-completion)

[Managed PyPI inventory and retained companions](https://ecluse-proxy.com/docs/threat-model/#pypi-inventory-and-companions)

### Holding a receipt for the life of its job

A durable queue hides a received message for a visibility window and hands it to another consumer
once that window lapses. The worker runs a batch one job at a time, so the window is the wrong
unit: a message waiting its turn, and a job slower than one window, would both be redelivered
while the first worker still held them. Écluse therefore leases every receipt in a batch from the
moment it arrives, renewing each one a third of the way into what is left of its window and never
past the ceiling the backend puts on a single receipt.

The failure policy is per-receipt continuation. A renewal that keeps failing inside its own
receipt's margin drops that receipt alone: a running job is cancelled, a waiting one is skipped,
and the message is left unacknowledged to redeliver normally. Siblings carry on while their own
renewals succeed. Stopping the whole batch on the first renewal failure would be simpler, but it
abandons healthy work for one transient fault. A transport-wide outage still stops the batch, one
receipt at a time, because every renewal in it fails.

<a id="the-terminus-for-a-job-that-can-never-succeed"></a>

### Failed jobs and dead-letter queues

A transient failure retries because the worker does not ack the message. That works only while
something eventually stops the retrying. A job that can never succeed has to end somewhere. An
artifact past the per-artifact byte cap is such a job. Without a terminus the worker fetches and
refuses it on every redelivery, for as long as the queue holds it.

The intended terminus is the operator's own **dead-letter queue**. Écluse returns such a message
without deleting it. An attached SQS redrive policy then moves the message to the dead-letter
queue and keeps it for inspection. That is the only outcome that preserves a forensic trail.
Écluse therefore probes the queue once at boot. A durable queue with no redrive policy attached
earns a loud start-up warning, because the operator cannot see what their proxy could not mirror.

A warning alone leaves the message cycling, so a second and weaker terminus sits beneath it: a
**redelivery budget**. It reaches a decoded job alone. Every delivery carries its own count, and
the worker discards a message that has used up that budget. It writes an error log that names the
job, and records a distinct `discarded` result on the mirror-job counter.

A payload that no longer decodes never becomes a job, so the budget cannot reach it. The poller
logs the drop and leaves the message un-acked, which leaves the dead-letter queue, or the queue's
own retention window, as the only terminus it has.

Discarding retains nothing, so it is deliberately the lesser outcome. The configured count is a
floor. When Écluse can read an attached policy's own `maxReceiveCount`, it holds the budget at
least one delivery above that count, so the dead-letter queue captures the message first and the
budget fires only for a deployment that has none. When the policy's count is unreadable, the
configured floor stands alone. Discarding is safe in the way the rest of mirroring is safe. Mirroring is demand-driven, so the job returns
on the next pull of that artifact and fails the same way until the cause is fixed.

The worker checks the budget before it runs the job, not after. That check spares the repeated
fetch the cycling would otherwise pay for.

The in-memory queue sits outside all of this. It never redelivers a job, so no delivery can
exhaust a budget and there is nothing to capture. Its terminus is the drop that a delivered job
already is. Its observability is the worker's error log and metric.

## Cloud backends

Four axes settle what a deployment runs, and the composition root resolves each one from the single
configuration every role reads, so no two roles disagree about a deployment.

| Axis | Scope | What it settles |
|---|---|---|
| Ecosystem | Per mount | The protocol Écluse speaks to clients and upstreams, as the `RegistryAdapter` |
| Store | Per mount | Where a mount's private packages live, and what its control plane can do |
| Platform | Per deployment | The mirror queue, and the object store the advisory database syncs from |
| Role | Per process | Which faces of the resolved plan a process runs: `ecluse proxy`, `mirror`, `pilot`, `dredger`, `check-config` |

The store and the platform are the two that name a backend, and they pair freely. A cloud is not an
axis of its own: AWS is the SQS platform plus the CodeArtifact store, and SQS carries the queue
whatever store a mount names. Every endpoint a mount declares carries the tag that names its store,
and the [configuration reference](https://ecluse-proxy.com/docs/configuration/) holds the tags with
the keys each one admits.

The queue, credentials, and repository maintenance use the following interfaces. Backend
implementations supply their operations through these records, alongside the
[registry abstraction](registry-model.md#registry-abstraction):

1. **`MirrorQueue`**, the platform's durable hand-off from the request path to the [mirror
   worker](#mirror-queue).
2. **`CredentialProvider`**, which mints a store's short-lived bearer token (see [The credential
   mint](#the-credential-mint)).
3. **`StoreMaintenance`**, which enumerates a store and deletes versions from it for
   `ecluse dredger`. Enumeration and deletion are store operations, not ecosystem ones, because the
   npm wire protocol has no enumeration and a hosted registry deletes through its own control
   plane. Every fact that varies by store is a value the handle supplies: the batch ceiling for a
   destructive call, whether a deleted version can be published again, whether a delete finishes
   before the call answers, the characters its name space partitions by, and whether it has
   somewhere to keep a walk cursor. `StoreObservation` holds the same store's observing calls on
   their own, and each backend builds one directly, so `ecluse dredger --dry-run` reaches no write
   at all.

`MirrorState` is a separate interface for durable managed PyPI completion evidence and
retained read data. It does not store package archives. Its consistency and retention
requirements are described in [durable PyPI read data](#durable-pypi-read-data).

The sweep joins actual mirror and private-cache inventories within the declared mount.
It hands each target's selected versions to its backend once, with a callback that reassesses
policy, consent and current inventory before each backend-owned batch. Request faults stop later
batches, while earlier per-version results remain visible. Destructive clients disable automatic
transport retries so an uncertain call cannot bypass reassessment.

Associated copies share one logical cap charge before their first destructive attempt.
Source operations precede cache operations. A cache residual is rediscovered from actual state
after restart, without an internal pending-work record. The operator contract is in
[Running the Dredger](../../web/content/docs/dredger.md).

### Walking a store

The handle lists a store one bucket of its name space at a time, as a stream of pages, so a large
store costs the memory of one bounded bucket rather than of its whole listing. A bucket is
a prefix of a package name's base component, the part after any namespace, because that is the
component a store's own listing filters on. The characters those prefixes are built from come from
the mount's ecosystem, whose grammar decides what a name may begin with. A store whose listing
carries no filter takes the one bucket that covers everything, so it is one pass rather than a
special case.

A walk over a whole store resumes where it stopped only where the store has somewhere to record
that. CodeArtifact keeps the record in one repository tag, keyed per ecosystem, which is the only
tag `ecluse dredger` writes. A store reached through the ecosystem protocol alone has no such
place: its listing is one document, read whole on every pass, so a walk over it starts
from the first bucket after a restart.

Only the opt-in full walk needs that record. The default cycle carries the names the advisory
database covers and the names an identity deny pins, which is a set the deployment already holds,
so it resumes nothing and writes nothing. That is why the tag-write permission is the operator's
separate decision rather than part of the Dredger's standing grant.

### The two store kinds

A store belongs to one of two kinds, and the kind fixes what the ecosystem contributes and what
the store contributes.

A **hosted registry** speaks the ecosystem's protocol over HTTPS itself, so the data plane is the
ecosystem adapter's, unchanged. One publish path therefore serves every hosted store, because such
a store is a protocol endpoint plus a token. The store adds its credential mint, a control plane
where it has one, its coordinates, and the facts that control plane obeys. CodeArtifact and
Verdaccio are this kind, and it is the kind this build carries.

An **object store** speaks no package protocol, and no build carries one.

### The join

The root joins one ecosystem adapter with one store per mount, and that join is the only place the
two meet. A store backend never imports an adapter, and an adapter never imports a backend. Both
speak the shared vocabulary alone: `Ecosystem`, `PackageName`, and `Version`. A backend that needs
an ecosystem fact reads it off the `Ecosystem` value, as the CodeArtifact format token does.

### Service mapping

| Concern | Axis | What carries it |
|---|---|---|
| Mirror queue | Platform | SQS, or the bounded in-memory queue inside a single process |
| Advisory store | Platform | S3 |
| Workload identity and token source | Deployment credentials, read by the store's mint | STS, through the task or instance role |
| Package store | Store, per mount | CodeArtifact under the `codeArtifact` tag, any host that speaks the ecosystem's protocol under `registry`, and Verdaccio, the development store, under `verdaccio` |
| Store maintenance | Store, per mount | The CodeArtifact control plane, or, for a `verdaccio` store, the ecosystem protocol's own listing and unpublish requests. A `registry` store carries none, so `ecluse dredger` refuses a mirror target under that tag and names it |
| Walk cursor | Store, per mount | One CodeArtifact repository tag per ecosystem. A `verdaccio` store keeps none, so a full walk over it does not resume |
| Private upstream aggregation | Store, per mount | CodeArtifact's `DescribeRepository`, walked over the upstream chain. A `registry` or `verdaccio` store reports none, which the boot warns about once |

### Onboarding a backend

A passing `fullPackageStoreContract` with a concrete driver defines completion for a package store.
The full contract requires reads, writes, inventory, deletion, integrity, and recovery.
Use a direct entrypoint such as `fullPackageStoreContract codeArtifactDriver`.
The driver supplies production handles, deterministic service boundaries, fixtures, and native observations.
The contract owns the common client-visible guarantees and the required family list.
Different stores may use different completion mechanisms under those guarantees.

Current full-contract profiles are Verdaccio npm and CodeArtifact npm, PyPI, and Cargo.
CodeArtifact runs the native formats in one fixed backend job with separate case identities.
The generic `registry` tag lacks maintenance operations and does not claim full package-store completion.
CodeArtifact uses the production adapter with deterministic control-plane doubles.
Its conformance result does not claim that CI performed live AWS operations or minted a real AWS token.
The npm E2E subject adds real-service Verdaccio evidence.
A future package store, including a cache provider such as Valkey, needs its own subject and full contract.
S3 advisory storage does not supply package-store operations.

A new backend supplies deterministic tests for each handle it provides before it counts as onboarded.
Mirror each implementation module with `Spec`, `IntegrationSpec`, or `SmokeSpec` under the matching suite root.
Register those modules in [ecluse.cabal](../../ecluse.cabal).
The [tier rules](../testing.md#what-gates-and-what-doesnt) decide placement from the external collaborator.

| Handle | Required tier and file pattern | Existing example |
|---|---|---|
| `MirrorQueue` | `ecluse-runtime-unit`: `runtime/test/unit/Ecluse/Runtime/Queue/<Backend>Spec.hs` for deterministic adapter behaviour. `ecluse-integration`: `test/integration/Ecluse/Runtime/Queue/<Backend>IntegrationSpec.hs` for the emulated service. | [SqsSpec](../../runtime/test/unit/Ecluse/Runtime/Queue/SqsSpec.hs) and [SqsIntegrationSpec](../../test/integration/Ecluse/Runtime/Queue/SqsIntegrationSpec.hs). [AwsEndToEndIntegrationSpec](../../test/integration/Ecluse/AwsEndToEndIntegrationSpec.hs) exercises the composed serve-to-worker path. |
| `CredentialProvider` | `ecluse-runtime-unit`: `runtime/test/unit/Ecluse/Runtime/Credential/<Backend>Spec.hs` for a stubbed mint. | [CodeArtifactSpec](../../runtime/test/unit/Ecluse/Runtime/Credential/CodeArtifactSpec.hs) asserts the token and expiry extracted from a stubbed response. No automatic test verifies a real AWS mint. |
| `StoreMaintenance` | `ecluse-runtime-unit`: `runtime/test/unit/Ecluse/Runtime/Maintenance/<Backend>Spec.hs` and matching child specs for control-plane responses. Add `test/integration/Ecluse/Runtime/Maintenance/<Backend>IntegrationSpec.hs` when an emulator supplies those operations. | [CodeArtifactSpec](../../runtime/test/unit/Ecluse/Runtime/Maintenance/CodeArtifactSpec.hs) and its [child specs](../../runtime/test/unit/Ecluse/Runtime/Maintenance/CodeArtifact/) cover the control plane with doubles. The protocol implementation uses [Npm/MaintenanceSpec](../../core/test/unit/Ecluse/Core/Registry/Npm/MaintenanceSpec.hs) in `ecluse-core-unit`. [Npm/DredgerE2ESpec](../../test/e2e/Ecluse/E2E/Npm/DredgerE2ESpec.hs) exercises Verdaccio in the npm E2E subject, not live CodeArtifact. |

Supply operation records for every full package-store family in `test/contracts/Ecluse/Test/Contract/`.
Keep acquisition, native faults, polling, and direct observations in the backend's provider.
Call `fullPackageStoreContract` with a named concrete provider in the subject's fixed entrypoint.
Register obligations independently of the provider's current registrations and capability flags.
Check the catalogue against actual Hspec registration and execution, including each required family.
A missing ecosystem profile, family or failed operation must prevent a full completion verdict.
CodeArtifact checks each explicit registration against independently declared native obligations.
Use `task test-conformance-verdaccio` and `task test-conformance-codeartifact` as worked examples.
The core and runtime unit specs also run the same laws under the existing coverage suites.
Use the [contract coverage map](../test-contracts.md) to find existing laws and native extensions.

Cargo maintenance uses CodeArtifact inventory and physical version deletion.
A generic Cargo registry's yank API does not delete the bytes and cannot satisfy deletion obligations.
Refuse that incompatible maintenance capability while preserving valid native reads and publication.

A queue profile declares whether receipt removes the message or leases it for redelivery.
Apply lease, delivery-count, and dead-letter laws only to durable providers.
A maintenance profile preserves `StoreFacts`, consent, batch ceilings, deletion completion, refill behaviour, and optional cursor support.
Keep credential mint request mapping and response parsing in the backend's deterministic tests.
No automatic test verifies a real AWS token mint.
Planned package storage does not inherit support from S3 advisory storage.
Future filesystem and S3 package providers require their own completion visibility contracts when those features ship.

Extend `test/unit/Ecluse/Composition/{MirrorQueue,Credential,Maintenance}Spec.hs` for the new backend's composition choices in `ecluse-unit`.
Shared credential refresh, caching, and expiry policy stays in `ecluse-core-unit`, with a fake clock and mint in
[`CredentialSpec`](../../core/test/unit/Ecluse/Core/CredentialSpec.hs) and
[`Refresh/InternalSpec`](../../core/test/unit/Ecluse/Core/Credential/Refresh/InternalSpec.hs).
Any new smoke case must name its deterministic unit or integration mirror.
Backends that add an ecosystem also owe the [ecosystem checklist](../testing.md#onboarding-an-ecosystem).

### The credential mint

Outbound auth (proxy to registry) is the mint facet of a hosted store. A `CredentialProvider`
yields the current secret for that store's endpoint, refreshing before expiry. Two consumers
use it: the mirror worker's write to the mirror target, and Dredger's store reads, including its
preview reads of the private cache. Client reads forward the caller's own credential and the public
upstream is anonymous
([Credential flow and authority](registry-model.md#credential-flow-and-authority)). The
tag the mirror target declares picks the mint, see
[Outbound registry credentials](configuration.md#outbound-registry-credentials).

The backend binds the secret to its protocol convention. npm renders Bearer credentials. PyPI
retains an explicit Basic username, with `aws` supplied by the CodeArtifact backend and generic
usernames supplied by configuration. The codec invents no username.
Cargo renders the secret directly in `Authorization`, without a Bearer prefix or Basic username.
RubyGems separates Basic read credentials from raw publication API keys and optional typed OTP.
A declared CodeArtifact service role supplies Basic username `aws` for reads and Bearer for writes.
Caller credentials retain their native presentation. Hostname guessing cannot select a service credential scheme.

Each credential provider serialises its mints and keeps its own breaker. During healthy minting,
expired callers wait for the current mint. After cooldown, one mint owns recovery. Other expired
callers receive `BreakerOpen` immediately, while callers with a valid cached token keep using it.
Success closes the breaker. A genuine failure starts a fresh cooldown. Cancellation releases the
mint and recovery claims without counting a failure or adding a cooldown, so another caller can
probe immediately. Older owners cannot settle or release a newer claim.

An expired token with a failed or refused mint fails the dependent store operation. The mirror
worker leaves that job un-acked, and it retries or dead-letters. Client reads forward the caller's
own credential and do not use these providers.
