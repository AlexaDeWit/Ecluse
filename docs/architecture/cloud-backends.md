# Cloud backends and mirroring

> Part of the [Écluse architecture overview](../architecture.md).

How Écluse mirrors approved public packages, and the two handles that couple it to a cloud
provider.

## Mirror queue

Mirroring is demand-driven. A client pulls an artifact on the tarball path after a private-upstream
miss. If that version passes the rules, the proxy enqueues a mirror job (package, version, artifact
location, filename). It serves the client immediately and never waits for the mirror to finish.
Metadata requests filter but never mirror, so Écluse mirrors only the versions a client fetches.
The mount configuration resolves the publish target, which the message never names.

A separate worker receives jobs and, for each one:

1. **Probes the mirror target** and acknowledges a confirmed-present duplicate outright, sparing
   a fleet-wide install's duplicate jobs a full download and no-op publish. A probe that cannot
   tell falls through to the full pipeline.
2. **Re-evaluates current policy** through the same shared admission gate the serve path runs
   (`Ecluse.Core.Package.Admission`), and re-checks the fetch URL against the mount's
   tarball-host gate. The worker drops a version refused since its serve-time admit: a new
   advisory, a raised floor, a withdrawn file. Such a version never freezes into the
   rule-exempt mirror.
3. Fetches the artifact from the public upstream.
4. **Verifies its bytes against the re-admitted artifact's integrity digest**: npm
   `dist.integrity`, read from the current metadata. The queue payload carries no digest at all.
5. Publishes to the mirror target and acknowledges the job.

A hash mismatch fails the job. The worker publishes nothing, routes it to retry or the
dead-letter path, and alarms. A corrupt or tampered artifact therefore never enters the private
upstream, which Écluse later serves without rules. At-least-once delivery is safe because
publishing is idempotent: versions are immutable, so a redelivered job finds the version already
present and succeeds.

Between approval and a package appearing in the private upstream, requests fall through to the
public upstream and re-run the deterministic rules.

The mirror worker exists only when a mount mirrors. A serve-only deployment starts no worker and
builds no queue. The worker runs inside the `ecluse proxy` process as a supervised concurrent
thread, not a separate service. Its consume-loop heartbeat backs the process's liveness surface,
distinct from HTTP readiness. The configured queue URL picks the backend: SQS for
durability, or a bounded in-memory queue with a boot warning when unset. The keys are in
[USAGE](../../USAGE.md).

```mermaid
sequenceDiagram
    autonumber
    participant W as Mirror worker
    participant Queue as Mirror queue
    participant Pub as Public upstream
    participant Cred as CredentialProvider
    participant Mirror as Mirror target

    loop consume loop
        W->>Queue: receive (long-poll)
        alt no message
            Queue-->>W: empty batch (timeout)
        else job delivered
            Queue-->>W: mirror job
            W->>Pub: fetch artifact
            Pub-->>W: bytes
            Note over W: verify bytes against dist.integrity
            alt hash mismatch
                W-->>Queue: do not ack (retry / DLQ) + alarm
            else verified
                W->>Cred: currentToken
                Cred-->>W: bearer token
                W->>Mirror: publishArtifact (npm protocol + token)
                alt published or already-exists
                    W->>Queue: ack
                else publish failed
                    W-->>Queue: do not ack (retry / DLQ)
                end
            end
        end
    end
    Note over W,Mirror: at-least-once delivery + idempotent publish
```

### The terminus for a job that can never succeed

A transient failure retries because the worker does not ack the message. That works only while
something eventually stops the retrying. A job that can never succeed has to end somewhere. An
artifact past the per-artifact byte cap and a payload that no longer decodes are both such jobs.
Without a terminus the worker fetches and refuses the job on every redelivery, for as long as
the queue holds it.

The intended terminus is the operator's own **dead-letter queue**. Écluse returns such a message
without deleting it. An attached SQS redrive policy then moves the message to the dead-letter
queue and keeps it for inspection. That is the only outcome that preserves a forensic trail.
Écluse therefore probes the queue once at boot. A durable queue with no redrive policy attached
earns a loud start-up warning, because the operator cannot see what their proxy could not mirror.

A warning alone leaves the message cycling, so a second and weaker terminus sits beneath it: a
**redelivery budget**. Every delivery carries its own count. The worker discards a message that
has used up that budget. It writes an error log that names the job, and records a distinct
`discarded` result on the mirror-job counter.

Discarding retains nothing, so it is deliberately the lesser outcome. Écluse holds the budget one
delivery above an attached policy's own `maxReceiveCount`. A dead-letter queue therefore always
captures the message first, and the budget fires only for a deployment that has none. Discarding
is safe in the way the rest of mirroring is safe. Mirroring is demand-driven, so the job returns
on the next pull of that artifact and fails the same way until the cause is fixed.

The worker checks the budget before it runs the job, not after. That check spares the repeated
fetch the cycling would otherwise pay for.

The in-memory queue sits outside all of this. It never redelivers a job, so no delivery can
exhaust a budget and there is nothing to capture. Its terminus is the drop that a delivered job
already is. Its observability is the worker's error log and metric.

## Cloud backends

Écluse couples to a cloud provider through exactly two handles. A new provider is an additive
backend, not a structural change, the same posture as the [registry
abstraction](registry-model.md#registry-abstraction):

1. **`MirrorQueue`**, the durable hand-off from the request path to the [mirror
   worker](#mirror-queue).
2. **`CredentialProvider`**, which mints the short-lived bearer token for the managed registry
   (see [Credential provider](#credential-provider)).

One npm data plane, publish included, serves every cloud, because a managed registry is an npm
endpoint plus a token. There is no per-cloud publish path. The mirror and publish paths need no
object-store handle, and only the advisory-database sync uses S3. AWS ships today.

### Service mapping

| Concern | AWS (shipped) |
|---------|-----|
| Mirror queue | SQS |
| Managed npm registry | CodeArtifact |
| Workload identity / token source | STS / instance role |

The managed registry speaks the npm protocol over HTTPS. Only the token source differs per
cloud.

### Credential provider

Outbound auth (proxy to registry) is its own handle. A `CredentialProvider` yields the current
bearer token for a registry endpoint, refreshing before expiry. Only the mirror-target write uses
it: reads forward the caller's own credential and the public upstream is anonymous
([Credential flow and authority](registry-model.md#credential-flow-and-authority)). The
credential is derived from the mirror-target URL, see
[Outbound registry credentials](configuration.md#outbound-registry-credentials).

Only an expired token *and* a still-failing mint fail the dependent operation, the mirror
publish. The worker leaves that job un-acked, and it retries or dead-letters. This never touches
the client serve path.
