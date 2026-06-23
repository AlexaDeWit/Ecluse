# Architecture Diagrams

> Part of the [Écluse architecture overview](../architecture.md).

A visual companion to the prose specifications under [`architecture/`](.). Like
the rest of these documents, the diagrams describe the **target design** (the
specification being implemented), not necessarily the current state of the code.
Each section links to the document that specifies it in full.

All diagrams are [Mermaid](https://mermaid.js.org/), which GitHub renders inline.

## Contents

1. [System overview](#1-system-overview)
2. [Packument (metadata) request](#2-packument-metadata-request)
3. [Tarball (artifact) request](#3-tarball-artifact-request)
4. [Mirror worker](#4-mirror-worker)
5. [Rules-engine decision flow](#5-rules-engine-decision-flow)
6. [Credential token lifecycle](#6-credential-token-lifecycle)
7. [Credential authority across the three registries](#7-credential-authority-across-the-three-registries)

---

## 1. System overview

A single Écluse binary runs the HTTP server and an in-process mirror worker over a
shared, handle-based `Env`. The **data plane** (metadata + artifact bytes) is
`http-client`; the **control plane** (queue, token mint) sits behind the
[`MirrorQueue`](cloud-backends.md#queue-abstraction) and
[`CredentialProvider`](cloud-backends.md#credential-provider) handles. Solid edges
are request-path / synchronous; dotted edges are best-effort / asynchronous. See
[Registry Model](registry-model.md) and [Cloud Backends](cloud-backends.md).

```mermaid
flowchart LR
    DEV["Developer / CI<br/>(npm, npm ci)"]

    subgraph ecluse["Écluse (single binary)"]
        direction TB
        WEB["Web layer<br/>router, streaming, middleware"]
        RULES["Rules engine<br/>deny-by-default"]
        CACHE["Metadata cache<br/>short-TTL, in-memory"]
        SYNC["Advisory sync<br/>in-memory OSV index"]
        WORKER["Mirror worker<br/>in-process, supervised"]
    end

    subgraph registries["Registries (npm protocol)"]
        PRIV["Private upstream<br/>e.g. CodeArtifact"]
        PUB["Public upstream<br/>registry.npmjs.org"]
        MIRROR["Mirror target<br/>managed npm registry"]
    end

    subgraph handles["Cloud handles"]
        QUEUE["MirrorQueue<br/>SQS / Pub/Sub"]
        CRED["CredentialProvider<br/>mint + refresh token"]
    end

    OSV["OSV advisory exports"]

    DEV -->|"packument / tarball"| WEB
    WEB --> RULES
    WEB --> CACHE
    WEB -->|"read: client token forwarded"| PRIV
    WEB -->|"read: anonymous"| PUB
    WEB -.->|"enqueue (best-effort)"| QUEUE
    RULES -.->|"reads index"| SYNC
    SYNC -->|"periodic pull"| OSV
    WORKER -->|"receive / ack"| QUEUE
    WORKER -->|"fetch artifact"| PUB
    WORKER -->|"token"| CRED
    WORKER -->|"publish (write)"| MIRROR
```

## 2. Packument (metadata) request

Resolving a package **merges** the upstreams rather than short-circuiting: the
private and public upstreams are fetched in parallel; public versions are gated by
the rules and private versions are trusted, and the two are merged into one
document (private wins on collision; integrity divergence is flagged). This is what
keeps not-yet-mirrored public versions visible so demand-driven mirroring can fire.
Metadata requests **filter but never mirror**. See
[Registry Model → Packument merge](registry-model.md#packument-merge-across-upstreams)
and [Rules Engine → Applying verdicts to a packument](rules-engine.md#applying-verdicts-to-a-packument).

```mermaid
sequenceDiagram
    autonumber
    actor Client
    participant E as Écluse
    participant Cache as Metadata cache
    participant Priv as Private upstream
    participant Pub as Public upstream
    participant Rules as Rules engine

    Client->>E: GET packument
    par fetch upstreams in parallel
        E->>Priv: fetch (client token forwarded)
        Priv-->>E: packument (or miss)
    and
        E->>Cache: lookup parsed public metadata
        alt cache miss
            E->>Pub: fetch (anonymous; token stripped)
            Pub-->>E: packument (or miss)
            E->>Cache: store parsed metadata (short TTL)
        end
    end
    E->>Rules: evaluate every public version
    Rules-->>E: verdicts (allow / deny / unavailable)
    Note over E: filter gated (public) versions; trust private;<br/>merge (private wins; flag integrity divergence);<br/>repoint latest; recompute ETag over merged body
    alt no survivors in merge
        E-->>Client: 403 policy / 503 transient or upstream-unavailable
    else some admitted
        E-->>Client: merged + filtered packument
    end
    Note over E,Pub: packument requests filter but never mirror
```

## 3. Tarball (artifact) request

A tarball is gated for that one version. A private hit is streamed unfiltered; a
private miss fetches the version's metadata, runs the rules, and on acceptance
streams from public **and** enqueues a demand-driven mirror job — non-blocking, so
the client is served immediately. See
[Web Layer → Streaming](web-layer.md#streaming-and-resource-lifetime) and
[Cloud Backends → Mirror Queue](cloud-backends.md#mirror-queue).

```mermaid
sequenceDiagram
    autonumber
    actor Client
    participant E as Écluse
    participant Priv as Private upstream
    participant Pub as Public upstream
    participant Rules as Rules engine
    participant Queue as Mirror queue

    Client->>E: GET tarball (e.g. npm ci, direct)
    E->>Priv: fetch (client token forwarded)
    alt private hit (2xx)
        Priv-->>E: tarball stream
        E-->>Client: stream unfiltered (already vetted)
    else private miss
        E->>Pub: fetch version metadata (anonymous)
        E->>Rules: evaluate that one version
        alt denied
            E-->>Client: 403 + denial message
        else unavailable
            E-->>Client: 503 Retry-After or 500
        else admitted
            E->>Pub: stream artifact bytes
            E-->>Client: stream (constant memory, backpressure)
            E-)Queue: enqueue mirror job (best-effort)
        end
    end
    Note over E,Queue: demand-driven — enqueue only when a tarball is accepted
```

## 4. Mirror worker

The worker consumes the queue, fetches each accepted artifact from the public
upstream, **verifies its bytes against the version's integrity hash**, and
publishes to the mirror target via the credential handle. Retry is "don't ack";
at-least-once delivery is safe because publishing is idempotent. See
[Cloud Backends → Mirror Queue](cloud-backends.md#mirror-queue).

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

## 5. Rules-engine decision flow

Each version is evaluated against the rule set. The two tiers are a **performance
ordering, not a precedence ordering**: pure rules run first because they are cheap,
then effectful rules run only where they could still change the winner — and
**precedence decides** (highest-precedence non-abstaining rule wins; deny beats
allow at a tie; all-abstain is denied by default). A needed-but-undecidable rule
yields `Unavailable` and fails closed. See [Rules Engine](rules-engine.md).

```mermaid
flowchart TD
    IN["PackageDetails (one version)"] --> PURE["Pure tier — evaluate rules<br/>(no IO): allow / deny / abstain"]
    PURE --> EFF["Effectful tier — only where it could<br/>still change the winner<br/>(CVE, extra fetch; timeout, retry, breaker)"]
    EFF --> SEL{"highest-precedence<br/>non-abstaining rule"}
    EFF -->|"needed rule cannot decide"| UNAV["Unavailable<br/>(fail-closed)"]
    SEL -->|"allow"| APP["Approved"]
    SEL -->|"deny (or deny beats allow at a tie)"| DEN["Denied"]
    SEL -->|"all abstain"| DBD["DeniedByDefault"]

    AP{{"apply verdict to the request"}}
    APP --> AP
    DEN --> AP
    DBD --> AP
    UNAV --> AP
    AP -->|"packument"| FILT["version filtered out;<br/>latest repointed to newest survivor"]
    AP -->|"artifact"| CODE["allow = 200 stream, deny = 403,<br/>unavailable = 503 or 500"]
```

## 6. Credential token lifecycle

A `CredentialProvider` refreshes a registry token off its own `expiresAt`,
proactively and single-flight, so the request hot path never blocks on a mint in
the common case. Under the default `passthrough` strategy credentials are
**mirror-write only**, so even a fully failed refresh never touches the client serve
path — only the mirror publish; under the `service` / `delegated-cache`
[strategies](access-model.md) a read credential failing does degrade serving. See
[Cloud Backends → Credential Provider](cloud-backends.md#credential-provider).

```mermaid
stateDiagram-v2
    [*] --> Valid: first mint
    Valid --> Refreshing: nearing expiry (proactive, single-flight)
    Refreshing --> Valid: mint succeeds
    Refreshing --> Valid: mint fails, token still valid (backoff + breaker, alarm)
    Valid --> Expired: TTL elapsed before a successful mint
    Expired --> Valid: mint succeeds
    Expired --> PublishFails: expired and mint still failing
    PublishFails --> Valid: mint recovers
    note right of PublishFails
        Under passthrough the mirror publish is the only
        dependent op: the job is left un-acked and retries /
        dead-letters, never touching the client serve path.
        (Under service / delegated-cache a read credential
        sits on the serve path — see access-model.md.)
    end note
```

## 7. Credential authority across the three registries

The diagram shows the default **`passthrough`** strategy. The invariant that holds
under **every** strategy is narrower: the client's credential is **never** sent to
the public upstream. Whether it reaches the private upstream is strategy-specific —
it does under `passthrough`; under `service` / `delegated-cache` Écluse reads with
its own credential instead. See [Access & Credential Model](access-model.md) and
[Registry Model → Credential flow and authority](registry-model.md#credential-flow-and-authority).

```mermaid
flowchart LR
    Client["Client (dev / CI)"]
    subgraph E["Écluse"]
        SRV["Server — reads"]
        WK["Worker — writes"]
    end
    Priv["Private upstream<br/>e.g. CodeArtifact"]
    Pub["Public upstream"]
    Mirror["Mirror target"]

    Client -->|"client credential"| SRV
    SRV -->|"forwards the client credential"| Priv
    SRV -->|"anonymous — client token stripped"| Pub
    WK -->|"Écluse's own token (CredentialProvider)"| Mirror
```
