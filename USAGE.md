# Using Écluse: the operator manual

This is the operator manual for deploying and running Écluse: how to configure it, connect
clients, and fence its network egress. It's the companion to the internal
[architecture documents](docs/architecture.md), which carry the _why_.

> **Status: pre-launch.** Écluse is under active development. This manual is the configuration
> and operational contract: the env vars, the config schema, the client setup, and the security
> responsibilities. Features still landing are marked **(planned)**.

## Contents

- [What Écluse does](#what-écluse-does)
- [Deployment model](#deployment-model)
- [The two-variable start (serve-only gate)](#the-two-variable-start-serve-only-gate)
- [The Golden Path](#the-golden-path)
- [Deviating from the Golden Path](#deviating-from-the-golden-path)
- [Configuration](#configuration)
  - [Environment variables](#environment-variables)
  - [The configuration document](#the-configuration-document)
  - [Secrets](#secrets)
- [Connecting your clients](#connecting-your-clients)
- [Securing network egress (required)](#securing-network-egress-required)
- [Locking down CI egress (recommended)](#locking-down-ci-egress-recommended)
- [Rule policy](#rule-policy)
- [Operating Écluse](#operating-écluse)
- [Planned controls](#planned-controls)
- [Appendix: runtime-sizing arithmetic](#appendix-runtime-sizing-arithmetic)
- [Learn more](#learn-more)

## What Écluse does

Écluse sits between your build (developer machine or CI) and the upstream registry and applies a
deny-by-default policy before any package reaches a build. It reads through a private upstream
first, falls back to the gated public registry, and mirrors approved packages asynchronously. It's
a policy gate, not a registry, and hosts nothing itself. npm is the first supported ecosystem; the
engine is ecosystem-agnostic, with PyPI on the roadmap. The design is in
[`docs/architecture.md`](docs/architecture.md).

## Deployment model

Écluse ships as one reproducible container image, a multicall executable selected by the container
command:

- **`ecluse proxy`** (default): the HTTP proxy on `ECLUSE_SERVER__PORT` (default `8080`) plus the
  mirror worker.
- **`ecluse pilot`**: the OSV advisory ingestion pipeline.
- **`ecluse dredger`**: the registry cleanup (reaper) worker.
- **`ecluse check-config`**: validates the shared configuration exactly as a boot would and prints
  the whole resolved posture without starting anything (exit `0` valid, `2` refused). Run it in CI
  or before a rollout.

All roles share one config file and rule set. The proxy scales horizontally behind a load balancer,
but **Pilot and Dredger must run as singletons**: multiple instances race, duplicate API calls, and
overlap registry deletions.

`ecluse pilot compile --out DIR` runs one OSV compilation and exits: it fetches an ecosystem's
advisory export (`--ecosystem`, default `npm`; `--source URL` overrides the configured
`advisories.osvExportBaseUrl`) and writes `<ecosystem>-osv-schema<N>.db` (e.g. `npm-osv-schema3.db`)
into `DIR`, exiting non-zero on failure. `--upload` also publishes the artifact to the advisory
bucket, a full sync cycle in one invocation, and aborts immediately without a configured bucket. A
systemically corrupt or truncated export aborts the compile without publishing, so a running proxy
keeps its last-good database rather than adopt one that silently omits advisories.

Point your package manager at the proxy as a registry (see
[Connecting your clients](#connecting-your-clients)). Before running a published image, verify its
provenance and SBOM attestations; the recipe is in the [README](README.md#verifying-the-image).

## The two-variable start (serve-only gate)

The fastest way to put the gate in front of real installs is a **serve-only** deployment: no
mirror, no queue, no cloud account, just the gated public leg.

```bash
ECLUSE_MOUNTS__NPM__ENABLED=true \
ECLUSE_SERVER__PUBLIC_URL=http://127.0.0.1:8080 \
ecluse proxy
```

Every rule, advisory gate, integrity floor, and egress control applies exactly as on a mirrored
deployment; the only thing missing is the mirror write. The trade: the public leg is permanent,
availability stays coupled to the public registry, and no mirrored copy survives an upstream yank.
Use it to evaluate the gate, then graduate to the [Golden Path](#the-golden-path) by declaring a
`mirrorTarget`.

## The Golden Path

This is the recommended, most resilient way to run Écluse, and the posture the
[threat model](https://ecluse-proxy.com/threat-model.html) treats as canonical. Aim for it unless
you have a specific reason to diverge.

1. **Run three registries, not one.** Give the three internal roles distinct backends: a
   first-party store (publication target), a public-derived mirror store (mirror target), and a
   pull-through read endpoint that unions both (`ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM`). Separating
   provenance keeps the mirror auditable. **The one hard rule:** the aggregating endpoint must union
   **trusted** stores only, never a direct public upstream, or raw ungated packages reach clients as
   trusted and bypass the gate. See
   [registry-level composition](docs/architecture/registry-model.md#registry-level-composition-the-recommended-topology).
2. **Let callers use their own identity.** The default forwards each caller's credential to the
   private upstream and publication target, so access matches your registry IAM exactly and Écluse
   holds no standing read credential. Nothing to set. See
   [access model](docs/architecture/access-model.md).
3. **Mint the mirror-write token from the container role.** Point
   `ECLUSE_MOUNTS__NPM__MIRROR_TARGET` at a CodeArtifact endpoint so the worker mints a short-lived
   token under the task/instance role instead of carrying a static secret. Scope that role
   **write-only** to the mirror store and keep `ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_TOKEN_DURATION`
   short: it's Écluse's only standing credential and it writes the trusted store. Scope the mirror
   queue the same way, granting only the serve role `SendMessage` and only the worker
   `ReceiveMessage`/delete, because anyone who can write the queue can force a write to the trusted
   store.
4. **Let the edge own access; leave `ECLUSE_SERVER__AUTH_TOKEN` off.** Écluse is not your access
   boundary. Front it with a gateway, mesh, or IAP, and restrict reachability **both** north-south
   and east-west (pod-to-pod): an ingress-only allow-list that leaves the pod reachable inside the
   cluster is a common vulnerability. See [Connecting your clients](#connecting-your-clients).
5. **Fence egress, keep metadata reachable.** Default-deny outbound, allowing only your upstreams,
   the mirror target, the advisory bucket when `ECLUSE_ADVISORIES__BUCKET` is set (the proxy needs
   `s3:GetObject` to sync it), and the metadata endpoint; require IMDSv2 with hop limit 1. Don't
   block the metadata endpoint: Écluse needs it to mint credentials. See
   [Securing network egress](#securing-network-egress-required).
6. **Make the proxy unbypassable.** Deny CI runners (and, where practical, workstations) outbound
   access to the public registries. See
   [Locking down CI egress](#locking-down-ci-egress-recommended).
7. **Verify what you run.** Pin the image by digest and verify its provenance and SBOM attestations
   (see [Verifying the image](README.md#verifying-the-image)).

The reasoning behind each choice, and the residual risks it accepts, is in the
[threat model](https://ecluse-proxy.com/threat-model.html) and
[Security invariants](docs/architecture/security.md#trust-assumptions--credential-posture).

## Deviating from the Golden Path

Écluse still runs if you diverge, but each deviation trades away a protection, and one is
**silent** (Écluse can't detect it, so nothing warns you):

- **Collapsing the registries onto one store** (declaring `ECLUSE_MOUNTS__NPM__MIRROR_TARGET` equal
  to the private upstream, or `ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET` onto either). The perimeter
  holds, but first-party and public-derived packages share one store, so you lose provenance
  separation and clean post-incident scoping. The proxy logs a boot warning for each pair of a
  mount's endpoints that resolve to the same registry, and **Écluse Dredger refuses to boot** if
  `MIRROR_TARGET` equals `PUBLICATION_TARGET`, since automated pruning on a shared store risks
  first-party data loss.
- **Pointing the private upstream at a registry that itself draws from public** (say a CodeArtifact
  repo with the stock `npm-store` upstream to npmjs). This is the **dangerous one**, and Écluse
  **can't detect it**: raw ungated packages reach clients through the trusted read path, behind the
  gate instead of through it, nullifying the rules, integrity floor, and freshness quarantine.
  Aggregate **trusted stores only** into the private upstream, and let the gated mirror be the only
  way public content enters.

The [threat model](https://ecluse-proxy.com/threat-model.html) records both. The other deviations
self-announce: an open edge leans on your network boundary, a static publish credential fails closed
at boot without that edge, and a static mirror-write secret forgoes the minted token.

## Configuration

Configuration has two layers: **environment variables** for process and secret values, and an
optional **config document** (YAML) for the two things too expressive for flat env vars, the rule
policy and the mount map. A single-mount npm deployment on the default policy needs no document.

A mount serves only when you declare it: any `ECLUSE_MOUNTS__<ECOSYSTEM>__*` variable (or any key
under `mounts.<ecosystem>` in the document) activates that mount, and a mount you never mention
stays off. Whether an active mount **mirrors** is derived from its endpoints: declaring
`mirrorTarget` makes it mirrored (its private upstream is then required, so the mirror reads back),
and omitting it makes the mount serve-only. Each boot logs one posture line per mount and warns on
any pair of a mount's endpoints that resolve to the same registry. The design rationale is in
[Configuration and authentication](docs/architecture/configuration.md#configuration).

A value resolves as defaults < config document < environment variable, so the environment wins, and
the boot log carries one `config:` line per resolved key naming the layer that supplied it (secrets
redacted), the same dump `ecluse check-config` prints.

### Environment variables

> **One spelling rule.** Environment variables are the mechanical transliteration of the document
> schema: `__` descends into an object and `_` joins a camelCase word, so `ECLUSE_CACHE__MAX_BYTES`
> spells `cache.maxBytes` and `ECLUSE_MOUNTS__NPM__MIRROR_TARGET` spells `mounts.npm.mirrorTarget`.

The secret-typed variables also accept the container-secret file pattern: set the `_FILE` form
(`ECLUSE_SERVER__AUTH_TOKEN_FILE`, `ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN_FILE`,
`ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN_FILE`) to a file path and the file's contents (trailing
newlines stripped) become the value, so the token never enters the environment. Setting both
a variable and its `_FILE` form, or naming an unreadable file, is a fail-loud boot error.

#### Process

| Variable | Required | Default | Description |
| :--- | :--- | :--- | :--- |
| `ECLUSE_CONFIG` | No | `/etc/ecluse/config.yaml` | Path of the [config document](#the-configuration-document); a process-level setting, not a document key. With it set, a missing file there is a **boot error**; at the default path an absent document is fine. |

#### Server (`server.*`)

| Variable | Required | Default | Description |
| :--- | :--- | :--- | :--- |
| `ECLUSE_SERVER__PORT` | No | `8080` | TCP port, in `0..65535` (`0` binds an OS-assigned ephemeral port). Out of range is rejected at load. |
| `ECLUSE_SERVER__PUBLIC_URL` | When any mount is active |  | The proxy's own externally-reachable base URL (e.g. `https://registry.example.com`); each served `dist.tarball` is rewritten to an **absolute** URL under it. Must be `http(s)` with a dialable authority (`http` stays legal for loopback). Required the moment a mount is active, else the boot refuses with `PublicUrlRequired`. |
| `ECLUSE_SERVER__AUTH_TOKEN` | No |  | If set, clients must present this token (`Bearer` / `_authToken`). Omit for network-secured deployments. |
| `ECLUSE_SERVER__HELP_MESSAGE` | No |  | String appended to every denial message (e.g. a support channel). |
| `ECLUSE_SERVER__SHUTDOWN_DRAIN_TIMEOUT` | No | `30` | Seconds the graceful shutdown waits for in-flight requests and artifact streams before exit. Positive integer. |

#### Mounts (`mounts.npm.*`)

| Variable | Required | Default | Description |
| :--- | :--- | :--- | :--- |
| `ECLUSE_MOUNTS__NPM__ENABLED` | No |  | The mount's on/off switch. Any declared `ECLUSE_MOUNTS__NPM__*` key activates the mount, so `ENABLED=true` exists for the mount that needs no other key (the serve-only pure public gate: `ENABLED=true` + `ECLUSE_SERVER__PUBLIC_URL`); `ENABLED=false` switches off a mount whose other keys remain. |
| `ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM` | Depends | Mirrored mounts | URL of the private upstream (the read authority under the default passthrough strategy). Required on a **mirrored** mount, so the mirror reads back; optional on a serve-only mount, where if present it still merges with the gated public set. |
| `ECLUSE_MOUNTS__NPM__PUBLIC_UPSTREAM` | No | `https://registry.npmjs.org` | URL of the public upstream, queried anonymously and gated by the rules. |
| `ECLUSE_MOUNTS__NPM__MIRROR_TARGET` | No |  | Registry approved packages mirror to. **Declaring it makes the mount mirrored**; absent, the mount is serve-only (never writes). May equal `PRIVATE_UPSTREAM`. **The write credential derives from this URL:** a CodeArtifact endpoint (`{domain}-{owner}.d.codeartifact.{region}.amazonaws.com`) mints a short-lived token scoped to that domain; any other host uses the static `MIRROR_TARGET_TOKEN`. |
| `ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN` | Depends |  | Static write token for a **non-CodeArtifact** mirror target. Required when the target is not CodeArtifact (absent ⇒ boot error); forbidden when it is one (the token is minted), and forbidden on a serve-only mount. |
| `ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_TOKEN_DURATION` | No |  | Lifetime in seconds of the minted CodeArtifact write token (CodeArtifact mirror target only). Accepts `900` to `43200` (15 minutes to 12 hours); a value outside that range is rejected at load. |
| `ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET` | No |  | Where client `npm publish` (first-party packages) is written. **Opt-in: unset ⇒ `PUT /{pkg}` is `405`.** May equal the private upstream. Protect this surface; see the warning below. |
| `ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN` | No |  | Static fallback for the publication target, forwarded only when a publishing client sends none. Default is **passthrough** (the publisher's own token). ⚠️ A static token with an open edge lets any unauthenticated client publish under it; see the warning below. |
| `ECLUSE_MOUNTS__NPM__PUBLISH_ALLOW` | Conditionally | If `PUBLICATION_TARGET` is set | Comma-separated allow-list of package names a client may publish, in the ecosystem's native form (npm scopes such as `@acme,@beta`); the anti-shadowing guard, refusing a publish outside the list before any upstream write. It limits names, not callers, and is not authentication. An empty list with a publication target set is a fail-loud boot error, as is a malformed entry (an empty segment from a stray comma, a wrong separator, or a character a scope cannot contain such as `/`, an interior `@`, or whitespace): a typo fails the load rather than seed a dead allow-list that refuses every publish. |
| `ECLUSE_MOUNTS__NPM__MIN_TRUSTED_INTEGRITY` | No | global `ECLUSE_INTEGRITY__MIN_TRUSTED` | Per-mount refinement of the trusted-integrity floor, so one legacy private registry's loosening (e.g. `sha1`) doesn't leak onto other mounts. |
| `ECLUSE_MOUNTS__NPM__DIVERGENCE_POLICY` | No | global `ECLUSE_INTEGRITY__DIVERGENCE_POLICY` | Per-mount refinement of the cross-upstream divergence policy (`warn`/`fail-closed`). |

#### Mirror queue (`queue.*`) and the ambient AWS environment

| Variable | Required | Default | Description |
| :--- | :--- | :--- | :--- |
| `ECLUSE_QUEUE__URL` | No | In-memory queue | Mirror-queue destination; **its shape selects the backend**. A real SQS URL (`https://sqs.{region}.amazonaws.com/{account}/{queue}`) selects the durable SQS backend, region from the host, validated in full (https, single-label region, 12-digit account, one queue segment, no port/query/fragment). A Pub/Sub resource (`projects/<p>/topics/<t>`) names the GCP backend, recognised but not yet built (fail-loud). Any other shape fails boot. **Unset with a mirroring mount ⇒ the bounded in-process queue**: non-durable, best-effort (single-node, trial, or air-gapped), warned loudly at boot. Never consulted serve-only. |
| `ECLUSE_QUEUE__MEMORY_MAX_DEPTH` | No | Memory budget | In-memory queue only. Depth cap, computed by the memory plan (`50000` with no ceiling datapoint) unless set. An enqueue past the cap is dropped (drop-newest) and re-mirrors on next demand. Positive integer. |
| `AWS_REGION` | Depends | AWS backends only | Region for SQS **only under an `AWS_ENDPOINT_URL_SQS` override** (a real SQS URL carries its own region), and for the S3 advisory client. **Never consulted for CodeArtifact** (the mint's region is parsed from the mirror-target host). Ambient AWS-SDK environment, read from the process env, **not** a document key: `awsRegion:` in the document is rejected as unknown. |
| `AWS_ENDPOINT_URL_SQS` | No |  | SQS endpoint override (the AWS-SDK-standard service-specific variable). Setting it **forces the SQS interpretation** of `ECLUSE_QUEUE__URL` regardless of shape, scoped by `AWS_REGION`, signed with `AWS_ACCESS_KEY_ID`/`AWS_SECRET_ACCESS_KEY`. Ambient, like `AWS_REGION`. |
| `AWS_ENDPOINT_URL` | No |  | Endpoint override for the S3 advisory-database client (the proxy's sync and Pilot's export). Deliberately **not** consulted for SQS, so an S3-only override can't silently redirect the queue. Ambient, like `AWS_REGION`. |

#### Limits (`limits.*`)

| Variable | Required | Default | Description |
| :--- | :--- | :--- | :--- |
| `ECLUSE_LIMITS__MAX_RESPONSE_BYTES` | No | Memory budget | Largest upstream **metadata** body buffered before the fetch aborts fail-closed. Computed by the memory plan, floored at 12 MiB so real packuments fit, unless set. Positive integer. |
| `ECLUSE_LIMITS__MAX_REQUEST_BYTES` | No | Memory budget | Largest client request body (a publish) buffered before refusal. Computed by the memory plan (25 MiB with no ceiling datapoint) unless set. Positive integer. |
| `ECLUSE_LIMITS__MAX_ARTIFACT_BYTES` | No | Memory budget | Largest **mirror-worker artifact** (tarball) buffered before the back-fill fetch aborts fail-closed. Computed by the memory plan's mirror-artifact tenant (512 MiB with no ceiling datapoint) so the transient publish envelope stays within the heap ceiling. An over-cap artifact is dropped, not retried. Raising it may be refused if the pod cannot hold the envelope. Positive integer. |
| `ECLUSE_LIMITS__MAX_VERSION_COUNT` | No | `100000` | Largest version count a packument may carry before refusal. Bounds per-version rule evaluation. Pinned policy. Positive integer. |
| `ECLUSE_LIMITS__MAX_NESTING_DEPTH` | No | `64` | Deepest JSON nesting a decoded upstream document may reach before refusal. Bounds CPU/stack. Pinned policy. Positive integer. |

#### Cache (`cache.*`)

| Variable | Required | Default | Description |
| :--- | :--- | :--- | :--- |
| `ECLUSE_CACHE__TTL` | No | `60` | Seconds metadata is kept in the shared packument cache. Non-negative integer; a fractional value is refused at load rather than silently truncated. |
| `ECLUSE_CACHE__MAX_ENTRIES` | No | Memory budget | Maximum items the metadata cache holds; computed by the memory plan (`1024` with no ceiling datapoint) unless set. |
| `ECLUSE_CACHE__MAX_BYTES` | No | Memory budget | The metadata cache's **one aggregate** resident-byte budget, split across its three stores (full-packument 60%, single-version 15%, assembled the remainder) so they sum exactly to it. Computed by the memory plan (256 MiB with no ceiling datapoint) unless set. On a pod too small for the tenants' floors the plan sheds this aggregate first, to zero if needed (a loud warning; the proxy then serves uncached). |

#### Integrity (`integrity.*`)

| Variable | Required | Default | Description |
| :--- | :--- | :--- | :--- |
| `ECLUSE_INTEGRITY__MIN_PUBLIC` | No | `sha256` | Minimum integrity algorithm a **public** (untrusted) version's digest must meet: `sha256`, `sha384`, `sha512`, or `blake2b`. A weaker or absent digest is refused with `403`. Hard-floored at SHA-256: `sha1`/`md5`/an unknown name is rejected at startup. |
| `ECLUSE_INTEGRITY__MIN_TRUSTED` | No | `sha256` | Minimum integrity algorithm a **trusted** (private) version's digest must meet. Defaults to `sha256`, so a SHA-1-only or hashless private version is dropped like a public one, but unlike the public floor is **loosenable below SHA-256** (`sha1`/`md5`) for a legacy private mirror. An unknown name is rejected at load. |
| `ECLUSE_INTEGRITY__DIVERGENCE_POLICY` | No | `warn` | What to do when a shared version's private and public copies contradict on a shared integrity algorithm. Either way the trusted copy wins the bytes, a `WARNING` logs, and `ecluse.registry.merge.divergence` increments. `warn` serves the trusted copy and relies on the alarm; `fail-closed` additionally withholds the contested version (dropping any `dist-tag`, including `latest`, that pointed at it). An unknown value is rejected at load. See [configuration.md](docs/architecture/configuration.md#cross-upstream-divergence-policy). |

#### Egress (`egress.*`)

| Variable | Required | Default | Description |
| :--- | :--- | :--- | :--- |
| `ECLUSE_EGRESS__ADDITIONAL_BLOCKED_RANGES` | No |  | Comma-separated CIDR ranges (e.g. `10.99.0.0/16,fd12::/8`) added to the fixed internal-address block, applied identically across every mount. Extends the block only, never narrows it; a malformed entry **fails closed at boot**. See [Securing network egress](#securing-network-egress-required). |

#### Advisories (`advisories.*`)

| Variable | Required | Default | Description |
| :--- | :--- | :--- | :--- |
| `ECLUSE_ADVISORIES__COMPILE_INTERVAL` | Depends | Pilot only, `3600` | How often the Pilot singleton refreshes the advisory database from upstream. Positive integer. |
| `ECLUSE_ADVISORIES__BUCKET` | No |  | The object-store bucket carrying the compiled advisory artifacts. Pilot uploads to it; the proxy polls it and shadow-swaps fresh artifacts into the rules engine. Unset, the proxy runs no advisory sync and `AllowIfRemediatesCve` abstains. |
| `ECLUSE_ADVISORIES__POLL_INTERVAL` | No | `60` | Proxy only: how often each ecosystem's sync task polls the bucket (a cheap conditional `HEAD`). Deliberately more frequent than Pilot's `COMPILE_INTERVAL`. Positive integer. |
| `ECLUSE_ADVISORIES__MAX_DATABASE_BYTES` | No | `536870912` | Proxy only: refuse to download an advisory database larger than this (default 512 MiB). The declared length fails fast and the streaming download enforces the cap. |
| `ECLUSE_ADVISORIES__DATA_DIR` | No | `data/osv` | Directory for the OSV advisory databases: where Pilot compiles them and where the proxy lands its synced per-ecosystem artifacts. |
| `ECLUSE_ADVISORIES__OSV_EXPORT_BASE_URL` | No | `https://osv-vulnerabilities.storage.googleapis.com` | Base URL of the per-ecosystem OSV advisory exports Pilot compiles from (`<base>/<ecosystem>/all.zip`). This is the host Pilot dials for raw advisories: allowlist it, not `osv.dev`. Override it if the upstream moves or you mirror the exports. |

#### Runtime (`runtime.*`)

| Variable | Required | Default | Description |
| :--- | :--- | :--- | :--- |
| `ECLUSE_RUNTIME__CORES` | No | derived | Cores (GHC capabilities) the process claims. Unset ⇒ derived from the cgroup CPU quota (floored, at least 1, clamped to the visible processors); with no cgroup limit, the runtime's own detection stands. Give the container **whole cores** (see the [appendix](#appendix-runtime-sizing-arithmetic)). Positive integer. |
| `ECLUSE_RUNTIME__MAX_HEAP_BYTES` | No | derived | Heap ceiling in bytes, enforced by the GHC runtime (a breach is a clean heap-overflow error, not a kernel OOM kill). Unset ⇒ derived from the cgroup memory limit less the nursery budget and 10% slack; with no cgroup limit, unbounded unless your `GHCRTS -M` says otherwise. Enforcing a ceiling re-executes the binary once, in place (same PID). Positive integer. |
| `ECLUSE_RUNTIME__SERVE_MAX_IN_FLIGHT` | No | computed | Process-wide cap on concurrent metadata materialisation. Unset, computed at boot as `max(8, 10 x cores)`. Over the cap, a request waits up to 1 second for a slot (a bounded waiting room, no queue-jumping); only a request that finds the room full or waits out that budget gets `503` with `Retry-After: 1`. Trusted private tarball hits, health probes, and local routes stream outside the cap. Positive integer. A `503` **with** `Retry-After: 1` is intentional backpressure: exclude it from alerts (a real upstream failure returns `503` without that header). |
| `ECLUSE_RUNTIME__PUBLIC_CONNECTIONS_PER_HOST` | No | computed | Maximum pooled (kept-for-reuse) connections per public upstream host. Unset, computed as `clamp(32, 1024, nofile / 8)`. Connections beyond the pool still open, but re-handshake TLS each time. Positive integer. |
| `ECLUSE_RUNTIME__PRIVATE_CONNECTIONS_PER_HOST` | No | computed | Maximum pooled connections to the private upstream host. Unset, computed as a quarter of the soft `RLIMIT_NOFILE`, clamped to `64..4096`. Sized for the trusted tarball hit, which streams outside `SERVE_MAX_IN_FLIGHT`. Positive integer. |

#### Observability (`observability.*`)

| Variable | Required | Default | Description |
| :--- | :--- | :--- | :--- |
| `ECLUSE_OBSERVABILITY__LOG_FORMAT` | No | `json` | Log shape: `json` (one JSON object per line, for log collectors) or `console` (human-readable). |
| `ECLUSE_OBSERVABILITY__TELEMETRY` | No | `off` | OpenTelemetry master switch (`on`/`off`). With it `off`, no telemetry is emitted. See [Operating Écluse](#operating-écluse) for the export configuration. |

Configuration is validated in full at startup and the process refuses to start on any problem (an
unknown rule type, a bad URL, an unresolved policy reference), so a misconfiguration is a loud,
immediate failure rather than a quietly mis-enforced policy. The validation model is in
[Validation: fail fast, reject the unknown](docs/architecture/configuration.md#validation-fail-fast-reject-the-unknown).

> ⚠️ **The first-party publish surface authorises _names_, not _callers_.** With publishing enabled
> (`ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET`), `ECLUSE_MOUNTS__NPM__PUBLISH_ALLOW` limits which
> package names may be published; it is not authentication. So a static
> `ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN` (used only when a publisher forwards none) is
> **fail-closed**: set it without `ECLUSE_SERVER__AUTH_TOKEN` and Écluse refuses to start
> (`PublishStaticCredentialNeedsEdge`), so "static publish credential + open edge" is
> unrepresentable. `ECLUSE_SERVER__AUTH_TOKEN` is the edge Écluse can verify itself; an external
> layer is good defence-in-depth but does not satisfy this. Pure passthrough (the default) needs
> none of it. See
> [Access model → Publishing](docs/architecture/access-model.md#publishing-the-publication-target-passthrough-write).

### The configuration document

A YAML file mounted at `/etc/ecluse/config.yaml` (relocate it with `ECLUSE_CONFIG`; a non-existent
explicit path is a boot error). It carries the **rule policy** (see [Rule policy](#rule-policy))
and, for multi-mount deployments, the **mount map**. Single-mount deployments desugar from the env
vars above and need no document. Schema and examples:
[Configuration and authentication](docs/architecture/configuration.md#configuration). Deployments
derive their initial policy from the [default baseline configuration](config/default.yaml).

### Secrets

Secrets never live in the config document. Client and registry tokens are always env vars, and
cloud-managed registries (CodeArtifact, Artifact Registry) derive short-lived tokens from ambient
cloud credentials. A **mirrored** mount holds a mirror-target **write** credential; a serve-only
mount never writes and holds none. Reads use the default passthrough strategy: the caller's own
credential is forwarded to the private upstream and stripped before the public one. The credential
model, including the planned per-mount strategies, is in
[access model](docs/architecture/access-model.md) and
[Outbound registry credentials](docs/architecture/configuration.md#outbound-registry-credentials).

## Connecting your clients

Point your package manager at the proxy as its registry. With `ECLUSE_SERVER__AUTH_TOKEN` set,
supply it the standard npm way:

```ini
# .npmrc
registry=https://ecluse.example.internal/
//ecluse.example.internal/:_authToken=${ECLUSE_TOKEN}
```

Edge authentication to the proxy has two shipped modes:

1. **Open**: `ECLUSE_SERVER__AUTH_TOKEN` unset; access control is delegated to the network layer
   (VPC, service mesh). Appropriate only on a closed network.
2. **Static token**: `ECLUSE_SERVER__AUTH_TOKEN` set; clients send it as
   `Authorization: Bearer <token>` or `.npmrc` `_authToken`.

A third mode, a **trusted edge identity** honoured over a verifiable binding to a fronting
gateway/IAP/mesh, is planned; see
[access model → edge authentication](docs/architecture/access-model.md#edge-authentication).

## Securing network egress (required)

Écluse fetches from the registries you point it at, and some URLs it follows (a version's
`dist.tarball`) come from upstream responses. Apply least-privilege egress in two layers. Écluse
provides the first in the application, with an **origin-aware trust model**:

- **Untrusted origins** (the public upstream and every `dist.tarball`) are gated by a host+port
  **allowlist**, fetched **HTTPS-only** with TLS certificate validation, and bounded by
  response-size limits. An upstream URL with no explicit port authorises port 443 alone; a
  nonstandard port must be written (`https://repo.internal:8443`) and authorises exactly that
  `host:port`. A non-HTTPS upstream, or a port outside `1..65535`, fails closed at boot. Certificate
  validation is the guarantor against the resolve-to-internal and DNS-rebinding SSRF class: an
  address a name is steered to can't present a CA-trusted certificate for the host. As
  defence-in-depth, a **literal internal-range block** (loopback, link-local including the
  `169.254.169.254` metadata endpoint, RFC1918, CGNAT, IPv6 ULA) refuses a `dist.tarball` whose host
  is an internal-address literal; extend it with `ECLUSE_EGRESS__ADDITIONAL_BLOCKED_RANGES`.
- **The trusted private origin** (`ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM`) is deliberately **not**
  subject to the internal-range block: a private registry legitimately lives on your internal
  network.

**The `dist.tarball` host gate.** `dist.tarball` is upstream-chosen, so Écluse fetches a tarball
only from the same allowlisted host that served the packument, host **and port** compared as a pair.
A plaintext `dist.tarball` is upgraded to https on its own host; on any other host it is dropped
(the version is skipped). There is no widening knob.

Provide the second layer at the platform, default-denying egress and allowing only your registries,
mirror target, and the metadata endpoint:

- **AWS**: security-group egress rules / network ACLs to the upstream and mirror CIDRs; reach
  CodeArtifact and S3 over VPC endpoints; **require IMDSv2 with hop limit 1**
  (`httpPutResponseHopLimit: 1`).
- **GCP**: VPC firewall egress rules and, where applicable, VPC Service Controls.
- **Kubernetes**: a default-deny `NetworkPolicy` with an explicit egress allowlist; allow your
  private upstream's internal range.
- **Service mesh (Istio/Linkerd)**: sidecar outbound policy `REGISTRY_ONLY`, each upstream a
  `ServiceEntry`, constrained by a `Sidecar` egress listener and an egress `AuthorizationPolicy`.

**Don't block the metadata endpoint or internal ranges for the proxy itself.** Écluse's own metadata
access goes through the AWS SDK to mint its instance-role credentials; denying it breaks those
credentials. IMDSv2 hop limit 1 keeps the minting working while stopping a neighbour or forwarded
request from reaching metadata through extra hops. Grant the proxy only the cloud permissions it
needs: the mirror-write credential and the advisory-bucket read (`s3:GetObject`) when
`ECLUSE_ADVISORIES__BUCKET` is set, nothing more. The invariants and their rationale are in
[Network egress is a shared responsibility](docs/architecture/security.md#network-egress-is-a-shared-responsibility).

### Securing Écluse Pilot and Dredger

Both auxiliary services need distinct, tightly scoped egress, and **both must run as singletons**:

- **Écluse Pilot**: no public ingress. Egress to the OSV export host in
  `ECLUSE_ADVISORIES__OSV_EXPORT_BASE_URL` (default `osv-vulnerabilities.storage.googleapis.com`),
  the metadata endpoint, and your object store (`s3:PutObject` to upload the advisory database). The
  object is named `<ecosystem>-osv-schema<N>.db` (e.g. `npm-osv-schema3.db`); the key is stable per
  ecosystem, so bucket policies and the proxy's ETag polling can target it. On an export-host
  `5xx`/`408`/`429`, Pilot retries with capped, jittered backoff, so a transient outage can't get
  your NAT address rate-limited.
- **Écluse Dredger**: no public ingress. Egress only to your private mirror for delete requests and
  to the metadata endpoint for credentials. It holds a standing high-privilege delete capability, so
  isolate it from all untrusted networks.

To avoid an idling Pilot pod, schedule the one-shot instead: run
`ecluse pilot compile --out /tmp/osv --upload` as a Kubernetes `CronJob` with
`concurrencyPolicy: Forbid` (which preserves the singleton by never overlapping a run). Give the pod
`s3:PutObject` via IRSA or workload identity rather than mounted keys, and align its schedule with
the proxy's polling (the proxy polls more often than Pilot publishes).

## Locking down CI egress (recommended)

The controls above secure Écluse's own egress. This one secures your consumers'. If you control CI,
**deny runners outbound access to the public registries** (`registry.npmjs.org` and the equivalents
for other ecosystems) and let them reach only Écluse and your internal services. Now a misconfigured
job (a stray `--registry` flag, a committed `.npmrc`, a tool that ignores your settings) can't
reach the public registry, so it fails instead of pulling an unvetted package. This makes the policy
_unbypassable_ rather than merely _default_: a per-project package-manager setup can override what
you ship to a machine, but none can route around a network that only reaches Écluse. See
[MOTIVATION → The bar](MOTIVATION.md#the-bar-a-chokepoint-you-cant-step-around). The same idea
extends to developer workstations, a softer control than CI.

## Rule policy

Écluse evaluates a named map of rules over a built-in **deny-by-default** policy: a package is
admitted only if a rule allows it, and every deny type outranks every allow type by default. The
policy lives in the config document's `rules` object; it also has an environment spelling
(`ECLUSE_RULES` carrying the JSON object), which suits a one-rule tweak while the document stays the
reviewable home for a real policy. The shipped default is small and biased toward resilience:

- **`min-age`** (`AllowIfOlderThan`): admit public versions older than a quarantine window (7 days
  by default), the core defence against race-to-publish typosquatting and dependency confusion.
- **`remediation-fast-track`** (`AllowIfRemediatesCve`): admit a release a synced advisory names as
  its exact fixed version ahead of the quarantine, provided no other advisory still affects it. It
  abstains until an advisory database has been synced (set `ECLUSE_ADVISORIES__BUCKET` and run
  Pilot), so without one only the quarantine governs.

Every other built-in rule is off by default and opts in by name:

- **`AllowByIdentity`**: admit a specific package or `package@version` past the quarantine, at the
  top of the allow band but still below every deny.
- **`DenyByIdentity`** (the `revoke` shape): a hard-deny for a specific package or `package@version`.
- **`DenyInstallTimeExecution`**: deny install-time code execution (off because many legitimate
  packages ship install scripts).
- **`DenyIfCve`**: block a version a synced advisory records as affected at or above a CVSS
  `minSeverity` (0-10). The npm malware feed carries no score and counts as above every threshold, so
  enabling it also blocks known-malicious packages. It sits just below `AllowByIdentity`, so an
  identity pin overrides it; its `onUnavailable` knob (`deny` by default, or `skip`) decides what
  happens when the advisory database can't answer. Read
  [Onboarding DenyIfCve](#onboarding-denyifcve) before enabling.

Override a value, add a rule, or suppress a default by name in the document:

```json
{
  "rules": {
    "min-age": { "ageSeconds": 1209600 },
    "deny-scripts": { "type": "DenyInstallTimeExecution", "precedence": 200 },
    "revoke-bad": { "type": "DenyByIdentity", "identity": "bad-package" },
    "cve-fast-lane": { "type": "AllowIfRemediatesCve" },
    "deny-known-cves": { "type": "DenyIfCve", "minSeverity": 8 },
    "pin-fix": { "type": "AllowByIdentity", "identity": "left-pad@1.3.0" }
  }
}
```

The precedence values, the patch/add/suppress merge model, and the strict validation are in
[Rule policy](docs/architecture/configuration.md#rule-policy) and
[Rules engine](docs/architecture/rules-engine.md#evaluation-model).

Independent of the rules, a **public** version is served only if it carries a digest meeting the
public integrity floor (`ECLUSE_INTEGRITY__MIN_PUBLIC`, default `sha256`; see the table above).
**Gotcha:** a custom or off-spec public upstream serving versions without a floor-meeting digest will
have those versions silently disappear and their tarballs `403`. To serve such a source, point it at
the **private** upstream slot and loosen `ECLUSE_INTEGRITY__MIN_TRUSTED` below `sha256`. The
mechanics are in [Public integrity floor](docs/architecture/configuration.md#public-integrity-floor).

### Onboarding DenyIfCve

`DenyIfCve` can break a cold deployment: on a freshly-stood-up mirror it can deny historical
versions your existing builds still depend on that an advisory has since covered. Enable it *after*
your private mirror is warmed:

1. Leave `DenyIfCve` out of your policy and run Écluse normally, so your CI and developers pull the
   versions you depend on. Each is mirrored into the trusted store, which the rules never re-gate
   once it is there.
2. Once your must-have builds are mirrored, add `DenyIfCve` with a `minSeverity` you are comfortable
   with (8 blocks high and critical CVEs; malware blocks regardless of the threshold).
3. If a specific version you must keep is then denied (a false positive, or a risk you accept), pin
   it with an `AllowByIdentity` rule, which outranks `DenyIfCve`.

Set `onUnavailable: skip` if you would rather the gate fail open (skip itself, logging loudly) than
refuse traffic when the advisory database is briefly unavailable; the default `deny` fails closed.

## Operating Écluse

- **Pre-warming the cache.** A cold `npm install` against an empty cache hits the proxy with dozens
  of heavy requests at once, causing latency spikes or `503` backpressure. Run an `npm install`
  after starting Écluse, before production traffic; once warm, request coalescing absorbs spikes.
- **Health probes.** `GET /livez` reports process liveness (on a mirroring deployment a stalled
  mirror worker fails it; a serve-only deployment's liveness is the listener alone). `GET /readyz`
  reports config loaded and the listener serving; it is deliberately lenient about public-upstream
  reachability, so a transient blip doesn't pull a healthy pod from rotation. With an advisory bucket
  configured, readiness also waits for each ecosystem's first advisory sync (a one-way flip, so it
  never flaps), so mounting an ecosystem whose artifact Pilot never publishes leaves the pod never
  ready. The npm liveness probe `GET /npm/-/ping` answers locally with `200 {}`, and
  `GET /npm/-/v1/search` returns `501` by design (search is a discovery convenience, not an install
  path). Pilot and Dredger export the same `/livez` and `/readyz` on `ECLUSE_SERVER__PORT`.
- **Graceful shutdown and pod drain.** On `SIGTERM`/`SIGINT` Écluse drains in-flight work rather than dropping it. `GET /readyz` flips to `503` (the signal a load balancer or mesh watches to stop routing new traffic here) while `GET /livez` stays `200`, so an orchestrator does not kill a still-draining instance early. Every response then carries `Connection: close`, so a keep-alive pool reconnects to a ready instance, and the process finishes in-flight requests and in-progress artifact streams (a half-delivered tarball runs to completion) before exiting, bounded by `ECLUSE_SERVER__SHUTDOWN_DRAIN_TIMEOUT` (default 30 seconds). **Set the platform's termination grace period above `ECLUSE_SERVER__SHUTDOWN_DRAIN_TIMEOUT`** so the orchestrator does not `SIGKILL` mid-drain: on Kubernetes, set `terminationGracePeriodSeconds` comfortably above it. When Écluse is attached to an interactive terminal, a second `Ctrl+C` (or `Ctrl+D`) forces an immediate halt that bypasses the drain; this is gated on standard input being a TTY, so production has no such bypass.
- **Process exit codes.** The exit status states how a run ended, so an orchestrator can branch
  without parsing logs:

  | Code | Meaning |
  |---|---|
  | `0` | Graceful shutdown: the drain completed and the services returned. |
  | `1` | A service exited abnormally; the detail is the last `ecluse: service exited:` line on standard error. |
  | `2` | The boot aborted: configuration or wiring was rejected, every problem already reported. A restart without changes fails identically. |
  | `3` | The run was cancelled from outside (a kill that bypassed the graceful path). |
  | `130` | The local-development halt (Ctrl-D on an interactive terminal). |

- **Logs.** One JSON object per line by default (`ECLUSE_OBSERVABILITY__LOG_FORMAT=json`), or
  `console` for local development. Bearer tokens render as a redacted placeholder, so token material
  never reaches a log field.
- **Telemetry (opt-in).** Set `ECLUSE_OBSERVABILITY__TELEMETRY=on`, then `DD_*` (`DD_SERVICE`,
  `DD_ENV`, `DD_VERSION`, `DD_AGENT_HOST`) for Datadog or the standard `OTEL_*` for any other
  backend; `DD_*` wins where both are set, and the resolved identity stamps both traces and the `dd`
  object on every log line. `DD_API_KEY`/`DD_SITE` are ignored: Écluse exports only to a node-local
  collector or Agent, at `http://localhost:4318` by default or wherever
  `DD_AGENT_HOST`/`OTEL_EXPORTER_OTLP_ENDPOINT` points (authenticate a remote collector out of band
  with `OTEL_EXPORTER_OTLP_HEADERS`). Export is async and batched, off the request path, so an
  unreachable collector never slows a request.
- **The memory plan.** Every byte-valued bound (cache, response cap, publish aggregate, in-memory
  queue) is a named tenant of the effective heap ceiling, not an independent multiplier, and each is
  boot-logged as a `memory plan:` line. A pod too small for the tenants' floors **degrades
  gracefully instead of refusing**: it sheds (cache first, to zero, serving uncached), each step a
  loud warning, and always boots. Only an explicit override that breaks the plan refuses (exit `2`).
  The model is in
  [Runtime sizing](docs/architecture/configuration.md#runtime-sizing-cores-and-heap-ceiling).
- **Runtime sizing.** Cores and the heap ceiling resolve at boot (config, else cgroup, else the
  runtime's own posture) and every decision is logged with its provenance. The whole-cores guidance
  and per-pod memory arithmetic are in the
  [runtime-sizing appendix](#appendix-runtime-sizing-arithmetic).
- **Revoking a mirrored version (internal yank).** The mirror store deliberately resists upstream
  yanks, so a benign yank doesn't break your installs, but a version later found malicious isn't
  removed automatically (Écluse never re-gates trusted content). Usually this resolves itself: once
  the public registry yanks the bad version, re-mirroring can't reproduce its bytes and you purge the
  stale copy at leisure. When your own scanning is ahead of the public yank, revoke in order:
  **(1)** deny the identity (a `DenyByIdentity` rule), so the serve path stops admitting it and the
  worker stops re-mirroring, then **(2)** purge that version from the mirror. **Order matters:**
  purge alone is a treadmill, since while the version is live upstream the next install re-admits and
  re-mirrors it.

### Datadog on Kubernetes

Deploy via the Datadog Operator: a `DatadogAgent` custom resource (`datadoghq.com/v2alpha1`)
manages the node Agent, traces and metrics go OTLP over TCP to the node-local Agent, and logs
are scraped from stdout with no extra wiring.

1. Enable the Agent's OTLP receiver in the CR. Sampling lives Agent-side (the probabilistic
   sampler needs Agent v7.70+):

   ```yaml
   apiVersion: datadoghq.com/v2alpha1
   kind: DatadogAgent
   spec:
     features:
       otlp:
         receiver:
           protocols:
             http: { enabled: true }   # :4318
     override:
       nodeAgent:
         env:
           - { name: DD_APM_PROBABILISTIC_SAMPLER_ENABLED, value: "true" }
           - { name: DD_APM_PROBABILISTIC_SAMPLER_SAMPLING_PERCENTAGE, value: "20" }
   ```

2. Point Écluse at the node-local Agent with the Downward API, one OTLP endpoint for traces
   and metrics both:

   ```yaml
   env:
     - name: HOST_IP
       valueFrom: { fieldRef: { fieldPath: status.hostIP } }
     - name: OTEL_EXPORTER_OTLP_ENDPOINT
       value: "http://$(HOST_IP):4318"
     - name: OTEL_EXPORTER_OTLP_PROTOCOL
       value: "http/protobuf"
   ```

3. Logs need no extra wiring: Écluse writes JSONL to stdout and the Agent's container log
   collection picks it up.

## Planned controls

Documented ahead of implementation so the configuration surface is known.

- **GCP backends** (**planned**): the Pub/Sub `MirrorQueue` and ADC credential leaf. The AWS
  equivalents (SQS `MirrorQueue`, CodeArtifact credential leaf, mirror worker, composition root) are
  built and wired.
- **Per-mount credential strategies and trusted-edge identity** (**planned**): today reads are
  passthrough (the caller's credential is forwarded) and the edge is open or static-token. The
  target model (a `service` read strategy, a trusted-edge identity mode) is in
  [access model](docs/architecture/access-model.md).

The full deployment runbook ships with the launch.

## Appendix: runtime-sizing arithmetic

**Give Écluse whole cores.** A fractional CPU limit (say 3.5) has no good option: claiming 4
capabilities overruns the CFS quota during stop-the-world GC and freezes the process mid-pause;
flooring to 3 strands the fraction. Écluse floors the derived count, so pair an integer limit with
`requests = limits` (and exclusive cores where offered) to remove throttling structurally. A CPU
**limit** doesn't shrink the processor count the runtime sees, so without `ECLUSE_RUNTIME__CORES` a
2-CPU pod on a 32-core node would claim 32 capabilities and 32 nurseries.

**Memory arithmetic (proxy pod).** The binary ships `-A64m -n4m` (a 64 MiB per-core allocation area
in 4 MiB chunks), trading bounded extra memory for far fewer GCs under load. Budget roughly
`cores x 64 MiB` of nursery, plus the live heap (dominated by the metadata cache), plus up to one
live-heap of copying headroom during a major GC. Worked shapes:

- a 2-CPU / 512 MiB pod runs as-is;
- a 2-CPU / 256 MiB pod also needs `GHCRTS="-A16m"`;
- a 4-CPU pod wants ~750 MiB on defaults, or 512 MiB with `-A32m`.

Taller pods amortise the cache and coalescing better, so prefer 4-CPU-ish shapes. Tune the
allocation area with `GHCRTS`; the boot log prints the effective value. Pilot and Dredger run
different workloads, so tune their allocation area separately, though the core/heap resolution above
applies to every role.

## Learn more

The internal design, for when you need the _why_:

- [Architecture overview](docs/architecture.md)
- [Configuration and authentication](docs/architecture/configuration.md)
- [Security invariants and network egress](docs/architecture/security.md)
- [Threat model](https://ecluse-proxy.com/threat-model.html), the STRIDE register, generated from the OWASP Threat Dragon model ([`threat-modelling/ecluse.json`](threat-modelling/ecluse.json))
- [Rules engine](docs/architecture/rules-engine.md)
- [Multi-ecosystem hosting and URL rewriting](docs/architecture/web-layer.md#web-layer)
- [Release and supply-chain operations](docs/architecture/release-supply-chain.md)
