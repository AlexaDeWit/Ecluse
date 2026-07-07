# Using Écluse: the operator manual

This is the operator manual for deploying and running Écluse: how to configure it, connect
your clients, and fence its network egress so it stays a safe link in your supply chain. It's
the companion to the internal [architecture documents](docs/architecture.md), which explain the
_why_ behind everything here.

> **Status: pre-launch.** Écluse is under active development. This manual is the configuration
> and operational contract: the env vars, the config schema, the client setup, and the security
> responsibilities. Features still landing are marked **(planned)**; treat this as the deployment
> contract, not a claim that every capability below is wired today.

## Contents

- [What Écluse does](#what-écluse-does)
- [Deployment model](#deployment-model)
- [The Golden Path](#the-golden-path)
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
- [Learn more](#learn-more)

## What Écluse does

Écluse sits between your build (developer machine or CI) and the upstream registry, and applies
a deny-by-default policy before any package reaches a build. It reads through a private upstream
first, falls back to the public registry with rules applied, and mirrors approved packages
asynchronously. It's a policy gate, not a registry, and hosts nothing itself. npm is the first
supported ecosystem; the engine is ecosystem-agnostic, with PyPI and RubyGems on the roadmap.
The design is in [`docs/architecture.md`](docs/architecture.md).

## Deployment model

Écluse ships as a single reproducible container image, a multicall executable: `ecluse proxy`
(the HTTP proxy), `ecluse pilot` (the OSV ingestion pipeline), or `ecluse dredger` (the registry
cleanup worker), selected by the container command. All three roles share one config file and
rule set.

`ecluse pilot compile --out DIR` runs one OSV compilation and exits: it fetches an ecosystem's
advisory export (`--ecosystem`, default `npm`; `--source URL` overrides the configured
`osvExportBaseUrl`), filters the flattened advisory rows to that ecosystem, writes `osv.db` into
`DIR`, and exits non-zero on failure, so it's safe to script and schedule. `--upload` also publishes
the artifact to the vulnerability-database bucket, making one invocation a full sync cycle; `--upload`
without a configured bucket aborts immediately.

The default command runs the `proxy` process (the HTTP front door on `ECLUSE_PORT`, default
`8080`) plus the mirror worker. The proxy scales horizontally behind a load balancer, but
**Pilot and Dredger must run as singletons**: multiple instances race, duplicate API calls, and
overlap registry deletions. Point your package manager at the proxy as a registry (see
[Connecting your clients](#connecting-your-clients)).

Before running a published image, verify its provenance and SBOM attestations: the recipe
(keyless Sigstore, Rekor, pinned by digest) is in the [README](README.md#verifying-the-image).

## The Golden Path

This is the recommended, most resilient way to run Écluse, and the posture the
[threat model](https://ecluse-proxy.com/threat-model.html) treats as canonical. Aim
for it unless you have a specific reason to diverge; each step links to its detail.

1. **Run three registries, not one.** Give the three internal roles distinct backends: a
   **first-party** store (publication target), a **public-derived mirror** store (mirror target),
   and a **pull-through** read endpoint that unions both (`ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM`).
   Separating first-party from public-derived inventory lets you scan and police each by
   provenance, and keeps the mirror auditable. Collapsing onto fewer registries works but muddies
   auditing and post-incident scoping. **The one hard rule:** the aggregating endpoint must union
   **trusted** stores only, never a direct public upstream, or raw ungated packages reach clients
   as trusted and bypass the gate. See [registry-level
   composition](docs/architecture/registry-model.md#registry-level-composition-the-recommended-topology).
2. **Let callers use their own identity (passthrough).** The default credential strategy forwards
   each caller's token to the private upstream and publication target, so access matches your
   registry IAM exactly (no escalation) and Écluse holds no standing read credential. This is the
   default; nothing to set. See [access model](docs/architecture/access-model.md).
3. **Mint the mirror-write token from the container role.** Set
   `ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER=codeartifact` so the worker mints a short-lived write
   token under the task/instance role instead of carrying a static secret (`static` is supported
   but discouraged). Scope that role **write-only** to the mirror store and keep
   `ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_TOKEN_DURATION` short: it's Écluse's only standing
   credential and it writes to the trusted store. **Scope the mirror queue the same way**: a job
   tells the worker to fetch-and-publish, so grant only the serve role `SendMessage` and only the
   worker `ReceiveMessage`/delete. Anyone who can write to the queue can force a write to the
   trusted store.
4. **Let the edge own access; leave `ECLUSE_AUTH_TOKEN` off.** Écluse is not your access boundary.
   Front it with a gateway, mesh, or IAP that admits only the networks you intend, and restrict
   reachability **both** north-south and east-west (pod-to-pod): an ingress-only allow-list that
   leaves the pod reachable inside the cluster is a common vulnerability. See [Connecting your
   clients](#connecting-your-clients).
5. **Fence egress, keep metadata reachable.** Default-deny outbound, allowing only your upstreams,
   the mirror target, the advisory bucket when `ECLUSE_VULNERABILITY_DATABASE_BUCKET` is configured
   (the proxy needs `s3:GetObject` on it to sync `osv.db`), and the metadata endpoint; reach
   CodeArtifact and S3 over VPC endpoints; require IMDSv2 with hop limit 1. Don't block the metadata
   endpoint; Écluse needs it to mint credentials. See [Securing network
   egress](#securing-network-egress-required).
6. **Make the proxy unbypassable.** Deny CI runners (and, where practical, workstations) outbound
   access to the public registries, so the only route to a package is through Écluse. This turns
   the policy from _default_ into _unbypassable_. See [Locking down CI
   egress](#locking-down-ci-egress-recommended).
7. **Verify what you run.** Pin the image by digest and verify its provenance + SBOM attestations
   before deploying (see [Verifying the image](README.md#verifying-the-image)).

The _why_ behind each choice, and the residual risks this posture accepts, is in the
[threat model](https://ecluse-proxy.com/threat-model.html) and
[Security invariants](docs/architecture/security.md#trust-assumptions--credential-posture).

## Deviating from the Golden Path

Écluse still runs if you diverge, but each deviation trades away a protection, and two are
**silent** (Écluse can't detect them, so nothing warns you):

- **Collapsing the registries onto one store** (leaving `ECLUSE_MOUNTS__NPM__MIRROR_TARGET` /
  `ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET` unset). The perimeter still holds, but first-party and
  public-derived packages share one store, so you lose provenance separation, per-provenance
  scanning, and clean post-incident scoping. **Écluse Dredger refuses to boot** if
  `MIRROR_TARGET` equals `PUBLICATION_TARGET`, since automated pruning on a shared store risks
  first-party data loss. (Register [threat #10](https://ecluse-proxy.com/threat-model.html#threat-10)
  and #16.)
- **Pointing the private upstream at a registry that itself draws from public** (say a CodeArtifact
  repo with the stock `npm-store` upstream to npmjs). This is the **dangerous one**, and Écluse
  **can't detect it**: raw ungated packages reach clients through the trusted read path, behind the
  gate instead of through it, nullifying the rules, integrity floor, and freshness quarantine.
  Aggregate **trusted stores only** into the private upstream (your first-party store plus Écluse's
  mirror), and let the gated mirror be the only way public content enters. (Register
  [threat #15](https://ecluse-proxy.com/threat-model.html#threat-15).)

The other deviations self-announce: an open edge (`ECLUSE_AUTH_TOKEN` unset) leans on your network
boundary, a static publish credential fails closed at boot without that edge, and a `static`
mirror-write secret forgoes the minted token. Each is covered at its step above.

## Configuration

Configuration has two layers: **environment variables** for process and secret values, and an
optional **config document** (YAML) for the two things too expressive for flat env vars: the rule
policy and the mount map. A single-mount npm deployment on the default policy needs no document.

The table below is the complete environment-variable reference. A value resolves as defaults <
config document < environment variable, so the environment wins. The resolution model and the
rationale behind each setting are in
[Configuration & Authentication](docs/architecture/configuration.md).

### Environment variables

| Variable | Required | Default | Description |
| :--- | :--- | :--- | :--- |
| `ECLUSE_PORT` | No | `8080` | TCP port the proxy listens on. Must be in `0..65535` (`0` binds an OS-assigned ephemeral port); an out-of-range value is rejected at load. |
| `ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM` | Yes |  | URL of the private upstream registry (the authority for reads under the default `passthrough` strategy). |
| `ECLUSE_MOUNTS__NPM__PUBLIC_UPSTREAM` | No | `https://registry.npmjs.org` | URL of the public upstream, queried anonymously and gated by the rules. |
| `ECLUSE_PUBLIC_URL` | Recommended |  | The proxy's own externally-reachable base URL (e.g. `https://registry.example.com`), used to rewrite each served `dist.tarball` to an **absolute** URL clients fetch back through the proxy. Unset, tarball URLs are path-relative and the `npm` CLI can't install from them (it reads a leading-slash `dist.tarball` as a `file:` path), so set this for any deployment serving real `npm install`s. |
| `ECLUSE_MOUNTS__NPM__MIRROR_TARGET` | No | `ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM` | Registry that approved packages are mirrored to. Unset ⇒ folds onto the private upstream (one registry, read and written). The write credential does **not** fold, set `ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER`. |
| `ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER` | No | `codeartifact` | Mirror-target write credential: `codeartifact` (mints a short-lived token under the container/task role, the shipped default) or `static` (a fixed `ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN`). `gcp-artifact-registry` is recognised but not yet built. |
| `ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN` | No |  | Static write token, used when `ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER=static`. |
| `ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN` | Depends | `codeartifact` only | CodeArtifact domain, or parsed from a CodeArtifact `ECLUSE_MOUNTS__NPM__MIRROR_TARGET` host. |
| `ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN_OWNER` | Depends | `codeartifact` only | 12-digit owning account id, or parsed from the host (a non-account-id value is rejected at boot). |
| `ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_REGION` | Depends | `codeartifact` only | Region, this key, else the host (its authoritative region), else `AWS_REGION`. |
| `ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_TOKEN_DURATION` | No |  | Token lifetime in seconds, capped at `43200` (12 h). |
| `ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET` | No |  | Where client `npm publish` (first-party packages) is written. **Opt-in: unset ⇒ `PUT /{pkg}` is `405`** (no implicit write path). May be the same registry as the private upstream. Protect this surface; see the warning below. |
| `ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN` | No |  | Static fallback credential for the publication target, forwarded only when a publishing client sends none. The default is **passthrough** (the publisher's own token). ⚠️ A static token with an open edge lets any unauthenticated client publish under it; see the warning below. |
| `ECLUSE_MOUNTS__NPM__PUBLISH_SCOPES` | Conditionally | If `ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET` is set | Comma-separated allow-list of package scopes a client may publish (e.g. `@acme,@beta`), the anti-shadowing guard: a publish outside the list is refused before any upstream write. It limits names, not callers, and is not authentication. An empty list with a publication target set is a fail-loud boot error. |
| `ECLUSE_QUEUE_BACKEND` | No | `sqs` | Mirror-queue backend: `sqs` (AWS), or `memory` (a bounded in-process queue: a non-durable, best-effort mirror for single-node or air-gapped deployments, never an automatic fallback, warns loudly at boot). `pubsub` (GCP) is recognised but not yet built. |
| `ECLUSE_QUEUE_URL` | Depends | Cloud backends only | Queue identifier: an SQS queue URL or a Pub/Sub `projects/<p>/topics/<t>` resource. **Required for the cloud backends** (absent ⇒ fail-loud at boot); not needed for `memory` (ignored). |
| `ECLUSE_QUEUE_MEMORY_MAX_DEPTH` | No | `50000` | `memory` only. Cap on in-process queue depth. An enqueue past the cap is dropped (drop-newest) and rate-limit-logged; a dropped job re-mirrors on next demand, so it's safe. Positive integer. |
| `AWS_REGION` | Depends | AWS backends only | Region for SQS and CodeArtifact. |
| `AWS_ENDPOINT_URL_SQS` / `AWS_ENDPOINT_URL` | No |  | SQS endpoint override (AWS-SDK-standard). Point at a local emulator (`ministack`) or VPC endpoint; with one set, requests are signed with `AWS_ACCESS_KEY_ID`/`AWS_SECRET_ACCESS_KEY`. Unset ⇒ normal AWS resolution. |
| `ECLUSE_GOOGLE_PROJECT` | Depends | GCP backends only | Project for Pub/Sub and Artifact Registry (credentials via ADC). |
| `ECLUSE_AUTH_TOKEN` | No |  | If set, clients must present this token (`Bearer` / `_authToken`). Omit for network-secured deployments. |
| `ECLUSE_MOUNTS__NPM__RESPECT_UPSTREAM_TARBALL_HOST` | No | `false` | Secure default. When `false`, a tarball is fetched only from the **same allowlisted upstream that served the packument**; set `true` only for a registry that serves tarballs from a separate CDN/files host (widens the fetch surface to any allowlisted host). See [Securing network egress](#securing-network-egress-required). |
| `ECLUSE_ADDITIONAL_BLOCKED_RANGES` | No |  | Comma-separated list of CIDR ranges (e.g. `10.99.0.0/16,fd12::/8`) an operator adds to the fixed internal-address block, applied identically across every mount. Extends the block only, never narrows it; a malformed entry **fails closed at boot**. See [Securing network egress](#securing-network-egress-required). |
| `ECLUSE_HELP_MESSAGE` | No |  | String appended to every denial message (e.g. a support channel). |
| `ECLUSE_LOG_FORMAT` | No | `json` | Log shape: `json` (one JSON object per line, for log collectors) or `console` (human-readable). |
| `ECLUSE_TELEMETRY` | No | `off` | OpenTelemetry master switch. With it `off`, no telemetry is emitted. When `on`, the SDK reads the standard `OTEL_*` variables. |
| `ECLUSE_CVE_SYNC_INTERVAL` | Depends | Pilot only, default `3600` | How often the Écluse Pilot singleton refreshes the OSV database from upstream. Positive integer. |
| `ECLUSE_VULNERABILITY_DATABASE_BUCKET` | No |  | The object-store bucket carrying the compiled `osv.db` advisory artifacts. Pilot uploads to it; the proxy polls it and shadow-swaps fresh artifacts into the rules engine. Unset, the proxy runs no advisory sync and `AllowIfRemediatesCve` abstains. |
| `ECLUSE_CVE_DB_POLL_INTERVAL` | No | `60` | Proxy only: how often each configured ecosystem's sync task polls the bucket for a fresh advisory database (a cheap conditional `HEAD`). Deliberately independent of, and more frequent than, Pilot's `ECLUSE_CVE_SYNC_INTERVAL`: matching them would nearly double the worst-case advisory age. Positive integer. |
| `ECLUSE_MAX_OSV_DB_BYTES` | No | `536870912` | Proxy only: refuse to download an advisory database larger than this many bytes (default 512 MiB). The declared length fails fast and the streaming download enforces the cap. |
| `ECLUSE_OSV_DATA_DIR` | No | `data/osv` | Directory for the OSV advisory databases: where Pilot compiles them, and where the proxy lands its synced per-ecosystem artifacts. During a swap, actual disk use briefly exceeds what `ls` or `du` show: the superseded file is unlinked while its last readers finish, and the kernel frees the space when the drained connection closes. |
| `ECLUSE_OSV_EXPORT_BASE_URL` | No | `https://osv-vulnerabilities.storage.googleapis.com` | Base URL of the per-ecosystem OSV advisory exports Pilot compiles from (`<base>/<ecosystem>/all.zip`). Override it if the upstream moves or you mirror the exports. |
| `ECLUSE_SHUTDOWN_DRAIN_TIMEOUT` | No | `30` | Seconds the graceful shutdown waits for in-flight requests and in-progress artifact streams to finish before the process exits. Positive integer. |
| `ECLUSE_CORES` | No | derived | Cores (GHC capabilities) the process claims. Unset ⇒ derived from the container's cgroup CPU quota (floored, at least 1, clamped to the visible processors); with no cgroup limit either, the runtime's own detection stands. Give the container **whole cores**; see the runtime sizing note. The boot log prints the decision and its provenance. Positive integer. See [Operating Écluse → Runtime sizing](#operating-écluse). |
| `ECLUSE_MAX_HEAP_BYTES` | No | derived | Heap ceiling in bytes, enforced by the GHC runtime (a breach is a clean heap-overflow error rather than a kernel OOM kill). Unset ⇒ derived from the cgroup memory limit less the nursery budget and 10% slack; with no cgroup limit, unbounded unless your own `GHCRTS -M` says otherwise. Enforcing a ceiling re-executes the binary once, in place (same PID). Positive integer. |
| `ECLUSE_SERVE_MAX_IN_FLIGHT` | No | computed | Process-wide cap on concurrent metadata materialisation (whole packument requests and the public-metadata gate a tarball miss reaches). Unset, computed at boot as `max(8, 10 x cores)` and logged. Over the cap, a request waits up to 1 second for a slot (a bounded waiting room, no queue-jumping) and proceeds when one frees; only a request that finds the room full or waits out that budget gets `503 Service Unavailable` with `Retry-After: 1`. Trusted private tarball hits, health probes, and local routes stream outside the cap. Positive integer. A 503 **with** `Retry-After: 1` is intentional backpressure, not a failure: exclude it from alerts (a real upstream failure returns 503 without that header), and a service mesh can auto-retry it. |
| `ECLUSE_PUBLIC_CONNECTIONS_PER_HOST` | No | computed | Maximum pooled (kept-for-reuse) connections per public upstream host. Unset, computed at boot as `clamp(32, 1024, nofile / 8)` and logged. Connections beyond the pool still open, but re-handshake TLS each time. Positive integer. The private pool is sized separately (next row). |
| `ECLUSE_PRIVATE_CONNECTIONS_PER_HOST` | No | computed | Maximum pooled connections to the private upstream host. Unset, computed at boot as a quarter of the soft `RLIMIT_NOFILE`, clamped to 64-4096, and logged. Sized for the trusted tarball hit, which streams outside `ECLUSE_SERVE_MAX_IN_FLIGHT`. The pool governs reuse, not socket count. Positive integer. |
| `ECLUSE_CACHE_TTL` | No | `60` | Seconds metadata is kept in the shared packument cache. |
| `ECLUSE_CACHE_MAX_ENTRIES` | No | `1024` | Maximum number of items the metadata cache will hold. |
| `ECLUSE_CACHE_MAX_BYTES` | No | `268435456`, 256 MiB | Resident-byte budget for **each** of the metadata cache's stores (the full-packument store, the single-version store, and the assembled-representation store), so the worst-case total is three budgets. |
| `ECLUSE_MAX_RESPONSE_BYTES` | No | `12582912`, 12 MiB | Largest upstream **metadata** body buffered before the fetch aborts fail-closed. Bounds memory against a hostile upstream returning a giant body. Positive integer. |
| `ECLUSE_MAX_VERSION_COUNT` | No | `100000` | Largest version count a packument may carry before it is refused. Bounds per-version rule evaluation against a version flood. Positive integer. |
| `ECLUSE_MAX_NESTING_DEPTH` | No | `64` | Deepest JSON nesting a decoded upstream document may reach before it is refused. Bounds CPU/stack against a pathologically nested payload. Positive integer. |
| `ECLUSE_MIN_PUBLIC_INTEGRITY` | No | `sha256` | Minimum integrity algorithm a **public** (untrusted) version's digest must meet: `sha256`, `sha384`, `sha512`, or `blake2b`. A weaker or absent digest is refused with `403`. Hard-floored at SHA-256: `sha1`/`md5`/an unknown name is rejected at startup. The trusted path has its own loosenable floor (`ECLUSE_MIN_TRUSTED_INTEGRITY`). |
| `ECLUSE_MIN_TRUSTED_INTEGRITY` | No | `sha256` | Minimum integrity algorithm a **trusted** (private) version's digest must meet. Defaults to `sha256`, so a SHA-1-only or hashless private version is dropped like a public one, but unlike the public floor is **loosenable below SHA-256** (`sha1`/`md5`) for a legacy private mirror. An unknown name is rejected at load. |

Configuration is validated in full at startup and the process refuses to start on any problem (an
unknown rule type, a bad URL, an unresolved policy reference): a misconfiguration is a loud,
immediate failure, never a quietly mis-enforced policy.

> ⚠️ **The first-party publish surface authorises _names_, not _callers_.** With publishing enabled
> (`ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET`), the `ECLUSE_MOUNTS__NPM__PUBLISH_SCOPES` allow-list
> limits which package names may be published; it's not authentication and says nothing about who
> may publish. So a static `ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN` (used only when a
> publisher forwards none) is **fail-closed**: set it without `ECLUSE_AUTH_TOKEN` and Écluse refuses
> to start (`PublishStaticCredentialNeedsEdge`), so "static publish credential + open edge", which
> would let any unauthenticated client publish under the operator's credential, is unrepresentable.
> `ECLUSE_AUTH_TOKEN` is the edge Écluse can verify itself; an external layer (gateway, mTLS,
> network policy) is good defence-in-depth but doesn't satisfy this. Pure **passthrough** (no static
> token, the default) needs none of this. See
> [Access model → Publishing](docs/architecture/access-model.md#publishing-the-publication-target-passthrough-write).

### The configuration document

A YAML file mounted at `/etc/ecluse/config.yaml`. It carries the **rule policy** (see [Rule
policy](#rule-policy)) and, for multi-mount deployments, the **mount map**. Single-mount
deployments desugar from the env vars above and need no document. Schema and examples:
[Configuration & Authentication](docs/architecture/configuration.md#configuration).

Deployments derive their initial policy from the [default baseline configuration
(`config/default.yaml`)](config/default.yaml).

### Secrets

Secrets never live in the config document. Client and registry tokens are always env vars, and
cloud-managed registries (CodeArtifact / Artifact Registry) derive short-lived tokens from
ambient cloud credentials. Écluse always holds a mirror-target **write** credential; reads follow
the mount's [credential strategy](docs/architecture/access-model.md): `passthrough` (default)
forwards the client's own token to the private upstream and strips it before the public one,
`service` reads with Écluse's own credential. See
[Outbound Registry Credentials](docs/architecture/configuration.md#outbound-registry-credentials).

## Connecting your clients

Point your package manager at the proxy as its registry. With `ECLUSE_AUTH_TOKEN` set, supply
it the standard npm way:

```ini
# .npmrc
registry=https://ecluse.example.internal/
//ecluse.example.internal/:_authToken=${ECLUSE_TOKEN}
```

Edge authentication to the proxy has three modes (and feeds the mount's
[credential strategy](docs/architecture/access-model.md), which decides how the upstreams
are then credentialled):

1. **Open**: `ECLUSE_AUTH_TOKEN` unset; access control is delegated entirely to the network
   layer (VPC, service mesh). Appropriate only on a closed network.
2. **Static token**: `ECLUSE_AUTH_TOKEN` set; clients send it as
   `Authorization: Bearer <token>` or `.npmrc` `_authToken`.
3. **Trusted edge identity**: a fronting gateway / IAP / mesh asserts a verified identity.
   Écluse honours it **only over a verifiable binding to that edge** (mutual TLS, or a shared
   secret / HMAC on the asserted identity), and **refuses to start** a `trusted-edge` mount with
   neither. A bare trusted header is forgeable wherever the proxy is reachable off the edge, so
   restrict reachability to the edge east-west as well as north-south.

## Securing network egress (required)

Écluse fetches from the registries you point it at, and some URLs it follows (a version's
`dist.tarball`) come from upstream responses. Apply least-privilege egress in two layers. Écluse
provides the first in the application, with an **origin-aware trust model**:

- **Untrusted origins**: the public-upstream fetch and every `dist.tarball` fetch are gated by a
  host **allowlist** (Écluse dials only your configured upstream hosts), fetched **HTTPS-only**
  with TLS certificate validation, and bounded by **response-size limits**. A non-HTTPS upstream
  fails closed at boot, and a `dist.tarball` is normalised to HTTPS or refused (below).
  Certificate validation closes the resolve-to-internal and DNS-rebinding SSRF class: an address
  a name is steered to can't present a CA-trusted certificate for the host. A **pure literal
  internal-range block** (loopback, link-local incl. the `169.254.169.254` metadata endpoint,
  unspecified `0.0.0.0/8` / `::`, RFC1918, CGNAT, IPv6 ULA `fc00::/7` incl. `fd00:ec2::254`) stays
  as cheap defence-in-depth on the `dist.tarball` host: a tarball whose host is an
  internal-address literal is refused. Extend it with `ECLUSE_ADDITIONAL_BLOCKED_RANGES`
  (comma-separated CIDRs, every mount alike); it only ever widens, never narrows.
- **The trusted private origin** (`ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM`) is deliberately **not**
  subject to the internal-range block: a private registry legitimately lives on your internal
  network.

SSRF to the instance-metadata endpoint is prevented in the application, not by blocking metadata at
the network. An untrusted upstream or `dist.tarball` can't steer a fetch to `169.254.169.254`: the
proxy dials only allowlisted hosts over HTTPS with certificate validation, and the literal block
refuses a `dist.tarball` whose host is that address. Écluse's own metadata access goes through the
AWS SDK to mint its instance-role credentials, so don't deny the proxy egress to metadata or
internal ranges, that breaks its own credentials.

Provide the second layer at the platform, protecting your data targets (registries, mirror):

- **Require IMDSv2, hop limit 1** (AWS `httpPutResponseHopLimit: 1`): keeps the proxy's own
  credential minting working while stopping a neighbour or forwarded request from reaching
  metadata through extra hops. Don't deny egress to `169.254.169.254` outright; Écluse needs it
  for credentials.
- **Default-deny egress, allow only your registries + mirror target.**
  - **AWS**: security-group egress rules / network ACLs to the upstream and mirror CIDRs (plus
    the metadata endpoint the instance role needs).
  - **GCP**: VPC firewall egress rules and, where applicable, VPC Service Controls.
  - **Kubernetes**: a default-deny `NetworkPolicy` with an explicit egress allowlist; allow your
    private upstream's internal range.
  - **Service mesh (Istio/Linkerd)**: set the sidecar outbound policy to `REGISTRY_ONLY`, declare
    each upstream as a `ServiceEntry`, and constrain it with a `Sidecar` egress listener and an
    egress `AuthorizationPolicy`.
- **Grant the proxy only the cloud permissions it needs**: the mirror-write credential, the
  advisory-bucket read (`s3:GetObject`) when `ECLUSE_VULNERABILITY_DATABASE_BUCKET` is set, and
  (under the `service` strategy) the private-read credential, nothing more.

**The `dist.tarball` host policy.** `dist.tarball` is upstream-chosen, so by default Écluse
fetches a tarball only from the same allowlisted upstream that served the packument; a different
host is refused even if allowlisted. If your registry serves artifacts from a separate CDN/files
host (the PyPI-files-host shape), set `ECLUSE_MOUNTS__NPM__RESPECT_UPSTREAM_TARBALL_HOST=true` to
allow any allowlisted host. It never escapes the allowlist or internal-range block, but widens the
fetch surface, so opt in deliberately.

The rationale is in [Security: outbound-request and input-validation
invariants](docs/architecture/security.md#network-egress-is-a-shared-responsibility).

### Securing Écluse Pilot and Dredger

The auxiliary services (the **Écluse Pilot** ingestion pipeline and the **Écluse Dredger** reaper)
need distinct, tightly scoped egress, and **both must run as singletons** (one replica).

- **Écluse Pilot**: no public ingress. Egress to `osv.dev` (raw advisories), the instance-metadata
  endpoint (credentials), and your object store (S3/GCS) with `s3:PutObject` to upload `osv.db`.
  The object is named `<ecosystem>-osv-schema<N>.db` (e.g. `npm-osv-schema1.db`, `N` = the
  table-schema epoch); the key is stable per ecosystem, so bucket policies and the proxy's ETag
  polling can target it. On `osv.dev` `5xx`/`408`/`429`, Pilot retries with capped, jittered
  backoff, then logs and waits the full `ECLUSE_CVE_SYNC_INTERVAL`, so a transient outage can't get
  your NAT address rate-limited. To avoid an idling pod, schedule the one-shot instead: `ecluse
  pilot compile --out /tmp/osv --upload` as a `CronJob` with `concurrencyPolicy: Forbid` (which
  preserves the singleton).
- **Écluse Dredger**: no public ingress. Egress only to your private mirror (Registry B) for
  delete requests and to the instance-metadata endpoint for credentials. It holds a standing
  high-privilege delete capability, so isolate it from all untrusted networks.

## Locking down CI egress (recommended)

The controls above secure Écluse's own egress. This one secures your consumers', turning Écluse
from a proxy clients are asked to use into the registry they can only reach.

If you control CI, **deny runners outbound access to the public registries** (`registry.npmjs.org`
and the equivalents for other ecosystems) and let them reach only Écluse and your internal
services. Point the runners' package managers at Écluse.

Now a misconfigured job (a stray `--registry` flag, a committed `.npmrc` at the public registry, a
tool that ignores your settings) can't quietly bypass the policy: it can't reach the public
registry, so it fails instead of pulling an unvetted package. You depend on the network you
administer centrally, not on every job being configured correctly.

This is what makes the policy _unbypassable_ rather than merely _default_: per-project
package-manager and version-manager setups (npm/pnpm config, nvm, Nix shells, containers) can
override what you ship to a machine, but none can route around a network that only reaches Écluse.
See [MOTIVATION → The bar](MOTIVATION.md#the-bar-a-chokepoint-you-cant-step-around).

The same idea extends to developer workstations (tarball fetches only through Écluse on a managed
network, browsing and search left open), though workstations are a softer control than CI.

## Rule policy

Écluse evaluates a named map of rules over a built-in **deny-by-default** policy: a package is
admitted only if a rule allows it, and every deny type outranks every allow type by default, so a
matching deny wins. The shipped default is small and biased toward resilience rather than blanket
bans:

- **`min-age`**: admit public versions older than a quarantine window (7 days by default), the core
  defence against race-to-publish typosquatting and dependency confusion. On at launch.
- **`AllowIfRemediatesCve`** (`remediation-fast-track`): admit a release a synced advisory names as
  its exact fixed version ahead of the quarantine, provided no other advisory still affects it. On
  at launch; it abstains until an advisory database has been synced (set
  `ECLUSE_VULNERABILITY_DATABASE_BUCKET` and run Pilot), so without one only the quarantine governs.
  It's a deliberate exact match on `fixed`: a fix under any other version string waits out the
  quarantine, with `AllowByIdentity` as the workaround.
- **`AllowByIdentity`**: admit a specific package or `package@version` past the quarantine (e.g. a
  security fix the exact-match probe can't see), at the top of the allow band but still below every
  deny. Available.
- **`revoke`**: a hard-deny (`DenyByIdentity`) rule for a specific package or `package@version`, at
  a precedence above the scope allow-list. Available.
- **`DenyIfCve`**: an **opt-in** deny gate that blocks a version a synced advisory records as
  affected at or above a CVSS `minSeverity` (a base score, 0-10). Because the npm malware feed
  carries no CVSS score, and an unscored advisory is treated as above every threshold, enabling it
  also blocks known-malicious packages, not only high-severity CVEs. It sits just *below*
  `AllowByIdentity`, so an explicit identity pin overrides it. Its `onUnavailable` knob (`deny` by
  default, or `skip`) decides what happens when the advisory database cannot answer. **Off by
  default**; read *Onboarding DenyIfCve* below before enabling.

You override values, add rules (e.g. opt into `DenyInstallTimeExecution`), or suppress a
default by name in the configuration document:

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

#### Onboarding DenyIfCve

`DenyIfCve` is powerful enough to break a cold deployment: on a freshly-stood-up mirror it can deny
historical versions your existing builds still depend on that an advisory has since covered. Enable
it *after* your private mirror is warmed, not before:

1. Leave `DenyIfCve` out of your policy and run Écluse normally, so your CI and developers pull the
   versions you depend on. Each is mirrored into the trusted store (Registry B), which the rules
   never re-gate once it is there.
2. Once your must-have builds are mirrored, add `DenyIfCve` with a `minSeverity` you are comfortable
   with (8 blocks high and critical CVEs; malware blocks regardless of the threshold).
3. If a specific version you must keep is then denied (a false positive, or a risk you accept), pin
   it with an `AllowByIdentity` rule, which outranks `DenyIfCve`.

Set `onUnavailable: skip` if you would rather the gate fail open (skip itself, logging loudly) than
refuse traffic when the advisory database is briefly unavailable; the default `deny` fails closed,
so a version that cannot be vetted is not admitted.

Full semantics (precedence, the patch/add/suppress merge, and the strict validation) are in
[Rule policy](docs/architecture/configuration.md#rule-policy) and
[Rules Engine](docs/architecture/rules-engine.md).

### Always-on: a public version must carry a strong integrity digest

Independent of the rules above, one admission policy is non-negotiable on **public** (untrusted)
upstreams: a version is served only if its `dist` carries an integrity digest meeting the
**integrity floor** (`ECLUSE_MIN_PUBLIC_INTEGRITY`, default **SHA-256**). SHA-1 and MD5 have
practical collisions, so a weak-or-absent digest could let a substituted artifact pass. A public
version whose strongest digest is absent or below the floor (e.g. only a legacy SHA-1 `shasum`) is
inadmissible: its tarball returns `403` and it's filtered from the served packument, so a client
never sees a version it couldn't safely fetch.

The floor may be **raised** (`sha512`, `blake2b`) but never lowered; a sub-floor value is rejected
at startup. The trusted private path has its own floor, `ECLUSE_MIN_TRUSTED_INTEGRITY`, also
defaulting to `sha256` (so a SHA-1-only private version is dropped too) but **loosenable below
SHA-256** (`sha1`/`md5`) for a legacy private mirror, where trust substitutes for cryptographic
strength.

**Gotcha.** A custom or off-spec public upstream serving versions without a floor-meeting digest
will have those versions silently disappear, and direct fetches `403`. This is deliberate. To
serve such a source, point it at the **private** upstream slot and loosen
`ECLUSE_MIN_TRUSTED_INTEGRITY` below `sha256`.

## Operating Écluse

- **Pre-warming the cache.** A cold `npm install` against an empty cache hits the proxy with
  dozens of heavy requests at once, causing latency spikes or `503` backpressure. Pre-warm as part
  of deployment: run an `npm install` (or a script fetching your heavy dependencies) after starting
  Écluse, before sending production traffic. Once warm, request coalescing absorbs spikes.
- **Health probes.** `GET /livez` reports process liveness (a stalled mirror worker fails it);
  `GET /readyz` reports config loaded and the listener serving. Readiness is deliberately lenient
  about public-upstream reachability, so a transient blip doesn't pull a healthy pod from rotation.
  With an advisory bucket configured, readiness also waits for each configured ecosystem's first
  advisory sync (a one-way flip per ecosystem, so it never flaps); the listener serves throughout,
  since an absent advisory database abstains into deny-by-default, so the gate governs routing, not
  whether the process answers. Mounting an ecosystem whose artifact Pilot never publishes declares a
  sync that never arrives, so the pod never reports ready. The npm liveness probe `GET /-/ping`
  answers locally with `200 {}`. **Pilot and Dredger** export the same `/livez` and `/readyz` on
  `ECLUSE_PORT`.
- **Logs.** One JSON object per line by default (`ECLUSE_LOG_FORMAT=json`), or `console` for local
  development. Bearer tokens render as a redacted placeholder, so token material never reaches a log
  field.
- **Telemetry (opt-in).** OpenTelemetry traces and metrics are off by default; set
  `ECLUSE_TELEMETRY=on`. Set `DD_*` (`DD_SERVICE`, `DD_ENV`, `DD_VERSION`, `DD_AGENT_HOST`) for
  Datadog or the standard `OTEL_*` for any other backend; `DD_*` wins where both are set, and the
  resolved identity stamps both traces and the `dd` object on every log line. `DD_API_KEY`/`DD_SITE`
  are ignored: Écluse only exports to a node-local collector or Agent.
  - **You declare the destination.** Export goes to `http://localhost:4318` by default, or wherever
    `DD_AGENT_HOST`/`OTEL_EXPORTER_OTLP_ENDPOINT` points. Écluse doesn't gate it; for a remote
    collector, authenticate out of band with `OTEL_EXPORTER_OTLP_HEADERS`.
  - **Never on the request path.** Export is async and batched, so an unreachable collector never
    slows a request; an absent endpoint logs one boot warning and falls back to localhost, and
    persistent errors throttle to a periodic heartbeat.
- **Search.** `GET /-/v1/search` returns `501` by design: search is a discovery convenience, not an
  install path. Use the public registry's website.
- **Runtime sizing (cores and memory).** At boot Écluse resolves how many cores to claim and what
  heap ceiling to run under, logging each decision with its provenance, so the posture is readable
  from the start-up lines. Resolution order per knob:
  1. **Explicit config wins**: `ECLUSE_CORES` (or `cores`) and `ECLUSE_MAX_HEAP_BYTES`
     (`maxHeapBytes`), positive integers.
  2. **Otherwise derive from the cgroup (v2)**: the CPU quota, floored (at least 1) and clamped to
     visible processors; the memory limit less the nursery budget (cores x allocation area) less
     10% slack, floored at half the limit.
  3. **No limit either way**: the GHC runtime's own resolution stands (its defaults plus any
     `GHCRTS`), and a `GHCRTS` heap ceiling you set is never overridden.

  **Give Écluse whole cores.** A fractional CPU limit (say 3.5) has no good option: claiming 4
  capabilities overruns the CFS quota during stop-the-world GC, freezing the process mid-pause;
  flooring to 3 never self-throttles but strands the fraction. Écluse floors the derived count, so
  pair an integer limit with `requests = limits` (and exclusive cores where offered) to remove
  throttling structurally. A CPU **limit** doesn't shrink the processor count the runtime sees, so
  without `ECLUSE_CORES` a 2-CPU pod on a 32-core node would claim 32 capabilities and 32 nurseries.
  Enforcing a heap ceiling needs runtime flags fixed at start, so Écluse **re-executes its own
  binary once, in place** (same PID), logging `runtime: re-launching with GHCRTS ...` first.
- **Runtime memory arithmetic (proxy pod).** For the **proxy** role; the other roles differ (Pilot
  runs a scheduled compute, the Dredger follows its pruning rules), so tune their allocation area
  via `GHCRTS` separately, though the cores/heap resolution above still applies to every role. The
  binary ships `-A64m -n4m` (a 64 MiB per-core allocation area in 4 MiB chunks), trading bounded
  extra memory for far fewer GCs under load. Budget roughly `cores x 64 MiB` of nursery, plus the
  live heap (dominated by the metadata cache), plus up to one live-heap of copying headroom during a
  major GC. Worked shapes: a 2-CPU / 512 MiB pod runs as-is; a 2-CPU / 256 MiB pod also needs
  `GHCRTS="-A16m"`; a 4-CPU pod wants ~750 MiB on defaults, or 512 MiB with `-A32m`. Taller pods
  amortise the cache and coalescing better, so prefer 4-CPU-ish shapes. Tune the allocation area
  with `GHCRTS`; the boot log prints the effective value.
- **Revoking a mirrored version (internal yank).** The mirror store (Registry B) deliberately
  resists upstream yanks, so a benign yank doesn't break your installs, but a version later found
  malicious isn't removed automatically (Écluse never re-gates trusted content). Usually this
  resolves itself: once the public registry yanks the bad version its bytes change or vanish,
  re-mirroring can't reproduce them, and you purge the stale copy from Registry B at leisure. When
  your own scanning is ahead of the public yank, revoke in order: **(1)** deny the identity (a
  `DenyByIdentity` rule), so the serve path stops admitting it and the worker stops re-mirroring,
  then **(2)** purge that version from Registry B. **Order matters:** purge alone is a treadmill,
  since while the version is live upstream the next install re-admits and re-mirrors it.

## Planned controls

Documented ahead of implementation so the configuration surface is known.

- **GCP backends** (**planned**): the Pub/Sub `MirrorQueue` and ADC credential leaf. The AWS
  equivalents (SQS `MirrorQueue`, CodeArtifact credential leaf, mirror worker, composition root)
  are built and wired.
The full deployment runbook ships with the launch.

## Learn more

The internal design, for when you need the _why_:

- [Architecture overview](docs/architecture.md)
- [Configuration & Authentication](docs/architecture/configuration.md)
- [Security invariants & network egress](docs/architecture/security.md)
- [Threat model](https://ecluse-proxy.com/threat-model.html), the STRIDE register, generated from the OWASP Threat Dragon model ([`threat-modelling/ecluse.json`](threat-modelling/ecluse.json))
- [Rules engine](docs/architecture/rules-engine.md)
- [Multi-ecosystem hosting & URL rewriting](docs/architecture/web-layer.md#web-layer)
- [Release & supply-chain operations](docs/architecture/release-supply-chain.md)
