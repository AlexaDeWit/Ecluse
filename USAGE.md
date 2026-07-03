# Using Écluse: Operator Manual

This is the **operator-facing manual** for deploying and running Écluse: how to configure
it, connect your clients, and, importantly, how to fence its network egress so it stays a
safe link in your supply chain. It's the consumer companion to the internal
[architecture documents](docs/architecture.md), which explain the _why_ behind everything
here.

> **Status: pre-launch.** Écluse is under active development. This manual is the
> **configuration and operational contract**, the env vars, the config schema, the client
> setup, and the security responsibilities. Features still landing are marked **(planned)**;
> treat this as the deployment contract, not a claim that every capability below is wired
> today.

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

Écluse sits between your build (developer machine or CI) and the npm registry, and enforces
a **deny-by-default resilience policy** before any package reaches a build. It reads through
a **private upstream** first, falls back to the **public** registry with rules applied, and
mirrors approved packages asynchronously. It's a policy gate, **not** a registry, and it
hosts nothing itself. The design is in [`docs/architecture.md`](docs/architecture.md).

## Deployment model

Écluse ships as a single, reproducible container image providing a **unified multicall executable**. It can run the HTTP proxy server (`ecluse proxy`), the OSV ingestion pipeline (`ecluse pilot`), or the registry cleanup worker (`ecluse dredger`) depending on the container command. All three roles share the exact same configuration file and rule definitions. 

The default command runs the `proxy` process (the HTTP front door on `ECLUSE_PORT`, default `4873`) and, alongside it, the mirror worker. While the `proxy` process is designed to scale horizontally behind a load balancer, **Écluse Pilot and Écluse Dredger must be deployed as singletons** (exactly one running instance each). Running multiple instances of Pilot or Dredger will cause race conditions, duplicate API calls, and aggressive overlapping registry deletions. Point your package manager at the proxy as a registry (see
[Connecting your clients](#connecting-your-clients)).

Before you run a published image, **verify its provenance and SBOM attestations**: the
recipe (keyless Sigstore + Rekor, pinned by digest) is in the
[README](README.md#verifying-the-image).

## The Golden Path

The sections below cover every knob; this is the **recommended, most resilient way to run
Écluse**, the posture the [threat model](https://alexadewit.github.io/Ecluse/threat-model.html) treats as canonical,
and the one to aim for unless you have a specific reason to diverge. Each step links to its
detail.

1. **Run three registries, not one.** Configure distinct backends for the three internal
   roles: a **first-party** store (publication target), a **public-derived mirror**
   store (mirror target), and a **pull-through** read endpoint that aggregates both
   (`ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM`). Keeping first-party and public-derived inventory physically
   separate lets you apply distinct storage-level policies and scanning per provenance. It also keeps your package inventory auditable. Collapsing them onto fewer registries still
   works, but muddies auditing and post-incident scoping. **The one hard rule:** your
   aggregating endpoint must union **trusted** stores only, never a direct public
   upstream. Otherwise, raw, ungated public packages reach clients as trusted, bypassing Écluse's
   gate. See [registry-level
   composition](docs/architecture/registry-model.md#registry-level-composition-the-recommended-topology).
2. **Let callers use their own identity (passthrough).** The default credential strategy
   forwards each caller's own registry token to the private upstream and the publication
   target. Access matches what your registry's IAM already grants, with no privilege
   escalation or compression, and Écluse holds no standing read credential. This is the
   launch default; there is nothing to set. See [access model](docs/architecture/access-model.md).
3. **Mint the mirror-write token from the container role.** Set
   `ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER=codeartifact` so the worker mints a short-lived write
   token under the task/instance role rather than carrying a static secret (`static` is
   supported but discouraged). Scope that role **write-only** to the mirror store and keep
   the token duration short (`ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_TOKEN_DURATION`). It is
   Écluse's only standing credential and it writes to the trusted store, so enforce least-privilege.
   **Scope the mirror queue the same way**. The queue is part of the same trust boundary:
   a job directs the worker to fetch-and-publish, so grant only the serve role `SendMessage`
   (enqueue) and only the worker `ReceiveMessage`/delete. Anyone who can write to the queue can
   force the worker to write to the trusted store.
4. **Let the edge own access; leave `ECLUSE_AUTH_TOKEN` off.** Écluse is not your access
   boundary. Front it with a gateway, service mesh, or IAP that admits only the networks you
   intend (e.g., office ranges, a VPN tunnel). Restrict **both** north-south _and_ east-west
   (pod-to-pod) reachability. Relying on an ingress-only allow-list that still leaves the pod
   reachable from inside the cluster is a common vulnerability. See [Connecting your
   clients](#connecting-your-clients).
5. **Fence egress, keep metadata reachable.** Default-deny outbound, allowing only your
   upstreams, the mirror target, and the metadata endpoint; reach CodeArtifact over **VPC
   endpoints**; require **IMDSv2 with hop limit 1**. Do **not** block the metadata endpoint
   , Écluse needs it to mint credentials. See [Securing network
   egress](#securing-network-egress-required).
6. **Make the proxy unbypassable.** Deny your CI runners (and, where practical,
   workstations) outbound access to the public registries so the only route to a package is
   through Écluse. This is what turns the policy from _default_ into _unbypassable_. See
   [Locking down CI egress](#locking-down-ci-egress-recommended).
7. **Verify what you run.** Pin the image by digest and verify its provenance + SBOM
   attestations before deploying (see [Verifying the image](README.md#verifying-the-image)).

The _why_ behind each choice, and the residual risks the canonical posture knowingly
accepts, is in the [threat model](https://alexadewit.github.io/Ecluse/threat-model.html) and
[Security invariants](docs/architecture/security.md#trust-assumptions--credential-posture).

## Deviating from the Golden Path

The [Golden Path](#the-golden-path) is the posture the threat model treats as canonical. Écluse still
_runs_ if you diverge, but each deviation trades away a specific protection, and a couple are **silent**,
in that Écluse cannot detect them, so nothing warns you. The registry-topology deviations are the sharpest:

- **Collapsing the registries onto one store**, the default if you leave `ECLUSE_MOUNTS__NPM__MIRROR_TARGET` /
  `ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET` unset. The _perimeter_ still holds (public content is still gated), but you
  lose **provenance separation**. First-party and public-derived packages share one store, so you can no
  longer apply distinct per-provenance scanning or policy, and post-incident scoping is muddied.
  Furthermore, **Écluse Dredger will refuse to boot** if `ECLUSE_MOUNTS__NPM__MIRROR_TARGET` matches `ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET`, because automated pruning on a shared datastore risks catastrophic first-party data loss. You give up auditability, defence-in-depth, and automated reaping, not the gate.
  (Register [threat #10](https://alexadewit.github.io/Ecluse/threat-model.html) and #16.)
- **Pointing the private upstream at a registry that itself draws from public**; e.g. a CodeArtifact repo
  carrying the stock `npm-store` upstream to npmjs. This is the **dangerous one.** Raw, ungated public
  packages then reach clients through the _trusted_ read path, _behind_ Écluse's gate instead of through
  it, silently nullifying the rules, integrity floor, and freshness quarantine that are the entire reason
  to run Écluse. **Écluse cannot detect this**: the private upstream is trusted by construction and its
  registry-level wiring is invisible to the proxy, so there is no boot error and no warning. Aggregate
  **trusted stores only** (your first-party store + Écluse's sanitized mirror) into the private upstream,
  and let Écluse's gated mirror be the _only_ path public content takes in. (Register
  [threat #15](https://alexadewit.github.io/Ecluse/threat-model.html).)

The other steps carry smaller, self-announcing trade-offs: an open edge (`ECLUSE_AUTH_TOKEN` unset) leans
entirely on your network boundary; a static publish credential _requires_ that edge, and Écluse fails
closed at boot if you set one without it; a `static` mirror-write secret forgoes the short-lived minted
token. Each is described at its step above and in
[Security invariants](docs/architecture/security.md#trust-assumptions--credential-posture).

## Configuration

Configuration has two layers: **environment variables** for process-level and secret
values, and an optional **structured config document** for the two things too expressive for
flat env vars, namely the rule policy and the mount map. The common single-mount npm
deployment on the default policy needs **no document at all**.

The authoritative semantics, validation rules, and rationale live in
[Configuration & Authentication](docs/architecture/configuration.md); this section is the
operator reference. **Keep the two in sync** when either changes.

### Environment variables

| Variable | Required | Default | Description |
| :--- | :--- | :--- | :--- |
| `ECLUSE_PORT` | No | `4873` | TCP port the proxy listens on. Must be in `0..65535` (`0` binds an OS-assigned ephemeral port); an out-of-range value is rejected at load. |
| `ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM` | Yes |  | URL of the private upstream registry (the authority for reads under the default `passthrough` strategy). |
| `ECLUSE_MOUNTS__NPM__PUBLIC_UPSTREAM` | No | `https://registry.npmjs.org` | URL of the public upstream, queried anonymously and gated by the rules. |
| `ECLUSE_PUBLIC_URL` | Recommended |  | The proxy's own externally-reachable base URL (e.g. `https://registry.example.com`), used to rewrite each served `dist.tarball` to an **absolute** URL clients fetch back through the proxy. **Unset, tarball URLs are path-relative, which the `npm` CLI cannot install from**; it reads a leading-slash `dist.tarball` as a local `file:` path, so set this for any deployment that serves real `npm install`s. |
| `ECLUSE_MOUNTS__NPM__MIRROR_TARGET` | No | `ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM` | Registry that approved packages are mirrored to. Unset ⇒ folds onto the private upstream (one registry, read and written). The write credential does **not** fold, set `ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER`. |
| `ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER` | No | `static` | Mirror-target write credential: `static` (`ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN`) or `codeartifact` (mints under the container/task role). `gcp-artifact-registry` is recognised but not yet built. |
| `ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN` | No |  | Static write token, when `ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER=static` (the default). |
| `ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN` | Depends | `codeartifact` only | CodeArtifact domain, or parsed from a CodeArtifact `ECLUSE_MOUNTS__NPM__MIRROR_TARGET` host. |
| `ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN_OWNER` | Depends | `codeartifact` only | 12-digit owning account id, or parsed from the host (a non-account-id value is rejected at boot). |
| `ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_REGION` | Depends | `codeartifact` only | Region, this key, else the host (its authoritative region), else `AWS_REGION`. |
| `ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_TOKEN_DURATION` | No |  | Token lifetime in seconds, capped at `43200` (12 h). |
| `ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET` | No |  | Where client `npm publish` (first-party packages) is written. **Opt-in: unset ⇒ a `PUT /{pkg}` is `405`** (no implicit write path). May be the same registry as the private upstream (so published packages are then readable via the private leg). **Protect this surface; see the warning below.** |
| `ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN` | No |  | Static fallback credential for the publication target, forwarded only when a publishing client sends no token of its own. The default model is **passthrough**, the publisher's own forwarded token. **⚠️ A static token with an open edge lets any unauthenticated client publish under it; see the warning below.** |
| `ECLUSE_MOUNTS__NPM__PUBLISH_SCOPES` | Conditionally | If `ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET` is set | Comma-separated allow-list of package scopes a client may publish (e.g. `@acme,@beta`), the anti-shadowing guard. A publish whose name is outside the list is refused **before any upstream write**, so a client cannot publish a name that shadows a public package. It limits **names, not callers**; it is not authentication. An empty list with a publication target set is a fail-loud boot error. |
| `ECLUSE_QUEUE_BACKEND` | No | `sqs` | Mirror-queue backend: `sqs` (AWS), or `memory` (a bounded in-process queue, no cloud queue, at the cost of a **non-durable, best-effort** mirror; an explicit choice for a simple/single-node/air-gapped deployment, never an automatic fallback, selecting it warns loudly at boot). `pubsub` (GCP) is recognised but not yet built. |
| `ECLUSE_QUEUE_URL` | Depends | Cloud backends only | Queue identifier: an SQS queue URL or a Pub/Sub `projects/<p>/topics/<t>` resource. **Required for the cloud backends** (absent ⇒ fail-loud at boot); **not needed for `memory`** (no external queue, ignored). |
| `ECLUSE_QUEUE_MEMORY_MAX_DEPTH` | No | `50000` | `memory` only. Cap on the in-process queue depth. A cold-cache `npm ci` enqueues thousands of jobs at once, so the queue is hard-bounded: an enqueue past the cap is **dropped (drop-newest)**, safe, since a dropped job is re-mirrored on the next demand, and rate-limit-logged. Positive integer. |
| `AWS_REGION` | Depends | AWS backends only | Region for SQS and CodeArtifact. |
| `AWS_ENDPOINT_URL_SQS` / `AWS_ENDPOINT_URL` | No |  | SQS endpoint override (AWS-SDK-standard). Point at a local emulator (`ministack`) or VPC endpoint; with one set, requests are signed with `AWS_ACCESS_KEY_ID`/`AWS_SECRET_ACCESS_KEY`. Unset ⇒ normal AWS resolution. |
| `ECLUSE_GOOGLE_PROJECT` | Depends | GCP backends only | Project for Pub/Sub and Artifact Registry (credentials via ADC). |
| `ECLUSE_AUTH_TOKEN` | No |  | If set, clients must present this token (`Bearer` / `_authToken`). Omit for network-secured deployments. |
| `ECLUSE_MOUNTS__NPM__RESPECT_UPSTREAM_TARBALL_HOST` | No | `false` | Secure default. When `false`, a tarball is fetched only from the **same allowlisted upstream that served the packument**; set `true` only for a registry that serves tarballs from a separate CDN/files host (widens the fetch surface to any allowlisted host). See [Securing network egress](#securing-network-egress-required). |
| `ECLUSE_HELP_MESSAGE` | No |  | String appended to every denial message (e.g. a support channel). |
| `ECLUSE_LOG_FORMAT` | No | `json` | Log shape: `json` (one JSON object per line, for log collectors) or `console` (human-readable). |
| `ECLUSE_TELEMETRY` | No | `off` | OpenTelemetry master switch. With it `off`, no telemetry is emitted. When `on`, the SDK reads the standard `OTEL_*` variables. |
| `ECLUSE_CVE_SYNC_INTERVAL` | Depends | Pilot only, default `3600` | How often the Écluse Pilot singleton refreshes the OSV database from upstream. |
| `ECLUSE_OSV_DATA_DIR` | No | `data/osv` | Directory path for the Pilot OSV advisory database. |
| `ECLUSE_SHUTDOWN_DRAIN_TIMEOUT` | No | `30` | Seconds the graceful shutdown waits for in-flight requests and in-progress artifact streams to finish before the process exits. Positive integer. |
| `ECLUSE_CORES` | No | derived | Cores (GHC capabilities) the process claims. Unset ⇒ derived from the container's cgroup CPU quota (rounded up, clamped to the visible processors); with no cgroup limit either, the runtime's own detection stands. The boot log prints the decision and its provenance. Positive integer. See [Operating Écluse → Runtime sizing](#operating-écluse). |
| `ECLUSE_MAX_HEAP_BYTES` | No | derived | Heap ceiling in bytes, enforced by the GHC runtime (a breach is a clean heap-overflow error rather than a kernel OOM kill). Unset ⇒ derived from the cgroup memory limit less the nursery budget and 10% slack; with no cgroup limit, unbounded unless your own `GHCRTS -M` says otherwise. Enforcing a ceiling re-executes the binary once, in place (same PID). Positive integer. |
| `ECLUSE_SERVE_MAX_IN_FLIGHT` | No | computed | Process-wide cap on concurrent metadata materialisation: whole packument requests and the public-metadata gate reached by a tarball miss. **Unset, the capacity is computed at boot from the resolved core count** (`max(8, 4 x cores)`, sized to keep the cores busy through one upstream round trip per request) and logged with its provenance; set it only to override that. Work beyond the cap is rejected immediately with `503 Service Unavailable` and `Retry-After: 1`; it is not placed in an application queue. Trusted private tarball hits stream outside the cap, as do health probes and cheap local routes. Positive integer. **Note**: operators deploying Écluse within an orchestration mesh (e.g., Istio) can configure the mesh to automatically manage retries upon receiving this 503 response. **Alerting note**: 503s returned with a `Retry-After: 1` header are a normal operational response indicating safe, intentional backpressure. Operational alerts on 503s should strictly exclude responses with this header, as true proxy or upstream failures (e.g., the public npm registry is down) will return a 503 *without* the `Retry-After: 1` header. |
| `ECLUSE_PUBLIC_CONNECTIONS_PER_HOST` | No | `10` | Maximum concurrent pooled connections to each public upstream host. Public metadata misses are single-flight-coalesced, so the default keeps the upstream library's conservative per-host bound. Positive integer. (The **private** upstream pool has no knob: it always equals the effective `ECLUSE_SERVE_MAX_IN_FLIGHT`, because private reads are per-request and never coalesced, so demand on that pool is exactly the admission capacity.) |
| `ECLUSE_CACHE_TTL` | No | `60` | Seconds metadata is kept in the shared packument cache. |
| `ECLUSE_CACHE_MAX_ENTRIES` | No | `500` | Maximum number of items the metadata cache will hold. |
| `ECLUSE_CACHE_MAX_BYTES` | No | `52428800`, 50 MiB | Maximum total byte size for the metadata cache. |
| `ECLUSE_MAX_RESPONSE_BYTES` | No | `12582912`, 12 MiB | Largest upstream **metadata** body buffered before the fetch aborts fail-closed. Bounds memory against a hostile upstream returning a giant body. Positive integer. |
| `ECLUSE_MAX_VERSION_COUNT` | No | `100000` | Largest version count a packument may carry before it is refused. Bounds per-version rule evaluation against a version flood. Positive integer. |
| `ECLUSE_MAX_NESTING_DEPTH` | No | `64` | Deepest JSON nesting a decoded upstream document may reach before it is refused. Bounds CPU/stack against a pathologically nested payload. Positive integer. |
| `ECLUSE_MIN_PUBLIC_INTEGRITY` | No | `sha256` | Minimum integrity algorithm a **public** (untrusted) version's digest must meet to be served: `sha256`, `sha384`, `sha512`, or `blake2b`. A public version whose strongest digest is weaker (e.g. a legacy SHA-1 `shasum` only) is refused with a `403`. **Hard-floored at SHA-256**, `sha1`/`md5`/an unknown name is rejected at startup. The trusted private path has its own, loosenable floor (`ECLUSE_MIN_TRUSTED_INTEGRITY`). |
| `ECLUSE_MIN_TRUSTED_INTEGRITY` | No | `sha256` | Minimum integrity algorithm a **trusted** (private) version's digest must meet to be served. Defaults to `sha256`, so by default a SHA-1-only or hashless private version is dropped, exactly like a public one, but unlike the public floor is **loosenable below SHA-256**: `sha1`/`md5` are accepted for a legacy private mirror, where trust substitutes for cryptographic strength. An unknown name is still rejected at load. |

Configuration is **validated in full at startup, and the process refuses to start on any
problem**: an unknown rule type, a bad URL, an unresolved policy reference. A
misconfiguration is a loud, immediate failure, never a quietly mis-enforced policy.

> ⚠️ **The first-party publish surface authorises _names_, not _callers_.** If you enable
> publishing (`ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET`), the `ECLUSE_MOUNTS__NPM__PUBLISH_SCOPES` allow-list limits **which
> package names** may be published; it is **not** authentication and says nothing about
> **who** may publish. So a static `ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN` (Écluse's own credential, used
> only when a publisher forwards none) is **fail-closed**: set it without `ECLUSE_AUTH_TOKEN`
> and Écluse **refuses to start** (`PublishStaticCredentialNeedsEdge`), making "static
> publish credential + open edge", which would otherwise let **any unauthenticated client
> publish** under the operator's credential, an unrepresentable state rather than a footgun.
> `ECLUSE_AUTH_TOKEN` is the verifiable edge Écluse can check itself; an external layer
> (gateway, service mesh / mTLS, network policy) is good defence-in-depth but does **not**
> satisfy this requirement. Pure **passthrough** (no static token, the default) needs none
> of this: the publisher's own forwarded token is the authority. See
> [Access model → Publishing](docs/architecture/access-model.md#publishing-the-publication-target-passthrough-write).

### The configuration document

Supplied as a YAML **file** (the reviewable source of truth) mounted at `/etc/ecluse/config.yaml`. It carries the **rule policy** (see [Rule policy](#rule-policy)) and, for
multi-mount deployments, the **mount map**. Single-mount deployments desugar from the
environment variables above and need no document. Schema and examples:
[Configuration & Authentication](docs/architecture/configuration.md#configuration).

The default baseline configuration, from which all deployments derive their initial policy and behaviour, is defined below.

[View the default baseline configuration (`config/default.yaml`)](config/default.yaml)

### Secrets

**Secrets never live in the configuration document.** Client and registry tokens are always
environment variables, and cloud-managed registries (CodeArtifact / Artifact Registry)
derive **short-lived** tokens from ambient cloud credentials, keeping long-lived secrets out
of config entirely. Écluse always holds a mirror-target **write** credential; how _reads_
are credentialled is the mount's
[credential strategy](docs/architecture/access-model.md): the default `passthrough` forwards
the _client's_ own token to the private upstream (and strips it before the public one),
while `service` reads with Écluse's own credential. See
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
3. **Trusted edge identity**: a fronting gateway / IAP / mesh asserts a verified identity
   Écluse trusts. Écluse honours the assertion **only over a verifiable binding to that
   edge** (mutual TLS from the edge, or a shared secret / HMAC on the asserted identity), and **refuses to start** a `trusted-edge` mount configured with neither. A bare trusted
   header is forgeable into access wherever the proxy is reachable other than through the
   edge, so restrict reachability to the edge **east-west as well as north-south**.
   Validating cloud IAM at the npm edge directly stays a gateway concern (the npm client
   can't speak it; let the managed mirror target enforce write IAM).

## Securing network egress (required)

Écluse makes outbound requests to the registries you point it at (that's its job), and some
of the URLs it follows (a version's `dist.tarball`) are taken from upstream responses. As
with any service that fetches on a client's behalf, the sensible posture is
**least-privilege egress**, in two layers. Écluse provides the first in the application
itself, with an **origin-aware trust model**:

- **Untrusted origins**: the public-upstream fetch and every artifact (`dist.tarball`)
  fetch from an untrusted origin go through a host **allowlist**, an **internal-address block** (loopback,
  link-local incl. the `169.254.169.254` metadata endpoint, the unspecified
  `0.0.0.0/8` / `::` range, RFC1918, CGNAT, and IPv6 ULA `fc00::/7` incl.
  `fd00:ec2::254`) **re-applied to every resolved IP** at connection time (so an
  allowlisted name that resolves to an internal address is refused, a DNS-rebinding
  backstop), a **disallow-by-default `dist.tarball` host policy** (below), and
  **response-size bounds**.
- **The trusted private origin**, your operator-configured `ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM`, is
  deliberately _not_ subject to the internal-address block: a private registry
  legitimately lives on your internal network, so Écluse has to be able to reach it.

Crucially, **SSRF access to the instance-metadata endpoint is prevented at the
service-behaviour level, not by blocking metadata at the network.** Écluse only follows
internal-resolving locations on the _trusted_ private origin, never on a client- or
upstream-influenced one, so an attacker can't steer it at `169.254.169.254`. Écluse itself
**needs** the metadata endpoint to mint its instance-role credentials (`AWS.newEnv
AWS.discover`, over amazonka's own HTTP client, independent of the guarded data-plane path),
so do **not** deny the proxy egress to metadata or to internal ranges: that would break its
own credentials.

You provide the second layer at the platform: the standard defence-in-depth for an
outbound-fetching service, protecting your **data targets** (registries, mirror) and
catching anything the application layer doesn't:

- **Require IMDSv2 and set the hop limit to 1** (AWS `httpPutResponseHopLimit: 1`).
  This is the right metadata hardening: it keeps the proxy's _own_ credential
  minting working while stopping a containerised neighbour or a forwarded request
  from reaching metadata through extra hops. **Do not** deny the instance egress to
  `169.254.169.254` outright; Écluse needs it for credentials.
- **Default-deny egress, allow only your registries + mirror target.**
  - **AWS**: security-group egress rules / network ACLs to the upstream and
    mirror CIDRs (plus the metadata endpoint the instance role needs).
  - **GCP**: VPC firewall egress rules and, where applicable, VPC Service Controls.
  - **Kubernetes**: a default-deny `NetworkPolicy` with an explicit egress
    allowlist (enforced by your CNI); allow your private upstream's internal range.
  - **Service mesh (Istio/Linkerd)**: set the sidecar outbound policy to
    `REGISTRY_ONLY`, declare each upstream as a `ServiceEntry`, and constrain it
    with a `Sidecar` egress listener and an egress `AuthorizationPolicy`.
- **Grant the proxy only the cloud permissions it needs**: the mirror-write
  credential (and, under the `service` strategy, the
  private-read credential), nothing more.

**The `dist.tarball` host policy.** A version's `dist.tarball` is upstream-chosen data, so
by default Écluse fetches a tarball only from the **same allowlisted upstream that served the
packument**: a `dist.tarball` pointing at a _different_ host is refused even if that host is
otherwise on the allowlist. If your registry legitimately serves artifacts from a separate
CDN/files host (the PyPI-files-host shape), set `ECLUSE_MOUNTS__NPM__RESPECT_UPSTREAM_TARBALL_HOST=true` to
relax this to _any allowlisted host_; it never escapes the allowlist or the internal-range
block, but it does widen the fetch surface, so opt in deliberately and pair it with the
platform egress controls above.

The rationale (and why both the application guards and the platform controls are worth
having) is in [Security: Outbound-Request & Input-Validation Invariants](docs/architecture/security.md#network-egress-is-a-shared-responsibility).

### Securing Écluse Pilot & Dredger Services

If you deploy the auxiliary services (the **Écluse Pilot** ingestion pipeline and the **Écluse Dredger** reaper), they require distinct, tightly scoped network configurations. Additionally, **both services must be deployed as singletons (one replica only)**.

- **Écluse Pilot (Singleton)**: Requires **no public ingress**. It requires egress to `osv.dev` public endpoints (to fetch raw vulnerability data), the cloud instance-metadata endpoint (to mint container credentials), and your configured object store (S3/GCS) with `s3:PutObject` permissions to upload the processed `osv.db`. Running multiple instances will cause overlapping writes and corrupted OSV databases.
- **Écluse Dredger (Singleton)**: Requires **no public ingress**. It requires egress _only_ to your private mirror (Registry B) to issue delete requests, and to the instance-metadata endpoint for credentials. It has a standing high-privilege delete capability, so isolating it from all untrusted networks is critical. Running multiple instances will result in conflicting delete sweeps and rate-limit exhaustion against your registry.

## Locking down CI egress (recommended)

The controls above secure Écluse's _own_ outbound path. This one is about your _consumers'_,
and it's the step that turns Écluse from a proxy clients are _asked_ to use into the registry
they _can only_ reach.

If you control your CI environment, **deny CI runners outbound access to the public
registries** (`registry.npmjs.org`, and the equivalents for other ecosystems) and let them
reach **only Écluse** and your own internal services. Point the runners' package managers at
Écluse as their registry.

The result is safe-by-default behaviour. A job that's misconfigured (a stray `--registry`
flag, a committed `.npmrc` pointing at the public registry, a tool that ignores the settings
you shipped) doesn't quietly bypass the policy: it simply **can't reach the public registry,
so it fails** instead of pulling an unvetted package. You stop depending on every job being
configured correctly, and depend only on the network, which you administer centrally.

This is what makes the deny-by-default policy _unbypassable_ rather than merely _default_.
Per-project package-manager and version-manager setups (npm/pnpm config, nvm, Nix shells,
containers) can each override what you ship to a machine, but none of them can route around a
network that only reaches Écluse. See
[MOTIVATION → The bar](MOTIVATION.md#the-bar-a-chokepoint-you-cant-step-around) for why this
is the layer that holds.

The same idea can extend to developer workstations (for example, allowing tarball fetches
only through Écluse on a managed or zero-trust network while leaving registry browsing and
search open), though workstations are usually a softer control than CI.

## Rule policy

Écluse evaluates a **named map of rules** over a built-in **default policy**,
**deny-by-default**: a package is admitted only if a rule takes an allow position, and at
equal precedence deny wins. The shipped default is deliberately small and biased toward
resilience rather than blanket bans:

- **`min-age`**: admit public versions older than a quarantine window (7 days by default),
  the core defence against race-to-publish typosquatting and dependency confusion. **On at
  launch.**
- **`remediation-fast-track`**: admit a release that fixes a known CVE immediately, ahead of
  the quarantine. **On once the CVE tier lands.**
- **`revoke`**: a hard-deny (`DenyByIdentity`) rule to deny a specific package or `package@version`, at a precedence above the scope allow-list. **Available.**

You override values, add rules (e.g. opt into `DenyInstallTimeExecution`), or suppress a
default by name in the configuration document:

```json
{
  "rules": {
    "min-age": { "ageSeconds": 1209600 },
    "deny-scripts": { "type": "DenyInstallTimeExecution", "precedence": 200 },
    "revoke-bad": { "type": "DenyByIdentity", "identity": "bad-package" }
  }
}
```

Full semantics (precedence, the patch/add/suppress merge, and the strict validation) are in
[Rule policy](docs/architecture/configuration.md#rule-policy) and
[Rules Engine](docs/architecture/rules-engine.md).

### Always-on: a public version must carry a strong integrity digest

Independent of the configurable rules above, Écluse enforces one **non-negotiable admission
policy** on **public** (untrusted) upstreams: a version is served only if its `dist` carries
at least one integrity digest whose algorithm meets the **integrity floor**
(`ECLUSE_MIN_PUBLIC_INTEGRITY`, default **SHA-256**). A public version whose strongest digest
is **absent** or **below the floor**, for example only a legacy SHA-1 `shasum`, with no
`sha256`/`sha512` SRI `integrity`, is **inadmissible**:

- requesting its tarball returns a **`403`** (the artifact is never fetched), and
- it's **filtered out of the served packument listing**, so a client never sees a version it
  couldn't safely fetch.

SHA-1 and MD5 have practical collisions, so admitting a weak-or-absent digest could let a
substituted artifact pass undetected. The floor may be **raised** (`sha512`,
`blake2b`) but never set below SHA-256; a sub-floor value is rejected at startup. The **private** (trusted) path is governed by its own floor (`ECLUSE_MIN_TRUSTED_INTEGRITY`). It defaults to `sha256`, the **same** secure default as the public floor, so by default a SHA-1-only or hashless private version is **dropped**. Unlike the public floor, it is **loosenable below SHA-256** (`sha1`/`md5`) for a legacy private mirror, where trust substitutes for cryptographic strength.

**Gotcha.** If a custom or off-spec public upstream serves versions without a digest meeting
the floor (no `integrity`, or only a legacy `shasum`), those versions silently disappear from
what Écluse serves and a direct fetch `403`s. This is deliberate. If you genuinely need to
serve such a source, point it at the **private** (trusted) upstream slot, not the public one,
**and loosen** `ECLUSE_MIN_TRUSTED_INTEGRITY` below `sha256`. See
[Security Policy](SECURITY.md#a-public-version-must-carry-an-integrity-digest).

## Operating Écluse

- **Pre-warming the cache.** A cold-start on a large monorepo (e.g., running `npm install` against a completely cold proxy cache) can hit the proxy with dozens of simultaneous heavy package requests, which may cause latency spikes or trigger `503 Service Unavailable` backpressure. We strongly recommend pre-warming the cache as part of your deployment process by running an automated `npm install` (or a script fetching your known heavy dependencies) after starting Écluse, before directing production CI traffic to it. Once the cache is warm, Écluse's request coalescing will effortlessly absorb large traffic spikes.
- **Health probes.** `GET /livez` reports process liveness (a stalled mirror worker fails
  it); `GET /readyz` reports that config is loaded and the listener is serving. Readiness is
  deliberately lenient about public-upstream reachability so a transient upstream blip
  doesn't pull a healthy pod from rotation. The npm liveness probe `GET /-/ping` is answered
  locally with `200 {}`. **Pilot and Dredger** export identical `/livez` and `/readyz` probes on the same `ECLUSE_PORT`, allowing unified readiness checks across all container roles.
- **Logs.** Structured, one JSON object per line by default (`ECLUSE_LOG_FORMAT=json`) for
  stdout log-collector autodiscovery, or `console` for local development. Bearer tokens are
  carried as a redacted type whose rendering is a placeholder, so token material never reaches
  a log field.
- **Telemetry (opt-in).** OpenTelemetry traces and metrics are **off by default**; set
  `ECLUSE_TELEMETRY=on` to enable them. Identity and endpoint are **self-aligning across
  dialects**: set the `DD_*` variables (`DD_SERVICE`, `DD_ENV`, `DD_VERSION`, `DD_AGENT_HOST`)
  if you run Datadog, or the standard `OTEL_*` ones for any other backend, the `DD_*` form
  wins where both are present, and the same resolved identity stamps both your traces and the
  `dd` object on every log line. `DD_API_KEY`/`DD_SITE` are deliberately ignored: Écluse only
  ever exports to a node-local collector or Agent.
  - **You declare the destination.** Export goes to `http://localhost:4318` by default, or
    wherever you point `DD_AGENT_HOST`/`OTEL_EXPORTER_OTLP_ENDPOINT`, a node-local collector
    or Agent in the usual deployment. The endpoint is yours to declare (like the mirror
    queue), so Écluse does not gate it; for a remote collector, authenticate out of band with
    `OTEL_EXPORTER_OTLP_HEADERS`.
  - **Never on the request path.** Export is asynchronous and batched, so an unreachable
    collector never slows or fails a served request; an absent endpoint logs one boot warning
    and falls back to localhost, and persistent export errors are logged once and then
    throttled to a periodic heartbeat rather than flooding your logs.
- **Search.** `GET /-/v1/search` returns `501` by design: search is a discovery convenience,
  not an install path. Use the public registry's website to discover packages.
- **Runtime sizing (cores and memory).** At boot Écluse resolves how many cores to claim
  and what heap ceiling to run under, and logs each decision with its provenance
  (`runtime: capabilities 2 (derived from the cgroup limit)` and friends), so the
  effective posture is always readable from the start-up lines. Resolution order, per
  knob:
  1. **Explicit configuration wins**: `ECLUSE_CORES` (or `cores` in the config document)
     and `ECLUSE_MAX_HEAP_BYTES` (`maxHeapBytes`), both positive integers.
  2. **Omitted values are derived from the container's cgroup (v2)**: the CPU quota,
     rounded up and clamped to the visible processors, and the memory limit less the
     nursery budget (cores x allocation area) less 10% slack, floored at half the limit.
  3. **No limit found either way**: whatever the GHC runtime resolved (its baked
     defaults plus any `GHCRTS` you set) stands, and the log says so. A `GHCRTS` heap
     ceiling you set yourself is never overridden by derivation.

  This exists because a container CPU **limit** is a cgroup quota that does not shrink
  the processor count the runtime sees: without it, a 2-CPU pod on a 32-core node claims
  32 capabilities and sizes 32 nurseries. Applying a heap ceiling requires runtime flags
  that cannot change after start, so when one must be enforced Écluse **re-executes its
  own binary once, in place** (same PID, no exit observed by the container runtime),
  logging `runtime: re-launching with GHCRTS ...` first.
- **Runtime memory arithmetic (sizing a proxy pod).** This modelling is for the
  **proxy** role; the other container roles have different shapes (Pilot performs a
  single scheduled ingestion computation, and the Dredger's profile will follow its
  pruning rules as they land), so do not carry the proxy's arithmetic over to them --
  the cores/heap **resolution** above still applies to every role (those derive from
  the container, not the workload), but tune their allocation area independently via
  `GHCRTS` if you run them in tight pods. The binary ships with a GC configuration
  tuned for the serve path's allocation profile (`-A64m -n4m`: a 64 MiB per-core
  allocation area, shared in 4 MiB chunks). This trades a bounded amount of extra memory
  for an order-of-magnitude fewer garbage collections under load. Budget roughly:
  `cores x 64 MiB` of nursery, plus the live heap (dominated by the metadata cache),
  plus up to one live-heap's worth of copying headroom during a major collection. Worked
  shapes: a 2-CPU / 512 MiB pod runs comfortably as-is (128 MiB nursery, ~330 MiB derived
  heap ceiling); for a 2-CPU / 256 MiB pod also shrink the allocation area
  (`GHCRTS="-A16m"`); a 4-CPU pod is comfortable from ~750 MiB with the shipped defaults,
  or at 512 MiB with `-A32m`. Taller pods amortise the metadata cache and request
  coalescing better than more small pods, so prefer 4-CPU-ish shapes when in doubt. The
  allocation area itself is deliberately not config-surfaced; tune it with `GHCRTS`
  (e.g. `GHCRTS="-A16m"`), and the boot log always prints the effective value.
- **Revoking a mirrored version (internal yank).** The mirror store (Registry B) deliberately
  resists upstream yanks. A benign yank (e.g., a maintainer rage-deletes, a name dispute) does not
  break your installs. The flip side is that a version _later found malicious_ is not removed
  automatically, and Écluse never re-gates trusted content. The **typical case resolves itself**: the public registry yanks or security-holds the bad version, its bytes change or vanish,
  re-mirroring can no longer reproduce them, and you purge the stale copy from Registry B at your
  leisure. For the **atypical case where your own scanning is ahead of the public yank**, revoke in
  this order: **(1)** deny the identity (the `DenyByIdentity` revocation rule) so the serve path stops admitting it and the worker stops re-mirroring it, then **(2)**
  purge that version from Registry B to remove the already-mirrored copy. **Order matters:** purge
  alone is a treadmill; while the version is still live upstream, the next install re-admits and
  re-mirrors it.

## Planned controls

Documented here so the configuration surface and its security trade-off are known ahead of
implementation. Écluse's posture is **secure by default, with overrides under your explicit
control: you decide your threat tolerance.**

- **GCP backends**: the Pub/Sub `MirrorQueue` and the ADC credential leaf. The AWS backends, the SQS `MirrorQueue`, the CodeArtifact credential leaf, the mirror worker, and the
  composition root that wires them into a config-driven deployment, are **built and wired**;
  the GCP equivalents are **planned**.
- **Effectful CVE rules**: `DenyIfCVE` / `AllowIfRemediatesCve` over a local OSV advisory
  index (**planned**).

The full deployment runbook ships with the launch.

## Learn more

The internal design, for when you need the _why_:

- [Architecture overview](docs/architecture.md)
- [Configuration & Authentication](docs/architecture/configuration.md)
- [Security invariants & network egress](docs/architecture/security.md)
- [Threat model](https://alexadewit.github.io/Ecluse/threat-model.html), the STRIDE register, generated from the OWASP Threat Dragon model ([`threat-modelling/ecluse.json`](threat-modelling/ecluse.json))
- [Rules engine](docs/architecture/rules-engine.md)
- [Multi-ecosystem hosting & URL rewriting](docs/architecture/hosting.md)
- [Release & supply-chain operations](docs/architecture/release-supply-chain.md)
