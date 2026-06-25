# Using Écluse: Operator Manual

This is the **operator-facing manual** for deploying and running Écluse: how to configure
it, connect your clients, and, importantly, how to fence its network egress so it stays a
safe link in your supply chain. It's the consumer companion to the internal
[architecture documents](docs/architecture.md), which explain the *why* behind everything
here.

> **Status: pre-launch.** Écluse is under active development. This manual is the
> **configuration and operational contract**: the env vars, the config schema, the client
> setup, and the security responsibilities, documented as they stabilise. Features still
> landing are marked **(planned)** with a pointer to the tracking slice; the end-to-end
> request path is tracked in the [delivery plan](planning/delivery-plan.md). Treat this as
> the deployment contract, not a claim that every capability below is wired today.

## Contents

- [What Écluse does](#what-écluse-does)
- [Deployment model](#deployment-model)
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

Écluse ships as a single, reproducible container image that runs **one process**: the HTTP
front door (a raw-`wai` application on `PROXY_PORT`, default `4873`) and, alongside it, the
mirror worker. Point your package manager at it as a registry (see
[Connecting your clients](#connecting-your-clients)).

Before you run a published image, **verify its provenance and SBOM attestations**: the
recipe (keyless Sigstore + Rekor, pinned by digest) is in the
[README](README.md#verifying-the-image).

## Configuration

Configuration has two layers: **environment variables** for process-level and secret
values, and an optional **structured config document** for the two things too expressive for
flat env vars, namely the rule policy and the mount map. The common single-mount npm
deployment on the default policy needs **no document at all**.

The authoritative semantics, validation rules, and rationale live in
[Configuration & Authentication](docs/architecture/configuration.md); this section is the
operator reference. **Keep the two in sync** when either changes.

### Environment variables

| Variable | Required | Description |
|----------|----------|-------------|
| `PROXY_PORT` | No (default `4873`) | TCP port the proxy listens on. |
| `PRIVATE_UPSTREAM_URL` | **Yes** | URL of the private upstream registry (the authority for reads under the default `passthrough` strategy). |
| `PUBLIC_UPSTREAM_URL` | No (default `https://registry.npmjs.org`) | URL of the public upstream, queried anonymously and gated by the rules. |
| `PROXY_PUBLIC_URL` | Recommended | The proxy's own externally-reachable base URL (e.g. `https://registry.example.com`), used to rewrite each served `dist.tarball` to an **absolute** URL clients fetch back through the proxy. **Unset, tarball URLs are path-relative, which the `npm` CLI cannot install from** — it reads a leading-slash `dist.tarball` as a local `file:` path — so set this for any deployment that serves real `npm install`s. |
| `PUBLIC_UPSTREAM_TOKEN` | No | Écluse's own token for a public mirror that itself requires auth. Never the client's. |
| `MIRROR_TARGET_URL` | **Yes** | Registry that approved packages are mirrored to. |
| `MIRROR_TARGET_TOKEN` | No | Static write token for the mirror target, when not using a cloud-managed credential. |
| `MIRROR_QUEUE_PROVIDER` | No (default `sqs`) | Mirror-queue backend: `sqs` (AWS) or `pubsub` (GCP). **(planned backends)** |
| `MIRROR_QUEUE_URL` | **Yes** | Queue identifier: an SQS queue URL or a Pub/Sub `projects/<p>/topics/<t>` resource. |
| `AWS_REGION` | AWS backends only | Region for SQS and CodeArtifact. |
| `GOOGLE_CLOUD_PROJECT` | GCP backends only | Project for Pub/Sub and Artifact Registry (credentials via ADC). |
| `PROXY_AUTH_TOKEN` | No | If set, clients must present this token (`Bearer` / `_authToken`). Omit for network-secured deployments. |
| `PROXY_RESPECT_UPSTREAM_TARBALL_HOST` | No (default `false`) | Secure default. When `false`, a tarball is fetched only from the **same allowlisted upstream that served the packument**; set `true` only for a registry that serves tarballs from a separate CDN/files host (widens the fetch surface to any allowlisted host). See [Securing network egress](#securing-network-egress-required). |
| `PROXY_HELP_MESSAGE` | No | String appended to every denial message (e.g. a support channel). |
| `PROXY_LOG_FORMAT` | No (default `json`) | Log shape: `json` (one JSON object per line, for log collectors) or `console` (human-readable). |
| `CVE_SYNC_INTERVAL_SECONDS` | No (default `3600`) | How often the in-memory advisory index refreshes from OSV. **(with the CVE tier)** |
| `PROXY_MAX_RESPONSE_BYTES` | No (default `16777216`, 16 MiB) | Largest upstream **metadata** body buffered before the fetch aborts fail-closed. Bounds memory against a hostile upstream returning a giant body. Positive integer. |
| `PROXY_MAX_VERSION_COUNT` | No (default `100000`) | Largest version count a packument may carry before it is refused. Bounds per-version rule evaluation against a version flood. Positive integer. |
| `PROXY_MAX_NESTING_DEPTH` | No (default `64`) | Deepest JSON nesting a decoded upstream document may reach before it is refused. Bounds CPU/stack against a pathologically nested payload. Positive integer. |
| `PROXY_CONFIG` | No | The configuration document as an inline JSON blob, for an env-only deployment with no mounted file. |

Configuration is **validated in full at startup, and the process refuses to start on any
problem**: an unknown rule type, a bad URL, an unresolved policy reference. A
misconfiguration is a loud, immediate failure, never a quietly mis-enforced policy.

### The configuration document

Supplied either as a JSON **file** (the reviewable source of truth) or inline via
`PROXY_CONFIG`. It carries the **rule policy** (see [Rule policy](#rule-policy)) and, for
multi-mount deployments, the **mount map**. Single-mount deployments desugar from the
environment variables above and need no document. Schema and examples:
[Configuration & Authentication](docs/architecture/configuration.md#configuration).

### Secrets

**Secrets never live in the configuration document.** Client and registry tokens are always
environment variables, and cloud-managed registries (CodeArtifact / Artifact Registry)
derive **short-lived** tokens from ambient cloud credentials, keeping long-lived secrets out
of config entirely. Écluse always holds a mirror-target **write** credential; how *reads*
are credentialled is the mount's
[credential strategy](docs/architecture/access-model.md): the default `passthrough` forwards
the *client's* own token to the private upstream (and strips it before the public one),
while `service` / `delegated-cache` read with Écluse's own credential. See
[Outbound Registry Credentials](docs/architecture/configuration.md#outbound-registry-credentials).

## Connecting your clients

Point your package manager at the proxy as its registry. With `PROXY_AUTH_TOKEN` set, supply
it the standard npm way:

```ini
# .npmrc
registry=https://ecluse.example.internal/
//ecluse.example.internal/:_authToken=${ECLUSE_TOKEN}
```

Edge authentication to the proxy has three modes (and feeds the mount's
[credential strategy](docs/architecture/access-model.md), which decides how the upstreams
are then credentialled):

1. **Open**: `PROXY_AUTH_TOKEN` unset; access control is delegated entirely to the network
   layer (VPC, service mesh). Appropriate only on a closed network.
2. **Static token**: `PROXY_AUTH_TOKEN` set; clients send it as
   `Authorization: Bearer <token>` or `.npmrc` `_authToken`.
3. **Trusted edge identity**: a fronting gateway / IAP / mesh asserts a verified identity
   Écluse trusts, sound only where Écluse is reachable solely through that edge. Validating
   cloud IAM at the npm edge directly stays a gateway concern (the npm client can't speak it;
   let the managed mirror target enforce write IAM).

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
- **The trusted private origin**, your operator-configured `PRIVATE_UPSTREAM_URL`, is
  deliberately *not* subject to the internal-address block: a private registry
  legitimately lives on your internal network, so Écluse has to be able to reach it.

Crucially, **SSRF access to the instance-metadata endpoint is prevented at the
service-behaviour level, not by blocking metadata at the network.** Écluse only follows
internal-resolving locations on the *trusted* private origin, never on a client- or
upstream-influenced one, so an attacker can't steer it at `169.254.169.254`. Écluse itself
**needs** the metadata endpoint to mint its instance-role credentials (`AWS.newEnv
AWS.discover`, over amazonka's own HTTP client, independent of the guarded data-plane path),
so do **not** deny the proxy egress to metadata or to internal ranges: that would break its
own credentials.

You provide the second layer at the platform: the standard defence-in-depth for an
outbound-fetching service, protecting your **data targets** (registries, mirror) and
catching anything the application layer doesn't:

- **Require IMDSv2 and set the hop limit to 1** (AWS `httpPutResponseHopLimit: 1`).
  This is the right metadata hardening: it keeps the proxy's *own* credential
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
  credential (and, under the `service` / `delegated-cache` strategies, the
  private-read credential), nothing more.

**The `dist.tarball` host policy.** A version's `dist.tarball` is upstream-chosen data, so
by default Écluse fetches a tarball only from the **same allowlisted upstream that served the
packument**: a `dist.tarball` pointing at a *different* host is refused even if that host is
otherwise on the allowlist. If your registry legitimately serves artifacts from a separate
CDN/files host (the PyPI-files-host shape), set `PROXY_RESPECT_UPSTREAM_TARBALL_HOST=true` to
relax this to *any allowlisted host*; it never escapes the allowlist or the internal-range
block, but it does widen the fetch surface, so opt in deliberately and pair it with the
platform egress controls above.

The rationale (and why both the application guards and the platform controls are worth
having) is in [Security: Outbound-Request & Input-Validation Invariants](docs/architecture/security.md#network-egress-is-a-shared-responsibility).

## Locking down CI egress (recommended)

The controls above secure Écluse's *own* outbound path. This one is about your *consumers'*,
and it's the step that turns Écluse from a proxy clients are *asked* to use into the registry
they *can only* reach.

If you control your CI environment, **deny CI runners outbound access to the public
registries** (`registry.npmjs.org`, and the equivalents for other ecosystems) and let them
reach **only Écluse** and your own internal services. Point the runners' package managers at
Écluse as their registry.

The result is safe-by-default behaviour. A job that's misconfigured (a stray `--registry`
flag, a committed `.npmrc` pointing at the public registry, a tool that ignores the settings
you shipped) doesn't quietly bypass the policy: it simply **can't reach the public registry,
so it fails** instead of pulling an unvetted package. You stop depending on every job being
configured correctly, and depend only on the network, which you administer centrally.

This is what makes the deny-by-default policy *unbypassable* rather than merely *default*.
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

You override values, add rules (e.g. opt into `DenyInstallTimeExecution`), or suppress a
default by name in the configuration document:

```json
{
  "rules": {
    "min-age":      { "ageSeconds": 1209600 },
    "deny-scripts": { "type": "DenyInstallTimeExecution", "precedence": 200 }
  }
}
```

Full semantics (precedence, the patch/add/suppress merge, and the strict validation) are in
[Rule policy](docs/architecture/configuration.md#rule-policy) and
[Rules Engine](docs/architecture/rules-engine.md).

### Always-on: a public version must carry an integrity digest

Independent of the configurable rules above, Écluse enforces one **non-negotiable admission
policy** on **public** (untrusted) upstreams: a version is served only if its `dist` carries
**at least one integrity digest**, an SRI `integrity` *or* a legacy `shasum`. A public
version with **neither** is **inadmissible**:

- requesting its tarball returns a **`403`** (the artifact is never fetched), and
- it's **filtered out of the served packument listing**, so a client never sees a version it
  couldn't safely fetch.

This closes a tamper-detection gap: a version with no integrity check can't be tied to a
fingerprint, so a divergence between two hashless copies would go undetected. The **private**
(trusted) upstream is **exempt**: its versions enter unfiltered, so a hashless private
version is still served.

**Gotcha.** If a custom or off-spec public upstream serves versions without
`integrity`/`shasum`, those versions silently disappear from what Écluse serves and a direct
fetch `403`s. This is deliberate. If you genuinely need to serve such a source, point it at
the **private** (trusted) upstream slot, not the public one. See
[Security Policy](SECURITY.md#a-public-version-must-carry-an-integrity-digest).

## Operating Écluse

- **Health probes.** `GET /livez` reports process liveness (a stalled mirror worker fails
  it); `GET /readyz` reports that config is loaded and the listener is serving. Readiness is
  deliberately lenient about public-upstream reachability so a transient upstream blip
  doesn't pull a healthy pod from rotation. The npm liveness probe `GET /-/ping` is answered
  locally with `200 {}`.
- **Logs.** Structured, one JSON object per line by default (`PROXY_LOG_FORMAT=json`) for
  stdout log-collector autodiscovery, or `console` for local development. Bearer tokens are
  carried as a redacted type whose rendering is a placeholder, so token material never reaches
  a log field.
- **Search.** `GET /-/v1/search` returns `501` by design: search is a discovery convenience,
  not an install path. Use the public registry's website to discover packages.

## Planned controls

Documented here so the configuration surface and its security trade-off are known ahead of
implementation. Écluse's posture is **secure by default, with overrides under your explicit
control: you decide your threat tolerance.**

- **AWS / GCP backends**: the mirror worker, the SQS `MirrorQueue`, and the CodeArtifact
  credential leaf are built behind their handles; the AWS composition root that wires them
  into a config-driven deployment, and the GCP backends (the Pub/Sub queue and the ADC
  credential leaf), are landing per the
  [delivery plan](planning/delivery-plan.md) (milestones M4, M7).
- **Effectful CVE rules**: `DenyIfCVE` / `AllowIfRemediatesCve` over a local OSV advisory
  index (milestone M5).

The full deployment runbook ships with the launch
([S32](planning/slices/S32-launch-docs.md)).

## Learn more

The internal design, for when you need the *why*:

- [Architecture overview](docs/architecture.md)
- [Configuration & Authentication](docs/architecture/configuration.md)
- [Security invariants & network egress](docs/architecture/security.md)
- [Rules engine](docs/architecture/rules-engine.md)
- [Multi-ecosystem hosting & URL rewriting](docs/architecture/hosting.md)
- [Release & supply-chain operations](docs/architecture/release-supply-chain.md)
