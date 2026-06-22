# Using Écluse — Operator Manual

This is the **operator-facing manual** for deploying and running Écluse: how to
configure it, connect your clients, and — importantly — how to fence its network
egress so it stays a safe link in your supply chain. It is the consumer companion
to the internal [architecture documents](docs/architecture.md), which explain the
*why* behind everything here.

> **Status: pre-launch.** Écluse is under active development. This manual is the
> **configuration and operational contract** — the env vars, the config schema,
> the client setup, and the security responsibilities — documented as they
> stabilise. Features still landing are marked **(planned)** with a pointer to the
> tracking slice; the end-to-end request path is tracked in the
> [delivery plan](planning/delivery-plan.md). Treat this as the deployment
> contract, not a claim that every capability below is wired today.

## Contents

- [What Écluse does](#what-écluse-does)
- [Deployment model](#deployment-model)
- [Configuration](#configuration)
  - [Environment variables](#environment-variables)
  - [The configuration document](#the-configuration-document)
  - [Secrets](#secrets)
- [Connecting your clients](#connecting-your-clients)
- [Securing network egress (required)](#securing-network-egress-required)
- [Rule policy](#rule-policy)
- [Operating Écluse](#operating-écluse)
- [Planned controls](#planned-controls)
- [Learn more](#learn-more)

## What Écluse does

Écluse sits between your build (developer machine or CI) and the npm registry and
enforces a **deny-by-default resilience policy** before any package reaches a
build. It reads through a **private upstream** first, falls back to the **public**
registry with rules applied, and mirrors approved packages asynchronously — it is
a policy gate, **not** a registry, and hosts nothing itself. The design is in
[`docs/architecture.md`](docs/architecture.md).

## Deployment model

Écluse ships as a single, reproducible container image that runs **one process**:
the HTTP front door (a raw-`wai` application on `PROXY_PORT`, default `4873`) and,
alongside it, the mirror worker. Point your package manager at it as a registry
(see [Connecting your clients](#connecting-your-clients)).

Before you run a published image, **verify its provenance and SBOM attestations** —
the recipe (keyless Sigstore + Rekor, pinned by digest) is in the
[README](README.md#verifying-the-image).

## Configuration

Configuration has two layers: **environment variables** for process-level and
secret values, and an optional **structured config document** for the two things
too expressive for flat env vars — the rule policy and the mount map. The common
single-mount npm deployment on the default policy needs **no document at all**.

The authoritative semantics, validation rules, and rationale live in
[Configuration & Authentication](docs/architecture/configuration.md); this section
is the operator reference. **Keep the two in sync** when either changes.

### Environment variables

| Variable | Required | Description |
|----------|----------|-------------|
| `PROXY_PORT` | No (default `4873`) | TCP port the proxy listens on. |
| `PRIVATE_UPSTREAM_URL` | **Yes** | URL of the private upstream registry (the authority for reads). |
| `PUBLIC_UPSTREAM_URL` | No (default `https://registry.npmjs.org`) | URL of the public upstream, queried anonymously and gated by the rules. |
| `PUBLIC_UPSTREAM_TOKEN` | No | Écluse's own token for a public mirror that itself requires auth. Never the client's. |
| `MIRROR_TARGET_URL` | **Yes** | Registry that approved packages are mirrored to. |
| `MIRROR_TARGET_TOKEN` | No | Static write token for the mirror target, when not using a cloud-managed credential. |
| `MIRROR_QUEUE_PROVIDER` | No (default `sqs`) | Mirror-queue backend: `sqs` (AWS) or `pubsub` (GCP). **(planned backends)** |
| `MIRROR_QUEUE_URL` | **Yes** | Queue identifier: an SQS queue URL or a Pub/Sub `projects/<p>/topics/<t>` resource. |
| `AWS_REGION` | AWS backends only | Region for SQS and CodeArtifact. |
| `GOOGLE_CLOUD_PROJECT` | GCP backends only | Project for Pub/Sub and Artifact Registry (credentials via ADC). |
| `PROXY_AUTH_TOKEN` | No | If set, clients must present this token (`Bearer` / `_authToken`). Omit for network-secured deployments. |
| `PROXY_HELP_MESSAGE` | No | String appended to every denial message (e.g. a support channel). |
| `PROXY_LOG_FORMAT` | No (default `json`) | Log shape: `json` (one JSON object per line, for log collectors) or `console` (human-readable). |
| `CVE_SYNC_INTERVAL_SECONDS` | No (default `3600`) | How often the in-memory advisory index refreshes from OSV. **(with the CVE tier)** |
| `PROXY_CONFIG` | No | The configuration document as an inline JSON blob, for an env-only deployment with no mounted file. |

Configuration is **validated in full at startup and the process refuses to start
on any problem** — an unknown rule type, a bad URL, an unresolved policy
reference. A misconfiguration is a loud, immediate failure, never a quietly
mis-enforced policy.

### The configuration document

Supplied either as a JSON **file** (the reviewable source of truth) or inline via
`PROXY_CONFIG`. It carries the **rule policy** (see [Rule policy](#rule-policy))
and, for multi-mount deployments, the **mount map**. Single-mount deployments
desugar from the environment variables above and need no document. Schema and
examples: [Configuration & Authentication](docs/architecture/configuration.md#configuration).

### Secrets

**Secrets never live in the configuration document.** Client and registry tokens
are always environment variables, and cloud-managed mirror targets (CodeArtifact /
Artifact Registry) derive **short-lived** tokens from ambient cloud credentials,
keeping long-lived secrets out of config entirely. Écluse holds a credential for
exactly one thing — writing to the mirror target; reads forward the *client's* own
token to the private upstream and strip it before the public one. See
[Outbound Registry Credentials](docs/architecture/configuration.md#outbound-registry-credentials).

## Connecting your clients

Point your package manager at the proxy as its registry. With
`PROXY_AUTH_TOKEN` set, supply it the standard npm way:

```ini
# .npmrc
registry=https://ecluse.example.internal/
//ecluse.example.internal/:_authToken=${ECLUSE_TOKEN}
```

Authentication to the proxy has three modes:

1. **Open** — `PROXY_AUTH_TOKEN` unset; access control is delegated entirely to
   the network layer (VPC, service mesh). Appropriate only on a closed network.
2. **Static token** — `PROXY_AUTH_TOKEN` set; clients send it as
   `Authorization: Bearer <token>` or `.npmrc` `_authToken`.
3. **Cloud IAM** — deferred as a gateway concern (validate at the edge / let the
   managed mirror target enforce write IAM).

## Securing network egress (required)

Écluse's built-in outbound guards — a host **allowlist**, an **internal-address
block** (loopback, link-local incl. the `169.254.169.254` metadata endpoint, the
unspecified `0.0.0.0/8` / `::` range, RFC1918, and CGNAT), and **response-size
bounds** — are an application-layer **backstop, not a substitute** for fencing
egress at the platform. A proxy sits in a privileged network position; a guard bug
or an unforeseen fetch path must not be able to become an SSRF into your cloud
control plane. **You are responsible for constraining where the proxy can reach.**
At minimum:

- **Block the instance-metadata endpoint.** Require IMDSv2 and set the hop limit
  to 1 (AWS `httpPutResponseHopLimit: 1`), or deny egress to `169.254.169.254`
  outright. This removes the single highest-value SSRF target.
- **Default-deny egress, allow only your registries + mirror target.**
  - **AWS** — security-group egress rules / network ACLs to the upstream and
    mirror CIDRs; deny RFC1918 and link-local.
  - **GCP** — VPC firewall egress rules and, where applicable, VPC Service Controls.
  - **Kubernetes** — a default-deny `NetworkPolicy` with an explicit egress
    allowlist (enforced by your CNI).
  - **Service mesh (Istio/Linkerd)** — set the sidecar outbound policy to
    `REGISTRY_ONLY`, declare each upstream as a `ServiceEntry`, and constrain it
    with a `Sidecar` egress listener and an egress `AuthorizationPolicy`.
- **Grant the proxy only the cloud permissions it needs** — the mirror-write
  credential, nothing more.

The reasoning behind these — and why the application guards alone are not enough —
is in [Security: Outbound-Request & Input-Validation Invariants](docs/architecture/security.md#network-egress-is-a-shared-responsibility).

## Rule policy

Écluse evaluates a **named map of rules** over a built-in **default policy**,
**deny-by-default**: a package is admitted only if a rule takes an allow position,
and at equal precedence deny wins. The shipped default is deliberately small and
biased toward resilience rather than blanket bans:

- **`min-age`** — admit public versions older than a quarantine window (7 days by
  default), the core defence against race-to-publish typosquatting and dependency
  confusion. **On at launch.**
- **`remediation-fast-track`** — admit a release that fixes a known CVE
  immediately, ahead of the quarantine. **On once the CVE tier lands.**

You override values, add rules (e.g. opt into `DenyHasInstallScripts`), or suppress
a default by name in the configuration document:

```json
{
  "rules": {
    "min-age":      { "ageSeconds": 1209600 },
    "deny-scripts": { "type": "DenyHasInstallScripts", "precedence": 200 }
  }
}
```

Full semantics — precedence, the patch/add/suppress merge, and the strict
validation — are in [Rule policy](docs/architecture/configuration.md#rule-policy)
and [Rules Engine](docs/architecture/rules-engine.md).

## Operating Écluse

- **Health probes.** `GET /livez` reports process liveness (a stalled mirror
  worker fails it); `GET /readyz` reports that config is loaded and the listener
  is serving. Readiness is deliberately lenient about public-upstream reachability
  so a transient upstream blip does not pull a healthy pod from rotation. The
  npm liveness probe `GET /-/ping` is answered locally with `200 {}`.
- **Logs.** Structured, one JSON object per line by default (`PROXY_LOG_FORMAT=json`)
  for stdout log-collector autodiscovery, or `console` for local development.
  Bearer tokens are carried as a redacted type whose rendering is a placeholder,
  so token material never reaches a log field.
- **Search.** `GET /-/v1/search` returns `501` by design — search is a discovery
  convenience, not an install path. Use the public registry's website to discover
  packages.

## Planned controls

Documented here so the configuration surface and its security trade-off are known
ahead of implementation. Écluse's posture is **secure by default, with overrides
under your explicit control — you decide your threat tolerance.**

- **`PROXY_RESPECT_UPSTREAM_TARBALL_HOST`** *(planned —
  [S40](planning/slices/S40-egress-ssrf-hardening.md))*. By default a tarball is
  fetched only from the **same allowlisted upstream that served the packument**;
  set `true` only for a registry that legitimately serves tarballs from a separate
  CDN/files host, which **widens the outbound fetch surface** to any allowlisted
  host. Pair the override with the egress controls above. See
  [Outbound egress safety](docs/architecture/configuration.md#outbound-egress-safety-planned).
- **AWS / GCP backends and the mirror worker** — the SQS/Pub-Sub queues, the
  CodeArtifact / ADC credential leaves, and the demand-driven mirror are landing
  per the [delivery plan](planning/delivery-plan.md) (milestones M4, M7).
- **Effectful CVE rules** — `DenyIfCVE` / `AllowIfRemediatesCve` over a local OSV
  advisory index (milestone M5).

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
