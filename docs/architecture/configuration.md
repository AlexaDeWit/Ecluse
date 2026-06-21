# Configuration & Authentication

> Part of the [Écluse architecture overview](../architecture.md).

## Configuration

Runtime configuration is provided entirely via environment variables. Rule sets
and other structured config are supplied as JSON strings.

The variables below configure a **single mount**. A multi-mount deployment (see
[Multi-Ecosystem Hosting](hosting.md#multi-ecosystem-hosting)) supplies the
equivalent set per mount via structured config, keyed by path prefix; the
single-mount variables are the one-entry degenerate form.

| Variable | Required | Description |
|----------|----------|-------------|
| `PROXY_PORT` | No (default: 4873) | Port the proxy listens on. |
| `PRIVATE_UPSTREAM_URL` | Yes | URL of the private upstream registry. |
| `PUBLIC_UPSTREAM_URL` | No (default: `https://registry.npmjs.org`) | URL of the public upstream. |
| `MIRROR_TARGET_URL` | Yes | URL of the registry to mirror approved packages to. |
| `MIRROR_QUEUE_PROVIDER` | No (default: `sqs`) | Mirror-queue backend: `sqs` (AWS) or `pubsub` (GCP). See [Cloud Backends](cloud-backends.md#cloud-backends). |
| `MIRROR_QUEUE_URL` | Yes | Queue identifier for mirror jobs: an SQS queue URL, or a Pub/Sub `projects/<project>/topics/<topic>` resource, per provider. |
| `AWS_REGION` | AWS backends only | Region for SQS and CodeArtifact. |
| `GOOGLE_CLOUD_PROJECT` | GCP backends only | Project for Pub/Sub and Artifact Registry. Credentials come from Application Default Credentials (ADC). |
| `PROXY_AUTH_TOKEN` | No | If set, clients must supply this token as `Bearer` or `_authToken`. Omit for open/network-secured deployments. |
| `PROXY_RULES` | Yes | JSON array of rule objects defining the allow policy (see below). |
| `PROXY_HELP_MESSAGE` | No | Custom string appended to all denial messages (e.g. `"Contact #platform-eng on Slack for assistance."`). |
| `CVE_CACHE_TTL_SECONDS` | No (default: 3600) | How long to cache advisory lookup results. |

### Outbound Registry Credentials

Each registry endpoint selects a
[`CredentialProvider`](cloud-backends.md#credential-provider). A
**cloud-managed** endpoint (its URL host identifies CodeArtifact or Artifact
Registry) derives its token from the ambient cloud credentials already configured
above (`AWS_REGION` / instance role, or ADC / `GOOGLE_CLOUD_PROJECT`) — no secret
is placed in Écluse's own config. A **plain** registry takes an optional static
token per endpoint (e.g. `PRIVATE_UPSTREAM_TOKEN`, `MIRROR_TARGET_TOKEN`); absent
one, the endpoint is treated as anonymous. The public upstream is anonymous by
default. This keeps long-lived registry secrets out of config wherever a cloud
identity can mint a short-lived token instead.

### Rule Configuration Format

```json
[
  { "type": "AllowScope",              "scope": "@myorg" },
  { "type": "AllowIfPublishedBefore",  "ageSeconds": 604800 },
  { "type": "DenyHasInstallScripts" }
]
```

The whole set is evaluated with deny precedence: any matching deny rule blocks
the package, otherwise the first matching allow rule wins; if none is decisive,
the package is denied by default.

## Client Authentication

This section covers **inbound** auth (client → proxy). **Outbound** auth
(proxy → registry) is a separate concern, handled by the
[`CredentialProvider`](cloud-backends.md#credential-provider) seam.

Authentication to the proxy is **optional**. Three modes:

1. **Open** — `PROXY_AUTH_TOKEN` is unset. Any client can reach the proxy.
   Access control is delegated entirely to the network layer (VPC, service mesh,
   etc.).
2. **Static token** — `PROXY_AUTH_TOKEN` is set. Clients must include it as
   `Bearer <token>` in the `Authorization` header or as `_authToken` in
   `.npmrc`. Standard npm tooling supports this out of the box.
3. **Cloud IAM (future)** — Validating cloud identity (AWS IAM / GCP IAM) at the
   proxy edge is deferred as a gateway concern. A managed registry (CodeArtifact /
   Artifact Registry) can be the mirror target with cloud IAM controlling writes
   independently.
