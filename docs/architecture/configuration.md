# Configuration & Authentication

> Part of the [Écluse architecture overview](../architecture.md).

## Configuration

Configuration has two layers: a small set of **environment variables** for
process-level and secret values, and **structured config** describing the
mount(s).

The **mount map** — each mount's prefix, externally-visible base URL,
three-registry endpoints with their credential providers, queue backend, and rule
set — is supplied as structured config in one of two forms:

- a **config file** (JSON) — the source of truth: reviewable, diffable, and the
  expected form as the mount count grows; or
- a **JSON blob in an env var** (e.g. `PROXY_CONFIG`) — the same schema, an
  alternate for consumers who want an env-only deployment with no mounted file.

JSON keeps one schema across both forms with no extra dependency; a YAML reader
over the same schema may be added later for comments/ergonomics.

The **single-mount environment variables** below are a **shorthand** that desugars
to a one-entry mount map — the common case at launch (one npm mount). Multi-mount
deployments (see [Multi-Ecosystem Hosting](hosting.md#multi-ecosystem-hosting))
use the file or JSON-blob form.

**Secrets never live in the structured config.** Tokens (`PROXY_AUTH_TOKEN`,
per-endpoint registry tokens) are always environment variables; cloud-managed
registries derive short-lived tokens from ambient cloud credentials (see
[Outbound Registry Credentials](#outbound-registry-credentials)).

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
| `CVE_SYNC_INTERVAL_SECONDS` | No (default: 3600) | How often to refresh the in-memory advisory index from OSV (see [CVE Subsystem](rules-engine.md#cve-subsystem)). |

### Outbound Registry Credentials

Écluse holds a credential for exactly **one** thing: writing to the **mirror
target**. That endpoint selects a
[`CredentialProvider`](cloud-backends.md#credential-provider) — **cloud-managed**
(CodeArtifact / Artifact Registry, token derived from the ambient cloud
credentials above: `AWS_REGION` / instance role, or ADC / `GOOGLE_CLOUD_PROJECT`)
or a static `MIRROR_TARGET_TOKEN`.

**Reads carry no Écluse credential.** The private upstream receives the
**client's** forwarded token (it is the authority for reads), and the public
upstream is queried anonymously with the client's token **stripped** — see
[Credential flow and authority](registry-model.md#credential-flow-and-authority).
(If a public mirror itself requires auth, set a separate `PUBLIC_UPSTREAM_TOKEN` —
Écluse's own, never the client's.) Minting the mirror-write credential from a
cloud identity also keeps long-lived secrets out of config.

### Rule Configuration Format

```json
[
  { "type": "AllowScope",             "scope": "@myorg",    "precedence": 300 },
  { "type": "DenyHasInstallScripts",                        "precedence": 200 },
  { "type": "AllowIfPublishedBefore", "ageSeconds": 604800, "precedence": 100 }
]
```

Each rule may set an integer `precedence` (higher wins); omit it to use the rule
type's default. Evaluation picks the **highest-precedence rule that takes a
position** (allow or deny); at equal precedence, deny wins; if every rule
abstains, the package is denied by default. See
[Rules Engine → Evaluation model](rules-engine.md#evaluation-model).

### Validation: fail fast, reject the unknown

Config is **validated in full at startup, and the process refuses to start on any
problem** — it never runs in a degraded or partially-applied state. Errors are
**aggregated** (as `envparse` does for env vars) so one run reports every issue,
not just the first.

Crucially, **unknown is an error, not a silent skip**:

- An unknown rule `type` is **rejected, not ignored**. Silently dropping a
  misspelled rule is a security hole: a typo'd **deny** rule
  (`DenyHasInstallScript` vs `DenyHasInstallScripts`) would vanish and stop
  blocking, and a typo'd **allow** rule would over-deny. Deny-by-default only
  protects you if the policy you wrote is the policy that loaded.
- **Unknown fields/keys are rejected** too — config is operator-authored
  alongside the binary, so forward-compat tolerance buys little and costs
  typo-catching; the decoders are strict rather than aeson's permissive default.
- **Malformed values** (bad URL, non-integer precedence, unparseable JSON) fail
  the same way.

A bad config is thus a loud, immediate startup failure an operator sees and fixes,
never a quietly mis-enforced policy.

## Client Authentication

This section covers **inbound** auth (client → proxy). Outbound credentials differ
by direction: the client's credential is **forwarded to the private upstream** (the
authority for reads) and **never to the public upstream**, while Écluse's own
[`CredentialProvider`](cloud-backends.md#credential-provider) is used **only** to
write to the mirror target — see
[Credential flow and authority](registry-model.md#credential-flow-and-authority).

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
