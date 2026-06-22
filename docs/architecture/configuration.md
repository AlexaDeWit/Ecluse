# Configuration & Authentication

> Part of the [Écluse architecture overview](../architecture.md).

## Configuration

Configuration has two layers: a small set of **environment variables** for
process-level and secret values, and a **structured config document** carrying the
two things too expressive for flat env vars — the **rule policy** and the **mount
map**.

Of the two, the **rule policy is what earns the document its keep**: a set of rules
with per-rule precedence and value overrides, layered over a built-in default (see
[Rule policy](#rule-policy)). **Mounts are comparatively flat** — a prefix, a base
URL, three registry endpoints, a queue backend — so the **single-mount environment
variables (below) desugar to a one-entry mount map**, and the common launch case
(one npm mount on the default policy) needs no document at all. Multi-mount
deployments (see [Multi-Ecosystem Hosting](hosting.md#multi-ecosystem-hosting))
name their mounts in the document.

The document is supplied in one of two forms:

- a **config file** (JSON) — the source of truth: reviewable, diffable, the
  expected form once the rule policy is non-trivial; or
- a **JSON blob in an env var** (e.g. `PROXY_CONFIG`) — the same schema, an
  alternate for an env-only deployment with no mounted file.

JSON keeps one schema across both forms with no extra dependency; a YAML reader
over the same schema may be added later for comments/ergonomics.

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
| `PROXY_HELP_MESSAGE` | No | Custom string appended to all denial messages (e.g. `"Contact #platform-eng on Slack for assistance."`). |
| `CVE_SYNC_INTERVAL_SECONDS` | No (default: 3600) | How often to refresh the in-memory advisory index from OSV (see [CVE Subsystem](rules-engine.md#cve-subsystem)). |

### Upstream composition (optional)

`PRIVATE_UPSTREAM_URL` may point at a single registry **or** at one that itself
aggregates others — e.g. an AWS CodeArtifact repository with upstream
relationships to a mirror-target repo and a first-party "published-by-us" repo, so
one fetch returns the whole trusted set. This is a supported topology but **never
required**: Écluse
[merges packuments across upstreams](registry-model.md#packument-merge-across-upstreams)
itself, so registry-level composition is an optimization, not a precondition. The
one rule that keeps it safe — the aggregator must **not** add a direct external
connection to the public registry (that would route unvetted packages around the
gate); the public upstream is always fetched and gated by Écluse.

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

### Outbound egress safety (planned)

> **Design only — not yet a live setting.** Recorded here so the configuration
> surface and its security trade-off are agreed before implementation
> ([`S40`](../../planning/slices/S40-egress-ssrf-hardening.md)).

Écluse constrains its own outbound fetches (host allowlist + internal-range block;
see [Security Invariants](security.md)), but **network egress is a shared
responsibility** — the deployment must also fence egress at the platform layer
(security groups, `NetworkPolicy`, Istio `ServiceEntry`/egress policy, and blocking
the `169.254.169.254` metadata endpoint). See
[Network egress is a shared responsibility](security.md#network-egress-is-a-shared-responsibility).

The one application-level knob, following Écluse's **secure-defaults /
configurable-overrides** principle — *the consumer decides their threat tolerance*:

| Variable (planned) | Default | Description |
|--------------------|---------|-------------|
| `PROXY_RESPECT_UPSTREAM_TARBALL_HOST` | `false` (secure default) | When `false`, a tarball is fetched only from the **same allowlisted upstream that served the packument**; a `dist.tarball` pointing at a *different* host is refused. Set `true` only for a registry that legitimately serves tarballs from a separate CDN/files host (e.g. the PyPI files host), which **widens the outbound fetch surface to any allowlisted host** — opt in deliberately, and pair it with platform egress controls. |

The override never escapes the host allowlist or the internal-range block: it
relaxes *which allowlisted host* may serve a tarball, not whether the allowlist
applies. The default keeps the tightest reading of
[invariant 2](security.md#invariants).

### Rule policy

The rule policy is a **named map** of rules layered over a **built-in default
policy** that ships with the binary. An entry whose name the default already
defines is a **patch** onto it (override precedence and/or values); an entry with a
**new** name must carry a full `type` (it **adds** a rule); and any entry may set
`"enabled": false` to **suppress** a default rule. With no rule config supplied at
all, the default policy applies unchanged. This top-level policy applies to **every
mount**; a multi-mount deployment may additionally give an individual mount its own
[refinement](hosting.md#mounts) that merges over it (the `/npm-prod` vs
`/npm-canary` case).

```json
{
  "rules": {
    "min-age":      { "ageSeconds": 1209600 },
    "deny-scripts": { "type": "DenyHasInstallScripts", "precedence": 200 }
  }
}
```

- `min-age` names a **default** rule, so this **overrides** its value (a 14-day
  window instead of 7); unspecified fields keep the default.
- `deny-scripts` is a new name carrying a `type`, so it **adds** a rule (here,
  opting into the install-script deny that is *not* on by default).
- a `"<name>": { "enabled": false }` entry **suppresses** the named default rule.

Each rule may set an integer `precedence` (higher wins); omit it to use the rule
type's default. Evaluation picks the **highest-precedence rule that takes a
position** (allow or deny); at equal precedence, deny wins; if every rule
abstains, the package is denied by default. See
[Rules Engine → Evaluation model](rules-engine.md#evaluation-model).

#### The default policy

The shipped default is deliberately small and **opinionated toward resilience, not
blanket bans** — a floor to extend, not a wall:

| Default rule (name) | Rule | Status | Why |
|---|---|---|---|
| `min-age` | `AllowIfPublishedBefore` (7 days) | **On at launch** | Admit public versions that have survived a quarantine window — the core defence against race-to-publish typosquatting and dependency confusion. |
| `remediation-fast-track` | `AllowIfRemediatesCve` | **On once the [CVE tier](rules-engine.md#cve-subsystem) lands** | Ranked **above** `min-age` so a release that fixes a known CVE is admitted **immediately** — a quarantine must never delay a security patch (see [Rules Engine](rules-engine.md#allowifremediatescve--remediation-fast-track)). |

Deliberately **not** in the default: `DenyHasInstallScripts` (plenty of legitimate
packages ship install scripts — a blanket ban is too blunt for a default) and
`DenyIfCVE` (blanket-denying every advisory-affected version can break installs of
widely-used packages over low-severity advisories). Both remain **available rules**
an operator opts into by name.

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
- **Merge references must resolve.** A `rules` entry that neither names a known
  default nor supplies a complete new rule — a typo'd default name, an
  `"enabled": false` against a rule that does not exist, a patch missing the `type`
  it would need to stand alone — is **rejected**. You cannot silently suppress or
  mistype a rule out of existence.

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
