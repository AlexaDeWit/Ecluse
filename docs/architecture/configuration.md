# Configuration & Authentication

> Part of the [Écluse architecture overview](../architecture.md).

## Configuration

> **Operators:** the deployment-facing reference — the environment-variable table,
> client setup, and the network-egress checklist — lives in the
> [Operator Manual (`USAGE.md`)](../../USAGE.md). This document is the *design
> rationale* behind those settings; keep the two in sync.

Configuration has two layers: a small set of **environment variables** for
process-level and secret values, and a **structured config document** carrying the
two things too expressive for flat env vars — the **rule policy** and the **mount
map**.

Of the two, the **rule policy is what earns the document its keep**: a set of rules
with per-rule precedence and value overrides, layered over a built-in default (see
[Rule policy](#rule-policy)). **Mounts are comparatively flat** — three registry
endpoints and a queue backend, under a prefix
[derived from the ecosystem](hosting.md#mounts), not configured — so the
**single-ecosystem environment variables (below) desugar to a one-entry mount map**,
and the common launch case (one npm mount on the default policy) needs no document
at all. Multi-ecosystem deployments (see
[Multi-Ecosystem Hosting](hosting.md#multi-ecosystem-hosting)) declare each
ecosystem's registries in the document's `mounts` object, **keyed by ecosystem name**
(`npm`, `pypi`) — the path prefix is derived from that key, never declared, so a
wrong or colliding prefix is unrepresentable.

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
| `MIRROR_TARGET_TOKEN` | No | Static write token for the mirror target, used when it is reached with a fixed credential rather than a cloud-managed one. |
| `PUBLICATION_TARGET_URL` | No | URL the proxy writes client `npm publish` (first-party packages) to. **Unset ⇒ the proxy refuses publishes with `405`.** May be the same registry as the private upstream (so published packages are then readable via the private leg). See [Registry roles → publication target](registry-model.md#publishing-first-party-packages-the-publication-target). |
| `PUBLICATION_TARGET_TOKEN` | No | Static credential for the publication target when it is not reached with the client's forwarded token. The default publish credential model is **passthrough** — the publisher's own token; see [Access model](access-model.md#publishing-the-publication-target-passthrough-write). |
| `PUBLISH_SCOPES` | Required when `PUBLICATION_TARGET_URL` is set | Comma-separated allow-list of package scopes a client may publish (e.g. `@acme`). A publish whose name is outside the list is refused — the anti-shadowing guard against publishing a name that collides with a public package. |
| `MIRROR_QUEUE_PROVIDER` | No (default: `sqs`) | Mirror-queue backend: `sqs` (AWS) or `pubsub` (GCP). See [Cloud Backends](cloud-backends.md#cloud-backends). |
| `MIRROR_QUEUE_URL` | Yes | Queue identifier for mirror jobs: an SQS queue URL, or a Pub/Sub `projects/<project>/topics/<topic>` resource, per provider. |
| `AWS_REGION` | AWS backends only | Region for SQS and CodeArtifact. |
| `GOOGLE_CLOUD_PROJECT` | GCP backends only | Project for Pub/Sub and Artifact Registry. Credentials come from Application Default Credentials (ADC). |
| `PROXY_AUTH_TOKEN` | No | If set, clients must supply this token as `Bearer` or `_authToken`. Omit for open/network-secured deployments. |
| `PROXY_RESPECT_UPSTREAM_TARBALL_HOST` | No (default: `false`) | When `false`, a tarball is fetched only from the same allowlisted upstream that served the packument; a `dist.tarball` pointing at a different host is refused. See [Outbound egress safety](#outbound-egress-safety). |
| `PROXY_HELP_MESSAGE` | No | Custom string appended to all denial messages (e.g. `"Contact #platform-eng on Slack for assistance."`). |
| `PROXY_LOG_FORMAT` | No (default: `json`) | Structured-log output shape: `json` (one object per line, for log collectors) or `console` (human-readable). See [Observability](observability.md). |
| `CVE_SYNC_INTERVAL_SECONDS` | No (default: 3600) | How often to refresh the in-memory advisory index from OSV (see [CVE Subsystem](rules-engine.md#cve-subsystem)). |
| `PROXY_CONFIG` | No | The structured config document as an inline JSON blob, the alternate to a mounted config file for an env-only deployment. |

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

Écluse always holds a credential for one thing — writing to the **mirror target** —
and, depending on the mount's [credential strategy](access-model.md), may also hold
one for **reading** the private upstream. Each such endpoint selects a
[`CredentialProvider`](cloud-backends.md#credential-provider) — **cloud-managed**
(CodeArtifact / Artifact Registry, token derived from the ambient cloud
credentials above: `AWS_REGION` / instance role, or ADC / `GOOGLE_CLOUD_PROJECT`)
or a static token.

**How reads are credentialled is the credential strategy** (see
[Access & Credential Model](access-model.md)). Under the default **`passthrough`**,
reads carry **no Écluse credential**: the private upstream receives the **client's**
forwarded token (it is the authority for reads) and the public upstream is queried
anonymously with the client's token **stripped**. Under **`service`** — and a
**service-populated `delegated-cache`** — Écluse reads the private upstream with its
**own** `CredentialProvider` token; a **caller-populated `delegated-cache`** keeps
forwarding the client's token, so it needs no read credential. (What lets the private
origin be *cached* is the strategy's serve-time authorisation — the edge or a probe —
not the populate; see [Access & Credential Model → Caching](access-model.md#caching).)
The public upstream is anonymous under every strategy — and the client's token is
**never** forwarded there. (If a public mirror itself requires auth, set a separate
`PUBLIC_UPSTREAM_TOKEN` — Écluse's own, never the client's.) Minting these
credentials from a cloud identity keeps long-lived secrets out of config.

### Outbound egress safety

Écluse constrains its own outbound fetches (host allowlist + internal-range block,
**re-applied to every resolved IP** at connection time; see
[Security Invariants](security.md)), but **network egress is a shared
responsibility** — the deployment must also fence egress at the platform layer
(security groups, `NetworkPolicy`, Istio `ServiceEntry`/egress policy, and blocking
the `169.254.169.254` metadata endpoint). See
[Network egress is a shared responsibility](security.md#network-egress-is-a-shared-responsibility).

The one application-level knob, following Écluse's **secure-defaults /
configurable-overrides** principle — *the consumer decides their threat tolerance*:

| Variable | Default | Description |
|----------|---------|-------------|
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
mount**; a multi-ecosystem deployment may additionally give an individual
ecosystem's mount its own [refinement](hosting.md#mounts) that merges over it (e.g.
a stricter policy on npm than on PyPI).

```json
{
  "rules": {
    "min-age":      { "ageSeconds": 1209600 },
    "deny-scripts": { "type": "DenyInstallTimeExecution", "precedence": 200 }
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

Deliberately **not** in the default: `DenyInstallTimeExecution` (plenty of legitimate
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
  (`DenyInstallTimeExecutio` vs `DenyInstallTimeExecution`) would vanish and stop
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
- **Credential references must resolve.** A mount whose
  [credential strategy](access-model.md) draws on a provider the deployment has not
  initialized — e.g. a `service` (or service-populated `delegated-cache`) mount with
  no read provider, or a mirror target naming a backend whose ambient cloud identity
  is absent — is **rejected at boot**. Credential providers are
  [process-global](cloud-backends.md#credential-provider) and a mount only references
  one, so an incompatible reference never reaches a request.

A bad config is thus a loud, immediate startup failure an operator sees and fixes,
never a quietly mis-enforced policy.

## Client Authentication

This section covers **inbound** auth (client → proxy) — the **edge authentication**
half of the [Access & Credential Model](access-model.md). How the *upstreams* are
then credentialled (forward the client token, or use Écluse's own) is the mount's
[credential strategy](access-model.md#credential-strategies-per-mount), covered there
and under [Outbound Registry Credentials](#outbound-registry-credentials); the one
invariant that holds regardless is that the client's credential is **never** sent to
the public upstream.

Edge authentication is **optional**. The modes (full rationale, including the npm
client's constraints, in [access-model](access-model.md#edge-authentication)):

1. **Open** — `PROXY_AUTH_TOKEN` is unset. Any client can reach the proxy.
   Access control is delegated entirely to the network layer (VPC, service mesh,
   etc.).
2. **Static token** — `PROXY_AUTH_TOKEN` is set. Clients must include it as
   `Bearer <token>` in the `Authorization` header or as `_authToken` in
   `.npmrc`. Standard npm tooling supports this out of the box.
3. **Trusted edge identity** — a fronting authenticating proxy / cloud IAP / service
   mesh performs SSO or mTLS and asserts a verified identity Écluse trusts — sound
   only where Écluse is reachable *exclusively* through that edge. Validating cloud
   IAM at the npm edge directly is out (the npm client cannot speak it); it stays a
   gateway concern, and a managed registry can independently enforce write IAM on the
   mirror target.
