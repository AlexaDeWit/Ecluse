# Configuration & Authentication

> Part of the [Écluse architecture overview](../architecture.md).

## Configuration

> **Operators:** the deployment-facing reference, the environment-variable table,
> client setup, and the network-egress checklist, lives in the
> [Operator Manual (`USAGE.md`)](../../USAGE.md). This document is the *design
> rationale* behind those settings; keep the two in sync.

Configuration has two layers: a small set of **environment variables** for
process-level and secret values, and a **structured config document** carrying the
two things too expressive for flat env vars, the **rule policy** and the **mount
map**.

Of the two, the **rule policy is what earns the document its keep**: a set of rules
with per-rule precedence and value overrides, layered over a built-in default (see
[Rule policy](#rule-policy)). **Mounts are comparatively flat**, three registry
endpoints and a queue backend, under a prefix
[derived from the ecosystem](hosting.md#mounts), not configured, so the
**single-ecosystem environment variables (below) desugar to a one-entry mount map**,
and the common launch case (one npm mount on the default policy) needs no document
at all. Multi-ecosystem deployments (see
[Multi-Ecosystem Hosting](hosting.md#multi-ecosystem-hosting)) declare each
ecosystem's registries in the document's `mounts` object, **keyed by ecosystem name**
(`npm`, `pypi`), the path prefix is derived from that key, never declared, so a
wrong or colliding prefix is unrepresentable.

The document is supplied in one of two forms:

- a **config file** (YAML), the source of truth: reviewable, diffable, the
  expected form once the rule policy is non-trivial.

**Secrets never live in the structured config.** Tokens (`ECLUSE_AUTH_TOKEN`,
per-endpoint registry tokens) are always environment variables; cloud-managed
registries derive short-lived tokens from ambient cloud credentials (see
[Outbound Registry Credentials](#outbound-registry-credentials)).

> For the comprehensive list of environment variables, their defaults, and operator semantics, see the [Operator Manual (`USAGE.md`)](../../USAGE.md#environment-variables).

### Registry endpoints must be https

Every registry endpoint (`ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM`, `ECLUSE_MOUNTS__NPM__PUBLIC_UPSTREAM`,
`ECLUSE_MOUNTS__NPM__MIRROR_TARGET`, `ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET`) **must** be an `https://` URL. A plain-HTTP
registry endpoint is not a supported configuration: the proxy fails closed at boot with
an error naming the offending URL. Certificate validation is the endpoint-authentication
boundary, so a private registry that uses an **internal CA** is supported by extending the
container image with your own cert chain (add it to the image's system trust store); the
proxy does not pre-bake custom CA trust. A legacy upstream that advertises a plaintext
`dist.tarball` on its own host is upgraded to https automatically; a plaintext tarball on
any other host is dropped (the version is skipped, and the drop is recorded).

### Upstream composition (optional)

`ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM` may point at a single registry **or** at one that itself
aggregates others; e.g. an AWS CodeArtifact repository with upstream
relationships to a mirror-target repo and a first-party "published-by-us" repo, so
one fetch returns the whole trusted set. This is a supported topology but **never
required**: Écluse
[merges packuments across upstreams](registry-model.md#packument-merge-across-upstreams)
itself, so registry-level composition is an optimization, not a precondition. The
one rule that keeps it safe: the aggregator must **not** add a direct external
connection to the public registry (that would route unvetted packages around the
gate); the public upstream is always fetched and gated by Écluse.

### Outbound Registry Credentials

Écluse always holds a credential for one thing, writing to the **mirror target**,and, depending on the mount's [credential strategy](access-model.md), may also hold
one for **reading** the private upstream. Each such endpoint selects a
[`CredentialProvider`](cloud-backends.md#credential-provider), **cloud-managed**
(CodeArtifact / Artifact Registry, token derived from the ambient cloud
credentials above: `AWS_REGION` / instance role, or ADC / `ECLUSE_GOOGLE_PROJECT`)
or a static token.

**Selecting the mirror-target write provider.** `ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER`
chooses how that one always-held write credential is obtained, `static` (a
`ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN`) or `codeartifact` (a token minted under the ambient task role,
its domain/owner/region from the `ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_*` keys or parsed from the
mirror-target host). The write credential is **explicit and does not fold** when
`ECLUSE_MOUNTS__NPM__MIRROR_TARGET` folds onto the private upstream: under the default `passthrough`
the private upstream carries no Écluse credential, while the mirror write runs on the
async worker under Écluse's own identity, so the two are independent. (When the
`service` strategy later gives the private-upstream read its own Écluse credential, a
write to the same registry **may** inherit it; that is the `service` slice's concern,
not this one.) The read-side providers (`ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM_*`) and the publish-target
provider (`ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_*`) will follow the **same prefixed-provider pattern**
when those slices land, so the shape is set once here.

**How reads are credentialled is the credential strategy** (see
[Access & Credential Model](access-model.md)). Under the default **`passthrough`**,
reads carry **no Écluse credential**: the private upstream receives the **client's**
forwarded token (it is the authority for reads) and the public upstream is queried
anonymously with the client's token **stripped**. Under **`service`**, Écluse reads the private upstream with its
**own** `CredentialProvider` token, per-request and **never cached** (Écluse forbids a
shared private cache; see [Access & Credential Model → Caching](access-model.md#caching)).
The public upstream is anonymous under every strategy, and the client's token is
**never** forwarded there. The public-origin fetch is built with no token at all:
there is deliberately no Écluse credential for the public upstream. Minting these
credentials from a cloud identity keeps long-lived secrets out of config.

### Outbound egress safety

Écluse constrains its own outbound fetches (host allowlist + internal-range block,
**re-applied to every resolved IP** at connection time; see
[Security Invariants](security.md)), but **network egress is a shared
responsibility**: the deployment must also fence egress at the platform layer
(security groups, `NetworkPolicy`, Istio `ServiceEntry`/egress policy, and blocking
the `169.254.169.254` metadata endpoint). See
[Network egress is a shared responsibility](security.md#network-egress-is-a-shared-responsibility).

The one application-level knob, following Écluse's **secure-defaults /
configurable-overrides** principle, *the consumer decides their threat tolerance*. 
See the `ECLUSE_MOUNTS__*__RESPECT_UPSTREAM_TARBALL_HOST` setting in the [Operator Manual](../../USAGE.md#environment-variables) for how to relax this constraint.

The override never escapes the host allowlist or the internal-range block: it
relaxes *which allowlisted host* may serve a tarball, not whether the allowlist
applies. The default keeps the tightest reading of
[invariant 2](security.md#invariants).

### Response bounds

Écluse bounds what an upstream response may cost it ([invariant 4](security.md#invariants)):
a hostile or compromised upstream cannot exhaust the proxy with a multi-gigabyte
body, a version flood, or a deeply-nested JSON document. The bounds are enforced on
the **upstream→proxy** metadata path and **fail closed**, a document past any ceiling
is refused outright (the contribution degrades exactly as a parse failure does), never
partially served. They are independent of the client→proxy request-body cap, which
guards the other direction. Artifacts are streamed with constant memory and are not
subject to the body-size bound.

The defaults are generous for real registry documents and tight enough to fail closed
on pathological input; each is a strictly positive integer (a non-positive value is a
degenerate budget and is rejected at startup). For the specific variables (`ECLUSE_MAX_RESPONSE_BYTES`, `ECLUSE_MAX_VERSION_COUNT`, `ECLUSE_MAX_NESTING_DEPTH`), see the [Operator Manual](../../USAGE.md#environment-variables).

The metadata ceilings are layered. `ECLUSE_MAX_RESPONSE_BYTES` (default **12 MiB**,the largest packuments seen today are ~4 MiB, so this leaves years of headroom) is
the **primary, pre-decode** bound: the body is bounded as it streams, **before** the
JSON is decoded, so the parse spend is fixed before aeson runs and a hostile body is
aborted while still streaming. `ECLUSE_MAX_VERSION_COUNT`, checked **after** the
packument is projected, is a deliberate **defence-in-depth** semantic backstop behind
it, it bounds per-version work the byte cap already keeps finite, rather than a
streaming early-reject (the byte cap makes that unnecessary).

### Aggregate serve capacity

Per-response ceilings do not bound aggregate residency when many clients resolve
different packages concurrently. Écluse therefore admits at most
`ECLUSE_SERVE_MAX_IN_FLIGHT` metadata materialisations process-wide (a whole
packument request or the public-metadata gate after a private tarball miss). The
default is **computed at boot** from the resolved capability count,
`max(8, 4 x capabilities)`: an admitted materialisation alternates upstream wait
and CPU work, so keeping `C` capabilities busy wants about `C x (1 + W/P)` in
flight, roughly 4 per capability at realistic latency ratios. The decision is
logged with its provenance beside the runtime posture lines; set the key only to
override it. The **private upstream connection pool always equals the effective
admission capacity** (no separate key): private reads are per-request and never
coalesced, so demand on that pool is the admission cap, and a smaller pool would
not queue but pay a fresh TLS handshake per overflow request (http-client's pool
bound governs keep-alive retention, not concurrency). Work beyond the cap is shed
immediately with a
mount-shaped `503` and `Retry-After: 1`; there is no application queue whose memory
or latency can grow with client concurrency. Health probes and locally answered
routes bypass the bound so an overloaded instance remains observable. A trusted
private tarball hit also bypasses it and streams with its existing constant-memory
backpressure; admission protects resident metadata structures, not download count.

### Runtime sizing: cores and heap ceiling

`ECLUSE_CORES` and `ECLUSE_MAX_HEAP_BYTES` are the first-class surface for the
process's runtime posture; anything omitted is **derived from the container's
cgroup (v2)** in the `automaxprocs` style, and with no cgroup limit either the
GHC runtime's own resolution (its baked defaults plus any operator `GHCRTS`)
stands. Resolution is per knob, strongest first: config, then cgroup, then
runtime. The derivation reads the process's own cgroup and every ancestor
(tightest limit wins), so a limit placed on a parent slice still binds. Every
decision is logged at boot with its provenance, so the effective posture is
always readable from the standard logs.

Two mechanics are deliberate. A derived heap ceiling subtracts the nursery
budget (cores x allocation area) and 10% slack from the memory limit, floored
at half the limit, so the ceiling accounts for memory the process spends
outside the heap. And because a heap ceiling can only be set at runtime start,
enforcing one **re-executes the binary once, in place** (same PID; a container
supervisor observes an uninterrupted process), loop-guarded by an internal
marker. An operator's own `GHCRTS` is never fought: an explicit `-M` there is
adopted rather than overridden, and a divergence that survives the re-launch
is logged as a warning, never an abort. See the [Operator
Manual](../../USAGE.md#operating-écluse) for the sizing arithmetic.

The resolution is **role-agnostic on purpose, and only the resolution**: cores
and the heap ceiling derive from the container's limits, which bind the proxy,
Pilot, and Dredger alike. What is *not* universalised is workload-shaped memory
modelling: the shipped allocation-area tuning and the pod-sizing arithmetic are
the **proxy serve path's** profile, while Pilot (a single scheduled ingestion
pipe) and the Dredger (profile to follow its pruning rules) are tuned
per-deployment via `GHCRTS` until their shapes earn their own defaults.

The two process-lifetime HTTP managers also carry explicit per-host pool bounds.
The public default remains **10** connections per host because same-key metadata
misses are single-flight-coalesced. The private default is **16**, matching the
serve admission cap: private reads preserve per-client authority and cannot be
coalesced, so an admitted request is not made to wait behind a smaller implicit
pool. Operators may tune the three positive integer controls independently; the
admission cap remains the aggregate process-wide ceiling even when a deployment
raises a per-host pool.

### Public integrity floor

A **public** (untrusted) upstream's version is admitted only if its selected artifact
carries at least one integrity digest whose algorithm meets the **public integrity floor**
([invariant 5](security.md#invariants)). SHA-1 and MD5 have practical collisions, so a
match on one cannot prove an artifact was not substituted; a public version whose
strongest digest is below the floor is refused (`403`) and filtered from the served
listing. The trusted private path is governed by its own, **loosenable** floor
([Trusted integrity floor](#trusted-integrity-floor)), but the public floor here is
**never** loosenable.

`ECLUSE_MIN_PUBLIC_INTEGRITY` sets the floor. It may be **raised** as
cryptanalysis ages an algorithm, but is **hard-floored at SHA-256**, a value below it
or an unknown name is a configuration error rejected at load, never silently clamped, and
there is **no escape-hatch** to accept a sub-SHA-256 digest from an untrusted public
upstream. See the [Operator Manual](../../USAGE.md#environment-variables) for supported algorithms.

### Trusted integrity floor

A **trusted** (private) upstream's version is served only if its selected artifact carries
at least one integrity digest whose algorithm meets the **trusted integrity floor**
([invariant 5](security.md#invariants)). It defaults to `sha256`, the **same** secure
default as the public floor, so by default a SHA-1-only or hashless private version is
**dropped** (filtered from the served listing, and a private miss on the artifact path that
falls through to the public origin). The old "trusted private path is exempt" behaviour is
no longer the default.

`ECLUSE_MIN_TRUSTED_INTEGRITY` sets the floor. Unlike the public floor it
is **loosenable below SHA-256** for a legacy private mirror, where trust in the operator's
own vetted source substitutes for cryptographic strength. This is the **only** way Écluse
will serve a sub-SHA-256 digest, and only on the trusted private origin, the public floor
is never lowerable. An unknown algorithm name is still rejected at load. See the [Operator Manual](../../USAGE.md#environment-variables) for supported values.

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
type's default. At boot the rules are arranged **once** into a single total order,**highest precedence first, then rule name ascending**, and evaluation walks that
order and takes the **first decisive result** (an allow, a deny, or a fail-closed
unavailability); if no rule is decisive, the package is denied by default. At
**equal explicit precedence** the tie is resolved by **rule name**, *not* by a
deny-over-allow priority, so two rules an operator gives the same precedence resolve
deterministically by name. Deny-over-allow still holds out of the box, because the
deny defaults sit strictly above the allow defaults. The resolved boot order is
**logged at start-up** (one line per rule, per mount), so the effective resolution is
visible in the start-up log. See
[Rules Engine → Evaluation model](rules-engine.md#evaluation-model).

#### The default policy

The shipped default is deliberately small and **opinionated toward resilience, not
blanket bans**, a floor to extend, not a wall:

| Default rule (name) | Rule | Status | Why |
|---|---|---|---|
| `min-age` | `AllowIfOlderThan` (7 days) | **On at launch** | Admit public versions that have survived a quarantine window, the core defence against race-to-publish typosquatting and dependency confusion. |
| `remediation-fast-track` | `AllowIfRemediatesCve` | **On once the [CVE rules](rules-engine.md#cve-subsystem) land** | Ranked **above** `min-age` so a release that fixes a known CVE is admitted **immediately**, a quarantine must never delay a security patch (see [Rules Engine](rules-engine.md#allowifremediatescve--remediation-fast-track)). |

Deliberately **not** in the default: `DenyInstallTimeExecution` (plenty of legitimate
packages ship install scripts, a blanket ban is too blunt for a default) and
`DenyIfCVE` (blanket-denying every advisory-affected version can break installs of
widely-used packages over low-severity advisories). Both remain **available rules**
an operator opts into by name.

### Validation: fail fast, reject the unknown

Config is **validated in full at startup, and the process refuses to start on any
problem**, it never runs in a degraded or partially-applied state. Errors are
**aggregated** (as `envparse` does for env vars) so one run reports every issue,
not just the first.

Crucially, **unknown is an error, not a silent skip**:

- An unknown rule `type` is **rejected, not ignored**. Silently dropping a
  misspelled rule is a security hole: a typo'd **deny** rule
  (`DenyInstallTimeExecutio` vs `DenyInstallTimeExecution`) would vanish and stop
  blocking, and a typo'd **allow** rule would over-deny. Deny-by-default only
  protects you if the policy you wrote is the policy that loaded.
- **Unknown fields/keys are rejected** too, config is operator-authored
  alongside the binary, so forward-compat tolerance buys little and costs
  typo-catching; the decoders are strict rather than aeson's permissive default.
- **Malformed values** (bad URL, non-integer precedence, unparseable JSON) fail
  the same way.
- **Merge references must resolve.** A `rules` entry that neither names a known
  default nor supplies a complete new rule, a typo'd default name, an
  `"enabled": false` against a rule that does not exist, a patch missing the `type`
  it would need to stand alone, is **rejected**. You cannot silently suppress or
  mistype a rule out of existence.
- **Credential references must resolve.** A mount whose
  [credential strategy](access-model.md) draws on a provider the deployment has not
  initialised; e.g. a `service` mount with
  no read provider, or a mirror target naming a backend whose ambient cloud identity
  is absent, is **rejected at boot**. Credential providers are
  [process-global](cloud-backends.md#credential-provider) and a mount only references
  one, so an incompatible reference never reaches a request.
- **A static publish credential requires a verifiable edge.** A `ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN`
  set without `ECLUSE_AUTH_TOKEN` is **rejected at boot** (`PublishStaticCredentialNeedsEdge`):
  a static credential makes Écluse publish under its own identity, so coupling it to an open
  edge would let any unauthenticated client publish under it, that combination is made
  unrepresentable rather than left as an operator footgun.

A bad config is thus a loud, immediate startup failure an operator sees and fixes,
never a quietly mis-enforced policy.

## Client Authentication

This section covers **inbound** auth (client → proxy), the **edge authentication**
half of the [Access & Credential Model](access-model.md). How the *upstreams* are
then credentialled (forward the client token, or use Écluse's own) is the mount's
[credential strategy](access-model.md#credential-strategies-per-mount), covered there
and under [Outbound Registry Credentials](#outbound-registry-credentials); the one
invariant that holds regardless is that the client's credential is **never** sent to
the public upstream.

Edge authentication is **optional**. The modes (full rationale, including the npm
client's constraints, in [access-model](access-model.md#edge-authentication)):

1. **Open**, `ECLUSE_AUTH_TOKEN` is unset. Any client can reach the proxy.
   Access control is delegated entirely to the network layer (VPC, service mesh,
   etc.).
2. **Static token**, `ECLUSE_AUTH_TOKEN` is set. Clients must include it as
   `Bearer <token>` in the `Authorization` header or as `_authToken` in
   `.npmrc`. Standard npm tooling supports this out of the box.
3. **Trusted edge identity**, a fronting authenticating proxy / cloud IAP / service
   mesh performs SSO or mTLS and asserts a verified identity Écluse trusts. Écluse
   honours the assertion **only over a verifiable binding to that edge**, mutual TLS
   from the edge, or a shared secret / HMAC on the asserted identity, and **fails
   fast** on a `trusted-edge` mount configured with neither (consistent with
   [Validation](#validation-fail-fast-reject-the-unknown)); a bare trusted header is
   forgeable into granted access wherever Écluse is reachable other than through the
   edge. Reaching Écluse *exclusively* through the edge remains the deployment's part.
   Validating cloud IAM at the npm edge directly is out (the npm client cannot speak
   it); it stays a gateway concern, and a managed registry can independently enforce
   write IAM on the mirror target.
