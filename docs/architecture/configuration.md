# Configuration and authentication

> Part of the [Écluse architecture overview](../architecture.md).

## Configuration

> **Operators:** [`USAGE.md`](../../USAGE.md#environment-variables) is the canonical reference for
> the environment variables, their defaults and values, client setup, and the network-egress
> checklist. This document is the design rationale behind those settings, not a second copy of the
> variable list; keep the two aligned.

Configuration has two layers: environment variables for process-level and secret values, and
a structured config document for the two things too expressive for flat env vars, the **rule
policy** and the **mount map**.

The rule policy earns the document its keep: a set of rules with per-rule precedence and value
overrides, layered over a built-in default (see [Rule policy](#rule-policy)). Mounts are
comparatively flat, three registry endpoints and a queue backend under a prefix
[derived from the ecosystem](web-layer.md#multi-ecosystem-mounts), so the single-ecosystem
environment variables (below) desugar to a one-entry mount map, and the common launch case (one npm
mount on the default policy) needs no document at all. Multi-ecosystem deployments (see
[multi-ecosystem mounts](web-layer.md#multi-ecosystem-mounts)) declare each ecosystem's
registries in the document's `mounts` object, keyed by ecosystem name (`npm`, `pypi`); the
path prefix is derived from that key, never declared, so a wrong or colliding prefix is
unrepresentable. The document is a YAML config file, the source of truth: reviewable,
diffable, the expected form once the rule policy is non-trivial.

A mount serves only when the operator declares it. The shipped defaults carry a dormant
template per ecosystem (the canonical public upstream and the default credential provider),
and any operator-supplied key under `mounts.<ecosystem>`, in the document or through the
`ECLUSE_MOUNTS__*` variables, activates that mount. An active mount must define its private
upstream: a declared-but-incomplete mount is a boot error naming the missing key, never a
mount that silently vanishes from service, and a mount the operator never mentions stays
off without ceremony. Declaring every registry endpoint explicitly is the recommended
posture; endpoints of one mount that resolve to the same registry are each logged as a
boot warning (the mirror target folding onto the private upstream included), since a
shared store narrows provenance separation and what maintenance tooling can safely do
(see [USAGE → Deviating from the Golden Path](../../USAGE.md#deviating-from-the-golden-path)).

Secrets never live in the structured config. Tokens (`ECLUSE_AUTH_TOKEN`, per-endpoint
registry tokens) are always environment variables; cloud-managed registries derive short-lived
tokens from ambient cloud credentials (see
[Outbound registry credentials](#outbound-registry-credentials)).

### Registry endpoints must be https

Every registry endpoint (the private and public upstreams, the mirror target, the publication
target) must be an `https://` URL. A plain-HTTP endpoint is unsupported: the proxy fails closed at
boot with an error naming the URL. Certificate
validation is the endpoint-authentication boundary, so a private registry on an internal CA is
supported by adding your cert chain to the image's system trust store; the proxy does not
pre-bake custom CA trust. A legacy upstream advertising a plaintext `dist.tarball` on its own
host is upgraded to https automatically; a plaintext tarball on any other host is dropped (the
version is skipped and the drop recorded).

### Upstream composition (optional)

`ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM` may point at a single registry or at one that aggregates
others, e.g. an AWS CodeArtifact repository with upstream relationships to a mirror-target repo
and a first-party repo, so one fetch returns the whole trusted set. This is supported but never
required: Écluse
[merges packuments across upstreams](registry-model.md#packument-merge-across-upstreams)
itself, so registry-level composition is an optimisation, not a precondition. The one rule that
keeps it safe: the aggregator must not add a direct connection to the public registry (that
would route unvetted packages around the gate); the public upstream is always fetched and gated
by Écluse.

### Outbound registry credentials

Écluse always holds a credential to write the mirror target, and, depending on the mount's
[credential strategy](access-model.md), may also hold one to read the private upstream. Each such
endpoint selects a [`CredentialProvider`](cloud-backends.md#credential-provider): cloud-managed
(CodeArtifact / Artifact Registry, a token minted from the ambient cloud identity) or a static token.

The mirror-write credential is explicit and does not fold when the mirror-target URL folds onto the
private upstream: under the default `passthrough` the private upstream carries no Écluse credential,
while the mirror write runs on the async worker under Écluse's own identity, so the two are chosen
independently. The read-side and publish-target providers follow the same prefixed-provider pattern;
see [USAGE](../../USAGE.md#environment-variables) for the exact keys.

How reads are credentialled is the mount's
[credential strategy](access-model.md#credential-strategies-per-mount). Config-side, the one
constant: the public upstream is queried anonymously under every strategy, with no token, and the
client's token is never forwarded there. Minting credentials from a cloud identity keeps long-lived
secrets out of config.

### Outbound egress safety

Écluse constrains its own outbound fetches (host allowlist + internal-range block, re-applied
to every resolved IP at connection time; see [Security Invariants](security.md)), but network
egress is a shared responsibility: the deployment must also fence egress at the platform layer
(security groups, `NetworkPolicy`, Istio `ServiceEntry`/egress policy, blocking the
`169.254.169.254` metadata endpoint). See
[Network egress is a shared responsibility](security.md#network-egress-is-a-shared-responsibility).

Two application-level knobs let the operator adjust threat tolerance in opposite directions. One
relaxes *which allowlisted host* may serve a tarball, never whether the allowlist or internal-range
block applies (the default keeps the tightest reading of [invariant 2](security.md#invariants)); the
other extends invariant 3's fixed internal-range set with operator-supplied CIDRs (a deployment's own
internal space) and only ever widens the block. See
[USAGE](../../USAGE.md#environment-variables) for the variable names and values.

### Response bounds

Écluse bounds what an upstream response may cost it ([invariant 4](security.md#invariants)): a
hostile or compromised upstream cannot exhaust the proxy with a multi-gigabyte body, a version
flood, or a deeply-nested JSON document. The bounds are enforced on the upstream→proxy metadata
path and fail closed, a document past any ceiling is refused outright (degrading exactly as a
parse failure does), never partially served. They are independent of the client→proxy
request-body cap. Artifacts stream with constant memory and are not subject to the body-size
bound.

Each bound is a strictly positive integer (a non-positive value is rejected at startup). The
ceilings are layered: `ECLUSE_MAX_RESPONSE_BYTES` (default 12 MiB; the largest packuments seen
today are ~4 MiB) is the primary, pre-decode bound, applied as the body streams before aeson
decodes it, so parse spend is fixed and a hostile body is aborted mid-stream.
`ECLUSE_MAX_VERSION_COUNT`, checked after the packument is projected, is a defence-in-depth
backstop on per-version work; `ECLUSE_MAX_NESTING_DEPTH` bounds document nesting. See the
[Operator Manual](../../USAGE.md#environment-variables).

### Aggregate serve capacity

Per-response ceilings do not bound aggregate residency when many clients resolve different
packages concurrently. Écluse therefore admits at most `ECLUSE_SERVE_MAX_IN_FLIGHT` metadata
materialisations process-wide (a whole packument request, or the public-metadata gate after a
private tarball miss). The default is computed at boot as `max(8, 10 x capabilities)`; the
multiplier is empirical (a slot is held across every upstream leg plus GC and scheduling delay,
so the load bench's dose-response levelled around 10 per capability, not the ~4 a naive
one-round-trip model predicts). Set the key only to override it. Work beyond the cap is shed
immediately with a mount-shaped `503` and `Retry-After: 1`; there is no application queue whose
memory or latency grows with client concurrency. Health probes, locally answered routes, and
trusted private tarball hits bypass the bound (the hit streams with constant-memory
backpressure); admission protects resident metadata structures, not download count.

The public and private connection pools are independently configurable and default to a share
of the process file-descriptor limit, the private pool taking the larger share because a
trusted tarball hit streams outside admission (its demand is the inbound hit fan-out, not the
admission capacity). `ECLUSE_PRIVATE_CONNECTIONS_PER_HOST` (default a quarter of the FD limit,
clamped 64-4096) sizes the private pool; http-client's pool bound governs keep-alive retention,
not concurrency. See
[Web Layer → serve admission and upstream pools](web-layer.md#serve-admission-and-upstream-pools).

### Runtime sizing: cores and heap ceiling

`ECLUSE_CORES` and `ECLUSE_MAX_HEAP_BYTES` are the first-class surface for the process's runtime
posture; anything omitted is derived from the container's cgroup (v2) in the `automaxprocs`
style, and with no cgroup limit the GHC runtime's own resolution (its defaults plus any operator
`GHCRTS`) stands. Resolution is per knob, strongest first: config, then cgroup, then runtime.
The derivation reads the process's own cgroup and every ancestor (tightest limit wins). Every
decision is logged at boot with its provenance.

Two mechanics are deliberate. A derived heap ceiling subtracts the nursery budget (cores x
allocation area) and 10% slack from the memory limit, floored at half the limit, so it accounts
for memory spent outside the heap. And because a heap ceiling can only be set at runtime start,
enforcing one re-executes the binary once, in place (same PID; a supervisor sees an
uninterrupted process), loop-guarded by an internal marker. An operator's own `GHCRTS` is never
fought: an explicit `-M` there is adopted, and a divergence surviving the re-launch is logged as
a warning, never an abort. See the [Operator Manual](../../USAGE.md#operating-écluse) for the
arithmetic.

The resolution is role-agnostic: cores and the heap ceiling derive from the container's limits,
which bind the proxy, Pilot, and Dredger alike. Workload-shaped memory modelling is not
universalised: the shipped allocation-area tuning is the proxy serve path's profile, while Pilot
(a scheduled ingestion pipe) and the Dredger are tuned per-deployment via `GHCRTS` until their
shapes earn their own defaults.

### Public integrity floor

A public (untrusted) upstream's version is admitted only if its selected artifact carries at
least one integrity digest whose algorithm meets the public integrity floor
([invariant 5](security.md#invariants)). SHA-1 and MD5 have practical collisions, so a match on
one cannot prove an artifact was not substituted; a public version whose strongest digest is
below the floor is refused (`403`) and filtered from the served listing.
`ECLUSE_MIN_PUBLIC_INTEGRITY` sets it. It may be raised as cryptanalysis ages an algorithm but
is hard-floored at SHA-256: a value below it or an unknown name is a configuration error
rejected at load, never silently clamped, and there is no escape-hatch to accept a sub-SHA-256
digest from a public upstream. See the [Operator Manual](../../USAGE.md#environment-variables)
for supported algorithms.

### Trusted integrity floor

A trusted (private) upstream's version is served only if its selected artifact meets the trusted
integrity floor ([invariant 5](security.md#invariants)). `ECLUSE_MIN_TRUSTED_INTEGRITY` sets it
and defaults to `sha256`, the same secure default as the public floor, so by default a
SHA-1-only or hashless private version is dropped (filtered from the listing, and a private miss
on the artifact path falls through to the public origin). Unlike the public floor it is
loosenable below SHA-256 for a legacy private mirror, where trust in the operator's vetted source
substitutes for cryptographic strength; this is the only way Écluse serves a sub-SHA-256 digest,
and only on the trusted private origin. An unknown algorithm name is still rejected at load. See
the [Operator Manual](../../USAGE.md#environment-variables) for supported values.

### Cross-upstream divergence policy

When a shared version's private and public copies contradict on a shared artifact's shared
integrity algorithm (a file both carry, under an algorithm both assert for it, whose digests
disagree), that is the supply-chain tampering Écluse exists to
catch ([threat #11](https://ecluse-proxy.com/threat-model.html#threat-11); see
[Packument merge](registry-model.md#packument-merge-across-upstreams)). The trusted copy always
wins the bytes, and the divergence is always logged (a `WARNING`) and metered
(`ecluse.registry.merge.divergence`). `ECLUSE_DIVERGENCE_POLICY` decides what else happens to the
contested version: `warn` (the default) serves the trusted copy and relies on the alarm;
`fail-closed` additionally withholds the contested version from the served listing, so a resolver
pinned to that exact version fails to resolve it rather than receive a contested copy. Fail-closed
trades availability for strictness and drops any `dist-tag` (including `latest`) that pointed at the
withheld version; run `warn` first and watch the counter to learn your benign-divergence rate before
enabling it. See the [Operator Manual](../../USAGE.md#environment-variables).

### Rule policy

The rule policy is a named map of rules layered over a built-in default policy that ships with
the binary. An entry whose name the default already defines is a patch (override precedence
and/or values); an entry with a new name must carry a full `type` (it adds a rule); and any
entry may set `"enabled": false` to suppress a default rule. With no rule config the default
policy applies unchanged. This top-level policy applies to every mount; a multi-ecosystem
deployment may give an individual mount its own [refinement](web-layer.md#multi-ecosystem-mounts)
that merges over it (e.g. a stricter policy on npm than PyPI).

```json
{
  "rules": {
    "min-age":      { "ageSeconds": 1209600 },
    "deny-scripts": { "type": "DenyInstallTimeExecution", "precedence": 200 }
  }
}
```

- `min-age` names a default rule, so this overrides its value (a 14-day window instead of 7);
  unspecified fields keep the default.
- `deny-scripts` is a new name carrying a `type`, so it adds a rule (here, the install-script
  deny that is *not* on by default).
- a `"<name>": { "enabled": false }` entry suppresses the named default rule.

Each rule may set an integer `precedence` (higher wins); omit it to use the type's default:

| `type` | Default precedence | Required field |
|---|---|---|
| `AllowIfOlderThan` | 100 | `ageSeconds` |
| `AllowIfRemediatesCve` | 150 | -- |
| `AllowScope` | 200 | `scope` |
| `DenyIfCve` | 225 | `minSeverity` |
| `AllowByIdentity` | 250 | `identity` |
| `DenyInstallTimeExecution` | 300 | -- |
| `DenyByIdentity` | 400 | `identity` |

At boot the rules are arranged once into a single total order, highest precedence first then
rule name ascending, and evaluation takes the first decisive result (an allow, a deny, or a
fail-closed unavailability); if none is decisive, the package is denied by default. Equal
explicit precedence is resolved by rule name, not by a deny-over-allow priority, so two rules
given the same precedence resolve deterministically. Deny-over-allow holds out of the box for
`DenyInstallTimeExecution` and `DenyByIdentity`, whose defaults sit strictly above every allow. The
one deliberate exception is `DenyIfCve` at 225, ranked just below `AllowByIdentity` (250) so an
explicit identity pin can override an advisory deny, while still outranking the passive quarantine
and scope allow-lists. The resolved boot order is logged at start-up (one line per rule, per mount).
See
[Rules Engine → Evaluation model](rules-engine.md#evaluation-model).

#### The default policy

The shipped default policy enables two rules. `min-age` (`AllowIfOlderThan`, 7 days) admits public
versions that have survived a quarantine window, the core defence against race-to-publish
typosquatting and dependency confusion. `remediation-fast-track` (`AllowIfRemediatesCve`) is ranked
above it so a release that fixes a known CVE is admitted immediately rather than waiting out the
quarantine: a quarantine must never delay a security patch (see
[Rules Engine](rules-engine.md#allowifremediatescve-remediation-fast-track)).

Every other built-in rule (the precedence table above) is off by default and opts in by name.
`DenyInstallTimeExecution` denies install-time code execution, off by default because many legitimate
packages ship install scripts. The effectful `DenyIfCve` denies a version an advisory affects at or
above a configured CVSS `minSeverity` (and, because unscored advisories count as above every
threshold, the npm malware feed); it is off by default and opts in by name, since enabling it before
the mirror is warmed can deny historical versions an existing build depends on.

### Advisory database sync

The remediation fast lane (and `DenyIfCve`) read a synced local advisory database rather
than calling an API per request; the compilation, ETag polling, and atomic shadow-swap are described
under [Rules Engine → CVE subsystem](rules-engine.md#cve-subsystem). Its operator knobs, the
advisory-database bucket, the poll interval, the OSV export source, and a size cap bounding an
oversized or hostile download, live in [USAGE](../../USAGE.md#environment-variables). With no
advisory-database bucket configured the fast lane abstains and the age quarantine governs alone.

### Validation: fail fast, reject the unknown

Config is validated in full at startup and the process refuses to start on any problem; it never
runs in a degraded or partially-applied state. Errors are aggregated (as `envparse` does for env
vars) so one run reports every issue. Unknown is an error, not a silent skip:

- An unknown rule `type` is rejected, not ignored. Silently dropping a misspelled rule is a
  security hole: a typo'd deny rule would stop blocking, and a typo'd allow rule would over-deny.
  Deny-by-default only protects you if the policy you wrote is the policy that loaded.
- Unknown fields/keys are rejected: config is operator-authored alongside the binary, so
  forward-compat tolerance buys little and costs typo-catching; the decoders are strict.
- Malformed values (bad URL, non-integer precedence, unparseable JSON) fail the same way.
- Merge references must resolve. A `rules` entry that neither names a known default nor supplies
  a complete new rule (a typo'd default name, an `"enabled": false` against a non-existent rule,
  a patch missing the `type` it needs to stand alone) is rejected.
- A declared mount must be complete. Any operator-supplied key under `mounts.<ecosystem>`
  activates that mount, and an active mount without a `privateUpstream` is rejected at boot,
  naming the mount and the key (every incomplete mount in one report); the shipped
  per-ecosystem templates alone never activate anything.
- Credential references must resolve. A mount whose [credential strategy](access-model.md) draws
  on an uninitialised provider (a `service` mount with no read provider, or a mirror target
  naming a backend whose ambient cloud identity is absent) is rejected at boot. Credential
  providers are [process-global](cloud-backends.md#credential-provider).
- A static publish credential requires a verifiable edge:
  `ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN` set without `ECLUSE_AUTH_TOKEN` is rejected at
  boot (`PublishStaticCredentialNeedsEdge`), since a static credential makes Écluse publish under
  its own identity and an open edge would let any unauthenticated client publish under it.

A bad config is a loud, immediate startup failure, never a quietly mis-enforced policy.

## Client authentication

This section covers inbound auth (client → proxy), the edge-authentication half of the
[Access & Credential Model](access-model.md); how the upstreams are then credentialled is the
mount's [credential strategy](access-model.md#credential-strategies-per-mount) (see also
[Outbound registry credentials](#outbound-registry-credentials)). The one invariant regardless:
the client's credential is never sent to the public upstream.

Edge authentication is optional; the full rationale is in
[access-model](access-model.md#edge-authentication). The modes:

1. Open, `ECLUSE_AUTH_TOKEN` unset. Any client can reach the proxy; access control is delegated
   to the network layer (VPC, service mesh).
2. Static token, `ECLUSE_AUTH_TOKEN` set. Clients present it as `Bearer <token>` in
   `Authorization` or as `_authToken` in `.npmrc`. Standard npm tooling supports this.
3. Trusted edge identity, a fronting proxy / cloud IAP / service mesh asserts a verified
   identity Écluse trusts, honoured only over a verifiable binding to that edge (mutual TLS, or a
   shared secret / HMAC on the assertion) and rejected at boot with neither. Validating cloud IAM
   at the npm edge is out (the npm client cannot speak it); a managed registry can independently
   enforce write IAM on the mirror target.
