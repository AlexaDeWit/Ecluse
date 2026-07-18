# Configuration and authentication

> Part of the [Écluse architecture overview](../architecture.md).

## Configuration

> **Operators:** [`USAGE.md`](../../USAGE.md#environment-variables) is the canonical reference for
> the environment variables, their defaults and values, client setup, and the network-egress
> checklist. This document is the design rationale behind those settings, not a second copy of the
> variable list.

Configuration has two layers: environment variables for process-level and secret values, and a
structured YAML document for the two things too expressive for flat env vars, the **rule policy** and
the **mount map**. The rule policy earns the document its keep: rules with per-rule precedence and
value overrides, layered over a built-in default (see [Rule policy](#rule-policy)). Mounts are flat,
so the single-ecosystem environment variables desugar to a one-entry mount map, and the common launch
case (one npm mount on the default policy) needs no document. Multi-ecosystem deployments key each
ecosystem under the `mounts` object; the
[path prefix is derived from that key](web-layer.md#multi-ecosystem-mounts), so a colliding prefix is
unrepresentable. Resolution is per key, strongest last: default, then document, then environment.

A mount's shape is **derived, not declared**: any operator-supplied key under `mounts.<ecosystem>`
activates it, and declaring a `mirrorTarget` (not a mode flag) is what makes it mirrored, its
`privateUpstream` then required so the mirror reads back. This structural coupling makes a
mirrored-without-private mount, or a serve-only mount carrying a mirror-write setting,
unrepresentable rather than a runtime surprise: each is a boot error naming the key. The boot log
names every mount's resolved mode. The operator rules are in
[USAGE → Configuration](../../USAGE.md#configuration).

Secrets never live in the structured config. Tokens are always environment variables; cloud-managed
registries derive short-lived tokens from ambient cloud credentials (see
[Outbound registry credentials](#outbound-registry-credentials)).

### Registry endpoints must be https

Every registry endpoint (the private and public upstreams, the mirror target, the publication
target) must be an `https://` URL; a plain-HTTP endpoint fails closed at boot with an error naming the
URL. Certificate validation is the endpoint-authentication boundary, so a private registry on an
internal CA is supported by adding your cert chain to the image's system trust store; the proxy
pre-bakes no custom CA trust. A legacy upstream advertising a plaintext `dist.tarball` on its own host
is upgraded to https; a plaintext tarball on any other host is dropped (the version is skipped).

### Upstream composition (optional)

`ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM` may point at a single registry or at one that aggregates
others (a CodeArtifact repository with upstream relationships to the mirror-target and first-party
repos), so one fetch returns the whole trusted set. This is an optimisation, never a precondition:
Écluse [merges packuments across upstreams](registry-model.md#packument-merge-across-upstreams)
itself. The one rule that keeps it safe: the aggregator must not add a direct connection to the public
registry, which would route unvetted packages around the gate. The public upstream is always fetched
and gated by Écluse.

### Outbound registry credentials

A **mirrored** mount holds a credential to write its mirror target, and that write is Écluse's only
standing credential: it runs on the async worker under Écluse's own identity, while reads carry none
of it. A serve-only mount, or a deployment with zero mirrored mounts, mints nothing.

The mirror-write credential is **derived from the mirror-target URL**, so it is always the credential
that endpoint dictates and can never be paired with an endpoint it was not minted for. A CodeArtifact
endpoint (`{domain}-{owner}.d.codeartifact.{region}.amazonaws.com`) encodes its whole mint identity
in its host, so the worker mints a short-lived token scoped to that domain; any other host is written
with a static token (`…MIRROR_TARGET_TOKEN`). Two arrangements are refused at load so neither degrades
silently: a non-CodeArtifact target with no static token, and a CodeArtifact target that also carries
a static token. A CodeArtifact token is minted per domain, so mounts whose resolved identities
coincide share one [`CredentialProvider`](cloud-backends.md#credential-provider) (one mint, refresh,
and breaker).

Reads are passthrough today: the client's own token is forwarded to the private upstream and
**stripped before the public upstream**, which is queried anonymously under every arrangement. A
per-mount `service` read strategy is planned; the full model is in
[access model](access-model.md#the-shipped-model-passthrough), and the keys in
[USAGE](../../USAGE.md#environment-variables).

### Outbound egress safety

Écluse constrains its own outbound fetches (an https-only host allowlist, a literal internal-range block
on the `dist.tarball` host, and certificate validation authenticating the dialled host), but network
egress is a shared responsibility: the deployment must
also fence egress at the platform layer (security groups, `NetworkPolicy`, Istio egress policy). See
[Network egress is a shared responsibility](security.md#network-egress-is-a-shared-responsibility).

Two application-level knobs adjust threat tolerance: one relaxes *which allowlisted host* may serve a
tarball (never whether the allowlist or internal-range block applies), the other widens the fixed
internal-range set with operator-supplied CIDRs. See
[USAGE](../../USAGE.md#environment-variables) for the names and values.

### Response bounds

Écluse bounds what an upstream response may cost it ([invariant 4](security.md#invariants)): a hostile
upstream cannot exhaust the proxy with a multi-gigabyte body, a version flood, or a deeply-nested
document. The bounds are enforced on the upstream to proxy metadata path and fail closed, refusing a
document past any ceiling outright (as a parse failure does). They are independent of the client to
proxy request-body cap, and artifacts stream with constant memory, outside the body-size bound.

`ECLUSE_LIMITS__MAX_RESPONSE_BYTES` (default 12 MiB) is the primary, pre-decode bound, applied as the
body streams before aeson decodes it, so a hostile body is aborted mid-stream.
`ECLUSE_LIMITS__MAX_VERSION_COUNT` (checked after the packument is projected) backstops per-version
work, and `ECLUSE_LIMITS__MAX_NESTING_DEPTH` bounds document nesting. See the
[Operator Manual](../../USAGE.md#environment-variables).

### Aggregate serve capacity

Per-response ceilings do not bound aggregate residency when many clients resolve different packages
at once, so Écluse admits at most `ECLUSE_RUNTIME__SERVE_MAX_IN_FLIGHT` metadata materialisations
process-wide (a whole packument request, or the public-metadata gate after a private tarball miss).
The default is computed at boot as `max(8, 10 x capabilities)`, the multiplier empirical since a slot
is held across every upstream leg plus GC and scheduling delay. Work beyond the cap waits up to ~1
second for a slot (a bounded waiting room, deliberately equal to the `Retry-After: 1` hint) and is
shed with `503` and `Retry-After: 1` only if that room is full or the wait outlives its budget; there
is no application queue whose memory or latency grows with client concurrency. Health probes, locally
answered routes, and trusted private tarball hits bypass the bound. The connection pools and their
sizing are in
[Web Layer → serve admission and upstream pools](web-layer.md#serve-admission-and-upstream-pools).

### Runtime sizing: cores and heap ceiling

`ECLUSE_RUNTIME__CORES` and `ECLUSE_RUNTIME__MAX_HEAP_BYTES` are the first-class surface; anything
omitted is derived from the container's cgroup (v2, reading every ancestor, tightest limit wins), and
with no cgroup limit the GHC runtime's own resolution stands. Resolution is per knob, strongest first:
config, then cgroup, then runtime, each decision boot-logged with its provenance.

Two mechanics are deliberate. A derived heap ceiling subtracts the nursery budget (cores x allocation
area) and 10% slack from the memory limit, floored at half the limit, so it accounts for memory spent
outside the heap. And because a heap ceiling can only be set at runtime start, enforcing one
re-executes the binary once, in place (same PID). An operator's own `GHCRTS -M` is adopted, never
fought.

The resolved posture seeds a second derivation, the **memory plan**: the effective heap ceiling is
partitioned between named tenants whose sum it bounds (a runtime reserve, the metadata cache, the
materialisation working space, the publish-body aggregate, the in-memory queue tenant when selected,
and the enqueue buffer). An explicit config value wins its own bound; otherwise the shipped fallbacks
apply. A pod too small for the tenants' floors sheds in a documented order (cache first, to zero) with
a loud warning per step and always boots; only an explicit override that breaks the plan is refused,
by the boot and `check-config` alike. The structural hostile-input counts (`maxVersionCount`,
`maxNestingDepth`) stay pinned policy: they bound document shape, not bytes, and do not scale with
RAM. The resolution is role-agnostic, binding proxy, Pilot, and Dredger alike; the Operator Manual
carries the [per-pod arithmetic](../../USAGE.md#appendix-runtime-sizing-arithmetic).

### Public integrity floor

A public (untrusted) version is admitted only if its selected artifact carries at least one integrity
digest whose algorithm meets the public integrity floor ([invariant 5](security.md#invariants)).
SHA-1 and MD5 have practical collisions, so a match on one cannot prove an artifact was not
substituted; a public version below the floor is refused (`403`) and filtered from the served listing.
`ECLUSE_INTEGRITY__MIN_PUBLIC` sets it and may be raised as cryptanalysis ages an algorithm, but is
**hard-floored at SHA-256**: a value below it or an unknown name is a load-time error, never silently
clamped, with no escape-hatch to accept a sub-SHA-256 digest from a public upstream.

### Trusted integrity floor

A trusted (private) version is served only if its selected artifact meets the trusted integrity
floor. `ECLUSE_INTEGRITY__MIN_TRUSTED` sets it globally (a mount refines it with
`mounts.<ecosystem>.minTrustedIntegrity`, so one legacy registry's loosening never leaks onto a
neighbour) and defaults to `sha256`, the same secure default as the public floor. Unlike the public
floor it is **loosenable below SHA-256** for a legacy private mirror, where trust in the operator's
vetted source substitutes for cryptographic strength; this is the only way Écluse serves a sub-SHA-256
digest. An unknown algorithm name is still rejected at load.

### Cross-upstream divergence policy

When a shared version's private and public copies contradict on a shared integrity algorithm, that is
the supply-chain tampering Écluse exists to catch (see
[Packument merge](registry-model.md#packument-merge-across-upstreams)). The trusted copy always wins
the bytes, and the divergence is always logged (a `WARNING`) and metered
(`ecluse.registry.merge.divergence`). `ECLUSE_INTEGRITY__DIVERGENCE_POLICY` (per mount,
`mounts.<ecosystem>.divergencePolicy` refines it) decides what else happens to the contested version:
`warn` (the default) serves the trusted copy and relies on the alarm; `fail-closed` additionally
withholds the version from the served listing, dropping any `dist-tag` (including `latest`) that
pointed at it, so a resolver pinned to it fails to resolve rather than receive a contested copy. Run
`warn` first and watch the counter to learn your benign-divergence rate before enabling `fail-closed`.

### Rule policy

The rule policy is a named map of rules layered over a built-in default that ships with the binary.
An entry whose name the default already defines is a **patch** (override precedence and/or values);
an entry with a new name must carry a full `type` (it **adds** a rule); and any entry may set
`"enabled": false` to **suppress** a default rule. With no rule config the default policy applies
unchanged. This top-level policy applies to every mount; a multi-ecosystem deployment may give an
individual mount its own [refinement](web-layer.md#multi-ecosystem-mounts) that merges over it.

```json
{
  "rules": {
    "min-age":      { "ageSeconds": 1209600 },
    "deny-scripts": { "type": "DenyInstallTimeExecution", "precedence": 200 }
  }
}
```

Here `min-age` names a default rule, so it overrides that rule's value; `deny-scripts` is a new name
carrying a `type`, so it adds a rule. Each rule may set an integer `precedence` (higher wins); omit it
for the type's default. The precedence values, the single total order the rules resolve into, and the
evaluation model live in [Rules engine → Evaluation model](rules-engine.md#evaluation-model), the
canonical home; this document owns only the document-merge schema above.

#### The default policy

The shipped default enables two rules. `min-age` (`AllowIfOlderThan`, 7 days) admits public versions
that have survived a quarantine window, the core defence against race-to-publish typosquatting and
dependency confusion. `remediation-fast-track` (`AllowIfRemediatesCve`) is ranked above it so a
release fixing a known CVE is admitted immediately rather than waiting out the quarantine (see
[Rules engine](rules-engine.md#allowifremediatescve-remediation-fast-track)). Every other built-in
rule is off and opts in by name; `DenyIfCve` in particular can deny historical versions an existing
build depends on if enabled before the mirror is warmed, so read its
[onboarding steps](../../USAGE.md#onboarding-denyifcve) first.

### Advisory database sync

The remediation fast lane and `DenyIfCve` read a synced local advisory database rather than an API per
request; the compilation, ETag polling, and atomic shadow-swap are under
[Rules engine → CVE subsystem](rules-engine.md#cve-subsystem), and the operator knobs (bucket, poll
interval, OSV export source, download size cap) in [USAGE](../../USAGE.md#environment-variables). With
no bucket configured the fast lane abstains and the age quarantine governs alone.

### Validation: fail fast, reject the unknown

Config is validated in full at startup and the process refuses to start on any problem, never running
in a degraded state. Errors are aggregated, so one run reports every issue. Unknown is an error, not a
silent skip:

- An unknown rule `type` or unknown field/key is rejected: config is operator-authored alongside the
  binary, and deny-by-default only protects you if the policy you wrote is the policy that loaded, so
  a typo must fail the load rather than silently stop blocking.
- Malformed values (bad URL, non-integer precedence, unparseable JSON) fail the same way.
- Merge references must resolve: a `rules` entry that neither names a known default nor supplies a
  complete new rule is rejected.
- A mount must be coherent with its derived mode: a mirrored mount without a `privateUpstream`, and a
  serve-only mount carrying a mirror-write setting, are each rejected naming the offending key (every
  incomplete mount in one report).
- The mirror-write credential must resolve: a non-CodeArtifact target with no static token, a
  CodeArtifact target that also carries a static token, or a CodeArtifact identity that cannot mint an
  initial token, is rejected.
- A static publish credential requires a verifiable edge:
  `ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN` set without `ECLUSE_SERVER__AUTH_TOKEN` is rejected
  (`PublishStaticCredentialNeedsEdge`), since it would let any unauthenticated client publish under
  Écluse's own identity.

The same validation runs without a boot: `ecluse check-config` runs the full resolution chain (config
load, runtime plan, sizing and memory-budget resolvers, mirror-queue selection) and prints every
decision, one provenance line per resolved key (environment > document > default, secrets redacted),
exiting `0` on a valid configuration and `2` with the same aggregated report a boot would log.

## Client authentication

Inbound auth (client to proxy) is the edge-authentication half of the
[Access and credential model](access-model.md); how the upstreams are then credentialled is the
mount's [credential strategy](access-model.md#the-shipped-model-passthrough) (see
[Outbound registry credentials](#outbound-registry-credentials)). The client's credential is never
sent to the public upstream.

Two edge modes ship: **open** (`ECLUSE_SERVER__AUTH_TOKEN` unset, access delegated to the network
layer) and **static token** (`ECLUSE_SERVER__AUTH_TOKEN` set, presented as `Bearer <token>` or
`.npmrc` `_authToken`, which standard npm tooling supports). A third mode, a trusted edge identity
asserted by a fronting proxy, IAP, or mesh and honoured only over a verifiable binding to that edge,
is planned. The full rationale is in
[access model → edge authentication](access-model.md#edge-authentication).
