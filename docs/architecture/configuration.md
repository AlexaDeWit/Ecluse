# Configuration and authentication

> Part of the [Écluse architecture overview](../architecture.md).

Why Écluse's configuration has the shape it has: two layers, a derived mount, safety floors that
fail closed, and client authentication at the edge.

## Configuration

> **Operators:** [`USAGE.md`](../../USAGE.md#environment-variables) is the canonical reference for
> the environment variables, their defaults and values, client setup, and the network-egress
> checklist. This document holds the design rationale behind those settings, not a second copy of
> the variable list.

Configuration has two layers. Environment variables carry process-level and secret values. A
structured YAML document carries the two things too expressive for flat env vars: the **rule
policy** and the **mount map**. The rule policy earns the document its keep: per-rule
precedence and value overrides, layered over a built-in default (see [Rule policy](#rule-policy)).

Mounts are flat, so the single-ecosystem environment variables desugar to a one-entry mount map.
The common launch case, one npm mount on the default policy, needs no document. A multi-ecosystem
deployment keys each ecosystem under the `mounts` object. The [path prefix comes from that
key](web-layer.md#multi-ecosystem-mounts), so a colliding prefix is unrepresentable. Resolution runs
per key, strongest last: default, then document, then environment.

A mount's shape is **derived, not declared**. Any operator-supplied key under `mounts.<ecosystem>`
activates it, and a declared `mirrorTarget`, not a mode flag, makes it mirrored. A mirrored mount
then requires a `privateUpstream` so the mirror reads back.

The coupling makes two arrangements unrepresentable, not a runtime surprise: a mirrored mount
with no private upstream, and a serve-only mount with a mirror-write setting. Each is a boot error
naming the key, and the boot log names every mount's resolved mode. The operator rules
are in [USAGE → Configuration](../../USAGE.md#configuration).

Secrets never live in the structured config. A token is always an environment variable. A
cloud-managed registry derives a short-lived token from ambient cloud credentials (see
[Outbound registry credentials](#outbound-registry-credentials)).

### Registry endpoints must be https

Every registry endpoint must be an `https://` URL: the private and public upstreams, the mirror
target, and the publication target. A plain-HTTP endpoint fails closed at boot with an error naming
the URL.

Certificate validation is the endpoint-authentication boundary. To use a private registry on an
internal CA, add your cert chain to the image's system trust store. The proxy pre-bakes no custom
CA trust. It upgrades a plaintext `dist.tarball` to https when the legacy upstream advertises it on
its own host. It drops a plaintext tarball on any other host and skips that version.

### Upstream composition (optional)

`ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM` may point at a single registry, or at one that aggregates
others: a CodeArtifact repository with upstream relationships to the mirror-target and first-party
repos. One fetch then returns the whole trusted set. This is an optimisation, never a precondition,
because Écluse [merges packuments across upstreams](registry-model.md#packument-merge-across-upstreams)
itself. One rule keeps it safe: the aggregator must not add a direct connection to the public
registry, which would route unvetted packages around the gate. The proxy always fetches and gates
the public upstream itself.

### Outbound registry credentials

A **mirrored** mount holds a credential to write its mirror target. That write is Écluse's only
standing credential. It runs on the async worker under Écluse's own identity, and reads carry none
of it. A serve-only mount, or a deployment with no mirrored mounts, mints nothing.

The mirror-write credential is **derived from the mirror-target URL**. It is always the credential
that endpoint dictates, and Écluse can never pair it with an endpoint it was not minted for. A
CodeArtifact endpoint (`{domain}-{owner}.d.codeartifact.{region}.amazonaws.com`) encodes its whole
mint identity in its host, so the worker mints a short-lived token scoped to that domain. The
worker writes any other host with a static token (`…MIRROR_TARGET_TOKEN`).

Écluse refuses two arrangements at load, so neither degrades silently: a non-CodeArtifact target
with no static token, and a CodeArtifact target that also carries one. The CodeArtifact mint is per
domain, so mounts whose resolved identities coincide share one
[`CredentialProvider`](cloud-backends.md#credential-provider): one mint, one refresh, one breaker.

Reads are passthrough today. The proxy forwards the client's own token to the private upstream and
**strips it before the public upstream**, which it queries anonymously under every arrangement. A
per-mount `service` read strategy is planned. The full model is in [access
model](access-model.md#the-shipped-model-passthrough), and the keys are in
[USAGE](../../USAGE.md#environment-variables).

### Outbound egress safety

Écluse constrains its own outbound fetches. It applies an https-only host allowlist, a literal
internal-range block on the `dist.tarball` host, and certificate validation that authenticates the
dialled host. Network egress is still a shared responsibility: the deployment must also fence
egress at the platform layer, with security groups, `NetworkPolicy`, or Istio egress policy. See
[Network egress is a shared responsibility](security.md#network-egress-is-a-shared-responsibility).

Two application-level knobs adjust threat tolerance. One relaxes *which allowlisted host* may serve
a tarball, never whether the allowlist or the internal-range block applies. The other widens the
fixed internal-range set with operator-supplied CIDRs. See
[USAGE](../../USAGE.md#environment-variables) for the names and values.

### Response bounds

Écluse bounds what an upstream response may cost it ([invariant 4](security.md#invariants)). A
hostile upstream cannot exhaust the proxy with a multi-gigabyte body, a version flood, or a
deeply-nested document. The bounds sit on the upstream-to-proxy metadata path and fail closed. The
proxy refuses a document past any ceiling outright, exactly as it refuses a parse failure. They are
independent of the client-to-proxy request-body cap. Artifacts stream with constant memory, outside
the body-size bound.

`ECLUSE_LIMITS__MAX_RESPONSE_BYTES` (default 12 MiB) is the primary, pre-decode bound. It applies
as the body streams, before aeson decodes it, so Écluse aborts a hostile body mid-stream.
`ECLUSE_LIMITS__MAX_VERSION_COUNT` backstops per-version work, and Écluse checks it after
projecting the packument. `ECLUSE_LIMITS__MAX_NESTING_DEPTH` bounds document nesting. See the
[Operator Manual](../../USAGE.md#environment-variables).

### Aggregate serve capacity

Per-response ceilings do not bound aggregate residency when many clients resolve different packages
at once. The proxy therefore admits at most `ECLUSE_RUNTIME__SERVE_MAX_IN_FLIGHT` metadata
materialisations process-wide: a whole packument request, or the public-metadata gate after a
private tarball miss. Boot computes the default as `max(8, 10 x capabilities)`. The multiplier is
empirical: one slot covers every upstream leg plus GC and scheduling delay.

Work beyond the cap waits up to ~1 second for a slot. That waiting room stays bounded and
deliberately matches the `Retry-After: 1` hint. The proxy sheds the work with `503` and
`Retry-After: 1` only when the room is full or the wait outlives its budget. No application queue
grows in memory or latency with client concurrency. Health probes, locally answered routes, and
trusted private tarball hits bypass the bound. The connection pools and their sizing are in
[Web Layer → serve admission and upstream pools](web-layer.md#serve-admission-and-upstream-pools).

### Runtime sizing: cores and heap ceiling

`ECLUSE_RUNTIME__CORES` and `ECLUSE_RUNTIME__MAX_HEAP_BYTES` are the first-class surface. Écluse
derives anything omitted from the container's cgroup: v2, reading every ancestor, tightest limit
wins. With no cgroup limit, the GHC runtime's own resolution stands. Resolution runs per knob,
strongest first: config, then cgroup, then runtime. The boot log carries each decision with its
provenance.

Two mechanics are deliberate. A derived heap ceiling accounts for memory spent outside the heap.
It subtracts the nursery budget (cores x allocation area) and 10% slack from the memory limit, with
a floor at half the limit. A heap ceiling can only be set at runtime start, so enforcing one
re-executes the binary once, in place, under the same PID. The proxy adopts an operator's own
`GHCRTS -M` and never fights it.

The resolved posture seeds a second derivation, the **memory plan**. It partitions the effective
heap ceiling between named tenants whose sum it bounds:

- a runtime reserve
- the metadata cache
- the materialisation working space
- the publish-body aggregate
- the in-memory queue tenant, when selected
- the enqueue buffer
- the mirror-artifact envelope, when any mount mirrors

The mirror-artifact tenant covers the transient the mirror worker holds. That worker buffers a
fetched tarball and base64-encodes it into a publish document. The bytes, the base64 text, and the
serialised document coexist before collection. The derivation sets the worker fetch cap
(`maxArtifactBytes`) so that this envelope is what the tenant charges. An explicit config value
wins its own bound, and otherwise the shipped fallbacks apply.

A pod too small for the tenants' floors sheds in a documented order and always boots. The
mirror-artifact cap goes first, to zero, so the background back-fill leg gives way before the serve
hot path. The cache goes next, also to zero, and each step logs a loud warning. The boot and
`check-config` alike refuse only an explicit override that breaks the plan.

The structural hostile-input counts (`maxVersionCount`, `maxNestingDepth`) stay pinned policy. They
bound document shape, not bytes, and do not scale with RAM. The resolution is role-agnostic and
binds proxy, Pilot, and Dredger alike. The Operator Manual carries the [per-pod
arithmetic](../../USAGE.md#appendix-runtime-sizing-arithmetic).

### Public integrity floor

Écluse admits a public (untrusted) version only when its selected artifact carries an integrity
digest whose algorithm meets the public integrity floor ([invariant 5](security.md#invariants)). One
such digest is enough. SHA-1 and MD5 have practical collisions, so a match on one cannot prove an
artifact was not substituted. The proxy refuses a public version below the floor with `403` and
filters it from the served listing.

`ECLUSE_INTEGRITY__MIN_PUBLIC` sets the floor. Raise it as cryptanalysis ages an algorithm. The
setting is **hard-floored at SHA-256**: a value below SHA-256, or an unknown name, is a load-time
error. There is no silent clamp, and no escape hatch accepts a sub-SHA-256 digest from a public
upstream.

### Trusted integrity floor

Écluse serves a trusted (private) version only when its selected artifact meets the trusted
integrity floor. `ECLUSE_INTEGRITY__MIN_TRUSTED` sets it globally and defaults to `sha256`, the
same secure default as the public floor. A mount refines it with
`mounts.<ecosystem>.minTrustedIntegrity`, so one legacy registry's loosening never leaks onto a
neighbour.

Unlike the public floor, this one is **loosenable below SHA-256** for a legacy private mirror.
There, trust in the operator's vetted source substitutes for cryptographic strength. This is the
only way Écluse serves a sub-SHA-256 digest. Écluse still rejects an unknown algorithm name at
load.

### Cross-upstream divergence policy

A shared version whose private and public copies contradict on a shared integrity algorithm is the
supply-chain tampering Écluse exists to catch. See [Packument
merge](registry-model.md#packument-merge-across-upstreams). The trusted copy always wins
the bytes, and Écluse always logs the divergence at `WARNING` and meters it on
`ecluse.registry.merge.divergence`.

`ECLUSE_INTEGRITY__DIVERGENCE_POLICY` decides what else happens to the contested version, and
`mounts.<ecosystem>.divergencePolicy` refines it per mount. `warn`, the default, serves the trusted
copy and relies on the alarm. `fail-closed` also withholds the version from the served listing and
drops any `dist-tag` that pointed at it, `latest` included. A resolver pinned to that version then
fails to resolve rather than receive a contested copy. Run `warn` first and watch the counter to
learn your benign-divergence rate before enabling `fail-closed`.

### Rule policy

The rule policy is a named map of rules layered over a built-in default that ships with the binary.
An entry whose name the default already defines is a **patch**: it overrides precedence, values, or
both. An entry with a new name must carry a full `type`, and it **adds** a rule. Any entry may set
`"enabled": false` to **suppress** a default rule. With no rule config, the default policy applies
unchanged.

This top-level policy applies to every mount. A multi-ecosystem deployment may give an individual
mount its own [refinement](web-layer.md#multi-ecosystem-mounts) that merges over it.

```json
{
  "rules": {
    "min-age":      { "ageSeconds": 1209600 },
    "deny-scripts": { "type": "DenyInstallTimeExecution", "precedence": 200 }
  }
}
```

Here `min-age` names a default rule, so it overrides that rule's value. `deny-scripts` is a new name
carrying a `type`, so it adds a rule. Each rule may set an integer `precedence`, where higher wins.
Omit it for the type's default.

[Rules engine → Evaluation model](rules-engine.md#evaluation-model) is the canonical home for the
precedence values, the single total order the rules resolve into, and the evaluation model. This
document owns only the document-merge schema above.

#### The default policy

The shipped default enables two rules. `min-age` (`AllowIfOlderThan`, 7 days) admits a public
version that survived a quarantine window, the core defence against race-to-publish typosquatting
and dependency confusion. `remediation-fast-track` (`AllowIfRemediatesCve`) ranks above it, so
Écluse admits a release fixing a known CVE at once rather than waiting out the quarantine (see
[Rules engine](rules-engine.md#allowifremediatescve-remediation-fast-track)).

Every other built-in rule is off and opts in by name. `DenyIfCve` in particular can deny historical
versions an existing build depends on, if an operator enables it before the mirror is warm. Read
its [onboarding steps](../../USAGE.md#onboarding-denyifcve) first.

### Advisory database sync

The remediation fast lane and `DenyIfCve` read a synced local advisory database, not an API per
request. The compilation, ETag polling, and atomic shadow-swap are under [Rules engine → CVE
subsystem](rules-engine.md#cve-subsystem). The operator knobs (bucket, poll interval, OSV export
source, download size cap) are in [USAGE](../../USAGE.md#environment-variables). With no bucket
configured, the fast lane abstains and the age quarantine governs alone.

### Validation: fail fast, reject the unknown

Écluse validates the whole config at startup and refuses to start on any problem, never running in
a degraded state. It aggregates the errors, so one run reports every issue. An unknown name is an
error, not a silent skip:

- Écluse rejects an unknown rule `type`, and an unknown field or key. An operator authors the
  config alongside the binary, and deny-by-default protects you only if the policy you wrote is the
  policy that loaded. A typo must fail the load rather than silently stop blocking.
- Malformed values (bad URL, non-integer precedence, unparseable JSON) fail the same way.
- Merge references must resolve. Écluse rejects a `rules` entry that neither names a known default
  nor supplies a complete new rule.
- Écluse rejects a mount incoherent with its derived mode: a mirrored mount with no
  `privateUpstream`, and a serve-only mount carrying a mirror-write setting. The error names the
  offending key, and one report covers every incomplete mount.
- The mirror-write credential must resolve. Écluse rejects a non-CodeArtifact target with no static
  token, and a CodeArtifact target that carries one. It also rejects a CodeArtifact identity that
  cannot mint an initial token.
- A static publish credential requires a verifiable edge.
  `ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN` without `ECLUSE_SERVER__AUTH_TOKEN` fails the load
  as `PublishStaticCredentialNeedsEdge`. That pairing would let any unauthenticated client publish
  under Écluse's own identity.

The same validation runs without a boot. `ecluse check-config` runs the full resolution chain:
config load, runtime plan, sizing and memory-budget resolvers, and mirror-queue selection. It prints
every decision, one provenance line per resolved key, secrets redacted, precedence environment >
document > default. It exits `0` on a valid configuration, and `2` with the same aggregated report a
boot would log.

## Client authentication

Inbound auth (client to proxy) is the edge-authentication half of the [Access and credential
model](access-model.md). The mount's [credential
strategy](access-model.md#the-shipped-model-passthrough) then covers how Écluse authenticates to the
upstreams (see [Outbound registry credentials](#outbound-registry-credentials)). The client's
credential never reaches the public upstream.

Two edge modes ship. The **open** mode leaves `ECLUSE_SERVER__AUTH_TOKEN` unset and delegates
access to the network layer. The **static token** mode sets `ECLUSE_SERVER__AUTH_TOKEN`, and the
client presents it as `Bearer <token>` or as an `.npmrc` `_authToken`, which standard npm tooling
supports. A third mode is planned: a trusted edge identity asserted by a fronting proxy, IAP, or
mesh. The design honours that identity only over a verifiable binding to the edge. The full
rationale is in [access model → edge authentication](access-model.md#edge-authentication).
