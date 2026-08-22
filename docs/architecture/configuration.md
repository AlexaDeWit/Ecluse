# Configuration and authentication

> Part of the [Écluse architecture overview](../architecture.md).

Why Écluse's configuration has the shape it has: two layers, a derived mount, safety floors that
fail closed, and client authentication at the edge.

## Configuration

> **Operators:** [`config/default.yaml`](../../config/default.yaml) documents every key and its
> default, and [`USAGE.md`](../../USAGE.md#configuration) covers the environment-variable mapping,
> client setup, and the network-egress checklist. This document holds the design rationale.

Configuration has two layers. Environment variables carry process-level and secret values. A
structured YAML document carries the two things too expressive for flat env vars: the **rule
policy** and the **mount map**. The rule policy earns the document its keep: per-rule
precedence and value overrides, layered over a built-in default (see [Rule policy](#rule-policy)).

A mount's shape is **derived, not declared**. Any operator-supplied key under `mounts.<ecosystem>`
activates it, and a declared `mirrorTarget`, not a mode flag, makes it mirrored. A mirrored mount
then requires a `privateUpstream` so the mirror reads back.

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

A credential is always its own key, never part of an endpoint. Écluse refuses a registry URL
carrying userinfo, a query string, or a fragment at load, and the error names the key. That refusal
lets the boot-time configuration echo, the endpoint-collision warnings, and the mount posture lines
print a configured endpoint as written.

The mirror-write credential is **derived from the mirror-target URL**. It is always the credential
that endpoint dictates, and Écluse can never pair it with an endpoint it was not minted for. A
CodeArtifact endpoint (`{domain}-{owner}.d.codeartifact.{region}.amazonaws.com`) encodes its whole
mint identity in its host, so the worker mints a short-lived token scoped to that domain. The
worker writes any other host with a static token (`…MIRROR_TARGET_TOKEN`).

The CodeArtifact mint is per
domain, so mounts whose resolved identities coincide share one
[`CredentialProvider`](cloud-backends.md#credential-provider): one mint, one refresh, one breaker.

### Outbound egress safety

Écluse constrains its own outbound fetches. It applies an https-only host allowlist, a literal
internal-range block on the `dist.tarball` host, and certificate validation that authenticates the
dialled host. Network egress is still a shared responsibility: the deployment must also fence
egress at the platform layer, with security groups, `NetworkPolicy`, or Istio egress policy. See
[Securing network egress](../../USAGE.md#securing-network-egress-required).

Two application-level knobs adjust threat tolerance. One relaxes *which allowlisted host* may serve
a tarball, never whether the allowlist or the internal-range block applies. The other widens the
fixed internal-range set with operator-supplied CIDRs. See
[USAGE](../../USAGE.md#environment-variables) for the names and values.

### Runtime sizing: cores and heap ceiling

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

Inbound auth (client to proxy) is the edge half of the credential model. Écluse authenticates to
the upstreams per [Credential flow and authority](registry-model.md#credential-flow-and-authority),
and the client's credential never reaches the public upstream.

Two edge modes ship. The **open** mode leaves `ECLUSE_SERVER__AUTH_TOKEN` unset and delegates
access to the network layer. The **static token** mode sets `ECLUSE_SERVER__AUTH_TOKEN`, and the
client presents it as `Bearer <token>` or as an `.npmrc` `_authToken`, which standard npm tooling
supports.
