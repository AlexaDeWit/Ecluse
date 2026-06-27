# Rules Engine & Responses

> Part of the [Écluse architecture overview](../architecture.md).

## Rules Engine

**Deny by default; the boot order decides.** Each rule carries a configurable
integer **precedence**. At boot the configured rule set is arranged **once** into
a single total order — **highest precedence first, then rule name ascending** as
the deterministic tiebreak — and evaluation walks that order and takes the **first
decisive result**. Every rule yields *allow*, *deny*, *no-decision*, or
*unavailable* for a given version; *allow*, *deny*, and a fail-closed *unavailable*
are **decisive**, while *no-decision* and a fail-open *unavailable* are no-ops. If
no rule is decisive, the package is denied by default. Built-in deny rules default
to a higher precedence than allow rules, so out of the box "any deny overrides any
allow" holds — but an operator can rank a specific allow above a specific deny
(e.g. to let a trusted internal scope through an install-script deny).

At **equal explicit precedence** the tie is resolved by **rule name** (the boot
order's deterministic tiebreak), **not** by a deny-over-allow priority. There is no
runtime comparison of results: the order *is* the tiebreak. Deny-over-allow still
holds in the default configuration, where the deny defaults sit strictly above the
allow defaults; only an operator who sets an allow and a deny to the *same* explicit
precedence sees that tie resolved by name instead.

Rules evaluate a single `PackageDetails` snapshot — the ecosystem-agnostic
per-version view produced by a registry adapter. A rule never sees registry wire
formats.

**Rules are ecosystem-agnostic by design.** A rule reasons only over the agnostic
`PackageDetails`; modelling an *ecosystem-specific* rule is out of scope. Where a
signal a rule reads is simply absent for an ecosystem — a declared scope on an
ecosystem with no namespacing, say — the rule yields **no decision**, which under
deny-by-default is the sensible no-op, never a per-ecosystem configuration error.
Rule **names** track the agnostic concept, not one ecosystem's mechanism (the
install-time code-execution signal, not npm's `hasInstallScript`).

**A rule is evaluation-agnostic data; one engine evaluates it.** A rule is a value of
the closed, `Eq`/`Show` data type `Rule` — *what* a rule is, carrying no evaluation.
*How* a rule decides is a separate concern: `evalRule` is the single dispatch over
that data ([`core/src/Ecluse/Core/Rules.hs`](../../core/src/Ecluse/Core/Rules.hs)). At
boot `prepare` turns each configured rule into the engine's runtime structure, a
**`PreparedRule`** — its precedence, a stable name (`ruleName`, derived from the data),
an optional **resilience policy**, and the bound per-version evaluator — and one engine
walks the boot-ordered list. A prepared rule is **pure** or **effectful** by whether it
carries a resilience policy, not by which of two tiers it lives in:

1. **Pure rules** — evaluated against `PackageDetails` with no IO. Fast and
   deterministic; `prepare` attaches no resilience (`prepResilience = Nothing`) and the
   engine runs them directly.
2. **Effectful rules** — may perform IO (advisory lookups, external policy checks).
   They carry a resilience policy (timeout / bounded retry+backoff / per-source
   circuit breaker) applied by the harness `runEffectfulRule`.

Keeping the `Rule` data closed is also a **security boundary**: untrusted config only
ever names built-in `Rule` constructors (`prepare` binds their evaluator from
`evalRule`); an arbitrary evaluator is a code-layer capability on `PreparedRule`, never
reachable from config.

There is no separate performance *tier*: the engine walks the boot order and takes
the first decisive result, so an effectful rule's IO runs **only up to the first
decisive result** — exactly the short-circuit the old two-tier skip gave, now a
consequence of the basic design. Evaluation is `IO`-typed throughout (a rule's
evaluator may do IO), so there is **no pure evaluation entry point**; a pure policy
simply launches no IO. Evaluation MAY run effectful rules speculatively **in
parallel**, but the result is always **as-if sequential by boot order**: the winner is
the *earliest-in-order* decisive rule, never the first to return in wall-clock time,
and once the winner is known every still-running strictly-later evaluation is
cancelled. The cheap pure prefix is evaluated directly, so no IO an earlier decisive
result would moot is ever launched. Determinism is non-negotiable: the boot order is
the published contract.

**Whether a rule is pure or effectful is determined by where its signal lives, not
only by whether it "feels" like IO.** Many inputs are already present in the
metadata an adapter fetches for resolution — publish age, declared scope, npm's
`hasInstallScript`, a PyPI file's `packagetype == sdist` — and support **pure**
rules. Others are *not* exposed in any metadata response and must be fetched and
parsed per version. RubyGems is the motivating case: a gem's native `extensions` —
its install-time code-execution signal, the analogue of npm's install scripts —
appears only in the gemspec inside the `.gem` (or the legacy `quick` Marshal spec),
never in the Compact Index or the JSON API (see
[`research/reverse-engineering/rubygems.md`](../research/reverse-engineering/rubygems.md)).
A rule over such a signal is necessarily **effectful**, even though it is
conceptually a simple per-version predicate. Guidance: `parseVersionDetails`
populates `PackageDetails` from the cheap metadata path; a signal that needs an
extra fetch carries a resilience policy alongside advisory lookups, and the same
logical rule (e.g. `DenyInstallTimeExecution`) may therefore be pure for one
ecosystem and effectful for another.

### Evaluation model

Each rule, applied to a `PackageDetails`, yields a `RuleResult`:

- **`Allow reason`** — the rule takes the position that this version is admissible.
  **Decisive.**
- **`Deny reason`** — the rule takes the position that it must be blocked.
  **Decisive.**
- **`NoDecision reason`** — the rule has no opinion. A no-op; the reason is retained
  for the audit trail. (Renamed from `Abstain`.)
- **`Unavailable transience alignment reason`** — the rule could not be computed (its
  IO failed, timed out, or its source breaker is open). It carries its own **failure
  alignment**: a fail-closed (`FailDeny`) `Unavailable` is **decisive**, a fail-open
  (`FailNoDecision`) `Unavailable` is a **no-op**. There is deliberately no
  fail-allow alignment — a failed check must never *admit* unvetted bytes.

Each rule carries a **precedence** (an integer; higher wins). The engine arranges
the rule set into the boot order (`bootOrder` — `(precedence descending, name
ascending)`), walks it, and takes the **first decisive result**, crediting the rule
by **name**: `Admitted name reason`, `Blocked name reason`, or `Undecidable
transience reason` (a fail-closed `Unavailable` that won). If no rule is decisive,
the result is `BlockedByDefault reasons` — deny-by-default, with each non-decisive
rule's reason collected **in boot order** so the denial response can explain what
was considered.

A rule that does not fire is a **no-op** (`NoDecision`, or a fail-open
`Unavailable`), yielding the floor to others rather than admitting or blocking on its
own. Because the **boot order** — not list position — decides, and it resolves every
equal-precedence tie by name, the decision and the credited rule are fully
order-independent: shuffling the configured rule set yields the same boot order and
hence the same `Decision`. Only the order in which the no-op reasons are gathered for
the audit trail follows the boot order.

Precedence is a **field, not an `Ord` instance**: the boot order sorts on a
`(precedence descending, name ascending)` key — there is **one comparator**,
expressed once as the construction of the ordered list, and **no runtime comparison
of competing results**. Equal precedence is legal (the boot order resolves it by
name), so a derived total `Ord` ranking *by priority* would be unlawful
(non-antisymmetric) and misleading — the same reason `Version` carries no derived
`Ord` (see [Internal Domain Model](domain-model.md)). The name tiebreak carries no
priority meaning; it only makes the order total and deterministic. The boot order is
**logged at start-up** (see
[Configuration → rule policy](configuration.md#rule-policy)) so an operator sees
exactly how their policy will resolve.

### Effectful-rule failure

An effectful rule does IO that can fail or hang (advisory source down, rate
limited, timeout). Each effectful rule has a short **timeout budget** (a couple of
seconds) with bounded **retry + backoff**, and a per-source **circuit breaker**:
after repeated failures the breaker trips and the rule fast-fails for a cooldown
(with periodic half-open probes), so a sustained outage neither adds latency to
every request nor hammers a down service.

When a rule cannot be evaluated, it yields **`Unavailable transience alignment
reason`** — a fourth `RuleResult` alongside allow/deny/no-decision — carrying its
**transience** (will-resolve vs not) and its **failure alignment**. The alignment is
the rule's own — folded into the result rather than carried as a separate failure
policy:

- **`FailDeny` (fail-closed, the default)** — the `Unavailable` is **decisive**: a
  version a needed rule could not vet is not admitted just because the scanner is
  down. A fail-closed `Unavailable` that wins becomes `Undecidable`.
- **`FailNoDecision` (fail-open)** — the `Unavailable` is a **no-op**: where a missing
  signal should not block availability (a remediation/allow-direction rule), the rule
  simply does not fire, yielding the floor to the rest. There is deliberately **no
  fail-allow** — fail-open never *admits* on its own, it only declines to decide.

The blast radius of a fail-closed `Unavailable` is small — only packages *not yet in
the private mirror* reach this path; already-approved versions are served from the
private upstream with no rules.

How a fail-closed `Undecidable` surfaces depends on the request shape:

- **Packument (metadata)** — the version is simply **filtered out**, exactly like a
  denied one (see [Applying verdicts to a packument](#applying-verdicts-to-a-packument)).
  The client's resolver just picks an admitted version; no error is raised unless
  *nothing* survives.
- **Concrete artifact** — there is one specific version, so it surfaces as an error
  via the serve [Error model](web-layer.md#error-model): `503` (+`Retry-After`) when
  transient/will-resolve, `500` when not.

Every fail-closed `Undecidable` and breaker trip emits an ERROR log + metric so infra
can detect and respond. The default is fail-closed; a rule sets its alignment to
fail-open where availability must beat safety.

### Applying verdicts to a packument

`evalRules` decides a single version, but a metadata request returns a whole
**packument** with many versions, so verdicts are applied across it. A packument is
**merged across upstreams** (see
[Registry Model → Packument merge](registry-model.md#packument-merge-across-upstreams)),
and verdicts apply **by provenance**: **gated (public-upstream)** versions are
filtered here, while **trusted (private-upstream)** versions are admitted
unfiltered, as already vetted. Filtering thus runs on the public set *before* the
merge unions it with the trusted set:

- **Filter the gated `versions`.** Every public-provenance version is evaluated;
  versions that are denied **or [undecidable](#effectful-rule-failure)** are removed
  from `versions` and from the `time` map, so a client's resolver only ever sees
  admitted versions. Trusted private versions skip this step.
- **Resolve `dist-tags.latest` — keep unless denied, prefer stable.** `latest` is
  **kept as the precedence-winning source published it** as long as that version
  survives, so `npm install <pkg>` resolves to the maintainer's chosen release,
  unchanged. Only when the chosen `latest` is itself denied or removed is it
  repointed — to the highest *stable* surviving version, falling back to the highest
  *prerelease* survivor only if no stable version survives. "Stable vs prerelease"
  is ecosystem-specific (`Ecluse.Core.Version.isStable`: semver prerelease tags, PEP 440
  pre/dev segments, RubyGems letter segments), so the packument core stays
  ecosystem-agnostic by calling the predicate. Other tags (`next`, `beta`, …) that
  point at a removed version are **dropped** rather than repointed — aiming `beta`
  at a stable release would misrepresent it.
- **No survivors → 403, 503, or 500.** If **nothing survives in the merged
  document** — no trusted private versions, and every gated public version was
  rejected — the status follows the most recoverable cause: **403** with the
  collected denial reasons when every rejection is by policy; **503**
  (+`Retry-After`) when any rejection was transient/undecidable, **or when an
  upstream the merge needed was itself unavailable** (it may resolve on retry);
  **500** when an exclusion is a permanent inability (`WontResolve`) and none is
  retryable. Never 404 — the package exists; its versions were withheld. See the
  serve [Error model](web-layer.md#error-model).

Repointing `latest` downward when its target is denied is a deliberate downgrade,
and it is the resilience posture — a not-yet-cleared (or actively bad) release does
not silently remain the default install once it has been withheld. The rule never
*promotes*, though: a higher prerelease is not elevated over a maintainer's chosen
stable `latest` (npm keeps `latest` on the last stable release even when a higher
prerelease exists), and a surviving `latest` is left exactly as published — so the
single-source case is the identity. Because the filtered body differs from
upstream's, the proxy computes its **own** response validators (`ETag`) over the
filtered body rather than relaying upstream's (see
[Web Layer](web-layer.md#web-layer)).

### Initial Rule Set

| Rule | Type | Description |
|------|------|-------------|
| `AllowIfPublishedBefore ageSeconds` | Pure | Allows a package version if it was published more than `ageSeconds` seconds ago. Default: 604800 (7 days). Guards against typosquatting and dependency confusion attacks where attackers race to publish before detection. |
| `AllowScope scope` | Pure | Unconditionally allows all packages under a given npm scope (e.g. `@myorg`). Use for internal scopes that bypass public-registry rules. |
| `DenyInstallTimeExecution` | Pure | Denies any version flagged with an install-time code-execution signal (npm's `hasInstallScript`, a RubyGems native extension, a PyPI sdist) — a common arbitrary-code-execution vector. Yields no decision otherwise. As a deny rule it overrides any allow at its higher default precedence. |

Further rules are added as later phases — the **effectful** CVE rules
[`DenyIfCVE` and `AllowIfRemediatesCve`](#cve-subsystem), and effectful per-version
checks like RubyGems native `extensions` (see [above](#rules-engine)).

Which rules ship **enabled by default** is a policy choice documented with the
[default policy](configuration.md#the-default-policy): at launch only the pure
`AllowIfPublishedBefore` quarantine is on; `AllowIfRemediatesCve` joins the default
when the CVE rules land; the install-script and CVE *denies* stay available but
opt-in.

## CVE Subsystem

Effectful rules read the **same synced advisory data in two directions**: a
**deny** direction — `DenyIfCVE` blocks a version that *is* affected by a known
advisory — and an **allow** direction — `AllowIfRemediatesCve` *fast-tracks* a
version that **fixes** one. Rather than call an advisory API per evaluation, Écluse
**syncs a local copy of the dataset and queries it in memory**: the `CVELookup`
handle reads a local index, never the network, on the hot path.

### `AllowIfRemediatesCve` — remediation fast-track

A publish-age quarantine (`AllowIfPublishedBefore`) has one perverse failure mode:
left alone it would also hold back the **security patch** that fixes an in-the-wild
vulnerability, delaying remediation by exactly the window meant to catch
typosquats. `AllowIfRemediatesCve` removes that tension. For version *V* of package
*P* it consults `CVELookup` and takes a position:

- **`Allow`** when an advisory affects an **earlier** version of *P* and *V* falls
  **outside** that advisory's affected range — i.e. *V* is the fix. The reason
  names the remediated advisory IDs (audit trail).
- **`NoDecision`** otherwise, and a **fail-open** (`FailNoDecision`) `Unavailable`
  **when the lookup itself fails.** This is the deliberate inverse of `DenyIfCVE`'s
  failure mode: a deny that cannot confirm *safety* fails **closed**
  (`FailDeny` → `Undecidable`, [below](#effectful-rule-failure)), but an allow that
  cannot confirm a *remediation* fails **open** — it simply does not fire, so the
  version falls back to the normal quarantine path rather than being admitted on an
  unverified claim. A CVE-source outage thus costs security patches their fast lane
  (they wait out `min-age`) but never admits anything it could not vouch for.

It is ranked **above** the quarantine allow so the fix is admitted **immediately**.
Both this rule and `DenyIfCVE` are decided locally against the synced advisory
ranges using the same per-ecosystem ordering as
[`compareVersions`](domain-model.md) — the allow direction just asks "is *V* past
the fix boundary?" instead of "is *V* inside the affected range?".

### Local sync, in memory

A supervised in-process background task periodically pulls **OSV's per-ecosystem
advisory exports** (`gs://osv-vulnerabilities/<ecosystem>/all.zip`) — one dataset
per supported ecosystem, under one schema — and parses each into a compact
in-memory index (package → affected version-ranges + advisory IDs), which is
**atomically swapped** in. The download is transient (stream-unzipped, or a temp
file deleted immediately), so there is **no persistent writable-disk requirement**
— the footprint is RAM: tens of MB per ecosystem, the index smaller than the raw
export. OSV is chosen as the aggregator — it covers npm/PyPI/RubyGems under one
schema and ships dumps built for mirroring, so a single mechanism serves every
ecosystem. The task feeds request-path rule evaluation, so it travels with the
server (see [Process model](cloud-backends.md#process-model)).

Syncing rather than looking up on demand removes the **one external dependency
that would otherwise sit under the deliberately fail-closed gate** (see
[Effectful-rule failure](#effectful-rule-failure)): an advisory-source outage
becomes **sync lag** — the last-good index keeps serving, with an alarm — instead
of per-package blocking. Lookups also leave the hot path entirely; and the cold
path is already rare, since a version is checked only *before* it is mirrored,
after which it is served rule-free, so lookup volume tracks first-time-seen
versions, not requests.

- **Cold start** — until the first sync lands the index is empty, so
  [readiness](web-layer.md#meta-routes-ping-health-and-search) gates on
  *first-sync-complete*: the proxy is not marked ready until advisories are loaded.
- **Sync failure** — keep the last-good index and alarm; never drop to an empty
  index on a failed refresh.
- **Version matching** — owned locally: a version is tested against an advisory's
  affected ranges using the same per-ecosystem ordering as
  [`compareVersions`](domain-model.md). (Owned whichever source is used — every
  source returns ranges.)

### Point-in-time gating — a known limitation

CVE gating happens **at ingestion**: a version is checked once, before it enters
the mirror. A CVE disclosed *after* a version is mirrored is **not** caught — the
private upstream serves it rule-free thereafter. Catching post-mirror disclosures
needs **periodically re-scanning the mirror** (quarantine/remove affected
versions) against the same local dataset; that is its own feature and is
**deferred**. Holding the dataset locally makes it straightforward to add later.

### Testing

Because the index is in memory and refreshed on a schedule, tests assert a
**bounded, self-cleaning footprint**: memory stays bounded across repeated syncs
(the old index is released — no growth or leak), the swap is atomic (no torn reads
mid-refresh), a failed sync retains the last-good index (and alarms), readiness
gates on first sync, and any transient download scratch is cleaned up. (A future
on-disk cache would instead add rotation / bounded-disk tests.)

## Denial Responses

When a request is denied (no allow rule matched, or a deny rule fired):

- HTTP **status** is decided by the agnostic serve layer (403 for policy denials);
  see [Web Layer → Error model](web-layer.md#error-model).
- The response **body shape is the mount's** — its
  [error renderer](hosting.md#mounts) shapes the bytes in the ecosystem's surface,
  so the agnostic layer holds no body shape of its own. For npm the renderer
  (`Ecluse.Core.Registry.Npm.Serve`) emits the npm error object:
  ```json
  {
    "error": "Package @evil/pkg@1.0.0 was denied: AllowIfPublishedBefore — published 3 hours ago, minimum age is 7 days. Contact #platform-eng on Slack for assistance."
  }
  ```
- The denial reason (which rule decided, and why) is always included.
- `PROXY_HELP_MESSAGE`, if configured, is appended to every denial (the
  ecosystem-neutral `appendHelp`, before the renderer wraps it).
