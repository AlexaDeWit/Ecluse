# Rules engine and responses

> Part of the [Écluse architecture overview](../architecture.md).

## Rules engine

**Deny by default; the boot order decides.** Each rule carries a configurable integer precedence.
At boot the rule set is arranged once into a single total order (highest precedence first, then
rule name ascending as the deterministic tiebreak), and evaluation walks that order and takes the
first decisive result. If nothing is decisive, the package is denied. Equal precedence is resolved
by name, not by a deny-over-allow priority: the order *is* the tiebreak, so shuffling the
configured set yields the same decision. Built-in deny rules default above allow rules, so "any
deny overrides any allow" holds out of the box, but an operator can rank a specific allow above a
specific deny (say, to let a trusted internal scope through an install-script deny).

Rules evaluate a single `PackageDetails` snapshot, the ecosystem-agnostic per-version view an
adapter produces (see [The internal domain model](registry-model.md#the-internal-domain-model)); a rule never sees registry wire
formats, and rule names track the agnostic concept, not one ecosystem's mechanism (the install-time
code-execution signal, not npm's `hasInstallScript`). Where a signal a rule reads is simply absent
for an ecosystem, the rule yields no decision, the sensible no-op under deny-by-default, never a
configuration error.

A `Rule` is closed `Eq`/`Show` data with no evaluation; `evalRule` is the single dispatch over it
([`Ecluse.Core.Rules`](../../core/src/Ecluse/Core/Rules.hs)). Keeping `Rule` closed is a security
boundary: untrusted config can only name a built-in constructor, never supply an evaluator. A rule
is **pure or effectful** by whether it carries a resilience policy, depending only on where its
signal lives, so `DenyInstallTimeExecution` is pure for npm's `hasInstallScript` but effectful for a
RubyGems native `extensions` signal that appears only inside the `.gem`.

### Evaluation model

Each rule, applied to a `PackageDetails`, yields a `RuleVerdict`, a deterministic answer, never a
fault:

- **`Allow`** and **`Deny`**: the version is admissible, or must be blocked. Decisive.
- **`NoDecision`**: no opinion. A no-op; the reason is retained for the audit trail.
- **`CannotVet alignment`**: the rule reached the version but cannot vet it deterministically and
  in-process (today: no advisory database is loaded). It carries its own failure alignment (below).
  There is deliberately no fail-allow: a check that cannot vet must never admit unvetted bytes.

Under its resilience harness a rule either returns a decided verdict, taken at face value, or the
harness synthesises `Unavailable` when it could obtain no verdict at all (the IO faulted, timed out,
or the breaker is open). The engine walks the boot order and credits the winning rule by name; with
nothing decisive it collects each non-decisive reason, in boot order, so the denial can explain what
was considered. The full verdict and harness vocabulary is in the
[`Ecluse.Core.Rules`](../../core/src/Ecluse/Core/Rules.hs) Haddock; the boot order is logged at
start-up (see [Configuration → rule policy](configuration.md#rule-policy)).

### Effectful-rule failure

An effectful rule does IO that can fail or hang. Each carries a short per-attempt timeout with
bounded retry and backoff and a per-source circuit breaker: after repeated failures the breaker
trips and the rule fast-fails for a cooldown, so a sustained outage neither adds latency to every
request nor hammers a down service. The shipped defaults are a **2-second per-attempt timeout, two
retries at 100ms then 250ms, and a breaker tripping after 5 consecutive failures and cooling for 30
seconds.**

A fault the harness observes becomes `Unavailable`; a deterministic in-process absence a rule
reports as `CannotVet` (no advisory database loaded) is taken at face value, never retried and never
counted towards the breaker, because no retry could change it. Either is governed by the rule's
failure alignment:

- **`FailDeny` (fail-closed, the default)**: decisive. A version a needed rule could not vet is not
  admitted just because the scanner is down or the advisory database is not yet loaded.
- **`FailNoDecision` (fail-open)**: a no-op, for a remediation or allow-direction rule where a
  missing signal should not block availability. There is deliberately no fail-allow.

The blast radius is small: only packages not yet in the private mirror reach this path;
already-approved versions serve from the private upstream with no rules. How a fail-closed failure
surfaces depends on the request shape: on a packument the version is filtered out like a denied one
(no error unless nothing survives); on a concrete artifact it surfaces via the
[error model](web-layer.md#error-model) as `503` (with `Retry-After`) when transient, `500` when
not. Every fail-closed undecidable result and breaker trip emits an ERROR log and metric.

### Applying verdicts to a packument

Evaluation decides a single version, but a metadata request returns a whole packument, so verdicts
are applied across it (for the cross-upstream merge, see
[Registry model → Packument merge](registry-model.md#packument-merge-across-upstreams)).

- **Resolve `dist-tags.latest`: keep unless denied, prefer stable.** `latest` is kept as published
  as long as that version survives, so `npm install <pkg>` resolves to the maintainer's chosen
  release. Only when the chosen `latest` is itself denied or removed is it repointed, to the highest
  stable surviving version, falling back to the highest prerelease survivor only if no stable version
  survives. "Stable vs prerelease" is ecosystem-specific (`Ecluse.Core.Version.isStable`), so the
  core stays agnostic by calling the predicate. Other tags (`next`, `beta`) at a removed version are
  dropped rather than repointed. The rule never promotes a higher prerelease over a chosen stable
  `latest`; repointing downward is a deliberate downgrade, so a withheld release does not silently
  remain the default install.
- **No survivors → 403, 503, or 500.** If nothing survives, the status follows the most recoverable
  cause: `403` with the collected denial reasons when every rejection is by policy; `503` (with
  `Retry-After`) when any rejection was transient or a needed upstream was unavailable; `500` when an
  exclusion is a permanent inability and none is retryable. Never `404`: the package exists, its
  versions were withheld. The HTTP status mapping belongs to the [error model](web-layer.md#error-model).

Because the filtered body differs from upstream's, the proxy computes its own `ETag` over the
filtered body (see [Web layer](web-layer.md#web-layer)).

### Initial rule set

| Rule | Type | Description |
|------|------|-------------|
| `AllowIfOlderThan ageSeconds` | Pure | Allows a version published more than `ageSeconds` ago. Default: 604800 (7 days). Guards against typosquatting and dependency confusion, where attackers race to publish before detection. |
| `AllowIfRemediatesCve` | Effectful | Allows a version a synced advisory names as its exact fixed version, provided no advisory still affects it: the [remediation fast lane](#allowifremediatescve-remediation-fast-track) past the quarantine. Abstains when it cannot confirm a remediation, including before a first advisory sync. |
| `AllowScope scope` | Pure | Unconditionally allows all packages under a given npm scope (e.g. `@myorg`). Use for internal scopes that bypass public-registry rules. |
| `AllowByIdentity identity` | Pure | Allows a specific package or `package@version` by exact identity: the allow twin of `DenyByIdentity`. Ranks above `DenyIfCve` (an identity pin overrides an advisory deny) but below the install-script and revocation denies. |
| `DenyInstallTimeExecution` | Pure | Denies any version flagged with an install-time code-execution signal (npm's `hasInstallScript`, a RubyGems native extension, a PyPI sdist), a common arbitrary-code-execution vector. Yields no decision otherwise, and overrides any allow at its higher default precedence. |
| `DenyByIdentity identity` | Pure | A hard deny for a specific package or `package@version`, at the top precedence: the post-mirror revocation mechanism. |
| `DenyIfCve params` | Effectful | Opt-in. Denies a version a synced advisory records as affected at or above a CVSS `minSeverity`; an unscored advisory (the npm malware feed carries no score) counts as above every threshold, so it blocks malware too. Ranked below `AllowByIdentity`; its `onUnavailable` fails closed by default. See the [deny direction](#denyifcve-the-deny-direction). |

The **default precedence ladder** climbs from most-passive to most-decisive:

```text
AllowIfOlderThan (100) < AllowIfRemediatesCve (150) < AllowScope (200) <
DenyIfCve (225) < AllowByIdentity (250) < DenyInstallTimeExecution (300) <
DenyByIdentity (400)
```

`DenyInstallTimeExecution` and `DenyByIdentity` default strictly above every allow, so "any deny
overrides any allow" holds for them out of the box. `DenyIfCve` is the deliberate exception: it sits
**below** `AllowByIdentity` (225 against 250) so an operator's exact-identity allow overrides an
advisory deny, the explicit "I have decided this version must ship" escape hatch, while still sitting
above the passive age gate, the remediation lane, and a scope allow-list. An operator may raise a
specific allow above a specific deny (or the reverse) with an explicit precedence.

Which rules ship enabled is documented with the [default policy](configuration.md#the-default-policy):
the pure `AllowIfOlderThan` quarantine (`min-age`) and the `AllowIfRemediatesCve` fast lane, which
abstains when no advisory database is configured so only the quarantine governs. Every other rule is
off by default and opts in by name.

## CVE subsystem

The advisory subsystem reads a synced local copy rather than calling an advisory API per evaluation:
the `CveLookup` handle ([`Ecluse.Core.Cve`](../../core/src/Ecluse/Core/Cve.hs)) reads the synced
`osv.db` SQLite artifact on local disk, never the network, on the hot path. It models an advisory's
affected set faithfully: range bounds (inclusive `introduced`, exclusive `fixed` or inclusive
`last_affected`), exactly-enumerated versions as points, and a numeric CVSS base score per advisory.
Access is acquisition-bracketed per evaluation, so the [shadow-swap](#local-polling-decoupled-ingestion)
can retire a superseded artifact the moment no evaluation still reads it. Two rules read it in
opposite directions.

### `AllowIfRemediatesCve`, remediation fast-track

A publish-age quarantine would also hold back the security patch that fixes an in-the-wild
vulnerability, delaying remediation by exactly the window meant to catch typosquats.
`AllowIfRemediatesCve` removes that tension. For version *V* of *P*:

- **`Allow`** when an advisory names *V* as its exact fixed version and no advisory's affected range
  still contains *V*. The reason names the remediated advisory IDs.
- **`NoDecision`** otherwise (including before a first sync), and a fail-open `Unavailable` when a
  lookup against a loaded database faults. An allow that cannot confirm a remediation **fails open**
  (unlike a deny, which fails closed), so the version falls back to the normal quarantine rather than
  being admitted on an unverified claim.

It ranks above the quarantine allow (so a fix is admitted immediately) and below the scope allow-list
(so a trusted scope never pays the probe). The fix test is a deliberate exact string match on the
advisory's canonical `fixed` version; a fix published under any other string waits out the quarantine,
with `AllowByIdentity` as the operator's workaround. Range membership is decided in Haskell using the
same per-ecosystem ordering as [`compareVersions`](registry-model.md#the-internal-domain-model), with every unprovable comparison
counting as affected, so the lane only opens on evidence.

### `DenyIfCve`, the deny direction

`DenyIfCve` reads the same lookup to block version *V* of *P* when an advisory affects *V* at or above
a configured CVSS `minSeverity`. It is **opt-in** and does two jobs against the npm feed. Most of that
feed is the malware feed (`MAL-*` advisories that carry no CVSS score and name the bad version
exactly); a smaller share is CVSS-scored CVEs. An unscored advisory counts as **above every
threshold**, so malware is always denied while `minSeverity` governs the scored CVEs.

- **`Deny`** when some advisory affects *V* and clears the threshold. The reason names the deciding
  advisories.
- **`NoDecision`** when no affecting advisory clears it.
- **`CannotVet`** when no advisory database is loaded, and the harness's **`Unavailable`** when a
  loaded-database lookup faults. Both align by `onUnavailable`: **`FailDeny`** (the default) refuses
  the version (a retryable `503`); **`FailNoDecision`** skips the rule, logging loudly. This is the
  inverse of `AllowIfRemediatesCve`: neither an allow nor a deny that cannot confirm safety may admit.

Because enabling it on a cold mirror can deny historical versions an existing build depends on, it
ships off; operators warm the mirror first (see USAGE → *Onboarding DenyIfCve*).

### Local polling, decoupled ingestion

Rather than parse raw JSON advisory dumps on the proxy (heavy GC pressure and memory spikes), Écluse
uses a decoupled pipeline, **Écluse Pilot**: a standalone service that pulls OSV's per-ecosystem
exports, compiles them into a read-only SQLite database (`osv.db`), and pushes it to a private S3/GCS
bucket. `advisories.bucket` names that bucket; unset, the advisory stack is off.

The proxy runs one supervised sync task per configured mount ecosystem
([`Ecluse.Runtime.Cve.Sync`](../../runtime/src/Ecluse/Runtime/Cve/Sync.hs)), each polling the bucket's
stable per-ecosystem key for ETag changes at `advisories.pollInterval` (deliberately more frequent
than Pilot's compile interval, since matching them would nearly double the worst-case advisory age).
The tasks are independent, so one ecosystem's missing artifact never holds back another's. A newly
detected `osv.db` is downloaded to a temp file, byte-bounded by `advisories.maxDatabaseBytes`, and,
treated as untrusted even behind the bucket's access controls, accepted only after a cheapest-first
verification (epoch stamp, integrity scan, the required tables' strict-schema conformance, ecosystem).
The accepted file is renamed atomically and shadow-swapped into the read path
([`Ecluse.Core.Cve.Slot`](../../core/src/Ecluse/Core/Cve/Slot.hs)): the swap waits for the displaced
generation's readers to drain, so pruning is the kernel's reclamation, never a mistimed delete. A
refused artifact is discarded, its ETag remembered, and the last-good generation keeps serving.
[Readiness](web-layer.md#meta-routes-ping-health-and-search) waits for each ecosystem's first sync
while the listener serves throughout: an absent database only abstains into deny-by-default.

Polling removes the one external dependency that would otherwise sit under the fail-closed gate: an
advisory-source outage becomes sync lag, not per-package blocking. Lookups also leave the hot path,
since a version is checked only before it is mirrored and served rule-free after.

#### The artifact contract

The object key is stable per ecosystem and embeds the table-schema epoch,
`<ecosystem>-osv-schema<N>.db` (currently `npm-osv-schema3.db`), which is what makes ETag polling
work. The epoch is a hand-bumped constant shared by the Pilot writer and the proxy reader
([`Ecluse.Core.Osv.Schema`](../../core/src/Ecluse/Core/Osv/Schema.hs)) and stamped inside the artifact
as SQLite's `user_version`; a mismatch keeps the last-good database and alarms. The artifact is
**immutable and rebuilt from scratch** on every compilation, so there are no migrations, only a
read-compatibility contract. The epoch moves only for a breaking change; additive changes (a new
column or table) do not bump it, because readers select explicit columns. Pilot filters rows to the
target ecosystem so an advisory spanning two ecosystems does not leak foreign package rows. Each
denial's audit log records the advisory database ETag live at emit (`active_advisory_db_etag`),
deliberately the one live at emit rather than the one the verdict was evaluated against, since a
shadow-swap can land mid-request.

### Point-in-time gating, a known limitation

CVE gating happens at ingestion: a version is checked once, before it enters the mirror, and served
rule-free thereafter, so a CVE disclosed after a version is mirrored is not caught by the gate. The
post-ingestion disposition (operator scanning, a hard deny-by-identity revocation, and operator purge,
*deny-then-purge*) is catalogued in the [threat model](https://ecluse-proxy.com/threat-model.html);
holding the dataset locally keeps a periodic mirror re-scan straightforward to add later.

## Denial responses

When a request is denied (no allow rule matched, or a deny rule fired):

- HTTP status is decided by the agnostic serve layer (403 for policy denials; see
  [Web layer → Error model](web-layer.md#error-model)).
- The response body shape is the ecosystem's: the route contract supplies the typed response
  constructor and codec, so the agnostic pipeline holds no body shape of its own. For npm the codec
  ([`Ecluse.Core.Registry.Npm.Serve`](../../core/src/Ecluse/Core/Registry/Npm/Serve.hs)) emits the
  npm error object:
  ```json
  {
    "error": "Package @evil/pkg@1.0.0 was denied: AllowIfOlderThan, published 3 hours ago, minimum age is 7 days. Contact #platform-eng on Slack for assistance."
  }
  ```
- The denial reason (which rule decided, and why) is always included.
- `ECLUSE_SERVER__HELP_MESSAGE`, if configured, is appended to every denial (the ecosystem-neutral
  `appendHelp`, before the renderer wraps it).
