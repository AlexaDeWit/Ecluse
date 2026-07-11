# Rules engine and responses

> Part of the [Écluse architecture overview](../architecture.md).

## Rules engine

**Deny by default; the boot order decides.** Each rule carries a configurable integer precedence.
At boot the rule set is arranged once into a single total order (highest precedence first, then
rule name ascending as the deterministic tiebreak), and evaluation walks it and takes the first
decisive result. Every rule yields *allow*, *deny*, *no-decision*, or *cannot-vet* (a deterministic
inability to vet, such as no advisory database being loaded); allow, deny, and a fail-closed
cannot-vet are decisive, while no-decision and a fail-open cannot-vet are no-ops. A rule whose
evaluation faults under its resilience harness resolves the same way, by its fail-closed or
fail-open alignment. If nothing is decisive, the package is denied by default. Built-in deny rules default above
allow rules, so "any deny overrides any allow" holds out of the box, but an operator can rank a
specific allow above a specific deny (e.g. to let a trusted internal scope through an install-script
deny). At equal explicit precedence the tie is resolved by rule name, not by a deny-over-allow
priority: the order *is* the tiebreak, there is no runtime comparison of results.

Rules evaluate a single `PackageDetails` snapshot, the ecosystem-agnostic per-version view a
registry adapter produces; a rule never sees registry wire formats. Modelling an ecosystem-specific
rule is out of scope: where a signal a rule reads is simply absent for an ecosystem (a declared
scope on an ecosystem with no namespacing), the rule yields no decision, the sensible no-op under
deny-by-default, never a configuration error. Rule names track the agnostic concept, not one
ecosystem's mechanism (the install-time code-execution signal, not npm's `hasInstallScript`).

A rule is evaluation-agnostic data; one engine evaluates it. A rule is a value of the closed,
`Eq`/`Show` data type `Rule`, carrying no evaluation; `evalRule` is the single dispatch over it
([`core/src/Ecluse/Core/Rules.hs`](../../core/src/Ecluse/Core/Rules.hs)). At boot `prepare` turns
each configured rule into a `PreparedRule`: its precedence, a stable name (`ruleName`, derived from
the data), an optional resilience policy, and the bound per-version evaluator. Keeping `Rule` closed
is a security boundary: untrusted config only ever names built-in constructors (`prepare` binds
their evaluator from `evalRule`); an arbitrary evaluator is a code-layer capability, never reachable
from config. A prepared rule is pure or effectful by whether it carries a resilience policy:

1. **Pure rules** evaluate against `PackageDetails` with no IO (`prepResilience = Nothing`); the
   engine runs them directly.
2. **Effectful rules** may perform IO (advisory lookups, external policy checks) and carry a
   resilience policy (timeout / bounded retry+backoff / per-source circuit breaker) applied by the
   harness `runEffectfulRule`.

There is no separate performance tier: because the engine takes the first decisive result, an
effectful rule's IO runs only up to that point. Evaluation is `IO`-typed throughout, so there is no
pure evaluation entry point; a pure policy simply launches no IO. Evaluation may run effectful rules
speculatively in parallel, but the result is always as-if sequential by boot order: the winner is
the earliest-in-order decisive rule, never the first to return in wall-clock time, and every
still-running strictly-later evaluation is cancelled once the winner is known. The cheap pure prefix
is evaluated directly, so no IO an earlier decisive result would moot is launched. Determinism is
the published contract.

Whether a rule is pure or effectful depends on where its signal lives. Many inputs are already in
the metadata an adapter fetches (publish age, declared scope, npm's `hasInstallScript`, a PyPI
file's `packagetype == sdist`) and support pure rules. Others are not exposed in any metadata
response and must be fetched per version: RubyGems is the motivating case, where a gem's native
`extensions` signal appears only in the gemspec inside the `.gem`, never in the Compact Index or
JSON API (see
[`research/reverse-engineering/rubygems.md`](../research/reverse-engineering/rubygems.md)). A rule
over such a signal is necessarily effectful even though it is conceptually a simple per-version
predicate, so the same logical rule (e.g. `DenyInstallTimeExecution`) may be pure for one ecosystem
and effectful for another.

### Evaluation model

Each rule, applied to a `PackageDetails`, yields a `RuleVerdict` -- a deterministic answer, never a
fault:

- **`Allow reason`**, the version is admissible. Decisive.
- **`Deny reason`**, the version must be blocked. Decisive.
- **`NoDecision reason`**, no opinion. A no-op; the reason is retained for the audit trail.
- **`CannotVet alignment reason`**, the rule reached the version but cannot vet it deterministically
  and in-process (today: no advisory database is loaded). It carries its own failure alignment: a
  fail-closed (`FailDeny`) `CannotVet` is decisive, a fail-open (`FailNoDecision`) one is a no-op.
  There is deliberately no fail-allow: a check that cannot vet must never admit unvetted bytes.

Running a rule under its resilience harness produces a `RuleEvaluation`: either `Decided` with the
rule's `RuleVerdict`, taken at face value, or `Unavailable transience alignment reason` -- the one
outcome a rule cannot report about itself, synthesised only by the harness when it could not obtain a
verdict at all (the IO failed, timed out, or the source breaker is open). Because a rule cannot
manufacture an `Unavailable`, the retry and breaker machinery provably reacts only to a fault the
harness observed, never to a verdict a rule returned (a deterministic `CannotVet` included).

The engine arranges the rules into `bootOrder` (`(precedence descending, name ascending)`), walks
it, and takes the first decisive result, crediting the rule by name: `Admitted name reason`,
`Blocked name reason`, or `Undecidable transience reason` (a fail-closed `CannotVet` verdict or
harness `Unavailable` that won). If
nothing is decisive, the result is `BlockedByDefault reasons`, with each non-decisive rule's reason
collected in boot order so the denial response can explain what was considered. Because the boot
order (not list position) decides and resolves every equal-precedence tie by name, the decision and
credited rule are fully order-independent: shuffling the configured set yields the same `Decision`.

Precedence is a field, not an `Ord` instance: the boot order sorts on one comparator, built once,
with no runtime comparison of competing results. Equal precedence is legal (resolved by name), so a
derived total `Ord` by priority would be unlawful (non-antisymmetric), the same reason `Version`
carries no derived `Ord` (see [Internal Domain Model](domain-model.md)). The boot order is logged at
start-up (see [Configuration → rule policy](configuration.md#rule-policy)).

### Effectful-rule failure

An effectful rule does IO that can fail or hang. Each has a short timeout budget (a couple of
seconds) with bounded retry+backoff and a per-source circuit breaker: after repeated failures the
breaker trips and the rule fast-fails for a cooldown (with periodic half-open probes), so a
sustained outage neither adds latency to every request nor hammers a down service. A **fault** the
harness observes (the IO threw, timed out, or the breaker is open) becomes an `Unavailable`, carrying
its transience (will-resolve vs not) and the rule's failure alignment. A **deterministic in-process
absence** a rule reports as a `CannotVet` verdict (no advisory database loaded) is taken at face
value instead: never retried and never counted towards the breaker, because no retry could change it.
Either path is governed by the same alignment:

- **`FailDeny` (fail-closed, the default)**: decisive, a version a needed rule could not vet is not
  admitted just because the scanner is down or the advisory database is not yet loaded. A fail-closed
  `CannotVet` or `Unavailable` that wins becomes `Undecidable`.
- **`FailNoDecision` (fail-open)**: a no-op, for a remediation/allow-direction rule where a missing
  signal should not block availability. There is deliberately no fail-allow.

The blast radius is small: only packages not yet in the private mirror reach this path;
already-approved versions serve from the private upstream with no rules. How a fail-closed
`Undecidable` surfaces depends on the request shape: on a packument the version is filtered out like
a denied one (no error unless nothing survives); on a concrete artifact it surfaces via the serve
[Error model](web-layer.md#error-model) as `503` (+`Retry-After`) when transient, `500` when not.
Every fail-closed `Undecidable` and breaker trip emits an ERROR log and metric.

### Applying verdicts to a packument

`evalRules` decides a single version, but a metadata request returns a whole packument, so verdicts
are applied across it (for how packuments are merged across upstreams and how trusted vs gated
provenances combine, see
[Registry Model → Packument merge](registry-model.md#packument-merge-across-upstreams)).

- **Resolve `dist-tags.latest`, keep unless denied, prefer stable.** `latest` is kept as the
  precedence-winning source published it as long as that version survives, so `npm install <pkg>`
  resolves to the maintainer's chosen release. Only when the chosen `latest` is itself denied or
  removed is it repointed, to the highest stable surviving version, falling back to the highest
  prerelease survivor only if no stable version survives. "Stable vs prerelease" is
  ecosystem-specific (`Ecluse.Core.Version.isStable`), so the packument core stays agnostic by
  calling the predicate. Other tags (`next`, `beta`, …) pointing at a removed version are dropped
  rather than repointed. The rule never promotes: a higher prerelease is not elevated over a
  maintainer's chosen stable `latest`, and a surviving `latest` is left exactly as published, so
  the single-source case is the identity. Repointing downward is a deliberate downgrade: a
  not-yet-cleared or actively bad release does not silently remain the default install once
  withheld.
- **No survivors → 403, 503, or 500.** If nothing survives the merged document, the status follows
  the most recoverable cause: `403` with the collected denial reasons when every rejection is by
  policy; `503` (+`Retry-After`) when any rejection was transient/undecidable or a needed upstream
  was unavailable; `500` when an exclusion is a permanent inability (`WontResolve`) and none is
  retryable. Never `404`: the package exists, its versions were withheld. See the
  [Error model](web-layer.md#error-model).

Because the filtered body differs from upstream's, the proxy computes its own `ETag` over the
filtered body rather than relaying upstream's (see [Web Layer](web-layer.md#web-layer)).

### Initial rule set

| Rule | Type | Description |
|------|------|-------------|
| `AllowIfOlderThan ageSeconds` | Pure | Allows a package version if it was published more than `ageSeconds` seconds ago. Default: 604800 (7 days). Guards against typosquatting and dependency confusion attacks where attackers race to publish before detection. |
| `AllowIfRemediatesCve` | Effectful | Allows a version a synced advisory names as its exact fixed version, provided no advisory still affects it: the [remediation fast lane](#allowifremediatescve-remediation-fast-track) past the quarantine. Abstains when it cannot confirm a remediation (including before a first advisory sync). |
| `AllowScope scope` | Pure | Unconditionally allows all packages under a given npm scope (e.g. `@myorg`). Use for internal scopes that bypass public-registry rules. |
| `AllowByIdentity identity` | Pure | Allows a specific package or `package@version` by exact identity: the allow twin of `DenyByIdentity` and the explicit operator lane for a fix the exact-match probe cannot see. Top of the allow band, still below every deny. |
| `DenyInstallTimeExecution` | Pure | Denies any version flagged with an install-time code-execution signal (npm's `hasInstallScript`, a RubyGems native extension, a PyPI sdist), a common arbitrary-code-execution vector. Yields no decision otherwise. As a deny rule it overrides any allow at its higher default precedence. |
| `DenyByIdentity identity` | Pure | A hard deny for a specific package or `package@version`, at the top precedence: the post-mirror revocation mechanism. |
| `DenyIfCve params` | Effectful | Opt-in. Denies a version a synced advisory records as affected at or above a CVSS `minSeverity`; an unscored advisory (the npm malware feed carries no score) counts as above every threshold, so it blocks malware too. Ranked below `AllowByIdentity`, so an identity pin overrides it; its `onUnavailable` alignment fails closed by default. See the [deny direction](#denyifcve-the-deny-direction). |

The remaining planned additions are effectful per-version checks like RubyGems native `extensions`.
Which rules ship enabled by default
is documented with the [default policy](configuration.md#the-default-policy): the pure
`AllowIfOlderThan` quarantine (`min-age`) and the `AllowIfRemediatesCve` fast lane, which abstains
when no advisory database is configured so only the quarantine governs. Every other rule above is off
by default and opts in by name.

## CVE subsystem

The advisory subsystem queries a synced local copy rather than calling an advisory API per
evaluation: the `CveLookup` handle (`Ecluse.Core.Cve`) reads the synced `osv.db` SQLite artifact on
local disk, never the network, on the hot path. `AllowIfRemediatesCve` reads it in the allow
direction, fast-tracking a version a synced advisory names as its fix; `DenyIfCve` reads the same
data in the deny direction, blocking a version an advisory affects at or above a severity threshold.
The artifact models an advisory's affected set faithfully: range bounds (an inclusive `introduced`,
and an exclusive `fixed` or an inclusive `last_affected` upper bound), exactly-enumerated versions as
points, and a numeric CVSS base score per advisory (computed from its vector at ingest, or its
qualitative label mapped to a band ceiling). `CveLookup`
reaches the rules through the engine's boot-bound capability record (`RuleDeps`, closed into the
prepared rules by `prepare`), and access is acquisition-bracketed per evaluation, so the
[shadow-swap](#local-polling-decoupled-ingestion) can retire a superseded artifact the moment no
evaluation still reads it.

### `AllowIfRemediatesCve`, remediation fast-track

A publish-age quarantine (`AllowIfOlderThan`) would otherwise also hold back the security patch that
fixes an in-the-wild vulnerability, delaying remediation by exactly the window meant to catch
typosquats. `AllowIfRemediatesCve` removes that tension. For version *V* of package *P* it consults
`CveLookup`:

- **`Allow`** when an advisory for *P* names *V* as its exact fixed version and no advisory's
  affected range still contains *V*, i.e. *V* is a fix and is not itself vulnerable. The reason
  names the remediated advisory IDs.
- **`NoDecision`** otherwise -- including when no advisory database is loaded yet, where an
  allow-direction rule simply abstains -- and a fail-open (`FailNoDecision`) `Unavailable` when a
  lookup against a loaded database faults. An allow that cannot confirm a remediation fails open
  (unlike a deny that cannot confirm safety, which fails closed), so the version falls back to the
  normal quarantine rather than being admitted on an unverified claim. A CVE-source outage thus costs
  patches their fast lane but never admits anything it could not vouch for.

It is ranked above the quarantine allow (so the fix is admitted immediately) and below the scope
allow-list (so a trusted scope never pays the probe). The fix test is a deliberate exact string
match on the advisory's canonical `fixed` version, one traversal of the artifact's
`(package_name, fixed_version)` index: a fix published under any other version string misses the
lane and waits out the quarantine, with `AllowByIdentity` as the operator's explicit workaround. The
not-itself-vulnerable guard, and the deny direction's "is *V* inside the affected range?", are
decided in Haskell against the fetched ranges using the same per-ecosystem ordering as
[`compareVersions`](domain-model.md) (SQLite's text collation cannot order versions), with every
unprovable comparison counting as affected, so the lane only opens on evidence.

### `DenyIfCve`, the deny direction

`DenyIfCve` reads the same `CveLookup` in the opposite direction: it blocks version *V* of package
*P* when an advisory affects *V* at or above a configured CVSS `minSeverity`. It is **opt-in** and,
against the npm feed, does two jobs at once. Roughly 2% of that feed is CVSS-scored CVEs, which the
threshold filters; the other ~96% is the malware feed (`MAL-*` advisories that carry no score and
name the bad version exactly). An unscored advisory is treated as **above every threshold**, so
malware is always denied while `minSeverity` governs the scored CVEs, i.e. severity that cannot be
shown to be low does not slip the gate.

- **`Deny`** when some advisory affects *V* (by the same range/point membership the allow direction
  uses) and clears the severity threshold. The reason names the deciding advisories.
- **`NoDecision`** when no affecting advisory clears the threshold.
- **`CannotVet`** when no advisory database is loaded (deterministic and in-process), and the
  harness's **`Unavailable`** when a lookup against a loaded database faults; both are aligned by the
  rule's `onUnavailable`: **`FailDeny`** (the default) is decisive, so a version that cannot be vetted
  is refused (`Undecidable`, a retryable 503); **`FailNoDecision`** skips the rule, logging loudly,
  for an operator who puts availability above the gate. The deterministic no-database case is taken at
  face value -- never retried, never tripping the breaker. This is the deliberate inverse of
  `AllowIfRemediatesCve`, which fails open: an allow that cannot confirm safety must not admit, a
  deny that cannot confirm safety must not admit either.

It is ranked just **below** `AllowByIdentity` (precedence 225 against 250): an operator's explicit
identity pin overrides an advisory deny, a graceful escape hatch for a false positive or an accepted
risk, while an unpinned affected version is still denied ahead of the passive quarantine and scope
allow-lists. Because enabling it on a cold mirror can deny historical versions an existing build
depends on, it ships off; operators warm the mirror first (see USAGE → *Onboarding DenyIfCve*).

### Local polling, decoupled ingestion

Rather than fetch and parse raw JSON advisory dumps on the proxy (heavy GC pressure and memory
spikes), Écluse uses a decoupled ingestion pipeline, **Écluse Pilot**, a standalone background service
that pulls OSV's per-ecosystem advisory exports, compiles them into a read-only SQLite database
(`osv.db`), and pushes it to a private S3/GCS bucket.

The proxy runs one supervised sync task per configured mount ecosystem (`Ecluse.Core.Cve.Sync`), each
polling the bucket's stable per-ecosystem key for ETag changes. The tasks are independent, so one
ecosystem's missing artifact never holds back another's. At boot each attempts an eager first fetch,
retried with incremental backoff and eventually allowed to fail, so a healthy deployment is
rules-complete within seconds while a broken bucket never wedges startup; the steady poll takes over
from there. Its interval (`cveDbPollInterval`) is deliberately more frequent than Pilot's compile
interval, since matching the two rates would nearly double the worst-case advisory age.

A newly detected `osv.db` is downloaded to a temp file, byte-bounded by `maxOsvDbBytes`, and verified
by the same acceptance that guards every open. Because the file is treated as untrusted even behind the
bucket's access controls, the connection is hardened before its first query (`trusted_schema` off,
`query_only` on, `cell_size_check` on, memory-mapped I/O disabled) and acceptance walks it cheapest-first:
the epoch stamp, a `quick_check` integrity scan (which also verifies stored values against each `STRICT`
table's declared column types), the required tables' strict-schema conformance, and the ecosystem. A
tampered or truncated artifact that parses as SQLite but is structurally unsound, and equally one whose
declarations or stored values would defeat the reader's row decoding, is refused here, before any lookup
reads it. The accepted file is renamed atomically onto the canonical per-ecosystem path and shadow-swapped into the
read path (`Ecluse.Core.Cve.Slot`): rule evaluations borrow the current generation through a bracketed
read, the swap waits for the displaced generation's readers to drain, and the drained close releases
the old artifact's last inode reference. The connection that verified is the connection that serves,
and pruning is the kernel's reclamation, never a mistimed delete. A refused artifact is discarded and
its ETag remembered; the last-good generation keeps serving.
[Readiness](web-layer.md#meta-routes-ping-health-and-search) waits for each configured ecosystem's
first sync (a one-way flip) while the listener serves throughout: an absent database only abstains
into deny-by-default.

#### The artifact contract

The object key is stable per ecosystem and embeds the table-schema epoch:
`<ecosystem>-osv-schema<N>.db` (e.g. `npm-osv-schema3.db`). Per-ecosystem artifacts keep uploads
independent: one ecosystem's failed compilation never holds back another's, and a Pilot restart
loses at most one ecosystem's work. Pilot also filters flattened advisory rows to the target
ecosystem before writing the artifact, so an OSV advisory that spans npm and another ecosystem does
not leak the foreign package rows into the npm database. The stable key is what makes ETag polling
work. The epoch is a hand-bumped constant shared by the Pilot writer and the proxy reader
(`Ecluse.Core.Osv.Schema`); the artifact is immutable and rebuilt from scratch on every compilation,
so there are no migrations, only a read-compatibility contract, and the epoch is that contract's
version.

Pilot stamps the same epoch inside the artifact as SQLite's `user_version`; the proxy verifies it
after download and before the shadow-swap, and a mismatch keeps the last-good database and alarms.
The epoch moves only for a breaking change to the shape. Additive changes (a new column or table) do
not bump it: readers select explicit columns, so additions are invisible to old readers, and the
catch-up window while a new rule waits for newly-populated data degrades per rule (as effectful-rule
unavailability under that rule's failure alignment) rather than per database. A column exists exactly
when the build populates it, so a NULL always means "no data known for this row", never "not
populated yet". The tables are declared `STRICT`, and the reader accepts an artifact only after
confirming that declaration and the required columns' types, so every value its queries decode is
type-sound by construction; a lax or forged schema is refused as a rejection value, the ETag
remembered like any other refusal. The artifact also carries a small `meta` key/value table of
provenance (Pilot version, ecosystem, build timestamp, source URL, row count). Each denial's audit log line records
the advisory database ETag active when the request was served (`active_advisory_db_etag`), resolved
once per request onto the evaluation context. It names the database live at emit, deliberately not
the one the verdict was evaluated against, since a shadow-swap can land mid-request; the log line's
own timestamp completes the true statement.

Polling rather than looking up on demand removes the one external dependency that would otherwise sit
under the fail-closed gate: an advisory-source outage becomes sync lag, the last-good `osv.db` keeps
serving with an alarm, instead of per-package blocking. Lookups also leave the hot path, and the cold
path is rare, since a version is checked only before it is mirrored and served rule-free after, so
lookup volume tracks first-time-seen versions, not requests.

### Point-in-time gating, a known limitation

CVE gating happens at ingestion: a version is checked once, before it enters the mirror, and served
rule-free thereafter, so a CVE disclosed after a version is mirrored is not caught by the gate. The
post-ingestion disposition (operator scanning, a hard deny-by-identity revocation, and operator
purge, *deny-then-purge*) is catalogued as
[threat #13](https://ecluse-proxy.com/threat-model.html#threat-13); holding the dataset locally
keeps a periodic mirror re-scan straightforward to add later.

### Testing

Tests assert a bounded, self-cleaning footprint: memory stays bounded across repeated syncs (the old
index is released), the swap is atomic (no torn reads mid-refresh), a failed sync retains the
last-good index (and alarms), readiness gates on first sync, and transient download scratch is
cleaned up.

## Denial responses

When a request is denied (no allow rule matched, or a deny rule fired):

- HTTP status is decided by the agnostic serve layer (403 for policy denials; see
  [Web Layer → Error model](web-layer.md#error-model)).
- The response body shape is the mount's: its error renderer shapes the bytes in the ecosystem's
  surface (see [Multi-ecosystem mounts](web-layer.md#multi-ecosystem-mounts)), so the agnostic layer
  holds no body shape of its own. For npm the renderer
  (`Ecluse.Core.Registry.Npm.Serve`) emits the npm error object:
  ```json
  {
    "error": "Package @evil/pkg@1.0.0 was denied: AllowIfOlderThan, published 3 hours ago, minimum age is 7 days. Contact #platform-eng on Slack for assistance."
  }
  ```
- The denial reason (which rule decided, and why) is always included.
- `ECLUSE_HELP_MESSAGE`, if configured, is appended to every denial (the ecosystem-neutral
  `appendHelp`, before the renderer wraps it).
