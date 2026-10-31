# Rules engine and responses

> Part of the [Écluse architecture overview](../architecture.md).

How Écluse decides whether to serve a package version, and what a denied request looks like on
the wire.

## Rules engine

**Deny by default. The boot order decides.** Each rule carries a configurable integer
precedence. At boot the rule set becomes one total order: highest precedence first, then rule
name ascending as the deterministic tiebreak. Evaluation walks that order and takes the first
decisive result. If nothing is decisive, the proxy denies the package. A precedence tie breaks
by name, not by a deny-over-allow priority, so shuffling the configured set yields the same
decision. Each built-in rule type carries a fixed default precedence, set out in the
[default precedence ladder](#initial-rule-set). An operator can still rank a specific allow above a
specific deny, say to let a trusted internal scope through an install-script deny.

A rule evaluates one `RuleEvidence` value: the package identity, plus one entry per further fact
the vocabulary reads, each of which is either a reading or nothing at all. The serve, admission,
and mirror paths build it from a `PackageDetails` snapshot, the ecosystem-agnostic per-version
view an adapter produces (see
[The internal domain model](registry-model.md#the-internal-domain-model)), so every fact is a
reading. The Dredger can build it from a store listing alone, which establishes identity with no
metadata read. A rule never sees a registry wire format. Rule names track the agnostic concept,
not one ecosystem's mechanism: the install-time code-execution signal, not npm's
`hasInstallScript`. A determined absence can yield no decision under deny-by-default.
Where nothing read that signal at all, the rule yields `CannotVet`, so evaluation stops rather than
letting a lower-precedence rule decide past an unresolved one.
An adapter declares a typed block when a rule's meaning does not fit its ecosystem or the ecosystem
can never determine the required fact. Boot refuses every blocked entry in an active mount's
resolved policy, with the adapter's reason and the configuration remedy.
This configuration check does not change the engine's treatment of evidence.

Dredger keeps the store listing's exact package and version identity when it adds manifest facts.
PyPI metadata association also accepts the canonical PEP 440 key.
The native deletion identifier and exact identity-rule matching stay unchanged.

A `Rule` is closed `Eq`/`Show` data with no evaluation. `verdictSource` is the single dispatch
over it ([`Ecluse.Core.Rules`](../../core/src/Ecluse/Core/Rules.hs)). Keeping `Rule` closed is a
security boundary: untrusted config can only name a built-in constructor, never supply an
evaluator. Most rules decide a version from its evidence alone. The four advisory rules decide it
from their package's advisory rows. One evaluator decides a request's versions. The first advisory
rule a request reaches reads the package's rows once, and every advisory rule then decides every
version from that one read with no further IO. A request parses each row's bounds at most once, and
the rules' working memory grows with the package's advisory rows, never with its version count. A
rule is **effectful** when that read runs under a resilience policy, which is when an advisory
database is configured. Without one, an advisory rule's verdict is fixed at boot, and the read does
no IO.

The advisory read pins the database generation before assessing its publication age.
The age gate and the query use that generation's store timestamp and rows, even if a sync swaps the slot between them.
An unchanged ETag can advance publication time for later reads without changing an already borrowed snapshot.

### Evaluation model

Each rule applied to a `RuleEvidence` value yields a `RuleVerdict`, a deterministic answer, never
a fault:

- **`Allow`** and **`Deny`**: admit the version, or block it. Decisive.
- **`NoDecision`**: no opinion. A no-op, but the engine keeps the reason for the audit trail.
- **`CannotVet alignment`**: the rule reached the version but cannot vet it deterministically
  and in-process. That means no advisory database is loaded, or the evidence carries no reading
  of a fact the rule needs. It carries its own failure alignment (below). There is deliberately
  no fail-allow: a check that cannot vet must never admit unvetted bytes.

Under its resilience harness the advisory read either returns rows, whose verdicts stand at face
value, or the harness gives it up, and every advisory rule resolves every version to `Unavailable`
under its own alignment. That means no verdict at all: the read faulted, it timed out, or the
breaker is open. The engine walks the boot order and credits the winning rule by name. With nothing
decisive it collects each non-decisive reason, in boot order, so the denial can explain what the
engine considered. An admission also carries the configured checks it did not benefit from, as
`SkippedCheck` evidence: a check that ran and could not vet under a fail-open alignment, kept apart
from a check an earlier allow pre-empted, so nothing downstream reads an unreached check as passed.
The public artifact gate logs the skipped ones once, at the admission, and a trusted read of a
mirrored copy runs no rules, so it never repeats them. The evidence lives in the decision and the
log only. The
[`Ecluse.Core.Rules`](../../core/src/Ecluse/Core/Rules.hs) Haddock holds the full verdict and
harness vocabulary. Every role logs the boot order at start-up, with the phases each rule applies
at (see [Configuration → rule policy](configuration.md#rule-policy)).

### Effectful-rule failure

The advisory read does IO that can fail or hang. It runs once per request under the resilience of
the first advisory rule reached: a short per-attempt timeout with bounded retry and backoff, and
that rule's circuit breaker. A rule that reuses the read sends its own breaker no traffic for that
request. After repeated failed requests the breaker trips and the rule fast-fails for a cooldown. A
sustained outage then neither adds latency to every request nor hammers a down service. The shipped
defaults are a 2-second per-attempt timeout, two retries at 100ms then 250ms. The breaker trips
after 5 consecutive failed requests and cools for 30 seconds. The timeout cannot interrupt SQLite
during one step of the query, so it takes effect when that step returns.

After cooldown, one caller owns recovery for that breaker's complete retry run. Other callers
receive the existing breaker-open outcome under each rule's own failure alignment. Success closes
the breaker. An exhausted source failure starts a fresh cooldown at failure commit. Cancellation
propagates and releases recovery without counting a source failure or adding a cooldown. A later
caller can probe immediately. Generation tickets prevent older outcomes or cleanup from changing a
newer probe. Healthy reads remain concurrent. Each prepared rule keeps its own breaker per mount,
separate from credential providers.

A fault the harness observes becomes `Unavailable` for every version of the request, under each
advisory rule's own alignment. A rule reports a deterministic in-process absence as `CannotVet`, for
example no advisory database loaded. The harness takes that at face value: no retry, no breaker
count, since no retry could change it. The rule's failure alignment governs either one:

- **`FailDeny` (fail-closed, the default)**: decisive. The gate refuses a version a needed rule
  could not vet, whether the scanner is down or the advisory database is not yet loaded.
- **`FailNoDecision` (fail-open)**: a no-op, where a missing signal should not block availability.
  There is deliberately no fail-allow.

The blast radius is small. Only packages not yet in the private mirror reach this path.
Already-approved versions serve from the private upstream with no rules. How a fail-closed
failure surfaces depends on the request shape. On a packument the pipeline filters the version
out like a denied one, with no error unless nothing survives. On a concrete artifact it
surfaces through the [error model](web-layer.md#error-model) as `503` with `Retry-After` when
transient, and `500` when not. A fail-closed undecidable result logs at WARNING with the denial
audit fields. A breaker trip logs nothing and moves the `ecluse.rule.breaker.state` gauge instead.

The advisory source's availability is reported apart from any request. Each advisory rule a request
reaches reports once to a per-ecosystem outage monitor
([`Ecluse.Core.Rules.Outage`](../../core/src/Ecluse/Core/Rules/Outage.hs)): a fault or an open
breaker with its detail, or whether the rows it read let it vet, so an absent database and an
expired push count too. A rule that no request reaches reports nothing. The monitor logs `error`
when an outage begins, `error` again at most every 15 minutes while it continues, and `info` on
recovery. A sustained outage therefore costs the log one line per period whatever the traffic, and
the reminder keeps a deduplicated outage from going quiet.

### Applying verdicts to a packument

Evaluation decides one version, but a metadata request returns a whole packument, so the
pipeline applies verdicts across it. For the cross-upstream merge, see
[Registry model → Packument merge](registry-model.md#packument-merge-across-upstreams).

- **Resolve `dist-tags.latest`: keep unless denied, prefer stable.** `latest` stays as published
  while that version survives, so `npm install <pkg>` resolves to the maintainer's chosen
  release. The pipeline repoints `latest` only when the chosen version is itself denied or
  removed, and then to the highest stable surviving version. It falls back to the highest
  prerelease survivor only if no stable version survives. "Stable vs prerelease" is
  ecosystem-specific (`Ecluse.Core.Version.isStable`), so the core stays agnostic by calling the
  predicate. The pipeline drops other tags (`next`, `beta`) at a removed version rather than
  repointing them. The rule never promotes a higher prerelease over a chosen stable `latest`.
  Repointing downward is a deliberate downgrade, so a withheld release does not silently remain
  the default install.
- **No survivors → 503, 502, 500, 404, or 403.** If nothing survives, the status follows the most
  recoverable cause. `503` (with `Retry-After`) when any rejection was transient or a needed
  upstream was unavailable. `502` when an upstream returned a document Écluse could not use.
  `500` when an exclusion is a permanent inability and none is retryable. `404` on a listing when
  all exclusions are policy or integrity refusals, with the reasons in the body and the logs. A
  PyPI body carries only the help message, so PyPI keeps the reasons in the logs only. A native
  coordinate keeps `403` there. `403` when nothing was excluded. The HTTP status mapping
  belongs to the [error model](web-layer.md#error-model).

Because the filtered body differs from upstream's, the proxy computes its own `ETag` over it
(see [Web layer](web-layer.md#web-layer)).

### Initial rule set

The rule catalogue, each rule's type and parameters, lives in
[`config/default.yaml`](../../config/default.yaml) and the operator manual's
[rule policy](https://ecluse-proxy.com/docs/configuration/#rule-policy).

The **default precedence ladder** climbs from most-passive to most-decisive:

```text
AllowIfOlderThan (100) < AllowIfRemediatesCve (150) < AllowScope (200) <
DenyIfCve = DenyIfEpss (225) < AllowByIdentity (250) <
DenyIfDeprecated = DenyIfYanked = DenyIfKnownMalicious = DenyInstallTimeExecution =
DenyExternalDependencies (300) < DenyByIdentity (400)
```

`DenyIfKnownMalicious`, availability denies, `DenyInstallTimeExecution`, `DenyExternalDependencies`, and `DenyByIdentity` default above every allow. The two
scored advisory denies share a rung **below** `AllowByIdentity` (225 against 250), so an operator's
exact-identity allow overrides either. That is the explicit "I have decided this version must ship"
escape hatch. Both still sit above the quarantine, the remediation lane, and a scope
allow-list, and configured together they tie, which the boot order resolves by name. An operator may
raise a specific allow above a specific deny, or the reverse, with an explicit precedence.

The source-free [default policy](configuration.md#the-default-policy) has one shared rule: the pure
`AllowIfOlderThan` quarantine (`min-age`).
The npm mount also ships `deny-scripts` (`DenyInstallTimeExecution`) and
`deny-external-dependencies` (`DenyExternalDependencies`) at admission only, so they refuse new
public admission without deleting stored copies. The Cargo mount ships `deny-external-dependencies`
the same way. Disable one with `enabled: false` under its mount's rules, or admit a named package
with an `AllowByIdentity` pin whose explicit precedence is above 300. `DenyExternalDependencies`
exists because no consumer fetch may leave Écluse: an npm git or URL dependency installs straight
from its source, past the quarantine, the advisories and the mirror. Any scheme Écluse does not know
to resolve through the registry or locally counts too, so a scheme a client adds later fails closed.
It reads specs the packument already carries, so it adds no fetch. The PyPI and RubyGems adapters
project no dependency source into rule evidence for admission, so they block the type at boot. The
Cargo adapter projects one from each public index row: a normal or build dependency from a registry
other than crates.io makes the version fetch from outside Écluse. crates.io refuses such crates at
upload, so on Cargo the rule guards a `publicUpstream` other than crates.io. Cargo first-party
publication refuses the same dependencies, with Écluse's own index also accepted.
When `advisories.url` is configured, two more shared defaults activate: `DenyIfKnownMalicious` and
the `AllowIfRemediatesCve` fast lane. The remaining rules opt in by name.

## CVE subsystem

The advisory subsystem reads a synced local copy rather than calling an advisory API per
evaluation. The `CveLookup` handle ([`Ecluse.Core.Cve`](../../core/src/Ecluse/Core/Cve.hs))
reads the synced `osv.db` SQLite artifact on local disk, never the network, on the hot path. It
models an advisory's affected set faithfully: range bounds (inclusive `introduced`, exclusive
`fixed` or inclusive `last_affected`) and exactly-enumerated versions as points. Each advisory also
carries a numeric CVSS base score and, where the EPSS feed scores one of its identifiers, an EPSS
probability. Each read brackets its own acquisition, so the
[shadow-swap](#local-polling-decoupled-ingestion) can retire a superseded artifact the moment no
read still holds it. One read per request serves every advisory rule, so every decision in one
response reads one generation. Four rule types read it.

### `AllowIfRemediatesCve`, remediation fast-track

The quarantine would also hold back the security patch that fixes an in-the-wild
vulnerability, delaying remediation by exactly the window that lets registries find and yank a
malicious publish.
`AllowIfRemediatesCve` removes that tension for an exact fix. For version *V* of *P*:

1. When no advisory names *V* as its fixed version, the rule abstains. Being merely unaffected
   earns no exception.
2. When advisories name *V* as their fix and no advisory still affects *V*, the rule admits *V*.
   It reads no score, so a missing score cannot block a full fix.
3. Otherwise *V* is a partial fix. An advisory whose fix is *V* but whose other range still holds
   *V* is no genuine fix. When no genuine fix remains, the rule abstains.
4. `remainingAdvisories` decides a partial fix. `abstain` returns no decision, so the fix waits out
   the quarantine. `differential` admits it only when every enabled condition holds.

The differential's conditions read the scores the artifact stores:

- Every remaining advisory stores a CVSS at or below `maxRemainingCvss`.
- Every remaining advisory stores an EPSS strictly below `maxRemainingEpss`.
- With `minCvssGap` above zero, the highest genuinely fixed CVSS exceeds the highest remaining CVSS
  by at least the gap.
- With `minEpssGap` above zero, the same holds for EPSS, in probability points.

The conditions compare maxima, so repeated rows cannot inflate a score. A missing score that a
condition needs makes the rule abstain, because unknown is not zero. A remaining advisory with any
unscored row counts as unscored. Scores compare as the shortest decimals that read back as the
stored values, so a gap of 1.0 holds between 8.1 and 7.1. The decision evidence names the genuinely
fixed and remaining advisories, the compared maxima, the limits, and each failed condition. Metric
labels carry no advisory ID.

The comparison reads stored scores. Pilot stores the highest valid CVSS vector score, or a
qualitative label's upper limit when no vector exists, and the artifact does not record which. So
the differential proves no exact severity reduction, and no reduced exposure against the version a
client already has. The shipped limits are chosen product defaults, not validated safety boundaries.
Many PyPI records carry no CVSS score, so the differential rarely admits a PyPI partial fix.

`onUnavailable` decides what the rule does when advisory evidence is unavailable: no database
configured or loaded, a failed or timed-out read, an open breaker, or an expired or undated push.
`deny` refuses a version the rule reaches, as a fail-closed deny does. `skip` abstains, and an
admission that follows records the skipped check. Unavailable evidence never supplies an exception.
Under `deny`, the rule makes an advisory store mandatory and makes readiness wait for the first
artifact, on the terms of the advisory denies. In differential mode the rule reads EPSS scores, so
it makes EPSS enrichment required.

The shipped `remediation-fast-track` declares `deny` and `differential`, and an advisory source
activates it as it activates `DenyIfKnownMalicious`. During an advisory outage it refuses a version
the quarantine alone would admit. Higher-ranked allows still decide first. The rule is an allow, so
it authorises no Dredger deletion, and an unavailable generation justifies none.

It ranks above the quarantine allow, so the rule admits a fix immediately. It ranks below the scope
allow-list, so a trusted scope avoids its read only when no higher advisory rule runs. Pilot stores version bounds and exact
points under one ecosystem spelling. The remediation lookup renders served versions and its parsed
fixed bound through the same renderer. PyPI `1.19.0` and `1.19` identify the same fix. npm advisory
`v1.2.0` identifies served `1.2.0`, while public npm parsing remains strict. Semver build metadata
remains part of exact fix identity, even though precedence ignores it. RubyGems uses its canonical
token spelling. Unparseable fixes retain literal matching and conservative affected membership.
This replaces the earlier raw-string decision (#1574). Wire versions and artifact URLs retain their
published text. The rule decides range membership
in Haskell with the same per-ecosystem ordering as
[`compareVersions`](registry-model.md#the-internal-domain-model). Every unprovable comparison counts
as affected, so the lane only opens on evidence.

### `DenyIfKnownMalicious`, primary namespace evidence

An exact, case-sensitive `MAL-` prefix with a nonempty suffix identifies the OpenSSF namespace.
The rule filters the existing stored primary `arCveId`, then uses the same affected-version matcher.
Withdrawn records already disappear during compilation. Aliases do not supply classification.
Epoch 5 stores the required identifier and ranges, so this rule needs no column or format change.

The MAL view filters existing parsed segments once per package. All advisory predicates reuse
one canonical lookup and one borrowed generation. Health reporting and version decisions share
the resolved function. Neither version count nor rule count adds SQL reads.

Configuration retains the named descriptor and its activation flag through the existing patch
layers. The source setting seeds that flag once. Runtime receives only the final effective policy.
The rule ranks at 300 because known MAL evidence must precede normal identity pins and scope allows.
Its unavailable default denies. Expired or undated generations always refuse, including under skip.
Only a decisive denial can authorise eligible Dredger deletion.

### `DenyIfCve`, the deny direction

`DenyIfCve` reads the same lookup to block version *V* of *P* when an advisory affects *V* at or
above a configured CVSS `minCvss`. It is **opt-in** and does two jobs against the npm feed.
Most of that feed is the malware feed: `MAL-*` advisories that carry no CVSS score and name the
bad version exactly. A smaller share is CVSS-scored CVEs. An unscored advisory counts as **above
every threshold**, so the rule denies unscored records while `minCvss` governs scored advisories.
The separate MAL rule does not depend on either score state.

- **`Deny`** when some advisory affects *V* and clears the threshold. The reason names the
  deciding advisories.
- **`NoDecision`** when no affecting advisory clears it.
- **`CannotVet`** when no advisory database is loaded, and the harness's **`Unavailable`** when a
  loaded-database lookup faults. Both align by `onUnavailable`. **`FailDeny`** (the default)
  refuses the version with a retryable `503`. **`FailNoDecision`** skips the rule, and the
  admission that follows carries the skipped check as evidence. Neither an allow nor a deny that
  cannot confirm safety may admit.
- **`CannotVet FailDeny`** when the serving artifact's push is older than the mount's maximum,
  whatever `onUnavailable` says. Expiry is unavailability the operator cannot waive, so the
  alignment is fixed here rather than configured. The gate runs before breaker admission, which
  an open breaker would otherwise skip past. `AllowIfRemediatesCve` follows its own
  `onUnavailable` on the same reading. The maximum and its derivation are in
  [Advisory push age](https://ecluse-proxy.com/docs/operations/#advisory-push-age).

Enabling it on a cold mirror can deny historical versions an existing build depends on, so it
ships off. Warm the mirror first, as
[Onboarding the advisory denies](https://ecluse-proxy.com/docs/configuration/#onboarding-the-advisory-denies)
sets out.

### `DenyIfEpss`, the exploitability direction

Severity states how bad a vulnerability would be. It does not state whether anyone is exploiting it.
FIRST.org's Exploit Prediction Scoring System (EPSS) estimates the probability that a vulnerability
is exploited in the wild within the next 30 days, and a moderate CVE under active exploitation is a
worse risk to a build than a critical one nobody has weaponised. `DenyIfEpss` denies version
*V* of *P* when an advisory affects *V* and its EPSS score is at or above the configured `minEpss`.

It shares `DenyIfCve`'s lookup, failure alignment, and precedence. An individual missing
EPSS score makes the rule abstain for that advisory. EPSS joins through CVE aliases, so malware
without a CVE alias receives no EPSS denial. `DenyIfCve` retains its independent malware and
severity behaviour, including denial on missing CVSS scores.

Other affecting advisories with known scores and other rules still apply. If all rules abstain,
deny-by-default refuses admission but does not authorise Dredger deletion. A different decisive
deny can authorise deletion under the existing guards. A valid feed that loses a score, including
a malformed row the parser skips, can remove an EPSS denial and let another allow admit the version.
That consequence does not establish that the version is harmless.

Pilot supplies the score. It fetches the EPSS daily feed each compile pass, joins it onto the
advisories through their aliases, and writes the result to the artifact's `epss_score` column
([`Ecluse.Core.Osv.Epss`](../../core/src/Ecluse/Core/Osv/Epss.hs)). The download is bounded twice, on
the served stream and again on its gzip expansion, so neither an endless stream nor a compression
bomb can hang or exhaust the pass. A feed past either bound is refused whole rather than truncated,
because a short table is indistinguishable from a complete one downstream. For the same reason a
gzip stream must reach its end-of-stream marker, so a feed cut short is refused rather than read.

Every pass attempts the feed, whatever the rules. What a failed feed means depends on the
ecosystem's resolved policy, the same requirement its consumers enforce. An ecosystem with an
active rule that reads EPSS scores requires enrichment: a `DenyIfEpss`, or an
`AllowIfRemediatesCve` in differential mode such as the shipped one. Its pass then fails and
publishes nothing, and consumers keep their last accepted artifact. Any other ecosystem publishes its OSV data with
`epss_status=unavailable`, so a new advisory and its fix still reach consumers during an EPSS
outage. The scheduled loop runs each ecosystem on its own, so one ecosystem's required failure
holds back no other. A one-shot compile of an ecosystem the configuration does not mount requires
the feed, because it prepares a dataset ahead of that mount. A one-shot compile with no
`advisories.url` requires it too, because no source activates the shipped remediation rule there,
so the compile cannot know whether the artifact's consumer reads EPSS. A failed fetch, a byte ceiling, an
empty feed, and a gzip error count as a failed feed. An invalid feed URL and any other fault still
fail the pass, whatever the requirement.

Pilot cannot see a consumer's rules, only its own configuration. The shared configuration is the
contract: when Pilot's copy omits a consumer's EPSS-reading rule, the consumer refuses the
unavailable artifact that results and keeps its last accepted one.

Each ecosystem's resolved policy decides whether its consumers require EPSS enrichment.
An active `DenyIfEpss`, or an active `AllowIfRemediatesCve` in differential mode, requires the exact
`epss_status=available` metadata marker, including under `onUnavailable: skip` or a threshold of 1.
A removed rule creates no requirement.
Missing, unavailable, or unrecognised markers do not establish enrichment, even when score rows exist.
A valid success marker needs neither optional feed dates nor a score for every advisory.
Consumers without EPSS-dependent rules accept otherwise valid artifacts regardless of this marker.
This qualification preserves individual-score abstention and each rule's unavailable behaviour.

### Local polling, decoupled ingestion

[OSV and EPSS ingestion with guarded artifact publication](https://ecluse-proxy.com/docs/threat-model/#advisory-ingestion-and-publication)

[Independent advisory polling by proxy, worker and Dredger](https://ecluse-proxy.com/docs/threat-model/#advisory-distribution)

Parsing raw JSON advisory dumps on the proxy costs heavy GC pressure and memory spikes. So
Écluse decouples that work into **Écluse Pilot**, a standalone service. Pilot pulls OSV's
per-ecosystem exports, compiles them into a read-only SQLite database (`osv.db`), and pushes it
to a private object store. `advisories.url` names that store as `s3://bucket[/prefix]`, its
scheme picking the provider the way `queue.url` does. Unset, the advisory stack is off: the artifact
carries no signature and a bucket name is global, so a shipped default would name a bucket the
project does not own. With the stack off, a mount whose rules refuse without advisory evidence
refuses the boot. Those rules are `DenyIfKnownMalicious`, `DenyIfCve`, `DenyIfEpss`, and an
`AllowIfRemediatesCve` under `onUnavailable: deny`. They cannot decide without a database, so the
pairing is a configuration error rather than a mount that refuses every version it evaluates. An
`AllowIfRemediatesCve` under `skip` boots with a warning, because it admits no fix without a source.

OSV retains withdrawn records in its [GCS exports](https://google.github.io/osv.dev/faq/#how-does-osvdev-handle-withdrawn-records).
Pilot excludes records carrying a withdrawal timestamp before it emits affected ranges or exact versions.
Their retained ranges cannot justify an advisory deny, a remediation exception, or Dredger deletion after a replacement artifact syncs.
Independent active advisories still supply their own evidence.

The proxy runs one supervised sync task per configured mount ecosystem
([`Ecluse.Runtime.Cve.Sync`](../../runtime/src/Ecluse/Runtime/Cve/Sync.hs)). Each task polls the
store's stable per-ecosystem key for ETag and publication-time changes at `advisories.pollInterval`. That interval
is deliberately shorter than Pilot's compile interval, since matching them would nearly double
the worst-case advisory age. The tasks are independent, so one ecosystem's missing artifact
never holds back another's.

The proxy downloads a newly detected `osv.db` to a temp file, byte-bounded by
`limits.maxAdvisoryDatabaseBytes`. It treats the file as untrusted even behind the store's access
controls. It accepts the file only after a cheapest-first verification: epoch stamp, integrity
scan, the required tables' strict-schema conformance, ecosystem, and required EPSS evidence. It then renames the accepted
file atomically and shadow-swaps it into the read path
([`Ecluse.Core.Cve.Slot`](../../core/src/Ecluse/Core/Cve/Slot.hs)). The swap waits for the
displaced generation's readers to drain. The last reader closes a retired generation even
if the sync task is cancelled, so pruning remains the kernel's reclamation. The proxy discards a refused artifact and remembers its ETag. The last-good
generation keeps serving within its existing maximum push age. Rejection and repeated polls for the
rejected ETag cannot refresh that age. A cold process creates empty slots and does not recover the
canonical file from disk. [Readiness](web-layer.md#meta-routes-ping-health-and-search) is decided
per mount from that mount's own rules: one whose rules refuse without advisory evidence waits for its
first sync, and one whose rules do not is ready without a database. The listener serves throughout. A
never-published artifact keeps the role polling and logs an operator-facing `ERROR` naming Pilot
and the store, first when the boot retry budget is spent and then every 15 minutes until one loads.
An absent database makes each active advisory rule follow its `onUnavailable` setting: `deny`
refuses and `skip` lets a lower rule decide.

Polling removes the one external dependency that would otherwise sit under the fail-closed gate:
an advisory-source outage becomes sync lag, not per-package blocking. Lookups also leave the hot
path, since Écluse checks a version only before mirroring it and serves it rule-free after.

#### The artifact contract

The object key is stable per ecosystem and embeds the table-schema epoch,
`<ecosystem>-osv-schema<N>.db` (currently `npm-osv-schema5.db`). That stability is what makes
ETag polling work. The epoch is a hand-bumped constant shared by the Pilot writer and the proxy
reader ([`Ecluse.Core.Osv.Schema`](../../core/src/Ecluse/Core/Osv/Schema.hs)), stamped inside
the artifact as SQLite's `user_version`. A mismatch keeps the last-good database and alarms.

The artifact is **immutable and rebuilt from scratch** on every compilation, so there are no
migrations, only a read-compatibility contract. Advance the epoch for an incompatible table shape
or an incompatible meaning of stored values, even if the columns remain unchanged. A compatible
addition does not bump it, because readers select explicit columns. What the reader requires is a
separate question from what the epoch names: within epoch 5
the reader requires `severity` and `epss_score` on `package_vulnerability_ranges`, so an artifact
missing a required column, or carrying it under a different declared type, fails schema
conformance (`CveDbSchemaNonConformant`) and the last-good database keeps serving.

Successful compilation records `epss_status=available` in the `meta` table, including when the
valid feed omits optional dates or has no scores matching this artifact's advisories. This marker
records whole-feed enrichment. An individual `epss_score` can still be absent. The added key
was compatible within epoch 4. Consumers enforce their resolved policy's requirement before accepting an artifact.
An artifact without the marker remains acceptable only without EPSS-dependent rules.
A pass whose optional feed failed records `epss_status=unavailable`, no scores, and no EPSS source
or dates. That whole-feed state is distinct from an available feed that scores none of the
artifact's advisories, and only an ecosystem without EPSS-dependent rules accepts it.

Epoch 5 retains the canonical package names introduced in epoch 4: PEP 503 for PyPI, verbatim for npm and RubyGems.
Rules query the same canonical key for denial and remediation. Dredger parses the stored keys
and store names into the same package identity before evaluating the shared rules.
Display spelling remains available for decision messages.

The [OSV schema](https://github.com/ossf/osv-schema/blob/main/docs/schema.md#defined-ecosystems)
requires normalised PyPI names. The sampled records
[GHSA-m2qf-hxjv-5gpq](https://api.osv.dev/v1/vulns/GHSA-m2qf-hxjv-5gpq) and
[PYSEC-2021-142](https://api.osv.dev/v1/vulns/PYSEC-2021-142) use `flask` and `pyyaml`.
Those samples do not establish feed-wide conformance. Pilot applies the existing package
canonicalisation itself because configured exports can carry other spellings. Epoch 3 did not
enforce this stored meaning, so epoch 4 consumers reject those artifacts despite their matching
table shape. Unknown ecosystems retain their package spelling.

Epoch 5 changes the meaning of stored version bounds and points to their canonical ecosystem
rendering. It rejects epoch 4 even when the columns conform. Rebuild and publish the new artifact
before rolling out consumers. During a mixed rollout, old consumers select epoch 4 keys and new
consumers select epoch 5 keys. No alias or in-place migration accepts the old format. An absent
new artifact follows the existing first-sync, readiness and alarm contract.

Pilot also decodes a version sentinel: OSV writes "affected from the beginning" as an `introduced`
of `0`, which semver rejects, so the compile records no lower bound and leaves the `fixed` or
`last_affected` bound beside it under its canonical ecosystem spelling. A version at or above the fix then computes as
admitted and one below it as denied.

A bound the ecosystem's version grammar cannot parse is a signal rather than a filter. The
compile counts those rows, names one on a warning line, and carries the count on its summary
line and its span, so an operator sees a feed going wrong without an advisory going missing.

The reader keeps two readings of such a bound apart. A range whose endpoint no grammar can
order stays fail-closed and denies every version of its package, since nothing can place a
version against it. A segment whose lower and upper bound are the same unorderable string
names one exact version, which is how OSV writes an enumerated version, so it matches that
string and nothing else. A bound the grammar does parse keeps the ordered comparison
throughout.

Pilot filters rows to the target ecosystem, so an advisory spanning two ecosystems does
not leak foreign package rows. It refuses zero persisted ranges before writing success metadata
or publishing. An empty or wrong-ecosystem export cannot replace last-good denial evidence or
refresh its timestamps. [Guarded publication](#guarded-publication) bounds a reduced nonempty output.
Each denial's audit log records the advisory database ETag live
at emit (`active_advisory_db_etag`). That is deliberately the ETag live at emit rather than the
one the rule evaluated against, since a shadow-swap can land mid-request.

#### Guarded publication

Pilot replaces the stored artifact only when the new compile is ordered after it and has not
shrunk sharply. Both checks read the object's S3 user metadata with one `HEAD`, so Pilot never
downloads the stored database. The write is one conditional `PUT` over the revision that `HEAD`
returned, so two Pilot runs cannot compare one object and then overwrite another.

Pilot orders the OSV export and the EPSS feed separately, because the two feeds age on their own.
For each source it records the SHA-256 of its credential-free URL, the `Last-Modified` it answered
with, and the SHA-256 of the whole response body. A later `Last-Modified` moves forward, and an
earlier one is refused. Equal timestamps with equal bytes refresh the artifact. Equal timestamps
with other bytes, and changed bytes with no timestamp to compare, are refused rather than guessed.
Compilation time orders nothing, because a slow run over older data finishes last. A changed
source URL starts a fresh order for that source. A `Last-Modified` more than five minutes past
Pilot's own clock counts as missing, so a source cannot claim a future date and outrank every
later snapshot.

The size guard is a damage alarm, not a completeness proof, since withdrawn advisories shrink a
feed legitimately. The metadata carries the largest accepted size as a durable baseline, so small
shrinks cannot add up, and no object version history is needed. An optional EPSS outage earns a
bounded credit once, and the feed's return clears it without lowering the baseline. The credit only
lowers the comparison: the baseline records the larger of itself and the accepted size, so an
estimate that overstates the scores' bytes cannot inflate it. An operator
who expects a smaller artifact rebaselines one upload against the revision they inspected.

Each publication writes a fresh nonce into the database, because an S3 ETag follows content and
two publications of one input would otherwise share it. Consumers therefore download every
accepted publication, a refresh of unchanged input included.

An operator's direct write bypasses all of this, and it is the rollback authority. Consumers
accept a restored older artifact that passes their usual verification and push age. Such a
restore can re-admit a version a later generation denied, and the mirror worker can then refill a
copy the Dredger removed. Pilot refuses to replace an object that carries no publication metadata.

### Re-checking stored versions

The serve path gates a version once, before it enters the mirror, and serves it from the trusted
store without gating it again at request time. A stored version does not stay exempt, because
[Dredger](https://ecluse-proxy.com/docs/dredger/) re-checks each mount's mirror target and the
private read cache paired with it against the current rules that apply at revocation, and prunes
what they deny. A CVE disclosed after mirroring, or a policy change, therefore removes the version
on a later Dredger cycle. It removes an eligible mirror version before its eligible cache copy, and
a restart or a later cycle rediscovers a cache copy left behind. The
[revocation procedure](https://ecluse-proxy.com/docs/operations/#revoking-a-mirrored-version-internal-yank)
is the operator's side of it.

### Admission and revocation

A rule applies at admission and at revocation unless its `appliesTo` limits it to admission. The
phase is data on the rule, and one list per mount still holds every rule, so precedence stays in one
place. The gate and the mirror worker evaluate the whole list. The composition root hands the
Dredger the rules that apply at revocation, and the engine itself takes no phase. That placement is
why a serve path cannot drop a deny by naming the wrong phase.

The design question is whether the Dredger can then delete a version the gate admits. It cannot, on
the same evidence, and two load refusals are what hold that:

| Refused at load | What it would allow |
|---|---|
| An allow limited to admission | The Dredger would not read the allow, so a deny below it could block a version the gate admits |
| A rule that applies at revocation alone | A deny the gate never read would delete versions the gate admits, and the worker would mirror them again |

With both refused, every rule the Dredger does not read is a deny, and a deny can block or abstain
and never admit. The Dredger's list keeps the gate's order, so each rule ahead of the one that
blocks for the Dredger either passed at the gate too or is a deny limited to admission. The gate
therefore refuses the version as well. A property test over generated policies and evidence holds
this ([`Ecluse.Core.Rules.TypesSpec`](../../core/test/unit/Ecluse/Core/Rules/TypesSpec.hs)).

The claim is about one set of evidence. The Dredger reads a store's own metadata and the gate
reads the upstream's, so the two can still differ where those documents differ. A rule limited to
admission also does not stop the Dredger's walk, so a lower deny that applies at revocation
decides a version the limited rule would have left undecided.

## Denial responses

When Écluse denies a request, because no allow rule matched or a deny rule fired:

- The agnostic serve layer decides the HTTP status: 403 for a denied artifact, and 404 for a
  listing that admitted nothing. See [Web layer → Error model](web-layer.md#error-model).
- The response body shape is the ecosystem's. The route contract supplies the typed response
  constructor and codec, so the agnostic pipeline holds no body shape of its own. For npm the
  codec ([`Ecluse.Core.Registry.Npm.Serve`](../../core/src/Ecluse/Core/Registry/Npm/Serve.hs))
  emits the npm error object:
  ```json
  {
    "error": "BlockedByDefault: no allow rule matched (rejection entries: 42) Contact #platform-eng for assistance."
  }
  ```
- npm metadata with no surviving version and a `404` groups typed causes by deciding rule or
  integrity category. Each group gives a short explanation and a count of rejection entries.
  Different source contributions can reject the same version, so counts are not unique versions.
  The summary discards the mapping from each version to its full reason. Existing audit records
  do not preserve every removed detail. The completed text retains no per-version reasons or decisions.
- Partial listings, private-access refusals, concrete-artifact errors, PyPI responses and
  `500`/`502`/`503` responses keep their existing bodies. Status selection, HEAD responses,
  denial audits and metrics do not change.
- The ecosystem-neutral `mkRefusal` retains `ECLUSE_SERVER__HELP_MESSAGE`, if configured,
  alongside the reason. The ecosystem renderer chooses the body and adds the help once.
