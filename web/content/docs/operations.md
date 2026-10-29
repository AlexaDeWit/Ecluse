+++
title = "Operations reference"
description = "What a running instance tells your orchestrator and your log collector, how it drains and exits, and how to size the pod underneath it."
weight = 5
+++

Use this reference when a process is unready, background work stalls, or resource limits need adjustment.
The [operating guide](@/docs/operate.md) provides the routine monitoring and upgrade path.

`ecluse check-config` validates the configuration without starting a role.
For mirror stores with a control plane, it reports that boot builds the maintenance client
against the live environment without attempting that build itself.

Configured `advisories.url` activates `DenyIfKnownMalicious` at precedence 300. Its first-generation
readiness and publication-age requirements apply before any successful sync. A failed poll keeps
the last-good generation until expiry. A stale or undated generation refuses even under `skip`
and supplies no deletion authority. Trusted private copies remain readable until eligible removal.
MAL denial retains consent, first-party protection, deletion caps, and the accepted worker-transfer
and client-cache limits. See [configuration](@/docs/configuration.md#known-malicious-packages-and-upgrades)
for migration, opt-out, precedence, and rollout order.

## Health probes

An orchestrator watches two endpoints on the proxy, and they answer for different things, so wire
both:

| Endpoint | What it reports | When it answers `503` |
|---|---|---|
| `GET /livez` | Process liveness: `200` while the process is healthy. | The process is not healthy, or its mirror worker's consume loop stalled. |
| `GET /readyz` | Whether the role can accept its work. | Startup, drain, a pending first advisory sync, or a latched Dredger cap halt. |

On a process that runs no mirror worker, liveness is the listener alone. The readiness advisory gate
lifts once any mount whose rules deny on the advisory database completes its first sync, and a mount
whose rules never deny on it is ready without one.

The `/livez` body is a JSON object with two keys, in no guaranteed order: `status`, the same
verdict the status code carries, and `lastPoll`, the mirror worker's last successful poll as an
ISO 8601 instant. A process that runs no mirror worker reports `lastPoll` as `null`. Alert on the
`503`, and read `lastPoll` when you want to see a loop slowing before it crosses the threshold.

Embedded and dedicated mirror workers have 660 seconds from heartbeat creation to complete
their first successful poll. After that allowance, `/livez` answers `503` until the worker makes
progress. `lastPoll` stays `null` until a successful poll or completed job. Each success restarts
the same allowance, so later stalls also fail liveness after 660 seconds without progress.

Readiness is deliberately lenient about public-upstream reachability, so a transient blip does not
pull a healthy pod from rotation. The starting-up case is the one to plan for. Readiness follows
each mount's own rules. `DenyIfCve`, `DenyIfEpss`, `DenyIfKnownMalicious`, and
`AllowIfRemediatesCve` with `onUnavailable: deny` require the first accepted advisory generation.
This readiness transition is one-way and never flaps back. A mount with none of these rules is
ready before an artifact exists. Give a cold pod room for
that first database download: a Kubernetes `startupProbe`, or a readiness `failureThreshold` sized
for it. Pilot publishes an artifact for every ecosystem the configuration mounts. If acquisition
fails, the process stays alive and keeps polling. Once the boot retry budget is spent, the sync
logs an `ERROR` with the cause and the store, and repeats it every 15 minutes until an artifact
loads. Missing or rejected artifacts require a Pilot publication. Access failures require a bucket,
object, or IAM correction. The same process recovers when a later poll acquires a valid artifact.
Alarm on these lines to detect a rollout that cannot acquire its advisory database.

A proxy with advisory synchronisation reports per-mount readiness, so one ecosystem's missing
artifact does not remove healthy mounts from routing. Its `/readyz` body carries a `mounts` object
keyed by ecosystem, and each
value is `ready` or `awaiting the advisory database that ecluse pilot publishes`. A missing PyPI
database therefore leaves a healthy npm mount routable, and the body names PyPI as the mount still
waiting. A latched Dredger reports
`halted` instead, and a draining instance reports `draining`. Readiness does not itself block direct
requests: a request that needs advisory data its mount does not have is refused by that mount's own
`onUnavailable` policy. A rule that admits without reading the database admits only when no
advisory rule ranked above it refuses first. A configured source puts the MAL default above every
default allow, and the remediation fast lane above the quarantine. Under their shipped `deny`, that
mount refuses public admission until its first sync, except through an allow ordered before them. A successful first sync does not prove that
data remains fresh. [Advisory push age](#advisory-push-age) governs retained evidence.

An EPSS-dependent ecosystem rejects artifacts without the exact `epss_status=available` marker.
Without an accepted qualified generation, its slot stays empty, and its readiness keeps awaiting the advisory database.
Other ecosystems accept marker-free artifacts, and artifacts Pilot published with
`epss_status=unavailable` during an EPSS outage, and become routable independently.
A running consumer keeps its accepted qualified generation after a rejected replacement.
Restarting creates empty slots, even when canonical files remain on disk.
Before you enable an EPSS-dependent rule, publish an artifact with `epss_status=available`, following
[the onboarding steps](@/docs/configuration.md#when-the-epss-feed-fails).

The npm liveness probe `GET /npm/-/ping` answers locally with `200 {}`. `GET /npm/-/v1/search`
returns `501` by design, because search is a discovery convenience, not an install path.
`GET /npm/-/package/{package}/dist-tags` and the `PUT` and `DELETE` of
`/npm/-/package/{package}/dist-tags/{tag}` also return `501`, because Écluse implements no mutable
named pointer. A package's tags are in the metadata document Écluse serves, and the publication
target owns setting and removing them. With a publication target configured, the npm publish route
`PUT /npm/{package}` also returns `501` unless the document carries exactly one version, a
non-empty `_attachments` object, and only `dist-tags` that name that version. The body
`npm deprecate` or `npm star` sends gets this `501`. `npm deprecate` sends back Écluse's served
document, and a store such as Verdaccio applies it over its own. The route returns `400` for a body
that cannot be read, is not a JSON object, or repeats a key, and for a version filed under a key
that differs from its own `version` field. The PyPI upload route returns `501` for an `:action`
other than `file_upload`. It returns `400` for an upload without exactly one `version` field and
one `content` file part. It also returns `400` when the `content` file name does not name the
declared project and `version` as a build tool spells them. A source distribution must be named
`{normalised_name}-{version}.tar.gz`, as PEP 625 defines. A body that hides a boundary delimiter
inside a part gets `400` too. Mirror, Pilot, and Dredger expose their health probes on
`ECLUSE_SERVER__PORT`. A separate metrics listener appears only when telemetry is on and
`OTEL_METRICS_EXPORTER=prometheus`. Co-located Prometheus listeners need distinct ports
([Telemetry](@/docs/operations.md#telemetry-opt-in)).

## Graceful shutdown and pod drain

On `SIGTERM`/`SIGINT` Écluse drains in-flight work rather than dropping it. `GET /readyz` flips
to `503`, which is the signal a load balancer or mesh watches to stop routing new traffic here,
while `GET /livez` stays `200`, so an orchestrator does not kill a still-draining instance early.
Every response then carries `Connection: close`, and a keep-alive pool reconnects to a ready
instance. In-flight requests and artifact streams can finish within the drain window.
Remaining transfers can stop when the timeout expires.

`ECLUSE_SERVER__SHUTDOWN_DRAIN_TIMEOUT` bounds the drain at 30 seconds by default. **Set the
platform's termination grace period above it**, so the orchestrator does not `SIGKILL` mid-drain.
On Kubernetes that is `terminationGracePeriodSeconds`. A second `SIGINT` or `SIGTERM` hard-stops
the process wherever it runs: the drain handler fires once, and the runtime default takes the next
signal. `Ctrl+D` forces the same immediate halt, and that path is armed only when standard input is
a TTY.

## Exit codes

The exit status states how a run ended, so an orchestrator can branch without parsing logs:

| Code | Meaning |
|---|---|
| `0` | Graceful shutdown: the drain completed and the services returned. |
| `1` | A service exited abnormally. The last `ecluse: service exited:` line on standard error carries the detail. |
| `2` | The boot aborted, and Écluse reported every problem. |
| `3` | Something outside cancelled the run: a kill that bypassed the graceful path. |
| `130` | The local-development halt (Ctrl-D on an interactive terminal). |

A boot aborts with `2` when Écluse refuses the configuration, refuses a role this build cannot run,
or cannot build what the configuration names in the live environment. A configuration refusal fails
identically on a restart without changes. A report that names a transient AWS or network fault may
clear on retry.

## Logs

Écluse writes one JSON object per line by default (`ECLUSE_OBSERVABILITY__LOG_FORMAT=json`). Set
the format to `console` for local development instead. Each JSON line carries these fields:

| Field | Content | Note |
|---|---|---|
| `timestamp` | When the line was emitted. | RFC 3339 UTC. |
| `status` | `debug`, `info`, `warn`, or `error`. | `ECLUSE_OBSERVABILITY__LOG_LEVEL` sets the floor, `info` by default. |
| `message` | The message text. | |
| `service`, `env`, `version` | The resolved identity. | |
| `dd` | `trace_id` and `span_id`. | Present only while a span is in scope. |
| `data` | The emitting call's own fields. | |
| `katip` | The `katip` emitter fields. | These include the emitting process's hostname (`katip.host`), so a collector's own host attribution governs the line's `host`. |

Four of those names matter to Datadog specifically: `timestamp`, `status`, `message`, and
`service` are its reserved log attributes, and its JSON preprocessing reads them unmodified.
`env` and `version` are ordinary attributes any backend indexes.

Typed bearer-token fields render as redacted placeholders. Pilot records provenance in the
artifact's metadata:

| Field | Holds |
|---|---|
| `source_url` | The OSV source's `host:port` identity. |
| `epss_source_url` | The EPSS source's `host:port` identity. |
| `osv_source` | The OSV source's scheme, host and port, then the SHA-256 of its path as `/<sha256:hex>`. With `advisories.recordSourcePath: true`, the URL as written, without userinfo, query, or fragment. |
| `epss_source` | The EPSS source, in the same form as `osv_source`. |
| `osv_digest` | The SHA-256 of the OSV export's response body. |
| `epss_digest` | The SHA-256 of the EPSS feed's response body, when the feed arrived. |
| `publication_nonce` | A random value that gives each publication its own bytes. |
| `built_at` | When compilation finished, not how old the source snapshot is. |

A credential in userinfo, the query, or the fragment never reaches the artifact. By default, one
written into the path does not reach it either. With `advisories.recordSourcePath: true`, the
artifact records the path, so a credential written into it reaches every consumer and every reader
of the advisory bucket. Sync logs parsed compilation time and row count from metadata, including when
it reads older artifacts with complete source URLs. On each swap it adds one `info` line naming
where the serving artifact came from: the object's publication time, the OSV source as
`host:port`, its newest advisory date, and the EPSS score date. A value the artifact never
recorded reads as `<unrecorded>`, which is how the EPSS score date reads on an artifact published
with `epss_status=unavailable`. Malformed or oversized display values appear as absent without
changing artifact acceptance.

The boot configuration echo prints configured endpoint
values. Use the dedicated [secret settings](@/docs/configuration.md#secrets) rather than putting
secrets into URLs.

## Alerting on `ERROR`

**Point a monitor at `status: error` and page on it.** These failures need operator attention,
even when a later retry can recover. They include:

- A sweep cycle that halted, or a Dredger latched and running no cycle at all.
- A store that refused a delete, or never received one.
- An advisory sync that could not be prepared: no database within the boot budget, or a
  published artifact Écluse refused.
- A mirror job nothing else can capture, and an artifact whose bytes failed their digest.
- A background loop that failed up and took the process with it.
- An advisory source that has gone quiet past its threshold
  ([Advisory quiet time](@/docs/operations.md#advisory-quiet-time)).
- An advisory database a mount's rules cannot consult, when the outage begins and every 15
  minutes while it lasts ([Advisory outages](@/docs/operations.md#advisory-outages)).
- A Pilot compile that published nothing because its ecosystem has an active rule that reads EPSS
  scores, such as `DenyIfEpss` or the shipped remediation fast lane, and the EPSS feed failed. The
  line starts `Aborting OSV compile:` and names the feed as `host:port` and the cause
  ([When the EPSS feed fails](@/docs/configuration.md#when-the-epss-feed-fails)).
- A Pilot publication that its guard refused, or that the store could not serve as the guard
  needs. The line starts `Advisory publication stopped:` and names the reason
  ([Advisory publication guard](@/docs/operations.md#advisory-publication-guard)).

Use the severity together with the event and its repetition:

| Status | What it means | What to do with it |
|---|---|---|
| `error` | A failed operation, exhausted budget, or halted role needs attention. Some conditions can recover on retry. | Page and check the affected role. |
| `warn` | Écluse absorbed a problem and carried on degraded. | Chart it, and alert on a sustained rate rather than on a line. |
| `info` | Audit and lifecycle records: mirrored artifacts, completed sweeps, deletions, Pilot compilation start and completion, advisory publication and activation, and deployment posture. | Retain these records for investigation. |
| `debug` | Ordinary serve requests, serve-time denial details, expected metadata absences, and download or parsing details. | Enable it during an investigation, then restore `info`. |

Serve-time denial counters remain available when telemetry is enabled, regardless of the log level.
Use those metrics to identify changes in denial activity. Set `ECLUSE_OBSERVABILITY__LOG_LEVEL=debug`
when the individual decisions are needed, then restore `info` after the investigation.
Routine reads and policy denials do not produce default-level request logs.
This also means a serve-only deployment has no default audit history of normal admissions.

Successful mirror publications carry `data.event=mirror_publication`, package, version, ecosystem,
artifact filename, the verified artifact digest, and the evaluated admission rationale.
The rationale uses the decision already made for that artifact, without another rule evaluation.
It includes the winning rule and skipped-check evidence. It does not claim a complete policy hash
or identify an advisory generation that the admitted decision does not carry.
A successful registry response can be idempotent, so this record does not prove that the artifact was newly created.
A job that finds a version already complete reports that repeat completion at `debug`.

Typical `warn` lines record:

- An upstream Écluse could not reach.
- A mirror job left to redeliver.
- A store call Écluse is retrying.
- A malformed advisory entry Écluse dropped, or an advisory date it had to ignore.
- An EPSS feed that failed for an ecosystem with no active rule requiring EPSS enrichment. The line starts
  `EPSS enrichment unavailable`, and Pilot publishes that ecosystem's artifact without scores.
  An EPSS outage repeats it on every compile.
- A background loop backing off.

A loop that keeps failing warns on every attempt, so `error` alone does not catch a slow death.
The mirror worker is covered: a stalled consume loop fails `GET /livez`
([Health probes](@/docs/operations.md#health-probes)). Every other background loop needs a rate
alert on the warnings carrying its name.

### Advisory outages

When a rule that reads the advisory database cannot consult it, you see the outage as a transition
rather than as a line per request. Écluse keeps one outage state per ecosystem and reports it three
ways:

| Line | Level | When |
|---|---|---|
| `advisory source outage began` | `error` | The first evaluation that cannot consult the database. It names the ecosystem, the rule, and the cause. |
| `advisory source outage continues` | `error` | At most every 15 minutes while any rule still cannot, listing each rule that last reported it could not, with its latest cause. |
| `advisory source outage recovered` | `info` | Once every rule consults the database again. |

The causes are no database loaded, a push past its maximum age, a lookup fault with its detail, and
an open circuit breaker. The report covers every advisory rule a request reaches, the remediation
fast lane under either `onUnavailable` included. Under `skip`, the fast lane abstains and an
admission that follows records the skipped check. A request never adds a line, so an outage costs
the log the same whatever the traffic. The report
fires only on traffic that reaches an advisory rule, so an idle mount reports nothing, and the sync
task's own `error` line covers a database that never loads. A request stops at the first rule that
decides, so a rule that a higher-precedence rule always decides ahead of never reports, and the
continuing line names only the rules requests reach. The recovery line follows the next
evaluation that finds the database answering, so a source no rule asks again reports no recovery.

A failed poll of the advisory store logs `error` on the same pacing, whether or not a database is
loaded: `sync fetch failed` at the first failure, `sync fetch still failing` at most every 15
minutes after it, and `sync fetch recovered` at `info` on the first poll that succeeds again.

The outage line says the checks are degraded. Which admissions went through without them is a
separate record. A version admitted while an advisory deny set to `onUnavailable: skip` could not
vet it carries that check in its decision, and the public artifact gate logs one `warn` line per
skipped check, `admitted with a check skipped for unavailability`, with the package, the version,
the rule, and the cause. The line is bounded: once per package, version, and skipped rule set for
the life of the outage, cleared when the source recovers, so a mount with no mirror that admits the
same version on every public serve logs it once per outage. The record behind that bound holds
4096 identities per ecosystem and forgets the oldest past that. A trusted read never logs it.
The mirror worker records each skipped check once per successful re-admission.
A job attempt can re-admit more than once as its expected inventory expands.
Retries can also repeat this evidence, independently of the public gate's outage bound.
The evidence lives in that decision and in the log only, never in registry metadata, so its retention is the log's.

A one-shot `ecluse pilot compile` using the same Prometheus port as a live Pilot can log a bind
failure and still complete its compilation. This applies only when the Prometheus exporter is selected
([Telemetry](@/docs/operations.md#telemetry-opt-in)). Any other failure to bind that listener wants
a look.

## Advisory quiet time

Pilot reads how old its sources say their data is, and tells you when one stops changing. After
each compile it logs the age of the ecosystem's newest advisory record and of the EPSS feed's
declared score date, at `info`. A compile that published with `epss_status=unavailable` has no
EPSS score date, so it logs no EPSS age. When either age passes its threshold, the same line repeats at
`error` and names the ecosystem, the source as the artifact records it, the age in seconds, and the
threshold in seconds.

The thresholds are `advisories.quietTime.<ecosystem>` and `advisories.epssQuietTime`, both in
seconds, and both seven days by default. That default comes from measured change frequency: over
60 days the longest gap between npm or PyPI advisories was four to five days.

A quiet source and a stalled export look the same from the bytes, so you decide which one you
have. Two remedies:

| What you found | What to do |
|---|---|
| The ecosystem really is this quiet. | Raise its `advisories.quietTime` entry past the gap you measured. |
| The export has stopped updating. | Point `advisories.osvExportBaseUrl` or `advisories.epssFeedUrl` at a source that is still publishing. |

The alarm never refuses a version. It reports what the artifact says about its own sources.

## Advisory push age

`DenyIfKnownMalicious`, `DenyIfCve` and `DenyIfEpss` decide on evidence, and evidence goes out of
date. Past a maximum age, each refuses instead of deciding, and the request gets a retryable `503`.
The denial message says how old the push is and what the maximum was. `onUnavailable: skip` does
not waive it, and an open circuit breaker does not skip past it. `AllowIfRemediatesCve` follows its
own `onUnavailable` on the same evidence. The shipped `deny` refuses, and `skip` leaves the version
to the quarantine.

A configured advisory source activates `DenyIfKnownMalicious` at precedence 300, above every
default allow. An expired or undated push therefore refuses every public admission on the mount,
scope and identity allows included. Only an allow ordered before the rule still admits: one above
300, or one at 300 whose name sorts first. Trusted private reads still serve.

The age is the time since the artifact was last pushed, taken from the published object's own
timestamp. Pilot writes that object after every publication its
[guard](@/docs/operations.md#advisory-publication-guard) accepts. Each publication carries new
bytes, so a consumer downloads it even when the sources have not changed. A refused publication
writes nothing, so the age keeps growing. A push of identical bytes, such as an operator copying the
object onto itself, moves the age without resetting installation age. A restart re-reads the
timestamp from the object, and a failed poll leaves it where it was. A rejected artifact cannot
refresh the last accepted artifact's publication age, including repeated HEAD responses for its rejected ETag.
EPSS qualification rejection therefore grants no extra freshness interval.

An artifact the object store gives no publication time for has no age to check, so it is refused
on the same terms as an expired one: the rules never admit evidence they cannot check. Écluse
logs `error` naming that artifact when it swaps it in. Before the first sync nothing is serving,
which is the ordinary absent-database case and still follows `onUnavailable`.

`advisories.maxAgeSeconds` sets the maximum. Unset, each mount derives its own: a day ahead of
that mount's earliest `AllowIfOlderThan` quarantine, and never under three days. The shipped
seven-day quarantine gives six days, so you see the failure before the next quarantined cohort
would have been admitted. A mount with no quarantine rule gets three days. The boot log names the
value and where it came from, once per mount.

At half the maximum, Écluse logs `error` once, naming the ecosystem, the push time, the age, and
the maximum. It logs once per crossing, not once per poll, and re-arms when a fresh push brings
the age back under half. `ecluse.advisory.source.age.seconds` carries the same age for a
dashboard. `ecluse.advisory.database.age.seconds` reports nothing at all while no artifact has
loaded, so an alert on it never reads a never-filled slot as a fresh database.

Two remedies:

| What you found | What to do |
|---|---|
| Pilot has stopped pushing. | Get it running again. Its own logs say whether the fetch, the compile, or the publication failed, and why its [guard](@/docs/operations.md#advisory-publication-guard) refused a publication. |
| The maximum is shorter than your update cadence. | Set `advisories.maxAgeSeconds` to a value you can meet, and accept the older evidence that comes with it. |

Readiness does not change. `GET /readyz` still reports the mount as ready, because the database is
loaded and every rule that does not read it still decides when it ranks above the advisory
denies. The advisory denies refuse, and the MAL default refuses every public version that reaches
it. The Dredger stops deleting on an advisory match for the same reason, while an identity deny still
acts under its usual guards.

## Advisory publication guard

Pilot replaces a published advisory artifact only when each source's snapshot, from the same source
URL as the published one, is newer or the same, and the artifact has not shrunk sharply. A source
URL that differs from the published one starts a new order, whatever its date. Pilot reads the
stored object's metadata first, then writes only over the revision it read, so two Pilot runs cannot
overwrite each other's work. It orders the OSV export and the EPSS feed separately, each by the
`Last-Modified` its server answered with:

| What a source answered | What Pilot does |
|---|---|
| A later `Last-Modified` than the stored artifact's | Publishes. |
| The same `Last-Modified` and the same bytes | Publishes, which refreshes the push age. |
| No `Last-Modified`, and the same bytes | Publishes, and keeps the stored date for later comparisons. |
| An earlier `Last-Modified` | Refuses. |
| The same `Last-Modified` with other bytes, or other bytes and no date to compare | Refuses. |
| A source URL other than the stored one | Publishes, and starts a new order for that source. |
| A `Last-Modified` more than five minutes after Pilot's clock | Counts as no `Last-Modified`, so the same bytes refresh and changed bytes are refused. Pilot never stores that date as its ordering marker. |

On a first publication, or after a source URL changes, a future date leaves that source's stored marker
unknown. Changed bytes from that source are then refused as different bytes without a `Last-Modified`
on both sides, until you replace or delete the stored object.

The new artifact must also stay within `advisories.maxShrinkPercent` of the size baseline, 2% by
default. The baseline is the largest size accepted under the object key, and it travels in the
object's metadata. So the guard needs no object versioning and no version listing, and a lifecycle
rule on old versions cannot erase it. When an optional EPSS feed fails, the publication that loses
the scores may shrink by up to `advisories.epssLossCapPercent` more. While that credit is in force,
the baseline still records only the larger of itself and the accepted size, so an overstated credit
cannot raise it. The
[configuration reference](@/docs/configuration.md#the-configuration-reference) states both limits.

A refusal logs `error` on a line that starts `Advisory publication stopped:`. The stored artifact
stays, so consumers keep it until it reaches its maximum push age. A one-shot
`ecluse pilot compile --ecosystem <ecosystem> --out DIR --upload` exits non-zero, and the scheduled loop tries again on its next
cycle. The local artifact under `advisories.dataDir` changes only after a publication lands.

| The reason names | What it means |
|---|---|
| An older snapshot | The source served data older than what is published. A mirror can serve stale data. The next snapshot with a later `Last-Modified` publishes. |
| Different bytes under the published `Last-Modified` | Nothing orders the two snapshots. This clears by itself: the next snapshot with a later `Last-Modified` publishes. |
| Different bytes without a `Last-Modified` on both sides | Pilot orders a changed snapshot only when the new snapshot and the published artifact both carry that source's `Last-Modified`, so a source must send the header for a changed snapshot to publish. Neither a later run nor a rebaseline gets past it. A refusal caused by a future `Last-Modified` is the exception, if the published artifact carries a date. The next snapshot with a later, current date clears it. Point `advisories.osvExportBaseUrl` or `advisories.epssFeedUrl` at a source that sends the header, which starts a new order, or replace or delete the stored object yourself. |
| A shrink under the baseline | The feed lost coverage, or legitimately shrank more than the allowance. Check the feed before you [rebaseline](#rebaselining). |
| Missing publication metadata | The stored object lacks one or more of the guard's metadata keys, so Pilot cannot order against it and does not replace it. |
| A refused read, or an absence the listing could not prove | Pilot lacks a permission from [Role identities and least privilege](@/docs/deployment.md#role-identities-and-least-privilege). |
| A store without conditional writes | Pilot publishes only to a store that enforces `If-Match` and `If-None-Match` on writes. |

### Rebaselining

When you expect a smaller artifact, such as after a source correction, approve one upload against
the revision you inspected. For example:

```bash
ecluse pilot compile --ecosystem npm --out /var/lib/ecluse/advisories --upload \
  --rebaseline-etag '"<the stored object's ETag>"' --rebaseline-max-shrink-percent 10
```

The run still refuses older data and missing metadata. It replaces only the object with that ETag,
so a retry cannot carry your approval to a revision someone wrote since. Its shrink is held to the
percentage you give, and the baseline then restarts at the new artifact's size. The scheduled loop
never rebaselines.

### Restoring an older artifact

Rollback is a storage action you take, outside Pilot. Write an older artifact to the object key
yourself, for example by restoring an earlier object version. Consumers accept it after their usual
verification and push-age checks. That restore can admit versions a later generation denied, and
the mirror worker can then refill copies the Dredger removed. A version restore keeps the older
artifact's publication metadata, so Pilot's next newer snapshot replaces it as usual. An artifact
written without that metadata stops Pilot's publications, as above.

## Telemetry (opt-in)

The repository provides a [Datadog Terraform example](https://github.com/AlexaDeWit/Ecluse/tree/main/examples/datadog)
with a deployment-scoped dashboard and signal inventory.
Copy and adapt it for your deployment. It does not configure collection or retention.

`ecluse.upstream.requests` counts public HTTP attempts from the proxy and mirror workers.
Its attributes are `server.address` (the formed request host), `kind`, and `status`.
Kinds are `listing`, `selected_read`, `artifact`, `companion`, `worker_metadata`, and `worker_artifact`.
Status carries the exact HTTP code, `transport_failure`, or `cancelled`.
A received status counts before the body, so a later body failure does not count again.
Cache hits, followers, local cooldown refusals, private reads, and target writes count nothing.
Failed connections count as transport failures. Attempt notification does not prove registry receipt.

The Datadog example shows rates, exact `429` rates, and 30-day totals by host.
For a scoped Datadog query, sum this count query over a rolling 30-day window:

```text
sum:ecluse.upstream.requests{ecluse_deployment:platform-east AND env:production AND (service:ecluse-proxy OR service:ecluse-mirror)} by {server.address}.as_count()
```

Replace the example deployment, environment, services, and ingestion names with your own values.
Include every proxy and mirror service for the deployment total.
New deployments, telemetry export gaps, and retention shorter than 30 days leave an incomplete total.
This count excludes registry traffic from other applications.

Metadata reads record HTTP refusals on `ecluse.upstream.fetch.errors` with cause
`upstream_status`, including `404`. A malformed successful response records `decode` instead.
Explicit `401` and `403` refusals retain `other` and their existing warning log.
Metadata `408`, `429`, and server errors log at `ERROR`. A genuine `404` absence logs at `DEBUG`.
Other non-success statuses log at `WARN`. These logs carry the status and upstream authority,
without the upstream body, credentials, or URL query.

Telemetry stays off until you ask for it. Set `ECLUSE_OBSERVABILITY__TELEMETRY=on`, then give the
instance its identity: `DD_*` (`DD_SERVICE`, `DD_ENV`, `DD_VERSION`, `DD_AGENT_HOST`) for
Datadog, or the standard `OTEL_*` variables for any other backend. `DD_*` wins where both are
set, and the resolved identity stamps both traces and every log line. With no `DD_VERSION` or
`service.version` set, exported traces and log lines carry the running binary's own build
version, so the version tag is never blank.

Écluse exports only to a node-local collector or Agent, at `http://localhost:4318` by default or
wherever `DD_AGENT_HOST`/`OTEL_EXPORTER_OTLP_ENDPOINT` points. That is why `DD_API_KEY` and
`DD_SITE` have no effect. Authenticate a remote collector out of band with
`OTEL_EXPORTER_OTLP_HEADERS`.

Metrics travel either way. They push over OTLP beside the traces by default. Set
`OTEL_METRICS_EXPORTER=prometheus` and Écluse serves them for a scraper to pull instead, in
Prometheus text exposition format, at `GET /metrics` on a listener of its own. It is never on the
proxy port your npm clients reach: that port answers `/metrics` with the same `404` it gives any
other unmounted path, whatever the transport. `OTEL_EXPORTER_PROMETHEUS_HOST` (default
`localhost`) and `OTEL_EXPORTER_PROMETHEUS_PORT` (default `9464`) address the listener. It reads
the instruments at the moment of the scrape, so `OTEL_METRIC_EXPORT_INTERVAL` does not apply to
it, and a scrape never enters the proxy's request path, so it adds nothing to the
`http.server.*` series. The listener runs only while telemetry is on, and a port it cannot bind
is an error in the log rather than a failed start.

Écluse hands its metric records to the SDK in batches: once a second, and again before every
scrape and OTLP export, the final export at shutdown included. A scrape or an export therefore
still sees every record made before it. Three details follow from the batching. A gauge point
carries the time of the batch, at most a second after the record. An exemplar carries the trace
and span of the request that made the record, but the time of the batch. A counter's exemplar
holds the total added since the previous batch, not one request's increment, and a gauge's
exemplar holds the newest reading in the batch.

**Give every co-located Prometheus exporter its own port.** These exporters all read
the same variable, so two on one host race for 9464. The loser logs the bind failure and serves
nothing. A scraper pointed at that port then collects one role's series and sees no sign the
others are missing, which reads on a dashboard as quiet rather than broken. So set a distinct
`OTEL_EXPORTER_PROMETHEUS_PORT` per role and scrape each one. A one-shot `ecluse pilot compile`
run beside a live Pilot boots the same way, so it attempts the same bind and logs the same error
before doing its work. That one is harmless.

**Keep that port inside your network.** The exposition carries the whole OpenTelemetry resource,
so it names your host and its machine id, the process owner, executable path, working directory
and container id, and whatever cloud or Kubernetes identity the SDK detected, next to your own
rule names. The `localhost` default reaches nothing off the host. Widening it with
`OTEL_EXPORTER_PROMETHEUS_HOST` publishes that inventory to whoever can reach the port, so pair
the change with something that decides who can.

Traces push over OTLP either way, so the endpoint variables still matter on a scraped deployment.

The W3C baggage limits cap `OTEL_RESOURCE_ATTRIBUTES` at 8192 bytes in total, 4096 bytes per
attribute, and 180 attributes. Écluse admits its own identity first, then your attributes in key
order, and warns once at boot naming every key that did not fit.

### Cache retention and collapsed requests

Use cache outcomes to separate retained data from requests that share an active fetch.
The request counters use `result=hit|miss|collapsed`:

| Metric | What it counts |
|---|---|
| `ecluse.metadata_cache.requests` | Full-document store requests |
| `ecluse.metadata_cache.version.requests` | Selected-version store requests |
| `ecluse.metadata_cache.assembled.requests` | Assembled-response store requests |
| `ecluse.metadata_cache.refused` | Capacity refusals and backend failures, by `store=full|version|assembled` |

A `hit` uses a retained value. A `miss` leads a fetch or render. A `collapsed` request joins
an existing leader and shares its result, including a failure. A follower that retries after
leader cancellation keeps its original classification, so one request never counts twice.
Capacity refusals count fetched values, not followers. A value above the aggregate byte bound causes no eviction.
Under shared pressure, a refused insertion can evict its own store down to its floor.
Backend failures count failed storage operations, including reads and writes.

Selected reads use only the selected retention capability. Their denominator is the sum of
`version.requests` outcomes. The retired `version.full_hits` counter is no longer emitted.
A provider may retrieve a selected projection from its own storage without loading a full entry
through the generic cache.

The `ecluse.metadata_cache.resident_bytes`, `ecluse.metadata_cache.version.resident_bytes`,
and `ecluse.metadata_cache.assembled.resident_bytes` gauges report accounted bytes after insertion,
eviction, and expiry removal. `ecluse.metadata_cache.entries` reports the full store's entry count.
The shipped local backend never retains full metadata. Full-store entries and resident bytes stay
zero, and local full reads report misses or collapsed active work without capacity refusals.
Selected-version, assembled, and public RubyGems directory stores share `cache.ttl`, one byte bound
and one entry bound, with no reserved shares. No gauge reports the directory store's bytes.
A full listing followed by a selected-version read can fetch upstream twice, because no store keeps
the full document. No external cache service is required.
Boot and `check-config` output report these local capabilities. `cache.maxBytes` applies only to
eligible local retention. Increasing it cannot enable full retention.
Each store evicts its own least-recently used entries under shared pressure. It cannot evict another
store's live entries. The shipped eviction floors are zero. If the other store holds the capacity,
the request serves without retention until expiry releases room.
One selected provider owns every metadata retention capability. Unsupported capabilities stay
uncached. A failed backend read fetches metadata from its origin or rerenders an assembled response
from authorised inputs. A failed write skips retention. Neither failure uses a retained local fallback.
Recency hints and occupancy reporting do not require a remote provider to use local LRU or report
exact server memory. Provider occupancy, when available, reports its charged bytes and entry count.

Local expiry removal checks every store on access or a retaining insert, without a background timer.
If the stores are idle, their gauges can retain expired charges until the next operation.

### Credential expiry

`ecluse.credential.token.ttl.seconds` reports the shortest remaining lifetime among observed
expiring credentials with the same `provider` label. Écluse measures whole seconds at each
collection and clamps expired credentials to zero. A successful refresh replaces the observed
expiry. A completed failed refresh reports the cached token's expiry.

Five consecutive mint failures open the breaker for 60 seconds. Requests during that cooldown
cannot mint, and refusals do not count as completed refresh attempts. Collection still measures
the cached token's lifetime, including when it expires during the cooldown.

After cooldown, one mint tests recovery for that provider. Other expired callers receive
`BreakerOpen` immediately. Valid cached tokens remain available while the probe runs. Cancellation
releases the probe for an immediate retry and emits no completed refresh outcome. Only the current
owner can settle or release its claim.

Refresh is demand-driven. An idle credential can expire without a provider fault, so zero TTL
alone is not an outage signal. `ecluse.credential.refresh` counts completed attempts by `provider`
and `result`. Read those outcomes alongside credential demand and failures of mirror writes.

The bounded `provider` label remains `registry`, `codeArtifact`, or `verdaccio`. Shared
CodeArtifact credentials contribute once, even when several ecosystems use them. Static providers
emit no TTL observations. The eager construction mint emits neither a refresh event nor an
expiry observation.

## Memory plan and runtime sizing

Run each Écluse pod with at least 1 GiB of memory. Écluse does not support smaller pods. At
512 MiB a proxy under load often overflows its heap. Each overflow ends the process with exit code
`1` and the line `ecluse: service exited: heap overflow`. The boot does not refuse a smaller pod,
so keep the minimum in your pod specification.

Check the effective plan in the boot log or `ecluse check-config` before changing pod resources.
Both use the same plan renderer. The checker predicts the runtime posture, while boot measures
what the runtime applied, so compare them under the same container limits and configuration.
The [configuration reference](@/docs/configuration.md#the-configuration-reference) owns the keys,
defaults and override rules.

The controls serve different purposes:

| Control | What it decides |
|---|---|
| Metadata ingest ceiling | The maximum decompressed source body accepted from one metadata response |
| Structural limits | Protocol-specific version and file counts, and retained structure depth |
| CPU admission | How much metadata work runs concurrently |
| Memory admission | How much live data the metadata requests in flight hold at once |
| Cache budget | How much eligible metadata the selected provider retains locally |
| Runtime heap ceiling | The heap limit applied to the process |

A larger count bound lets more requests share public reads and wait for upstreams.
Private listings still parse per-request, so more concurrency can increase their memory use and latency.

The default data ceilings leave growth room for large real-world metadata while keeping input work bounded.
They express policy headroom, not a measured maximum that fits every pod. A larger ingest ceiling
does not automatically enlarge the cache or reduce CPU concurrency.

### Memory admission

Memory admission keeps the heap's live data where the garbage collector still works normally, so
the process neither thrashes its collector nor overflows its heap ceiling. Écluse charges each
listing and each public artifact decision for the memory it uses, as it uses it, against one budget:

| Work | What it pays |
|---|---|
| Entering, before the request takes a CPU slot | One 1 MiB step |
| A full metadata read, which a listing makes | Per decompressed source byte, chunk by chunk before parsing: 1.1 bytes for npm, 3.5 bytes for PyPI, 1.7 bytes for Cargo, 18.0 bytes for RubyGems |
| A listing's response | Per byte of its output basis, before assembly: 2.0 bytes for npm, 1.9 bytes for PyPI, 2.6 bytes for Cargo, 4.6 bytes for RubyGems |
| A selected read, which a public artifact decision makes | Nothing beyond the entry step |
| An assembled hit or a `304` | Its reads, but no response charge |
| A gzip listing response, hit or miss | 512 KiB for the deflater, then 36 KiB for each 32 KiB output buffer |
| A request that joins another request's public fetch or render | Nothing for that fetch or render |

A listing's output basis is the larger of two estimates. One starts from the largest document the
listing merges. The other starts from the base document, whose top-level fields the response
keeps: the private document when there is one. Each adds the other document's size in proportion
to the versions the starting document does not hold. A package mirrored into a private upstream
renders about one document, so it pays for about one. A listing of one document, or of two that
share no version, pays for every byte it merges.

A listing gives charges back as it finishes each phase. When a full read completes, its charge
steps down to what the read keeps: 0.9 bytes per source byte for npm, 3.3 bytes for PyPI, 1.5 bytes
for Cargo, 17.3 bytes for RubyGems. A failed read keeps nothing. Once the response body exists, the reads' charges go back, so only the response
charge and any gzip charge stay while the body is sent.

RubyGems per-gem `/info` documents are small, but their per-byte charges are the highest of the four
ecosystems. A RubyGems `/info` reply also pays twice its admitted record bytes, plus 4 KiB, for its
native serialisation. A `/versions` reply has no per-byte factor. Each directory scan pays about twice
the bytes it retains while it reads, and the response pays twice the merged directory's bytes plus
the merge workspace. The rubygems.org directory captured on 2026-10-28 is 24.5 MB, so the public
directory is the largest RubyGems sizing input. A request that joins another request's directory
fetch, or finds the public directory in the local cache, pays no public scan.

For a listing that reads at least 1 MiB, the per-byte charges sit above the largest the residency
tests measure on the captured package corpus, for one document and for merges that model the
private copies deployments serve. Those are the most its full reads hold while they parse, and a
response's encoding with its copy. A smaller listing can peak above its per-byte charge. The tests
hold every listing's reads and render within what the meter holds for it, whole 1 MiB steps and at
least the entry step, and fail when one would need more. A document shaped or sized unlike that
corpus can hold more than its charge. The sampler measures the excess as live data outside the
charges after the next major collection.
The ceiling the budget may reach, described below, falls as that remainder grows.

A request in the memory gate's waiting room has a 1 s budget for its entry step. If that wait
expires, it gets `503` with `Retry-After: 1`. A request that has started reading pauses instead,
keeping its CPU slot until
memory frees. A pause during an upstream read fails when it outlives the 50-second cap on that
exchange, and a pause before a render fails at the 60-second request timeout. Only one request at a
time may run past the budget, and it keeps that right until it ends. Work that request waits on, a
public fetch or a render another request leads, runs past the budget with it. So the overshoot stays
near one request and the shared work it waits on, and a pause always leaves a request that
finishes. A listing gives up its CPU slot once its response is built, but keeps its charge while
Écluse writes the body, so a slow client holds memory and no CPU slot. Everything a request paid
returns when it ends. Trusted private artifact hits and artifact relay pay nothing.

A request that finds either waiting room full can wait in overflow outside both gates, without a
CPU slot or memory charge. It can retry admission for 0.5 s. The next sweeper tick ends an expired
wait, normally within another 100 ms. A freed place wakes the oldest request waiting for that gate.
New requests can still enter first, and a woken request follows the normal admission rule.

A retry uses each gate's normal one-second room wait. Those waits can occur before overflow and
after its last wake, so total time before a refusal can exceed 0.5 s. No new retry starts after the
overflow budget expires. A refusal still carries `503` and `Retry-After: 1`.

Overflow has 8 reservations per serve slot, 320 at 2 CPUs. A request keeps its reservation through
retries and any work they admit. Past that cap, Écluse answers `503` at once with `Retry-After: 1`
and permits normal connection reuse. This avoids forcing new connections after overflow refusals between an ingress or sidecar and
the pod. It applies to HTTP/1 and HTTP/2. The queue limits held overflow state, but
does not impose a per-client request rate. A proxy can retry `503` responses according to its
configured policy, independently of connection reuse.

The budget starts at a quarter of the heap the nursery leaves, less the cache budget, the queue
tenants and the idle process. That is the live data at which the copying collector still runs
normally. A sampler reads the collector and the cgroup ten times a second and moves the budget:

- It halves the budget when the collector takes more than half the CPU over the last second, when
  live data after a major collection passes 80% of the point where copying would overflow the heap,
  or when the cgroup's memory use, less reclaimable page cache, passes 90% of its limit. It halves
  at most once a second, so a sustained surge still drives the budget down, one step each second.
- After each major collection it measures the live data outside the charges: the cache, the idle
  process and any error in the charges. The budget may grow until the charges and that remainder
  reach a third of the heap the nursery leaves, and it drops at once when a larger remainder lowers
  that ceiling. A smaller remainder counts a quarter at a time.
- The ceiling also leaves room under the heap's overflow point for the largest request seen lately,
  since that one request may run past the budget.
- After a calm second, with the collector under 35% of the CPU, it grows the budget by an eighth
  toward that ceiling.

The `memory plan: transient budget` boot line names the starting budget, its live target, the
deductions, the floor and the live ceiling. Without a heap ceiling the budget starts at 1 GiB, and
only the collector's share moves it.

| Metric | What it reports |
|---|---|
| `ecluse.serve.admission.memory.budget_bytes` | The current budget |
| `ecluse.serve.admission.memory.charged_bytes` | What the requests in flight hold against it |
| `ecluse.serve.admission.memory.brake_level` | `0` calm, `1` holding, `2` braking |
| `ecluse.serve.admission.memory.waiting` | New requests waiting at the memory gate now |
| `ecluse.serve.admission.memory.paused_now` | Started requests paused for memory now |
| `ecluse.serve.admission.memory.queued` | Requests that waited for their entry step |
| `ecluse.serve.admission.memory.shed` | Requests refused at the memory gate |
| `ecluse.serve.admission.memory.pauses` | Times a started request paused for memory |
| `ecluse.serve.admission.memory.overdraws` | Steps a request took past the budget |
| `ecluse.serve.admission.overflow.waiting` | Requests parked in overflow now, excluding retries and admitted work |
| `ecluse.serve.admission.overflow.admitted` | Overflow requests that reached a room or ran after a retry, including later room expiry |
| `ecluse.serve.admission.overflow.shed` | Requests refused when the overflow retry budget expired |
| `ecluse.serve.admission.overflow.full` | Requests refused because all overflow reservations were occupied |

A `503` with `Retry-After` from either gate is backpressure, so exclude it from alerts. A brake
level that stays at `2`, or a `shed` count that keeps rising, means the pod needs more memory for
its package mix. A rising `overflow.full` count means queued requests, retries and admitted work occupy all overflow
reservations. Consider more replicas or slots.

### The rest of the plan

The memory plan also accounts for the runtime reserve, the enqueue buffer, cache retention, publish
bodies, the in-memory queue and mirror-artifact work. The cache budget is 30% of the live target, at
least 64 MiB, so it grows with the pod. Their accounted sum does not measure all live process
allocations. Small automatic plans shed mirror-artifact capacity before cache retention. Read each
warning for the resulting loss of capacity. An explicit override can still fail plan validation. The
ingest ceiling is independent of those tenant allocations.

Cores and the heap ceiling resolve at boot from config, else the cgroup, else a capped fallback.
The log records each decision and its source. The
[appendix](@/docs/operations.md#appendix-runtime-sizing-arithmetic) explains the resource arithmetic.

Écluse reads its memory limit from cgroup v2's `memory.max`. Under cgroup v1, or any layout
where the container cannot see that limit, Écluse boots as if unlimited: no heap ceiling applies,
and the boot log's `runtime: max heap unbounded` line reports that it found no cgroup memory
limit. Set the heap ceiling and the core count by hand in that case, with `runtime.maxHeapBytes`
(`ECLUSE_RUNTIME__MAX_HEAP_BYTES`) and `runtime.cores` (`ECLUSE_RUNTIME__CORES`).

A warm selected-version or assembled response can avoid repeated work, and simultaneous eligible
reads can share an active fetch. The local provider never retains full metadata. A warm-up install
therefore does not promise that later full listings avoid origin reads. Test cold listings and
selected reads as well as retained hits before admitting production traffic.

### Upgrade existing limits and admission pins

Review your config document and deployment environment together. Environment variables override
the document, so removing a document pin alone does not restore the automatic value.

| Existing setting | Upgrade consequence | Action |
|---|---|---|
| `limits.maxResponseBytes: 12582912` | The old 12 MiB pin still refuses larger metadata after the upgrade | Remove the pin to adopt the shipped ingest ceiling, or retain it as an intentional policy |
| A larger response pin used as a workaround | The declared ceiling still wins, including above the shipped default | Compare the effective ceiling and warning with your source sizes and process headroom |
| Explicit `limits.maxVersionCount` or `limits.maxArtifactCount` pins | The existing count policy still applies | Remove old pins to adopt the larger defaults, or keep the intended restriction |
| An explicit `runtime.serveMaxInFlight` pin | The declared positive concurrency still wins, and it also sizes the memory gate's waiting room | Review it against the pod's cores and concurrent workload |
| No explicit response or CPU pin | The new automatic controls apply | Compare boot output with `check-config` under the deployment's actual resources |

Response and CPU pins are not silently clamped. Read the override warnings before rollout.
The `memory plan: metadata ingest ceiling` line names the effective body limit.
The `memory plan: transient budget` and `runtime: serve admission` lines name the two admission
controls. A smaller memory budget does not reduce the body ceiling or CPU pin.
Raising the ingest ceiling admits more input, not more memory. A formerly refused package can now
reach parsing, policy and assembly work, so repeat your install and latency checks without widening
performance budgets to hide a regression.

## Revoking a mirrored version (internal yank)

An upstream yank does not revoke a trusted private copy. A new deny stops public admission and
worker re-admission when it wins under the configured precedence, but private hits continue until
the serving copies are removed. Deletion can lose the only remaining bytes, so identify the exact
version and stores before acting.

1. Add the identity denial to the intended policy and roll it to the proxy, mirror worker, and
   Dredger. Leave its `appliesTo` unset, or name both phases, because the Dredger passes over a
   deny limited to admission. Verify that it wins over any deliberate allow. Older workers and
   in-flight transfers can still publish during the rollout.
2. Run [Dredger](@/docs/dredger.md) with independent consent for the mirror and private cache.
   It removes eligible mirror versions before their eligible cache copies under the shared policy.
3. Inspect per-target results. A source success does not prove cache completion. A restart or
   later cycle rediscovers residual cache copies. Use `DeletePackageVersions` for CodeArtifact
   cleanup, never disposal. `--dry-run` previews both inventories without deleting anything.
4. Verify both store inventories and authorised metadata/artifact reads after earlier writes
   settle. Do not treat a permission error as proof of absence. Use fresh client state so an
   already-cached artifact does not stand in for a registry read.

Keep denial before deletion, and remove the source copy before retained downstream copies.
Otherwise a verification read or an old writer can refill the private repository. One successful
deletion or negative read does not establish that no late writer remains. The CodeArtifact
[retention contract](https://docs.aws.amazon.com/codeartifact/latest/ug/repo-upstream-behavior.html)
explains why retained copies outlive upstream deletion.

Removing an allow alone is not revocation. If that leaves only deny-by-default, Dredger keeps the
mirrored version. If it exposes a winning named deny that applies at revocation, normal eligible
removal applies. A deny limited to admission with `appliesTo: [admission]` refuses new requests
and worker re-admission, and it never revokes a stored copy
([Where a rule applies](@/docs/configuration.md#where-a-rule-applies)). Installed or client-cached
bytes remain outside registry revocation.

### Policy rollout order

Use the same intended configuration across roles, Pilot included. Pilot reads its own copy to decide
whether a failed EPSS feed stops an ecosystem's artifact. When tightening policy, update admission
and writer roles before Dredger. When you add a rule that reads EPSS scores, a `DenyIfEpss` or a
differential `AllowIfRemediatesCve`, update Pilot before them all
([When the EPSS feed fails](@/docs/configuration.md#when-the-epss-feed-fails)). When relaxing a
deny, update Dredger before writers can rely on the new permission. These are ordering
recommendations, not an atomic cutover requirement.

The `appliesTo` key adds three points to that order:

| Change | Order |
|---|---|
| The first configuration that carries `appliesTo` | Update every role's binary first. A binary that predates the key refuses the configuration as an unknown rule key, so an older Dredger cannot start on a narrowed rule and delete on it |
| Narrowing a rule to `[admission]` | It relaxes the rule for the Dredger alone, so update the Dredger before the other roles |
| Widening a rule to `[admission, revocation]` | It tightens the Dredger. Pin every version you must keep, run `ecluse dredger --dry-run`, then run one full walk with `dredger.fullWalk`, because a candidate cycle does not see a rule change |

Old writes outlive the role that queued them. A mirror worker still on the old policy decides a
queued job by its own rules, so it can publish a version a newly started proxy denies. That proxy
then serves the version on a private hit, because a private read applies no rules. A second
deployment writing into one shared mirror has the same effect.

Convergence is eventual, not immediate. Once every participating role runs the intended policy and
the outstanding old writes finish, repeated sweeps of actual mirror and cache state find and remove
every eligible denied copy. Consent, first-party protection, backend availability and the per-cycle
cap still bound what one cycle removes. Roles left on permanently conflicting policies never
converge, which is a deployment fault rather than a Dredger limit.

Read each cycle's per-target results rather than assume a clean sweep. A refused or unavailable
backend leaves its copy in place and the run says so. A later cycle rediscovers that residual. It
also finds anything an old writer added after an earlier scan reported the name clean.

If an old Dredger deletes the only bytes during a rollout, later policy agreement cannot restore
them. A removed version returns only when a usable source still holds its bytes and something
admits it again. The [threat model](@/docs/threat-model.md) records that accepted residual as
risk 109, over deletions from the mirror target and from the private cache alike.

## Mirror receipts and their visibility

A durable queue hands the worker a message and hides it for a visibility window. The worker
holds every message it received, one job at a time, so a message far down a batch waits out
several windows before its own job starts. Écluse therefore renews each received message's
visibility continually, from the moment it arrives until the worker has decided it: acknowledged,
dead-lettered, or left unacknowledged to redeliver. The renewal asks for the same window the queue
granted, a third of the way into what is left of it, and never past the twelve hours SQS holds one
receipt for. A publish that fails transiently is the one case Écluse resets the window to zero for,
so that message redelivers at once instead of waiting; every other retry waits out its window.

When a renewal keeps failing, Écluse gives up on that one message inside its remaining margin,
writes a `warning` naming the transport reason, and leaves the message unacknowledged. A running
job for it is cancelled, and one still waiting is skipped. The queue makes that message visible
again once its window lapses, so another worker picks it up and the delivery counts against
`ECLUSE_QUEUE__MAX_RECEIVE_COUNT`. The other messages in the batch carry on while their own
renewals succeed. Grant the worker `sqs:ChangeMessageVisibility`: without it every job longer
than one window is handed to a second consumer mid-mirror.

## Poison mirror jobs

Some decoded mirror jobs cannot succeed: an artifact past `ECLUSE_LIMITS__MAX_ARTIFACT_BYTES`
or a publication that remains refused. On SQS, **attach a redrive policy with a dead-letter queue**
to the mirror queue. The worker leaves such a message
undeleted, your policy moves it to the dead-letter queue, and there you can read it and work out
what happened. At boot, Écluse reads the queue's redrive configuration. A queue with no policy
draws a loud start-up `WARNING` that poison messages have no terminus, and when the probe itself
fails, that warning names the missing `sqs:GetQueueAttributes` permission. In both cases the
process boots.

For a message the adapter decodes and delivers to the worker, Écluse applies
`ECLUSE_QUEUE__MAX_RECEIVE_COUNT` even without a dead-letter queue. At that budget it writes an
error log naming the job and the reason, and the `ecluse.mirror.jobs.processed` counter records
it at `result="discarded"`. **Alert on that series**, because every discard is a job nothing else
caught. That count is a floor. With a redrive policy attached whose own `maxReceiveCount` Écluse
can read, it runs one delivery above that count, so your dead-letter queue always captures first
and the discard path stays dormant. When the policy's count is unreadable the configured floor
stands alone. Those decoded jobs have a visible terminal signal through redrive or worker
retirement. Mirroring is demand-driven, so another client request can enqueue the artifact again
until you fix the cause.

An undecodable SQS payload is different. The adapter rejects it before the worker receives a
`QueueMessage`, so the worker's delivery budget, error log, and discard counter do not apply.
The adapter logs the decode failure at `debug`, below the default `info` threshold, without
dumping the payload. Configure SQS redrive to capture it. Without redrive it can repeat until
queue retention removes it, with none of the per-job worker signals described above.

## Public registry traffic {#registry-traffic}

Use these costs to estimate public traffic across all your deployments, including requests that bypass Écluse. A shared egress address concentrates bursts, while npm's terms also count traffic across the organisation.

### Requests per operation

These counts reflect shipped request paths checked on 2026-10-18, and the Cargo and RubyGems paths on 2026-10-29. They assume admitted packages outside `firstParty`, the local cache, and a stable registry. They exclude private reads, redirects, retries, and failures. A document means the full npm packument, the project's PyPI Simple index, the crate's Cargo sparse index file, or the gem's RubyGems `/info` file.

| Operation | Serve-only or copy unavailable | Complete private copy available |
| --- | --- | --- |
| Listing | 1 public document | 1 public document |
| Cargo discovery (`config.json`) | 0 public requests | 0 public requests |
| RubyGems `/versions` directory | 1 public directory unless the local cache holds it | 1 public directory unless the local cache holds it |
| Artifact or declared PyPI companion download | 1 public document unless the selected-version cache hits, plus 1 artifact or companion request | 0 public requests |
| RubyGems gem or compact specification download | 1 public document for each gem name the file name can parse as, usually one, plus 1 gem or specification request | The same public documents, and no public gem or specification request |
| Mirror copy pass | 1 public document unless npm finds a selected-version cache hit, plus files that need copying or verification and missing companions | No public read when the job confirms completion |

A successful admitted artifact GET can enqueue a separate mirror job. A companion request does not enqueue one. PyPI, Cargo, and RubyGems workers always read their own public document, because they need publication and source fields. PyPI workers copy the whole supported version, including its wheels, source distributions, and declared companions. RubyGems workers copy every platform gem of the version, each with its original compact specification. Completion passes and queue retries add requests.

First-party names never fall back to public registries. A private authorisation refusal also stops public fallback.

### Cache and mirror effects

The local cache never retains full public documents. Identical metadata reads share a fetch only while it runs in the same process. Each pod keeps separate sharing state and retained values, so more replicas can increase public traffic.

The selected-version cache keeps eligible entries for `cache.ttl`, which defaults to [60 seconds](https://github.com/AlexaDeWit/Ecluse/blob/4971c1a6422d1666cdf6a0750783f2b97bc43f78/config/default.yaml#L111-L129) (checked 2026-10-18). Entries can expire, be evicted, or exceed the retention budget. Listings do not populate this cache. An assembled listing cache hit avoids rendering work but still needs the public document first. An embedded npm worker can reuse a selected version from its serving process.

npm requests the full packument with `application/json` because age rules need its `time` map. PyPI full and selected reads use the same Simple-index request.

For example, 1,500 distinct npm versions can require 3,000 public requests through a serve-only mount: one metadata read and one tarball per version. This estimate follows the request table above and assumes no cache hits, listings, or retries. Complete private copies remove those public artifact requests. Listings still read public metadata.

[Configure mirroring](@/docs/mirror-setup.md) to reduce repeated downloads at scale. Keep package-manager caches in CI and stagger cold installs where possible.

### Registry limits and guidance

The npm and PyPI positions were checked on 2026-10-18, and the crates.io and rubygems.org positions on 2026-10-29. Recheck them before capacity changes. None of these registries guarantees a request allowance for your egress address.

| Registry | Published position |
| --- | --- |
| npm | [Open-Source Terms, Acceptable Use item 17](https://docs.npmjs.com/policies/open-source-terms/#acceptable-use) treats five million requests in a month-long period from an individual, organisation, or affiliated group as unreasonable. Lower volumes can also impose unreasonable load. This is not a safe quota or a per-IP limit. Contact npm sales for high-volume needs. |
| PyPI | [API policies](https://docs.pypi.org/api/#rate-limiting) say the edge currently applies no API rate limiting. PyPI can prohibit irresponsible consumers. It asks clients to identify themselves through `User-Agent`, preferably with contact information, and avoid thousands of requests within minutes. It recommends a mirror or cache for services that download many packages. |
| PyPI outlook | The [August 14, 2026 infrastructure update](https://blog.pypi.org/posts/2026-08-14-how-aws-powers-pypi-and-the-psf/) says PyPI is considering rate limits for its heaviest consumers. It does not announce a numeric allowance. |
| crates.io | The [data access page](https://crates.io/data-access) says the sparse index at `index.crates.io` requires no rate limits, and no rate limits apply to `static.crates.io` at present. Écluse reads both. The crates.io API, which Écluse does not use, allows at most one request per second with an identifying `User-Agent`. |
| rubygems.org | The [rate limits guide](https://guides.rubygems.org/rubygems-org-rate-limits/) sets a load-balancer guideline of 10 requests per second for the API and website, by client IP unless stated. Some endpoints can be cached by the CDN and allow higher rates. The guide does not name the Compact Index `/versions` and `/info` endpoints Écluse reads. |

npm's organisation-wide wording includes deployments and direct npm traffic across different egress addresses. Monthly totals alone do not describe the load from bursts.

## Appendix: runtime-sizing arithmetic

**Give Écluse whole cores.** A fractional CPU limit, say 3.5, has no good option. Claiming 4
capabilities overruns the CFS quota during a stop-the-world collection and freezes the process
mid-pause. Flooring to 3 strands the fraction. So pair an integer limit with `requests = limits`
(and exclusive cores where offered) to remove throttling structurally, since Écluse floors the
derived count.

**A pod with no CPU limit is the case to configure.** A CPU **limit** is a cgroup quota Écluse
reads, and it does not shrink the processor count the runtime sees. A CPU **request** is not a
quota. It reaches the container only as a scheduler weight. The same weight has meant requests up
to 3.4x apart across runc versions, so Écluse does not guess a core count from it. With no CPU
limit set, Écluse falls back to the count the memory limit can feed. With no memory limit either,
it caps at `ECLUSE_RUNTIME__CORES_CEILING` (8). Neither number is your request, and the boot log
warns and says so. On a 32-core node a 2-CPU-request pod with no memory limit therefore claims 8
capabilities, not 2. Tell it the number with the Downward API:

```yaml
env:
  - name: ECLUSE_RUNTIME__CORES
    valueFrom:
      resourceFieldRef:
        resource: requests.cpu
        divisor: "1"
```

Read `requests.cpu`, never `limits.cpu`: with no limit set, the kubelet substitutes the node's
allocatable CPU, which is the whole-node claim you are trying to avoid. `divisor: "1"` rounds up
to whole cores, so a `500m` request becomes 1.

**Bare metal and dev hosts** have no cgroup limits either, so they take the same ceiling of 8, or
the processor count when that is lower. Raise `ECLUSE_RUNTIME__CORES_CEILING`, or set
`ECLUSE_RUNTIME__CORES`, to use a bigger box fully.

**Set a memory limit, not only a memory request.** Écluse reads a memory limit from cgroup v2's
`memory.max`, and fits its allocation area and heap ceiling to it. With no CPU limit, the memory
limit also sets the capability count. Kubernetes uses a memory request to schedule the pod, but
writes no file Écluse reads for it, and Écluse has no key that takes it. So with a request alone,
Écluse sees no bound and boots on its generic defaults.

With no CPU limit, no `runtime` keys set and at least 8 processors on the node, Écluse boots with:

| Pod memory | Capabilities | Allocation area | Heap ceiling | Serve admission |
|---|---|---|---|---|
| 1 GiB limit | 4 | 32 MiB | 864 MiB | 80 |
| 2 GiB limit | 8 | 32 MiB | 1,760 MiB | 160 |
| A request of any size, no limit | 8 | 64 MiB | None | 160 |

A node with fewer processors caps the count at its processor count, and serve admission follows at
20 per capability. Under a limit, the allocation area and the heap ceiling follow the count too.
With a request alone, the allocation area stays 64 MiB. In load tests on two CPUs, the table's
counts scored best of those tried: 1, 2, 3, 4 and 8 at a 1 GiB limit, and 2, 4 and 8 at 2 GiB.

With a request alone, Écluse cannot see the request, so its memory use does not scale with it.
Under heavy load it bursts above a request sized for typical use. In load tests on two CPUs with no
limit, the median peak was near 990 MiB and the worst near 2 GiB. This matches the Kubernetes
Burstable class. Under node memory pressure, the kubelet evicts a pod above its request before pods
within theirs.

To keep Écluse inside a number, set a memory limit, ideally equal to the request:

```yaml
resources:
  requests:
    memory: 1Gi
  limits:
    memory: 1Gi
```

`runtime.maxHeapBytes` caps the heap alone. Socket buffers, OS thread stacks and native zlib state
sit outside the heap, and the heap itself can pass its ceiling by one allocation area between
collections. When Écluse derives a ceiling from a limit, it leaves room outside it for that memory:
an eighth of the limit (at least 32 MiB), plus one allocation area. Leave the same room above a
ceiling you set. A memory limit is the better tool for a whole-process bound. Under cgroup v1, or
any layout that hides the limit, Écluse cannot read it.
[Memory plan and runtime sizing](@/docs/operations.md#memory-plan-and-runtime-sizing) names the
keys to set by hand there.

**Size a proxy pod from measured process usage as well as the RTS numbers.** The boot sizes the
per-core allocation area (`-A`) from the memory limit: an eighth of the limit across the cores, in
whole MiB from 4 to 64. A heap ceiling you set that is tighter than the memory limit, or set with no
memory limit, takes the limit's place, so the nursery still fits. The heap ceiling (`-M`) is the
limit less an eighth of it (at least 32 MiB) for memory outside the heap. Écluse also takes off one
allocation area for the growth between collections. The nursery sits inside that ceiling. An
allocation area you set through `GHCRTS` stands, unless it equals the shipped `-A64m`. The binary
also ships `-T` for the memory sampler, and `--disable-delayed-os-memory-return` so memory the heap
gives back leaves the cgroup's count at once.

These examples meet the [1 GiB minimum](@/docs/operations.md#memory-plan-and-runtime-sizing).
They show the arithmetic, not a workload guarantee:

| Pod resources | Allocation area | Nursery | Heap ceiling | Live target |
|---|---|---|---|---|
| 2 CPU / 1 GiB | 64 MiB | 128 MiB | 832 MiB | 176 MiB |
| 4 CPU / 1 GiB | 32 MiB | 128 MiB | 864 MiB | 184 MiB |
| 4 CPU / 2 GiB | 64 MiB | 256 MiB | 1,728 MiB | 368 MiB |
| 8 CPU / 4 GiB | 64 MiB | 512 MiB | 3,520 MiB | 752 MiB |

The live target is a quarter of the heap ceiling less the nursery. The memory budget starts there,
less the cache budget and the idle process, so read the effective allocation area, the budget and
the admission controls from the boot log after each change. Compare cold reads, retained selected
reads and listings under the intended concurrency.
Pilot runs a different workload, so measure its process memory and allocation area separately.
