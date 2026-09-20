+++
title = "Operating Écluse"
description = "What a running instance tells your orchestrator and your log collector, how it drains and exits, and how to size the pod underneath it."
weight = 5
+++

Deployment ends with a running instance, and this page is about living with one. Come here when
you wire probes into an orchestrator, point a collector at the logs, size a pod, or have to pull
a bad version back out of the mirror.

`ecluse check-config` validates the configuration without starting a role.
For mirror stores with a control plane, it reports that boot builds the maintenance client
against the live environment without attempting that build itself.

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
each mount's own rules. A mount whose rules include `DenyIfCve` or `DenyIfEpss` waits for its first
advisory sync, a one-way flip that never flaps back. A mount with no such rule is ready before any
artifact exists, because every rule it holds decides without the database. Give a cold pod room for
that first database download: a Kubernetes `startupProbe`, or a readiness `failureThreshold` sized
for it. Pilot publishes an artifact for every ecosystem the configuration mounts. If acquisition
fails, the process stays alive and keeps polling. Once the boot retry budget is spent, the sync
logs an `ERROR` naming Pilot and the store, and repeats it every 15 minutes until an artifact
loads. Alarm on that line: it is the one that says a rollout is stuck on Pilot.

Readiness reports every mount separately, so one ecosystem's missing artifact does not take the
others out of rotation. The `/readyz` body carries a `mounts` object keyed by ecosystem, and each
value is `ready` or `awaiting the advisory database that ecluse pilot publishes`. A missing PyPI
database therefore leaves a healthy npm mount routable, and the body names PyPI as the mount still
waiting. A latched Dredger reports
`halted` instead, and a draining instance reports `draining`. Readiness does not itself block direct
requests: a request that needs advisory data its mount does not have is refused by that mount's own
`onUnavailable` policy, and a rule that admits without reading the database still admits. A
successful first sync does not prove that data remains fresh. [Advisory push age](#advisory-push-age)
governs retained evidence.

An EPSS-dependent ecosystem rejects artifacts without the exact `epss_status=available` marker.
Without an accepted qualified generation, its slot stays empty, and its readiness keeps awaiting the advisory database.
Other ecosystems can accept marker-free artifacts and become routable independently.
A running consumer keeps its accepted qualified generation after a rejected replacement.
Restarting creates empty slots, even when canonical files remain on disk.
Publish marked artifacts before you enable an EPSS-dependent rule, following
[the onboarding order](@/docs/configuration.md#onboarding-the-advisory-denies).

The npm liveness probe `GET /npm/-/ping` answers locally with `200 {}`. `GET /npm/-/v1/search`
returns `501` by design, because search is a discovery convenience, not an install path.
`GET /npm/-/package/{package}/dist-tags` and the `PUT` and `DELETE` of
`/npm/-/package/{package}/dist-tags/{tag}` also return `501`, because Écluse implements no mutable
named pointer. A package's tags are in the metadata document Écluse serves, and the publication
target owns setting and removing them. Mirror, Pilot, and Dredger expose their health probes on
`ECLUSE_SERVER__PORT`. A separate metrics listener appears only when telemetry is on and
`OTEL_METRICS_EXPORTER=prometheus`. Co-located Prometheus listeners need distinct ports
([Telemetry](@/docs/operations.md#telemetry-opt-in)).

## Graceful shutdown and pod drain

On `SIGTERM`/`SIGINT` Écluse drains in-flight work rather than dropping it. `GET /readyz` flips
to `503`, which is the signal a load balancer or mesh watches to stop routing new traffic here,
while `GET /livez` stays `200`, so an orchestrator does not kill a still-draining instance early.
Every response then carries `Connection: close`, and a keep-alive pool reconnects to a ready
instance. In-flight requests and in-progress artifact streams finish before the process exits, so
a half-delivered tarball runs to completion.

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

Typed bearer-token fields render as redacted placeholders. Pilot stores each source's
`host:port` in artifact provenance, and beside it the source URL with its userinfo, query, and
fragment removed, so neither carries a credential. Sync logs only parsed compilation time and row
count from metadata, including when it reads older artifacts with complete source URLs. On each
swap it adds one `info` line naming where the serving artifact came from: the object's publication
time, the OSV source as `host:port`, its newest advisory date, and the EPSS score date. A value the
artifact never recorded reads as `<unrecorded>`. Malformed or oversized display values appear as
absent without changing artifact acceptance.

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

Use the severity together with the event and its repetition:

| Status | What it means | What to do with it |
|---|---|---|
| `error` | A failed operation, exhausted budget, or halted role needs attention. Some conditions can recover on retry. | Page and check the affected role. |
| `warn` | Écluse absorbed a problem and carried on degraded. | Chart it, and alert on a sustained rate rather than on a line. |
| `info` | What the run did: a completed sweep cycle, a version deleted, a mirrored artifact, a served package. | Index it, and read it back during an incident. |
| `debug` | Per-request and per-entry detail. Verbose under load, and off by default. | Turn it on while you investigate. |

Typical `warn` lines record:

- An upstream Écluse could not reach.
- A mirror job left to redeliver.
- A store call Écluse is retrying.
- A malformed advisory entry Écluse dropped, or an advisory date it had to ignore.
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
| `advisory source outage continues` | `error` | At most every 15 minutes while any rule still cannot, listing every such rule and its latest cause. |
| `advisory source outage recovered` | `info` | Once every rule consults the database again. |

The causes are no database loaded, a push past its maximum age, a lookup fault with its detail, and
an open circuit breaker. The report covers the rules that deny on advisories, `DenyIfCve` and
`DenyIfEpss`, which are also the rules that make the store mandatory at boot: a mount whose only
advisory rule is `AllowIfRemediatesCve` abstains without a database and reports no outage. A
request never adds a line, so an outage costs the log the same whatever the traffic. The report
fires only on traffic that reaches an advisory rule, so an idle mount reports nothing, and the sync
task's own `error` line covers a database that never loads. The recovery line follows the next
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
4096 identities per ecosystem and forgets the oldest past that. A trusted read never logs it. The
evidence lives in that decision and in the log only, never in registry metadata, so its retention
is the log's.

A one-shot `ecluse pilot compile` using the same Prometheus port as a live Pilot can log a bind
failure and still complete its compilation. This applies only when the Prometheus exporter is selected
([Telemetry](@/docs/operations.md#telemetry-opt-in)). Any other failure to bind that listener wants
a look.

## Advisory quiet time

Pilot reads how old its sources say their data is, and tells you when one stops changing. After
each compile it logs the age of the ecosystem's newest advisory record and of the EPSS feed's
declared score date, at `info`. When either age passes its threshold, the same line repeats at
`error` and names the ecosystem, the credential-free source URL, the age in seconds, and the
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

`DenyIfCve` and `DenyIfEpss` decide on evidence, and evidence goes out of date. Past a maximum
age, both refuse instead of deciding: the request gets a retryable `503`, and the denial message
says how old the push is and what the maximum was. `onUnavailable: skip` does not waive it, and an
open circuit breaker does not skip past it. `AllowIfRemediatesCve` abstains on the same evidence,
so the quarantine governs rather than the fast lane.

The age is the time since Pilot last pushed the artifact, taken from the published object's own
timestamp. Pilot writes that object after every successful run, so unchanged bytes still move it.
A running consumer observes a newer timestamp for the accepted artifact even when its ETag stays
unchanged. This updates publication age without resetting installation age. A restart re-reads the
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
| Pilot has stopped pushing. | Get it running again. Its own logs say whether the fetch, the compile, or the upload failed. |
| The maximum is shorter than your update cadence. | Set `advisories.maxAgeSeconds` to a value you can meet, and accept the older evidence that comes with it. |

Readiness does not change. `GET /readyz` still reports the mount as ready, because the database is
loaded and every rule that does not read it still decides. Only the CVE-deny path refuses. The
Dredger stops deleting on an advisory match for the same reason, while an identity deny still
acts under its usual guards.

## Telemetry (opt-in)

Metadata reads record HTTP refusals on `ecluse.upstream.fetch.errors` with cause
`upstream_status`, including `404`. A malformed successful response records `decode` instead.
Explicit `401` and `403` refusals retain `other` and their existing warning log.
Metadata `408`, `429`, and server errors log at `ERROR`. A genuine `404` absence and other
non-success statuses log at `WARN`. These logs carry the status and upstream authority,
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
| `ecluse.metadata_cache.refused` | Capacity refusals or external backend failures, by `store=full|version|assembled` |

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
Selected-version and assembled stores share one byte bound and one entry bound, with no reserved shares. A full listing followed by
a selected-version read can therefore fetch upstream twice. No external cache service is required.
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

Local expiry removal checks both stores on access or a retaining insert, without a background timer.
If both stores are idle, their gauges can retain expired charges until the next operation.

### Credential expiry

`ecluse.credential.token.ttl.seconds` reports the shortest remaining lifetime among observed
expiring credentials with the same `provider` label. Écluse measures whole seconds at each
collection and clamps expired credentials to zero. A successful refresh replaces the observed
expiry. A completed failed refresh reports the cached token's expiry.

Five consecutive mint failures open the breaker for 60 seconds. Requests during that cooldown
cannot mint, and refusals do not count as completed refresh attempts. Collection still measures
the cached token's lifetime, including when it expires during the cooldown.

Refresh is demand-driven. An idle credential can expire without a provider fault, so zero TTL
alone is not an outage signal. `ecluse.credential.refresh` counts completed attempts by `provider`
and `result`. Read those outcomes alongside credential demand and failures of mirror writes.

The bounded `provider` label remains `registry`, `codeArtifact`, or `verdaccio`. Shared
CodeArtifact credentials contribute once, even when several ecosystems use them. Static providers
emit no TTL observations. The eager construction mint emits neither a refresh event nor an
expiry observation.

## Memory plan and runtime sizing

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
| Materialisation admission | How much estimated transient metadata work runs concurrently |
| Cache budget | How much eligible metadata the selected provider retains locally |
| Runtime heap ceiling | The heap limit applied to the process |

The default data ceilings leave growth room for large real-world metadata while keeping input work bounded.
They express policy headroom, not a measured maximum that fits every pod. A larger ingest ceiling
does not automatically enlarge the cache or reduce CPU concurrency.
Materialisation admission uses static allowances for cold selected reads, captured local selected
results, full origins and listing output. A local retained result receives the smaller allowance
only while that request holds the result. Deferred fetches and external-provider reads receive the
cold allowance. A listing charges each permitted configured origin plus its output before fetching,
even when it later finds an assembled hit or returns `304`. An estimate above the materialisation
capacity charges that entire capacity, so one request can still run alone. This does not prove
that its actual memory use fits.

Either admission gate can return `503` with `Retry-After: 1` when its waiting room fills or its wait
expires. Treat these responses as backpressure and review concurrency alongside process memory.

These allowances estimate slightly-worse-than-average work. They are not a worst-case heap bound.
They use no package-size history or expiring estimates. Streaming skips unsupported fields, but
supported fields and useful listing results still occupy memory. Keep process headroom and edge
rate limits, then measure your package mix under concurrent traffic.

The memory plan still accounts for runtime reserve, enqueue buffer, cache retention, materialisation,
publish bodies, in-memory queue and mirror-artifact work. Their accounted sum does not measure
all live process allocations. Small automatic plans shed mirror-artifact capacity before cache
retention. Read each warning for the resulting loss of capacity. An explicit override can still
fail plan validation. The ingest ceiling is independent of those tenant allocations.

Cores and the heap ceiling resolve at boot from config, else the cgroup, else a capped fallback.
The log records each decision and its source. The
[appendix](@/docs/operations.md#appendix-runtime-sizing-arithmetic) explains the resource arithmetic.

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
| An explicit `runtime.serveMaxInFlight` pin | The declared positive concurrency still wins | Review it against the separate materialisation capacity and concurrent workload |
| No explicit response or CPU pin | The new automatic controls apply | Compare boot output with `check-config` under the deployment's actual resources |

Response and CPU pins are not silently clamped. Read the override warnings before rollout.
The `memory plan: metadata ingest ceiling` line names the effective body limit.
The `memory plan: material estimate budget` and `runtime: serve admission` lines name the two
admission controls. The `metadata admission estimates` line reports each static workload allowance.
A smaller material estimate budget does not reduce the body ceiling or CPU pin.
Raising the ingest ceiling admits more input, not more memory. A formerly refused package can now
reach parsing, policy and assembly work, so repeat your install and latency checks without widening
performance budgets to hide a regression.

## Revoking a mirrored version (internal yank)

An upstream yank does not revoke a trusted private copy. A new deny stops public admission and
worker re-admission when it wins under the configured precedence, but private hits continue until
the serving copies are removed. Deletion can lose the only remaining bytes, so identify the exact
version and stores before acting.

1. Add the identity denial to the intended policy and roll it to the proxy, mirror worker, and
   Dredger. Verify that it wins over any deliberate allow. Older workers and in-flight transfers
   can still publish during the rollout.
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
mirrored version. If it exposes a winning named deny, normal eligible removal applies. Installed
or client-cached bytes remain outside registry revocation.

### Policy rollout order

Use the same intended configuration across roles. When tightening policy, update admission and
writer roles before Dredger. When relaxing a deny, update Dredger before writers can rely on the
new permission. These are ordering recommendations, not an atomic cutover requirement.

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
admits it again. The [threat model](@/docs/threat-model.md) records that accepted residual.

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

## Appendix: runtime-sizing arithmetic

**Give Écluse whole cores.** A fractional CPU limit, say 3.5, has no good option: claiming 4
capabilities overruns the CFS quota during stop-the-world GC and freezes the process mid-pause,
while flooring to 3 strands the fraction. So pair an integer limit with `requests = limits` (and
exclusive cores where offered) to remove throttling structurally, since Écluse floors the derived
count.

**A pod with no CPU limit is the case to configure.** A CPU **limit** is a cgroup quota Écluse
reads, and it does not shrink the processor count the runtime sees. A CPU **request** is not a
quota. It reaches the container only as a scheduler weight, and the same weight has meant
requests up to 3.4x apart across runc versions, so Écluse will not guess a core count from it.
With no limit set, Écluse falls back to the count the memory limit can feed, and with no memory
limit either it caps at `ECLUSE_RUNTIME__CORES_CEILING` (8). Neither number is your request, and
the boot log warns and says so. On a 32-core node a 2-CPU-request pod with no memory limit
therefore claims 8 capabilities, not 2. Tell it the number with the Downward API:

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

**Size a proxy pod from measured process usage as well as the RTS numbers.** The binary ships
`-A64m -n4m`, a 64 MiB per-core allocation area in 4 MiB chunks. Budget the nursery, live heap,
copying space during major collection and allocations outside the managed heap.
The heap ceiling alone does not describe the container's peak memory.

These examples show nursery arithmetic, not a minimum supported pod size or a workload guarantee:

| Pod resources | Allocation area | Nursery arithmetic | What remains to verify |
|---|---|---|---|
| 2 CPU / 512 MiB | Default `-A64m` | 128 MiB | Effective controls, peak process memory and concurrent listings |
| 2 CPU / 256 MiB | `GHCRTS="-A16m"` | 32 MiB | Effective controls, reduced throughput and any degradation warnings |
| 4 CPU / 750 MiB | Default `-A64m` | 256 MiB | Effective controls and collection headroom under the package mix |
| 4 CPU / 512 MiB | `GHCRTS="-A32m"` | 128 MiB | Effective controls, collection frequency and peak process memory |

Read the effective allocation area and admission controls from the boot log after each change.
Compare cold reads, retained selected reads and listings under the intended concurrency.
Pilot runs a different workload, so measure its process memory and allocation area separately.
