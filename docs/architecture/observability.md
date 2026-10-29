# Observability

> Part of the [Écluse architecture overview](../architecture.md).

What Écluse emits, why each signal exists, and how an operator points it at a backend.

Écluse sits in the install path of someone else's build. An operator must see why it is slow, or
why it refuses a package, without attaching a debugger. The substrate is OpenTelemetry over OTLP, a
vendor-neutral wire protocol. One set of instrumentation feeds any compatible backend: a Collector,
Jaeger, Honeycomb, Grafana Tempo, or a Prometheus scrape. The vendor choice collapses to an
endpoint.

Telemetry is opt-in and off by default. With `ECLUSE_OBSERVABILITY__TELEMETRY` unset, Écluse wires
nothing, opens no spans, and sits the instruments on a no-op meter. An emit then becomes a discarded
measurement rather than a branch.

Datadog is an optional tested target. The core uses the OTLP interfaces.
Logs include Datadog-compatible trace IDs while a span is active, regardless of the export backend.
A Datadog Agent can apply further sampling.

## What gets traced

The instrumentation maps onto the [request path](https://ecluse-proxy.com/docs/how-it-works/#a-request-step-by-step). Each
request opens a WAI server span with a child span for each upstream fetch. Ordinary metadata
requests fetch private and public sources concurrently. First-party metadata uses only the
private source. Artifact requests prefer the private source before a public fallback.
A child span carries W3C TraceContext to the next hop. Metrics ride the same OTLP pipeline.
A child span covers one exchange, from the request to the response headers, and the
`http.client.*` metrics count the same exchanges. `http.client.active_requests` counts the
exchanges still waiting for their headers. The Dredger's reads of a store open no child span.
Hand-added domain spans carry the decisions operators care about:

- **Rule evaluation**: the verdict, and on denial the `RuleName` and `RejectReason` (the
  [error model](web-layer.md#error-model)). The trace alone explains a 403.
- **Mirror enqueue to worker**: the serve-time enqueue and the worker's probe-to-publish run under
  linked spans. A worker poll mixing jobs from many requests links each back to its own triggering
  request. A job enqueued with tracing off bears no link.
- **Advisory sync**: one `ecluse.advisory.sync.attempt` span per
  [advisory-dataset sync](rules-engine.md#cve-subsystem) attempt, carrying the ecosystem and which of
  the five outcomes the attempt reached. A failed fetch also sets the span's error status to its
  typed fault. The object key, ETag, and provenance stay off it. That same bounded result labels the
  attempt metrics below, so a trace and a series join on one value.

When an HTTP client or AWS SDK exception escapes a span, the span records its typed fault as the
error status. An upstream fetch's client span records the client's reason instead. Neither records
the request the exception carries, because its headers, and the query of a signed AWS request, can
hold a credential. An upstream fetch's client span does record the URL it fetched, path and query
included, and never a request header.

Sampling is head-based and always-on by default, so Écluse never drops a rare denial or error
trace. `OTEL_TRACES_SAMPLER` and `OTEL_TRACES_SAMPLER_ARG` set a parent-based ratio without a code
change. A Datadog Agent can resample exported traces. Sampling still affects collection and
export cost, so choose it for the deployment's traffic and diagnostic needs.

## Metrics

Écluse emits only what it uniquely knows. Queue backlog and DLQ depth are cloud-native metrics
(CloudWatch, Cloud Monitoring), so Écluse does not re-emit them. Names follow OTel HTTP conventions
(`http.server.*`) plus an `ecluse.*` namespace for domain signals. The alarm-worthy signals:

- `ecluse.upstream.requests` counts public HTTP exchange attempts by `server.address`, `kind`, and `status`.
  The host comes from the formed request, without its port, path, query, or credentials.
  Kinds are `listing`, `selected_read`, `artifact`, `companion`, `worker_metadata`, and `worker_artifact`.
  Status is the exact HTTP code, `transport_failure`, or `cancelled`.
  The proxy and mirror workers record only public reads.
  Private reads, target writes, cache hits, followers, and local cooldown refusals do not count.
  The observer records headers before body consumption, so later body failures retain that status without another count.
  Internal retries record each notified attempt. Connection failure before notification records one transport failure.
  Notification precedes header writes and does not prove remote receipt.
  A cancellation before notification records no attempt.
- `ecluse.serve.perimeter.faults` (render/unclassified) and `ecluse.serve.relay.anomalies`
  (odd_shape/non_success) are steady-state zero. Any movement is an invariant break: a pre-commit
  handler escape answered with the neutral 500, or a public relay that was not the admitted
  artifact.
- `ecluse.registry.merge.divergence` is the cross-upstream integrity alarm. It increments per
  contradicting version, and the package and version go on the paired `WARNING` line, never on a
  label. See the [threat model](https://ecluse-proxy.com/docs/threat-model/).
- `ecluse.credential.token.ttl.seconds` measures remaining lifetime at collection, including while
  the mint breaker refuses new attempts. The minimum per provider exposes the earliest expiry
  among its configured credentials. Refresh is demand-driven, so idle expiry alone does not
  establish a provider fault. See the [operator policy](https://ecluse-proxy.com/docs/operations/#credential-expiry).
  `ecluse.credential.refresh` counts completed attempts by result and provider. The bounded
  provider labels match the configured store tags: `registry`, `codeArtifact`, and `verdaccio`.
- `ecluse.mirror.jobs.processed` carries (result), one of `published`, `failed`, or `discarded`.
  `discarded` is worth an alarm on its own. It means the worker retired a mirror job that the
  queue redelivered past its budget. That happens only when no dead-letter queue captured the job
  first (see [cloud backends](cloud-backends.md#the-terminus-for-a-job-that-can-never-succeed)).
  The job and the reason stay on the paired `ERROR` line, never a label.
- `ecluse.dredger.versions` (a counter) carries (target, result). The target is `mirrorTarget` or
  `privateUpstream`, and the result is one of `examined`, `deleted`,
  `would_delete`, `kept`, or `guard_skipped`. Every version a sweep cycle examines counts once as
  `examined` and once more under what the cycle did with it, so deletions read as a fraction of what
  was seen. A dry run counts under `would_delete` and never `deleted`, so a preview cannot be
  mistaken for a deletion. A cycle that decided with no advisory generation loaded, whatever its
  shape, writes the missing-generation `ERROR` line and closes its counts with the partial-evidence
  suffix, so a count taken across that gap is never read as a whole-store count. A cycle whose `deleted` count jumps is worth an alarm: the deletion cap
  halts such a cycle for the life of the process, and the halt's `ERROR` line names the cap and the
  advisory generation. The package, the version, and the denying rule stay on the sweep's audit
  line, never a label.
- `ecluse.advisory.sync.attempts` (a counter) and `ecluse.advisory.sync.duration` (a histogram, in
  seconds) both carry (ecosystem, result), where result is one of `swapped`, `unchanged`,
  `none_published`, `fetch_failed`, or `refused`. A run of `fetch_failed` or `refused` means that
  ecosystem gates against an ageing advisory database or none at all. While the last good generation
  is within its maximum advisory age, every rule decides as usual. With no generation loaded, the
  advisory rules follow `onUnavailable`: the shipped fast lane and the MAL default refuse. Past the
  maximum age, the advisory denies refuse, and the fast lane follows `onUnavailable`. The quarantine
  always decides as usual. Check the bucket, the object key, and the IAM the sync task reads under.
  The artifact's own identifiers stay on the sync log line, never a label.
- `ecluse.advisory.database.age.seconds` (a gauge) carries (ecosystem). The proxy, mirror worker,
  and Dredger each observe their own slots. The gauge reads the seconds since
  that ecosystem's serving advisory database was installed. Écluse measures it at each collection,
  from the slot that holds the database, so it climbs on its own whether or not a sync task is
  alive. One threshold therefore alarms on a stale database, on a sync that stopped swapping, and
  on a sync task that is crash-looping. It observes nothing before the first swap, so a slot that
  has never loaded an artifact reports no age rather than one that reads like a fresh database.
- `ecluse.advisory.source.age.seconds` (a gauge) carries (ecosystem). It reads the seconds since
  the serving artifact was published, which is the age the advisory denies expire on. That makes it
  the one to alarm on for data freshness, while `ecluse.advisory.database.age.seconds` above stays
  an installation diagnostic for this process. It observes nothing before the first sync, and
  nothing where the store reported no publication time.
- `ecluse.advisory.compile.accepted` and `ecluse.advisory.compile.dropped` (both counters) carry
  (ecosystem), and the dropped counter adds (cause), one of `oversize` or `malformed`. Pilot records
  both once per compile pass, so a backend computes the drop rate from the pair. The dropped entry's
  own name and bytes stay on the compile log line, never a label.
- `ecluse.advisory.compile.runs` (a counter) carries (ecosystem, result), where result is
  `completed` or `aborted`. An `aborted` run means Pilot judged the feed's drop rate systemic. It
  abandoned the artifact rather than publish one that silently omits advisories. A run of them
  leaves every consumer on its last-good artifact, ageing.

The remaining serving, gate, upstream, cache, publish-budget, and mirror signals populate
dashboards. All of them leave by whichever metrics transport the deployment selected below.

### Cardinality and attributes

An inline proxy sees thousands of distinct packages, so the failure mode is a metric-series
explosion. The instrumentation applies these controls:

- **High-cardinality identifiers stay on spans and logs, never metric labels**. `package`,
  `version`, `scope`, and the full denial message go on the rule-eval span and the log line. Metric
  labels use bounded enums, permitted public hosts, and built-in rule type names.
  Public request hosts can include an allowed ecosystem artifact host. They carry no package path or query.
- **Forwarded authorisation headers are excluded from instrumentation.** Tests cover the
  caller's `Authorization` header. Client spans can still contain URL paths and queries, so
  this is not a guarantee against every secret or personal value. Apply the URL restrictions
  and minimisation described below. See [security](security.md).

## Logs

Logs are structured JSON lines through `katip`, stitched to traces by trace-ID injection. The
operator manual's [Logs section](https://ecluse-proxy.com/docs/operations/#logs) states the line shape, the
reserved Datadog attributes, the level floor, and the redaction.

Metrics carry routine operational signals. Serve requests and serve-time denial details stay at debug.
Mirror publications retain the evaluated admission rationale with the artifact identity.
The shared admission helper returns the original decision so auditing does not re-run the rules.

### URL minimisation

An upstream supplies the artifact location. An operator supplies the advisory export URL. Either
can carry a credential in its userinfo or in a pre-signed query string, and both logs and spans
leave the node. The runtime diagnostics listed below reduce URLs to `host:port` through
`Ecluse.Core.Security.Authority`. A value with
no dialable authority renders as `<unresolved>`, never as a fragment of the input. The paths this
covers:

- **Serve.** The packument origin and upstream fields on the degrade warnings, the URL a
  url-formation fault carries, and the artifact URL a dropped-entry record holds.
- **Mirror enqueue and worker fetch.** The `ecluse.mirror.artifact_host` span attribute, the
  worker's tarball-host drop reason, and its artifact-fetch line. A failed fetch's reason names the
  authority and the bounded transport cause, not the client's rendered exception.
- **Advisory sync and export.** The `ecluse.osv.source_host` span attribute on the compile and
  stream spans, the stream's start line, and both source identities stored in artifact metadata.
  Sync logs parsed compilation time and row count from metadata. The `info` line it logs on each
  swap adds the OSV source as `host:port`, the newest OSV advisory date, and the EPSS score date.
  It excludes arbitrary fields, including complete URLs in older artifacts.

The span attribute names say what they hold: `ecluse.mirror.artifact_host` and
`ecluse.osv.source_host`.

The **boot-time configuration echo** prints a URL whole, by design. The resolved-key provenance
dump, the endpoint-collision warnings, and the mount posture lines print each configured upstream
and mirror URL as the operator gave it. The effective posture then reads straight from the start-up
log. Those lines need no reduction, because a configured registry URL has nothing to reduce. The
loader refuses one carrying userinfo, a query string, or a fragment, and the error names the key.
The credential then sits in a secret-typed key, which the dump redacts
(see [Secrets](https://ecluse-proxy.com/docs/configuration/#secrets)).

## Configuration and deployment

Telemetry is off until an operator sets `ECLUSE_OBSERVABILITY__TELEMETRY`. The operator manual's
[Telemetry section](https://ecluse-proxy.com/docs/operations/#telemetry-opt-in) owns the variables
and the wiring. Six design facts hold regardless:

- **Metrics push or pull, and the pull side gets its own port.** OTLP push is the default and
  shares the traces' pipeline, which suits the Datadog Agent and any Collector. A shop that
  scrapes sets `OTEL_METRICS_EXPORTER=prometheus` instead, and Écluse answers `GET /metrics` on a
  dedicated listener, addressed by `OTEL_EXPORTER_PROMETHEUS_HOST` (default `localhost`) and
  `OTEL_EXPORTER_PROMETHEUS_PORT` (default 9464). The SDK resolves that value to a no-op push
  exporter, so the endpoint is the whole transport. It is never mounted on the proxy's data port,
  because the exposition carries the entire OpenTelemetry resource: host, process, and cloud or
  cluster identity, none of which belongs on the port untrusted clients reach. The bounded labels
  above answer cardinality, not exposure, so the loopback default is what makes reaching the
  exposition off the host a deliberate act. A scrape never enters the request path, so it does not
  appear in the `http.server.*` series it reports. The listener belongs to the telemetry
  lifecycle, not to the front door, so every role opens its own and one scrape covers one role.
- **No agentless export.** Écluse never reads `DD_API_KEY` or `DD_SITE`. It exports to a node-local
  Collector or Agent, never to a vendor's cloud. The OTLP endpoint is an operator-declared
  destination, so it is deliberately not SSRF-classified.
- **Export never touches the request path.** The batch exporter runs asynchronously, and Écluse
  logs a failed export under a throttle.
- **Metric records batch per capability.** The SDK keeps every series in one shared cell. Écluse
  records into one cell per capability by compare-and-swap, and hands the totals to the SDK once a
  second and before every collection, including the reader's final export at shutdown.
  Records remain exact over time. A collection drains the cells separately, so concurrent records
  can reach different collections. An up-down counter can briefly report a negative value when its
  decrement reaches a collection before its increment. The next collection includes the increment.
  Exemplars keep the trace of the request that recorded them.
- **Threaded RTS required.** The OTel SDK's batch span processor aborts under the non-threaded
  runtime, so telemetry needs the threaded runtime the image runs.
- **The resource-attribute header is bounded.** `service.name` travels on `OTEL_SERVICE_NAME`
  alone, because every SDK signal path reads the service name from that variable. The rest is
  admitted against the W3C baggage limits, resolved identity first, so an oversized operator
  configuration cannot cost a key a dashboard joins on. A key the limits exclude warns at boot by
  name, because the SDK's own encoder would otherwise shed it silently and in hash order.

A Dockerised, Datadog-free [integration tier](../testing.md) verifies telemetry against a real
Agent or Collector. It asserts that the spans and metrics arrive.
