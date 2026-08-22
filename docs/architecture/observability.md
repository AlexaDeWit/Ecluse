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

Datadog is a first-class, tested target and what the maintainer runs. It is never required and
never a lock-in: nothing in the core depends on it, and switching backends is a config change. The
Datadog-specific pieces are optional add-ons on the OTLP baseline: the `dd` trace-correlation object
on a log line, Agent-side sampling, and the [Operator recipe](../../USAGE.md#datadog-on-kubernetes).

## What gets traced

The instrumentation maps onto the [request lifecycle](../architecture.md#request-lifecycle). Each
request opens a WAI server span, with a child span for each upstream fetch, private then public.
A child span carries W3C TraceContext to the next hop. Metrics ride the same OTLP pipeline.
Hand-added domain spans carry the decisions operators care about:

- **Rule evaluation**: the verdict, and on denial the `RuleName` and `RejectReason` (the
  [error model](web-layer.md#error-model)). The trace alone explains a 403.
- **Mirror enqueue to worker**: the serve-time enqueue and the worker's probe-to-publish run under
  linked spans. A worker poll mixing jobs from many requests links each back to its own triggering
  request. A job enqueued with tracing off bears no link.
- **Advisory sync**: one `ecluse.advisory.sync.attempt` span per
  [advisory-dataset sync](rules-engine.md#cve-subsystem) attempt, carrying the ecosystem and which of
  the five outcomes the attempt reached. The bucket, object key, ETag, and provenance stay off it.
  That same bounded result labels the attempt metrics below, so a trace and a series join on one
  value.

Sampling is head-based and always-on by default, so Écluse never drops a rare denial or error
trace. `OTEL_TRACES_SAMPLER` and `OTEL_TRACES_SAMPLER_ARG` set a parent-based ratio without a code
change. Against Datadog the node-local Agent resamples, so always-on is not wasteful. Tail sampling
needs a collector and is planned.

## Metrics

Écluse emits only what it uniquely knows. Queue backlog and DLQ depth are cloud-native metrics
(CloudWatch, Cloud Monitoring), so Écluse does not re-emit them. Names follow OTel HTTP conventions
(`http.server.*`) plus an `ecluse.*` namespace for domain signals. The alarm-worthy signals:

- `ecluse.serve.perimeter.faults` (gate/render/unclassified) and `ecluse.serve.relay.anomalies`
  (odd_shape/non_success) are steady-state zero. Any movement is an invariant break: a pre-commit
  handler escape answered with the neutral 500, or a public relay that was not the admitted
  artifact. The [fault model](fault-model.md) maps the fault channels.
- `ecluse.registry.merge.divergence` is the cross-upstream integrity alarm. It increments per
  contradicting version, and the package and version go on the paired `WARNING` line, never on a
  label. See the [threat model](https://ecluse-proxy.com/threat-model.html).
- `ecluse.credential.token.ttl.seconds` alarms a stuck refresh. `ecluse.credential.refresh` carries
  (result, provider).
- `ecluse.mirror.jobs.processed` carries (result), one of `published`, `failed`, or `discarded`.
  `discarded` is worth an alarm on its own. It means the worker retired a mirror job that the
  queue redelivered past its budget. That happens only when no dead-letter queue captured the job
  first (see [cloud backends](cloud-backends.md#the-terminus-for-a-job-that-can-never-succeed)).
  The job and the reason stay on the paired `ERROR` line, never a label.
- `ecluse.advisory.sync.attempts` (a counter) and `ecluse.advisory.sync.duration` (a histogram, in
  seconds) both carry (ecosystem, result), where result is one of `swapped`, `unchanged`,
  `none_published`, `fetch_failed`, or `refused`. A run of `fetch_failed` or `refused` means that
  ecosystem gates against an ageing advisory database or none at all. Its rules then deny by
  default. Check the bucket, the object key, and the IAM the sync task reads under. The artifact's
  own identifiers stay on the sync log line, never a label.

The remaining serving, gate, upstream, cache, publish-budget, and mirror signals populate
dashboards, and all export over the same OTLP push pipeline as traces. A Prometheus scrape
endpoint (`OTEL_METRICS_EXPORTER=prometheus`) is deferred: the SDK honours the selection, but no
scrape renderer ships yet.

### Cardinality and attributes

An inline proxy sees thousands of distinct packages, so the failure mode is a metric-series
explosion. Two guarantees keep it and the telemetry safe:

- **High-cardinality identifiers stay on spans and logs, never metric labels**. `package`,
  `version`, `scope`, and the full denial message go on the rule-eval span and the log line. Metric
  labels are bounded enums, so such an identifier cannot become a series. `rule` is the one
  operator-bounded label, a small fixed set per deployment.
- **Secrets and PII never appear in any signal**: no tokens, no `Authorization`. The proxy scrubs
  a forwarded client token from anything the WAI or http-client instrumentation captures. See
  [security](security.md).

## Logs

Logs stay structured JSON through `katip`, stitched to traces by trace-ID injection. The production
format is one compact JSON object per line to stdout (JSONL), which the Datadog Agent's stdout
autodiscovery consumes. Set the shape with `ECLUSE_OBSERVABILITY__LOG_FORMAT`: `json`, the
in-container default, or `console`, human-readable for development.

Every JSON line carries the reserved attribute names a log backend reads:

```json
{"timestamp":"2026-06-22T09:14:03.118Z","status":"warn","message":"denied","service":"ecluse","env":"prod","version":"0.1.0","dd":{"trace_id":"…","span_id":"…"},"data":{"module":"Ecluse.Server.Pipeline.Internal","package":"@evil/pkg","version":"1.0.0","rule":"DenyInstallTimeExecution"},"katip":{"ns":["ecluse","serve"],"app":["ecluse"],"host":"…","pid":"1","thread":"…","loc":null}}
```

`timestamp`, `status`, `message`, and `service` are Datadog's reserved log attributes. Datadog's
JSON preprocessing reads them unmodified. `env` and `version` are ordinary attributes any backend
indexes. On Datadog the matching unified-service tags normally come from `DD_ENV` and `DD_VERSION`
on the Agent, not from the line. `status` folds `katip`'s eight syslog severities onto the four an
operator acts on: `debug`, `info`, `warn`, `error`.

`service`, `env`, and `version` come from the same resolved identity as the traces, so a log-to-trace
pivot lines up. The formatter stamps that identity, so a line raised outside any request scope
carries it too. With no `DD_ENV` or `deployment.environment` set, `env` falls back to the deployment
label the process boots under. `version` falls back to the binary's own build version.

The `dd` object appears only while a span is in scope. Datadog needs those ids as low-64-bit decimal
for OTLP-ingested traces to match. The emitting call's own fields sit under `data`, and the `katip`
emitter fields under `katip`. The emitting process's hostname is `katip.host`, so a collector's own
host attribution (the Datadog Agent supplies it) governs the line's `host`.

`ECLUSE_OBSERVABILITY__LOG_LEVEL` sets the severity floor: `debug`, `info` (the default), `warn`, or
`error`. The scribe drops anything below the floor before rendering it, so `debug` instrumentation
costs nothing at `info`. An unrecognised value is a boot error, like every other configuration enum.

### URL minimisation

An upstream supplies the artifact location. An operator supplies the advisory export URL. Either
can carry a credential in its userinfo or in a pre-signed query string, and both logs and spans
leave the node. So on every running path Écluse reduces a URL to its validated `host:port` before it
names anything, through the one shared reduction in `Ecluse.Core.Security.Authority`. A value with
no dialable authority renders as `<unresolved>`, never as a fragment of the input. The paths this
covers:

- **Serve.** The packument origin and upstream fields on the degrade warnings, the URL a
  url-formation fault carries, and the artifact URL a dropped-entry record holds.
- **Mirror enqueue and worker fetch.** The `ecluse.mirror.artifact_host` span attribute, the
  worker's tarball-host drop reason, and its artifact-fetch line. A failed fetch's reason names the
  authority and the bounded transport cause, not the client's rendered exception.
- **Advisory sync and export.** The `ecluse.osv.source_host` span attribute on the compile and
  stream spans, and the stream's start line.

The span attribute names say what they hold: `ecluse.mirror.artifact_host` and
`ecluse.osv.source_host`.

The **boot-time configuration echo** prints a URL whole, by design. The resolved-key provenance
dump, the endpoint-collision warnings, and the mount posture lines print each configured upstream
and mirror URL as the operator gave it. The effective posture then reads straight from the start-up
log. Those lines need no reduction, because a configured registry URL has nothing to reduce. The
loader refuses one carrying userinfo, a query string, or a fragment, and the error names the key.
The credential then sits in a secret-typed key, which the dump redacts
(see [Secrets](../../USAGE.md#secrets)).

## Configuration and deployment

Telemetry is off until an operator sets `ECLUSE_OBSERVABILITY__TELEMETRY`. The operator surface
lives in the operator manual: the `OTEL_*` and `DD_*`
[variables](../../USAGE.md#observability-observability) and the
[Datadog recipe](../../USAGE.md#datadog-on-kubernetes). Logs sit outside that switch. They go to
stdout on every run and reach a backend through the collector's container log collection, never
through OTLP export. So telemetry off costs no logs. The design facts here:

- **No agentless export.** Écluse never reads `DD_API_KEY` or `DD_SITE`. It exports to a node-local
  Collector or Agent, never to a vendor's cloud. Telemetry data leaves your network only if you
  point the collector outward. The OTLP endpoint is an operator-declared destination, so it is
  deliberately not SSRF-classified: that classifier guards the untrusted package-download path.
  Authenticate a remote collector out of band with `OTEL_EXPORTER_OTLP_HEADERS`.
- **Export never touches the request path.** The batch exporter runs asynchronously, so an
  unreachable collector never blocks a request. A failed OTLP export is not silent: Écluse logs it
  through `katip` under a throttle, so a broken collector surfaces without flooding logs.
- **Threaded RTS required.** Telemetry needs the threaded runtime the image runs, because the OTel
  SDK's batch span processor aborts under the non-threaded runtime. Core and heap sizing are in the
  [runtime-sizing appendix](../../USAGE.md#appendix-runtime-sizing-arithmetic).

A Dockerised, Datadog-free [integration tier](../testing.md) verifies telemetry against a real
Agent or Collector, not by compiling it alone. It asserts that the spans and metrics arrive.
