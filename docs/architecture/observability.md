# Observability

> Part of the [Écluse architecture overview](../architecture.md).

Écluse sits in the install path of someone else's build, so an operator must see why it is slow or
refusing a package without attaching a debugger. The substrate is OpenTelemetry, emitting OTLP, a
vendor-neutral wire protocol: one set of instrumentation feeds any compatible backend (a Collector,
Jaeger, Honeycomb, Grafana Tempo, a Prometheus scrape), so the vendor choice collapses to an
endpoint. Telemetry is opt-in and off by default: with `ECLUSE_OBSERVABILITY__TELEMETRY` unset
nothing is wired, no spans open, and the instruments sit on a no-op meter, so an emit is a discarded
measurement rather than a branch.

Datadog is a first-class, tested target, what the maintainer runs, but never required and not a
lock-in: nothing in the core depends on it, and switching backends is a config change. Its
Datadog-specific pieces (the `dd` trace-correlation object on a log line, Agent-side sampling, the
[Operator recipe](../../USAGE.md#datadog-on-kubernetes)) are optional add-ons on the OTLP baseline.

## What gets traced

The instrumentation maps onto the [request lifecycle](../architecture.md#request-lifecycle): a WAI
server span per request, with a child span for each upstream fetch (private then public) carrying
W3C TraceContext to the next hop. Metrics ride the same OTLP pipeline. Hand-added domain spans carry
the decisions operators care about:

- **Rule evaluation**: the verdict, and on denial the `RuleName` and `RejectReason` (the
  [error model](web-layer.md#error-model)), so a 403 is explainable from the trace alone.
- **Mirror enqueue to worker**: the serve-time enqueue and the worker's probe-to-publish run under
  linked spans, so a worker poll mixing jobs from many requests links each back to its own triggering
  request. A job enqueued with tracing off bears no link.
- **Advisory sync**: one span per [advisory-dataset sync](rules-engine.md#cve-subsystem) run.

Sampling is head-based and always-on by default, so rare denial and error traces are never dropped;
`OTEL_TRACES_SAMPLER` / `OTEL_TRACES_SAMPLER_ARG` set a parent-based ratio without a code change, and
against Datadog the node-local Agent resamples so always-on is not wasteful. Tail sampling needs a
collector and is planned.

## Metrics

Écluse emits only what it uniquely knows: queue backlog and DLQ depth are cloud-native metrics
(CloudWatch, Cloud Monitoring), not re-emitted here. Names follow OTel HTTP conventions
(`http.server.*`) plus an `ecluse.*` namespace for domain signals. The alarm-worthy signals:

- `ecluse.serve.perimeter.faults` (gate/render/unclassified) and `ecluse.serve.relay.anomalies`
  (odd_shape/non_success) are steady-state zero, so any movement is an invariant break: a pre-commit
  handler escape answered with the neutral 500, or a public relay that was not the admitted artifact.
  The fault channels are mapped in the [fault model](fault-model.md).
- `ecluse.registry.merge.divergence` is the cross-upstream integrity alarm, incremented per
  contradicting version (package and version on the paired `WARNING` line, not a label); see the
  [threat model](https://ecluse-proxy.com/threat-model.html).
- `ecluse.credential.token.ttl.seconds` alarms a stuck refresh; `ecluse.credential.refresh` carries
  (result, provider).

The remaining serving, gate, upstream, cache, publish-budget, and mirror signals populate dashboards;
all export over the same OTLP push pipeline as traces. A Prometheus scrape endpoint
(`OTEL_METRICS_EXPORTER=prometheus`) is deferred: the SDK honours the selection, but no scrape
renderer ships yet.

### Cardinality and attributes

An inline proxy sees thousands of distinct packages, so the failure mode is a metric-series
explosion. Two guarantees hold it and the telemetry safe:

- **High-cardinality identifiers stay on spans and logs, never metric labels.** `package`,
  `version`, `scope`, and the full denial message go on the rule-eval span and the log line; metric
  labels are bounded enums, so such an identifier cannot become a series. `rule` is the one
  operator-bounded label (a small fixed set per deployment).
- **Secrets and PII never appear in any signal**: no tokens, no `Authorization`. A forwarded client
  token is scrubbed from anything the WAI or http-client instrumentation captures. See
  [security](security.md).

## Logs

Logs stay structured JSON via `katip`, stitched to traces by trace-ID injection. The production
format is one compact JSON object per line to stdout (JSONL), which the Datadog Agent's stdout
autodiscovery consumes. Set the shape with `ECLUSE_OBSERVABILITY__LOG_FORMAT`: `json` (the
in-container default) or `console` (human-readable, for development).

Every JSON line carries the reserved attribute names a log backend reads:

```json
{"timestamp":"2026-06-22T09:14:03.118Z","status":"warn","message":"denied","service":"ecluse","env":"prod","version":"0.1.0","dd":{"trace_id":"…","span_id":"…"},"data":{"module":"Ecluse.Server.Pipeline.Internal","package":"@evil/pkg","version":"1.0.0","rule":"DenyInstallTimeExecution"},"katip":{"ns":["ecluse","serve"],"app":["ecluse"],"host":"…","pid":"1","thread":"…","loc":null}}
```

`timestamp`, `status`, `message`, and `service` are Datadog's reserved log attributes, read
unmodified by its JSON preprocessing; `env` and `version` are ordinary attributes any backend
indexes, and on Datadog the matching unified-service tags normally come from `DD_ENV` and
`DD_VERSION` on the Agent rather than from the line. `status` folds `katip`'s eight syslog
severities onto the four an operator acts on: `debug`, `info`, `warn`, `error`.

`service`, `env`, and `version` come from the same resolved identity as the traces, so a log-to-trace
pivot lines up; the formatter stamps that identity, so a line raised outside any request scope
carries it too. With no `DD_ENV` or `deployment.environment` set, `env` falls back to the deployment
label the process boots under, and `version` to the binary's own build version. The `dd` object
appears only while a span is in scope, and Datadog needs those ids as low-64-bit decimal for
OTLP-ingested traces to match. The emitting call's own fields sit under `data`, the `katip` emitter
fields under `katip`; the emitting process's hostname is `katip.host`, so a collector's own
host attribution (the Datadog Agent supplies it) governs the line's `host`.

`ECLUSE_OBSERVABILITY__LOG_LEVEL` sets the severity floor: `debug`, `info` (the default), `warn`, or
`error`. The scribe drops anything below the floor before rendering it, so `debug` instrumentation
costs nothing at `info`. An unrecognised value is a boot error, like every other configuration enum.

### URL minimisation

An artifact location is upstream-supplied, and an advisory export URL and a configured upstream base
URL are operator-supplied; any of them can carry a credential in its userinfo or in a pre-signed
query string, and both logs and spans leave the node. So on every running path a URL is reduced to
its validated `host:port` before it names anything, through the one shared reduction in
`Ecluse.Core.Security.Authority`; a value with no dialable authority renders as `<unresolved>`,
never as a fragment of the input. The paths this covers:

- **Serve.** The packument origin and upstream fields on the degrade warnings, the URL a
  url-formation fault carries, and the artifact URL a dropped-entry record holds.
- **Mirror enqueue and worker fetch.** The `ecluse.mirror.artifact_host` span attribute, the
  worker's tarball-host drop reason and artifact-fetch line, and a failed fetch's reason, which
  names the authority and the bounded transport cause rather than the client's rendered exception.
- **Advisory sync and export.** The `ecluse.osv.source_host` span attribute on the compile and
  stream spans, and the stream's start line.

The span attribute names say what they hold: `ecluse.mirror.artifact_host` and
`ecluse.osv.source_host`.

The **boot-time configuration echo** is the exception, by design: the resolved-key provenance dump,
the endpoint-collision warnings, and the mount posture lines print each configured upstream and
mirror URL as the operator gave it, so the effective posture reads straight from the start-up log.
Secret-typed keys are redacted there, and a credential belongs in one of those rather than inside a
URL (see [Secrets](../../USAGE.md#secrets)).

## Configuration and deployment

Telemetry is off until `ECLUSE_OBSERVABILITY__TELEMETRY` is set; the operator surface (the `OTEL_*`
and `DD_*` [variables](../../USAGE.md#observability-observability) and the
[Datadog recipe](../../USAGE.md#datadog-on-kubernetes)) lives in the operator manual. The design
facts here:

- **No agentless export.** `DD_API_KEY` and `DD_SITE` are never read: Écluse exports to a node-local
  Collector or Agent, never a vendor's cloud, so telemetry data leaves your network only if you point
  the collector outward. The OTLP endpoint is an operator-declared destination, so it is deliberately
  not SSRF-classified (that classifier guards the untrusted package-download path). Authenticate a
  remote collector out of band via `OTEL_EXPORTER_OTLP_HEADERS`.
- **Export never touches the request path.** The batch exporter runs asynchronously, so an
  unreachable collector never blocks a request. A failed OTLP export is not silent: Écluse logs it
  through `katip` under a throttle, so a broken collector surfaces without flooding logs.
- **Threaded RTS required.** Telemetry needs the threaded runtime the image runs: the OTel SDK's
  batch span processor aborts under the non-threaded runtime. Core and heap sizing are in the
  [runtime-sizing appendix](../../USAGE.md#appendix-runtime-sizing-arithmetic).

Telemetry is verified against a real Agent or Collector, not merely compiled: a Dockerised,
Datadog-free [integration tier](../testing.md) asserts the spans and metrics arrive.
