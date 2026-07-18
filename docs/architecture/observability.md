# Observability

> Part of the [Écluse architecture overview](../architecture.md).

Écluse sits in the install path of someone else's build, so an operator has to see why it is
slow or refusing a package without attaching a debugger. Observability is opt-in and
vendor-neutral: the substrate is OpenTelemetry, emitting OTLP, which any compatible backend
receives (a Collector, Jaeger, Honeycomb, Grafana Tempo, a Prometheus scrape). Telemetry is
off by default.

Datadog is a first-class target, what the maintainer runs, so it gets a documented, tested
path. It is never required: nothing in the core depends on it, and switching backends is a
config change. The Datadog-specific pieces (the `dd.*` log fields, Agent-side sampling, the
Operator recipe in [USAGE](../../USAGE.md#datadog-on-kubernetes)) are optional add-ons
on the OTLP baseline.

## OpenTelemetry as the substrate

OTLP is a vendor-neutral wire protocol, so one set of instrumentation feeds any backend and
the vendor choice collapses to an endpoint. It is also the only realistic route for Haskell,
which has no first-party Datadog tracing library. Écluse builds on
[`hs-opentelemetry`](https://github.com/iand675/hs-opentelemetry) `^>=1.0`, which ships
metrics alongside tracing, so metrics ride the same OTLP pipeline. Dependency pins live in
[Technology stack](technology-stack.md#technology-stack).

With `ECLUSE_OBSERVABILITY__TELEMETRY` unset, nothing is wired: the SDK is not initialised,
no spans open, and the instruments are built on the no-op meter, so an emit is a discarded
measurement, not a branch.

The wired packages and their roles:

| Package | Role |
|---|---|
| `hs-opentelemetry-sdk` | Tracer and meter provider, batching, lifecycle. |
| `hs-opentelemetry-exporter-otlp` | OTLP export for traces and metrics (HTTP/protobuf). |
| `hs-opentelemetry-instrumentation-wai` | One server span per request, into the raw-WAI [middleware stack](web-layer.md#middleware-and-helper-libraries). |
| `hs-opentelemetry-instrumentation-http-client` | Child spans and context propagation on the [data plane](web-layer.md#control-plane-vs-data-plane), the upstream fetches that are the proxy's work. |
| `hs-opentelemetry-propagator-w3c` | W3C TraceContext propagation across the outbound hops. |

The substrate lives in `Ecluse.Runtime.Telemetry` (self-aligning config, with `.Resolve`),
`.Tracing` (request-lifecycle tracing), `.Instruments` (the runtime instruments), and
`.Correlation` (the logs-to-traces `dd` glue over `Ecluse.Runtime.Log`). The metric
catalogue is `Ecluse.Core.Telemetry.Metrics`.

## What gets traced

The instrumentation maps onto the [request lifecycle](../architecture.md#request-lifecycle):
a WAI server span, with child spans for each upstream fetch (private then public) from the
http-client instrumentation. Domain spans are added by hand because they carry the decisions
an operator cares about:

- **Rule evaluation**: the verdict, and on denial the `RuleName` and `RejectReason` (the
  [error model](web-layer.md#error-model)), so a 403 is explainable from the trace alone.
- **Mirror enqueue**: a producer span over the serve-time hand-off. It writes its W3C trace
  context onto the `MirrorJob` so the worker links back, and records a swallowed best-effort
  enqueue failure on its status.
- **Mirror worker job**: the async probe-to-publish consumer span, carrying a span link
  re-established from the job's W3C context (a true cross-async link, not a `package@version`
  correlation), so one poll mixing jobs from many requests links each to its own producer. A
  job enqueued with tracing off bears no link.
- **Advisory sync**: one span per [advisory-dataset sync](rules-engine.md#cve-subsystem) run.

Sampling is head-based. The SDK is always-on by default, so rare denial and error traces are
never missed; `OTEL_TRACES_SAMPLER` / `OTEL_TRACES_SAMPLER_ARG` let a high-volume backend
dial in a parent-based ratio without a code change. Against Datadog the node-local Agent is
the real sampler: it keeps error and denial traces, derives APM metrics from the full stream,
and forwards only a sampled fraction, so always-on at the SDK is correct rather than wasteful.
Tail sampling, keeping every error and downsampling the rest, needs a collector and is
planned.

## Metrics

Écluse emits only what it uniquely knows: queue backlog and DLQ depth are cloud-native
metrics (CloudWatch, Cloud Monitoring), not re-emitted here. Names follow the OTel HTTP
semantic conventions (`http.server.*`) plus an `ecluse.*` namespace for domain signals:

- **Serving**: `http.server.request.duration` (histogram); `ecluse.serve.decision`
  (admit/deny/unavailable); `ecluse.serve.admission.in_flight` and `.queued` from the shared
  admission gate; `ecluse.registry.merge.divergence`, the cross-upstream integrity-divergence
  alarm (see the [threat model](https://ecluse-proxy.com/threat-model.html)), incremented per
  contradicting version with package and version on the paired `WARNING` line rather than a
  label; `ecluse.serve.perimeter.faults` (cause gate/render/unclassified), pre-commit handler
  escapes answered with the neutral 500, so any movement is an invariant break; and
  `ecluse.serve.relay.anomalies` (cause odd_shape/non_success), public relays that were not
  the admitted artifact. The fault channels are mapped in the [fault model](fault-model.md).
- **Gate**: `ecluse.rule.denials` (rule, reason-class); `ecluse.rule.eval.duration` (tier);
  `ecluse.rule.effectful.failures` (cause); `ecluse.rule.breaker.state` (source).
- **Upstream (data plane)**: `ecluse.upstream.fetch.duration` (upstream, status-class);
  `ecluse.upstream.fetch.errors`.
- **Metadata cache**: `ecluse.metadata_cache.requests` (hit/miss) and `.entries`, plus the
  `*.resident_bytes` gauges for the packument, single-version, and assembled stores.
- **Publish body**: `ecluse.publish.body.in_flight_bytes` and `.shed`, the body-byte budget.
- **Mirror**: `ecluse.mirror.enqueued`, `.enqueue.failures`, `.jobs.processed`
  (published/failed, where an idempotent already-present 409 counts as published), and
  `.publish.duration`.
- **Credentials**: `ecluse.credential.refresh` (result, provider);
  `ecluse.credential.token.ttl.seconds`, which alarms a stuck refresh.

`http.server.request.duration` comes from the WAI instrumentation, not emitted by hand. The
`ecluse.*` catalogue is the typed `MetricName` enumeration in `Ecluse.Core.Telemetry.Metrics`.
The serve path and mirror worker record through abstract ports (`Ecluse.Core.Telemetry.Record`)
whose OpenTelemetry-backed instruments live in `Ecluse.Runtime.Telemetry.Instruments`, so the
core records without naming OpenTelemetry.

Metrics export over OTLP push, the same pipeline as traces, to a node-local Collector or the
Datadog Agent's OTLP receiver. A Prometheus scrape endpoint
(`OTEL_METRICS_EXPORTER=prometheus`) is a deferred pull alternative: the SDK honours the
selection, but the pinned set ships no scrape-endpoint renderer, so it is not yet wired.

### Cardinality and attributes

An inline proxy sees thousands of distinct packages, so the failure mode is a metric-series
explosion. The discipline:

- **High-cardinality identifiers stay on spans and logs, never metric labels.** `package`,
  `version`, `scope`, and the full denial message go on the rule-eval span and the log line,
  where a specific decision is debugged.
- **Metric labels are bounded enums only.** The keys are the closed `LabelKey` sum and the
  values the closed `Label` sum, both in `Ecluse.Core.Telemetry.Metrics`; `package`,
  `version`, `scope`, and `message` have no constructor in either, so a high-cardinality
  identifier cannot become a label. `rule` is the one operator-bounded label (a deployment
  defines a small fixed rule set), and a guard test (`Ecluse.Telemetry.MetricsSpec`) pins the
  key set.
- **Secrets and PII never appear in any signal**: no tokens, no `Authorization`. A forwarded
  client token is scrubbed from anything the WAI or http-client instrumentation might capture.
  See [security](security.md).
- **Exemplars** (trace-ID samples on metric buckets) are deferred, pending sampling wiring.

## Logs

Logs stay structured JSON via `katip` on the existing pipeline, not routed over OTLP (1.0
can, but logs already have a working home), and are stitched to traces by trace-ID injection.

The production format is one compact JSON object per line to stdout (JSONL), with embedded
newlines escaped as `\n` so a record never spans physical lines. This is what the Datadog
Agent's stdout autodiscovery consumes. Each line carries a `dd` object for correlation and
unified service tagging:

```json
{"level":"warn","msg":"denied","dd":{"trace_id":"…","span_id":"…","service":"ecluse","env":"prod","version":"1.4.2"},"package":"@evil/pkg","version":"1.0.0","rule":"DenyInstallTimeExecution"}
```

`dd.service` / `dd.env` / `dd.version` come from the same config as the traces, so logs and
traces share one identity. Set the format with `ECLUSE_OBSERVABILITY__LOG_FORMAT`: `json`
(the JSONL above, the in-container default) or `console` (human-readable, for development).

The `dd` object lives in `Ecluse.Runtime.Log`, which has no OpenTelemetry dependency:
`formatDdTraceId` / `formatDdSpanId` render the low 64 bits of the trace and span ids as
unsigned decimal (the 128-bit hex form is a separate opt-in).
`Ecluse.Runtime.Telemetry.Correlation` is the IO half, reading the active span and filling
its ids onto the resolved `service` / `env` / `version` identity
(`Ecluse.Runtime.Telemetry.Resolve`). It is installed as the initial `katip` context at
request and worker entry, so every line carries `dd`; the ids appear only when a span is in
scope, and are absent (never all-zero) otherwise. They must be in the id format Datadog
expects for OTLP-ingested traces to line up, historically the low 64 bits as decimal.

## Configuration and deployment

Telemetry is off until `ECLUSE_OBSERVABILITY__TELEMETRY` is set. The full operator surface
lives in the operator manual: the `OTEL_*` and `DD_*`
[variables](../../USAGE.md#observability-observability) and the
[Datadog Operator recipe](../../USAGE.md#datadog-on-kubernetes). The design facts that matter
here:

- **Self-aligning identity.** An operator may set either dialect, `DD_*` or `OTEL_*`. A
  bounded resolver collapses both into one answer over four fields, `service.name`,
  `deployment.environment`, `service.version`, and the OTLP endpoint, each resolved
  Datadog-wins, then OpenTelemetry, then a default. The one identity feeds both the SDK and
  the `dd` log object, so logs and traces align whichever dialect was used.
- **No agentless export.** `DD_API_KEY` and `DD_SITE` are never read: Écluse exports to a
  node-local Collector or Agent, never to a vendor's cloud. The OTLP endpoint is an
  operator-declared destination, not attacker-influenced input, so it is not range-checked or
  SSRF-classified (that classifier guards the untrusted package-download path). A remote
  collector is authenticated out of band via `OTEL_EXPORTER_OTLP_HEADERS`.
- **Export never touches the request path.** The batch exporter runs asynchronously, so an
  unreachable collector never blocks a request. An absent endpoint defaults to
  `http://localhost:4318` with one boot warning. `hs-opentelemetry` drops a failed OTLP
  export silently, so Écluse wraps both exporters to route a failure through `katip` under a
  throttle: the first failure logged plainly, then a periodic heartbeat with the suppressed
  count.

> **Runtime (CPU): pin the RTS capability count to the container's CPU limit.** The image
> runs the threaded RTS, which telemetry requires: the OTel SDK's batch span processor aborts
> under the non-threaded runtime. The default `-N` ignores the cgroup CPU quota, so in a
> CPU-limited container it over-subscribes the node's cores, and the resulting GC and
> scheduler contention surfaces as tail latency. Set `GHCRTS=-Nx` to the pod's CPU limit. See
> [core and heap sizing](../../USAGE.md#runtime-runtime).

## Verifying it

A passing compile does not prove a span or metric arrives. Telemetry is checked at the
cheapest [testing](../testing.md) tier that can carry each concern: unit tests for config
parsing, the denial span-attribute mapping, the JSONL scribe, and the label-domain guard; a
Dockerised integration tier that drives a request through an in-process Écluse and asserts a
real Agent or Collector container accepted the spans and metrics, with no Datadog SaaS; and a
non-gating, secret-gated smoke tier that emits a uniquely stamped span and metric and polls
the Datadog API until it appears. The gate stays fast and hermetic; the live check carries its
own weight.
