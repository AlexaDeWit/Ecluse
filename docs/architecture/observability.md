# Observability

> Part of the [Écluse architecture overview](../architecture.md).

Écluse sits in the install path of someone else's build, so an operator needs to see
*why* it is slow or refusing a package without attaching a debugger. Observability is
opt-in and vendor-neutral: the substrate is **OpenTelemetry**, emitting OTLP, which any
compatible backend receives (an OpenTelemetry Collector, Jaeger, Honeycomb, Grafana
Tempo, a Prometheus scrape).

Datadog is a first-class target, what the maintainer runs, so it gets a documented,
tested path, but it is never required: nothing in the core depends on it, switching
backends is a config change, and telemetry is off by default. The Datadog-specific
pieces below (the Operator recipe, trace propagator, `dd.*` log fields, Agent-side
sampling) are optional add-ons on the OTLP baseline.

The substrate is built: self-aligning configuration (`Ecluse.Telemetry`,
`Ecluse.Telemetry.Resolve`), request-lifecycle tracing (`Ecluse.Telemetry.Tracing`), the
`ecluse.*` metric catalogue and its bounded-label discipline (`Ecluse.Telemetry.Metrics`)
with the runtime instruments (`Ecluse.Telemetry.Instruments`), and the logs↔traces `dd`
correlation (`Ecluse.Telemetry.Correlation` over the `dd` object in `Ecluse.Log`). It
slots into the web layer's [middleware stack](web-layer.md#middleware-and-helper-libraries).
Everything stays inert when `ECLUSE_TELEMETRY` is unset: the SDK is not initialised, no
spans open, and the instruments are built on the no-op meter, so an emit is a discarded
measurement, not a branch.

## OpenTelemetry as the substrate

OpenTelemetry keeps Datadog optional: OTLP is a vendor-neutral wire protocol, so one
set of instrumentation feeds any backend and the vendor choice collapses to an
endpoint. (It is also the only realistic route for Haskell, which has no first-party
Datadog tracing library.) Écluse builds on
[`hs-opentelemetry`](https://github.com/iand675/hs-opentelemetry) **1.0** (May 2026),
which ships metrics and logs alongside tracing, with OTLP export and a built-in
scrapable Prometheus exporter, so metrics ride the same OTLP pipeline as traces. The
packages, all wired in the composition root:

| Package | Role for Écluse |
|---|---|
| `hs-opentelemetry-sdk` | Tracer **and meter** provider, batching, lifecycle. |
| `hs-opentelemetry-instrumentation-wai` | One server span per request, slots into the raw-WAI [middleware stack](web-layer.md#middleware-and-helper-libraries). |
| `hs-opentelemetry-instrumentation-http-client` | Child spans + context propagation on the **data plane** (`http-client`), the upstream fetches that *are* the proxy's work (see [Control plane vs. data plane](web-layer.md#control-plane-vs-data-plane)). |
| `hs-opentelemetry-exporter-otlp` | OTLP export for **traces and metrics**, HTTP/protobuf by default; gRPC is behind a cabal flag (pulls in `grapesy`), unused. |
| the library's **Prometheus exporter** | Optional pull alternative, a scrapable `/metrics` endpoint for Prometheus/Grafana stacks. |
| the **GHC runtime-metrics** instrumentation (new in 1.0) | GC pauses and heap live/allocated; GC pauses drive an inline proxy's tail latency directly. |
| `hs-opentelemetry-propagator-datadog` | **Optional, Datadog-only.** Reads/writes Datadog's `x-datadog-*` trace headers so traces join up with services already running `dd-trace`. Every other backend uses the default W3C TraceContext propagator. |

Only the last row is vendor-specific and optional; everything above is
backend-agnostic. The pins live with the other dependency choices in
[Technology Stack](technology-stack.md).

## What gets traced

The instrumentation maps onto the [Request Lifecycle](../architecture.md#request-lifecycle):
a WAI server span, with child spans for each upstream fetch (private then public) from
the http-client instrumentation. Domain spans are added by hand because they carry the
decisions an operator cares about:

- **rule evaluation**, attributes for the verdict and, on denial, the `RuleName` and
  `RejectReason` (mirroring the [error model](web-layer.md#error-model)), so a 403 is
  explainable from the trace alone.
- **mirror enqueue**, a producer span over the serve-time hand-off to the async mirror.
  It captures its W3C trace context onto the `MirrorJob` so the worker can link back, and
  records a swallowed best-effort enqueue failure on its status, so the trace says *why*
  the mirror did not happen beyond the `ecluse.mirror.enqueue.failures` counter.
- **mirror worker job**, the async probe→re-evaluate→fetch→verify→publish consumer span.
  It carries a span link to the enqueue span, re-established from the job's W3C context, a
  true cross-async link rather than a `package@version` correlation. Batch-safe: one poll
  may mix jobs from many requests, so each links to its own producer, not an arbitrary
  parent. A job enqueued with tracing off bears no link.
- **advisory sync**, one span per [advisory-dataset sync](rules-engine.md#cve-subsystem)
  run.

### Sampling

The traffic shape matters: the high-volume path is dull (private-mirror hits, served
fast and rule-free) and the low-volume path is the interesting one (public fallback →
rules → denial/mirror). The ideal, keep every error/denial and downsample the rest, is
tail sampling, which needs a collector and is a planned follow-up.

For launch, sampling is head-based, in two places:

- **SDK (Écluse): always-on by default.** Every trace is emitted, so rare denial/error
  traces are never missed. The standard `OTEL_TRACES_SAMPLER` / `OTEL_TRACES_SAMPLER_ARG`
  env vars (read by `hs-opentelemetry`) let a high-volume deployment dial in a
  parent-based ratio without a code change, the right lever for a non-Datadog OTLP
  backend with no sampling stage of its own.
- **Datadog Agent: the actual sampler.** Send everything to the node-local Agent and let
  it sample: it derives accurate APM/span metrics from the full stream, its error sampler
  keeps error/denial traces, and only a sampled percentage reaches the paid backend.
  Always-on at the SDK is therefore correct, not wasteful, since the in-cluster hop is
  cheap (see [Datadog deployment](#datadog-deployment-operator)).

## Metrics

**Emit only what Écluse uniquely knows.** Queue backlog and DLQ depth are already
cloud-native metrics (CloudWatch for SQS, Cloud Monitoring for Pub/Sub), so Écluse does
not re-emit them. Names follow OTel semantic conventions for HTTP (`http.server.*`,
`http.client.*`) plus a custom `ecluse.*` namespace for domain signals. The catalogue:

- **Serving**, `http.server.request.duration` (histogram); `ecluse.serve.decision`
  (counter; admit/deny/unavailable); `ecluse.registry.merge.divergence` (counter) ← the
  cross-upstream integrity-divergence alarm (threat #11), incremented once per contradicting
  version. The package and version are on the paired `WARNING` log line, never a metric label.
  `ecluse.serve.perimeter.faults` (counter; cause gate/render/unclassified) counts pre-commit
  handler escapes the typed request perimeter answered with the neutral 500 -- steady state is
  zero, so any movement is an invariant break worth a look; the paired `ERROR` audit line carries
  the request path and the bounded rendered detail (`perimeterCause`/`perimeterDetail` fields).
  The perimeter and the fault channels feeding these signals are mapped in the
  [fault model](fault-model.md).
- **Gate**, `ecluse.rule.denials` (counter; rule, reason-class); `ecluse.rule.eval.duration`
  (histogram; tier); `ecluse.rule.effectful.failures` (counter; rule, cause);
  `ecluse.rule.breaker.state` (gauge; source).
- **Advisory sync (CVE)**, `ecluse.advisory.sync.age.seconds` (gauge) ← the
  staleness alarm; `ecluse.advisory.sync.failures` (counter);
  `ecluse.advisory.serving_last_good` (gauge 0/1).
- **Upstream (data plane)**, `ecluse.upstream.fetch.duration` (histogram;
  upstream, status-class); `ecluse.upstream.fetch.errors` (counter).
- **Metadata cache**, `ecluse.metadata_cache.requests` (counter; result hit/miss)
  → hit rate; `ecluse.metadata_cache.entries` (gauge).
- **Mirror** (what we know, not queue depth), `ecluse.mirror.enqueued`,
  `ecluse.mirror.enqueue.failures`, `ecluse.mirror.jobs.processed`
  (result published/failed, the idempotent already-present 409 counts as published),
  `ecluse.mirror.publish.duration`. `ecluse.serve.relay.anomalies` (counter; cause
  odd_shape/non_success) counts public artifact relays that were not the admitted
  artifact, judged from status and headers alone at relay time -- the body still relays
  verbatim, and such a relay enqueues no mirror job; the paired `WARNING` line carries
  the package and version.
- **Credentials**, `ecluse.credential.refresh` (counter; result, provider);
  `ecluse.credential.token.ttl.seconds` (gauge) ← alarms a stuck refresh.
- **Runtime**, the GHC runtime-metrics instrumentation (GC pauses, heap).

`http.server.request.duration` comes from the WAI instrumentation
(`Ecluse.Telemetry.Tracing`), not re-emitted by hand. The `ecluse.*` catalogue is the
typed `MetricName` enumeration in `Ecluse.Core.Telemetry.Metrics`; the live instruments
and one `record*` helper per signal live in `Ecluse.Telemetry.Instruments`, recorded from
the serve path (`Ecluse.Core.Server.Pipeline`), the metadata cache
(`Ecluse.Core.Server.Cache`), and the mirror worker (`Ecluse.Core.Worker`). Two signals
are defined but not yet wired: the circuit-breaker state gauge and the credential
refresh/ttl signals, whose sources sit off the composition root, so emitting them is a
boot-sequencing follow-up. The advisory-sync metrics are likewise defined but not yet
emitted.

**Transport.** Metrics export over OTLP push (the same pipeline as traces) to a
node-local collector or the Datadog Agent's OTLP receiver, which auto-maps OTLP to the
backend's format. A Prometheus scrape endpoint (`OTEL_METRICS_EXPORTER=prometheus`) is a
deferred pull alternative: the SDK honours the selection, but the pinned OpenTelemetry
set ships no scrape-endpoint renderer, so it is not yet wired. DogStatsD is not used (no
maintained GHC 9.10 client).

### Cardinality and attributes

An inline proxy sees thousands of distinct packages, so the failure mode is a
metric-series explosion. The discipline:

- **High-cardinality identifiers live on spans and logs, never metric labels.**
  `package`, `version`, `scope`, and the full denial message go on the rule-eval span and
  the log line, where you debug a specific decision, never on a metric.
- **Metric labels are bounded enums only:** `rule`, `decision`, `reason_class`,
  `ecosystem`, `mount`, `upstream`, `status_class`, `result`, `provider`,
  `cause`/`error_class`, breaker `source`, `tier`, each with a small fixed domain. The
  type system enforces it: the closed set is the `Label` sum in `Ecluse.Telemetry.Metrics`,
  and `package`, `version`, `scope`, and `message` have no constructor at all, so a
  high-cardinality identifier cannot become a label; `rule` is the one operator-bounded
  label. A guard test (`Ecluse.Telemetry.MetricsSpec`) pins the key set and rejects the
  high-cardinality keys.
- **Secrets and PII never appear in any signal**, no tokens, no `Authorization`. In
  particular a forwarded client token (under the `passthrough` [strategy](access-model.md))
  is scrubbed from anything the WAI / http-client instrumentation might capture.
- **Exemplars** (trace-ID samples on metric buckets, for dashboard→trace drill-down
  without high-cardinality labels) are deferred: they depend on sampling being wired and
  on confirming the 1.0 metrics SDK emits them.

## Logs

Logs stay structured JSON via `katip` on the existing log pipeline, not routed over OTLP
(1.0 can, but logs already have a working home). They are stitched to traces by trace-ID
injection.

The production format is one compact JSON object per line to stdout (JSONL): the whole
line is the JSON, no prefix outside the object, embedded newlines escaped as `\n` so a
record never spans physical lines. This is what the Datadog Agent's stdout autodiscovery
consumes. Each line carries a `dd` object for correlation and unified service tagging:

```json
{"level":"warn","msg":"denied","dd":{"trace_id":"…","span_id":"…","service":"ecluse","env":"prod","version":"1.4.2"},"package":"@evil/pkg","version":"1.0.0","rule":"DenyInstallTimeExecution","reason_class":"ReasonPolicy","active_advisory_db_etag":"d41d8cd98f00b204e9800998ecf8427e"}
```

`dd.service`/`dd.env`/`dd.version` come from the same config as the traces
(`OTEL_SERVICE_NAME` / `OTEL_RESOURCE_ATTRIBUTES`, or `DD_SERVICE`/`DD_ENV`/`DD_VERSION`),
so logs and traces share one identity.

Switch format with `ECLUSE_LOG_FORMAT`: `json` (the JSONL above, in-container default) or
`console` (human-readable, for development).

**Correlation gotcha.** `dd.trace_id`/`dd.span_id` must be in the id format Datadog
expects for OTLP-ingested traces to line up: historically the low 64 bits as decimal,
full 128-bit hex where enabled. Verify against the Agent's trace-id handling.

The `dd` object lives in `Ecluse.Log` (no OpenTelemetry dependency):
`formatDdTraceId`/`formatDdSpanId` render the unsigned decimal of the low 64 bits of the
trace id and the 64-bit span id (the 128-bit-hex form is a separate opt-in).
`Ecluse.Telemetry.Correlation` is the IO half: it reads the active span and fills its ids
onto the resolved `service`/`env`/`version` identity (the same `Ecluse.Telemetry.Resolve`
answer the exporter uses). It is installed as the initial `katip` context at request
entry (`runHandler`) and worker entry (`runWorkerM`), so every line carries `dd`; the
trace/span ids appear only when a span is in scope, and are absent (never all-zero)
otherwise.

## Datadog deployment (operator)

Deployment is via the Datadog Operator, a `DatadogAgent` custom resource
(`datadoghq.com/v2alpha1`) managing the node Agent. No UDS/hostPath socket machinery:
traces and metrics go OTLP over TCP to the node-local Agent, and logs are scraped from
stdout.

1. **Enable the Agent's OTLP receiver** in the CR, traces and metrics are on by
   default once OTLP is configured:

   ```yaml
   apiVersion: datadoghq.com/v2alpha1
   kind: DatadogAgent
   spec:
     features:
       otlp:
         receiver:
           protocols:
             http: { enabled: true }   # :4318
     override:
       nodeAgent:
         env:                          # Agent-side sampling (the sampler lives here)
           - { name: DD_APM_PROBABILISTIC_SAMPLER_ENABLED, value: "true" }
           - { name: DD_APM_PROBABILISTIC_SAMPLER_SAMPLING_PERCENTAGE, value: "20" }
   ```

   The probabilistic sampler needs Agent **v7.70+**; the error sampler is already
   on, the rare sampler optional.

2. **Point Écluse at the node-local Agent** using the Downward API for the host IP,
   one OTLP endpoint for both traces and metrics:

   ```yaml
   env:
     - name: HOST_IP
       valueFrom: { fieldRef: { fieldPath: status.hostIP } }
     - name: OTEL_EXPORTER_OTLP_ENDPOINT
       value: "http://$(HOST_IP):4318"
     - name: OTEL_EXPORTER_OTLP_PROTOCOL
       value: "http/protobuf"
   ```

3. **Logs** need no extra wiring: Écluse writes JSONL to stdout and the Agent's
   container log collection picks it up.

> **Runtime (CPU): pin the RTS capability count to the container's CPU limit.** The
> image runs the threaded RTS (required: the OTel SDK's batch span processor aborts under
> the non-threaded runtime) and defaults to `-N`. But `-N` reads the host core count and
> ignores the cgroup CPU quota, so in a CPU-limited container it over-subscribes to the
> node's cores, and the resulting GC/scheduler contention shows up as tail latency. Set
> `GHCRTS=-Nx` (or `+RTS -Nx -RTS`) where `x` matches the pod's CPU `limit`, so the RTS
> schedules to the cores it is allotted.

## Configuration

Telemetry uses the standard `OTEL_*` variables (read by `hs-opentelemetry`) plus a few
`PROXY_*` ones; see [Configuration](configuration.md). With `ECLUSE_TELEMETRY` unset,
nothing is wired and no telemetry is emitted.

**Self-aligning configuration.** An operator may set either dialect: `DD_*` or `OTEL_*`.
A bounded resolver collapses both into one answer over four fields, `service.name`,
`deployment.environment`, `service.version`, and the OTLP endpoint, each resolved
Datadog-wins → OpenTelemetry → default (e.g. `DD_SERVICE` → `OTEL_SERVICE_NAME` →
`service.name` in `OTEL_RESOURCE_ATTRIBUTES` → `ecluse`; `DD_AGENT_HOST` →
`OTEL_EXPORTER_OTLP_ENDPOINT` → `http://localhost:4318`). The one identity feeds both the
SDK and the `dd` log object, so logs and traces align whichever dialect was used.
`DD_API_KEY`/`DD_SITE` are never read: Écluse exports to a node-local collector/Agent,
never agentless to a vendor's cloud.

**The OTLP endpoint is an operator-declared destination, not classified.** Like the
mirror-queue endpoint, the collector/Agent address is configuration, not
attacker-influenced input, so Écluse does not range-check or gate it (the SSRF classifier
guards the untrusted package-download path, where the target is upstream-supplied; this
endpoint is neither). Agentless export to a vendor's SaaS is excluded structurally, since
`DD_API_KEY`/`DD_SITE` are never read; deliberate remote export is just a declared
endpoint, authenticated out of band via `OTEL_EXPORTER_OTLP_HEADERS`.

**Export failures never touch the request path.** The batch exporter runs asynchronously,
so an unreachable collector never blocks a request. An absent endpoint defaults to
`http://localhost:4318` with one boot warning. `hs-opentelemetry 1.0.0.0` drops a failed
OTLP export silently, so Écluse wraps both exporters to observe the result and route a
failure through `katip` under a shared throttle (`Ecluse.Telemetry.ExportFailure`): the first
failure logged plainly, then a periodic heartbeat with the suppressed count. The wrappers only
observe; export semantics are unchanged.

| Variable | Purpose |
|---|---|
| `ECLUSE_TELEMETRY` | Master switch (`off` by default, telemetry is opt-in). |
| `OTEL_SERVICE_NAME` / `DD_SERVICE` | Service identity (`ecluse`); also `dd.service`. `DD_SERVICE` wins. |
| `OTEL_RESOURCE_ATTRIBUTES` / `DD_ENV` / `DD_VERSION` | `deployment.environment`, `service.version` (feed `dd.env`/`dd.version`). `DD_*` win. |
| `OTEL_EXPORTER_OTLP_ENDPOINT` / `DD_AGENT_HOST` | OTLP receiver, a Collector or the Datadog Agent (`http://$(HOST_IP):4318`). `DD_AGENT_HOST` wins (as `:4318`). |
| `OTEL_EXPORTER_OTLP_PROTOCOL` | `http/protobuf` (the only transport built; gRPC/`grapesy` flag is off). |
| `OTEL_TRACES_SAMPLER` / `…_ARG` | SDK sampler, default always-on; ratio lever for non-Datadog backends. |
| `OTEL_EXPORTER_OTLP_HEADERS` | Out-of-band auth for a remote collector/Agent (read by the SDK, never by Écluse). |
| `OTEL_METRICS_EXPORTER` | `otlp` (default). `prometheus` is recognised but the scrape endpoint is **deferred**. |
| `ECLUSE_LOG_FORMAT` | `json` (one-line JSONL to stdout, default) or `console` (human-readable, dev). |

## Verifying it, smoke-test plan

The goal is confidence that a span and a metric Écluse emits actually arrive, beyond a
passing compile. It layers onto the three-tier [testing strategy](../testing.md),
proving each concern at the cheapest tier that can. Tiers 1-2 are backend-agnostic; the
live Datadog check is the Datadog target carrying its own weight, not a baseline:

1. **Unit (pure, gating).**
   - Telemetry config parsing from the env vars above (present/absent/malformed).
   - Span-attribute mapping for a denial (verdict + `RuleName` → attributes).
   - The **JSONL log scribe**: a record serialises to exactly one line, with a
     populated `dd` object and embedded newlines escaped (table-driven).
   - Metric naming/labels stay within the bounded-enum set (a label-domain guard).

2. **Integration (Dockerised, `testcontainers`, the same tier as the mirror-queue
   tests).** Prove the wires carry bytes, with no dependency on the Datadog SaaS:
   - **Traces + metrics to a real Agent.** Run the Datadog Agent container with OTLP
     enabled and a dummy API key (intake is accepted locally). Drive a request through an
     in-process Écluse, then assert the Agent accepted the spans and metrics via its
     diagnostics (`agent status` OTLP-receiver counters).
   - **Prometheus endpoint.** With `OTEL_METRICS_EXPORTER=prometheus`, scrape
     `/metrics` and assert the expected series/labels appear.
   - *(Optional, vendor-neutral)* an OTLP **Collector** container with a `debug`
     exporter, asserting spans/metrics are received.

3. **Smoke (live, non-gating, scheduled, secret-gated).** End-to-end including the
   Datadog backend: emit a span and metric stamped with a unique `smoke.run_id`, then
   poll the Datadog API until it appears (or time out). Runs against a sandbox org,
   guarded by repository secrets, never on the PR gate.

The split keeps the gate fast and hermetic (tiers 1-2 need no network or Datadog
account) while still giving a periodic end-to-end signal (tier 3).
