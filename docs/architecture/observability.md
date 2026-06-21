# Observability

> Part of the [Écluse architecture overview](../architecture.md).

Écluse is an inline dependency in someone else's build path, so when it is slow
or refuses a package the operator needs to see *why* without attaching a
debugger. Observability is therefore **opt-in and vendor-neutral**: the substrate
is **OpenTelemetry**, emitting **OTLP**, which any compatible backend can receive
— an OpenTelemetry Collector, Jaeger, Honeycomb, Grafana Tempo, a Prometheus
scrape, and so on.

**Datadog is a first-class, fully supported target** — it is what this project's
maintainer runs, so it gets a documented, tested deployment path — but it is
**never required**. Nothing in the core takes a hard dependency on Datadog;
switching backends is a configuration change, not a code change; and Écluse runs
perfectly with telemetry switched **off entirely, which is the default**. This is
a FOSS project: the maintainer's choice of backend must not become every
consumer's obligation. The Datadog-specific pieces below (the Operator deployment
recipe, the Datadog trace propagator, the `dd.*` log fields, and the Agent-side
sampling) are all clearly marked as optional add-ons on top of the neutral OTLP
baseline.

This is designed but not yet built — the web layer's middleware stack leaves a
slot for it (see
[Web Layer → Middleware](web-layer.md#middleware-and-helper-libraries)).

## OpenTelemetry as the substrate

Choosing OpenTelemetry is precisely what keeps Datadog optional: OTLP is a
vendor-neutral wire protocol, so one set of instrumentation feeds any backend and
the choice of vendor collapses to an endpoint. (It also happens to be the only
realistic route for Haskell — there is no first-party Datadog tracing library —
but the neutrality is the point, not a consolation.) We build on
[`hs-opentelemetry`](https://github.com/iand675/hs-opentelemetry) **1.0** (May
2026), which — after years of "coming soon" — now ships **metrics and logs
alongside tracing**, with OTLP export *and* a built-in scrapable Prometheus
exporter. That maturity is what lets **metrics ride the same OTLP pipeline as
traces** (no separate metrics transport). The packages, all wired in the
composition root:

| Package | Role for Écluse |
|---|---|
| `hs-opentelemetry-sdk` | Tracer **and meter** provider, batching, lifecycle. |
| `hs-opentelemetry-instrumentation-wai` | One server span per request — slots into the raw-WAI [middleware stack](web-layer.md#middleware-and-helper-libraries). |
| `hs-opentelemetry-instrumentation-http-client` | Child spans + context propagation on the **data plane** (`http-client`) — the upstream fetches that *are* the proxy's work (see [Control plane vs. data plane](web-layer.md#control-plane-vs-data-plane)). |
| `hs-opentelemetry-exporter-otlp` | OTLP export for **traces and metrics** — **HTTP/protobuf by default**; gRPC is behind a cabal flag (pulls in `grapesy`) and we do not need it. |
| the library's **Prometheus exporter** | Optional pull alternative — a scrapable `/metrics` endpoint for Prometheus/Grafana stacks. |
| the **GHC runtime-metrics** instrumentation (new in 1.0) | GC pauses, heap live/allocated — GC pauses directly drive an inline proxy's tail latency. |
| `hs-opentelemetry-propagator-datadog` | **Optional, Datadog-only.** Reads/writes Datadog's `x-datadog-*` trace headers so traces join up with services already running `dd-trace`. Every other backend uses the default W3C TraceContext propagator. |

Only the last row is vendor-specific, and it is optional; everything above it is
backend-agnostic. The pins live with the rest of the dependency choices in
[Technology Stack](technology-stack.md).

## What gets traced

The instrumentation maps onto the [Request Lifecycle](../architecture.md#request-lifecycle):
a server span from the WAI middleware, with child spans for each upstream fetch
(private then public) from the http-client instrumentation. Domain spans added by
hand because they carry the decisions an operator cares about:

- **rule evaluation** — attributes for the verdict and, on denial, the
  `RuleName` and `RejectReason` (mirrors the [error model](web-layer.md#error-model)),
  so a 403 is explainable from the trace alone.
- **mirror enqueue** — links the synchronous request to the asynchronous mirror
  job (see [Cloud Backends](cloud-backends.md#cloud-backends)).
- **mirror worker job** — the async fetch→verify→publish, linked from the enqueue
  span, so background work is not invisible.
- **advisory sync** — one span per [advisory-dataset sync](rules-engine.md#cve-subsystem)
  run.

### Sampling

Note the traffic shape: the **high-volume path is boring** (private-mirror hits,
served fast and rule-free) and the **low-volume path is the interesting one**
(public fallback → rules → denial/mirror). The ideal — "keep every error/denial,
downsample the boring" — is **tail sampling**, which needs a collector and so is a
**fast-follow**, not a launch item.

For launch, sampling is **head-based and lives in two places**:

- **SDK (Écluse): always-on by default.** When telemetry is enabled, every trace
  is emitted, so the rare-but-important denial/error traces are never missed. The
  standard `OTEL_TRACES_SAMPLER` / `OTEL_TRACES_SAMPLER_ARG` env vars (read
  directly by `hs-opentelemetry`) let a high-volume deployment dial in a
  parent-based ratio without a code change — the right lever for a non-Datadog
  OTLP backend that has no sampling stage of its own.
- **Datadog Agent: the actual sampler.** With the Datadog target, *send everything
  to the node-local Agent and let it sample* — it derives accurate APM/span
  metrics from the full stream, its **error sampler keeps error/denial traces**,
  and only a sampled percentage is forwarded to the paid backend. Always-on at the
  SDK is therefore correct, not wasteful: the in-cluster hop is cheap and the
  Agent is the sampling stage (see [Datadog deployment](#datadog-deployment-operator)).

## Metrics

**Emit only what Écluse uniquely knows.** Queue *backlog* and *DLQ depth* are
already first-class **cloud-native** metrics (CloudWatch for SQS, Cloud Monitoring
for Pub/Sub); having Écluse poll the queue API to re-emit them is duplicative, so
we rely on cloud-native for those. Names follow **OTel semantic conventions** for
HTTP (`http.server.*`, `http.client.*`) and a custom **`ecluse.*`** namespace for
domain signals. The catalog:

- **Serving** — `http.server.request.duration` (histogram); `ecluse.serve.decision`
  (counter; admit/deny/unavailable).
- **Gate** — `ecluse.rule.denials` (counter; rule, reason-class); `ecluse.rule.eval.duration`
  (histogram; tier); `ecluse.rule.effectful.failures` (counter; rule, cause);
  `ecluse.rule.breaker.state` (gauge; source).
- **Advisory sync (CVE)** — `ecluse.advisory.sync.age.seconds` (gauge) ← the
  staleness alarm; `ecluse.advisory.sync.failures` (counter);
  `ecluse.advisory.serving_last_good` (gauge 0/1).
- **Upstream (data plane)** — `ecluse.upstream.fetch.duration` (histogram;
  upstream, status-class); `ecluse.upstream.fetch.errors` (counter).
- **Metadata cache** — `ecluse.metadata_cache.requests` (counter; result hit/miss)
  → hit rate; `ecluse.metadata_cache.entries` (gauge).
- **Mirror** (what we know, not queue depth) — `ecluse.mirror.enqueued`,
  `ecluse.mirror.enqueue.failures`, `ecluse.mirror.jobs.processed`
  (result published/already-exists/failed), `ecluse.mirror.publish.duration`.
- **Credentials** — `ecluse.credential.refresh` (counter; result, provider);
  `ecluse.credential.token.ttl.seconds` (gauge) ← alarms a stuck refresh.
- **Runtime** — the GHC runtime-metrics instrumentation (GC pauses, heap).

**Transport.** Metrics export over **OTLP** (the same pipeline as traces) by
default; the built-in **Prometheus scrape** endpoint is config-selectable
(`OTEL_METRICS_EXPORTER=prometheus`) for pull-based stacks.

### Cardinality and attributes

An inline proxy sees thousands of distinct packages, so the failure mode is a
**metric-series explosion**. The discipline:

- **High-cardinality identifiers live on spans and logs, never on metric labels.**
  `package`, `version`, `scope`, and the full denial *message* go on the rule-eval
  **span** and the structured **log line** — that is where you debug a specific
  decision — and must never become metric labels.
- **Metric labels are a closed set of bounded enums only:** `rule`, `decision`,
  `reason_class`, `ecosystem`, `mount`, `upstream`, `status_class`, `result`,
  `provider`, `cause`/`error_class`, breaker `source`, `tier`. Every one has a
  small, fixed domain. (Any PR adding a label whose domain is not obviously finite
  is rejected.)
- **Secrets/PII never appear in any signal** — no tokens, no `Authorization`,
  anywhere. In particular the **forwarded client token** (see
  [Credential flow](registry-model.md#credential-flow-and-authority)) must be
  scrubbed from anything the WAI / http-client instrumentation might capture.
- **Exemplars** (trace-ID samples attached to metric buckets, for dashboard→trace
  drill-down without high-cardinality labels) are the intended bridge between
  bounded metrics and high-cardinality traces, but are **deferred**: they depend
  on sampling being wired and on confirming the brand-new 1.0 metrics SDK emits
  them.

## Logs

Logs stay structured JSON via `katip`, shipped on the existing log pipeline —
**not** routed over OTLP (1.0 *can* do OTLP logs, but logs already have a working
home and re-plumbing buys nothing). They are stitched to traces by **trace-ID
injection**.

The production format is **one compact JSON object per line to stdout** (JSONL):
the whole line *is* the JSON — no pretty-printing, no level/timestamp prefix
outside the object, embedded newlines escaped as `\n` so a record never spans
physical lines. This is exactly what the Datadog Agent's stdout/stderr
autodiscovery JSON parsing consumes. Each line carries a populated **`dd` object**
for correlation and unified service tagging:

```json
{"level":"warn","msg":"denied","dd":{"trace_id":"…","span_id":"…","service":"ecluse","env":"prod","version":"1.4.2"},"package":"@evil/pkg","version":"1.0.0","rule":"DenyHasInstallScripts"}
```

`dd.service`/`dd.env`/`dd.version` are sourced from the **same** config as the
traces (`OTEL_SERVICE_NAME` / `OTEL_RESOURCE_ATTRIBUTES`, or `DD_SERVICE`/`DD_ENV`/
`DD_VERSION`) so logs and traces share one identity.

The format is switchable: **`PROXY_LOG_FORMAT=json`** (the JSONL above, the
in-container default) or **`console`** (human-readable, for the dev ecosystem).

> **Correlation gotcha (implementation).** `dd.trace_id`/`dd.span_id` must be in
> the **id format Datadog expects** for the OTLP-ingested traces to line up —
> historically the low-64-bits-as-decimal, full 128-bit hex where enabled. Verify
> against the Agent's trace-id handling; it is the one fiddly correlation detail.

## Datadog deployment (Operator)

Deployment is via the **Datadog Operator** — a `DatadogAgent` custom resource
(`datadoghq.com/v2alpha1`) that manages the node Agent. There is **no UDS/hostPath
socket machinery**: traces *and* metrics go OTLP over TCP to the node-local Agent,
and logs are scraped from stdout.

1. **Enable the Agent's OTLP receiver** in the CR — traces and metrics are on by
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

2. **Point Écluse at the node-local Agent** using the Downward API for the host IP
   — one OTLP endpoint for both traces and metrics:

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

## Configuration

Telemetry uses the **standard `OTEL_*` variables** (read directly by
`hs-opentelemetry`) plus a few `PROXY_*` ones; see
[Configuration](configuration.md). With `PROXY_TELEMETRY` unset nothing is wired
and no telemetry is emitted.

| Variable | Purpose |
|---|---|
| `PROXY_TELEMETRY` | Master switch (`off` by default — telemetry is opt-in). |
| `OTEL_SERVICE_NAME` | Service identity (`ecluse`); also `dd.service`. |
| `OTEL_RESOURCE_ATTRIBUTES` | `deployment.environment`, `service.version`, … (feed `dd.env`/`dd.version`). |
| `OTEL_EXPORTER_OTLP_ENDPOINT` | Any OTLP receiver — a Collector, Jaeger, the Datadog Agent (`http://$(HOST_IP):4318`). |
| `OTEL_EXPORTER_OTLP_PROTOCOL` | `http/protobuf` (default; avoids the gRPC/`grapesy` flag). |
| `OTEL_TRACES_SAMPLER` / `…_ARG` | SDK sampler — default always-on; ratio lever for non-Datadog backends. |
| `OTEL_METRICS_EXPORTER` | `otlp` (default) or `prometheus` (scrape). |
| `OTEL_EXPORTER_PROMETHEUS_HOST` / `…_PORT` | Bind address for the Prometheus scrape endpoint, when selected. |
| `PROXY_LOG_FORMAT` | `json` (one-line JSONL to stdout, default) or `console` (human-readable, dev). |

## Verifying it — smoke-test plan

The goal is *full confidence that a span and a metric emitted by Écluse actually
arrive*, not just that the code compiles. It layers onto the existing three-tier
[testing strategy](../../CONTRIBUTING.md), proving each concern at the cheapest
tier that can. Tiers 1–2 are **backend-agnostic** (they prove the OTLP path any
consumer relies on); the live Datadog check is the Datadog target carrying its own
weight, not a baseline requirement:

1. **Unit (pure, gating).**
   - Telemetry config parsing from the env vars above (present/absent/malformed).
   - Span-attribute mapping for a denial (verdict + `RuleName` → attributes).
   - The **JSONL log scribe**: a record serialises to exactly one line, with a
     populated `dd` object and embedded newlines escaped (table-driven).
   - Metric naming/labels stay within the bounded-enum set (a label-domain guard).

2. **Integration (Dockerised, `testcontainers`, the same tier as the mirror-queue
   tests).** Prove the wires carry bytes, with no dependency on the Datadog SaaS:
   - **Traces + metrics to a real Agent.** Run the **Datadog Agent** container with
     OTLP enabled and a *dummy* API key (intake is accepted locally even though
     forwarding to DD fails). Drive a request through an in-process Écluse, then
     assert the Agent *accepted* the spans and metrics via its own diagnostics
     (`agent status` OTLP-receiver counters).
   - **Prometheus endpoint.** With `OTEL_METRICS_EXPORTER=prometheus`, scrape
     `/metrics` and assert the expected series/labels appear.
   - *(Optional, vendor-neutral)* an OTLP **Collector** container with a `debug`
     exporter, asserting spans/metrics are received.

3. **Smoke (live, non-gating, scheduled, secret-gated — like the live-registry
   oracle check).** True end-to-end including the Datadog backend: emit a span and
   a metric stamped with a unique `smoke.run_id`, then poll the **Datadog API**
   until that trace/metric appears (or time out). Runs against a sandbox org,
   guarded by repository secrets, never on the PR gate.

The split keeps the gate fast and hermetic (tiers 1–2 need no network and no
Datadog account) while still giving a real, periodic end-to-end signal (tier 3).
