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
consumer's obligation. The Datadog-specific pieces below (the Agent deployment
recipe, the DogStatsD socket, the Datadog propagator) are all clearly marked as
optional add-ons on top of the neutral OTLP baseline.

This is designed but not yet built — the web layer's middleware stack leaves a
slot for it (see
[Web Layer → Middleware](web-layer.md#middleware-and-helper-libraries)).

## OpenTelemetry as the substrate

Choosing OpenTelemetry is precisely what keeps Datadog optional: OTLP is a
vendor-neutral wire protocol, so one set of instrumentation feeds any backend and
the choice of vendor collapses to an endpoint. (It also happens to be the only
realistic route for Haskell — there is no first-party Datadog tracing library —
but the neutrality is the point, not a consolation.) We build on
[`hs-opentelemetry`](https://github.com/iand675/hs-opentelemetry) (traces/metrics/
logs marked *Stable*; OTLP exporter at 1.0). It gives us exactly the seams our
architecture already has, as middleware rather than hand-rolled spans:

| Package | Role for Écluse |
|---|---|
| `hs-opentelemetry-sdk` | Tracer provider, batching, lifecycle (wired in the composition root). |
| `hs-opentelemetry-instrumentation-wai` | One server span per request — slots into the raw-WAI [middleware stack](web-layer.md#middleware-and-helper-libraries). |
| `hs-opentelemetry-instrumentation-http-client` | Child spans + context propagation on the **data plane** (`http-client`) — the upstream fetches that *are* the proxy's work (see [Control plane vs. data plane](web-layer.md#control-plane-vs-data-plane)). |
| `hs-opentelemetry-exporter-otlp` | OTLP export — **HTTP/protobuf by default**; gRPC is behind a cabal flag (pulls in `grapesy`) and we do not need it. |
| `hs-opentelemetry-propagator-datadog` | **Optional, Datadog-only.** Reads/writes Datadog's `x-datadog-*` trace headers so traces join up with services already running `dd-trace`. Every other backend uses the default W3C TraceContext propagator. |

Only the last row is vendor-specific, and it is optional; everything above it is
backend-agnostic. The pins live with the rest of the dependency choices in
[Technology Stack](technology-stack.md).

## What gets traced

The instrumentation maps onto the [Request Lifecycle](../architecture.md#request-lifecycle):
a server span from the WAI middleware, with child spans for each upstream fetch
(private then public) from the http-client instrumentation. Two domain spans are
worth adding by hand because they carry the decisions an operator cares about:

- **rule evaluation** — attributes for the verdict and, on denial, the
  `RuleName` and `RejectReason` (mirrors the [error model](web-layer.md#error-model)),
  so a 403 is explainable from the trace alone.
- **mirror enqueue** — links the synchronous request to the asynchronous mirror
  job (see [Cloud Backends](cloud-backends.md#cloud-backends)).

Structured logs already flow through `katip`; the integration point is injecting
`trace_id`/`span_id` into log lines (log–trace correlation — a standard OTel
concept that Datadog, Grafana, and others all consume), via the
`hs-opentelemetry` katip bridge. Logs themselves keep going out as structured
JSON for whatever log collector is deployed — we do not route logs over OTLP.

## Datadog deployment: OTLP is TCP; UDS is for metrics

For any plain OTLP backend the deployment is trivial — point
`OTEL_EXPORTER_OTLP_ENDPOINT` at the receiver and you are done. Everything from
here on is the **Datadog-target recipe**, which is more involved only because of
one Datadog-specific fact; consumers on other backends can skip it.

That fact: **the Datadog Agent's OTLP receiver is TCP-only** (gRPC `:4317` / HTTP
`:4318`). Unix sockets are a first-class Datadog transport — but only for two
other intakes, and neither speaks OTLP:

| Provided socket | Speaks | For |
|---|---|---|
| `/var/run/datadog/dsd.socket` | DogStatsD **datagrams** (not HTTP) | metrics |
| `/var/run/datadog/apm.socket` | HTTP-over-UDS, **native DD trace API** (`/v0.4/traces`, msgpack) | traces, in DD's own format |
| OTLP receiver | OTLP (gRPC/HTTP) — **TCP only, no socket** | traces + metrics |

So "send our OTLP exporter at the provided Datadog socket" does not work: pointing
a UDS-dialing OTLP/HTTP client at `apm.socket` connects fine and is then rejected
at the application layer, because that endpoint wants native msgpack, not OTLP.
**Transport and protocol must both match** — swapping the transport to UDS is the
easy half; nothing DD provides accepts OTLP over a socket. Re-implementing DD's
native trace wire format to use `apm.socket` is the only way to put *traces* on
the provided socket, and it is not worth it (it discards the OTLP/OTel ecosystem
to save one TCP hop). We therefore split the two signals by transport.

## Reaching the Agent (Kubernetes daemonset)

- **Traces → OTLP/HTTP over TCP to the node-local Agent.** Inject the node IP
  with the Downward API and target `:4318`; the Agent must enable its OTLP
  receiver bound to `0.0.0.0`:

  ```yaml
  env:
    - name: DD_AGENT_HOST
      valueFrom: { fieldRef: { fieldPath: status.hostIP } }
    - name: OTEL_EXPORTER_OTLP_ENDPOINT
      value: "http://$(DD_AGENT_HOST):4318"
  ```

- **Metrics → DogStatsD over the Unix socket** (`hostPath` mount of
  `/var/run/datadog`). This is a datagram write, not HTTP — no manager, no
  framing:

  ```haskell
  sock <- S.socket S.AF_UNIX S.Datagram S.defaultProtocol
  S.connect sock (S.SockAddrUnix "/var/run/datadog/dsd.socket")
  SBS.sendAll sock "ecluse.tarball.bytes:12345|c|#route:tarball\n"
  ```

The Haskell DogStatsD libraries (`datadog`, latest 0.3.0.0 / 2022) are UDP-first
and stale, and DogStatsD is a trivial line protocol, so a ~30-line UDS datagram
sender is the durable choice over taking a dependency. If we would rather run a
single pipeline and forgo the socket, metrics can instead ride **OTLP over the
same TCP path** as traces — simpler operationally, at the cost of not using UDS.

**Escape hatch — UDS for traces, if a network policy ever forbids the TCP hop:**
the transport bridge is mechanically simple — `http-client` will dial `AF_UNIX`
through a custom `Manager`:

```haskell
unixManagerSettings :: FilePath -> ManagerSettings
unixManagerSettings sockPath = defaultManagerSettings
  { managerRawConnection = pure $ \_host _ _port -> do
      sock <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
      S.connect sock (S.SockAddrUnix sockPath)
      makeConnection (SBS.recv sock 8192) (SBS.sendAll sock) (S.close sock)
  }
```

But because no *provided* socket accepts OTLP, the only sound use of this is a
**sidecar OpenTelemetry Collector** whose OTLP receiver is bound to a socket on a
shared `emptyDir`; the Collector then forwards to Datadog. Both ends speak OTLP,
so the bridge is sufficient — but it adds a sidecar and is *our* socket, not the
daemonset's. We do not adopt it unless forced.

## Configuration

Following the existing `OTEL_*` (read directly by `hs-opentelemetry`) and
`PROXY_*` conventions; see [Configuration](configuration.md). The `OTEL_*`
variables are the **standard, backend-agnostic** set — they are all a non-Datadog
consumer ever needs; only `PROXY_DOGSTATSD_SOCKET` is Datadog-specific. With
`PROXY_TELEMETRY` unset, none of this is touched and no telemetry is emitted.

| Variable | Purpose |
|---|---|
| `PROXY_TELEMETRY` | Master switch (`off` by default — telemetry is opt-in). |
| `OTEL_SERVICE_NAME` | Service name in any APM/trace backend (e.g. `ecluse`). |
| `OTEL_EXPORTER_OTLP_ENDPOINT` | Any OTLP receiver — a Collector, Jaeger, the Datadog Agent (`http://$(DD_AGENT_HOST):4318`), etc. |
| `OTEL_EXPORTER_OTLP_PROTOCOL` | `http/protobuf` (default; avoids the gRPC/grapesy build flag). |
| `OTEL_RESOURCE_ATTRIBUTES` | `deployment.environment`, `service.version`, etc. |
| `PROXY_DOGSTATSD_SOCKET` | *(Datadog-only, optional)* DogStatsD UDS path; unset → metrics disabled (or routed via OTLP to any backend). |

## Verifying it — smoke-test plan

The goal is *full confidence that a span and a metric emitted by Écluse actually
arrive*, not just that the code compiles. It layers onto the existing three-tier
[testing strategy](../../CONTRIBUTING.md), so each concern is proven at the
cheapest tier that can prove it. Tier 1 and the Collector variant of tier 2 are
**backend-agnostic** — they prove the OTLP path any consumer relies on; the
Datadog Agent and live-API checks are the Datadog target carrying its own weight,
not a baseline requirement:

1. **Unit (pure, gating).** The pieces that are pure functions:
   - DogStatsD line formatting — `Metric -> ByteString` must produce the exact
     `name:value|type|#tags` bytes (table-driven).
   - Telemetry config parsing from the env vars above (present/absent/malformed).
   - Span-attribute mapping for a denial (verdict + `RuleName` → attributes).

2. **Integration (Dockerised, `testcontainers`/ministack, the same tier as the
   mirror-queue tests).** Prove the wires carry bytes, with no dependency on the
   Datadog SaaS:
   - **UDS datagram round-trip.** Bind an `AF_UNIX` `SOCK_DGRAM` listener on a
     temp path, run the metrics sender, assert the exact datagram is received.
     This is the highest-risk custom code, and it needs no container at all.
   - **Traces to a real Agent.** Run the **Datadog Agent** container with OTLP
     enabled and a *dummy* API key (intake is accepted locally even though
     forwarding to DD fails). Drive one request through an in-process Écluse,
     then assert the Agent *accepted* the spans via its own diagnostics
     (`agent status` OTLP-receiver counters / `DD_DOGSTATSD_STATS_ENABLE` for the
     metric). This exercises our real exporter → real intake end of the chain
     deterministically and offline.
   - *(Optional)* the sidecar-Collector UDS path: a Collector container with an
     OTLP receiver on a shared-volume socket + a `debug` exporter; point the
     UDS manager at it and assert spans are logged.

3. **Smoke (live, non-gating, scheduled, secret-gated — like the live-registry
   oracle check).** True end-to-end including the Datadog backend: emit a span and
   a metric stamped with a unique `smoke.run_id`, then poll the **Datadog API**
   until that trace/metric appears (or time out). Runs against a sandbox org,
   guarded by repository secrets, never on the PR gate.

The split keeps the gate fast and hermetic (tiers 1–2 need no network and no
Datadog account) while still giving a real, periodic end-to-end signal (tier 3).
