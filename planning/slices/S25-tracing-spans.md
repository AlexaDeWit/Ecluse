---
id: S25
title: WAI/http-client + domain spans
milestone: M6 — Observability
status: not-started
depends-on: [S12, S19, S24]
test-tier: [unit, integration]
arch-refs:
  - docs/architecture/observability.md#what-gets-traced
  - docs/architecture/observability.md#sampling
  - docs/architecture/observability.md#verifying-it--smoke-test-plan
pr: null
---

# S25 — WAI/http-client + domain spans

> Milestone **M6** · depends on: [S12](S12-wai-app-middleware.md), [S19](S19-mirror-worker.md), [S24](S24-otel-substrate.md) · tier: unit, integration

**Goal.** Instrument the request lifecycle: a server span from the WAI middleware,
child spans for each upstream fetch (http-client instrumentation), and hand-added
domain spans carrying the decisions an operator cares about — with secrets scrubbed.

**Acceptance criteria.**
- [ ] WAI instrumentation slots into the middleware stack (S12) for one server span
  per request; http-client instrumentation adds child spans + context propagation on
  the **data plane** (private/public fetches). — _observability.md#what-gets-traced_
- [ ] Domain spans: **rule evaluation** (verdict; on denial the `RuleName` +
  `RejectReason`), **mirror enqueue**, **mirror worker job**, **advisory sync** —
  linked appropriately. — _observability.md#what-gets-traced_
- [ ] Head-based sampling: always-on by default when telemetry is enabled (rare
  denial/error traces never missed); `OTEL_TRACES_SAMPLER` ratio lever honoured. —
  _observability.md#sampling_
- [ ] **Secret scrubbing**: the forwarded client token and any `Authorization` are
  scrubbed from anything the WAI/http-client instrumentation might capture. —
  _observability.md#cardinality-and-attributes_
- [ ] Integration: drive a request through an in-process Écluse into a real Agent /
  OTLP Collector container and assert spans were accepted. — _observability.md#verifying-it--smoke-test-plan_

**File fence.**
- `src/Ecluse/Telemetry/Tracing.hs` — domain-span helpers + attribute mapping.
- `src/Ecluse/Server.hs`, `src/Ecluse/Server/Pipeline.hs`, `src/Ecluse/Worker.hs`, `src/Ecluse/Cve/Sync.hs` — add spans (additive).
- `ecluse.cabal` — add `hs-opentelemetry-instrumentation-wai`, `-http-client`.
- `test/unit/...` — span-attribute mapping for a denial; token-scrub assertion.
- `test/integration/Ecluse/TelemetrySpec.hs` — spans accepted by an Agent/Collector container.

**Test tier.** Unit (attribute mapping + scrubbing, gating) + integration (real
collector accepts spans, gating; no Datadog SaaS).

**Notes / risks.** The secret-scrub assertion is load-bearing (no token in any
signal). High-cardinality identifiers (package/version/scope) belong on spans/logs,
**never** metric labels (that discipline is S26). Keep instrumentation additive and
inert when telemetry is off (S24).
