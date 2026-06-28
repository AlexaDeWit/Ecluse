---
id: S25
title: WAI/http-client + domain spans
milestone: M6 — Observability
status: merged
depends-on: [S12, S19, S24]
test-tier: [unit, integration]
arch-refs:
  - docs/architecture/observability.md#what-gets-traced
  - docs/architecture/observability.md#sampling
  - docs/architecture/observability.md#verifying-it--smoke-test-plan
pr: 293
---

# S25 — WAI/http-client + domain spans

> **As-built (merged, [#293](https://github.com/AlexaDeWit/Ecluse/pull/293)).**
> - WAI server span wired into the S12 middleware stack as the outermost layer via
>   `Ecluse.Server.tracedApplication` (`runServer` uses it); http-client child spans
>   + W3C context propagation come from instrumenting the two data-plane `Manager`s
>   at the composition root (`Ecluse.run`), gated on telemetry being enabled so the
>   data plane is untouched when off. The instrumentation packages drive the
>   process-global tracer/meter/propagator the S24 substrate installs through
>   `withOpenTelemetry` (`initializeGlobalTracerProvider` sets the global propagator),
>   so the handle's provider and the instrumentation's globals are one and the same.
> - Domain spans (`Ecluse.Telemetry.Tracing`): **rule evaluation** (`ecluse.rule.eval`,
>   on the single-version tarball gate where a denial → 403 is explainable from the
>   trace; carries the verdict and, on denial, the rule name + reason class + message),
>   **mirror enqueue** (`ecluse.mirror.enqueue`, a Producer span on the serve-time
>   enqueue), **mirror worker job** (`ecluse.mirror.job`, a Consumer span around
>   `processJob`, outcome label + Error status on a failed/dropped job). The pure
>   verdict→attribute mapping (`ruleVerdictFields`) is unit-tested.
> - Secret scrubbing is load-bearing and proven by dedicated unit tests driving a real
>   `Authorization: Bearer …` request through both the instrumented http-client
>   `Manager` and the WAI middleware against an in-memory span exporter, asserting the
>   token appears in no captured span attribute (the default instrumentation config
>   records no headers).
> - Sampling is head-based and always-on by default (the SDK default
>   `parentbased_always_on`), with the standard `OTEL_TRACES_SAMPLER` lever read
>   directly by the SDK, no code in this slice overrides it.
> - **Deferred (tracked in #307):** the *advisory-sync* domain span lands with S22 (the
>   CVE sync module does not exist yet). True cross-async **span links** between the
>   enqueue span and the worker-job span need trace context carried on the `MirrorJob`
>   payload (the shared queue type, out of this slice's scope); the two spans currently
>   correlate by package@version attributes and are deferred for the link itself.
> - Deps added to the pinned set: `hs-opentelemetry-instrumentation-wai`,
>   `-http-client`, and the transitive `-instrumentation-conduit`, all 1.0.0.0 (cabal
>   freeze + the flake OTel overlay).

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

**File scope.**
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
