---
id: S26
title: ecluse.* metrics + JSONL dd correlation
milestone: M6 — Observability
status: in-progress
depends-on: [S04, S24]
test-tier: [unit, integration]
arch-refs:
  - docs/architecture/observability.md#metrics
  - docs/architecture/observability.md#cardinality-and-attributes
  - docs/architecture/observability.md#logs
pr: 296
---

# S26 — `ecluse.*` metrics + JSONL `dd` correlation

> Milestone **M6** · depends on: [S04](S04-logging-katip.md), [S24](S24-otel-substrate.md) · tier: unit, integration

**Goal.** Emit the domain metrics catalogue over the same OTLP pipeline (with a
Prometheus-scrape alternative), under a strict bounded-label discipline, and stitch
logs to traces via trace-ID injection into the JSONL `dd` object.

**Acceptance criteria.**
- [ ] The `ecluse.*` metric catalogue (serve decision, rule denials/eval-duration/
  effectful-failures/breaker-state, advisory sync age/failures/last-good, upstream
  fetch, metadata-cache, mirror, credential refresh/ttl) plus OTel HTTP semantic
  conventions. — _observability.md#metrics_
- [ ] **Bounded-label discipline**: metric labels are a closed set of bounded enums
  only; high-cardinality identifiers (package/version/scope/message) never become
  labels — a label-domain guard test rejects an unbounded label. — _observability.md#cardinality-and-attributes_
- [ ] Transport: **OTLP push to the Datadog Agent's OTLP receiver is the launch
  transport** (the already-pinned `hs-opentelemetry-exporter-otlp`; the Agent
  auto-maps OTLP → Datadog metric format). A Prometheus `/metrics` scrape is a
  deferred pull alternative ([#288](https://github.com/AlexaDeWit/Ecluse/issues/288)):
  the SDK honours `OTEL_METRICS_EXPORTER=prometheus` but the pinned set ships no
  scrape-endpoint renderer, so the actual endpoint is out of scope here.
  DogStatsD is out (no maintained GHC 9.10 client). — _observability.md#metrics_
- [ ] **Logs ↔ traces**: katip JSONL (S04) gains a populated `dd` object
  (`trace_id`/`span_id`/`service`/`env`/`version`) in the id format Datadog expects;
  one compact line per record. — _observability.md#logs_

**File scope.**
- `src/Ecluse/Telemetry/Metrics.hs` — instrument definitions + the bounded-label types.
- `src/Ecluse/Log.hs` — `dd`-object injection (additive to S04).
- call sites (pipeline/worker/cve/credential) — emit metrics (additive).
- `test/unit/...` — label-domain guard; JSONL `dd` shape + id format.
- `test/integration/Ecluse/TelemetrySpec.hs` — Prometheus `/metrics` scrape asserts expected series/labels; Agent accepts metrics.

**Test tier.** Unit (label guard + JSONL, gating) + integration (Prometheus scrape +
Agent metric intake, gating).

**Notes / risks.** Cardinality is the failure mode for an inline proxy seeing
thousands of packages — the label-domain guard is the safeguard; **any PR adding an
unbounded label is rejected**. Queue backlog/DLQ depth are cloud-native metrics — do
**not** re-emit them. The `dd.trace_id` id-format detail is the one fiddly
correlation gotcha (verify against the Agent). Exemplars are deferred.

## As-built

The architect ratified a **Datadog-Agent-first, OTLP-push** transport (no Prometheus
exporter dependency, no DogStatsD). The slice is delivered as **two stacked PRs**, the
substrate-config part split out per the orchestration's stacked-PR pattern:

- **PR1 — telemetry substrate config (this PR).** A small, pure, self-aligning
  **config resolver** (`Ecluse.Telemetry.Resolve`): a bounded precedence table over
  four fields — `service.name`, `deployment.environment`, `service.version`, OTLP
  endpoint — resolved **DD-value-wins → vanilla OTEL → default**
  (`DD_SERVICE`/`DD_ENV`/`DD_VERSION`/`DD_AGENT_HOST` over `OTEL_SERVICE_NAME` /
  `OTEL_RESOURCE_ATTRIBUTES` / `OTEL_EXPORTER_OTLP_ENDPOINT`, default `ecluse` /
  `http://localhost:4318`). The resolved identity is the single source of truth for
  both the SDK (via env normalization) and the `dd` log object (PR2). `DD_API_KEY` /
  `DD_SITE` are deliberately **not** read (no agentless SaaS auto-egress). Plus
  **export-failure handling** (absent endpoint → default + one boot warning; SDK export
  failures routed through katip, throttled, via the SDK's settable global error
  handler). The OTLP endpoint is normalized and used as declared, not classified.
- **PR2 — `ecluse.*` catalogue + bounded-label guard + `dd` correlation** (AC1/AC2/AC4),
  stacked on PR1.

**Mechanism decisions (verified against the pinned SDK):**
- *Config feed = env normalization, not programmatic SDK config.* The pinned
  `hs-opentelemetry-sdk` `withOpenTelemetry` (which S25's tracer/propagator setup also
  rides) is env-driven; a programmatic `createFromConfig` path exists but would change
  the substrate lifecycle out from under S25. So the resolved values are written to the
  canonical `OTEL_*` env before `withTelemetry` — surgical and dialect-agnostic.
- *Export-failure routing.* `OpenTelemetry.Internal.Logging.setGlobalErrorHandler ::
  (String -> IO ()) -> IO ()` is settable and compatible with `withOpenTelemetry`, so
  no exporter wrapping / programmatic path is needed.
- *Public-egress guard removed (architect decision).* An earlier revision classified the
  resolved OTLP endpoint against the data-plane internal-range check and fail-booted a
  public endpoint unless `PROXY_TELEMETRY_ALLOW_PUBLIC_EGRESS=true`. That was removed: the
  internal-range classifier is an **SSRF control for the untrusted package-download path**,
  where the target is upstream-supplied; the OTLP endpoint — like the mirror-queue endpoint
  — is an **operator-declared destination**, not an attack surface, so classifying it is
  over-reach. The only real footgun (agentless export to a vendor's SaaS) is already
  excluded structurally — `DD_API_KEY`/`DD_SITE` are never read — so the endpoint is always
  explicitly declared. `prepareTelemetry` now collapses to *resolve identity → normalize
  `OTEL_*` → install the throttled error handler*; the `PROXY_TELEMETRY_ALLOW_PUBLIC_EGRESS`
  knob and the `Ecluse.Security` coupling are gone.

**Deferrals.**
- **Advisory-sync metrics** (`ecluse.advisory.sync.*`) are deferred: the CVE/OSV sync
  module (S22) is not built — there is no `src/Ecluse/Cve/`. Not stubbed.
- **Prometheus `/metrics` scrape endpoint** (old AC3) is deferred to #288; the pinned
  OTel set ships no scrape-endpoint renderer.
- **Exemplars** remain deferred (sampling not wired; 1.0 metrics SDK emission unconfirmed).
