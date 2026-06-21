---
id: S26
title: ecluse.* metrics + JSONL dd correlation
milestone: M6 — Observability
status: not-started
depends-on: [S04, S24]
test-tier: [unit, integration]
arch-refs:
  - docs/architecture/observability.md#metrics
  - docs/architecture/observability.md#cardinality-and-attributes
  - docs/architecture/observability.md#logs
pr: null
---

# S26 — `ecluse.*` metrics + JSONL `dd` correlation

> Milestone **M6** · depends on: [S04](S04-logging-katip.md), [S24](S24-otel-substrate.md) · tier: unit, integration

**Goal.** Emit the domain metrics catalog over the same OTLP pipeline (with a
Prometheus-scrape alternative), under a strict bounded-label discipline, and stitch
logs to traces via trace-ID injection into the JSONL `dd` object.

**Acceptance criteria.**
- [ ] The `ecluse.*` metric catalog (serve decision, rule denials/eval-duration/
  effectful-failures/breaker-state, advisory sync age/failures/last-good, upstream
  fetch, metadata-cache, mirror, credential refresh/ttl) plus OTel HTTP semantic
  conventions. — _observability.md#metrics_
- [ ] **Bounded-label discipline**: metric labels are a closed set of bounded enums
  only; high-cardinality identifiers (package/version/scope/message) never become
  labels — a label-domain guard test rejects an unbounded label. — _observability.md#cardinality-and-attributes_
- [ ] Transport: OTLP by default; `OTEL_METRICS_EXPORTER=prometheus` selects the
  scrape endpoint. — _observability.md#metrics_
- [ ] **Logs ↔ traces**: katip JSONL (S04) gains a populated `dd` object
  (`trace_id`/`span_id`/`service`/`env`/`version`) in the id format Datadog expects;
  one compact line per record. — _observability.md#logs_

**File fence.**
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
