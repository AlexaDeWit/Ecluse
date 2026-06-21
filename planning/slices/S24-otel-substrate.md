---
id: S24
title: OTel substrate + telemetry config (off by default)
milestone: M6 — Observability
status: not-started
depends-on: [S01, S03]
test-tier: [unit]
arch-refs:
  - docs/architecture/observability.md#opentelemetry-as-the-substrate
  - docs/architecture/observability.md#configuration
pr: null
---

# S24 — OTel substrate + telemetry config (off by default)

> Milestone **M6** · depends on: [S01](S01-app-env-scaffold.md), [S03](S03-config-loader.md) · tier: unit

**Goal.** Wire the OpenTelemetry substrate into the composition root: the SDK
tracer+meter provider, the OTLP exporter (HTTP/protobuf), and the standard `OTEL_*` /
`PROXY_TELEMETRY` configuration — **off by default**, so telemetry is fully opt-in
and nothing is emitted when unset.

**Acceptance criteria.**
- [ ] `hs-opentelemetry-sdk` tracer+meter provider built in the composition root,
  exported via `hs-opentelemetry-exporter-otlp` (HTTP/protobuf default; no gRPC/
  grapesy). — _observability.md#opentelemetry-as-the-substrate_
- [ ] `PROXY_TELEMETRY` master switch (default `off`); with it unset **nothing is
  wired and no telemetry is emitted**. Standard `OTEL_*` vars
  (`OTEL_SERVICE_NAME`/`…_RESOURCE_ATTRIBUTES`/`…_EXPORTER_OTLP_ENDPOINT`/`…_PROTOCOL`/
  sampler) read by the SDK. — _observability.md#configuration_
- [ ] Telemetry config parsing unit-tested (present/absent/malformed); the provider
  lifecycle is bracketed in `Env`.

**File fence.**
- `src/Ecluse/Telemetry.hs` — provider construction/lifecycle, config plumbing.
- `src/Ecluse/Env.hs`, `src/Ecluse/Config.hs` — telemetry handle + `PROXY_TELEMETRY` (additive).
- `ecluse.cabal` — add `hs-opentelemetry-sdk`, `hs-opentelemetry-exporter-otlp`.
- `test/unit/Ecluse/TelemetrySpec.hs` — config parsing; off-by-default no-op.

**Test tier.** Unit — config + off-by-default behaviour (no exporter network).

**Notes / risks.** Pin `hs-opentelemetry` 1.0 (metrics+logs+traces) in `flake.nix`/
cabal — confirm availability in the pinned package set; **escalate** if the 1.0 pin
is not yet in nixpkgs (a real external dependency on the toolchain). Keep this purely
substrate — spans (S25) and metrics/logs (S26) layer on. Off-by-default is the FOSS
posture: the maintainer's Datadog choice must not become every consumer's obligation.
