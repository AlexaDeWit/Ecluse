---
id: S24
title: OTel substrate + telemetry config (off by default)
milestone: M6, Observability
status: merged
depends-on: [S01, S03]
test-tier: [unit]
arch-refs:
  - docs/architecture/observability.md#opentelemetry-as-the-substrate
  - docs/architecture/observability.md#configuration
pr: 158
---

# S24, OTel substrate + telemetry config (off by default)

> Milestone **M6** · depends on: [S01](S01-app-env-scaffold.md), [S03](S03-config-loader.md) · tier: unit

**Goal.** Wire the OpenTelemetry substrate into the composition root: the SDK
tracer+meter provider, the OTLP exporter (HTTP/protobuf), and the standard `OTEL_*` /
`ECLUSE_TELEMETRY` configuration, **off by default**, so telemetry is fully opt-in
and nothing is emitted when unset.

**Acceptance criteria.**
- [ ] `hs-opentelemetry-sdk` tracer+meter provider built in the composition root,
  exported via `hs-opentelemetry-exporter-otlp` (HTTP/protobuf default; no gRPC/
  grapesy).  _observability.md#opentelemetry-as-the-substrate_
- [ ] `ECLUSE_TELEMETRY` master switch (default `off`); with it unset **nothing is
  wired and no telemetry is emitted**. Standard `OTEL_*` vars
  (`OTEL_SERVICE_NAME`/`…_RESOURCE_ATTRIBUTES`/`…_EXPORTER_OTLP_ENDPOINT`/`…_PROTOCOL`/
  sampler) read by the SDK.  _observability.md#configuration_
- [ ] Telemetry config parsing unit-tested (present/absent/malformed); the provider
  lifecycle is bracketed in `Env`.

**File scope.**
- `src/Ecluse/Telemetry.hs`, provider construction/lifecycle, config plumbing.
- `src/Ecluse/Env.hs`, `src/Ecluse/Config.hs`, telemetry handle + `ECLUSE_TELEMETRY` (additive).
- `ecluse.cabal`, add `hs-opentelemetry-sdk`, `hs-opentelemetry-exporter-otlp`.
- `test/unit/Ecluse/TelemetrySpec.hs`, config parsing; off-by-default no-op.

**Test tier.** Unit, config + off-by-default behaviour (no exporter network).

**Notes / risks.** Pin `hs-opentelemetry` 1.0 (metrics+logs+traces) in `flake.nix`/
cabal, confirm availability in the pinned package set; **escalate** if the 1.0 pin
is not yet in nixpkgs (a real external dependency on the toolchain). Keep this purely
substrate, spans (S25) and metrics/logs (S26) layer on. Off-by-default is the FOSS
posture: the maintainer's Datadog choice must not become every consumer's obligation.

## As-built notes

- **OTel 1.0 sourcing proved on both paths.** Hackage (index-state
  `2026-06-23T19:20:36Z`) carries the whole 1.0 stack; `cabal.project` pins it
  (`hs-opentelemetry-{api, api-types, sdk, exporter-otlp} ==1.0.*`) and
  `cabal.project.freeze` was regenerated via `make freeze` (sdk/api/api-types/
  exporter-otlp/otlp `1.0.0.0`, semantic-conventions `1.40.0.0`; the
  exporter resolves with its `grpc` flag **off**, HTTP/protobuf only, no
  `grapesy`).
- **Nix path needed an overlay**, since the pinned nixpkgs (26.05) ships only the
  0.x line (sdk `0.1.0.1`, api `0.3.1.0`, and no `hs-opentelemetry-api-types` at
  all). `flake.nix` adds an `otelOverlay` (`callHackageDirect`, version + tarball
  sha256 pinned, no new flake input) bumping the full stack to 1.0: api-types,
  api, otlp, semantic-conventions (1.40), the five propagators (b3/datadog/
  jaeger/w3c/xray), exporter-handle, exporter-in-memory, sdk, exporter-otlp. The
  in-memory exporter and propagators had to move too: the 0.x ones don't compile
  against the 1.0 api. `make nix-check` (callCabal2nix → unit check) is green.
- **Substrate shape.** `Ecluse.Telemetry` exposes a `ECLUSE_TELEMETRY` master
  switch (`TelemetrySwitch`, default `off`) wired into `EnvConfig` (`cfgTelemetry`),
  and a `Telemetry` handle held in `Env` (`envTelemetry`). `withTelemetry` brackets
  the providers in the composition root: `off` is a pure pass-through that never
  initialises the SDK and emits nothing; `on` runs `hs-opentelemetry-sdk`'s
  `withOpenTelemetry`, reading the standard `OTEL_*` env, and exposes the tracer +
  meter providers. The unit tier covers config parsing, the off-by-default no-op,
  and the enabled-handle wiring, `telemetryEnabled` is exercised against /offline/
  providers (an empty-processor tracer provider + the no-op meter provider; no
  exporter opened, no `OTEL_*` read). Only the live `withTelemetry on` path
  (`withOpenTelemetry`, which opens an OTLP exporter) stays integration-tier, per
  the verification plan. `codecov/patch` is 100% (every changed line carries a
  covered tick; the `withOpenTelemetry` lambda body shares its line with the
  covered `TelemetryOn` case-head).
