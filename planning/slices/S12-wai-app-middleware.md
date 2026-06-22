---
id: S12
title: WAI app + meta-routes + middleware + dispatch
milestone: M2 — Web front door
status: not-started
depends-on: [S01, S10, S11]
test-tier: [unit]
arch-refs:
  - docs/architecture/web-layer.md#meta-routes-ping-health-and-search
  - docs/architecture/web-layer.md#middleware-and-helper-libraries
  - docs/architecture/hosting.md#dispatch
  - docs/architecture/cloud-backends.md#process-model
pr: null
---

# S12 — WAI app + meta-routes + middleware + dispatch

> Milestone **M2** · depends on: [S01](S01-app-env-scaffold.md), [S10](S10-router.md), [S11](S11-response-model.md) · tier: unit

**Goal.** The raw-WAI `Application` served by `warp`: mount dispatch, the
meta-routes, the middleware stack, and `runServer` wired into the composition root.
Package/tarball handlers are stubbed against `ServeDecision` until S14/S15 fill the
real pipeline.

**Acceptance criteria.**
- [ ] **Mount dispatch**: match the leading path segment to a mount, strip the
  prefix, hand the remainder to `classify` (S10); accept the prefix with/without a
  trailing slash. — _hosting.md#dispatch_
- [ ] **Meta-routes**: `/-/ping`→200 `{}` (answered locally); `/livez`/`/readyz`
  distinct — liveness reflects the worker heartbeat in single-process mode,
  readiness is **lenient about public-upstream reachability** and (later) gates on
  CVE first-sync (S22); `/-/v1/search`→501 with a pointer message; everything
  unrecognised→404. — _web-layer.md#meta-routes-ping-health-and-search, cloud-backends.md#process-model_
- [ ] **Middleware stack** composed around the app: `RequestSizeLimit`, `RealIp`/
  forwarded-for, `Timeout`, and a thin `katip` logging middleware (S04). **Not**
  `Autohead` or `Gzip` (documented why). — _web-layer.md#middleware-and-helper-libraries_
- [ ] `runServer :: Env -> IO ()` starts warp on the configured port; handlers run
  in plain `IO` taking `Env`. — _cloud-backends.md#process-model_
- [ ] Handlers return `ServeDecision`/responses via S11; the real fetch→rules→serve
  body is deferred to S14/S15 (honest stub returning a clear "not yet wired" path
  for package routes, **not** a fake 200).

**File scope.**
- `src/Ecluse/Server.hs` — the `Application`, dispatch, meta-routes, middleware, `runServer`.
- `src/Ecluse.hs` — wire `runServer` into `run` (additive to S01).
- `ecluse.cabal` — add `warp`, `wai`, `wai-extra`.
- `test/unit/Ecluse/ServerSpec.hs` — `hspec-wai` over the `Application`: ping/health/search/404, dispatch + prefix-strip, middleware (size limit).

**Test tier.** Unit — `hspec-wai` drives the `Application` end-to-end for routing,
meta-routes, and middleware (no upstream network).

**Notes / risks.** Health semantics are subtle: readiness must not flap on a
public-upstream blip (the proxy still serves private hits). Keep the package-route
handler an explicit "wired in S14" path; do **not** return a placeholder success.
The streaming/ETag/cache concerns are S13 — this slice is routing + meta + middleware.
