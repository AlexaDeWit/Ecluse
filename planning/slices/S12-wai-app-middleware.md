---
id: S12
title: WAI app + meta-routes + middleware + dispatch
milestone: M2 — Web front door
status: merged
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

**As-built notes.**

- **Server config is local, not `Ecluse.Config`.** S12 does not depend on S03 and
  `Env` (S01) carries no port/mounts (its growth stays additive — caches in S13,
  composition-root config wiring in S20). So `Ecluse.Server` introduces a small
  local `ServerConfig` (port + `[Mount]` + `RequestSizeLimit`); `application ::
  ServerConfig -> Env -> Application` is the testable entry point, and `runServer :: Env
  -> IO ()` uses `defaultServerConfig` (port **4873** — the documented
  `PROXY_PORT` default — and a single root mount). S20 supplies the real port and
  resolved `MountMap` at the composition root without changing this signature.
- **Mount dispatch** matches the request's leading path segment(s) to a `Mount`
  prefix, strips it (accepting a bare-prefix trailing slash), and hands the
  remainder to S10's `classify`; an unmatched mount classifies as-is and so denies
  by default (404). Root mount = empty prefix.
- **Meta-routes split by layer:** `/livez` and `/readyz` are control-plane probes
  matched at the top level (above any mount), each `200` and deliberately distinct
  (liveness will consult the worker heartbeat once S19 carries one; readiness stays
  lenient about public-upstream reachability). `/-/ping` (`200 {}`) and
  `/-/v1/search` (`501`) are ecosystem-native and matched by `classify` after
  mount-strip. `Packument`/`Tarball` return an explicit `501` "not yet served"
  (honest stub; the real pipeline is S14/S15), never a fake `200`.
- **Middleware** = `RequestSizeLimit` (25 MiB default) ∘ `RealIp` ∘ `Timeout`
  (60 s); `Autohead`/`Gzip` deliberately excluded (documented in
  `serverMiddleware`). The size-limit middleware rejects only once a handler reads
  the body, which no S12 handler does, so the size-limit test drives a body-reading
  app via `Network.Wai.Test` with a `ChunkedBody` request (hspec-wai's `request`
  fixes `requestBodyLength` at a known zero and cannot reach the check).
- **Out-of-scope test follow-through (flagged for review).** Making `runServer`
  start `warp` (and `run` run it concurrently) means both now **block** rather than
  return, which invalidated two pre-existing S01 assertions that asserted they
  return (`EnvSpec` "runServer … returns", `EcluseSpec` `run`). Those two specs
  (outside this slice's stated file scope) were updated to assert the server
  *starts and keeps serving under a short timeout* — the correct test for a
  blocking listener. The routing/meta/middleware behaviour itself is covered
  socket-free in `Ecluse.ServerSpec`.

**Reconciliation (post-merge).** The "single root mount" / "root mount = empty
prefix" model here is **superseded by #122 / #133 (mandatory path-mounting)**: every
registry is path-mounted, a root mount is now unrepresentable
(`bindingPrefix :: NonEmpty Text`), and the per-mount unit is a `MountBinding`
carrying its prefix, classifier, packument deps and error renderer as one
composition-root-wired unit. The base-hardening track
([`design-queue.md`](../design-queue.md) D1 / D5) keys the mount map by **ecosystem**
and derives the prefix from it; the env-only single mount defaults to npm → `/npm`.
See [hosting.md → Mounts](../../docs/architecture/hosting.md#mounts).
