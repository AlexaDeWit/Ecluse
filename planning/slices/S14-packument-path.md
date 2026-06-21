---
id: S14
title: Packument path end-to-end (walking skeleton)
milestone: M3 — Request pipeline
status: not-started
depends-on: [S08, S09, S13]
test-tier: [unit]
arch-refs:
  - docs/architecture.md#request-lifecycle
  - docs/architecture/registry-model.md#credential-flow-and-authority
  - docs/architecture/web-layer.md#control-plane-vs-data-plane
  - docs/architecture/rules-engine.md#applying-verdicts-to-a-packument
pr: null
---

# S14 — Packument path end-to-end (**walking skeleton**)

> Milestone **M3** · depends on: [S08](S08-npm-data-plane.md), [S09](S09-npm-rewrite-filter.md), [S13](S13-streaming-cache.md) · tier: unit

**Goal.** Close the thinnest end-to-end path: a `GET /{pkg}` packument request flows
router → three-registry fetch (private→public fallback) → parse → rules → filter →
serve, with the credential authority model enforced. Cloud seams use the S02
in-memory doubles; real AWS backends arrive in M4.

**Acceptance criteria.**
- [ ] **Three-registry fetch**: try the **private upstream** first; on a non-2xx
  miss, fall back to the **public upstream**. A private hit is served **unfiltered**
  (already vetted). — _architecture.md#request-lifecycle, registry-model.md_
- [ ] **Credential authority**: the client's `Authorization`/`_authToken` is
  **forwarded to the private upstream** and **stripped before any public-upstream
  fetch**; the public leg is anonymous. Pin this with a test — it is the
  non-negotiable invariant. — _registry-model.md#credential-flow-and-authority, web-layer.md#control-plane-vs-data-plane_
- [ ] **Public path**: fetch (full packument for `time`) → `parsePackageInfo` →
  evaluate rules per version → `filterPackument` (S09) → rewrite tarball URLs (S09)
  → serve with our **own ETag** (S13). — _rules-engine.md#applying-verdicts-to-a-packument_
- [ ] **No survivors** → 403 (all by-policy) or 503 (any transient, once S21 lands);
  never 404 when the package exists. — _rules-engine.md#applying-verdicts-to-a-packument_
- [ ] Optional inbound `PROXY_AUTH_TOKEN` validated at the edge before proxying (S03).
- [ ] Uses the metadata cache (S13) so the fetch+parse is shared/collapsed.

**File fence.**
- `src/Ecluse/Server/Pipeline.hs` — the packument handler: fetch orchestration, credential forward/strip, rules+filter+serve.
- `src/Ecluse/Server.hs` — wire the packument route to the pipeline (replace the S12 "wired in S14" stub).
- `test/unit/Ecluse/Server/PipelineSpec.hs` — `hspec-wai` with an in-process upstream double: private-hit-unfiltered, public-fallback-filtered, credential forward/strip, no-survivors 403.

**Test tier.** Unit — full fetch→parse→rules→filter→serve asserted against an
in-process WAI stub standing in for both upstreams (no network), per CONTRIBUTING's
testing strategy.

**Notes / risks.** **This is the walking skeleton** — once it merges, the system
proxies a filtered packument end to end. The credential strip-before-public test is
the single most important security assertion in this slice. Keep the handler in plain
`IO` taking `Env` (no transformer lifting on the hot path). Mirror enqueue is **not**
here (packument requests don't mirror) — that is the tarball path, S15.
