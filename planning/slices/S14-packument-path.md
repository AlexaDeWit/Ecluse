---
id: S14
title: Packument path end-to-end (walking skeleton)
milestone: M3 — Request pipeline
status: not-started
depends-on: [S08, S09, S13, S33]
test-tier: [unit]
arch-refs:
  - docs/architecture.md#request-lifecycle
  - docs/architecture/registry-model.md#credential-flow-and-authority
  - docs/architecture/registry-model.md#packument-merge-across-upstreams
  - docs/architecture/web-layer.md#control-plane-vs-data-plane
  - docs/architecture/rules-engine.md#applying-verdicts-to-a-packument
pr: null
---

# S14 — Packument path end-to-end (**walking skeleton**)

> Milestone **M3** · depends on: [S08](S08-npm-data-plane.md), [S09](S09-npm-rewrite-filter.md), [S13](S13-streaming-cache.md), [S33](S33-packument-merge.md) · tier: unit

**Goal.** Close the thinnest end-to-end path: a `GET /{pkg}` packument request flows
router → **parallel multi-upstream fetch** → parse → (gate public / trust private) →
**merge** (S33) + filter (S09) → serve, with the credential authority model
enforced. The packument is **merged across upstreams, not a private-then-public
fallback** (see registry-model.md#packument-merge-across-upstreams). Cloud handles use
the S02 in-memory doubles; real AWS backends arrive in M4.

**Acceptance criteria.**
- [ ] **Multi-upstream merge**: fetch the **private and public upstreams in
  parallel**; trust private versions (unfiltered) and gate public versions (rules);
  **merge** into one document via S33 (private wins on collision; integrity
  divergence flagged). No private-hit short-circuit. —
  _registry-model.md#packument-merge-across-upstreams, architecture.md#request-lifecycle_
- [ ] **Credential authority**: the client's `Authorization`/`_authToken` is
  **forwarded to the private upstream** and **stripped before any public-upstream
  fetch**; the public leg is anonymous. Pin this with a test — it is the
  non-negotiable invariant. — _registry-model.md#credential-flow-and-authority, web-layer.md#control-plane-vs-data-plane_
- [ ] **Public set gated**: fetch (full packument for `time`) → `parsePackageInfo`
  → evaluate rules per version → `filterPackument` (S09) → rewrite tarball URLs
  (S09); the filtered public set is merged (S33) with the trusted private set and
  served with our **own ETag** (S13). — _rules-engine.md#applying-verdicts-to-a-packument_
- [ ] **No survivors in the merge** → 403 (all by-policy) or 503 (any
  transient/undecidable once S21 lands, **or a needed upstream unavailable**); never
  404 when the package exists. **Partial-upstream availability**: one upstream
  failing while another succeeds serves the best-effort union, not an error. —
  _rules-engine.md#applying-verdicts-to-a-packument, registry-model.md#packument-merge-across-upstreams_
- [ ] Optional inbound `PROXY_AUTH_TOKEN` validated at the edge before proxying (S03).
- [ ] Uses the metadata cache (S13) so the fetch+parse is shared/collapsed.

**File fence.**
- `src/Ecluse/Server/Pipeline.hs` — the packument handler: fetch orchestration, credential forward/strip, rules+filter+serve.
- `src/Ecluse/Server.hs` — wire the packument route to the pipeline (replace the S12 "wired in S14" stub).
- `test/unit/Ecluse/Server/PipelineSpec.hs` — `hspec-wai` with in-process upstream doubles: private+public merged, public-gated/private-trusted, collision→private-wins + divergence flagged, credential forward/strip, partial-upstream union, no-survivors 403.

**Test tier.** Unit — full fetch→parse→rules→filter→serve asserted against an
in-process WAI stub standing in for both upstreams (no network), per CONTRIBUTING's
testing strategy.

**Notes / risks.** **This is the walking skeleton** — once it merges, the system
proxies a merged, filtered packument end to end. The credential strip-before-public test is
the single most important security assertion in this slice. Keep the handler in plain
`IO` taking `Env` (no transformer lifting on the hot path). Mirror enqueue is **not**
here (packument requests don't mirror) — that is the tarball path, S15.
