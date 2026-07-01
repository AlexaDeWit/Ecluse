---
id: S14
title: Packument path end-to-end (walking skeleton)
milestone: M3, Request pipeline
status: merged
depends-on: [S08, S09, S13, S33]
test-tier: [unit]
arch-refs:
  - docs/architecture.md#request-lifecycle
  - docs/architecture/registry-model.md#credential-flow-and-authority
  - docs/architecture/registry-model.md#packument-merge-across-upstreams
  - docs/architecture/web-layer.md#control-plane-vs-data-plane
  - docs/architecture/rules-engine.md#applying-verdicts-to-a-packument
pr: 109
---

# S14, Packument path end-to-end (**walking skeleton**)

> Milestone **M3** · depends on: [S08](S08-npm-data-plane.md), [S09](S09-npm-rewrite-filter.md), [S13](S13-streaming-cache.md), [S33](S33-packument-merge.md) · tier: unit

**Goal.** Close the thinnest end-to-end path: a `GET /{pkg}` packument request flows
router → **parallel multi-upstream fetch** → parse → (gate public / trust private) →
**merge** (S33) + filter (S09) → serve, with the credential authority model
enforced. The packument is **merged across upstreams, not a private-then-public
fallback** (see registry-model.md#packument-merge-across-upstreams). Cloud handles use
the S02 in-memory doubles; real AWS backends arrive in M4.

**Acceptance criteria.**
- [x] **Multi-upstream merge**: fetch the **private and public upstreams in
  parallel**; trust private versions (unfiltered) and gate public versions (rules);
  **merge** into one document via S33 (private wins on collision; integrity
  divergence flagged). No private-hit short-circuit.   _registry-model.md#packument-merge-across-upstreams, architecture.md#request-lifecycle_
- [x] **Credential authority** (the default [`passthrough`](../../docs/architecture/access-model.md)
  strategy this slice ships): the client's `Authorization`/`_authToken` is
  **forwarded to the private upstream** and **stripped before any public-upstream
  fetch**; the public leg is anonymous. Pin this with a test. The invariant that
  holds under *every* strategy is the **public strip**; forwarding to the private
  upstream is specifically the `passthrough` behaviour.  _registry-model.md#credential-flow-and-authority, access-model.md, web-layer.md#control-plane-vs-data-plane_
- [x] **Public set gated**: fetch (full packument for `time`) → `parsePackageInfo`
  → evaluate rules per version → `filterPackument` (S09) → rewrite tarball URLs
  (S09); the filtered public set is merged (S33) with the trusted private set and
  served with our **own ETag** (S13).  _rules-engine.md#applying-verdicts-to-a-packument_
- [x] **No survivors in the merge** → 403 (all by-policy) or 503 (any
  transient/undecidable once S21 lands, **or a needed upstream unavailable**); never
  404 when the package exists. **Partial-upstream availability**: one upstream
  failing while another succeeds serves the best-effort union, not an error.   _rules-engine.md#applying-verdicts-to-a-packument, registry-model.md#packument-merge-across-upstreams_
- [x] Optional inbound `ECLUSE_AUTH_TOKEN` validated at the edge before proxying (S03).
- [x] Uses the metadata cache (S13) so the fetch+parse is shared/collapsed (the
  **public** leg only; see as-built notes).

**File scope.**
- `src/Ecluse/Server/Pipeline.hs`, the packument handler: fetch orchestration, credential forward/strip, rules+filter+serve.
- `src/Ecluse/Server.hs`, wire the packument route to the pipeline (replace the S12 "wired in S14" stub).
- `test/unit/Ecluse/Server/PipelineSpec.hs`, `hspec-wai` with in-process upstream doubles: private+public merged, public-gated/private-trusted, collision→private-wins + divergence flagged, credential forward/strip, partial-upstream union, no-survivors 403.

**Test tier.** Unit, full fetch→parse→rules→filter→serve asserted against an
in-process WAI stub standing in for both upstreams (no network), per CONTRIBUTING's
testing strategy.

**Notes / risks.** **This is the walking skeleton**, once it merges, the system
proxies a merged, filtered packument end to end. The credential strip-before-public test is
the single most important security assertion in this slice. Keep the handler in plain
`IO` taking `Env` (no transformer lifting on the hot path). Mirror enqueue is **not**
here (packument requests don't mirror), that is the tarball path, S15.

**As-built deltas.**
- **`PackumentDeps` as an explicit handler input.** `Env` carries one
  `RegistryClient` and no upstream URLs / rule policy (config → `Env` wiring is
  S20). The handler `servePackument :: PackumentDeps -> Env -> …` takes its
  mount-level inputs (private/public base URLs, mount base URL, resolved
  `[PrecededRule]`, optional inbound token, `IO UTCTime` clock, help message) as a
  record. The two legs are built per-request as `NpmClientConfig`s over the shared
  `Manager`, private with the client's forwarded token, public anonymous, so the
  credential authority lives in one place. Fetched in parallel via
  `UnliftIO.concurrently`.
- **Decision-surface replay.** Each leg yields `(PackageInfo, raw Value)`. Public is
  gated (`filterPackument` over its raw `Value`); private is trusted as-is. The two
  typed `PackageInfo`s feed `mergePackuments` → `MergePlan`; the served body is built
  by taking each surviving version's object from the raw `Value` of its winning
  `SourceId`, carrying the plan's reconciled `dist-tags`/`time` (with `time`'s
  `created`/`modified` bookkeeping relayed from the winning doc), relaying every other
  top-level key, and rewriting tarball URLs (S09). The typed model is never
  re-serialised. `time` values are re-rendered from the plan's `UTCTime` as ISO-8601
  (the merge owns `time` as a typed decision; integrity-bearing fields are relayed raw).
- **Cache is the public leg only.** S13's `MetadataCache` keys on **package
  identity**, but a packument is fetched from two distinct upstreams whose documents
  differ, sharing one entry across both legs cross-contaminates them. The **public**
  leg owns the cache entry (it is the gated set the tarball path also resolves on a
  private miss, so the cache reuses one fetch+parse across both paths); the trusted
  **private** leg is fetched uncached. See escalation: a per-source cache key is the
  proper fix and belongs to S13/S15.
- **Server wiring.** `ServerConfig` gains `scPackumentDeps :: Mount -> Maybe
  PackumentDeps` (default `noPackumentDeps`, leaving the route the S12 `501` stub);
  `dispatchMount` now returns the matched mount so its deps reach the handler. `Mount`
  is unchanged (kept `Eq`/`Show`), so the function-valued deps do not infect it. S20
  supplies real per-mount deps from the resolved mount map.

**Reconciliation (post-merge).** Three notes above have since been overtaken, the
code is being brought into line by refactors and the base-hardening track:
- The "**cache is the public leg only / per-source key belongs to S13/S15**"
  escalation was **resolved by #111 / #113**: the metadata cache now keys per source
  and stores the raw document, so the trusted private leg is cacheable (kept uncached
  only under `passthrough`, per #115 / #117 and
  [access-model → Caching](../../docs/architecture/access-model.md#caching)).
- "`Mount` is unchanged" is **superseded by #122 / #133**: the per-mount unit is now
  `MountBinding` (`bindingPrefix :: NonEmpty Text`, no root mount), carrying its
  classifier, packument deps and error renderer.
- The plain-`IO`-taking-`Env` handler and the explicit `PackumentDeps` threading are
  **superseded by the base-hardening Reader migration**
  (base-hardening track): handlers now run in
  `ReaderT RequestCtx IO`, reading per-mount deps from `RequestCtx`. See
  [technology-stack.md → Key Decisions](../../docs/architecture/technology-stack.md#key-decisions).
