---
id: S34
title: Capability manifest (OpenAPI) — static generation + docs publish
milestone: M2 — Web front door
status: not-started
depends-on: [S03, S12, S14]
test-tier: [unit]
arch-refs:
  - docs/architecture/api-surface.md
  - docs/architecture/web-layer.md#capability-manifest
  - docs/architecture/hosting.md#capability-manifest
pr: 427
---

# S34 — Capability manifest (OpenAPI) — static generation + docs publish

> Milestone **M2** · depends on: [S03](S03-config-loader.md), [S12](S12-wai-app-middleware.md), [S14](S14-packument-path.md) · tier: unit
>
> **Not on the AWS-launch critical path.** A discoverability artifact; it can land
> any time after its dependencies. It does **not** gate M3/M4.

**Goal.** Emit Écluse's **capability manifest** — an OpenAPI 3 document describing
*which registry protocols this server speaks and exactly what is / isn't supported* —
as a **statically generated build-time artifact**, render it to a browsable static
page, and **publish it to GitHub Pages alongside the Haddock** through the existing
docs pipeline. See [api-surface.md](../../docs/architecture/api-surface.md) for the
full rationale (a capability manifest, **not** a client-integration contract).

> **Revised (2026-06-27, architect).** The manifest is **statically generated and
> published, not served.** The server does **not** expose a runtime `GET /openapi.json`
> endpoint — there is no control-plane meta-route and no WAI wiring. The document is
> produced at build time from a fixed canonical config and shipped as static content
> on the docs site, exactly like the rendered Markdown and the Haddock. This removes
> `/openapi.json` from M2's web front door.

> **As-built (PR 1 of the S34 delivery — generation core).** Delivered: the deps
> (`autodocodec`/`autodocodec-openapi3`/`openapi3`, plus `aeson-pretty`,
> `insert-ordered-containers`, `http-media`); the pure `Ecluse.Manifest` assembly
> (`buildOpenApi :: ManifestSource -> OpenApi`) folding the closed `Route` over the
> mounts; the owned `ErrorEnvelope` schema (one `autodocodec` codec → `aeson` + OpenAPI)
> and the hand-written partial synthesized-packument `ToSchema`; the `openapi-gen`
> generator **plus the `ecluse-manifest` internal sublibrary that holds the assembly**
> (so the OpenAPI dependency tree stays out of the shipped `ecluse` app / `exe:ecluse`
> proxy closure); a deterministic artifact generated to **`openapi/openapi.json`**
> (**git-ignored** — it is derived build data, regenerated on demand by `cabal run
> openapi-gen`, not committed); and unit tests (`test/unit/Ecluse/ManifestSpec.hs`).
> The Redoc render + `make site`/Pages publishing is **PR 2** (which runs the
> generator at publish time, since the artifact is not committed); the structural
> drift controls (`validateToJSON`, `Route`↔operation exhaustiveness, the `hspec-wai`
> live-status contract) are **S35 (PR 3)**. Status stays `not-started` until PR 2
> closes the render/publish half (the repo has no in-progress status value).
> **Config-as-JSON-Schema is cut** (architect decision, recorded in the owned-schemas
> AC): the manifest is config-agnostic, so the config model defines no `autodocodec`
> codec and the strict hand-rolled config decoders are untouched.

**Acceptance criteria.**
- [ ] **Owned schemas.** The error/denial envelope (S11, via S12) is an owned
  code-first type whose `aeson` instances *and* OpenAPI schema derive from one
  `autodocodec` codec; the **synthesized packument** (the served merged-and-filtered
  view — S14, over the S06 wire type) is a **partial, hand-written** schema: modelled
  known/transformed fields + `additionalProperties: true` with the "relayed from
  upstream, private wins" note. —
  _api-surface.md#the-synthesized-packument-schema--the-trust-boundary_
  - **Config-as-JSON-Schema is cut (architect decision):** the OpenAPI manifest is
    **config-agnostic** — a config schema would be an orphan in `components.schemas`,
    documenting no operation. If an operator config schema is ever wanted it is a
    **separate artifact** (a hand-written `ToSchema`; the strict hand-rolled config
    decoders stay untouched). The config model defines **no** `autodocodec` codec here.
- [ ] **Paths derived from `Route` × mounts.** Operations are folded from the closed
  `Route` enumeration (`Ecluse.Core.Server.Route`) over the configured mounts; each
  mount contributes its per-ecosystem path template + support status. **`Search` is
  documented as `501`** (a first-class boundary), tarballs as opaque streamed media.
  — _api-surface.md#source-of-truth-the-route-enumeration--mounts_
- [ ] **Tags = ecosystems.** Operations grouped by mount so the rendered doc reads as
  "one server, these protocols." — _hosting.md#capability-manifest_
- [ ] **Statically generated, not served.** A build-time **generator** (a small
  executable / `cabal run`, kept **out of the library dependency closure** like the
  benchmark components) assembles the `openapi3` document from a **fixed canonical
  config** and writes `openapi.json`. The assembly is a **pure** function of (config,
  mounts) — **no WAI route, not wired into the running app.** Output is
  **deterministic** (stable key ordering, fixed base URLs) so the artifact is
  byte-reproducible across machines. It is **derived build data** (a pure function of
  the source), so it is **generated on demand, not committed**; drift is caught by
  S35's structural controls, not a committed snapshot.
- [ ] **Rendered & published in CI, node-free.** `make docs-site` / `make site`
  render the spec to a static **Redoc** page and stage it into `./_site` next to the
  Haddock, and `openapi.json` itself is published at a stable URL. The Redoc bundle is
  **vendored and hash-pinned as a flake input** — mirroring the existing `mermaidJs`
  `fetchurl` pattern (`flake.nix` → `_site/vendor`) — so the lean `.#docs` shell needs
  **no Node** and the published site has **no external runtime dependency**. The
  `pages.yml` workflow publishes it on push to `main` with the rest of the site.

**File scope.**
- `manifest/Ecluse/Manifest.hs` (the **`ecluse-manifest` internal sublibrary**) —
  assemble the `openapi3` document (owned schemas + the `Route` × mount path fold) as a
  **pure** `ManifestSource -> OpenApi` function. The sublibrary (the
  `ecluse-test-support` pattern) carries the heavy OpenAPI dependency tree and is
  depended on **only** by the generator and the unit test — **not** by the `ecluse`
  app library, so `openapi3` never reaches the shipped proxy. It links `ecluse-core`
  (the `Route` enumeration and the served types), not the app library. **No
  `/openapi.json` handler.**
- The owned error/denial envelope is a **new code-first type** in the sublibrary
  (`ErrorEnvelope`, via one `autodocodec` codec). **No** `autodocodec` codecs are
  added to `core/src/Ecluse/Core/Server/Response.hs`, the config model, or the npm
  served view; npm **inbound** wire decoding stays lenient `aeson` and the renderer is
  unchanged.
- `ecluse.cabal` — the `ecluse-manifest` sublibrary plus an `openapi-gen` executable
  (the generator), both kept out of the app-library closure (cf the `ecluse-bench` /
  `bench-load` precedent and `ecluse-test-support`).
- `test/unit/Ecluse/ManifestSpec.hs` — the document validates; every `Route`
  constructor × mount appears; `Search` carries `501`; `hedgehog` round-trips the
  owned codecs (conformance-by-construction in lieu of an external fuzzer). _No
  `hspec-wai` serving test for the manifest — it is not served._
- `Makefile` (`docs-site` / `site`) — generate `openapi.json` and stage the Redoc
  page + spec into `./_site`; `flake.nix` — the vendored Redoc bundle + the `.#docs`
  shell wiring; `web/` — the small Redoc HTML wrapper / api-index link.

**Test tier.** Unit — manifest assembly + codec round-trips. The Redoc render and the
spec publish are CI artifact steps, not a test tier.

**Dependency rationale.** S34 depends on **S14** (not the S09/S33 transforms
directly) so the published packument schema describes a body the server can actually
serve — documenting it before the packument path closes would be drift-at-birth. S14
transitively brings the wire type (S06), the filter (S09), and the merge (S33);
**S12** is kept because it finalizes the **`Route` × mount enumeration** the path fold
consumes (no longer because the manifest mounts into the app — it does not); **S03**
supplies the config model for config-as-JSON-Schema and the fixed canonical config the
generator runs against.

**Notes / risks — and findings on the _how_.**
- **Raw-WAI, not Servant → assembled, not derived.** There is no Servant route table
  to reflect, so the document is **assembled by hand** from the closed `Route` sum ×
  mounts (a total `Route → Operation` fold) plus `autodocodec`-derived schemas for the
  owned types — exactly the trade accepted in
  [web-layer.md → raw WAI](../../docs/architecture/web-layer.md#raw-wai-not-a-web-framework).
- **Generator-as-executable** mirrors the benchmark components: a non-library
  component, out of the app's dependency closure, run at build time. Static generation
  means the *only* consumers of the assembly function are this generator and the unit
  tests — no runtime surface.
- **Determinism is load-bearing.** `openapi3` keeps paths/definitions in insertion
  order (`InsOrdHashMap`), but the JSON encode must pin object-key ordering (e.g.
  `aeson-pretty` with `confCompare = compare`, or an explicit ordering) so the
  published artifact is byte-stable and a regeneration is a reviewable diff. Generate
  from a **fixed canonical config** (known mounts / base URLs), never a live
  deployment, or it churns on per-environment values.
- **New deps:** `autodocodec` (+ `autodocodec-openapi3`) and `openapi3` — record in
  [technology-stack.md](../../docs/architecture/technology-stack.md#technology-stack).
  Availability probed 2026-06-27 (nixpkgs-unstable): `autodocodec` 0.5.0.0,
  `autodocodec-openapi3` 0.3.0.1 present; **confirm `openapi3` and all three in the
  pinned nixpkgs 26.05** at implementation.
- **Renderer:** prefer **vendoring Redoc's `redoc.standalone.js`** (hash-pinned
  `fetchurl` flake input, copied into `_site/vendor`, with a tiny static HTML wrapper
  pointing at the published `openapi.json`) — zero Node, consistent with the vendored
  Mermaid bundle. `redocly` (2.17.0, packaged in nixpkgs) is a fallback if a
  build-time self-contained-HTML transform is later preferred — but it adds Node to the
  `.#docs` shell, against the lean/node-free posture.
- **Scope guard.** Do **not** model npm's full packument or registry protocol (that is
  npm's contract). The manifest documents Écluse's *coverage*; pass-through bodies link
  out rather than reproduce the upstream schema.

**Reconciliation (post-#133).** The owned *error/denial* schema is the **agnostic
serve outcome** (`Ecluse.Core.Server.Response`); npm's `{"error": …}` **body** lives
in the mount's renderer (`Ecluse.Core.Registry.Npm.Serve`, per #122 / #133). So the
manifest models the agnostic envelope as the owned schema and treats each mount's
rendered body as that ecosystem's surface. See
[web-layer.md → Error model](../../docs/architecture/web-layer.md#error-model).
