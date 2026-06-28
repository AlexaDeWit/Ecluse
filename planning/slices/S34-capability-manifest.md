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
pr: null
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

**Acceptance criteria.**
- [ ] **Owned schemas via `autodocodec`.** The error/denial envelope (S11, via S12),
  the **synthesized packument** (the served merged-and-filtered view — S14, over the
  S06 wire type), and the config model (S03) define their JSON via `autodocodec`,
  deriving `aeson` instances *and* the OpenAPI/JSON-Schema from one codec (no drift).
  The synthesized packument is a **partial** schema: modelled known/transformed
  fields + `additionalProperties: true` with the "relayed from upstream, private
  wins" note. — _api-surface.md#the-synthesized-packument-schema--the-trust-boundary_
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
  byte-reproducible across machines and a committed copy yields a meaningful diff
  (this is the same artifact S35's golden snapshot guards — the two converge).
- [ ] **Rendered & published in CI, node-free.** `make docs-site` / `make site`
  render the spec to a static **Redoc** page and stage it into `./_site` next to the
  Haddock, and `openapi.json` itself is published at a stable URL. The Redoc bundle is
  **vendored and hash-pinned as a flake input** — mirroring the existing `mermaidJs`
  `fetchurl` pattern (`flake.nix` → `_site/vendor`) — so the lean `.#docs` shell needs
  **no Node** and the published site has **no external runtime dependency**. The
  `pages.yml` workflow publishes it on push to `main` with the rest of the site.

**File scope.**
- `src/Ecluse/App/Manifest.hs` (app library) — assemble the `openapi3` document
  (owned schemas + the `Route` × mount path fold) as a **pure** `Config -> OpenApi`
  function. (Module name indicative; the exact home follows the `ecluse-core` /
  `ecluse` split — the `Route` enumeration lives in `ecluse-core`, the config/mounts
  in the app, so the assembly sits in the app library that composes both.) **No
  `/openapi.json` handler.**
- `core/src/Ecluse/Core/Server/Response.hs`, the app config model, the npm served
  view — *additive* `autodocodec` codecs for the owned types (no behaviour change to
  existing decoders; keep npm **inbound** wire decoding lenient `aeson`).
- `ecluse.cabal` — a `openapi-gen` executable component (the generator), kept out of
  the library closure (cf the `ecluse-bench` / `bench-load` precedent).
- `test/unit/Ecluse/App/ManifestSpec.hs` — the document validates; every `Route`
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
  means the *only* consumer of the assembly function is this generator (and the S35
  golden) — no runtime surface.
- **Determinism is load-bearing.** `openapi3` keeps paths/definitions in insertion
  order (`InsOrdHashMap`), but the JSON encode must pin object-key ordering (e.g.
  `aeson-pretty` with `confCompare = compare`, or an explicit ordering) so the
  committed/published artifact is byte-stable and the S35 golden diff is meaningful.
  Generate from a **fixed canonical config** (known mounts / base URLs), never a live
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
