---
id: S34
title: Capability manifest (OpenAPI) + /openapi.json + docs render
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

# S34 — Capability manifest (OpenAPI) + `/openapi.json` + docs render

> Milestone **M2** · depends on: [S03](S03-config-loader.md), [S12](S12-wai-app-middleware.md), [S14](S14-packument-path.md) · tier: unit
>
> **Not on the AWS-launch critical path.** A discoverability artifact; it can land
> any time after its dependencies. It does **not** gate M3/M4.

**Goal.** Emit Écluse's **capability manifest** — an OpenAPI 3 document describing
*which registry protocols this server speaks and exactly what is / isn't
supported* — serve it at `GET /openapi.json`, and render browsable docs in CI. See
[api-surface.md](../../docs/architecture/api-surface.md) for the full rationale
(capability manifest, **not** a client-integration contract).

**Acceptance criteria.**
- [ ] **Owned schemas via `autodocodec`.** The error/denial envelope (S11, via
  S12), the **synthesized packument** (the served merged-and-filtered view — S14,
  over the S06 wire type), and the config model (S03) define their JSON via
  `autodocodec`, deriving `aeson` instances and
  the OpenAPI/JSON-Schema from one codec (no drift). The synthesized packument is a
  **partial** schema: modelled known/transformed fields + `additionalProperties:
  true` with the "relayed from upstream, private wins" note. — _api-surface.md#the-synthesized-packument-schema--the-trust-boundary_
- [ ] **Paths derived from `Route` × mounts.** Operations are folded from the
  closed `Route` enumeration over the configured mounts; each mount contributes its
  per-ecosystem path template + support status. **`Search` is documented as `501`**
  (a first-class boundary), tarballs as opaque streamed media. — _api-surface.md#source-of-truth-the-route-enumeration--mounts_
- [ ] **Tags = ecosystems.** Operations grouped by mount so the rendered doc reads
  as "one server, these protocols." — _hosting.md#capability-manifest_
- [ ] **Served locally.** `GET /openapi.json` returns the document from a plain WAI
  control-plane route (not under any mount), wired into the S12 app. — _web-layer.md#capability-manifest_
- [ ] **Rendered in CI, node-free at runtime.** A `docs-site` step renders static
  **Redoc** HTML alongside the Haddock for GitHub Pages; the renderer is pinned in
  the dev shell / vendored — never a runtime dependency.

**File fence.**
- `src/Ecluse/Server/Manifest.hs` — assemble the `openapi3` document (owned schemas
  + `Route`×mount path fold) and the `/openapi.json` handler.
- `src/Ecluse/Server/Response.hs`, config model, npm filter — *additive*
  `autodocodec` codecs for the owned types (no behaviour change to existing
  decoders; keep npm **inbound** wire decoding lenient `aeson`).
- `test/unit/Ecluse/Server/ManifestSpec.hs` — the document validates; every `Route`
  constructor × mount appears; `Search` carries `501`; `hedgehog` round-trips the
  owned codecs (conformance-by-construction in lieu of an external fuzzer).
- `Makefile` / docs workflow — extend `docs-site` to emit Redoc HTML.

**Test tier.** Unit — manifest assembly + codec round-trips. The Redoc render is a
CI artifact step, not a test tier.

**Dependency rationale.** S34 depends on **S14** (not the S09/S33 transforms
directly) so the published packument schema describes a body the server can
actually serve — documenting it before the packument path closes would be
drift-at-birth. S14 transitively brings the wire type (S06), the filter (S09), and
the merge (S33); **S12** (the WAI app + `Route` × mount enumeration) is implied by
S14 but kept explicit because `/openapi.json` mounts into that app; **S03** supplies
the config model for config-as-JSON-Schema.

**Notes / risks.** New deps: `autodocodec` (+ `autodocodec-openapi3`) and
`openapi3` — record them in [technology-stack.md](../../docs/architecture/technology-stack.md#technology-stack).
Do **not** model npm's full packument or registry protocol (out of scope — that is
npm's contract). The manifest documents Écluse's *coverage*; pass-through bodies
link out rather than reproduce the upstream schema.
