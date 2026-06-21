---
id: S08
title: "npm data plane: fetch + publish"
milestone: M1 — npm protocol adapter
status: not-started
depends-on: [S02, S07]
test-tier: [unit, smoke]
arch-refs:
  - docs/architecture/web-layer.md#control-plane-vs-data-plane
  - docs/research/reverse-engineering/npm.md#2-transport--conventions
  - docs/research/reverse-engineering/npm.md#10-write-path-for-completeness
  - docs/architecture/registry-model.md#registry-abstraction
pr: null
---

# S08 — npm data plane: fetch + publish

> Milestone **M1** · depends on: [S02](S02-seam-interfaces.md), [S07](S07-npm-projection.md) · tier: unit, smoke

**Goal.** Implement the effectful `RegistryClient` fields over `http-client` (the
data plane, never `amazonka`): `fetchMetadata`, `fetchArtifact`, `publishArtifact`,
behind a `newNpmClient` smart constructor that closes over its HTTP manager and
optional bearer token.

**Acceptance criteria.**
- [ ] `fetchMetadata` requests the **abbreviated** form
  (`Accept: application/vnd.npm.install-v1+json`) with `Accept-Encoding: gzip`, and
  fetches the **full** packument when `time`/publish-age is needed; conditional-GET
  validators relayed. — _npm.md#2-transport--conventions, npm.md#8-version--availability-resolution_
- [ ] `fetchArtifact` returns a streamable handle (the response body is streamed by
  the web layer, S13 — this slice exposes it without buffering the whole tarball). —
  _web-layer.md#streaming-and-resource-lifetime_
- [ ] `publishArtifact` performs the npm `PUT /{pkg}` publish (packument +
  `_attachments`), treating a `409`/already-present as idempotent success. —
  _npm.md#10-write-path-for-completeness_
- [ ] A `RegistryClient` is assembled by `newNpmClient :: NpmClientConfig -> IO RegistryClient`,
  wiring S07's `parse*` into the pure fields and these over the effectful ones. —
  _registry-model.md#registry-abstraction_
- [ ] Bearer-token attachment is per the credential-flow authority model
  (which token, by leg, is the request pipeline's job in S14 — here the client
  accepts an injected token and never originates credential policy). — _web-layer.md#control-plane-vs-data-plane_

**File fence.**
- `src/Ecluse/Registry/Npm.hs` — `newNpmClient`, the effectful fields, request building.
- `ecluse.cabal` — `http-client`, `http-client-tls` (added in S01 if not already), `zlib`/gzip handling as needed.
- `test/unit/Ecluse/Registry/NpmSpec.hs` — request shaping against an in-process WAI/`http-client` stub (Accept/encoding headers, scoped `%2F`, idempotent publish on 409).
- `test/smoke/Ecluse/RegistryProtocolSpec.hs` — live `fetchMetadata` of a real package (non-gating).

**Test tier.** Unit (in-process stub upstream, gating) + smoke (live npm, non-gating).

**Notes / risks.** Keep the streaming body un-buffered — the bracket/respond
lifetime is owned by the web layer (S13); here just hand back the open response in a
way S13 can bracket. **Never route the data plane through `amazonka`** (the
control/data split is the whole point). Publish is exercised end-to-end against a
real npm-speaking registry / WAI stub in S19/S20, not here.
