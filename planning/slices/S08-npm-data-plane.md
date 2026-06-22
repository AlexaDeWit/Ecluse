---
id: S08
title: "npm data plane: fetch + publish"
milestone: M1 — npm protocol adapter
status: merged
depends-on: [S02, S07]
test-tier: [unit, smoke]
arch-refs:
  - docs/architecture/web-layer.md#control-plane-vs-data-plane
  - docs/research/reverse-engineering/npm.md#2-transport--conventions
  - docs/research/reverse-engineering/npm.md#10-write-path-for-completeness
  - docs/architecture/registry-model.md#registry-abstraction
pr: 50
---

# S08 — npm data plane: fetch + publish

> Milestone **M1** · depends on: [S02](S02-handle-interfaces.md), [S07](S07-npm-projection.md) · tier: unit, smoke

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

**File scope.**
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

**As-built notes (PR #50).**
- **Streaming split into two surfaces.** The `RegistryClient.fetchArtifact` handle
  field stayed **buffered** (it returns a whole-bytes `RegistryResponse`) — that
  field is for the mirror worker, which must read the entire artifact to verify its
  integrity before publishing. The **un-buffered** path for bounded-memory client
  streaming is exposed separately as a request *builder*, `artifactRequest` (marked
  non-decompressing), which the web layer (S13) brackets with
  `withResponse`/`responseStream`. So "expose a streamable handle" became "buffered
  handle field + un-buffered `artifactRequest` builder for S13" rather than one
  streaming field. *(See the open question escalated to the architect: whether the
  handle field itself should be streaming.)*
- **`fetchMetadataForm` added.** The handle's `fetchMetadata` field has no place to
  carry the abbreviated-vs-full selector or relayed conditional-GET validators, so a
  richer `fetchMetadataForm :: NpmClientConfig -> MetadataForm -> Validators ->
  PackageName -> IO RegistryResponse` is exposed alongside it. `fetchMetadata`
  requests the `Abbreviated` form unconditionally; the request pipeline calls
  `fetchMetadataForm` directly when it needs the `Full` packument (for `time`) or to
  revalidate against an `ETag`. `MetadataForm`/`Validators` are part of the module's
  public surface.
- **`PublishError` reused for URL-formation faults.** The request builders
  (`metadataRequest`/`artifactRequest`/`publishRequest`) report an unformable URL
  (empty/unparseable base) as a `PublishError`, and a `UrlError` from S36 is adapted
  into it — there is no separate fetch-error type. On the effectful fetch paths an
  unformable URL is thrown as an `IO` exception (a config fault, not a per-response
  condition). *(See the escalation on whether `PublishError` should become a shared
  `RegistryError`.)*
- **Anonymous-by-default config.** `newNpmClient` takes an `NpmClientConfig`
  (base URL + shared `Manager` + optional injected `Secret` token);
  `defaultNpmConfig` / `publicRegistryBaseUrl` give an anonymous public-registry
  client. The client attaches whatever token it is given and never originates
  credential policy, per the authority model.
