---
id: S52
title: First-party publish path → publication target
milestone: M4, AWS cloud backends & worker
status: merged
depends-on: [S03, S08, S12, S20]
test-tier: [unit, integration]
arch-refs:
  - docs/architecture/registry-model.md#publishing-first-party-packages-the-publication-target
  - docs/architecture/access-model.md
  - docs/architecture/configuration.md
issue: 163
---

# S52, First-party publish path → publication target

**Goal.** Give the proxy a **client publish path**: `npm publish` (`PUT /{pkg}`) is
accepted at the mount and routed to a new fourth registry role, the **publication
target**, the write destination for first-party / internal packages. Closes #163,
where internal publishers had no proxy-mediated flow and were forced out-of-band
(the "leaked complexity"). The publication target is the symmetric partner to the
mirror target: the same `RegistryClient.publishArtifact` primitive, but
client-triggered, first-party content, and the **client's own** credential.

## The model, two read roles + two write roles

| | Read | Write |
|---|---|---|
| **Public** | public upstream (gated, anonymous) | **mirror target**, proxy-driven, approved public pkgs, Écluse's own credential |
| **Private** | private upstream (trusted, per-client auth) | **publication target**, client-driven, first-party pkgs, client's forwarded credential |

The publication target **may coincide** with the private upstream (so published
packages are immediately readable via the private leg) or the mirror target, but is
configured as its own role.

## Ratified behaviour (architect-confirmed, #163)

- **Anti-shadowing guard (load-bearing).** A publish whose package name is not within
  the operator's **configured publish scope allow-list**, the MVP mechanism, e.g.
  `@acme/*`, is **rejected before any upstream write**. This stops a publish that
  would shadow an existing public package (dependency confusion). _Future: richer name
  grammars / live collision resolution; the MVP is the scope allow-list only._
- **Credential: passthrough.** The client's publish credential (`Authorization` /
  `_authToken`) is **forwarded** to the publication target, which authorises the
  publisher, symmetric with the private-upstream read under `passthrough`. The mirror
  target keeps Écluse's **own** credential; these are distinct write roles. **This flow
  must be documented in `access-model.md` to the same standard as the read flows.**
- **No readback role.** Published packages are read back via the **private upstream**;
  the publication target is write-only from the proxy's perspective. To read what you
  published, the operator configures the publication target to be the **same registry**
  as the private upstream.
- **Opt-in config.** `PUBLICATION_TARGET_URL` (+ its credential) enables the path;
  **unset ⇒ `PUT /{pkg}` returns `405`** (no implicit write path).

## Approach

1. **Route.** Add the `PUT /{pkg}` publish route to the npm route classifier and the
   WAI dispatch (today read-only + meta routes). The npm adapter already carries
   `publishRequest` (publish document → `PUT` builder) and `publishArtifact`.
2. **Publish policy.** Parse the publish document, enforce the scope allow-list (reject
   out-of-scope with a clear npm-shaped error), then `publishArtifact` against the
   publication target with the **forwarded** client token.
3. **Composition / config.** The publication-target role + credential are reserved in
   S20's composition root; this slice consumes them. `405` when unset.

## Acceptance criteria

- `PUT /{pkg}` with an **in-scope** name and a configured publication target publishes
  (forwarded credential) and returns the npm success shape. (integration)
- An **out-of-scope** name is **rejected before any upstream write**, with a clear
  error, the anti-shadowing guard. (unit + integration)
- With no `PUBLICATION_TARGET_URL`, `PUT /{pkg}` returns `405`. (unit)
- The client credential is forwarded to the publication target and **never** to the
  public upstream or used as the mirror-target credential. (unit)
- Docs reconciled in the same PR: `registry-model.md` (role + publish path),
  `access-model.md` (publish credential flow), `hosting.md` (4-role set),
  `configuration.md` (config + guard).
- Local gate green incl. `codecov/patch` ≥ 95%.

## Scope

- `src/Ecluse/Registry/Npm/Route.hs` + `src/Ecluse/Server/Route.hs`, the publish route.
- `src/Ecluse/Server/…`, the publish handler + scope-allow-list policy.
- `src/Ecluse/Config.hs`, `PUBLICATION_TARGET_URL` + publish scopes.
- Composition root (with S20), wire the publication-target role + credential.
- docs + tests.

## Out of scope

- Richer publish-name grammars / live public-registry collision checks (future; the MVP
  guard is the scope allow-list).
- A publication-target **read** role (reads come via the private upstream).
- Unpublish / dist-tag / deprecate flows.

## Notes

Depends on the **publication-target role being reserved in S20's composition root**
(config + credential), encoded ahead of the build so it is not retrofitted. The
ratified design of record is in `docs/architecture/registry-model.md`.

## As-built (merged, [#379](https://github.com/AlexaDeWit/Ecluse/pull/379), closes [#163](https://github.com/AlexaDeWit/Ecluse/issues/163))

Delivered against the acceptance criteria above, with these design decisions recorded
as built:

- **Route model.** The agnostic `Route` sum gained `Publish PackageName`, and the
  `Classifier` was generalised from `[Text] -> Route` to `Method -> [Text] -> Route`,  a classifier now maps a *request* (method + path), so `PUT /{pkg}` classifies as
  `Publish` while `HEAD` still renders like its `GET`.
- **Body relay, not the write verdict.** First-party publish is a **body relay**, the
  publication target's own status and body are forwarded to the client verbatim, which
  is distinct from the mirror worker's `RegistryClient.publishArtifact` (a
  success/`PublishFault` verdict). The npm adapter gained `relayPublishDocument` over a
  per-request `NpmClientConfig` carrying the forwarded client token; the agnostic
  `RegistryClient` is unchanged. `PUBLICATION_TARGET_TOKEN` is the static fallback when a
  client sends no token.
- **Config.** `PUBLICATION_TARGET_URL`, `PUBLICATION_TARGET_TOKEN`, `PUBLISH_SCOPES`
  (global env, single-ecosystem desugaring). `PUBLISH_SCOPES` is **required when a target
  is set** (a fail-loud `PublishScopesMissing` boot error); `bindingPublishDeps :: Maybe
  PublishDeps` models the opt-in (`Nothing` ⇒ `405`). The scope guard keys on the
  **route** name; the document's self-reported name is the registry's concern (a future
  richer-grammar item, per Out of scope).
- **Credential-redirect invariant (security, application-wide).** Promoted out of this
  slice during review and made a correctness rule: any outbound request carrying a
  forwarded bearer credential MUST NOT follow HTTP redirects, enforced structurally at the
  single attachment point (`withToken` sets `redirectCount = 0`), covering every npm
  data-plane builder, the publish relay, and the mirror-worker publish. See
  `docs/architecture/security.md`.
- **Bounded relay read.** The publish relay reads the target response through the npm byte
  cap (`readBoundedBody`); an over-cap relay fails closed to `502` rather than buffering
  unbounded.
- **Tests as built:** core-unit route specs (PUT → `Publish`; method decides read vs
  write), app-unit `ServerSpec` (405 / 403 scoped + unscoped / 502 unreachable / 500
  unformable / 401 edge-gate) and `CompositionSpec` (publish-deps wiring, scope parsing,
  `PublishScopesMissing`), and a hermetic in-process Warp integration `PublishSpec`
  (success relay + forwarded credential + static fallback + refuse-before-write + 405 +
  409-relay). **E2E coverage of the publish flow is a fast-follow** on the `ecluse-e2e`
  tier (`test/e2e`); the merged slice carried unit + integration only, hence the
  `test-tier` frontmatter is unchanged here and advances when the e2e PR lands.
