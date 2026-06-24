---
id: S52
title: First-party publish path → publication target
milestone: M4 — AWS cloud backends & worker
status: not-started
depends-on: [S03, S08, S12, S20]
test-tier: [unit, integration]
arch-refs:
  - docs/architecture/registry-model.md#publishing-first-party-packages-the-publication-target
  - docs/architecture/access-model.md
  - docs/architecture/configuration.md
issue: 163
---

# S52 — First-party publish path → publication target

**Goal.** Give the proxy a **client publish path**: `npm publish` (`PUT /{pkg}`) is
accepted at the mount and routed to a new fourth registry role, the **publication
target** — the write destination for first-party / internal packages. Closes #163,
where internal publishers had no proxy-mediated flow and were forced out-of-band
(the "leaked complexity"). The publication target is the symmetric partner to the
mirror target: the same `RegistryClient.publishArtifact` primitive, but
client-triggered, first-party content, and the **client's own** credential.

## The model — two read roles + two write roles

| | Read | Write |
|---|---|---|
| **Public** | public upstream (gated, anonymous) | **mirror target** — proxy-driven, approved public pkgs, Écluse's own credential |
| **Private** | private upstream (trusted, per-client auth) | **publication target** — client-driven, first-party pkgs, client's forwarded credential |

The publication target **may coincide** with the private upstream (so published
packages are immediately readable via the private leg) or the mirror target, but is
configured as its own role.

## Ratified behaviour (architect-confirmed, #163)

- **Anti-shadowing guard (load-bearing).** A publish whose package name is not within
  the operator's **configured publish scope allow-list** — the MVP mechanism, e.g.
  `@acme/*` — is **rejected before any upstream write**. This stops a publish that
  would shadow an existing public package (dependency confusion). _Future: richer name
  grammars / live collision resolution; the MVP is the scope allow-list only._
- **Credential: passthrough.** The client's publish credential (`Authorization` /
  `_authToken`) is **forwarded** to the publication target, which authorises the
  publisher — symmetric with the private-upstream read under `passthrough`. The mirror
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
  error — the anti-shadowing guard. (unit + integration)
- With no `PUBLICATION_TARGET_URL`, `PUT /{pkg}` returns `405`. (unit)
- The client credential is forwarded to the publication target and **never** to the
  public upstream or used as the mirror-target credential. (unit)
- Docs reconciled in the same PR: `registry-model.md` (role + publish path),
  `access-model.md` (publish credential flow), `hosting.md` (4-role set),
  `configuration.md` (config + guard).
- Local gate green incl. `codecov/patch` ≥ 95%.

## Scope

- `src/Ecluse/Registry/Npm/Route.hs` + `src/Ecluse/Server/Route.hs` — the publish route.
- `src/Ecluse/Server/…` — the publish handler + scope-allow-list policy.
- `src/Ecluse/Config.hs` — `PUBLICATION_TARGET_URL` + publish scopes.
- Composition root (with S20) — wire the publication-target role + credential.
- docs + tests.

## Out of scope

- Richer publish-name grammars / live public-registry collision checks (future; the MVP
  guard is the scope allow-list).
- A publication-target **read** role (reads come via the private upstream).
- Unpublish / dist-tag / deprecate flows.

## Notes

Depends on the **publication-target role being reserved in S20's composition root**
(config + credential) — encoded ahead of the build so it is not retrofitted. The
ratified design of record is in `docs/architecture/registry-model.md`.
