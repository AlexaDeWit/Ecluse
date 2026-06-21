---
id: S10
title: Pure router (classify/Route)
milestone: M2 — Web front door
status: not-started
depends-on: []
test-tier: [unit]
arch-refs:
  - docs/architecture/web-layer.md#raw-wai-not-a-web-framework
  - docs/research/reverse-engineering/npm.md#2-transport--conventions
pr: null
---

# S10 — Pure router (`classify` / `Route`)

> Milestone **M2** · depends on: — (root) · tier: unit

**Goal.** The pure routing function that turns an ecosystem-native path (post
mount-strip) into a small `Route` sum type, so the whole routing table is
unit-testable with no server.

**Acceptance criteria.**
- [ ] `Route = Packument PackageName | Tarball PackageName Text | Ping | Search | Unsupported`
  (and the liveness/readiness routes — see S12) and `classify :: [Text] -> Route`,
  pure. — _web-layer.md#raw-wai-not-a-web-framework_
- [ ] Encoded-slash handling: a scoped name arriving as one already-decoded segment
  (`@scope/pkg`) **and** as two segments (`@scope`,`pkg`) both classify correctly
  (WAI percent-decodes `pathInfo`). — _npm.md#2-transport--conventions_
- [ ] Reserved meta-routes (`/-/…`) matched **first** (a real package name can't
  begin with `-`); the tarball path `/{pkg}/-/{file}.tgz` distinguished from
  packument; anything unrecognised → `Unsupported`. — _web-layer.md#raw-wai-not-a-web-framework_

**File fence.**
- `src/Ecluse/Server/Route.hs` — `Route`, `classify`, name normalisation.
- `ecluse.cabal` — register module (no new deps).
- `test/unit/Ecluse/Server/RouteSpec.hs` — table of `pathInfo` → `Route` (scoped both encodings, tarball, meta-routes, junk → Unsupported).

**Test tier.** Unit — exhaustive routing table; this is the cheapest place to pin
the whole URL surface.

**Notes / risks.** Pure and dependency-free (root) — a Wave-1 candidate. Normalise
the scoped-name encodings early so downstream never re-checks. Keep `classify`
ecosystem-native (mount dispatch/prefix-strip is S12) — this function never sees the
mount prefix.
