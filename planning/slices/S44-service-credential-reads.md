---
id: S44
title: Service-credential read path
milestone: M4 — AWS cloud backends & worker
status: not-started
depends-on: [S43, S16, S13]
test-tier: [unit, integration]
arch-refs:
  - docs/architecture/access-model.md#service--the-edge-authenticates-écluse-brokers
  - docs/architecture/access-model.md#caching
  - docs/architecture/access-model.md#credential-supply-the-credentialprovider-generalised
pr: null
---

# S44 — Service-credential read path

> Milestone **M4** · depends on: [S43](S43-credential-strategy.md), [S16](S16-credential-wrapper.md), [S13](S13-streaming-cache.md) · tier: unit, integration
>
> _Access-model enhancement; **off the launch critical path.** Implements the
> `service` strategy: private-upstream **reads** via a `CredentialProvider`, which
> makes the private leg of the metadata cache shareable._

**Goal.** Under the `service` strategy, fetch the private upstream with **Écluse's
own** credential (the existing [`CredentialProvider`](S16-credential-wrapper.md), now
used for reads, not only the mirror write) instead of forwarding the caller's token,
and **admit the private leg into the shared cache**. The caller is authenticated at
the edge (S43); the upstream sees one identity.

**Acceptance criteria.**
- [ ] When a mount's strategy is `service`, the private-upstream fetch uses a
  configured **read** `CredentialProvider` (reusing the S16 wrapper + an S17/S29 leaf
  or `static`), never the caller's forwarded token. — _access-model.md#credential-supply-the-credentialprovider-generalised_
- [ ] The private leg becomes **cache-eligible** under `service`: the cache admits a
  private entry, keyed by source + package (never a credential), and concurrent
  resolutions collapse to one upstream fetch — matching the public leg's sharing. —
  _access-model.md#caching, web-layer.md#metadata-cache_
- [ ] `passthrough` behaviour is unchanged (no read credential; private leg not
  cached). The cache-admission witness from S43 gates whether a shared **private**
  entry is **admitted to serving**, independent of how it was populated — population
  is an orthogonal operational choice (#129), since every serve is freshly authorised. —
  _access-model.md#caching_
- [ ] A read-credential refresh failure degrades **reads** (surfaced per the
  [serve error model](../../docs/architecture/web-layer.md#error-model)); document that, unlike the
  mirror-write-only past, a read credential now sits on the serve path under
  `service`. — _access-model.md#credential-supply-the-credentialprovider-generalised_
- [ ] Tests: unit over the strategy branch (service vs passthrough fetch identity +
  cache admission) with a fake provider and an in-process upstream stub; integration
  exercising a `service`-mount read through the composition root.

**File scope.**
- `src/Ecluse/Server/*` (read path) — strategy-aware private fetch + cache admission.
- `src/Ecluse/Env.hs` — a per-mount read `CredentialProvider` slot.
- `test/unit/...`, `test/integration/...` — strategy-branch + end-to-end read.

**Test tier.** Unit (strategy branch, cache admission) + integration (service-mount read).

**Notes / risks.** Reuses S16/S17 with **no change to the provider machinery** — only
its point of use widens from write to read. Coordinate the per-mount read-provider
wiring with **S20** (composition) so backend selection stays in one place. This is
the slice that makes the `service` half of the access model real; `delegated-cache`
(S45) layers the per-request probe on top.
