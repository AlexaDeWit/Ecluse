---
id: S45
title: Delegated-cache authorisation probe
milestone: M4 — AWS cloud backends & worker
status: not-started
depends-on: [S44]
test-tier: [unit, integration]
arch-refs:
  - docs/architecture/access-model.md#delegated-cache--the-upstream-decides-retrievability-écluse-caches-the-compute
  - docs/architecture/access-model.md#authorisation-granularity
  - docs/architecture/access-model.md#the-four-corner-trade-off
pr: null
---

# S45 — Delegated-cache authorisation probe

> Milestone **M4** · depends on: [S44](S44-service-credential-reads.md) · tier: unit, integration
>
> _Access-model enhancement; **off the launch critical path.** Implements the
> `delegated-cache` strategy: keep the upstream as the authority for retrievability,
> but reuse the cached compute — "the upstream decides; we cache the result."_

**Goal.** Under `delegated-cache`, the private leg is **shared**, and **every cache
hit is gated by a fresh per-request authorisation probe** against the upstream before
it is served — that probe, not how the entry was populated, is what makes a shared
private entry safe. This recovers caching without making the edge the authority and
without holding any caller-credential state. The shared entry may be **service-
populated** (reusing the S44 read path; proactively warmable) or **caller-populated**
(filled by the first authorised caller's forwarded token, holding no read credential);
populate is an orthogonal operational choice (see
[access-model.md → Caching](../../docs/architecture/access-model.md#caching)).

**Acceptance criteria.**
- [ ] Before serving a `delegated-cache` hit, Écluse issues an **authorisation probe**
  to the upstream carrying the caller's credential; a non-2xx probe is refused per the
  [serve error model](../../docs/architecture/web-layer.md#error-model), and a 2xx admits the cached compute. —
  _access-model.md#delegated-cache--the-upstream-decides-retrievability-écluse-caches-the-compute_
- [ ] **Probe granularity is configurable and must match the upstream's authorisation**:
  `mount` (a coarse, per-mount probe, e.g. `whoami`/`HEAD`) or `resource` (a
  per-package probe). The probe must be **cheaper than the fetch it replaces**; a
  strategy declared on an upstream that offers no such probe fails validation. —
  _access-model.md#authorisation-granularity_
- [ ] The probe holds **no credential state** — the caller's token is used transiently
  for the probe and discarded, exactly as `passthrough`. — _access-model.md#the-four-corner-trade-off_
- [ ] Probe outcomes are not cached as a long-lived verdict (the deferred `memoised`
  strategy, explicitly out of scope here); each request re-probes, bounding revocation
  latency to one request. — _access-model.md#memoised--deferred-documented-for-completeness_
- [ ] Tests: unit over probe-admits / probe-refuses and the granularity branch with a
  fake upstream; integration proving a coarse (`whoami`) probe gates a shared cached
  packument for two distinct callers (authorised vs refused).

**File scope.**
- `src/Ecluse/Server/*` (read path) — the pre-serve probe + admission gate.
- `src/Ecluse/Access.hs` — probe-granularity model (additive to S43).
- `test/unit/...`, `test/integration/...` — probe admission + granularity.

**Test tier.** Unit (probe gate, granularity) + integration (probe gates a shared hit per caller).

**Notes / risks.** This is the "ingenious compromise" of the access model: it keeps
the cloud-native, no-auth-to-build posture (the upstream decides retrievability) while
eliminating the expensive duplicate fetch + parse + merge. It is only viable where the
upstream exposes a probe cheaper than the fetch (npm `whoami` for a coarse upstream);
where it does not, `delegated-cache` collapses toward `passthrough` and must be
rejected at config validation rather than silently degraded. **Escalate** if a target
upstream's cheapest authorisation check is the fetch itself.
