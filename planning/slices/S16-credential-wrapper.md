---
id: S16
title: CredentialProvider generic wrapper + static leaf
milestone: M4 — AWS cloud backends & worker
status: merged
depends-on: [S02]
test-tier: [unit]
arch-refs:
  - docs/architecture/cloud-backends.md#credential-provider
pr: null
---

# S16 — `CredentialProvider` generic wrapper + static leaf

> Milestone **M4** · depends on: [S02](S02-handle-interfaces.md) · tier: unit

**Goal.** The interesting part of outbound auth is the refresh/cache/expiry/
concurrency policy, not the cloud call. Build the generic wrapper that holds that
policy, parameterised over a tiny per-cloud `mintToken` leaf, plus the `static`
leaf. Everything but `mintToken` is unit-testable deterministically.

**Acceptance criteria.**
- [ ] A generic wrapper takes `mintToken :: IO AuthToken` + an **injected clock** and
  returns a `CredentialProvider` whose `currentToken` serves a cached token,
  refreshing **proactively in the background** at ~80% of lifetime (configurable,
  with jitter) plus a hard floor near expiry. — _cloud-backends.md#credential-provider_
- [ ] **Single-flight** refresh (STM flag / `TMVar`): at most one mint in flight; a
  cohort never stampedes. — _cloud-backends.md#credential-provider_
- [ ] **Mint failure** keeps serving the still-valid token, retries with backoff
  behind a **circuit breaker**, and alarms; only an *expired* token + failing mint
  surfaces as failure to the caller. — _cloud-backends.md#credential-provider_
- [ ] `static` leaf: a fixed token, no expiry, never refreshes.
- [ ] Deterministic unit tests with the injected clock + a fake mint cover: refresh
  timing, single-flight, serve-stale-on-failure, breaker trip/half-open.

**File scope.**
- `src/Ecluse/Credential.hs` — the wrapper + `static` (additive to S02; the handle type is already there).
- `src/Ecluse/Credential/Refresh.hs` — refresh/cache/breaker policy (if it earns its own module).
- `test/unit/Ecluse/CredentialSpec.hs` — clock-driven policy tests.

**Test tier.** Unit — the whole policy is deterministic with an injected clock and a
fake mint; no cloud.

**Notes / risks.** Depends only on the handle (S02), so it can pull in early to
de-risk M4. The circuit-breaker machinery is shared with the effectful rule tier
(S21) — consider a small reusable breaker module both can use (coordinate naming/
location; **escalate** if it forces a shared-module decision). The real cloud
`mintToken` leaves are S17 (CodeArtifact) and S29 (ADC); their *real* mint is
smoke-tier only.

## As-built notes

- The policy earned its own module, `Ecluse.Credential.Refresh`
  (`refreshingProvider` + `RefreshConfig`/`defaultRefreshConfig`); the handle and
  `staticProvider` stay in `Ecluse.Credential`. `Refresh` imports `Credential`
  (one-way) rather than introducing a `.Types` module, so the existing scope held
  and no cyclic-import split was needed.
- **No shared breaker module.** The circuit breaker is a small self-contained
  state machine (`Closed`/`Open`/`HalfOpen`) private to `Refresh`, shared by the
  background and synchronous mint paths via one `admitMint` gate. This stayed
  inside the slice scope, so the cross-slice "reusable breaker" decision flagged
  for S21 was **not** triggered and needed no escalation; S21 can still factor a
  shared module later if it wants one.
- `mintToken`/clock/jitter are injected on `RefreshConfig`, so the whole policy is
  deterministic under an injected clock + fake mint — refresh timing, single-flight,
  serve-stale-on-failure, breaker trip/half-open/re-open, default-knob behaviour,
  and the no-expiry (never-refresh) case are all unit-tested with no network.
- Added `stm` as a direct library dependency (already pinned transitively) for
  `Control.Concurrent.STM.retry`, used to make an expired caller wait on an
  in-flight single-flight refresh instead of stampeding a second mint.
- Surfaced one typed failure, `CredentialError(BreakerOpen)` — the only case that
  reaches the caller (expired token + open breaker); a still-valid token is always
  served, so this never touches the client serve path (credentials are
  mirror-write only **under the default `passthrough` strategy**; `service` puts a
  read credential on the serve path; see
  [access-model](../../docs/architecture/access-model.md) and S44).
