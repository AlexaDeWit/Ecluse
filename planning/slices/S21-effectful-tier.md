---
id: S21
title: Effectful rule tier (Unavailable, timeout/retry/breaker)
milestone: M5 — Effectful rules & CVE
status: not-started
depends-on: [S05, S14]
test-tier: [unit]
arch-refs:
  - docs/architecture/rules-engine.md#effectful-rule-failure
  - docs/architecture/rules-engine.md#rules-engine
  - docs/architecture/web-layer.md#error-model
pr: null
---

# S21 — Effectful rule tier (`Unavailable`, timeout/retry/breaker)

> Milestone **M5** · depends on: [S05](S05-rules-precedence.md), [S14](S14-packument-path.md) · tier: unit

**Goal.** Add the effectful rule tier on top of the pure one: rules that may do IO,
evaluated as a **performance ordering** (pure first; effectful only where it could
still change the outcome), with the fourth outcome `Unavailable` and per-source
resilience (timeout budget, bounded retry/backoff, circuit breaker), fail-closed.

**Acceptance criteria.**
- [ ] `Unavailable Transience` added as a fourth `RuleOutcome`/`Decision` arm,
  carrying will-resolve vs not — **fail-closed** (an `Unavailable` version is not
  admitted). — _rules-engine.md#effectful-rule-failure_
- [ ] **Tier is performance, not precedence**: once the pure tier yields a winner at
  precedence *P*, effectful rules ranked below *P* are skipped; the effectful tier is
  skipped entirely when no effectful rule is ranked ≥ *P*. — _rules-engine.md#rules-engine_
- [ ] Each effectful rule has a timeout budget, bounded retry+backoff, and a
  per-source circuit breaker (reusing the S16 breaker machinery); a rule may set
  `onError: abstain` where availability beats safety. — _rules-engine.md#effectful-rule-failure_
- [ ] `Unavailable` surfaces correctly by request shape: filtered out of a packument
  (like a denial); a concrete artifact maps to 503/500 via the error model (S11). —
  _rules-engine.md#effectful-rule-failure, web-layer.md#error-model_
- [ ] Every `Unavailable`/breaker trip emits an ERROR log + metric (metric in M6).
- [ ] `EvalContext` extended with the fetchers/lookups the effectful tier needs.

**File scope.**
- `src/Ecluse/Rules/Types.hs`, `src/Ecluse/Rules.hs` — `Unavailable`, two-tier eval, `EvalContext` extension.
- `src/Ecluse/Rules/Effectful.hs` — the effectful-rule interface + breaker/retry/timeout harness (if it earns a module).
- `src/Ecluse/Server/Pipeline.hs`, `src/Ecluse/Registry/Npm/Filter.hs` — wire `Unavailable` into serve + filter (replacing the S09/S14 "deny-only until S21" stubs).
- `test/unit/Ecluse/Rules/EffectfulSpec.hs` — tier-skipping, fail-closed, breaker, onError:abstain.

**Test tier.** Unit — deterministic with fake effectful rules / injected clock;
properties for tier-skip and fail-closed.

**Notes / risks.** This unlocks `DenyIfCVE` (S23) but the engine change is independent
of the data source. Reconcile the S09 filter and S14 serve "deny-only" placeholders
to handle `Unavailable` here — those were explicitly flagged as awaiting this slice.
Keep the pure tier's behaviour unchanged when no effectful rules are configured.
