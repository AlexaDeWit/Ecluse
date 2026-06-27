---
id: S21
title: Effectful rule tier (Unavailable, timeout/retry/breaker)
milestone: M5 — Effectful rules & CVE
status: merged
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

> **As-built (superseded by #381).** The separate effectful **tier**
> (`Ecluse.Core.Rules.Effectful`, `evalRulesEffectful`, `PrecededEffectfulRule`,
> `FailurePolicy`) is gone: pure and effectful rules are now **one representation**
> (`Rule`) evaluated by **one engine**. The "performance ordering" is now an
> intrinsic property — the boot-ordered walk runs effectful IO only up to the first
> decisive result, MAY speculate in parallel, and is deterministic (as-if sequential
> by boot order, later evaluations cancelled once the winner is known). The
> resilience harness (timeout / bounded retry+backoff / per-source breaker) is
> retained as a per-rule wrapper (`runEffectfulRule`). `FailurePolicy` is folded into
> the result's **`FailureAlignment`** (`FailDeny` | `FailNoDecision`) on
> `Unavailable`. See
> [Rules Engine](../../docs/architecture/rules-engine.md#rules-engine).

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

## As-built notes

- **`Unavailable Transience Text` (`RuleOutcome`) + `Undecidable Transience Text`
  (`Decision`).** The fourth outcome the effectful tier yields; it folds to the
  `Decision`'s `Undecidable` arm (fail-closed). `Transience`/`RetryAfter` __moved
  down__ from `Ecluse.Server.Response` to `Ecluse.Rules.Types` (they are rules-engine
  vocabulary) to break the cycle that would arise from `Rules.Types` importing them
  back from `Response`; `Response` re-exports them, so every existing importer is
  unchanged. Two effectful decision arms were added — `ApprovedEffectful Text Text` /
  `DeniedEffectful Text Text` — because an effectful rule is __not__ a member of the
  pure `Rule` enumeration, so it carries its deciding identity as a name, not a
  `Rule`. The pure tier's behaviour and types are otherwise untouched.
- **`Ecluse.Rules.Effectful`** earned its own module: the `EffectfulRule` interface
  (name, `erEval :: PackageDetails -> IO RuleOutcome`, config, `FailurePolicy`, a
  per-source breaker `TVar`), the resilience harness (`runEffectfulRule`: per-attempt
  timeout, bounded retry+backoff, circuit breaker), and the two-tier `evalRulesEffectful`.
  `Ecluse.Rules.evalRulesWithPrecedence` is exported so the effectful tier knows the
  pure winner's precedence and can __skip__ rules ranked below it (performance, not
  precedence). Tier-skip, fail-closed, cross-tier precedence, timeout/retry/breaker
  trip-half-open-reopen, and `onError: abstain` are all property-/example-tested in
  `test/unit/Ecluse/Rules/EffectfulSpec.hs` with an injected clock (`ctxNow`) and an
  injected backoff sleep — no real time passes.
- **Breaker reuse — pattern, not module (flagged decision).** S16 left its breaker
  __private__ to `Ecluse.Credential.Refresh` and explicitly deferred the
  shared-module decision to S21. To avoid editing a merged out-of-scope file (and
  colliding with parallel work), S21 __mirrors__ the same `Closed`/`Open`/`HalfOpen`
  shape and `admit`/`trip` gates in `Rules.Effectful` rather than extracting a shared
  `Ecluse.Breaker`. Extracting a common module remains a clean, deferrable refactor.
- **Pipeline wiring.** `PackumentDeps` gained `pdEffectfulRules :: [PrecededEffectfulRule]`
  (empty at the composition root — no effectful rule type is configurable until S23).
  `Ecluse.Package.Filter.filterPlanFromDecisions` was split out so the (IO) effectful
  decisions feed the same pure survivor/`latest` replay. Both serve paths now gate via
  `evalRulesEffectful`; an `Undecidable` version is filtered out of a packument like a
  denial (→ `503` when nothing survives) and surfaces as `503`/`500` on a concrete
  artifact, all reachability now exercised end-to-end in `PipelineSpec`.
- **One accepted-partial line.** `Ecluse.Rules.evalRulesWithPrecedence`'s `classify`
  must be total over `RuleOutcome`, but a __pure__ `Rule` provably never yields
  `Unavailable`; that arm (`Unavailable _ reason -> Left reason`) is therefore dead by
  construction and unreachable in a unit test. Flagged as an accepted partial per
  `docs/testing.md` rather than covered with a contrived test.
- **`EvalContext` unchanged — fetchers live in the rule closure.** The acceptance
  criterion "`EvalContext` extended with the fetchers/lookups the effectful tier
  needs" was satisfied by a different design: `EvalContext` stayed
  `newtype { ctxNow :: UTCTime }`, and each `EffectfulRule` carries its fetchers inside
  its `erEval :: PackageDetails -> IO RuleOutcome` closure, so no shared context field
  was added. The criterion's intent (the tier can reach what it needs) holds; the
  mechanism differs.
- **Breaker extraction — since landed.** The "mirror, not extract" decision above was
  the deferrable refactor it described; the shared `Ecluse.Breaker` was subsequently
  extracted (#189), and both `Credential.Refresh` and `Rules.Effectful` now consume it.
