---
id: S05
title: Rules precedence alignment
milestone: M0, Shell, handles & foundations
status: merged
depends-on: []
test-tier: [unit]
arch-refs:
  - docs/architecture/rules-engine.md#rules-engine
  - docs/architecture/rules-engine.md#evaluation-model
  - docs/architecture/configuration.md#rule-policy
pr: null
---

# S05, Rules precedence alignment

> Milestone **M0** · depends on:, (root) · tier: unit

> **As-built (superseded by #381).** This slice's `maximumBy`
> `(precedence, deny-before-allow, identity)` selection has been replaced by a single
> **boot-ordered list** the engine walks to the first decisive result (see
> [Rules Engine → Evaluation model](../../docs/architecture/rules-engine.md#evaluation-model)).
> The equal-precedence **deny-over-allow** runtime tiebreak is **dropped**: at equal
> explicit precedence the rule **name** decides (deny-over-allow still holds out of
> the box via the higher deny defaults). `Abstain` is renamed `NoDecision`. The
> order-independence and shuffle properties this slice established are retained,
> re-expressed against the unified engine.

**Goal.** Bring `Ecluse.Rules` to the end-state evaluation model: each rule carries
an integer **precedence**, and selection is the **highest-precedence
non-abstaining rule** (deny beats allow at equal precedence), making the rule set
**order-independent** except for the equal-precedence deny tiebreak and the order
abstain reasons are gathered. Today the code selects by list order, this is the
one current/end-state gap.

**Acceptance criteria.**
- [ ] Each rule carries a precedence (a field on a wrapper such as
  `PrecededRule { rulePrecedence :: Int, rule :: Rule }`, or precedence threaded
  through `evalRules`), **a field, not an `Ord Rule` instance** (equal precedence is
  legal, so a derived total `Ord` would be unlawful).  _rules-engine.md#evaluation-model_
- [ ] `evalRules` selects via a `maximumBy` over a `(precedence, deny-before-allow)`
  comparator; equal-precedence `Deny` beats `Allow`; all-abstain ⇒ `DeniedByDefault`
  with reasons collected in order.  _rules-engine.md#evaluation-model_
- [ ] Built-in **deny rules default to higher precedence than allow rules** (so
  "any deny overrides any allow" holds out of the box), but an operator can rank a
  specific allow above a specific deny.  _rules-engine.md#rules-engine_
- [ ] **Per-type default precedences** are defined (e.g. AllowScope 300,
  DenyInstallTimeExecution 200, AllowIfPublishedBefore 100), each rule *type's*
  default when `precedence` is omitted, independent of **which** rules ship enabled
  (that is the default policy, S03's concern).  _configuration.md#rule-policy_
- [ ] **Properties (hedgehog):** deny-by-default holds; deny-precedence over allows
  at equal precedence; **order-independence** (shuffling the rule list does not
  change the decision, modulo the audit-reason order); an operator-elevated allow
  can outrank a lower deny.

**File scope.**
- `src/Ecluse/Rules/Types.hs`, add the precedence representation.
- `src/Ecluse/Rules.hs`, precedence-based `evalRules`; keep `evalRule` pure/total.
- `test/unit/Ecluse/RulesSpec.hs`, extend properties (the existing suite is the model).
- `docs/`, only if a rules-engine doc detail needs reconciling (it should already match).

**Test tier.** Unit, the rules engine is the property-test centrepiece; this slice
strengthens those invariants.

**Notes / risks.** Pure and independent, a strong Wave-1/early pull. Keep the
`Decision`/`RuleOutcome` types stable so S09 (filtering), S11 (responses), and the
effectful CVE rules (S21 `Unavailable`; S23 `DenyIfCVE` / `AllowIfRemediatesCve`,
the high-precedence remediation allow) layer on cleanly. Do not add the
`Unavailable` outcome here, that is S21 (it needs the effectful tier). This slice
is pure-tier only.

**As-built notes (PR #36).**
- **`PrecededRule { rulePrecedence :: Int, prRule :: Rule }`.** The precedence
  representation is the wrapper-with-a-field shape (the field is `prRule`, type-tagged
  per STYLE.md §6.3, not the sketch's bare `rule`), with no `Ord Rule` instance,  `evalRules` selects with `maximumBy` over a `(precedence, isDeny)` key (deny ranks
  above allow at equal precedence), exactly as specified. Helpers ship alongside:
  `defaultPrecedence :: Rule -> Int` and `atDefaultPrecedence :: Rule -> PrecededRule`.
- **Per-type default precedences as named bindings.** `AllowIfPublishedBefore`=**100**,
  `AllowScope`=**200**, `DenyInstallTimeExecution`=**300**, each an exported top-level
  binding (`defaultAllowIfPublishedBeforePrecedence`, etc.). These differ from the
  *illustrative* example values in the acceptance criterion (which paired AllowScope
  with 300 and DenyInstallTimeExecution with 200); the load-bearing invariant, **every
  deny default strictly above every allow default**, holds, with the two allow types
  in an ordered allow band (scope above the passive age quarantine) below the deny
  band.
