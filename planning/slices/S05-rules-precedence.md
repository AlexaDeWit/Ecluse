---
id: S05
title: Rules precedence alignment
milestone: M0 — Shell, seams & foundations
status: not-started
depends-on: []
test-tier: [unit]
arch-refs:
  - docs/architecture/rules-engine.md#rules-engine
  - docs/architecture/rules-engine.md#evaluation-model
  - docs/architecture/configuration.md#rule-policy
pr: null
---

# S05 — Rules precedence alignment

> Milestone **M0** · depends on: — (root) · tier: unit

**Goal.** Bring `Ecluse.Rules` to the end-state evaluation model: each rule carries
an integer **precedence**, and selection is the **highest-precedence
non-abstaining rule** (deny beats allow at equal precedence), making the rule set
**order-independent** except for the equal-precedence deny tiebreak and the order
abstain reasons are gathered. Today the code selects by list order — this is the
one current/end-state gap.

**Acceptance criteria.**
- [ ] Each rule carries a precedence (a field on a wrapper such as
  `PrecededRule { rulePrecedence :: Int, rule :: Rule }`, or precedence threaded
  through `evalRules`) — **a field, not an `Ord Rule` instance** (equal precedence is
  legal, so a derived total `Ord` would be unlawful). — _rules-engine.md#evaluation-model_
- [ ] `evalRules` selects via a `maximumBy` over a `(precedence, deny-before-allow)`
  comparator; equal-precedence `Deny` beats `Allow`; all-abstain ⇒ `DeniedByDefault`
  with reasons collected in order. — _rules-engine.md#evaluation-model_
- [ ] Built-in **deny rules default to higher precedence than allow rules** (so
  "any deny overrides any allow" holds out of the box), but an operator can rank a
  specific allow above a specific deny. — _rules-engine.md#rules-engine_
- [ ] **Per-type default precedences** are defined (e.g. AllowScope 300,
  DenyHasInstallScripts 200, AllowIfPublishedBefore 100) — each rule *type's*
  default when `precedence` is omitted, independent of **which** rules ship enabled
  (that is the default policy — S03's concern). — _configuration.md#rule-policy_
- [ ] **Properties (hedgehog):** deny-by-default holds; deny-precedence over allows
  at equal precedence; **order-independence** (shuffling the rule list does not
  change the decision, modulo the audit-reason order); an operator-elevated allow
  can outrank a lower deny.

**File fence.**
- `src/Ecluse/Rules/Types.hs` — add the precedence representation.
- `src/Ecluse/Rules.hs` — precedence-based `evalRules`; keep `evalRule` pure/total.
- `test/unit/Ecluse/RulesSpec.hs` — extend properties (the existing suite is the model).
- `docs/` — only if a rules-engine doc detail needs reconciling (it should already match).

**Test tier.** Unit — the rules engine is the property-test centrepiece; this slice
strengthens those invariants.

**Notes / risks.** Pure and independent — a strong Wave-1/early pull. Keep the
`Decision`/`RuleOutcome` types stable so S09 (filtering), S11 (responses), and the
effectful CVE rules (S21 `Unavailable`; S23 `DenyIfCVE` / `AllowIfRemediatesCve`,
the high-precedence remediation allow) layer on cleanly. Do not add the
`Unavailable` outcome here — that is S21 (it needs the effectful tier). This slice
is pure-tier only.
