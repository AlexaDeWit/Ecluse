---
id: S23
title: DenyIfCVE rule
milestone: M5 — Effectful rules & CVE
status: not-started
depends-on: [S03, S22]
test-tier: [unit]
arch-refs:
  - docs/architecture/rules-engine.md#initial-rule-set
  - docs/architecture/rules-engine.md#cve-subsystem
  - docs/architecture/configuration.md#rule-configuration-format
pr: null
---

# S23 — `DenyIfCVE` rule

> Milestone **M5** · depends on: [S03](S03-config-loader.md), [S22](S22-cve-sync.md) · tier: unit

**Goal.** The first effectful rule: deny a version that matches a known advisory,
querying the in-memory `CVELookup` index (S22) through the effectful tier (S21).

**Acceptance criteria.**
- [ ] `DenyIfCVE` evaluates a version against `CVELookup`; a match → `Deny` (with the
  advisory IDs in the reason for the audit trail); no match → `Abstain`; lookup
  failure → `Unavailable` (fail-closed, S21). — _rules-engine.md#cve-subsystem, #effectful-rule-failure_
- [ ] Wired into the rule config decoder (S03) with a default precedence consistent
  with the deny-over-allow posture. — _configuration.md#rule-configuration-format_
- [ ] Because the index is in memory, evaluation does no network IO on the hot path
  (the `Unavailable` path is for an empty/unloaded index pre-first-sync, already
  guarded by readiness). — _rules-engine.md#cve-subsystem_

**File fence.**
- `src/Ecluse/Rules/Types.hs`, `src/Ecluse/Rules.hs` — add `DenyIfCVE` (constructor + `evalRule` arm using the effectful context).
- `src/Ecluse/Config.hs` — decode `DenyIfCVE` (additive).
- `test/unit/Ecluse/RulesSpec.hs` — match→deny (with IDs), no-match→abstain, lookup-fail→unavailable.

**Test tier.** Unit — with a fake `CVELookup` in `EvalContext`.

**Notes / risks.** Keep the rule thin — all sync/index logic is S22, all tier
machinery is S21. The reason string should name the advisory IDs so denials are
explainable (the audit-trail posture). This completes the launch rule set
(AllowScope, AllowIfPublishedBefore, DenyHasInstallScripts, DenyIfCVE).
