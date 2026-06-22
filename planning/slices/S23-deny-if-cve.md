---
id: S23
title: CVE rules — DenyIfCVE + AllowIfRemediatesCve
milestone: M5 — Effectful rules & CVE
status: not-started
depends-on: [S03, S22]
test-tier: [unit]
arch-refs:
  - docs/architecture/rules-engine.md#cve-subsystem
  - docs/architecture/rules-engine.md#allowifremediatescve--remediation-fast-track
  - docs/architecture/configuration.md#rule-policy
  - docs/architecture/configuration.md#the-default-policy
pr: null
---

# S23 — CVE rules: `DenyIfCVE` + `AllowIfRemediatesCve`

> Milestone **M5** · depends on: [S03](S03-config-loader.md), [S22](S22-cve-sync.md) · tier: unit

**Goal.** The effectful CVE rules — both directions over the same in-memory
`CVELookup` index (S22), through the effectful tier (S21): **`DenyIfCVE`** blocks a
version *affected* by a known advisory, and **`AllowIfRemediatesCve`** fast-tracks a
version that *fixes* one past the publish-age quarantine.

**Acceptance criteria.**
- [ ] `DenyIfCVE` evaluates a version against `CVELookup`; a match → `Deny` (with the
  advisory IDs in the reason for the audit trail); no match → `Abstain`; lookup
  failure → `Unavailable` (fail-closed, S21). — _rules-engine.md#cve-subsystem, #effectful-rule-failure_
- [ ] `AllowIfRemediatesCve` evaluates a version against `CVELookup`: an advisory
  affects an **earlier** version of the package **and** this version is **outside**
  its affected range → `Allow` (remediated advisory IDs in the reason); otherwise →
  `Abstain`. **Lookup failure → `Abstain`, not `Unavailable`** — an unconfirmable
  remediation must not admit; it falls back to the quarantine. (The deliberate
  inverse of the deny direction.) — _rules-engine.md#allowifremediatescve--remediation-fast-track_
- [ ] Both wired into the rule config decoder (S03): `DenyIfCVE` at a default
  precedence consistent with deny-over-allow; `AllowIfRemediatesCve` at a **high**
  precedence (above the `min-age` quarantine) so a fix is admitted immediately. —
  _configuration.md#rule-policy, #the-default-policy_
- [ ] Because the index is in memory, evaluation does no network IO on the hot path
  (the `Unavailable` / abstain-on-failure paths cover an empty/unloaded index
  pre-first-sync, already guarded by readiness). — _rules-engine.md#cve-subsystem_

**File scope.**
- `src/Ecluse/Rules/Types.hs`, `src/Ecluse/Rules.hs` — add `DenyIfCVE` and
  `AllowIfRemediatesCve` (constructors + `evalRule` arms over the effectful context).
- `src/Ecluse/Config.hs` — decode both rule types + their default precedences (additive).
- `test/unit/Ecluse/RulesSpec.hs` — deny: match→deny (with IDs), no-match→abstain,
  lookup-fail→unavailable; remediation: fix→allow (with IDs), non-fix→abstain,
  lookup-fail→**abstain** (the inverse failure mode).

**Test tier.** Unit — with a fake `CVELookup` in `EvalContext`.

**Notes / risks.** Keep the rules thin — all sync/index logic is S22, all tier
machinery is S21; these arms only *interpret* `CVELookup` results in the two
directions. Both reasons name the advisory IDs so decisions are explainable (the
audit-trail posture). Mind the **opposite failure modes** — `Unavailable` for the
deny, `Abstain` for the remediation allow — that asymmetry is the point and the one
thing a reviewer must check. `AllowIfRemediatesCve` is the CVE-era addition to the
default policy; `DenyIfCVE` stays opt-in.
