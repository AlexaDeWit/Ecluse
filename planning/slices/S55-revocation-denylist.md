---
id: S55
title: Revocation denylist, DenyByIdentity (hard-deny operator revocation)
milestone: M8, Release hardening
status: not-started
depends-on: [S03, S05]
test-tier: [unit]
arch-refs:
  - docs/architecture/rules-engine.md#rule-precedence
  - docs/architecture/configuration.md#rule-policy
  - threat-modelling/ecluse.json (threat #13, Registry B)
pr: null
---

# S55, Revocation denylist: `DenyByIdentity` (hard deny)

> Milestone **M8** · depends on: [S03](S03-config-loader.md), [S05](S05-rules-precedence.md) · tier: unit

**Goal.** A pure, operator-configured **deny-by-identity** rule, deny a specific
package, or a specific `package@version`, that is a **hard deny**: top precedence,
**not** overridable by `AllowScope` (the deliberate exception to the allow-over-deny
precedence of #13). It is the post-mirror **revocation** enabler: it halts re-admission
of a known-bad version on the serve path and re-mirroring at the worker ingest re-check
([#414](https://github.com/AlexaDeWit/Ecluse/issues/414)), so an operator can revoke a
version *before*, or without, an upstream yank. Paired with an operator **purge** of the
version from Registry B (which removes the already-mirrored *trusted* copy, since the rules
never run on trusted content), it is the complete revocation path. Detection is out of
scope, delegated to operator scanning / upstream advisories; *what* to revoke is the
operator's decision (see threat #13, Registry B).

**Acceptance criteria.**
- [ ] `DenyByIdentity` matches a configured package name, or a `package@version`; a match →
  `Deny` (the matched identity in the reason, for the audit trail); no match → `Abstain`.
- [ ] It is a **hard deny**: evaluated at a precedence **above `AllowScope`**, so an
  allow-listed scope cannot override a revocation, the one deliberate exception to
  allow-over-deny (a revocation an allow-list could outrank is not a revocation).,  _rules-engine.md#rule-precedence_
- [ ] Pure, no IO; evaluated on the serve path, and honoured by the worker mirror-job
  ingest re-evaluation (#414) so a revoked identity is **neither served nor (re-)mirrored**.
- [ ] Wired into the rule config decoder (S03): a revocation list via config (additive).
- [ ] Does **not** reach an already-mirrored trusted copy (rules do not run on trusted, by
  design), the operator purges Registry B for that. The rule prevents re-admission and
  re-mirroring (the treadmill); the documented playbook is **deny first, then purge**.

**File scope.**
- `core/src/Ecluse/Core/Rules/Types.hs`, `core/src/Ecluse/Core/Rules.hs`, add
  `DenyByIdentity` (constructor + `evalRule` arm) at a hard-deny precedence.
- `src/Ecluse/Config.hs`, decode the revocation list + its precedence (additive).
- worker ingest path, ensure the re-eval (#414) honours `DenyByIdentity` (drop, not publish).
- `core/test/unit/Ecluse/RulesSpec.hs`, match→deny (with identity), no-match→abstain, and
  the precedence test: `DenyByIdentity` outranks an `AllowScope` for the same name.

**Test tier.** Unit.

**Notes / risks.** The precedence is the crux, this is the *only* rule that outranks an
allow, so the precedence test is the one a reviewer must check. The full revocation story
also needs the worker ingest re-eval (#414) for the re-mirror brake; without it a revoked
version is refused on serve but could still be re-mirrored. Detection is deliberately out of
scope. The typical pattern is the inverse, an upstream yank/security-hold changes or removes
the bytes first, after which re-mirroring cannot reproduce them and the operator purges the
stale copy; this rule is for the atypical **internal-yank-before-public-yank**.
