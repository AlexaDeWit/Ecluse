-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | What an operator /selects/: the closed built-in rule vocabulary, and the precedence
at which each rule competes.

This is the config-facing half of the rules engine. A 'Rule' is __evaluation-agnostic
data__: it says /what/ a rule is, never /how/ it is evaluated. How a rule decides is a
separate concern that lives in "Ecluse.Core.Rules" ('Ecluse.Core.Rules.evalRule'
dispatches over this data; the engine wraps it in a 'Ecluse.Core.Rules.PreparedRule' to
run it), and what an evaluation /returns/ lives in "Ecluse.Core.Rules.Decision".
-}
module Ecluse.Core.Rules.Policy (
    -- * The built-in rule vocabulary
    Rule (..),
    DenyIfCveParams (..),
    ruleName,

    -- * Precedence
    PrecededRule (..),
    defaultPrecedence,
    defaultAllowIfOlderThanPrecedence,
    defaultAllowIfRemediatesCvePrecedence,
    defaultAllowScopePrecedence,
    defaultDenyIfCvePrecedence,
    defaultAllowByIdentityPrecedence,
    defaultDenyInstallTimeExecutionPrecedence,
    defaultDenyByIdentityPrecedence,
) where

import Data.Time (NominalDiffTime)
import Ecluse.Core.Package (Scope)
import Ecluse.Core.Rules.Decision (FailureAlignment)

{- | The closed, evaluation-agnostic vocabulary of __built-in__ rules an operator
selects and refines in config. Most built-in rules reason only over the
'Ecluse.Core.Package.PackageDetails' an adapter already fetched;
'AllowIfRemediatesCve' and 'DenyIfCve' additionally consult the local advisory
database through the boot-bound 'Ecluse.Core.Rules.RuleDeps'.

This is __data, not the engine's runtime representation__: a small, inspectable,
@Eq@\/@Show@ enum so config can parse, patch (override a rule's parameters), and name
each rule. "Ecluse.Core.Rules" turns it into the engine's runtime
'Ecluse.Core.Rules.PreparedRule' (binding /how/ it is evaluated) for evaluation. It
carries no allow\/deny "direction" -- whether a rule admits or blocks is simply what
its evaluation returns.

It is also the __security boundary__ on what config can express: untrusted config only
ever yields closed 'Rule' data, never arbitrary computation. A rule whose evaluation
performs IO ('AllowIfRemediatesCve', 'DenyIfCve') is a plain constructor here that
'Ecluse.Core.Rules.evalRule' dispatches on; arbitrary evaluation closures are a
code-layer capability, never reachable from config.
-}
data Rule
    = -- | Unconditionally allow every package under the given scope.
      AllowScope Scope
    | {- | Allow a version only if it was published at least this long ago.
      Guards against race-to-publish supply-chain attacks where an attacker
      publishes a malicious version and hopes it is consumed before takedown.
      -}
      AllowIfOlderThan NominalDiffTime
    | {- | Deny any package version that runs code at install time -- npm install
      scripts, a RubyGems native-extension build, a PyPI sdist build backend -- a
      common arbitrary-code-execution vector. Yields no decision otherwise.
      -}
      DenyInstallTimeExecution
    | {- | A hard deny for a specific package or package@version. Evaluated at top
      precedence (above AllowScope) as a post-mirror revocation mechanism.
      -}
      DenyByIdentity Text
    | {- | Allow a specific package or package\@version by exact identity -- the
      allow twin of 'DenyByIdentity' and the operator's explicit escape hatch, e.g.
      for a security fix published under a version string the remediation fast
      lane's exact-match probe cannot see. Top of the allow band, still under every
      deny default.
      -}
      AllowByIdentity Text
    | {- | Fast-track a version a synced advisory names as its __exact fix__, so a
      security patch is admitted immediately rather than waiting out the
      publish-age quarantine. Effectful: it consults the local advisory database
      ('Ecluse.Core.Cve.CveLookup') through the boot-bound
      'Ecluse.Core.Rules.RuleDeps', and abstains when no database is loaded, when
      the version is not an exact fixed bound, or when the version still sits
      inside another advisory's affected range.
      -}
      AllowIfRemediatesCve
    | {- | Deny a version a synced advisory records as __affected__, at or above the
      configured severity threshold -- the deny direction over the same advisory
      database as 'AllowIfRemediatesCve', with the deliberately __opposite failure
      mode__: where an unconfirmable remediation merely falls back to the
      quarantine, an unanswerable deny check refuses the version (unless the
      operator configured it fail-open; see 'DenyIfCveParams'). Ships opt-in, not
      in the default policy: enabled before the mirror is warmed, it would deny
      the historical versions an existing build already depends on.
      -}
      DenyIfCve DenyIfCveParams
    deriving stock (Eq, Show)

{- | 'DenyIfCve''s configured behaviour -- a separate record rather than fields on
the constructor, so its selectors stay total under the sum (@-Wpartial-fields@).
-}
data DenyIfCveParams = DenyIfCveParams
    { dicMinSeverity :: Double
    {- ^ The CVSS base score (0 to 10) at or above which an affecting advisory
    denies; below it the advisory is noted in the audit trail but does not block.
    A qualitative label counts as its band's ceiling, and an unscored advisory
    counts as above every threshold ('Ecluse.Core.Cve.severityAtLeast'): severity
    that cannot be proven low must not slip a deny gate.
    -}
    , dicOnUnavailable :: FailureAlignment
    {- ^ How the rule resolves when the advisory database cannot answer (not
    loaded, failing, or timed out): 'Ecluse.Core.Rules.Decision.FailDeny' refuses
    the version (fail-closed, the shipped default),
    'Ecluse.Core.Rules.Decision.FailNoDecision' skips the rule (fail-open, for the
    operator whose availability outranks a blind gate; the skip is recorded in the
    decision's audit reasons).
    -}
    }
    deriving stock (Eq, Show)

{- | A stable, human-facing name for a rule -- its identity, derived from the data: the
boot-order tiebreak and the credited identity in logs and denial messages.
-}
ruleName :: Rule -> Text
ruleName = \case
    AllowScope{} -> "AllowScope"
    AllowIfOlderThan{} -> "AllowIfOlderThan"
    DenyInstallTimeExecution -> "DenyInstallTimeExecution"
    DenyByIdentity{} -> "DenyByIdentity"
    AllowByIdentity{} -> "AllowByIdentity"
    AllowIfRemediatesCve -> "AllowIfRemediatesCve"
    DenyIfCve{} -> "DenyIfCve"

{- | A 'Rule' paired with the integer precedence at which it competes (higher
wins). This is config's resolved-policy element; "Ecluse.Core.Rules" prepares it into
the engine's runtime rule, whose boot-time ordering ('Ecluse.Core.Rules.bootOrder')
turns precedence -- and, at equal precedence, the rule name -- into the single total
order the engine walks.

Precedence is a __field, not an @Ord@ instance__: equal precedence between two rules
is legal (it is resolved by name in the boot order), so a total derived 'Ord' would
be non-antisymmetric -- unlawful and misleading. This mirrors
'Ecluse.Core.Version.Version', whose ordering likewise goes through a function rather
than a derived instance.
-}
data PrecededRule = PrecededRule
    { rulePrecedence :: Int
    -- ^ The precedence at which this rule competes; higher wins.
    , prRule :: Rule
    -- ^ The rule itself.
    }
    deriving stock (Eq, Show)

{- | The default precedence for a rule /type/ -- used when a policy omits an
explicit precedence for a rule.

The rule types climb one ladder, most-passive to most-decisive:

@AllowIfOlderThan@ (100) < @AllowIfRemediatesCve@ (150) < @AllowScope@ (200) <
@DenyIfCve@ (225) < @AllowByIdentity@ (250) < @DenyInstallTimeExecution@ (300) <
@DenyByIdentity@ (400)@

Two placements carry the design and are worth stating plainly:

* __@DenyByIdentity@ and @DenyInstallTimeExecution@ default strictly above every
  allow__, so a blanket "any deny overrides any allow" holds for them out of the
  box: revocation and the install-script deny keep the last word.
* __@DenyIfCve@ is the deliberate exception__: it sits /below/ @AllowByIdentity@ so
  an operator's exact-identity allow -- the explicit "I have decided this specific
  version must ship" escape hatch -- overrides an advisory deny (a graceful pin for
  a false positive or an accepted risk), while still sitting /above/ the passive age
  gate, the remediation lane, and a scope allow-list, so an unpinned affected version
  is denied despite them.

An operator may still raise a /specific/ allow above a /specific/ deny (or vice
versa) with an explicit precedence -- the per-type defaults set only the
out-of-the-box ordering.
-}
defaultPrecedence :: Rule -> Int
defaultPrecedence = \case
    AllowIfOlderThan{} -> defaultAllowIfOlderThanPrecedence
    AllowIfRemediatesCve -> defaultAllowIfRemediatesCvePrecedence
    AllowScope{} -> defaultAllowScopePrecedence
    DenyIfCve{} -> defaultDenyIfCvePrecedence
    AllowByIdentity{} -> defaultAllowByIdentityPrecedence
    DenyInstallTimeExecution -> defaultDenyInstallTimeExecutionPrecedence
    DenyByIdentity{} -> defaultDenyByIdentityPrecedence

{- | Default precedence of 'AllowIfOlderThan': the lowest band, a passive
quarantine that yields to an explicit allow-list and to every deny.
-}
defaultAllowIfOlderThanPrecedence :: Int
defaultAllowIfOlderThanPrecedence = 100

{- | Default precedence of 'AllowIfRemediatesCve': above the passive age
quarantine, which is the point of the fast lane -- a security fix is admitted
immediately instead of waiting out @min-age@ -- but below 'AllowScope', so a
scoped package an operator already trusts never pays the advisory probe and the
more explicit rule keeps the audit credit.
-}
defaultAllowIfRemediatesCvePrecedence :: Int
defaultAllowIfRemediatesCvePrecedence = 150

{- | Default precedence of 'AllowScope': above the passive age quarantine -- an
explicit allow-list of a trusted internal scope is a stronger statement than the
time gate -- but still below every deny.
-}
defaultAllowScopePrecedence :: Int
defaultAllowScopePrecedence = 200

{- | Default precedence of 'DenyIfCve': above the passive age gate, the
remediation lane, and a scope allow-list, so an affected version is denied
despite them -- but deliberately /below/ 'AllowByIdentity', so an operator's
exact-identity allow can pin a specific version past a false-positive or
risk-accepted advisory. The one deny type that is not strictly above every allow
(see 'defaultPrecedence').
-}
defaultDenyIfCvePrecedence :: Int
defaultDenyIfCvePrecedence = 225

{- | Default precedence of 'AllowByIdentity': the top of the allow band -- an
exact identity is the most explicit allow an operator can state. It sits above
'DenyIfCve' (the identity pin overrides an advisory deny) but still strictly
below the 'DenyInstallTimeExecution' and 'DenyByIdentity' defaults, so the
install-script deny and revocation keep the last word.
-}
defaultAllowByIdentityPrecedence :: Int
defaultAllowByIdentityPrecedence = 250

{- | Default precedence of 'DenyInstallTimeExecution': the deny band, strictly above
every allow default, so a matching deny overrides any allow out of the box.
-}
defaultDenyInstallTimeExecutionPrecedence :: Int
defaultDenyInstallTimeExecutionPrecedence = 300

{- | Default precedence of 'DenyByIdentity': the top precedence, strictly above
every other rule (including explicit allow-lists), to serve as a hard revocation.
-}
defaultDenyByIdentityPrecedence :: Int
defaultDenyByIdentityPrecedence = 400
