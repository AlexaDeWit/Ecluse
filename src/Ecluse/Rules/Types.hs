{- | Data types for the policy rules engine.

The evaluation model lives in "Ecluse.Rules"; this module holds only the
types it operates on.
-}
module Ecluse.Rules.Types (
    -- * Rules
    Rule (..),

    -- * Precedence
    PrecededRule (..),
    defaultPrecedence,
    atDefaultPrecedence,
    defaultAllowIfPublishedBeforePrecedence,
    defaultAllowScopePrecedence,
    defaultDenyHasInstallScriptsPrecedence,

    -- * Evaluation
    EvalContext (..),
    RuleOutcome (..),
    Decision (..),
) where

import Data.Time (NominalDiffTime, UTCTime)
import Ecluse.Package (Scope)

{- | A single policy rule.

Rules come in two flavours. /Allow/ rules either allow a package or abstain
(they never deny), so that a later rule still gets the chance to allow. /Deny/
rules either deny a package or abstain. Selection is by precedence, not list
order — see 'Ecluse.Rules.evalRules' and 'PrecededRule'.
-}
data Rule
    = -- | Unconditionally allow every package under the given scope.
      AllowScope Scope
    | {- | Allow a version only if it was published at least this long ago.
      Guards against race-to-publish supply-chain attacks where an attacker
      publishes a malicious version and hopes it is consumed before takedown.
      -}
      AllowIfPublishedBefore NominalDiffTime
    | {- | Deny any package version that runs install scripts (a common vector
      for arbitrary code execution at install time). Abstains otherwise.
      -}
      DenyHasInstallScripts
    deriving stock (Eq, Show)

{- | A 'Rule' paired with the integer precedence at which it competes (higher
wins). 'Ecluse.Rules.evalRules' selects the highest-precedence non-abstaining
rule; at equal precedence a deny beats an allow.

Precedence is a __field, not an @Ord Rule@ instance__: equal precedence between
two rules is legal (it is the deny-over-allow tiebreak), so a total derived 'Ord'
would be non-antisymmetric — unlawful and misleading. This mirrors
'Ecluse.Version.Version', whose ordering likewise goes through a function rather
than a derived instance.
-}
data PrecededRule = PrecededRule
    { rulePrecedence :: Int
    -- ^ The precedence at which this rule competes; higher wins.
    , prRule :: Rule
    -- ^ The rule itself.
    }
    deriving stock (Eq, Show)

{- | The default precedence for a rule /type/ — used when a policy omits an
explicit precedence for a rule.

__Every deny type defaults strictly above every allow type__, so "any deny
overrides any allow" holds out of the box. The three rule types occupy two
bands: the allow band (@AllowIfPublishedBefore@ <
'defaultAllowScopePrecedence'), then the deny band
('defaultDenyHasInstallScriptsPrecedence') strictly above both. An operator may
still elevate a /specific/ allow above a /specific/ deny by giving it a higher
explicit precedence — the per-type defaults set only the out-of-the-box ordering.
-}
defaultPrecedence :: Rule -> Int
defaultPrecedence = \case
    AllowIfPublishedBefore{} -> defaultAllowIfPublishedBeforePrecedence
    AllowScope{} -> defaultAllowScopePrecedence
    DenyHasInstallScripts -> defaultDenyHasInstallScriptsPrecedence

-- | Pair a rule with its type's 'defaultPrecedence'.
atDefaultPrecedence :: Rule -> PrecededRule
atDefaultPrecedence r = PrecededRule (defaultPrecedence r) r

{- | Default precedence of 'AllowIfPublishedBefore': the lowest band, a passive
quarantine that yields to an explicit allow-list and to every deny.
-}
defaultAllowIfPublishedBeforePrecedence :: Int
defaultAllowIfPublishedBeforePrecedence = 100

{- | Default precedence of 'AllowScope': above the passive age quarantine — an
explicit allow-list of a trusted internal scope is a stronger statement than the
time gate — but still below every deny.
-}
defaultAllowScopePrecedence :: Int
defaultAllowScopePrecedence = 200

{- | Default precedence of 'DenyHasInstallScripts': the deny band, strictly above
every allow default, so a matching deny overrides any allow out of the box.
-}
defaultDenyHasInstallScriptsPrecedence :: Int
defaultDenyHasInstallScriptsPrecedence = 300

{- | Ambient information a rule may need that is not part of the package itself
(the wall-clock "now" for age calculations).
-}
newtype EvalContext = EvalContext
    { ctxNow :: UTCTime
    }
    deriving stock (Eq, Show)

-- | The verdict of a single rule against a single package version.
data RuleOutcome
    = -- | This rule explicitly allows the package (with a human reason).
      Allow Text
    | {- | This rule explicitly denies the package (with a human reason). At
      equal precedence a 'Deny' beats an 'Allow'.
      -}
      Deny Text
    | -- | This rule has no opinion; the reason is kept for the audit trail.
      Abstain Text
    deriving stock (Eq, Show)

-- | The overall decision for a package version against a whole rule set.
data Decision
    = -- | Allowed by a specific rule, with its reason.
      Approved Rule Text
    | -- | Denied by a specific (deny) rule, with its reason.
      Denied Rule Text
    | {- | No rule allowed it. Deny-by-default; carries every rule's reason so
      the denial response can explain what was considered.
      -}
      DeniedByDefault [Text]
    deriving stock (Eq, Show)
