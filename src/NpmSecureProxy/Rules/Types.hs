{- | Data types for the policy rules engine.

The evaluation model lives in "NpmSecureProxy.Rules"; this module holds only the
types it operates on.
-}
module NpmSecureProxy.Rules.Types (
    Rule (..),
    EvalContext (..),
    RuleOutcome (..),
    Decision (..),
) where

import Data.Time (NominalDiffTime, UTCTime)
import NpmSecureProxy.Package (Scope)

{- | A single policy rule.

The current rules are /allow/ rules: they either allow a package or abstain
(they never deny), so that a later rule still gets the chance to allow.
'RuleOutcome' carries a 'Deny' case for future deny rules.
-}
data Rule
    = -- | Unconditionally allow every package under the given scope.
      AllowScope Scope
    | -- | Allow a version only if it was published at least this long ago.
      -- Guards against race-to-publish supply-chain attacks where an attacker
      -- publishes a malicious version and hopes it is consumed before takedown.
      AllowIfPublishedBefore NominalDiffTime
    deriving stock (Eq, Show)

{- | Ambient information a rule may need that is not part of the package itself
(currently just the wall-clock "now" for age calculations).
-}
newtype EvalContext = EvalContext
    { ctxNow :: UTCTime
    }
    deriving stock (Eq, Show)

-- | The verdict of a single rule against a single package version.
data RuleOutcome
    = -- | This rule explicitly allows the package (with a human reason).
      Allow Text
    | -- | This rule explicitly denies the package (with a human reason).
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
    | -- | No rule allowed it. Deny-by-default; carries every rule's reason so
      -- the denial response can explain what was considered.
      DeniedByDefault [Text]
    deriving stock (Eq, Show)
