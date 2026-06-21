{- | Data types for the policy rules engine.

The evaluation model lives in "Ecluse.Rules"; this module holds only the
types it operates on.
-}
module Ecluse.Rules.Types (
    Rule (..),
    EvalContext (..),
    RuleOutcome (..),
    Decision (..),
) where

import Data.Time (NominalDiffTime, UTCTime)
import Ecluse.Package (Scope)

{- | A single policy rule.

Rules come in two flavours. /Allow/ rules either allow a package or abstain
(they never deny), so that a later rule still gets the chance to allow. /Deny/
rules either deny a package or abstain. A single matching deny rule overrides
every allow — see 'Ecluse.Rules.evalRules'.
-}
data Rule
    = -- | Unconditionally allow every package under the given scope.
      AllowScope Scope
    | -- | Allow a version only if it was published at least this long ago.
      -- Guards against race-to-publish supply-chain attacks where an attacker
      -- publishes a malicious version and hopes it is consumed before takedown.
      AllowIfPublishedBefore NominalDiffTime
    | -- | Deny any package version that runs install scripts (a common vector
      -- for arbitrary code execution at install time). Abstains otherwise.
      DenyHasInstallScripts
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
    | -- | This rule explicitly denies the package (with a human reason). A
      -- single 'Deny' overrides any 'Allow' in the rule set.
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
