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
    defaultDenyInstallTimeExecutionPrecedence,

    -- * Evaluation
    EvalContext (..),
    RuleOutcome (..),
    Decision (..),

    -- * Unavailability
    Transience (..),
    RetryAfter (..),
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
    | {- | Deny any package version that runs code at install time — npm install
      scripts, a RubyGems native-extension build, a PyPI sdist build backend — a
      common arbitrary-code-execution vector. Abstains otherwise.
      -}
      DenyInstallTimeExecution
    deriving stock (Eq, Show)

{- | A 'Rule' paired with the integer precedence at which it competes (higher
wins). 'Ecluse.Rules.evalRules' selects the highest-precedence non-abstaining
rule; at equal precedence a deny beats an allow, and any remaining tie is broken
by rule identity rather than list order (see 'Ecluse.Rules.evalRules').

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
('defaultDenyInstallTimeExecutionPrecedence') strictly above both. An operator may
still elevate a /specific/ allow above a /specific/ deny by giving it a higher
explicit precedence — the per-type defaults set only the out-of-the-box ordering.
-}
defaultPrecedence :: Rule -> Int
defaultPrecedence = \case
    AllowIfPublishedBefore{} -> defaultAllowIfPublishedBeforePrecedence
    AllowScope{} -> defaultAllowScopePrecedence
    DenyInstallTimeExecution -> defaultDenyInstallTimeExecutionPrecedence

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

{- | Default precedence of 'DenyInstallTimeExecution': the deny band, strictly above
every allow default, so a matching deny overrides any allow out of the box.
-}
defaultDenyInstallTimeExecutionPrecedence :: Int
defaultDenyInstallTimeExecutionPrecedence = 300

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
    | {- | An effectful rule the evaluator needed could not be consulted — its IO
      failed, timed out, or its source circuit breaker is open. This is
      __fail-closed__: a version a needed rule could not vet is not admitted just
      because the scanner is unreachable. The 'Transience' records whether a retry
      can help (transient outage) or not (a permanent inability); the 'Text' is the
      audit reason. A pure rule never yields this — only the effectful tier does, and
      only when a rule whose verdict could still change the outcome was unreachable.
      -}
      Unavailable Transience Text
    deriving stock (Eq, Show)

{- | The overall decision for a package version against a whole rule set.

The first three arms are the pure tier's; the effectful tier ("Ecluse.Rules.Effectful")
adds the last three. An effectful approval\/denial carries the deciding rule's
__name__ ('Text') rather than a pure 'Rule', because an effectful rule is not a
member of the pure 'Rule' enumeration — its identity is just the name it logs under.
-}
data Decision
    = -- | Allowed by a specific pure rule, with its reason.
      Approved Rule Text
    | -- | Denied by a specific pure (deny) rule, with its reason.
      Denied Rule Text
    | {- | No rule allowed it. Deny-by-default; carries every rule's reason so
      the denial response can explain what was considered.
      -}
      DeniedByDefault [Text]
    | -- | Allowed by an effectful rule (named), with its reason.
      ApprovedEffectful Text Text
    | -- | Denied by an effectful rule (named), with its reason.
      DeniedEffectful Text Text
    | {- | Undecidable: an effectful rule whose verdict could still have changed the
      outcome could not be consulted, so the version could not be vetted. This is
      __fail-closed__ — it is not admitted (a packument filters it out like a denial;
      a concrete artifact surfaces a @503@\/@500@ by the serve error model). The
      'Transience' carries whether a retry can help; the 'Text' is the audit reason.
      -}
      Undecidable Transience Text
    deriving stock (Eq, Show)

-- ── unavailability ────────────────────────────────────────────────────────────

{- | Whether an unavailability is expected to resolve on its own.

This is the single distinction the serve status mapping turns on: a transient cause
('WillResolve') is worth retrying (a @503@); a permanent or internal one
('WontResolve') is not, so it must not be dressed up as a retryable @503@ (it is a
@500@). The effectful tier sets it from the nature of the failure: an upstream
outage, rate limit, timeout, or open breaker is transient; an internal or parse
fault is not.
-}
data Transience
    = {- | Transient — a retry may succeed (an advisory source briefly down, a
      timeout, an open circuit breaker). The optional 'RetryAfter' is the delay to
      suggest to the client.
      -}
      WillResolve (Maybe RetryAfter)
    | {- | Not expected to self-heal (an internal or parse error). Retrying cannot
      help, so the request is a @500@, never a @503@.
      -}
      WontResolve
    deriving stock (Eq, Show)

{- | A @Retry-After@ delay, in whole seconds. A 'newtype' so a raw count of seconds
is never confused with some other integer when it reaches the response header.
-}
newtype RetryAfter = RetryAfter Int
    deriving stock (Eq, Ord, Show)
