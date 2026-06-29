{- | Data types for the policy rules engine.

The evaluation model lives in "Ecluse.Core.Rules"; this module holds only the
dependency-light types it operates on — the closed built-in rule vocabulary config
selects from, a rule's per-version result, and the overall decision.

A 'Rule' is __evaluation-agnostic data__: it says /what/ a rule is, never /how/ it is
evaluated. How a rule decides is a separate concern that lives in "Ecluse.Core.Rules"
('Ecluse.Core.Rules.evalRule' dispatches over this data; the engine wraps it in a
'Ecluse.Core.Rules.PreparedRule' to run it).
-}
module Ecluse.Core.Rules.Types (
    -- * The built-in rule vocabulary
    Rule (..),
    ruleName,

    -- * Precedence
    PrecededRule (..),
    defaultPrecedence,
    atDefaultPrecedence,
    defaultAllowIfOlderThanPrecedence,
    defaultAllowScopePrecedence,
    defaultDenyInstallTimeExecutionPrecedence,
    defaultDenyByIdentityPrecedence,

    -- * Evaluation
    EvalContext (..),
    Reason,
    RuleResult (..),
    FailureAlignment (..),
    Decision (..),

    -- * Unavailability
    Transience (..),
    RetryAfter (..),
) where

import Data.Time (NominalDiffTime, UTCTime)
import Ecluse.Core.Package (Scope)

{- | The closed, evaluation-agnostic vocabulary of __built-in__ rules an operator
selects and refines in config. A built-in rule reasons only over the
'Ecluse.Core.Package.PackageDetails' an adapter already fetched.

This is __data, not the engine's runtime representation__: a small, inspectable,
@Eq@\/@Show@ enum so config can parse, patch (override a rule's parameters), and name
each rule. "Ecluse.Core.Rules" turns it into the engine's runtime
'Ecluse.Core.Rules.PreparedRule' (binding /how/ it is evaluated) for evaluation. It
carries no allow\/deny "direction" — whether a rule admits or blocks is simply what
its evaluation returns.

It is also the __security boundary__ on what config can express: untrusted config only
ever yields closed 'Rule' data, never arbitrary computation. A rule whose evaluation
performs IO (a future advisory\/CVE lookup) is added here as a plain constructor that
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
    | {- | Deny any package version that runs code at install time — npm install
      scripts, a RubyGems native-extension build, a PyPI sdist build backend — a
      common arbitrary-code-execution vector. Yields no decision otherwise.
      -}
      DenyInstallTimeExecution
    | {- | A hard deny for a specific package or package@version. Evaluated at top
      precedence (above AllowScope) as a post-mirror revocation mechanism.
      -}
      DenyByIdentity Text
    deriving stock (Eq, Show)

{- | A stable, human-facing name for a rule — its identity, derived from the data: the
boot-order tiebreak and the credited identity in logs and denial messages.
-}
ruleName :: Rule -> Text
ruleName = \case
    AllowScope{} -> "AllowScope"
    AllowIfOlderThan{} -> "AllowIfOlderThan"
    DenyInstallTimeExecution -> "DenyInstallTimeExecution"
    DenyByIdentity{} -> "DenyByIdentity"

{- | A 'Rule' paired with the integer precedence at which it competes (higher
wins). This is config's resolved-policy element; "Ecluse.Core.Rules" prepares it into
the engine's runtime rule, whose boot-time ordering ('Ecluse.Core.Rules.bootOrder')
turns precedence — and, at equal precedence, the rule name — into the single total
order the engine walks.

Precedence is a __field, not an @Ord@ instance__: equal precedence between two rules
is legal (it is resolved by name in the boot order), so a total derived 'Ord' would
be non-antisymmetric — unlawful and misleading. This mirrors
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

{- | The default precedence for a rule /type/ — used when a policy omits an
explicit precedence for a rule.

__Every deny type defaults strictly above every allow type__, so "any deny
overrides any allow" holds out of the box. The three rule types occupy two
bands: the allow band (@AllowIfOlderThan@ <
'defaultAllowScopePrecedence'), then the deny band
('defaultDenyInstallTimeExecutionPrecedence') strictly above both. An operator may
still elevate a /specific/ allow above a /specific/ deny by giving it a higher
explicit precedence — the per-type defaults set only the out-of-the-box ordering.
-}
defaultPrecedence :: Rule -> Int
defaultPrecedence = \case
    AllowIfOlderThan{} -> defaultAllowIfOlderThanPrecedence
    AllowScope{} -> defaultAllowScopePrecedence
    DenyInstallTimeExecution -> defaultDenyInstallTimeExecutionPrecedence
    DenyByIdentity{} -> defaultDenyByIdentityPrecedence

-- | Pair a rule with its type's 'defaultPrecedence'.
atDefaultPrecedence :: Rule -> PrecededRule
atDefaultPrecedence r = PrecededRule (defaultPrecedence r) r

{- | Default precedence of 'AllowIfOlderThan': the lowest band, a passive
quarantine that yields to an explicit allow-list and to every deny.
-}
defaultAllowIfOlderThanPrecedence :: Int
defaultAllowIfOlderThanPrecedence = 100

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

{- | Default precedence of 'DenyByIdentity': the top precedence, strictly above
every other rule (including explicit allow-lists), to serve as a hard revocation.
-}
defaultDenyByIdentityPrecedence :: Int
defaultDenyByIdentityPrecedence = 400

{- | Ambient information a rule may need that is not part of the package itself
(the wall-clock "now" for age calculations).
-}
newtype EvalContext = EvalContext
    { ctxNow :: UTCTime
    }
    deriving stock (Eq, Show)

-- | A human-facing reason a rule attaches to its result, kept for the audit trail.
type Reason = Text

{- | The verdict of a single rule against a single package version.

A result is __decisive__ iff it is 'Allow', 'Deny', or @'Unavailable' _ 'FailDeny' _@.
'NoDecision' and @'Unavailable' _ 'FailNoDecision' _@ are __non-decisive__ no-ops; the
engine collects their reasons (in boot order) for the deny-by-default audit trail.
-}
data RuleResult
    = -- | This rule admits the package (with a human reason). Decisive.
      Allow Reason
    | -- | This rule blocks the package (with a human reason). Decisive.
      Deny Reason
    | -- | This rule has no opinion; the reason is kept for the audit trail. A no-op.
      NoDecision Reason
    | {- | The rule could not be computed — its IO failed, timed out, or its source
      circuit breaker is open. It carries its own __failure alignment__: a
      'FailDeny' rule is decisive (fail-closed, → 'Undecidable'), a 'FailNoDecision'
      rule is a no-op (fail-open). The 'Transience' records whether a retry can help;
      the 'Reason' is the audit reason. The built-in rules never yield this.
      -}
      Unavailable Transience FailureAlignment Reason
    deriving stock (Eq, Show)

{- | How a rule's 'Unavailable' result aligns when the rule could not be computed.

There is deliberately __no @FailAllow@__: a failed or uncomputable check must never
/admit/ unvetted bytes. A rule whose verdict is load-bearing for safety fails
__closed__ ('FailDeny'); a remediation\/allow-direction rule whose missing signal
should not block availability fails __open__ ('FailNoDecision').
-}
data FailureAlignment
    = -- | __Fail closed.__ An uncomputable result is decisive: the version is not admitted.
      FailDeny
    | -- | __Fail open.__ An uncomputable result is a no-op: the rule simply does not fire.
      FailNoDecision
    deriving stock (Eq, Show)

{- | The overall decision for a package version against a whole rule set.

The deciding rule is credited by __name__ ('Text'): a rule's stable identity is its
name (see 'ruleName'), independent of how it is evaluated.
-}
data Decision
    = -- | Admitted by the named rule, with its reason (was @Approved@\/@ApprovedEffectful@).
      Admitted Text Reason
    | -- | Blocked by the named rule, with its reason (was @Denied@\/@DeniedEffectful@).
      Blocked Text Reason
    | {- | No rule was decisive. Deny-by-default; carries every non-decisive reason,
      in boot order, so the denial response can explain what was considered.
      -}
      BlockedByDefault [Reason]
    | {- | Undecidable: a 'FailDeny' rule that could not be computed __won__, so the
      version could not be vetted. Fail-closed — it is not admitted (a packument
      filters it out like a denial; a concrete artifact surfaces a @503@\/@500@ by the
      serve error model). The 'Transience' carries whether a retry can help; the
      'Reason' is the audit reason.
      -}
      Undecidable Transience Reason
    deriving stock (Eq, Show)

-- ── unavailability ────────────────────────────────────────────────────────────

{- | Whether an unavailability is expected to resolve on its own.

This is the single distinction the serve status mapping turns on: a transient cause
('WillResolve') is worth retrying (a @503@); a permanent or internal one
('WontResolve') is not, so it must not be dressed up as a retryable @503@ (it is a
@500@). The resilience harness sets it from the nature of the failure: an upstream
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
