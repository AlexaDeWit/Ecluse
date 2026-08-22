-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Data types for the policy rules engine.

The evaluation model lives in "Ecluse.Core.Rules". This module holds only the
dependency-light types it operates on. Those are the closed built-in rule vocabulary
config selects from, a rule's per-version result, and the overall decision.

A 'Rule' is __evaluation-agnostic data__. It says /what/ a rule is, never /how/ it is
evaluated. How a rule decides is a separate concern that lives in "Ecluse.Core.Rules".
'Ecluse.Core.Rules.evalRule' dispatches over this data, and the engine wraps it in a
'Ecluse.Core.Rules.PreparedRule' to run it.
-}
module Ecluse.Core.Rules.Types (
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

    -- * Evaluation
    EvalContext (..),
    mkEvalContext,
    Reason,
    RuleVerdict (..),
    RuleEvaluation (..),
    FailureAlignment (..),
    Decision (..),

    -- * Unavailability
    Transience (..),
    RetryAfter (..),
) where

import Data.Time (NominalDiffTime, UTCTime)
import Ecluse.Core.Cve (DbEtag)
import Ecluse.Core.Package (Scope)

{- | The closed, evaluation-agnostic vocabulary of __built-in__ rules an operator
selects and refines in config. Most built-in rules reason only over the
'Ecluse.Core.Package.PackageDetails' an adapter already fetched.
'AllowIfRemediatesCve' and 'DenyIfCve' also consult the local advisory database
through the boot-bound 'Ecluse.Core.Rules.RuleDeps'.

This is __data, not the engine's runtime representation__. It is a small, inspectable,
@Eq@\/@Show@ enum, so config can parse each rule, patch it (override its parameters),
and name it. "Ecluse.Core.Rules" turns it into the engine's runtime
'Ecluse.Core.Rules.PreparedRule', binding /how/ it is evaluated. It carries no
allow\/deny "direction": whether a rule admits or blocks is simply what its evaluation
returns.

It is also the __security boundary__ on what config can express. Untrusted config only
ever yields closed 'Rule' data, never arbitrary computation. A rule whose evaluation
performs IO ('AllowIfRemediatesCve', 'DenyIfCve') is a plain constructor here that
'Ecluse.Core.Rules.evalRule' dispatches on. An arbitrary evaluation closure is a
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
    | {- | Deny any package version that runs code at install time: npm install
      scripts, a RubyGems native-extension build, or a PyPI sdist build backend. That
      is a common arbitrary-code-execution vector. Yields no decision otherwise.
      -}
      DenyInstallTimeExecution
    | {- | A hard deny for a specific package or package@version. Evaluated at top
      precedence (above AllowScope) as a post-mirror revocation mechanism.
      -}
      DenyByIdentity Text
    | {- | Allow a specific package or package\@version by exact identity: the allow
      twin of 'DenyByIdentity', and the operator's explicit escape hatch. One use is a
      security fix published under a version string the remediation fast lane's
      exact-match probe cannot see. Top of the allow band, still under every deny
      default.
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
      configured severity threshold. This is the deny direction over the same advisory
      database as 'AllowIfRemediatesCve', with the deliberately __opposite failure
      mode__. Where an unconfirmable remediation merely falls back to the quarantine,
      an unanswerable deny check refuses the version, unless the operator configured it
      fail-open (see 'DenyIfCveParams'). It ships opt-in, not in the default policy:
      enabled before the mirror is warmed, it would deny the historical versions an
      existing build already depends on.
      -}
      DenyIfCve DenyIfCveParams
    deriving stock (Eq, Show)

{- | 'DenyIfCve''s configured behaviour: a separate record rather than fields on
the constructor, so its selectors stay total under the sum (@-Wpartial-fields@).
-}
data DenyIfCveParams = DenyIfCveParams
    { dicMinSeverity :: Double
    {- ^ The CVSS base score (0 to 10) at or above which an affecting advisory denies.
    Below it, the audit trail notes the advisory but it does not block. A qualitative
    label counts as its band's ceiling, and an unscored advisory counts as above every
    threshold ('Ecluse.Core.Cve.severityAtLeast'). Severity that cannot be proven low
    must not slip a deny gate.
    -}
    , dicOnUnavailable :: FailureAlignment
    {- ^ How the rule resolves when the advisory database cannot answer, whether it is
    not loaded, failing, or timed out. 'FailDeny' refuses the version: fail-closed, the
    shipped default. 'FailNoDecision' skips the rule: fail-open, for the operator whose
    availability outranks a blind gate. The decision's audit reasons record the skip.
    -}
    }
    deriving stock (Eq, Show)

{- | A stable, human-facing name for a rule: its identity, derived from the data. It is
the boot-order tiebreak and the credited identity in logs and denial messages.
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

{- | A 'Rule' paired with the integer precedence at which it competes (higher wins).
This is config's resolved-policy element. "Ecluse.Core.Rules" prepares it into the
engine's runtime rule. That rule's boot-time ordering ('Ecluse.Core.Rules.bootOrder')
turns precedence, and at equal precedence the rule name, into the single total order the
engine walks.

Precedence is a __field, not an @Ord@ instance__. Equal precedence between two rules is
legal, resolved by name in the boot order, so a total derived 'Ord' would be
non-antisymmetric: unlawful and misleading. This mirrors 'Ecluse.Core.Version.Version',
whose ordering likewise goes through a function rather than a derived instance.
-}
data PrecededRule = PrecededRule
    { rulePrecedence :: Int
    -- ^ The precedence at which this rule competes. Higher wins.
    , prRule :: Rule
    -- ^ The rule itself.
    }
    deriving stock (Eq, Show)

{- | The default precedence for a rule /type/, used when a policy omits an explicit
precedence for a rule.

The rule types climb one ladder, most-passive to most-decisive:

@AllowIfOlderThan@ (100) < @AllowIfRemediatesCve@ (150) < @AllowScope@ (200) <
@DenyIfCve@ (225) < @AllowByIdentity@ (250) < @DenyInstallTimeExecution@ (300) <
@DenyByIdentity@ (400)@

Two placements carry the design and are worth stating plainly:

* @DenyByIdentity@ and @DenyInstallTimeExecution@ default strictly above every allow.
  A blanket "any deny overrides any allow" therefore holds for them out of the box:
  revocation and the install-script deny keep the last word.
* The @DenyIfCve@ default is the deliberate exception. It sits /below/
  @AllowByIdentity@. An operator's exact-identity allow, the explicit "I have decided
  this specific version must ship" escape hatch, therefore overrides an advisory deny.
  That is a graceful pin for a false positive or an accepted risk. It still sits
  /above/ the passive age gate, the remediation lane, and a scope allow-list, so an
  unpinned affected version is denied despite them.

An operator may still raise a /specific/ allow above a /specific/ deny, or the reverse,
with an explicit precedence. The per-type defaults set only the out-of-the-box
ordering.
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

{- | Default precedence of 'AllowIfRemediatesCve': above the passive age quarantine.
That is the point of the fast lane, because a security fix is admitted immediately
instead of waiting out @min-age@. It is below 'AllowScope', so a scoped package an
operator already trusts never pays the advisory probe. The more explicit rule also keeps
the audit credit.
-}
defaultAllowIfRemediatesCvePrecedence :: Int
defaultAllowIfRemediatesCvePrecedence = 150

{- | Default precedence of 'AllowScope': above the passive age quarantine. An explicit
allow-list of a trusted internal scope is a stronger statement than the time gate. It
still sits below every deny.
-}
defaultAllowScopePrecedence :: Int
defaultAllowScopePrecedence = 200

{- | Default precedence of 'DenyIfCve': above the passive age gate, the remediation
lane, and a scope allow-list, so the rule denies an affected version despite them. It sits
deliberately /below/ 'AllowByIdentity', so an operator's exact-identity allow can pin a
specific version past a false-positive or risk-accepted advisory. It is the one deny
type that is not strictly above every allow (see 'defaultPrecedence').
-}
defaultDenyIfCvePrecedence :: Int
defaultDenyIfCvePrecedence = 225

{- | Default precedence of 'AllowByIdentity': the top of the allow band, because an
exact identity is the most explicit allow an operator can state. It sits above
'DenyIfCve', so the identity pin overrides an advisory deny. It still sits strictly
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

{- | Ambient information a rule may need that is not part of the package itself. It
carries the wall-clock "now" for age calculations, and the active advisory database's
identity for a decision's audit trail.
-}
data EvalContext = EvalContext
    { ctxNow :: UTCTime
    -- ^ The wall-clock "now" for age-based rules.
    , ctxAdvisoryEtag :: Maybe DbEtag
    {- ^ The advisory database 'DbEtag' active when this request was admitted, or
    'Nothing' when none is loaded, or on a path that does not consult one. It is the
    artifact a denial's audit line names as active at emit. It is deliberately __not__
    "the database this decision was evaluated against", because a shadow-swap may land
    mid-request. Resolved once per request.
    -}
    }
    deriving stock (Eq, Show)

{- | Assemble the ambient evaluation context: the __one__ assembly point for every
consumer, the packument sweep, the tarball gate, and the mirror worker's ingest
re-evaluation. What feeds a decision is therefore defined once, not at each call site.

The single point holds two contract terms. 'ctxNow' must come from the injected clock
the mount's decisions share ('Ecluse.Core.Server.Context.pdNow', which the worker's
bundle reuses), never an ad-hoc 'Data.Time.getCurrentTime'. The age gate then cannot
drift between contexts. 'ctxAdvisoryEtag' is __audit-only__ and never enters a rule's
decision, so a consumer that emits no audit line passes 'Nothing' without changing any
decision.
-}
mkEvalContext :: IO UTCTime -> IO (Maybe DbEtag) -> IO EvalContext
mkEvalContext now advisoryEtag = EvalContext <$> now <*> advisoryEtag

-- | A human-facing reason a rule attaches to its result, kept for the audit trail.
type Reason = Text

{- | What a single rule returns for a single package version: a __deterministic__
verdict. The rule computes its answer over the package, and for the effectful rules the
advisory database, then returns one of these. A rule cannot manufacture an
'Unavailable'. That is the distinction the resilience harness turns on. A verdict is a
decided value the harness takes at face value, never a fault it retries.

A verdict is __decisive__ iff it is 'Allow', 'Deny', or @'CannotVet' 'FailDeny' _@.
'NoDecision' and @'CannotVet' 'FailNoDecision' _@ are __non-decisive__ no-ops, and the
engine collects their reasons, in boot order, for the deny-by-default audit trail.
-}
data RuleVerdict
    = -- | This rule admits the package (with a human reason). Decisive.
      Allow Reason
    | -- | This rule blocks the package (with a human reason). Decisive.
      Deny Reason
    | -- | This rule has no opinion. The reason stays for the audit trail. A no-op.
      NoDecision Reason
    | {- | The rule reached the package but cannot vet it: a __deterministic,
      in-process absence__, not a fault. Today that means no advisory database is
      loaded. It carries its own __failure alignment__. A 'FailDeny' rule is decisive
      (fail-closed, → 'Undecidable'), and a 'FailNoDecision' rule is a no-op
      (fail-open). It carries __no__ 'Transience' on purpose: the absence is
      deterministic, so no in-process retry can change it, which is exactly why the
      harness must not route it through the retry\/breaker path.
      -}
      CannotVet FailureAlignment Reason
    deriving stock (Eq, Show)

{- | The outcome the resilience harness produces for one rule. Either the rule
'Decided', carrying any 'RuleVerdict' taken at face value, or the harness could not
obtain a verdict at all. An 'Unavailable' evaluation means the rule's IO threw, timed
out, or its source circuit breaker was open. Only the harness constructs 'Unavailable',
never a rule. The retry\/breaker machinery therefore provably reacts only to a fault the
harness itself observed, never to a verdict a rule deliberately returned.

Decisive iff it credits a 'Decision': a decisive 'RuleVerdict', or an
@'Unavailable' _ 'FailDeny' _@. A non-decisive verdict, or an @'Unavailable' _
'FailNoDecision' _@, is a no-op whose reason the engine gathers for the audit trail.
-}
data RuleEvaluation
    = -- | The rule returned a verdict, and the harness takes it at face value.
      Decided RuleVerdict
    | {- | The harness could not obtain a verdict: the rule's IO failed, timed out, or
      its source circuit breaker is open. It carries the rule's __failure alignment__
      (a 'FailDeny' evaluation is decisive → 'Undecidable', a 'FailNoDecision' one is a
      no-op) and a 'Transience' recording whether a retry can help. Only the harness
      builds this.
      -}
      Unavailable Transience FailureAlignment Reason
    deriving stock (Eq, Show)

{- | How a rule aligns when it cannot vet a version, or its evaluation faults.

There is deliberately __no @FailAllow@__: a failed or uncomputable check must never
/admit/ unvetted bytes. A rule whose verdict is load-bearing for safety fails
__closed__ ('FailDeny'). A remediation or allow-direction rule whose missing signal
should not block availability fails __open__ ('FailNoDecision').
-}
data FailureAlignment
    = -- | __Fail closed.__ An uncomputable result is decisive: the version is not admitted.
      FailDeny
    | -- | __Fail open.__ An uncomputable result is a no-op: the rule simply does not fire.
      FailNoDecision
    deriving stock (Eq, Show)

{- | The overall decision for a package version against a whole rule set.

A 'Decision' credits the deciding rule by __name__ ('Text'). A rule's stable identity is
its name (see 'ruleName'), independent of how the engine evaluates it.
-}
data Decision
    = -- | Admitted by the named rule, with its reason.
      Admitted Text Reason
    | -- | Blocked by the named rule, with its reason.
      Blocked Text Reason
    | {- | No rule was decisive. Deny-by-default; carries every non-decisive reason,
      in boot order, so the denial response can explain what was considered.
      -}
      BlockedByDefault [Reason]
    | {- | Undecidable: a 'FailDeny' rule that could not be computed __won__, so the
      version could not be vetted. Fail-closed, so it is not admitted. A packument
      filters it out like a denial, and a concrete artifact surfaces a @503@\/@500@ by
      the serve error model. The 'Transience' carries whether a retry can help, and the
      'Reason' is the audit reason.
      -}
      Undecidable Transience Reason
    deriving stock (Eq, Show)

{- | Whether an unavailability is expected to resolve on its own.

This is the single distinction the serve status mapping turns on. A transient cause
('WillResolve') is worth retrying, so it is a @503@. A permanent or internal one
('WontResolve') is not, so the serve layer never dresses it up as a retryable @503@. It
is a @500@. The resilience harness sets it from the nature of the failure. An upstream
outage, rate limit, timeout, or open breaker is transient. An internal or parse fault
is not.
-}
data Transience
    = {- | Transient: a retry may succeed (an advisory source briefly down, a
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
