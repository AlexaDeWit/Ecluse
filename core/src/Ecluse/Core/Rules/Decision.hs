-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | What the rules engine /returns/: the vocabulary of an evaluation's outcome.

A rule's per-version 'RuleVerdict', the 'RuleEvaluation' the resilience harness wraps it
in, and the overall 'Decision' for a package version against a whole rule set, plus the
ambient 'EvalContext' an evaluation reads.

This vocabulary is deliberately __independent of the rules that produce it__: a
'Decision' credits the deciding rule by __name__ ('Text'), never by
'Ecluse.Core.Rules.Policy.Rule', so what a decision /is/ does not depend on what the
built-in rule vocabulary happens to contain. The engine that joins the two lives in
"Ecluse.Core.Rules"; the rules an operator selects live in "Ecluse.Core.Rules.Policy".
-}
module Ecluse.Core.Rules.Decision (
    -- * The ambient evaluation context
    EvalContext (..),
    mkEvalContext,

    -- * A rule's result
    Reason,
    RuleVerdict (..),
    RuleEvaluation (..),
    FailureAlignment (..),

    -- * The overall decision
    Decision (..),

    -- * Unavailability
    Transience (..),
    RetryAfter (..),
) where

import Data.Time (UTCTime)
import Ecluse.Core.Cve (DbEtag)

{- | Ambient information a rule may need that is not part of the package itself:
the wall-clock "now" for age calculations, and the active advisory database's
identity for a decision's audit trail.
-}
data EvalContext = EvalContext
    { ctxNow :: UTCTime
    -- ^ The wall-clock "now" for age-based rules.
    , ctxAdvisoryEtag :: Maybe DbEtag
    {- ^ The advisory database 'DbEtag' active when this request was admitted, or
    'Nothing' when none is loaded (or on a path that does not consult one). It is
    the artifact a denial's audit line names as active at emit; it is
    deliberately __not__ "the database this decision was evaluated against",
    since a shadow-swap may land mid-request. Resolved once per request.
    -}
    }
    deriving stock (Eq, Show)

{- | Assemble the ambient evaluation context -- the __one__ assembly point for every
consumer (the packument sweep, the tarball gate, and the mirror worker's ingest
re-evaluation), so what feeds a decision is defined once, not at each call site.

The contract the single point holds: 'ctxNow' must come from the injected clock the
mount's decisions share ('Ecluse.Core.Server.Context.pdNow', which the worker's
bundle reuses), never an ad-hoc 'Data.Time.getCurrentTime', so the age gate cannot
drift between contexts; 'ctxAdvisoryEtag' is __audit-only__ (it never enters a rule's
decision), so a consumer that emits no audit line passes 'Nothing' without changing
any decision.
-}
mkEvalContext :: IO UTCTime -> IO (Maybe DbEtag) -> IO EvalContext
mkEvalContext now advisoryEtag = EvalContext <$> now <*> advisoryEtag

-- | A human-facing reason a rule attaches to its result, kept for the audit trail.
type Reason = Text

{- | What a single rule returns for a single package version: a __deterministic__
verdict. The rule computes its answer -- over the package, and for the effectful rules
the advisory database -- and returns one of these. A rule cannot manufacture an
'Unavailable'; that is the distinction the resilience harness turns on. A verdict is a
decided value the harness takes at face value, never a fault it retries.

A verdict is __decisive__ iff it is 'Allow', 'Deny', or @'CannotVet' 'FailDeny' _@.
'NoDecision' and @'CannotVet' 'FailNoDecision' _@ are __non-decisive__ no-ops; the
engine collects their reasons (in boot order) for the deny-by-default audit trail.
-}
data RuleVerdict
    = -- | This rule admits the package (with a human reason). Decisive.
      Allow Reason
    | -- | This rule blocks the package (with a human reason). Decisive.
      Deny Reason
    | -- | This rule has no opinion; the reason is kept for the audit trail. A no-op.
      NoDecision Reason
    | {- | The rule reached the package but cannot vet it -- a __deterministic,
      in-process absence__, not a fault (today: no advisory database is loaded). It
      carries its own __failure alignment__: a 'FailDeny' rule is decisive
      (fail-closed, → 'Undecidable'), a 'FailNoDecision' rule is a no-op (fail-open).
      It carries __no__ 'Transience' on purpose: the absence is deterministic, so no
      in-process retry can change it -- which is exactly why the harness must not
      route it through the retry\/breaker path.
      -}
      CannotVet FailureAlignment Reason
    deriving stock (Eq, Show)

{- | The outcome the resilience harness produces for one rule: either the rule
'Decided' (any 'RuleVerdict', taken at face value), or the harness could not obtain a
verdict at all and the evaluation is 'Unavailable' -- the rule's IO threw, timed out,
or its source circuit breaker was open. __Only the harness constructs 'Unavailable'__;
a rule cannot, so the retry\/breaker machinery provably reacts only to a fault the
harness itself observed, never to a verdict a rule deliberately returned.

Decisive iff it credits a 'Decision': a decisive 'RuleVerdict', or an
@'Unavailable' _ 'FailDeny' _@. A non-decisive verdict, or an @'Unavailable' _
'FailNoDecision' _@, is a no-op whose reason is gathered for the audit trail.
-}
data RuleEvaluation
    = -- | The rule returned a verdict; the harness takes it at face value.
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
name (see 'Ecluse.Core.Rules.Policy.ruleName'), independent of how it is evaluated.
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
      version could not be vetted. Fail-closed -- it is not admitted (a packument
      filters it out like a denial; a concrete artifact surfaces a @503@\/@500@ by the
      serve error model). The 'Transience' carries whether a retry can help; the
      'Reason' is the audit reason.
      -}
      Undecidable Transience Reason
    deriving stock (Eq, Show)

{- | Whether an unavailability is expected to resolve on its own.

This is the single distinction the serve status mapping turns on: a transient cause
('WillResolve') is worth retrying (a @503@); a permanent or internal one
('WontResolve') is not, so it must not be dressed up as a retryable @503@ (it is a
@500@). The resilience harness sets it from the nature of the failure: an upstream
outage, rate limit, timeout, or open breaker is transient; an internal or parse
fault is not.
-}
data Transience
    = {- | Transient -- a retry may succeed (an advisory source briefly down, a
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
