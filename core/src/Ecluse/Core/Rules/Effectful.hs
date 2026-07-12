-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The resilience harness for effectful rules: the per-attempt timeout, bounded
retry with backoff, and per-source circuit breaker wrapped around a rule evaluation
that does IO. "Ecluse.Core.Rules" attaches a 'Resilience' to each effectful rule at
'Ecluse.Core.Rules.prepare' and runs it through 'runResilient'; the pure built-ins
never enter this module.

A resilient evaluation runs under its breaker's admission gate, a per-attempt
timeout, and bounded retry with backoff. Any 'RuleVerdict' the rule returns -- a
deterministic 'CannotVet' included -- resets the breaker and is passed on 'Decided',
taken at face value and never retried; only a __fault__ the harness observes (a
timeout, an exception, or the breaker already open) advances the breaker and
resolves to @'Unavailable' transience alignment reason@, the alignment from the
rule's 'Resilience' (fail-closed 'FailDeny' or fail-open 'FailNoDecision'). Total:
'runResilient' never throws; a rule failure becomes a result.

The breaker timing reads the injected resilience clock ('resClock'), read fresh at
each breaker decision, so it is deterministic under test and independent of the
request snapshot the age rules hold constant. Reading it again after the retry run
means a tripped breaker's cooldown starts when the failure commits, not when the
run began.
-}
module Ecluse.Core.Rules.Effectful (
    -- * The resilience policy
    Resilience (..),
    EffectfulConfig (..),
    defaultEffectfulConfig,
    newBreaker,

    -- * Effectful-fault observation
    FaultReporter (..),
    reportFault,

    -- * Running an evaluation through it
    runResilient,
    backoffPolicy,
) where

import Control.Retry (
    RetryPolicyM (RetryPolicyM),
    RetryStatus (rsIterNumber),
    retrying,
 )
import Data.Time (NominalDiffTime, UTCTime)
import UnliftIO (timeout, tryAny)

import Ecluse.Core.Breaker (
    Breaker,
    BreakerReporter,
    admit,
    initialBreaker,
    recordFailure,
    recordSuccess,
    reportBreakerChange,
 )
import Ecluse.Core.Package (PackageDetails)
import Ecluse.Core.Rules.Types
import Ecluse.Core.Text (displayExceptionT)

{- | The resilience policy wrapped around an effectful rule's IO: the timeout\/retry\/
breaker knobs, the per-source circuit-breaker state, its observer, and the
__failure alignment__ an exhausted evaluation resolves to (fail-closed 'FailDeny' or
fail-open 'FailNoDecision'). The alignment rides on the prepared rule, folding away the
separate failure-policy the two-tier design once carried.
-}
data Resilience = Resilience
    { resConfig :: EffectfulConfig
    -- ^ The per-attempt timeout, retry budget\/backoff, and breaker threshold\/cooldown.
    , resAlignment :: FailureAlignment
    -- ^ Whether an exhausted evaluation fails closed ('FailDeny') or open ('FailNoDecision').
    , resBreaker :: TVar Breaker
    -- ^ This rule's per-source circuit-breaker state, shared across evaluations.
    , resBreakerReporter :: BreakerReporter
    {- ^ The observer this rule's breaker reports its state transitions to
    (@ecluse.rule.breaker.state@). Inert ('Ecluse.Core.Breaker.noBreakerReporter') for an
    unobserved rule; the composition root installs the live one.
    -}
    , resClock :: IO UTCTime
    {- ^ The injected wall clock the breaker reads for its admission gate and its
    cooldown arithmetic. 'Data.Time.getCurrentTime' in production, overridable under
    test for deterministic breaker timing. Deliberately separate from the request
    snapshot 'ctxNow' (which the age rules hold constant across a packument): the
    breaker is a wall-clock device, and reading it fresh at the point a failure commits
    is what makes the cooldown start when the failure is recorded, not when the retry
    run began.
    -}
    , resFaultReporter :: FaultReporter
    {- ^ The observer an exhausted evaluation reports its fault detail to (the rendered
    exception, or a timeout), so a live-database query fault is diagnosable from the
    operator log rather than collapsing to a bare @Unavailable@. Inert
    ('noFaultReporter') for an unobserved rule; the composition root installs the live
    one. It never reaches the client-facing decision message.
    -}
    }

{- | The observer an exhausted effectful evaluation reports its fault detail to: the
deciding rule's name and the rendered fault (an exception's 'displayException', or a
timeout). A telemetry-agnostic callback in the shape of 'Ecluse.Core.Breaker.BreakerReporter',
so the pure rules engine names no logger; the composition root closes a katip line over
it. Fires once per exhausted evaluation, never on a verdict or a still-cooling breaker.
-}
newtype FaultReporter = FaultReporter (Text -> Text -> IO ())

-- | Report one exhausted evaluation's fault: the rule name and the rendered detail.
reportFault :: FaultReporter -> Text -> Text -> IO ()
reportFault (FaultReporter report) = report

{- | Run one effectful rule evaluation through its 'Resilience' policy: the breaker
admission gate, then the per-attempt timeout under bounded retry, then the breaker
settlement. The rule's name tags the audit reason; the evaluator is the rule's raw
per-version IO with the evaluation context already applied. See the module header
for the verdict-vs-fault contract; 'Ecluse.Core.Rules.runEffectfulRule' is the
engine-level entry that dispatches a prepared rule here.
-}
runResilient :: Resilience -> Text -> (PackageDetails -> IO RuleVerdict) -> PackageDetails -> IO RuleEvaluation
runResilient res name evalAt pd = do
    admitted <- admitProbe res =<< resClock res
    if not admitted
        then -- Breaker open and still cooling down: fast-fail without running the
        -- rule's IO, the cheap path a sustained outage stays on. An open breaker
        -- is an infrastructural outage, so it is transient.
            pure (exhausted res name (transientCause (resConfig res)) "the rule source circuit breaker is open")
        else do
            result <- attemptWithRetry res evalAt pd
            -- Read the clock again, after the retry run: an exhausted result opens the
            -- breaker for its cooldown from the instant the failure is committed here,
            -- so the retry duration is not subtracted from the effective cooldown.
            settledNow <- resClock res
            settleOutcome res name settledNow result

{- Settle a finished retry run against the breaker: a returned verdict resets the
breaker and is passed on 'Decided'; an exhausted run advances the breaker and resolves
to the rule's aligned 'Unavailable'. -}
settleOutcome :: Resilience -> Text -> UTCTime -> Either (Transience, Text) RuleVerdict -> IO RuleEvaluation
settleOutcome res name now = \case
    Right verdict -> do
        commitBreaker res recordSuccess
        pure (Decided verdict)
    Left (transience, detail) -> do
        commitBreaker res (tripOnFailure (resConfig res) now)
        -- Surface the fault detail to the operator log before it collapses to the
        -- client-facing generic reason: an exhausted evaluation otherwise leaves only a
        -- bare 'Unavailable', hiding a live-database query fault's cause.
        reportFault (resFaultReporter res) name detail
        pure (exhausted res name transience "the rule could not be evaluated")

{- Attempt the rule's IO under the per-attempt timeout, retrying with backoff until the
retry budget is spent. 'Right' the rule's 'RuleVerdict' on success -- any verdict is
taken at face value and __not__ retried; 'Left' the transient 'Transience' when the
attempt faulted (an exception or a timeout), the only condition a retry might clear.
'retrying' re-runs solely on a 'Left', so a deterministic verdict never enters the
retry loop. -}
attemptWithRetry :: Resilience -> (PackageDetails -> IO RuleVerdict) -> PackageDetails -> IO (Either (Transience, Text) RuleVerdict)
attemptWithRetry res evalAt pd =
    retrying (backoffPolicy (ecBackoff (resConfig res))) shouldRetry (\_ -> attemptOnce res evalAt pd)
  where
    shouldRetry _ = pure . isLeft

{- | An 'ecBackoff' schedule compiled to a "Control.Retry" policy: the retry at
iteration n waits the n-th delay (microseconds) before it, and the policy stops
(yields 'Nothing') once the schedule is exhausted -- so the list's length is the retry
budget. @[]@ admits no retry (a single attempt); @[a, b]@ admits up to two. Inspect
the resulting delays without sleeping with 'Control.Retry.simulatePolicy'.
-}
backoffPolicy :: [Int] -> RetryPolicyM IO
backoffPolicy backoffs = RetryPolicyM (\rs -> pure (backoffs !!? rsIterNumber rs))

{- One attempt: run the rule's IO under the timeout, catching any exception. 'Right'
the rule's 'RuleVerdict' -- whatever it decided, a deterministic 'CannotVet' included,
is a decided value taken at face value. 'Left' the transient 'Transience' only when the
harness itself could not obtain a verdict: the rule's IO threw, or the attempt timed
out. Those are the sole retryable conditions -- a fault a later attempt might clear --
and the sole inputs to the breaker; a verdict is never either. -}
attemptOnce :: Resilience -> (PackageDetails -> IO RuleVerdict) -> PackageDetails -> IO (Either (Transience, Text) RuleVerdict)
attemptOnce res evalAt pd = do
    result <- tryAny (timeout (ecTimeout (resConfig res)) (evalAt pd))
    pure $ case result of
        Left e -> Left (transient, "the rule threw: " <> displayExceptionT e) -- the rule's IO threw
        Right Nothing -> Left (transient, "the attempt timed out") -- the attempt timed out
        Right (Just verdict) -> Right verdict -- a verdict is decided; never retried
  where
    transient = transientCause (resConfig res)

{- The result a faulted evaluation resolves to: @'Unavailable' transience alignment@ --
'WillResolve' for the infrastructural fault that produced it (a timeout, an exception,
or an open breaker); the alignment is the rule's own (fail-closed 'FailDeny' or
fail-open 'FailNoDecision'). The reason is carried for the audit trail. -}
exhausted :: Resilience -> Text -> Transience -> Text -> RuleEvaluation
exhausted res name transience reason = Unavailable transience (resAlignment res) (name <> ": " <> reason)

{- The transient 'Transience' an infrastructural failure (a timeout, an exception, an
open breaker) surfaces: retryable, carrying the rule's configured 'RetryAfter'. -}
transientCause :: EffectfulConfig -> Transience
transientCause cfg = WillResolve (ecRetryAfter cfg)

{- The breaker admission gate: defer the decision to 'Ecluse.Core.Breaker.admit' and
commit the breaker state it returns, reporting any change (a half-open recovery probe).
See 'Ecluse.Core.Breaker.admit' for the admission policy. -}
admitProbe :: Resilience -> UTCTime -> IO Bool
admitProbe res now = do
    (permitted, old, new) <- atomically $ do
        st <- readTVar (resBreaker res)
        let (p, st') = admit now st
        writeTVar (resBreaker res) st'
        pure (p, st, st')
    reportBreakerChange (resBreakerReporter res) old new
    pure permitted

{- Commit a breaker fold to this rule's breaker and report any observable state change
it makes (a trip, a reset). Reads the breaker before and after in one transaction so the
report reflects exactly the transition committed. -}
commitBreaker :: Resilience -> (Breaker -> Breaker) -> IO ()
commitBreaker res step = do
    (old, new) <- atomically $ do
        st <- readTVar (resBreaker res)
        let st' = step st
        writeTVar (resBreaker res) st'
        pure (st, st')
    reportBreakerChange (resBreakerReporter res) old new

{- Advance the breaker on a failed evaluation per this rule's configured threshold
and cooldown ('Ecluse.Core.Breaker.recordFailure'). -}
tripOnFailure :: EffectfulConfig -> UTCTime -> Breaker -> Breaker
tripOnFailure cfg = recordFailure (ecBreakerThreshold cfg) (ecBreakerCooldown cfg)

{- | The resilience knobs around an effectful rule's IO: a per-attempt timeout,
how many retries to make on failure with the backoff before each, and the breaker
threshold and cooldown. The breaker's timing reads the injected resilience clock
('resClock') fresh at failure commit, not the request snapshot 'ctxNow'.
-}
data EffectfulConfig = EffectfulConfig
    { ecTimeout :: Int
    {- ^ The per-attempt timeout in microseconds. An attempt that does not return
    within it is treated as a failure (a transient, retryable cause).
    -}
    , ecBackoff :: [Int]
    {- ^ The backoff delays in microseconds, one per retry, applied __before__ the
    corresponding retry attempt. Its length is the retry budget: @[]@ means the
    single initial attempt only, @[100, 200]@ means up to two retries after it.
    -}
    , ecBreakerThreshold :: Int
    -- ^ Consecutive exhausted-rule failures that trip the breaker.
    , ecBreakerCooldown :: NominalDiffTime
    {- ^ How long the breaker stays open (fast-failing the rule) before a single
    half-open probe is allowed to test recovery.
    -}
    , ecRetryAfter :: Maybe RetryAfter
    {- ^ The @Retry-After@ delay to suggest to a client when this rule's
    unavailability surfaces on a concrete-artifact request; 'Nothing' suggests none.
    -}
    }

{- | Sensible defaults for the resilience knobs: a 2-second per-attempt timeout, two
retries at 100ms then 250ms, and a breaker tripping after 5 consecutive failures and
cooling for 30 seconds. The caller supplies the rule's IO; the knobs are policy with
these defaults.
-}
defaultEffectfulConfig :: EffectfulConfig
defaultEffectfulConfig =
    EffectfulConfig
        { ecTimeout = 2_000_000
        , ecBackoff = [100_000, 250_000]
        , ecBreakerThreshold = 5
        , ecBreakerCooldown = 30
        , ecRetryAfter = Nothing
        }

-- | A fresh, healthy breaker (no failures recorded) in a new 'TVar'.
newBreaker :: IO (TVar Breaker)
newBreaker = newTVarIO initialBreaker
