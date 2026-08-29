-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The resilience harness for effectful rules: the per-attempt timeout, bounded
retry with backoff, and per-source circuit breaker wrapped around a rule evaluation
that does IO. "Ecluse.Core.Rules" attaches a 'Resilience' to each effectful rule at
'Ecluse.Core.Rules.prepare' and runs it through 'runResilient'. The pure built-ins
never enter this module.

A resilient evaluation runs under its breaker's admission gate, a per-attempt timeout,
and bounded retry with backoff. Any 'RuleVerdict' the rule returns, a deterministic
'CannotVet' included, resets the breaker and comes back 'Decided'. The harness takes
that verdict at face value and never retries it. Only a __fault__ the harness observes
advances the breaker: a timeout, an exception, or the breaker already open. Such a
fault resolves to @'Unavailable' transience alignment reason@, with the alignment from
the rule's 'Resilience', fail-closed 'FailDeny' or fail-open 'FailNoDecision'. Total:
'runResilient' never throws, and a rule failure becomes a result.

The breaker timing reads the injected resilience clock ('resClock') fresh at each
breaker decision. That makes it deterministic under test and independent of the request
snapshot the age rules hold constant. Reading it again after the retry run means a
tripped breaker's cooldown starts when the failure commits, not when the run began.
-}
module Ecluse.Core.Rules.Effectful (
    -- * The resilience policy
    Resilience (..),
    EffectfulConfig (..),
    defaultEffectfulConfig,
    newBreaker,

    -- * Effectful-fault observation
    FaultReporter (..),

    -- * Running an evaluation through it
    runResilient,
) where

import Control.Retry (retrying)
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
import Ecluse.Core.Supervision (delayListPolicy)
import Ecluse.Core.Text (displayExceptionT)

{- | The resilience policy wrapped around one effectful rule's IO. The prepared rule carries
it, so each rule holds its own breaker state and failure alignment.
-}
data Resilience = Resilience
    { resConfig :: EffectfulConfig
    -- ^ The per-attempt timeout, retry budget\/backoff, and breaker threshold\/cooldown.
    , resAlignment :: FailureAlignment
    -- ^ Whether an exhausted evaluation fails closed ('FailDeny') or open ('FailNoDecision').
    , resBreaker :: TVar Breaker
    -- ^ This rule's per-source circuit-breaker state, shared across evaluations.
    , resBreakerReporter :: BreakerReporter
    {- ^ The observer this rule's breaker reports state transitions to
    (@ecluse.rule.breaker.state@). Inert ('Ecluse.Core.Breaker.noBreakerReporter') when unobserved.
    -}
    , resClock :: IO UTCTime
    {- ^ The wall clock the breaker reads for admission and cooldown, separate from the request
    snapshot 'ctxNow'. A fresh read at failure commit starts the cooldown at the failure.
    -}
    , resFaultReporter :: FaultReporter
    {- ^ The observer an exhausted evaluation reports its fault detail to. Inert
    ('noFaultReporter') when unobserved. The detail never reaches the client-facing message.
    -}
    }

{- | The observer that receives an exhausted evaluation's rule name and rendered fault.
It fires once per exhausted evaluation, never on a verdict or a still-cooling breaker.
-}
newtype FaultReporter = FaultReporter (Text -> Text -> IO ())

-- Report one exhausted evaluation's fault: the rule name and the rendered detail.
reportFault :: FaultReporter -> Text -> Text -> IO ()
reportFault (FaultReporter report) = report

{- | Run one effectful rule evaluation under its 'Resilience' policy. The evaluator is the
rule's per-version IO with the evaluation context applied, and the name tags the audit reason.
-}
runResilient :: Resilience -> Text -> (PackageDetails -> IO RuleVerdict) -> PackageDetails -> IO RuleEvaluation
runResilient res name evalAt pd = do
    admitted <- admitProbe res =<< resClock res
    if not admitted
        then -- Breaker open and still cooling down: fast-fail without running the
        -- rule's IO, the cheap path a sustained outage stays on. An open breaker is an
        -- infrastructural outage, so it is transient.
            pure (exhausted res name (transientCause (resConfig res)) "the rule source circuit breaker is open")
        else do
            result <- attemptWithRetry res evalAt pd
            -- Read the clock again after the retry run. An exhausted result then starts its
            -- cooldown at the failure commit, not at the start of the run.
            settledNow <- resClock res
            settleOutcome res name settledNow result

{- Settle a finished retry run against the breaker. A verdict resets it, an exhausted run
trips it. -}
settleOutcome :: Resilience -> Text -> UTCTime -> Either (Transience, Text) RuleVerdict -> IO RuleEvaluation
settleOutcome res name now = \case
    Right verdict -> do
        commitBreaker res recordSuccess
        pure (Decided verdict)
    Left (transience, detail) -> do
        commitBreaker res (tripOnFailure (resConfig res) now)
        -- Surface the fault detail before it collapses to the generic client-facing reason,
        -- which would otherwise hide the cause of a live-database query fault.
        reportFault (resFaultReporter res) name detail
        pure (exhausted res name transience "the rule could not be evaluated")

{- Attempt the rule's IO under the per-attempt timeout until the retry budget is spent.
Only a 'Left' fault retries, so a deterministic verdict never enters the retry loop. -}
attemptWithRetry :: Resilience -> (PackageDetails -> IO RuleVerdict) -> PackageDetails -> IO (Either (Transience, Text) RuleVerdict)
attemptWithRetry res evalAt pd =
    retrying (delayListPolicy (ecBackoff (resConfig res))) shouldRetry (\_ -> attemptOnce res evalAt pd)
  where
    shouldRetry _ = pure . isLeft

{- One attempt under the timeout. A 'RuleVerdict', a deterministic 'CannotVet' included, is
taken at face value, so only a throw or a timeout retries and feeds the breaker. -}
attemptOnce :: Resilience -> (PackageDetails -> IO RuleVerdict) -> PackageDetails -> IO (Either (Transience, Text) RuleVerdict)
attemptOnce res evalAt pd = do
    result <- tryAny (timeout (ecTimeout (resConfig res)) (evalAt pd))
    pure $ case result of
        Left e -> Left (transient, "the rule threw: " <> displayExceptionT e) -- the rule's IO threw
        Right Nothing -> Left (transient, "the attempt timed out") -- the attempt timed out
        Right (Just verdict) -> Right verdict -- a verdict is decided, never retried
  where
    transient = transientCause (resConfig res)

{- The result a faulted evaluation resolves to. The reason rides along for the audit trail. -}
exhausted :: Resilience -> Text -> Transience -> Text -> RuleEvaluation
exhausted res name transience reason = Unavailable transience (resAlignment res) (name <> ": " <> reason)

{- The transient 'Transience' an infrastructural failure (a timeout, an exception, an
open breaker) surfaces: retryable, carrying the rule's configured 'RetryAfter'. -}
transientCause :: EffectfulConfig -> Transience
transientCause cfg = WillResolve (ecRetryAfter cfg)

{- The breaker admission gate. 'Ecluse.Core.Breaker.admit' owns the admission policy, and
this commits its decision so the move out of 'Open' takes effect. -}
admitProbe :: Resilience -> UTCTime -> IO Bool
admitProbe res now = do
    (permitted, old, new) <- atomically $ do
        st <- readTVar (resBreaker res)
        let (p, st') = admit now st
        writeTVar (resBreaker res) st'
        pure (p, st, st')
    reportBreakerChange (resBreakerReporter res) old new
    pure permitted

{- Commit a breaker fold and report the transition it made. It reads the breaker before and
after in one transaction, so the report reflects exactly the committed transition. -}
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

{- | The resilience knobs around an effectful rule's IO: the per-attempt timeout, the retries
and the backoff before each, and the breaker threshold and cooldown. The breaker's timing reads
'resClock' fresh at failure commit, not the request snapshot 'ctxNow'.
-}
data EffectfulConfig = EffectfulConfig
    { ecTimeout :: Int
    {- ^ The per-attempt timeout in microseconds. The harness treats an attempt that
    does not return within it as a failure, a transient and retryable cause.
    -}
    , ecBackoff :: [Int]
    {- ^ The delay in microseconds before each retry, one entry per retry. Its length is
    the retry budget, so @[]@ admits no retry at all.
    -}
    , ecBreakerThreshold :: Int
    -- ^ Consecutive exhausted-rule failures that trip the breaker.
    , ecBreakerCooldown :: NominalDiffTime
    {- ^ How long the breaker stays open (fast-failing the rule) before it allows a
    single half-open probe to test recovery.
    -}
    , ecRetryAfter :: Maybe RetryAfter
    {- ^ The @Retry-After@ hint a faulted evaluation carries back to the client.
    'Nothing' sends no hint.
    -}
    }

{- | The default resilience knobs are a 2-second per-attempt timeout and two retries, at
100ms then 250ms. The breaker trips after 5 consecutive failures and cools for 30
seconds. The caller supplies the rule's IO. The knobs are policy, with these defaults.
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
