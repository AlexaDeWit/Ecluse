-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The resilience harness around the advisory package read: a per-attempt timeout, bounded retry
with backoff, and a per-rule circuit breaker, attached by 'Ecluse.Core.Rules.prepare'.

Returned values settle success. Exhausted faults count once per run. Cancellation propagates
and releases an owned recovery probe without counting a source failure.
-}
module Ecluse.Core.Rules.Effectful (
    -- * The resilience policy
    Resilience (..),
    EffectfulConfig (..),
    defaultEffectfulConfig,
    newBreaker,

    -- * Running a read through it
    runResilient,
    ReadFault (..),
) where

import Control.Retry (retrying)
import Data.Time (NominalDiffTime, UTCTime)
import UnliftIO (timeout, tryAny)
import UnliftIO.Exception (mask)

import Ecluse.Core.Breaker (
    Attempt,
    Breaker,
    BreakerReporter,
    admit,
    initialBreaker,
    recordFailure,
    recordSuccess,
    release,
    reportBreakerChange,
 )
import Ecluse.Core.InFlight (guardInFlight)
import Ecluse.Core.Rules.Types
import Ecluse.Core.Supervision (delayListPolicy)
import Ecluse.Core.Text (displayExceptionT)

-- | The resilience policy around one advisory rule's reads. Each rule holds its own breaker state.
data Resilience = Resilience
    { resConfig :: EffectfulConfig
    -- ^ The per-attempt timeout, retry budget\/backoff, and breaker threshold\/cooldown.
    , resBreaker :: TVar Breaker
    -- ^ This rule's circuit-breaker state, shared across requests.
    , resBreakerReporter :: BreakerReporter
    -- ^ Observes coarse transitions for @ecluse.rule.breaker.state@.
    , resClock :: IO UTCTime
    -- ^ Fresh clock readings for admission and failure commit, separate from 'ctxNow'.
    }

-- | Why the harness gave a read up. Each rule relying on it resolves this under its own alignment.
data ReadFault = ReadFault
    { rfTransience :: Transience
    -- ^ Whether a retry may succeed, with the configured @Retry-After@ hint.
    , rfReason :: Inability
    -- ^ The client-facing cause a decision carries.
    , rfDetail :: Inability
    -- ^ The fault detail an operator reads in the outage report, never in a client message.
    }
    deriving stock (Eq, Show)

-- | Run one read under its 'Resilience' policy: the read's value, or why the harness gave it up.
runResilient :: Resilience -> IO a -> IO (Either ReadFault a)
runResilient res act = mask $ \restore -> do
    now <- resClock res
    (ticket, old, new) <- atomically (admitProbe res now)
    case ticket of
        Nothing -> pure (Left (ReadFault (transientCause (resConfig res)) SourceBreakerOpen SourceBreakerOpen))
        Just admitted ->
            guardInFlight restore (const pass) (releaseProbe res admitted) $ do
                reportBreakerChange (resBreakerReporter res) old new
                result <- attemptWithRetry res act
                settledNow <- resClock res
                settleOutcome res admitted settledNow result

settleOutcome :: Resilience -> Attempt -> UTCTime -> Either (Transience, Inability) a -> IO (Either ReadFault a)
settleOutcome res ticket now = \case
    Right value -> do
        commitBreaker res (recordSuccess ticket)
        pure (Right value)
    Left (transience, detail) -> do
        commitBreaker res (tripOnFailure (resConfig res) now ticket)
        pure (Left (ReadFault transience EvaluationFailed detail))

releaseProbe :: Resilience -> Attempt -> IO ()
releaseProbe res ticket = atomically (modifyTVar' (resBreaker res) (release ticket))

-- Attempt the read under the per-attempt timeout until the retry budget is spent. Only a fault retries.
attemptWithRetry :: Resilience -> IO a -> IO (Either (Transience, Inability) a)
attemptWithRetry res act =
    retrying (delayListPolicy (ecBackoff (resConfig res))) shouldRetry (\_ -> attemptOnce res act)
  where
    shouldRetry _ = pure . isLeft

-- One attempt under the timeout. Only a throw or a timeout retries and feeds the breaker.
attemptOnce :: Resilience -> IO a -> IO (Either (Transience, Inability) a)
attemptOnce res act = do
    result <- tryAny (timeout (ecTimeout (resConfig res)) act)
    pure $ case result of
        Left e -> Left (transient, RuleThrew (displayExceptionT e))
        Right Nothing -> Left (transient, AttemptTimedOut)
        Right (Just value) -> Right value
  where
    transient = transientCause (resConfig res)

transientCause :: EffectfulConfig -> Transience
transientCause cfg = WillResolve (ecRetryAfter cfg)

admitProbe :: Resilience -> UTCTime -> STM (Maybe Attempt, Breaker, Breaker)
admitProbe res now = do
    old <- readTVar (resBreaker res)
    let (ticket, new) = admit now old
    writeTVar (resBreaker res) new
    pure (ticket, old, new)

-- Reads the breaker before and after in one transaction, so the report reflects exactly the
-- transition that committed.
commitBreaker :: Resilience -> (Breaker -> Breaker) -> IO ()
commitBreaker res step = do
    (old, new) <- atomically $ do
        st <- readTVar (resBreaker res)
        let st' = step st
        writeTVar (resBreaker res) st'
        pure (st, st')
    reportBreakerChange (resBreakerReporter res) old new

tripOnFailure :: EffectfulConfig -> UTCTime -> Attempt -> Breaker -> Breaker
tripOnFailure cfg = recordFailure (ecBreakerThreshold cfg) (ecBreakerCooldown cfg)

-- | Per-rule limits. Breaker timing uses the fresh 'resClock', separate from request snapshots.
data EffectfulConfig = EffectfulConfig
    { ecTimeout :: Int
    -- ^ Per-attempt timeout in microseconds. Expiry is a retryable fault.
    , ecBackoff :: [Int]
    -- ^ Microsecond delay per retry. An empty list disables retries.
    , ecBreakerThreshold :: Int
    -- ^ Consecutive exhausted reads that trip the breaker, one read per request.
    , ecBreakerCooldown :: NominalDiffTime
    -- ^ Cooldown in seconds before one owned recovery run.
    , ecRetryAfter :: Maybe RetryAfter
    -- ^ Optional client @Retry-After@ hint for a faulted evaluation.
    }

-- | Timeout 2s, retries after 100ms and 250ms, trip after five failed runs, cooldown 30s.
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
