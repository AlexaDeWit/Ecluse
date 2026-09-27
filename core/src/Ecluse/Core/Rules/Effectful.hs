-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The resilience harness around the advisory package read: a per-attempt timeout, bounded retry
with backoff, and a per-rule circuit breaker, attached by 'Ecluse.Core.Rules.prepare'.

Any value the read returns resets the breaker unretried, so only a harness-observed fault advances
the breaker and returns a 'ReadFault'. 'runResilient' never throws.
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

import Ecluse.Core.Breaker (
    Breaker,
    BreakerReporter,
    admit,
    initialBreaker,
    recordFailure,
    recordSuccess,
    reportBreakerChange,
 )
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
    {- ^ The observer this rule's breaker reports state transitions to
    (@ecluse.rule.breaker.state@). Inert ('Ecluse.Core.Breaker.noBreakerReporter') when unobserved.
    -}
    , resClock :: IO UTCTime
    {- ^ The wall clock the breaker reads for admission and cooldown, separate from the request
    snapshot 'ctxNow'. A fresh read at failure commit starts the cooldown at the failure.
    -}
    }

-- | Why the harness gave a read up. Each rule relying on it resolves this under its own alignment.
data ReadFault = ReadFault
    { rfTransience :: Transience
    -- ^ Whether a retry may succeed, with the configured @Retry-After@ hint.
    , rfReason :: Text
    -- ^ The client-facing cause a decision carries.
    , rfDetail :: Text
    -- ^ The fault detail an operator reads in the outage report, never in a client message.
    }
    deriving stock (Eq, Show)

-- | Run one read under its 'Resilience' policy: the read's value, or why the harness gave it up.
runResilient :: Resilience -> IO a -> IO (Either ReadFault a)
runResilient res act = do
    admitted <- admitProbe res =<< resClock res
    if not admitted
        then
            -- Breaker open and still cooling down: fast-fail without running the read, the cheap
            -- path a sustained outage stays on.
            pure (Left (ReadFault (transientCause (resConfig res)) breakerOpen breakerOpen))
        else do
            result <- attemptWithRetry res act
            -- Read the clock again after the retry run. An exhausted result then starts its
            -- cooldown at the failure commit, not at the start of the run.
            settledNow <- resClock res
            settleOutcome res settledNow result
  where
    breakerOpen = "the rule source circuit breaker is open"

-- Settle a finished retry run against the breaker. A value resets it, an exhausted run trips it.
settleOutcome :: Resilience -> UTCTime -> Either (Transience, Text) a -> IO (Either ReadFault a)
settleOutcome res now = \case
    Right value -> do
        commitBreaker res recordSuccess
        pure (Right value)
    Left (transience, detail) -> do
        commitBreaker res (tripOnFailure (resConfig res) now)
        pure (Left (ReadFault transience "the rule could not be evaluated" detail))

-- Attempt the read under the per-attempt timeout until the retry budget is spent. Only a fault retries.
attemptWithRetry :: Resilience -> IO a -> IO (Either (Transience, Text) a)
attemptWithRetry res act =
    retrying (delayListPolicy (ecBackoff (resConfig res))) shouldRetry (\_ -> attemptOnce res act)
  where
    shouldRetry _ = pure . isLeft

-- One attempt under the timeout. Only a throw or a timeout retries and feeds the breaker.
attemptOnce :: Resilience -> IO a -> IO (Either (Transience, Text) a)
attemptOnce res act = do
    result <- tryAny (timeout (ecTimeout (resConfig res)) act)
    pure $ case result of
        Left e -> Left (transient, "the rule threw: " <> displayExceptionT e)
        Right Nothing -> Left (transient, "the attempt timed out")
        Right (Just value) -> Right value
  where
    transient = transientCause (resConfig res)

transientCause :: EffectfulConfig -> Transience
transientCause cfg = WillResolve (ecRetryAfter cfg)

-- Commits what 'Ecluse.Core.Breaker.admit' decided, so the move out of 'Open' takes effect.
admitProbe :: Resilience -> UTCTime -> IO Bool
admitProbe res now = do
    (permitted, old, new) <- atomically $ do
        st <- readTVar (resBreaker res)
        let (p, st') = admit now st
        writeTVar (resBreaker res) st'
        pure (p, st, st')
    reportBreakerChange (resBreakerReporter res) old new
    pure permitted

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

tripOnFailure :: EffectfulConfig -> UTCTime -> Breaker -> Breaker
tripOnFailure cfg = recordFailure (ecBreakerThreshold cfg) (ecBreakerCooldown cfg)

{- | The resilience knobs around an advisory rule's package read. The breaker's timing reads
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
    -- ^ Consecutive exhausted reads that trip the breaker, one read per request.
    , ecBreakerCooldown :: NominalDiffTime
    {- ^ How long the breaker stays open (fast-failing the rule) before it allows a
    single half-open probe to test recovery.
    -}
    , ecRetryAfter :: Maybe RetryAfter
    {- ^ The @Retry-After@ hint a faulted evaluation carries back to the client.
    'Nothing' sends no hint.
    -}
    }

{- | A 2-second per-attempt timeout and two retries, at 100ms then 250ms. The breaker trips after
5 consecutive failures and cools for 30 seconds.
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
