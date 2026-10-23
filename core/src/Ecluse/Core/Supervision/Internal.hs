-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Background supervision with typed fault classification and bounded retry pacing.
module Ecluse.Core.Supervision.Internal (
    -- * The combinator
    superviseLoop,
    superviseLoopWithDelay,
    superviseBackground,
    superviseUnder,
    SupervisionPolicy (..),
    transientPolicy,
    FaultDisposition (..),

    -- * Bounded exponential backoff
    BackoffSchedule (..),
    backoffMicros,
    backgroundLoopBackoff,

    -- * Bounded retry pacing
    delayListPolicy,
) where

import Control.Retry (RetryPolicyM, RetryStatus (rsIterNumber), retryPolicy)
import Katip (KatipContext, LogEnv, Namespace (Namespace), Severity (ErrorS, WarningS), SimpleLogPayload, logFM, ls, runKatipContextT)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO, tryAny)

import Ecluse.Core.Fault.Http (displayFaultT)

-- | Synchronous fault policy. Asynchronous cancellation always propagates.
data FaultDisposition
    = -- | Log at 'WarningS', back off (bounded exponential), rerun the step.
      Transient
    | -- | Rethrow: fail up to the process supervisor, taking the process down.
      Permanent
    deriving stock (Eq, Show)

-- | Consecutive failures double the base delay up to the cap.
data BackoffSchedule = BackoffSchedule
    { bsBaseMicros :: Int
    -- ^ The delay after the first failure, in microseconds.
    , bsCapMicros :: Int
    -- ^ The ceiling the doubling saturates at, in microseconds.
    }
    deriving stock (Eq, Show)

-- | Compute the retry delay in microseconds, bounded by the schedule cap.
backoffMicros :: BackoffSchedule -> Int -> Int
backoffMicros schedule consecutiveFailures =
    min (bsCapMicros schedule) (bsBaseMicros schedule * (2 ^ min consecutiveFailures backoffShiftClamp))

-- The exponent clamp that keeps the doubling from overflowing before the
-- ceiling applies.
backoffShiftClamp :: Int
backoffShiftClamp = 12

-- | Classify escaped synchronous faults and set the retry pace.
data SupervisionPolicy = SupervisionPolicy
    { spLabel :: Text
    -- ^ Names the loop in its supervision log lines.
    , spClassify :: SomeException -> FaultDisposition
    -- ^ Classify a synchronous fault the step let escape.
    , spBackoff :: BackoffSchedule
    -- ^ The pace for retrying transient faults. A completed step resets it.
    }

-- | Retry every escaped synchronous fault at the supplied pace.
transientPolicy :: Text -> BackoffSchedule -> SupervisionPolicy
transientPolicy label schedule =
    SupervisionPolicy
        { spLabel = label
        , spClassify = const Transient
        , spBackoff = schedule
        }

-- | Repeat the step, resetting backoff after success. Cancellation propagates.
superviseLoop :: (MonadUnliftIO m, KatipContext m) => SupervisionPolicy -> m () -> m Void
superviseLoop = superviseLoopWithDelay threadDelay

-- | Run the same supervision policy with caller-supplied retry pacing.
superviseLoopWithDelay :: (MonadUnliftIO m, KatipContext m) => (Int -> m ()) -> SupervisionPolicy -> m () -> m Void
superviseLoopWithDelay delayAction policy step = go 0
  where
    go consecutiveFaults =
        tryAny step >>= \case
            Right () -> go 0
            Left fault -> case spClassify policy fault of
                Permanent -> do
                    logFM ErrorS (ls (spLabel policy <> ": permanent fault, failing up: " <> displayFaultT fault))
                    throwIO fault
                Transient -> warnAndBackOff delayAction policy consecutiveFaults fault >> go (consecutiveFaults + 1)

{- | Run a background loop that never fails up: every escaped fault backs off and the step reruns.
Pacing lives in the step, and the loop logs under the label's namespace.
-}
superviseBackground :: Text -> LogEnv -> IO () -> IO ()
superviseBackground label = superviseUnder (transientPolicy label backgroundLoopBackoff)

-- | Run a background loop under a policy, logging under the policy's label.
superviseUnder :: SupervisionPolicy -> LogEnv -> IO () -> IO ()
superviseUnder policy logEnv task =
    void . runKatipContextT logEnv (mempty :: SimpleLogPayload) (Namespace [spLabel policy]) $
        superviseLoop policy (liftIO task)

-- A retry the loop makes for itself, so it warns. The 'Permanent' arm of 'superviseLoop' is this
-- combinator's only error, and it fails the process up.
warnAndBackOff :: (KatipContext m) => (Int -> m ()) -> SupervisionPolicy -> Int -> SomeException -> m ()
warnAndBackOff delayAction policy consecutiveFaults fault = do
    let delay = backoffMicros (spBackoff policy) consecutiveFaults
    logFM WarningS (ls (spLabel policy <> ": iteration faulted (retrying in " <> show delay <> "µs): " <> displayFaultT fault))
    delayAction delay

-- | Retry background faults from one second to a thirty-second cap.
backgroundLoopBackoff :: BackoffSchedule
backgroundLoopBackoff = BackoffSchedule{bsBaseMicros = 1_000_000, bsCapMicros = 30_000_000}

-- | Use explicit microsecond delays as a finite retry budget.
delayListPolicy :: (Monad m) => [Int] -> RetryPolicyM m
delayListPolicy delays = retryPolicy (\rs -> delays !!? rsIterNumber rs)
