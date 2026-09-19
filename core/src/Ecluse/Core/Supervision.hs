-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The one supervision combinator every background loop runs under, so no loop carries a
private copy of the catch-log-backoff machinery.

Typed fault channels stay in the steps: a step receiving an @Either fault a@ from a handle
makes its own domain decision and sets its own pacing. What reaches this combinator's catch
is residue, an exception escaping some dependency's typed contract, plus whatever a step's
policy classifies 'Permanent'.
-}
module Ecluse.Core.Supervision (
    -- * The combinator
    superviseLoop,
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
import Katip (KatipContext, Severity (ErrorS, WarningS), logFM, ls)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO, tryAny)

import Ecluse.Core.Text (displayExceptionT)

{- | What the supervisor does with a synchronous fault the step let escape. An asynchronous
exception is never classified, so cancellation propagates and the shutdown race always wins.
-}
data FaultDisposition
    = -- | Log at 'WarningS', back off (bounded exponential), rerun the step.
      Transient
    | -- | Rethrow: fail up to the process supervisor, taking the process down.
      Permanent
    deriving stock (Eq, Show)

{- | A bounded exponential backoff, doubling from the base towards the cap as consecutive
failures mount, so a persistently-failing dependency retries at most once per cap interval.
-}
data BackoffSchedule = BackoffSchedule
    { bsBaseMicros :: Int
    -- ^ The delay after the first failure, in microseconds.
    , bsCapMicros :: Int
    -- ^ The ceiling the doubling saturates at, in microseconds.
    }
    deriving stock (Eq, Show)

{- | The delay before the next retry, given how many failures ran consecutively:
@base * 2^failures@, saturated at the cap.
-}
backoffMicros :: BackoffSchedule -> Int -> Int
backoffMicros schedule consecutiveFailures =
    min (bsCapMicros schedule) (bsBaseMicros schedule * (2 ^ min consecutiveFailures backoffShiftClamp))

-- The exponent clamp that keeps the doubling from overflowing before the
-- ceiling applies.
backoffShiftClamp :: Int
backoffShiftClamp = 12

{- | One loop's supervision policy. A loop classifies a fault that no retry can fix, such as
an unconfigured handle reached at runtime, as 'Permanent', and everything else as 'Transient'.
-}
data SupervisionPolicy = SupervisionPolicy
    { spLabel :: Text
    -- ^ Names the loop in its supervision log lines.
    , spClassify :: SomeException -> FaultDisposition
    -- ^ Classify a synchronous fault the step let escape.
    , spBackoff :: BackoffSchedule
    -- ^ The pace for retrying transient faults. A completed step resets it.
    }

{- | The policy for a loop with no wiring fault to fail up on: every synchronous escape is
residue, logged and retried at @schedule@'s pace.
-}
transientPolicy :: Text -> BackoffSchedule -> SupervisionPolicy
transientPolicy label schedule =
    SupervisionPolicy
        { spLabel = label
        , spClassify = const Transient
        , spBackoff = schedule
        }

{- | Run the step forever under the policy: a completed step resets the backoff and reruns at once,
since the step owns its own pacing. 'tryAny' leaves asynchronous exceptions alone, so cancellation
tears the loop down.
-}
superviseLoop :: (MonadUnliftIO m, KatipContext m) => SupervisionPolicy -> m () -> m Void
superviseLoop policy step = go 0
  where
    go consecutiveFaults =
        tryAny step >>= \case
            Right () -> go 0
            Left fault -> case spClassify policy fault of
                Permanent -> do
                    logFM ErrorS (ls (spLabel policy <> ": permanent fault, failing up: " <> displayExceptionT fault))
                    throwIO fault
                Transient -> warnAndBackOff policy consecutiveFaults fault >> go (consecutiveFaults + 1)

-- A retry the loop makes for itself, so it warns. The 'Permanent' arm of 'superviseLoop' is this
-- combinator's only error, and it fails the process up.
warnAndBackOff :: (KatipContext m) => SupervisionPolicy -> Int -> SomeException -> m ()
warnAndBackOff policy consecutiveFaults fault = do
    let delay = backoffMicros (spBackoff policy) consecutiveFaults
    logFM WarningS (ls (spLabel policy <> ": iteration faulted (retrying in " <> show delay <> "µs): " <> displayExceptionT fault))
    threadDelay delay

{- | The pace a background loop retries a transient fault at: one second after the first
failure, doubling to a thirty-second ceiling.
-}
backgroundLoopBackoff :: BackoffSchedule
backgroundLoopBackoff = BackoffSchedule{bsBaseMicros = 1_000_000, bsCapMicros = 30_000_000}

{- | A delay list as a "Control.Retry" policy: retry @n@ waits the @n@-th delay in microseconds,
so the list's length is the retry budget. It paces a bounded run, not an endless loop.
-}
delayListPolicy :: (Monad m) => [Int] -> RetryPolicyM m
delayListPolicy delays = retryPolicy (\rs -> delays !!? rsIterNumber rs)
