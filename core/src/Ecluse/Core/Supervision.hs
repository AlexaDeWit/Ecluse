-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | One supervision combinator for every background loop: rerun a step forever,
absorbing transient faults with a bounded exponential backoff and failing
permanent ones up to the process supervisor.

The proxy's background loops (the mirror worker's poll-and-process, the
enqueue-buffer drain, the advisory sync tasks, Pilot's export cycle) all share
one robustness contract: a __transient__ fault (a dependency outage the next
iteration might clear) is logged and retried at a bounded rate, a __permanent__
fault (a wiring error no retry can fix) fails up so the process exits loudly,
and __cancellation__ (the shutdown race tearing the loop down) passes through
untouched. This module is that contract, written once, so each loop's file
carries only its step and its policy rather than a private copy of the
catch-log-backoff machinery.

The typed fault channels stay in the steps: a step that receives an
@Either fault a@ from a handle makes its own domain decision (its own pacing
included), and what reaches this combinator's catch is __residue__ -- an
exception escaping some dependency's typed contract -- plus whichever faults a
step's policy deliberately classifies 'Permanent'.
-}
module Ecluse.Core.Supervision (
    -- * The combinator
    superviseLoop,
    SupervisionPolicy (..),
    FaultDisposition (..),

    -- * Bounded exponential backoff
    BackoffSchedule (..),
    backoffMicros,
) where

import Katip (KatipContext, Severity (ErrorS), logFM, ls)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO, tryAny)

import Ecluse.Core.Text (displayExceptionT)

{- | What the supervisor does with a synchronous fault the step let escape.
Asynchronous exceptions are never classified: cancellation propagates untouched,
so the shutdown race can always tear a supervised loop down.
-}
data FaultDisposition
    = -- | Log at 'ErrorS', back off (bounded exponential), rerun the step.
      Transient
    | -- | Rethrow: fail up to the process supervisor, taking the process down.
      Permanent
    deriving stock (Eq, Show)

{- | A bounded exponential backoff: doubling from the base towards the cap as
consecutive failures mount, so a persistently-failing dependency is retried at
most once per cap interval. A base equal to the cap is a fixed-interval retry.
-}
data BackoffSchedule = BackoffSchedule
    { bsBaseMicros :: Int
    -- ^ The delay after the first failure, in microseconds.
    , bsCapMicros :: Int
    -- ^ The ceiling the doubling saturates at, in microseconds.
    }
    deriving stock (Eq, Show)

{- | The delay before the next retry, given how many failures have run
consecutively: @base * 2^failures@, saturated at the cap. The exponent is
clamped so the doubling cannot overflow before the ceiling applies.
-}
backoffMicros :: BackoffSchedule -> Int -> Int
backoffMicros schedule consecutiveFailures =
    min (bsCapMicros schedule) (bsBaseMicros schedule * (2 ^ min consecutiveFailures backoffShiftClamp))

-- The exponent clamp that keeps the doubling from overflowing before the
-- ceiling applies.
backoffShiftClamp :: Int
backoffShiftClamp = 12

{- | One loop's supervision policy: the label its log lines carry, how a
synchronous fault is classified, and the backoff its transient faults pace at.
Loops with wiring faults that no retry can fix (an unconfigured handle reached
at runtime) classify those 'Permanent'; everything else defaults 'Transient'.
-}
data SupervisionPolicy = SupervisionPolicy
    { spLabel :: Text
    -- ^ Names the loop in its supervision log lines.
    , spClassify :: SomeException -> FaultDisposition
    -- ^ Classify a synchronous fault the step let escape.
    , spBackoff :: BackoffSchedule
    -- ^ The pace transient faults are retried at (reset by a completed step).
    }

{- | Run the step forever under the policy: a completed step resets the backoff
and reruns at once (the step owns its own pacing -- poll waits and cycle delays
live inside it); a synchronous fault classifies through the policy ('Transient'
logs and backs off, 'Permanent' rethrows); an asynchronous exception is never
caught ('tryAny'), so cancellation tears the loop down like any other thread.
The 'Void' return makes "this loop never returns" a fact of the type.
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
                Transient -> do
                    let delay = backoffMicros (spBackoff policy) consecutiveFaults
                    logFM ErrorS (ls (spLabel policy <> ": iteration faulted (retrying in " <> show delay <> "µs): " <> displayExceptionT fault))
                    threadDelay delay
                    go (consecutiveFaults + 1)
