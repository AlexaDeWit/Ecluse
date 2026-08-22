-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | One supervision combinator for every background loop. It reruns a step forever,
absorbs transient faults with a bounded exponential backoff, and fails permanent ones
up to the process supervisor.

The proxy's background loops (the mirror worker's poll-and-process, the enqueue-buffer
drain, the advisory sync tasks, Pilot's export cycle) all share one contract. This
module logs a transient fault, a dependency outage the next iteration might clear, and
retries it at a bounded rate. A permanent fault, a wiring error no retry can fix, fails
up so the process exits loudly. Cancellation, the shutdown race tearing the loop down,
passes through untouched. This module is that contract, written once. Each loop's file
therefore carries only its step and its policy, not a private copy of the
catch-log-backoff machinery.

The typed fault channels stay in the steps. A step that receives an @Either fault a@
from a handle makes its own domain decision, its own pacing included. What reaches this
combinator's catch is residue: an exception escaping some dependency's typed contract,
plus whichever faults a step's policy deliberately classifies 'Permanent'.
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

{- | What the supervisor does with a synchronous fault the step let escape. It never
classifies an asynchronous exception: cancellation propagates untouched, so the
shutdown race can always tear a supervised loop down.
-}
data FaultDisposition
    = -- | Log at 'ErrorS', back off (bounded exponential), rerun the step.
      Transient
    | -- | Rethrow: fail up to the process supervisor, taking the process down.
      Permanent
    deriving stock (Eq, Show)

{- | A bounded exponential backoff, doubling from the base towards the cap as
consecutive failures mount. A persistently-failing dependency therefore retries at most
once per cap interval. A base equal to the cap is a fixed-interval retry.
-}
data BackoffSchedule = BackoffSchedule
    { bsBaseMicros :: Int
    -- ^ The delay after the first failure, in microseconds.
    , bsCapMicros :: Int
    -- ^ The ceiling the doubling saturates at, in microseconds.
    }
    deriving stock (Eq, Show)

{- | The delay before the next retry, given how many failures ran consecutively:
@base * 2^failures@, saturated at the cap. It clamps the exponent, so the doubling
cannot overflow before the ceiling applies.
-}
backoffMicros :: BackoffSchedule -> Int -> Int
backoffMicros schedule consecutiveFailures =
    min (bsCapMicros schedule) (bsBaseMicros schedule * (2 ^ min consecutiveFailures backoffShiftClamp))

-- The exponent clamp that keeps the doubling from overflowing before the
-- ceiling applies.
backoffShiftClamp :: Int
backoffShiftClamp = 12

{- | One loop's supervision policy: the label its log lines carry, how it classifies a
synchronous fault, and the backoff its transient faults pace at. A loop with wiring
faults that no retry can fix (an unconfigured handle reached at runtime) classifies
those 'Permanent'. Everything else defaults to 'Transient'.
-}
data SupervisionPolicy = SupervisionPolicy
    { spLabel :: Text
    -- ^ Names the loop in its supervision log lines.
    , spClassify :: SomeException -> FaultDisposition
    -- ^ Classify a synchronous fault the step let escape.
    , spBackoff :: BackoffSchedule
    -- ^ The pace for retrying transient faults. A completed step resets it.
    }

{- | Run the step forever under the policy. A completed step resets the backoff and
reruns at once, since the step owns its own pacing: poll waits and cycle delays live
inside it. A synchronous fault classifies through the policy, where 'Transient' logs
and backs off and 'Permanent' rethrows. 'tryAny' never catches an asynchronous
exception, so cancellation tears the loop down like any other thread. The 'Void'
return makes "this loop never returns" a fact of the type.
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
