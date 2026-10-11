-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A clock-injected circuit breaker for advisory reads and credential mints.
Recovery admits one owner for a complete run. Tickets fence older outcomes and cleanup.
Callers commit transitions atomically and release tickets on every exit.
-}
module Ecluse.Core.Breaker (
    -- * State and phase
    Breaker,
    BreakerPhase (..),
    initialBreaker,
    breakerPhase,

    -- * Owned attempts
    Attempt,
    admit,
    recordSuccess,
    recordFailure,
    release,

    -- * Observations
    BreakerReporter (..),
    noBreakerReporter,
    reportBreakerChange,
    breakerState,
) where

import Data.Time (NominalDiffTime, UTCTime, addUTCTime)

import Ecluse.Core.Telemetry.Metrics qualified as Metric

-- | Opaque state retaining its generation through settlement and cancellation.
data Breaker = Breaker Integer BreakerPhase
    deriving stock (Eq, Show)

-- | A ticket for an admitted run. Only its recovery generation can settle an owned probe.
data Attempt = Ordinary Integer | Recovery Integer
    deriving stock (Eq, Show)

-- | The phase view excludes the generation used to fence stale owners.
data BreakerPhase
    = -- | Healthy, with its consecutive failure count.
      Closed Int
    | -- | Refuses calls before this cooldown deadline.
      Open UTCTime
    | -- | One admitted run owns recovery.
      HalfOpen
    | -- | Cancelled recovery permits another probe immediately.
      RecoveryReady
    deriving stock (Eq, Show)

-- | A healthy breaker with no failures.
initialBreaker :: Breaker
initialBreaker = Breaker 0 (Closed 0)

-- | Observe the phase without exposing an ownership ticket.
breakerPhase :: Breaker -> BreakerPhase
breakerPhase (Breaker _ phase) = phase

-- | Commit the returned state atomically and release the admitted ticket on every exit.
admit :: UTCTime -> Breaker -> (Maybe Attempt, Breaker)
admit now br@(Breaker generation phase) = case phase of
    Open until' | now < until' -> (Nothing, br)
    HalfOpen -> (Nothing, br)
    Closed{} -> (Just (Ordinary generation), br)
    _ -> (Just (Recovery next), Breaker next HalfOpen)
  where
    next = generation + 1

-- | Matching success closes the breaker. An older outcome leaves it unchanged.
recordSuccess :: Attempt -> Breaker -> Breaker
recordSuccess ticket br@(Breaker generation _)
    | owns ticket br = Breaker generation (Closed 0)
    | otherwise = br

-- | Count an admitted failure once per exhausted run, with a fresh cooldown after a failed probe.
recordFailure :: Int -> NominalDiffTime -> UTCTime -> Attempt -> Breaker -> Breaker
recordFailure threshold cooldown now ticket br@(Breaker generation phase)
    | not (owns ticket br) = br
    | Closed n <- phase, n + 1 < threshold = Breaker generation (Closed (n + 1))
    | otherwise = Breaker generation (Open (addUTCTime cooldown now))

-- | Release unsettled recovery without adding a failure or cooldown. Stale releases do nothing.
release :: Attempt -> Breaker -> Breaker
release (Recovery ticket) (Breaker generation HalfOpen)
    | ticket == generation = Breaker generation RecoveryReady
release _ br = br

owns :: Attempt -> Breaker -> Bool
owns (Ordinary ticket) (Breaker generation phase) =
    ticket == generation && case phase of
        Closed{} -> True
        Open{} -> True
        _ -> False
owns (Recovery ticket) (Breaker generation phase) = ticket == generation && phase == HalfOpen

-- | Observe changes through the composition root's instrument adapter.
newtype BreakerReporter = BreakerReporter (Breaker -> IO ())

-- | Discard transitions.
noBreakerReporter :: BreakerReporter
noBreakerReporter = BreakerReporter (const pass)

-- | Report coarse phase changes. Failure counts and ownership generations do not emit events.
reportBreakerChange :: BreakerReporter -> Breaker -> Breaker -> IO ()
reportBreakerChange (BreakerReporter report) old new
    | breakerState old == breakerState new = pass
    | otherwise = report new

-- | Project the phase onto the existing three gauge values.
breakerState :: Breaker -> Metric.BreakerState
breakerState br = case breakerPhase br of
    Closed{} -> Metric.Closed
    Open{} -> Metric.Open
    HalfOpen -> Metric.HalfOpen
    RecoveryReady -> Metric.HalfOpen
