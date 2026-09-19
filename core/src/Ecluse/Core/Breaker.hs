-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The circuit-breaker state machine fronting a call that can fail or hang: minting an
outbound credential, or consulting an effectful rule source.

Every transition takes the caller's @now@, so no wall clock is read here. The two policy
knobs, the trip threshold and the cooldown, are the caller's and reach 'recordFailure' per
call, and so are storage and concurrency. These functions only fold one state into the next.
-}
module Ecluse.Core.Breaker (
    Breaker (..),
    initialBreaker,
    admit,
    recordSuccess,
    recordFailure,

    -- * Observing transitions
    BreakerReporter (..),
    noBreakerReporter,
    reportBreakerChange,
    breakerState,
) where

import Data.Time (NominalDiffTime, UTCTime, addUTCTime)

import Ecluse.Core.Telemetry.Metrics qualified as Metric

-- | The breaker's state, gating whether the guarded operation may be attempted.
data Breaker
    = -- | Healthy: the consecutive-failure count so far, up to the trip threshold.
      Closed Int
    | -- | Tripped until the given instant: attempts fast-fail until then.
      Open UTCTime
    | -- | Cooldown elapsed: 'admit' lets one probe attempt through to test recovery.
      HalfOpen
    deriving stock (Eq, Show)

-- | A fresh, healthy breaker with no failures recorded.
initialBreaker :: Breaker
initialBreaker = Closed 0

{- | Decide whether the guarded operation may be attempted at @now@. The caller must commit the
returned breaker state, or the move from 'Open' to 'HalfOpen' never takes effect.
-}
admit :: UTCTime -> Breaker -> (Bool, Breaker)
admit now = \case
    Open until' | now < until' -> (False, Open until')
    Open _ -> (True, HalfOpen)
    healthy -> (True, healthy)

-- | Fold a successful attempt into the breaker: reset it to healthy from any state.
recordSuccess :: Breaker -> Breaker
recordSuccess Closed{} = initialBreaker
recordSuccess Open{} = initialBreaker
recordSuccess HalfOpen = initialBreaker

{- | Fold a failed attempt into the breaker, given the caller's trip @threshold@ and @cooldown@.
A 'Closed' breaker counts up and trips at the threshold, and any other state opens a fresh cooldown.
-}
recordFailure :: Int -> NominalDiffTime -> UTCTime -> Breaker -> Breaker
recordFailure threshold cooldown now = \case
    Closed n | n + 1 >= threshold -> tripped
    Closed n -> Closed (n + 1)
    Open{} -> tripped
    HalfOpen -> tripped
  where
    tripped = Open (addUTCTime cooldown now)

{- | An observer of breaker state changes. The callback takes the breaker itself, so no
instrument handle reaches here, and the composition root installs the live observer.
-}
newtype BreakerReporter = BreakerReporter (Breaker -> IO ())

-- | The inert reporter: discards the state, recording nothing.
noBreakerReporter :: BreakerReporter
noBreakerReporter = BreakerReporter (const pass)

{- | Report a transition through the observer, but only when @old@ and @new@ differ observably.
The failure tally inside 'Closed' is not observable, so advancing it alone fires nothing.
-}
reportBreakerChange :: BreakerReporter -> Breaker -> Breaker -> IO ()
reportBreakerChange (BreakerReporter report) old new
    | breakerState old == breakerState new = pass
    | otherwise = report new

{- | The breaker's coarse observable state, the bounded value the @ecluse.rule.breaker.state@
gauge records. It drops the failure tally, so two 'Closed' breakers project alike.
-}
breakerState :: Breaker -> Metric.BreakerState
breakerState = \case
    Closed{} -> Metric.Closed
    HalfOpen -> Metric.HalfOpen
    Open{} -> Metric.Open
