-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The small circuit-breaker state machine that guards an unreliable operation.

A breaker fronts a call that can fail or hang: minting an outbound credential, or
consulting an effectful rule source. While the call is healthy the breaker stays out
of the way. Once failures pile up it trips open and fast-fails further calls for a
cooldown, which spares both the caller's latency and the failing dependency. After
the cooldown it admits a single half-open probe. A probe that succeeds resets the
breaker, and a probe that fails re-opens it for another cooldown.

The machine is pure and clock-injected: every transition takes the caller's @now@, so
a test runs deterministically with no real time passing. The two policy knobs, the
trip threshold and the cooldown, do not live here. Each caller passes its own to
'recordFailure', so one breaker shape serves consumers that tune them differently.
Concurrency and storage (an STM 'TVar', a record field) are the caller's concern too:
these functions only fold one state into the next.
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
) where

import Data.Time (NominalDiffTime, UTCTime, addUTCTime)

{- | The breaker's state, gating whether the guarded operation may be attempted.

A 'Closed' breaker is healthy and counts consecutive failures towards the trip
threshold. An 'Open' breaker fast-fails until its instant passes. A 'HalfOpen'
breaker holds one admitted recovery probe and waits on its outcome.
-}
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

{- | Decide whether the guarded operation may be attempted at @now@, returning the
admission and the breaker state to keep.

A 'Closed' or 'HalfOpen' breaker always admits and stays unchanged. An 'Open' breaker
denies while its instant is still in the future. Once @now@ reaches that instant the
breaker moves to 'HalfOpen' and admits a single recovery probe. The caller commits the
returned state (for example, writes it back to its 'TVar') so the half-open transition
takes effect.
-}
admit :: UTCTime -> Breaker -> (Bool, Breaker)
admit now = \case
    Open until' | now < until' -> (False, Open until')
    Open _ -> (True, HalfOpen)
    healthy -> (True, healthy)

{- | Fold a successful attempt into the breaker: reset it to healthy, clearing any
accumulated failures or a half-open probe.
-}
recordSuccess :: Breaker -> Breaker
recordSuccess Closed{} = initialBreaker
recordSuccess Open{} = initialBreaker
recordSuccess HalfOpen = initialBreaker

{- | Fold a failed attempt into the breaker, given the caller's trip @threshold@ and
@cooldown@ and the current instant.

A 'Closed' breaker counts the failure up, tripping 'Open' for the cooldown once the
count reaches the threshold. Any other state (a failed half-open probe, or a failure
folded in while already open) (re-)opens for a fresh cooldown.
-}
recordFailure :: Int -> NominalDiffTime -> UTCTime -> Breaker -> Breaker
recordFailure threshold cooldown now = \case
    Closed n | n + 1 >= threshold -> tripped
    Closed n -> Closed (n + 1)
    _ -> tripped
  where
    tripped = Open (addUTCTime cooldown now)

{- | An observer of breaker state changes. 'reportBreakerChange' calls it with the
new state after a transition commits, so a layer that cares (a state gauge) can
record it.

It is telemetry-agnostic by design. A bare @'Breaker' -> IO ()@ callback keeps the
breaker and its callers ("Ecluse.Core.Rules", the credential refresher) free of any
metric dependency. The composition root supplies the bridge to the instruments.
'noBreakerReporter' is the inert default: a breaker observed by it records nothing. A
breaker built before the telemetry substrate exists behaves the same way until the
composition root installs the live observer.
-}
newtype BreakerReporter = BreakerReporter (Breaker -> IO ())

-- | The inert reporter: discards the state, recording nothing.
noBreakerReporter :: BreakerReporter
noBreakerReporter = BreakerReporter (const pass)

{- | Report a transition through the observer, but only when @old@ and @new@ differ in
their observable state. 'Closed' carries a failure tally that is not itself observable.
A failure that only advances the count within 'Closed' is therefore no state change, and
fires nothing. A genuine change (a trip, a recovery probe, a reset) fires the reporter
with the new state.
-}
reportBreakerChange :: BreakerReporter -> Breaker -> Breaker -> IO ()
reportBreakerChange (BreakerReporter report) old new
    | observable old == observable new = pass
    | otherwise = report new
  where
    -- The coarse observable state: it drops the failure tally inside 'Closed', so two
    -- 'Closed' states compare equal however many failures each counted.
    observable :: Breaker -> Int
    observable = \case
        Closed{} -> 0
        HalfOpen -> 1
        Open{} -> 2
