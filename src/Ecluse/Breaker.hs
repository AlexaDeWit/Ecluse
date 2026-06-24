{- | The small circuit-breaker state machine that guards an unreliable operation.

A breaker fronts a call that can fail or hang — minting an outbound credential, or
consulting an effectful rule source. While the call is healthy it stays out of the
way; once failures pile up it __trips open__ and fast-fails further calls for a
cooldown, sparing both the caller's latency and the failing dependency. After the
cooldown it admits a single __half-open probe__: if the probe succeeds the breaker
resets, and if it fails the breaker re-opens for another cooldown.

The machine is pure and clock-injected: every transition takes the caller's @now@,
so it is deterministic under test with no real time passing. The two policy knobs —
the trip /threshold/ and the /cooldown/ — are not held here; each caller passes its
own to 'recordFailure', so one breaker shape serves consumers that tune them
differently. Concurrency and storage (an STM 'TVar', a record field) are the
caller's concern too: these functions only fold one state into the next.
-}
module Ecluse.Breaker (
    Breaker (..),
    initialBreaker,
    admit,
    recordSuccess,
    recordFailure,
) where

import Data.Time (NominalDiffTime, UTCTime, addUTCTime)

{- | The breaker's state, gating whether the guarded operation may be attempted.

A 'Closed' breaker is healthy and counts consecutive failures towards the trip
threshold; an 'Open' breaker fast-fails until its instant passes; a 'HalfOpen'
breaker has admitted one recovery probe and is waiting on its outcome.
-}
data Breaker
    = -- | Healthy: the consecutive-failure count so far, up to the trip threshold.
      Closed Int
    | -- | Tripped until the given instant: attempts fast-fail until then.
      Open UTCTime
    | -- | Cooldown elapsed: one probe attempt is admitted to test recovery.
      HalfOpen
    deriving stock (Eq, Show)

-- | A fresh, healthy breaker with no failures recorded.
initialBreaker :: Breaker
initialBreaker = Closed 0

{- | Decide whether the guarded operation may be attempted at @now@, returning the
admission and the breaker state to keep.

A 'Closed' or 'HalfOpen' breaker always admits and is unchanged. An 'Open' breaker
denies while its instant is still in the future; once @now@ reaches it the breaker
moves to 'HalfOpen' and admits a single recovery probe. The caller commits the
returned state (e.g. writes it back to its 'TVar') so the half-open transition is
recorded.
-}
admit :: UTCTime -> Breaker -> (Bool, Breaker)
admit now = \case
    Open until'
        | now < until' -> (False, Open until')
        | otherwise -> (True, HalfOpen)
    healthy -> (True, healthy)

{- | Fold a successful attempt into the breaker: reset it to healthy, clearing any
accumulated failures or a half-open probe.
-}
recordSuccess :: Breaker -> Breaker
recordSuccess _ = initialBreaker

{- | Fold a failed attempt into the breaker, given the caller's trip @threshold@ and
@cooldown@ and the current instant.

A 'Closed' breaker counts the failure up, tripping 'Open' for the cooldown once the
count reaches the threshold. Any other state (a failed half-open probe, or a failure
folded in while already open) (re-)opens for a fresh cooldown.
-}
recordFailure :: Int -> NominalDiffTime -> UTCTime -> Breaker -> Breaker
recordFailure threshold cooldown now = \case
    Closed n
        | n + 1 >= threshold -> tripped
        | otherwise -> Closed (n + 1)
    _ -> tripped
  where
    tripped = Open (addUTCTime cooldown now)
