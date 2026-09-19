-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The monotonic clock every deadline and imposed wait in the core is measured on. A wall-clock
adjustment must never read as a longer lease or a shorter pause.
-}
module Ecluse.Core.Clock (
    MonoTime (..),
    monotonicNow,
    monoAfter,
    monoSecondsBetween,
    waitUntilMonotonic,
    waitSeconds,
    secondsToMicros,
) where

import Data.Time (NominalDiffTime)
import GHC.Clock (getMonotonicTime)
import UnliftIO.Concurrent (threadDelay)

-- | A reading of the monotonic clock, in seconds from an arbitrary origin.
newtype MonoTime = MonoTime Double
    deriving stock (Eq, Ord, Show)

-- | Read the monotonic clock.
monotonicNow :: IO MonoTime
monotonicNow = MonoTime <$> getMonotonicTime

-- | The instant this many seconds after the given one. A negative offset reads backwards.
monoAfter :: MonoTime -> Double -> MonoTime
monoAfter (MonoTime at) offset = MonoTime (at + offset)

-- | The seconds from the first instant to the second, negative once the second has passed.
monoSecondsBetween :: MonoTime -> MonoTime -> Double
monoSecondsBetween (MonoTime from') (MonoTime to') = to' - from'

-- | Wait until the given instant. The clock is re-read, so a wait can only ever come up short.
waitUntilMonotonic :: MonoTime -> IO ()
waitUntilMonotonic target = do
    now <- monotonicNow
    let pause = monoSecondsBetween now target
    when (pause > 0) (threadDelay (round (pause * 1_000_000)))

{- | Wait a duration, keeping its sub-second part. The microseconds saturate rather than wrap, so
an absurd duration waits a very long time instead of returning at once.
-}
waitSeconds :: NominalDiffTime -> IO ()
waitSeconds seconds = when (micros > 0) (threadDelay (fromInteger (min ceilingMicros micros)))
  where
    micros = round (toRational seconds * 1_000_000)
    ceilingMicros = toInteger (maxBound :: Int)

{- | A delay in seconds as the microseconds a delay primitive takes. Every config decoder that
spells a pause bounds it below @maxBound `div` 1_000_000@, so the conversion cannot wrap.
-}
secondsToMicros :: NominalDiffTime -> Int
secondsToMicros seconds = round seconds * 1_000_000
