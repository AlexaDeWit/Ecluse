-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The collector brake of feedback memory admission: detect garbage-collector thrash from
cumulative RTS counters, so admission stops starting memory-heavy work until it recovers.

Two signals feed it. The collector's share of process CPU over a sliding window is measured.
The share of the old generation each major collection frees is modelled from measured live
bytes and the collector's own sizing rule, because the RTS does not report per-collection
reclaim. The brake engages on either signal and releases only when both recover. Every
threshold here is a starting point that load measurement will set.
-}
module Ecluse.Core.Server.Admission.Memory.Brake (
    -- * Brake state
    BrakeState (..),

    -- * Thresholds
    BrakeThresholds (..),
    defaultBrakeThresholds,
    brakeWindowSamples,
    engageGcShare,
    releaseGcShare,
    engageReclaim,
    releaseReclaim,
    minWindowCpuNs,

    -- * Collector samples
    CollectorSample (..),
    HeapGeometry (..),
    reclaimRatio,

    -- * Stepping the brake
    CollectorState,
    initialCollectorState,
    collectorBrake,
    ticksSinceMajor,
    CollectorReading (..),
    stepCollector,
    settleBrake,
) where

import Data.Sequence qualified as Seq

-- | Whether the brake currently stops memory-heavy admissions.
data BrakeState = BrakeReleased | BrakeEngaged
    deriving stock (Eq, Show)

{- | The brake's marks and window. Each release mark lies on the recovered side of its engage
mark, so the brake has a band in which it holds its state.
-}
data BrakeThresholds = BrakeThresholds
    { btWindowSamples :: Int
    , btEngageGcShare :: Double
    , btReleaseGcShare :: Double
    -- ^ Below 'btEngageGcShare'.
    , btEngageReclaim :: Double
    , btReleaseReclaim :: Double
    -- ^ Above 'btEngageReclaim'.
    , btMinWindowCpuNs :: Int
    -- ^ The process CPU a window must hold before its GC share counts.
    }
    deriving stock (Eq, Show)

-- | The shipped starting thresholds, each from its named constant.
defaultBrakeThresholds :: BrakeThresholds
defaultBrakeThresholds =
    BrakeThresholds
        { btWindowSamples = brakeWindowSamples
        , btEngageGcShare = engageGcShare
        , btReleaseGcShare = releaseGcShare
        , btEngageReclaim = engageReclaim
        , btReleaseReclaim = releaseReclaim
        , btMinWindowCpuNs = minWindowCpuNs
        }

-- | Samples in the GC-share window: ten 100 ms samples, so one second smooths collection timing noise.
brakeWindowSamples :: Int
brakeWindowSamples = 10

-- | The GC share of process CPU that engages the brake. Go's memory limit caps the collector near this share.
engageGcShare :: Double
engageGcShare = 0.5

-- | The GC share at or below which an engaged brake may release.
releaseGcShare :: Double
releaseGcShare = 0.3

{- | The modelled share of the old generation a major frees, at or below which the brake engages.
It is 0.5 in the copying collector's normal regime (F = 2) and falls to 0 at heap overflow.
-}
engageReclaim :: Double
engageReclaim = 0.2

-- | The modelled reclaim share at or above which an engaged brake may release.
releaseReclaim :: Double
releaseReclaim = 0.35

-- | The process CPU a window must hold before its GC share counts, so an idle collection cannot engage the brake.
minWindowCpuNs :: Int
minWindowCpuNs = 50_000_000

-- | One read of the RTS's cumulative counters.
data CollectorSample = CollectorSample
    { csCpuNs :: Int
    -- ^ Process CPU since the RTS started, nanoseconds.
    , csGcCpuNs :: Int
    -- ^ Collector CPU since the RTS started, nanoseconds.
    , csMajorGcs :: Int
    , csCumulativeLiveBytes :: Int
    -- ^ Live bytes summed over every major collection.
    }
    deriving stock (Eq, Show)

-- | The heap shape the reclaim model needs. Present only when the RTS runs with a heap ceiling.
data HeapGeometry = HeapGeometry
    { hgMaxHeapBytes :: Int
    -- ^ The heap ceiling @-M@.
    , hgNurseryBytes :: Int
    -- ^ The allocation area the collector reserves beside the old generation.
    , hgOldGenFactor :: Double
    -- ^ The old-generation growth factor @-F@.
    }
    deriving stock (Eq, Show)

{- | The share of the old generation a major collection frees in steady state. The copying
collector collects the old generation when it reaches @min (F x live) ((M - nursery) / 2)@, so
each major frees that size less the live bytes. It reaches 0 where the heap overflows.
-}
reclaimRatio :: HeapGeometry -> Int -> Double
reclaimRatio geometry live
    | live <= 0 = 1
    | trigger <= 0 = 0
    | otherwise = clampUnit (1 - fromIntegral live / trigger)
  where
    liveD = fromIntegral live :: Double
    cap = fromIntegral (hgMaxHeapBytes geometry - hgNurseryBytes geometry) / 2
    trigger = min (hgOldGenFactor geometry * liveD) cap

-- | The brake's rolling state across samples.
data CollectorState = CollectorState
    { colWindow :: Seq CollectorSample
    -- ^ Oldest first, at most one more than the window.
    , colLiveAtMajor :: Maybe Int
    , colTicksSinceMajor :: Int
    , colBrake :: BrakeState
    }
    deriving stock (Eq, Show)

-- | No samples yet, the brake released.
initialCollectorState :: CollectorState
initialCollectorState = CollectorState Seq.empty Nothing 0 BrakeReleased

-- | The brake state after the latest step.
collectorBrake :: CollectorState -> BrakeState
collectorBrake = colBrake

-- | Samples since the last one that saw a major collection.
ticksSinceMajor :: CollectorState -> Int
ticksSinceMajor = colTicksSinceMajor

-- | What one step measured, for metrics and the gate.
data CollectorReading = CollectorReading
    { crGcShare :: Maybe Double
    -- ^ Collector CPU over process CPU across the window. 'Nothing' when the window held too little CPU.
    , crLiveAtMajor :: Maybe Int
    -- ^ Mean live bytes over the most recent sample's major collections, carried while none ran.
    , crReclaim :: Maybe Double
    -- ^ The modelled reclaim share. 'Nothing' without a heap ceiling or a measured major.
    , crBrake :: BrakeState
    }
    deriving stock (Eq, Show)

-- | Fold one sample into the window, then settle the brake on the refreshed signals.
stepCollector :: BrakeThresholds -> Maybe HeapGeometry -> CollectorSample -> CollectorState -> (CollectorReading, CollectorState)
stepCollector t geometry sample st = (reading, st')
  where
    window = Seq.drop (Seq.length appended - (btWindowSamples t + 1)) appended
    appended = colWindow st Seq.|> sample
    previous = Seq.lookup (Seq.length (colWindow st) - 1) (colWindow st)
    majorsNow = maybe 0 (\p -> csMajorGcs sample - csMajorGcs p) previous
    live
        | majorsNow > 0 = (\p -> (csCumulativeLiveBytes sample - csCumulativeLiveBytes p) `div` majorsNow) <$> previous
        | otherwise = colLiveAtMajor st
    share = windowGcShare t window
    reclaim = reclaimRatio <$> geometry <*> live
    brake = settleBrake t (colBrake st) share reclaim
    reading = CollectorReading share live reclaim brake
    st' =
        CollectorState
            { colWindow = window
            , colLiveAtMajor = live
            , colTicksSinceMajor = if majorsNow > 0 then 0 else colTicksSinceMajor st + 1
            , colBrake = brake
            }

-- The collector's CPU share between the oldest and newest samples in the window.
windowGcShare :: BrakeThresholds -> Seq CollectorSample -> Maybe Double
windowGcShare t window = do
    oldest <- Seq.lookup 0 window
    newest <- Seq.lookup (Seq.length window - 1) window
    let cpu = csCpuNs newest - csCpuNs oldest
        gcCpu = csGcCpuNs newest - csGcCpuNs oldest
    guard (cpu > 0 && cpu >= btMinWindowCpuNs t)
    pure (clampUnit (fromIntegral gcCpu / fromIntegral cpu))

{- | Move the brake with hysteresis: engage when either signal crosses its engage mark, release
only when every present signal is back past its release mark. An absent signal never engages
the brake and never holds it.
-}
settleBrake :: BrakeThresholds -> BrakeState -> Maybe Double -> Maybe Double -> BrakeState
settleBrake t current share reclaim = case current of
    BrakeReleased
        | engage -> BrakeEngaged
        | otherwise -> BrakeReleased
    BrakeEngaged
        | recovered -> BrakeReleased
        | otherwise -> BrakeEngaged
  where
    engage = any (>= btEngageGcShare t) share || any (<= btEngageReclaim t) reclaim
    recovered = all (<= btReleaseGcShare t) share && all (>= btReleaseReclaim t) reclaim

clampUnit :: Double -> Double
clampUnit = max 0 . min 1
