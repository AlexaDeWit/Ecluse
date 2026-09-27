-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE DeriveAnyClass #-}

{- | RTS counters read from the process that did a scenario's work, and what changed across its
measured window. Allocation and collector cost divide by successful requests, never by attempts,
because a flood of sheds is cheap per attempt and says nothing about served work.
-}
module Ecluse.BenchLoad.RtsWindow (
    Collection (..),
    collectionName,
    RtsSnapshot (..),
    RtsWindow (..),
    rtsWindow,
    gcCpuShare,
    meanLiveAtMajors,
    compactionThresholdCrossed,
    perSuccess,
) where

import Data.Aeson (FromJSON, ToJSON)

{- | The collection a snapshot runs first. The RTS updates its cumulative counters only at a
collection, so every snapshot takes one: a minor one to close a window, a major one for live data.
-}
data Collection = MinorCollection | MajorCollection
    deriving stock (Eq, Show, Enum, Bounded)

-- | The collection's name on the control endpoint's query string.
collectionName :: Collection -> Text
collectionName = \case
    MinorCollection -> "minor"
    MajorCollection -> "major"

-- | One reading of the RTS statistics and the live runtime posture. Sizes are bytes.
data RtsSnapshot = RtsSnapshot
    { rsAllocatedBytes :: Word64
    , rsGcs :: Word32
    , rsMajorGcs :: Word32
    , rsGcCpuNs :: Int64
    , rsCpuNs :: Int64
    , rsGcElapsedNs :: Int64
    , rsMaxLiveBytes :: Word64
    -- ^ Live data at the fullest major collection so far.
    , rsMaxMemInUseBytes :: Word64
    -- ^ Every megablock the RTS held at its fullest, the figure @-M@ is compared with.
    , rsMaxLargeObjectsBytes :: Word64
    , rsCumulativeLiveBytes :: Word64
    -- ^ Live data summed over every major collection so far.
    , rsLiveBytes :: Word64
    -- ^ Live data after the latest collection, exact only when that collection was major.
    , rsMemInUseBytes :: Word64
    , rsCapabilities :: Int
    , rsMaxHeapBytes :: Maybe Int
    -- ^ The @-M@ ceiling in force, 'Nothing' when unbounded.
    , rsAllocAreaBytes :: Int
    , rsCompactAlways :: Bool
    -- ^ The RTS compacts the oldest generation at every major collection (@-c@ with no threshold).
    , rsCompactThresholdPercent :: Double
    -- ^ The share of @-M@ the oldest generation passes before the RTS compacts it (@-c@, 30 by default).
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | The change in the cumulative counters between two snapshots of one process.
data RtsWindow = RtsWindow
    { rwAllocatedBytes :: Word64
    , rwGcs :: Word32
    , rwMajorGcs :: Word32
    , rwGcCpuNs :: Int64
    , rwCpuNs :: Int64
    , rwGcElapsedNs :: Int64
    , rwCumulativeLiveBytes :: Word64
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | The window from the first snapshot to the second. A counter that went backwards reads zero.
rtsWindow :: RtsSnapshot -> RtsSnapshot -> RtsWindow
rtsWindow before after =
    RtsWindow
        { rwAllocatedBytes = delta rsAllocatedBytes
        , rwGcs = delta rsGcs
        , rwMajorGcs = delta rsMajorGcs
        , rwGcCpuNs = delta rsGcCpuNs
        , rwCpuNs = delta rsCpuNs
        , rwGcElapsedNs = delta rsGcElapsedNs
        , rwCumulativeLiveBytes = delta rsCumulativeLiveBytes
        }
  where
    delta :: (Ord a, Num a) => (RtsSnapshot -> a) -> a
    delta field = if field after >= field before then field after - field before else 0

-- | The collector's share of the process's CPU time over the window, the GC-thrash signal.
gcCpuShare :: RtsWindow -> Maybe Double
gcCpuShare w
    | rwCpuNs w <= 0 = Nothing
    | otherwise = Just (fromIntegral (rwGcCpuNs w) / fromIntegral (rwCpuNs w))

-- | The mean live data the window's major collections left, 'Nothing' when none ran.
meanLiveAtMajors :: RtsWindow -> Maybe Double
meanLiveAtMajors w
    | rwMajorGcs w == 0 = Nothing
    | otherwise = Just (fromIntegral (rwCumulativeLiveBytes w) / fromIntegral (rwMajorGcs w))

{- | Whether the fullest small-object live data passed the compaction threshold, the point where
every collection turns single-threaded. Inferred from the maxima. 'Nothing' without a heap ceiling.
-}
compactionThresholdCrossed :: RtsSnapshot -> Maybe Bool
compactionThresholdCrossed s
    | rsCompactAlways s = Just True
    | otherwise = do
        ceiling' <- rsMaxHeapBytes s
        let smallObjects = fromIntegral (rsMaxLiveBytes s) - fromIntegral (rsMaxLargeObjectsBytes s) :: Double
        pure (smallObjects > rsCompactThresholdPercent s / 100 * fromIntegral ceiling')

-- | A window total divided by the successful requests, 'Nothing' when none succeeded.
perSuccess :: Double -> Int -> Maybe Double
perSuccess total successes
    | successes <= 0 = Nothing
    | otherwise = Just (total / fromIntegral successes)
