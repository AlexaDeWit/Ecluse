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
    , rsLiveBytes :: Word64
    -- ^ Live data after the latest collection, exact only when that collection was major.
    , rsMemInUseBytes :: Word64
    , rsCapabilities :: Int
    , rsMaxHeapBytes :: Maybe Int
    -- ^ The @-M@ ceiling in force, 'Nothing' when unbounded.
    , rsAllocAreaBytes :: Int
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
        }
  where
    delta :: (Ord a, Num a) => (RtsSnapshot -> a) -> a
    delta field = if field after >= field before then field after - field before else 0

-- | The collector's share of the process's CPU time over the window, the GC-thrash signal.
gcCpuShare :: RtsWindow -> Maybe Double
gcCpuShare w
    | rwCpuNs w <= 0 = Nothing
    | otherwise = Just (fromIntegral (rwGcCpuNs w) / fromIntegral (rwCpuNs w))

-- | A window total divided by the successful requests, 'Nothing' when none succeeded.
perSuccess :: Double -> Int -> Maybe Double
perSuccess total successes
    | successes <= 0 = Nothing
    | otherwise = Just (total / fromIntegral successes)
