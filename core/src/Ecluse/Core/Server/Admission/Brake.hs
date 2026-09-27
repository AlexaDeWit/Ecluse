-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The memory budget's feedback: how one sample of the collector and the heap moves the budget.

After each major collection the brake measures the live data outside the charges, so the budget may
grow until charges and that measured remainder reach the live ceiling. The ceiling also leaves room
under the heap's overflow point for the largest request seen lately, which may run past the budget.
The brake halves the budget when the collector takes too much of the CPU or the heap nears overflow,
and grows it back in small steps while the collector stays calm. "Ecluse.Rts.Sampler" feeds it.
-}
module Ecluse.Core.Server.Admission.Brake (
    -- * The marks
    BrakeMarks (..),
    defaultBrakeMarks,

    -- * One step
    BrakeState (..),
    initialBrakeState,
    GcSample (..),
    brakeStep,

    -- * Reading the collector
    CollectorReading (..),
    gcSharePermille,
    SampleWindow,
    newSampleWindow,
    windowSample,
) where

import Data.Bits (shiftR)

import Ecluse.Core.Server.Admission.Types (BrakeBounds (..), BrakeLevel (..))

-- | The brake's thresholds. Shares are thousandths of the process's CPU time over the sampler's window.
data BrakeMarks = BrakeMarks
    { bmGcHighPermille :: Int
    -- ^ Above this GC share of CPU, the budget halves.
    , bmGcLowPermille :: Int
    -- ^ Below this GC share of CPU, the budget may grow.
    , bmKernelHighPermille :: Int
    -- ^ Above this share of the cgroup limit in use (less reclaimable file pages), the budget halves.
    , bmOverflowGuardPermille :: Int
    -- ^ Above this share of the copying collector's overflow point in live data, the budget halves.
    , bmCalmSamples :: Int
    -- ^ Consecutive calm samples before one growth step.
    , bmGrowPermille :: Int
    -- ^ One growth step, as a share of the current budget.
    , bmReturnPermille :: Int
    -- ^ How much of a fall in the measured outside live data one major collection gives back.
    , bmForgetShift :: Int
    -- ^ The largest request seen fades by one part in @2^shift@ per sample.
    , bmCooldownSamples :: Int
    -- ^ The fewest samples from one halving to the next.
    }
    deriving stock (Eq, Show)

{- | Above half the CPU the collector is thrashing, the point where Go's memory limit stops helping.
A loaded proxy with no memory pressure spends 20 to 34% of its CPU in the collector.
-}
defaultBrakeMarks :: BrakeMarks
defaultBrakeMarks =
    BrakeMarks
        { bmGcHighPermille = 500
        , bmGcLowPermille = 350
        , bmKernelHighPermille = 900
        , bmOverflowGuardPermille = 800
        , bmCalmSamples = 10
        , bmGrowPermille = 125
        , bmReturnPermille = 250
        , bmForgetShift = 8
        , bmCooldownSamples = 10
        }

-- | The brake's memory between samples.
data BrakeState = BrakeState
    { bsBudget :: Int
    , bsOutside :: Maybe Int
    -- ^ Live data outside the charges at recent major collections. 'Nothing' before the first.
    , bsLargestRequest :: Int
    -- ^ The largest charge one request reached lately, fading while no request matches it.
    , bsCalmSamples :: Int
    , bsCooldown :: Int
    -- ^ Samples left before pressure may halve the budget again.
    , bsLevel :: BrakeLevel
    }
    deriving stock (Eq, Show)

-- | Start at the boot budget, with nothing measured yet.
initialBrakeState :: BrakeBounds -> BrakeState
initialBrakeState bounds =
    BrakeState{bsBudget = min (budgetCeiling bounds Nothing 0) (bbBootBytes bounds), bsOutside = Nothing, bsLargestRequest = 0, bsCalmSamples = 0, bsCooldown = 0, bsLevel = Calm}

-- | One sample's readings. A missing reading leaves its rule out of this step.
data GcSample = GcSample
    { gsGcSharePermille :: Maybe Int
    -- ^ The collector's share of CPU over the window. 'Nothing' when the process was idle.
    , gsLiveAfterMajor :: Maybe Int
    -- ^ Live bytes, present only when a major collection finished since the last sample.
    , gsChargedBytes :: Int
    -- ^ What the meter held when the sample was taken.
    , gsLargestCharge :: Int
    -- ^ The largest total one request reached since the previous sample.
    , gsKernelPermille :: Maybe Int
    -- ^ The cgroup's non-reclaimable use against its limit, when a limit binds.
    }
    deriving stock (Eq, Show)

{- | Move the budget by one sample: pressure halves it at most once per cooldown, a lower ceiling cuts
it at once, and only a calm stretch grows it. It stays between the floor and the ceiling.
-}
brakeStep :: BrakeMarks -> BrakeBounds -> BrakeState -> GcSample -> BrakeState
brakeStep marks bounds before sample =
    BrakeState
        { bsBudget = grown
        , bsOutside = outside
        , bsLargestRequest = largest
        , bsCalmSamples = if level == Calm && not growNow then calm else 0
        , bsCooldown = if halveNow then bmCooldownSamples marks - 1 else max 0 (bsCooldown before - 1)
        , bsLevel = level
        }
  where
    outside = maybe (bsOutside before) (Just . measuredOutside marks before (gsChargedBytes sample)) (gsLiveAfterMajor sample)
    largest = max (gsLargestCharge sample) (bsLargestRequest before - bsLargestRequest before `shiftR` bmForgetShift marks)
    ceilingNow = budgetCeiling bounds outside largest
    level
        | pressed marks bounds sample = Braking
        | all (< bmGcLowPermille marks) (gsGcSharePermille sample) = Calm
        | otherwise = Holding
    calm = bsCalmSamples before + 1
    growNow = level == Calm && calm >= bmCalmSamples marks
    kept = min ceilingNow (bsBudget before)
    halveNow = level == Braking && bsCooldown before <= 0
    held
        | halveNow = max (bbFloorBytes bounds) (kept `div` 2)
        | otherwise = kept
    grown
        | growNow = min ceilingNow (held + max (bbGrowFloorBytes bounds) (held * bmGrowPermille marks `div` 1000))
        | otherwise = held

{- The most the budget may reach: the live ceiling, or the overflow point less the largest request
when that is lower, less the live data outside the charges. Without a heap ceiling, the boot budget. -}
budgetCeiling :: BrakeBounds -> Maybe Int -> Int -> Int
budgetCeiling bounds outside largest = max (bbFloorBytes bounds) $ case bbLiveCeilingBytes bounds of
    Nothing -> bbBootBytes bounds
    Just live -> maybe live (min live . subtract largest) (bbOverflowLiveBytes bounds) - max (bbFixedLiveBytes bounds) (fromMaybe (bbExplainedBytes bounds) outside)

-- A rise in the remainder counts at once. A fall is given back a fraction at a time.
measuredOutside :: BrakeMarks -> BrakeState -> Int -> Int -> Int
measuredOutside marks before charged live = case bsOutside before of
    Just previous | measured < previous -> previous - (previous - measured) * bmReturnPermille marks `div` 1000
    _ -> measured
  where
    measured = live - charged

pressed :: BrakeMarks -> BrakeBounds -> GcSample -> Bool
pressed marks bounds sample =
    any (> bmGcHighPermille marks) (gsGcSharePermille sample)
        || any (> bmKernelHighPermille marks) (gsKernelPermille sample)
        || nearOverflow
  where
    nearOverflow = case (gsLiveAfterMajor sample, bbOverflowLiveBytes bounds) of
        (Just live, Just overflow) -> live * 1000 > overflow * bmOverflowGuardPermille marks
        _ -> False

-- | The collector's cumulative counters at one sample.
data CollectorReading = CollectorReading
    { crCpuNs :: Int64
    -- ^ Process CPU time, collector included.
    , crGcCpuNs :: Int64
    -- ^ Collector CPU time.
    , crMajorCollections :: Word32
    , crLiveBytes :: Int
    -- ^ Live bytes after the latest collection, exact after a major one.
    }
    deriving stock (Eq, Show)

-- | The collector's share of the CPU between two readings, or 'Nothing' when no CPU passed.
gcSharePermille :: CollectorReading -> CollectorReading -> Maybe Int
gcSharePermille older newer
    | cpu <= 0 = Nothing
    | otherwise = Just (fromIntegral (min 1000 (max 0 (gc * 1000 `div` cpu))))
  where
    cpu = crCpuNs newer - crCpuNs older
    gc = crGcCpuNs newer - crGcCpuNs older

-- | The recent readings the GC share spans, newest first, and the capacity of that span.
data SampleWindow = SampleWindow Int [CollectorReading]

-- | An empty window that spans the given number of sampling periods.
newSampleWindow :: Int -> SampleWindow
newSampleWindow periods = SampleWindow (max 1 periods) []

{- | Fold one reading into the window and form the brake's sample. Live data counts only after a major
collection since an earlier reading, and a missing reading (no @-T@) leaves the collector's rules out.
-}
windowSample :: SampleWindow -> Maybe CollectorReading -> Int -> Int -> Maybe Int -> (GcSample, SampleWindow)
windowSample (SampleWindow periods readings) reading charged largest kernel =
    ( GcSample
        { gsGcSharePermille = do
            newest <- reading
            oldest <- listToMaybe (reverse spanned)
            gcSharePermille oldest newest
        , gsLiveAfterMajor = do
            newest <- reading
            previous <- listToMaybe readings
            guard (crMajorCollections previous /= crMajorCollections newest)
            pure (crLiveBytes newest)
        , gsChargedBytes = charged
        , gsLargestCharge = largest
        , gsKernelPermille = kernel
        }
    , SampleWindow periods spanned
    )
  where
    spanned = take (periods + 1) (maybeToList reading <> readings)
