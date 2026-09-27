-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The memory budget's feedback: how one sample of the collector and the heap moves the budget.

After each major collection the brake measures the live data outside the charges, so the budget may
grow until charges and that measured remainder reach the live ceiling. It halves the budget when the
collector takes too much of the CPU or the heap nears overflow, and grows it back in small steps
while the collector stays calm. The sampler in "Ecluse.Rts.Sampler" reads the statistics.
-}
module Ecluse.Core.Server.Admission.Brake (
    -- * The marks
    BrakeMarks (..),
    defaultBrakeMarks,

    -- * The fixed bounds for one process
    BrakeBounds (..),

    -- * One step
    BrakeLevel (..),
    brakeLevelCode,
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
        }

-- | What the boot fixed for this process.
data BrakeBounds = BrakeBounds
    { bbBootBytes :: Int
    -- ^ The budget the boot computed, and the ceiling when no heap ceiling binds.
    , bbFloorBytes :: Int
    -- ^ The budget never falls below this.
    , bbLiveCeilingBytes :: Maybe Int
    -- ^ The live data charges and the measured remainder may reach together, when a heap ceiling binds.
    , bbFixedLiveBytes :: Int
    -- ^ The least live data ever counted outside the charges: the idle process.
    , bbExplainedBytes :: Int
    -- ^ The boot's estimate of live data outside the charges, used until a major collection measures it.
    , bbOverflowLiveBytes :: Maybe Int
    -- ^ The live data at which the copying collector overflows the heap ceiling, when one binds.
    , bbGrowFloorBytes :: Int
    -- ^ The smallest growth step.
    }
    deriving stock (Eq, Show)

-- | Where the brake stands after a sample.
data BrakeLevel
    = -- | The collector is calm: the budget may grow.
      Calm
    | -- | Neither calm nor pressed: the budget holds.
      Holding
    | -- | Pressure seen in this sample: the budget halved.
      Braking
    deriving stock (Eq, Show)

-- | The gauge value of a level: 0 calm, 1 holding, 2 braking.
brakeLevelCode :: BrakeLevel -> Int
brakeLevelCode = \case
    Calm -> 0
    Holding -> 1
    Braking -> 2

-- | The brake's memory between samples.
data BrakeState = BrakeState
    { bsBudget :: Int
    , bsOutside :: Maybe Int
    -- ^ Live data outside the charges at recent major collections. 'Nothing' before the first.
    , bsCalmSamples :: Int
    , bsLevel :: BrakeLevel
    }
    deriving stock (Eq, Show)

-- | Start at the boot budget, with nothing measured yet.
initialBrakeState :: BrakeBounds -> BrakeState
initialBrakeState bounds =
    BrakeState{bsBudget = min (budgetCeiling bounds Nothing) (bbBootBytes bounds), bsOutside = Nothing, bsCalmSamples = 0, bsLevel = Calm}

-- | One sample's readings. A missing reading leaves its rule out of this step.
data GcSample = GcSample
    { gsGcSharePermille :: Maybe Int
    -- ^ The collector's share of CPU over the window. 'Nothing' when the process was idle.
    , gsLiveAfterMajor :: Maybe Int
    -- ^ Live bytes, present only when a major collection finished since the last sample.
    , gsChargedBytes :: Int
    -- ^ What the meter held when the sample was taken.
    , gsKernelPermille :: Maybe Int
    -- ^ The cgroup's non-reclaimable use against its limit, when a limit binds.
    }
    deriving stock (Eq, Show)

{- | Move the budget by one sample: pressure halves it, a lower ceiling cuts it at once, and only a
calm stretch grows it. The result always stays between the floor and the ceiling.
-}
brakeStep :: BrakeMarks -> BrakeBounds -> BrakeState -> GcSample -> BrakeState
brakeStep marks bounds before sample =
    BrakeState
        { bsBudget = grown
        , bsOutside = outside
        , bsCalmSamples = if level == Calm && not growNow then calm else 0
        , bsLevel = level
        }
  where
    outside = maybe (bsOutside before) (Just . measuredOutside marks before (gsChargedBytes sample)) (gsLiveAfterMajor sample)
    ceilingNow = budgetCeiling bounds outside
    level
        | pressed marks bounds sample = Braking
        | all (< bmGcLowPermille marks) (gsGcSharePermille sample) = Calm
        | otherwise = Holding
    calm = bsCalmSamples before + 1
    growNow = level == Calm && calm >= bmCalmSamples marks
    kept = min ceilingNow (bsBudget before)
    held = case level of
        Braking -> max (bbFloorBytes bounds) (kept `div` 2)
        _ -> kept
    grown
        | growNow = min ceilingNow (held + max (bbGrowFloorBytes bounds) (held * bmGrowPermille marks `div` 1000))
        | otherwise = held

{- The most the budget may reach: the live ceiling less the live data outside the charges, never
below the idle process's share, or the boot budget when no heap ceiling binds. -}
budgetCeiling :: BrakeBounds -> Maybe Int -> Int
budgetCeiling bounds outside = max (bbFloorBytes bounds) $ case bbLiveCeilingBytes bounds of
    Nothing -> bbBootBytes bounds
    Just live -> live - max (bbFixedLiveBytes bounds) (fromMaybe (bbExplainedBytes bounds) outside)

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

{- | Fold one reading into the window and form the brake's sample. Live data counts only after a new
major collection, and a missing reading (no @-T@) leaves the collector's rules out.
-}
windowSample :: SampleWindow -> Maybe CollectorReading -> Int -> Maybe Int -> (GcSample, SampleWindow)
windowSample (SampleWindow periods readings) reading charged kernel =
    ( GcSample
        { gsGcSharePermille = do
            newest <- reading
            oldest <- listToMaybe (reverse spanned)
            gcSharePermille oldest newest
        , gsLiveAfterMajor = do
            newest <- reading
            guard (fmap crMajorCollections (listToMaybe readings) /= Just (crMajorCollections newest))
            pure (crLiveBytes newest)
        , gsChargedBytes = charged
        , gsKernelPermille = kernel
        }
    , SampleWindow periods spanned
    )
  where
    spanned = take (periods + 1) (maybeToList reading <> readings)
