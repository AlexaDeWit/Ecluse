-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The memory budget's feedback: how one sample of the collector and the heap moves the budget.

The budget shrinks by any live data the charges do not explain, halves when the collector takes too
much of the CPU or the heap nears overflow, and grows back in small steps while the collector stays
calm, up to a fixed cap. The sampler in "Ecluse.Rts.Sampler" reads the statistics and applies this.
-}
module Ecluse.Core.Server.Admission.Brake (
    -- * The marks measurement sets
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

    -- * The collector's share of the CPU
    CpuReading (..),
    gcSharePermille,
) where

{- | The brake's thresholds. Each is a starting value that the thrash probe is expected to replace.
Shares are thousandths of the process's CPU time over the sampler's window.
-}
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
    , bmCorrectionDecayPermille :: Int
    -- ^ How much of a closing gap the live correction gives back per major collection.
    }
    deriving stock (Eq, Show)

{- | Starting marks. A GC share above half the CPU is where Go's memory limit stops helping, and
a quarter is about what the unconstrained load test already spends.
-}
defaultBrakeMarks :: BrakeMarks
defaultBrakeMarks =
    BrakeMarks
        { bmGcHighPermille = 500
        , bmGcLowPermille = 250
        , bmKernelHighPermille = 900
        , bmOverflowGuardPermille = 800
        , bmCalmSamples = 10
        , bmGrowPermille = 125
        , bmCorrectionDecayPermille = 250
        }

-- | What the boot fixed for this process.
data BrakeBounds = BrakeBounds
    { bbBootBytes :: Int
    -- ^ The budget the boot computed, where the live target is met.
    , bbFloorBytes :: Int
    -- ^ The budget never falls below this.
    , bbCapBytes :: Int
    -- ^ The budget never grows above this.
    , bbExplainedBytes :: Int
    -- ^ The live data the boot expects outside the budget: the idle floor and the retained tenants.
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
    , bsCorrection :: Int
    -- ^ Live data the charges did not explain at recent major collections.
    , bsCalmSamples :: Int
    , bsLevel :: BrakeLevel
    }
    deriving stock (Eq, Show)

-- | Start at the boot budget with nothing to correct.
initialBrakeState :: BrakeBounds -> BrakeState
initialBrakeState bounds =
    BrakeState{bsBudget = clampBudget bounds (bbBootBytes bounds), bsCorrection = 0, bsCalmSamples = 0, bsLevel = Calm}

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

{- | Move the budget by one sample. The live correction applies first, then pressure halves the
budget, and only a calm stretch grows it. The result always stays within the floor and the cap.
-}
brakeStep :: BrakeMarks -> BrakeBounds -> BrakeState -> GcSample -> BrakeState
brakeStep marks bounds before sample =
    BrakeState
        { bsBudget = clampBudget bounds grown
        , bsCorrection = correction
        , bsCalmSamples = if level == Calm && not growNow then calm else 0
        , bsLevel = level
        }
  where
    correction = maybe (bsCorrection before) (correctionAfterMajor marks bounds before sample) (gsLiveAfterMajor sample)
    corrected = bsBudget before - max 0 (correction - bsCorrection before)
    ceilingNow = max (bbFloorBytes bounds) (bbCapBytes bounds - correction)
    level
        | pressed marks bounds sample = Braking
        | all (< bmGcLowPermille marks) (gsGcSharePermille sample) = Calm
        | otherwise = Holding
    calm = bsCalmSamples before + 1
    growNow = level == Calm && calm >= bmCalmSamples marks
    held = case level of
        Braking -> corrected `div` 2
        _ -> min ceilingNow corrected
    grown
        | growNow = min ceilingNow (held + max (bbGrowFloorBytes bounds) (held * bmGrowPermille marks `div` 1000))
        | otherwise = held

-- A new gap raises the correction at once. A closing gap returns it a fraction at a time.
correctionAfterMajor :: BrakeMarks -> BrakeBounds -> BrakeState -> GcSample -> Int -> Int
correctionAfterMajor marks bounds before sample live
    | gap >= previous = gap
    | otherwise = previous - (previous - gap) * bmCorrectionDecayPermille marks `div` 1000
  where
    previous = bsCorrection before
    gap = max 0 (live - bbExplainedBytes bounds - gsChargedBytes sample)

pressed :: BrakeMarks -> BrakeBounds -> GcSample -> Bool
pressed marks bounds sample =
    any (> bmGcHighPermille marks) (gsGcSharePermille sample)
        || any (> bmKernelHighPermille marks) (gsKernelPermille sample)
        || nearOverflow
  where
    nearOverflow = case (gsLiveAfterMajor sample, bbOverflowLiveBytes bounds) of
        (Just live, Just overflow) -> live * 1000 > overflow * bmOverflowGuardPermille marks
        _ -> False

clampBudget :: BrakeBounds -> Int -> Int
clampBudget bounds = max (bbFloorBytes bounds) . min (max (bbFloorBytes bounds) (bbCapBytes bounds))

-- | Cumulative process CPU and collector CPU, in nanoseconds, at one sample.
data CpuReading = CpuReading
    { crCpuNs :: Int64
    , crGcCpuNs :: Int64
    }
    deriving stock (Eq, Show)

-- | The collector's share of the CPU between two readings, or 'Nothing' when no CPU passed.
gcSharePermille :: CpuReading -> CpuReading -> Maybe Int
gcSharePermille older newer
    | cpu <= 0 = Nothing
    | otherwise = Just (fromIntegral (min 1000 (max 0 (gc * 1000 `div` cpu))))
  where
    cpu = crCpuNs newer - crCpuNs older
    gc = crGcCpuNs newer - crGcCpuNs older
