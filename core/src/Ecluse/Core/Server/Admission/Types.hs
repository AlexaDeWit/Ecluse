-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The memory gate's shared vocabulary: what adapters charge, what the meter reports, and the
bounds the boot hands the brake. "Ecluse.Core.Server.Admission.Meter" and
"Ecluse.Core.Server.Admission.Brake" act on these, and the adapters, the sampler, the memory plan
and the gauges read or build them.
-}
module Ecluse.Core.Server.Admission.Types (
    -- * Charges
    ChargeFactors (..),
    FlightKey (..),

    -- * The meter's figures
    MeterSnapshot (..),

    -- * The brake
    BrakeLevel (..),
    brakeLevelCode,
    BrakeBounds (..),
) where

-- | Live heap bytes charged per source byte, in thousandths, for one ecosystem's metadata.
data ChargeFactors = ChargeFactors
    { cfFullReadPermille :: Int
    {- ^ Per decompressed byte of a full read: 1.25 times the largest read peak per source byte on a
    residency capture of at least one meter step, rounded up to a tenth.
    -}
    , cfOutputPermille :: Int
    -- ^ Per source byte a listing merges and encodes into its response.
    }
    deriving stock (Eq, Show)

-- | One unit of work several requests may wait on: a shared fetch or a shared render, by its cache key.
newtype FlightKey = FlightKey Text
    deriving stock (Eq, Ord, Show)

-- | What the meter holds and who waits on it, for the gauges and the sampler.
data MeterSnapshot = MeterSnapshot
    { snBudgetBytes :: Int
    , snChargedBytes :: Int
    -- ^ What the requests in flight hold against the budget.
    , snWaiting :: Int
    -- ^ New requests waiting at the gate for their entry step.
    , snPaused :: Int
    -- ^ Started requests paused until memory frees.
    , snBrakeLevel :: BrakeLevel
    -- ^ The level of the brake step that last moved the budget.
    }
    deriving stock (Eq, Show)

-- | Where the brake stands after a sample.
data BrakeLevel
    = -- | The collector is calm: the budget may grow.
      Calm
    | -- | Neither calm nor pressed: the budget holds.
      Holding
    | -- | Pressure seen in this sample: the budget halved, or holds while a halving settles.
      Braking
    deriving stock (Eq, Show)

-- | The gauge value of a level: 0 calm, 1 holding, 2 braking.
brakeLevelCode :: BrakeLevel -> Int
brakeLevelCode = \case
    Calm -> 0
    Holding -> 1
    Braking -> 2

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
