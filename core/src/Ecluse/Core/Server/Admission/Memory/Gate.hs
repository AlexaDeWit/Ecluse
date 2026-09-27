-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The pure decisions of feedback memory admission: whether one memory-heavy request starts
now, waits, or is shed, from the latest measured memory plus the bytes reserved for work that
was admitted but has not yet shown up in a measurement.

Measurement lags admission, so each admitted request reserves bytes until the sampler has had
time to see them or the request ends. A closed gate reopens only below a lower mark, so a
reading that hovers at the close mark does not flip the gate on every sample. The collector
brake ("Ecluse.Core.Server.Admission.Memory.Brake") holds heavy work whatever memory reads.
Every threshold here is a starting point that load measurement will set.
-}
module Ecluse.Core.Server.Admission.Memory.Gate (
    -- * Measured memory
    MemoryView,
    mkMemoryView,
    mvUsedBytes,
    mvLimitBytes,
    Reading (..),
    unmeasured,
    pressure,
    bindingView,

    -- * Thresholds
    GateThresholds (..),
    defaultGateThresholds,
    sampleIntervalMicros,
    closeFraction,
    reopenFraction,
    reservationLifetimeTicks,
    refreshTicks,
    listingReservationBytes,
    selectedReservationBytes,

    -- * The gate
    GateState (..),
    settleGate,
    GateCore,
    newGateCore,
    coreState,
    coreReading,
    reservedBytes,
    corePressure,
    ReservationKey,
    Hold (..),
    Decision (..),
    decide,
    release,
    observe,
    refreshDue,
) where

import Data.IntMap.Strict qualified as IntMap

import Ecluse.Core.Server.Admission.Memory.Brake (BrakeState (BrakeEngaged, BrakeReleased))

-- | One measured memory figure against the ceiling at which the process is killed or halted.
data MemoryView = MemoryView
    { mvUsedBytes :: Int
    , mvLimitBytes :: Int
    -- ^ Always positive.
    }
    deriving stock (Eq, Show)

-- | Pair a measured figure with its ceiling. 'Nothing' when the ceiling is not positive.
mkMemoryView :: Int -> Int -> Maybe MemoryView
mkMemoryView used limit = MemoryView (max 0 used) limit <$ guard (limit > 0)

-- | The sampler's latest reading: every memory view that has a ceiling, and the collector brake.
data Reading = Reading
    { rdViews :: [MemoryView]
    , rdBrake :: BrakeState
    }
    deriving stock (Eq, Show)

-- | No ceiling known and the brake released: the reading before the first sample.
unmeasured :: Reading
unmeasured = Reading [] BrakeReleased

{- | The largest share of any ceiling that measured memory plus the reserved bytes would fill.
'Nothing' when no view has a ceiling, which leaves memory ungated.
-}
pressure :: Int -> [MemoryView] -> Maybe Double
pressure reserved views = case map share views of
    [] -> Nothing
    top : rest -> Just (foldl' max top rest)
  where
    share v = fromIntegral (mvUsedBytes v + max 0 reserved) / fromIntegral (mvLimitBytes v)

-- | The view closest to its ceiling, the one a gauge should report.
bindingView :: [MemoryView] -> Maybe MemoryView
bindingView = foldl' pick Nothing
  where
    pick Nothing v = Just v
    pick (Just best) v
        | fill v > fill best = Just v
        | otherwise = Just best
    fill v = fromIntegral (mvUsedBytes v) / fromIntegral (mvLimitBytes v) :: Double

{- | The gate's marks. The reopen mark sits below the close mark, so the gate has a band in which
it holds its state.
-}
data GateThresholds = GateThresholds
    { gtCloseFraction :: Double
    -- ^ The share of a ceiling at which an open gate closes.
    , gtReopenFraction :: Double
    -- ^ The share at or below which a closed gate reopens. Below 'gtCloseFraction'.
    , gtReservationTicks :: Int
    -- ^ Samples a reservation lasts while its request still runs.
    }
    deriving stock (Eq, Show)

-- | The shipped starting thresholds, each from its named constant.
defaultGateThresholds :: GateThresholds
defaultGateThresholds = GateThresholds closeFraction reopenFraction reservationLifetimeTicks

-- | How often the sampler reads memory and the collector, in microseconds.
sampleIntervalMicros :: Int
sampleIntervalMicros = 100_000

-- | The share of the tightest ceiling at which an open gate closes to new heavy work.
closeFraction :: Double
closeFraction = 0.85

-- | The share of the tightest ceiling at or below which a closed gate reopens.
reopenFraction :: Double
reopenFraction = 0.75

{- | Samples a reservation outlives its admission when the request is still running: five 100 ms
samples, about the processing time of the heaviest listing, after which measurement shows it.
-}
reservationLifetimeTicks :: Int
reservationLifetimeTicks = 5

{- | Samples a closed gate waits for a major collection before the sampler forces one. Only a
major collection lowers the heap figure the gate reads.
-}
refreshTicks :: Int
refreshTicks = 10

-- | The bytes one admitted cold listing reserves: about the live projection and output of a large package.
listingReservationBytes :: Int
listingReservationBytes = 16 * 1024 * 1024

-- | The bytes one admitted cold selected read reserves: one version's projection and its read buffers.
selectedReservationBytes :: Int
selectedReservationBytes = 1024 * 1024

-- | Whether the gate admits new heavy work.
data GateState = GateOpen | GateClosed
    deriving stock (Eq, Show)

{- | Move the gate with hysteresis on a pressure figure. An open gate closes at the close mark, a
closed gate reopens at the reopen mark, and no ceiling at all opens it.
-}
settleGate :: GateThresholds -> GateState -> Maybe Double -> GateState
settleGate t current = \case
    Nothing -> GateOpen
    Just p -> case current of
        GateOpen
            | p >= gtCloseFraction t -> GateClosed
            | otherwise -> GateOpen
        GateClosed
            | p <= gtReopenFraction t -> GateOpen
            | otherwise -> GateClosed

-- | Identifies one admitted request's reservation, so its release removes exactly that entry.
newtype ReservationKey = ReservationKey Int
    deriving stock (Eq, Show)

data Held = Held
    { heldBytes :: Int
    , heldTick :: Int
    }
    deriving stock (Eq, Show)

-- | The gate's whole state: the latest reading, the hysteresis state, and the live reservations.
data GateCore = GateCore
    { coreReading :: Reading
    , coreState :: GateState
    , coreHeld :: IntMap Held
    , coreReserved :: Int
    -- ^ The sum of 'coreHeld', kept alongside it.
    , coreNextKey :: Int
    , coreTick :: Int
    }
    deriving stock (Eq, Show)

-- | An open gate with nothing reserved and nothing measured.
newGateCore :: GateCore
newGateCore = GateCore unmeasured GateOpen IntMap.empty 0 0 0

-- | The bytes reserved for admitted work not yet measured.
reservedBytes :: GateCore -> Int
reservedBytes = coreReserved

-- | Measured memory plus the reservations, as a share of the tightest ceiling.
corePressure :: GateCore -> Maybe Double
corePressure core = pressure (coreReserved core) (rdViews (coreReading core))

-- | Why a heavy request cannot start now.
data Hold
    = -- | Measured memory plus reservations reached the close mark, or has not yet fallen to the reopen mark.
      HoldMemory
    | -- | The collector brake is engaged.
      HoldBrake
    deriving stock (Eq, Show)

-- | What one heavy request does now.
data Decision
    = -- | Start, holding the returned reservation until release.
      Admit ReservationKey
    | -- | Wait for the gate, because of the hold.
      Wait Hold
    | -- | Refuse with a retry hint: the hold outlived the wait budget or found no room to wait.
      Shed Hold
    deriving stock (Eq, Show)

{- | Decide one heavy request that would reserve the given bytes. With waiting not allowed (the
room is full or the wait budget ran out), a hold becomes a shed. The returned core carries the
settled gate state and, on admission, the new reservation. When nothing is reserved, the request
is judged on measured memory alone, so a reservation wider than the band between the marks cannot
starve every request.
-}
decide :: GateThresholds -> Int -> Bool -> GateCore -> (Decision, GateCore)
decide t bytes mayWait core = case rdBrake (coreReading core) of
    BrakeEngaged -> (holding HoldBrake, core)
    BrakeReleased -> case settled of
        GateOpen -> admitted
        GateClosed -> (holding HoldMemory, core{coreState = GateClosed})
  where
    charged = if coreReserved core == 0 then 0 else max 0 bytes
    settled = settleGate t (coreState core) (pressure (coreReserved core + charged) (rdViews (coreReading core)))
    holding hold = if mayWait then Wait hold else Shed hold
    key = coreNextKey core
    admitted =
        ( Admit (ReservationKey key)
        , core
            { coreState = GateOpen
            , coreHeld = IntMap.insert key (Held (max 0 bytes) (coreTick core)) (coreHeld core)
            , coreReserved = coreReserved core + max 0 bytes
            , coreNextKey = key + 1
            }
        )

-- | Drop one reservation. Releasing a reservation that already expired changes nothing.
release :: ReservationKey -> GateCore -> GateCore
release (ReservationKey key) core = case IntMap.lookup key (coreHeld core) of
    Nothing -> core
    Just held ->
        core
            { coreHeld = IntMap.delete key (coreHeld core)
            , coreReserved = coreReserved core - heldBytes held
            }

{- | Install a new sample: advance the tick, expire reservations older than their lifetime, and
settle the gate on the measured memory plus what stays reserved.
-}
observe :: GateThresholds -> Reading -> GateCore -> GateCore
observe t reading core =
    core
        { coreReading = reading
        , coreState = settleGate t (coreState core) (pressure reserved (rdViews reading))
        , coreHeld = kept
        , coreReserved = reserved
        , coreTick = tick
        }
  where
    tick = coreTick core + 1
    kept = IntMap.filter (\held -> tick - heldTick held <= gtReservationTicks t) (coreHeld core)
    reserved = sum (map heldBytes (IntMap.elems kept))

{- | Whether the sampler should force a major collection: the gate is closed on memory, the brake
is released, and no major collection has run for the given number of samples. A heap figure read
between majors keeps the garbage of finished work, so without one a closed gate can stay closed.
-}
refreshDue :: Int -> GateState -> BrakeState -> Int -> Bool
refreshDue interval gate brake samplesSinceMajor =
    gate == GateClosed && brake == BrakeReleased && samplesSinceMajor >= interval
