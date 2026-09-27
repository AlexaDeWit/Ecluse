-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The arithmetic of the metadata memory budget: what a byte count costs, how many steps cover a
shortfall, and which request the meter lets move. "Ecluse.Core.Server.Admission.Meter" applies
these decisions inside its transactions.
-}
module Ecluse.Core.Server.Admission.Budget (
    -- * Charges
    ChargeFactors (..),
    scaleCharge,
    roundUpToStep,

    -- * Decisions
    MeterView (..),
    EntryGate (..),
    entryDecision,
    entryReady,
    GrowthGate (..),
    growthDecision,
) where

-- | Live heap bytes charged per source byte, in thousandths, for one ecosystem's metadata.
data ChargeFactors = ChargeFactors
    { cfFullReadPermille :: Int
    -- ^ Per decompressed byte of a full read: the typed projection plus the raw document it keeps.
    , cfOutputPermille :: Int
    -- ^ Per source byte a listing merges and encodes into its response.
    }
    deriving stock (Eq, Show)

-- | The charge for a byte count, rounded up, so a non-empty read never costs nothing.
scaleCharge :: Int -> Int -> Int
scaleCharge permille bytes
    | bytes <= 0 || permille <= 0 = 0
    | otherwise = (bytes * permille + 999) `div` 1000

-- | The smallest whole number of steps that covers a shortfall, in bytes.
roundUpToStep :: Int -> Int -> Int
roundUpToStep step shortfall
    | shortfall <= 0 = 0
    | otherwise = ((shortfall + size - 1) `div` size) * size
  where
    size = max 1 step

-- | What a meter transaction reads before it decides.
data MeterView = MeterView
    { mvBudget :: Int
    , mvCharged :: Int
    , mvToken :: Maybe Int
    -- ^ The ticket that holds the overdraw token, if any.
    , mvOldestWaiter :: Maybe Int
    -- ^ The oldest ticket paused on a growth step, if any.
    }
    deriving stock (Eq, Show)

-- | The door's answer to a new request.
data EntryGate
    = -- | The entry step fits now.
      EntryAdmit
    | -- | Wait for the step, up to the admission wait.
      EntryQueue
    | -- | The waiting room is full: shed at once.
      EntryRefuse
    deriving stock (Eq, Show)

{- | New work takes its entry step only when it fits, nobody queued before it, and no started read
is paused. A paused read always has the prior claim on freed memory.
-}
entryDecision :: MeterView -> Int -> Int -> Int -> EntryGate
entryDecision view waiting room step
    | waiting == 0 && entryReady view step = EntryAdmit
    | waiting >= room = EntryRefuse
    | otherwise = EntryQueue

-- | Whether a queued request may take its entry step now.
entryReady :: MeterView -> Int -> Bool
entryReady view step = fits view step && isNothing (mvOldestWaiter view)

-- | The answer to a started request that needs more bytes.
data GrowthGate
    = -- | The step fits within the budget.
      GrowWithin
    | -- | The step does not fit, but this ticket may overdraw.
      GrowOverdraw
    | -- | Pause until memory frees.
      GrowWait
    deriving stock (Eq, Show)

{- | A step that fits proceeds. Otherwise only the token holder moves, or, when nobody holds the
token, the oldest paused ticket. So at most one ticket overdraws at a time.
-}
growthDecision :: MeterView -> Int -> Int -> GrowthGate
growthDecision view ticket want
    | fits view want = GrowWithin
    | mvToken view == Just ticket = GrowOverdraw
    | isNothing (mvToken view) && all (>= ticket) (mvOldestWaiter view) = GrowOverdraw
    | otherwise = GrowWait

fits :: MeterView -> Int -> Bool
fits view bytes = mvCharged view + bytes <= mvBudget view
