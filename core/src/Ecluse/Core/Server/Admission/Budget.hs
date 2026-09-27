-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The arithmetic of the metadata memory budget: what a byte count costs, how many steps cover a
shortfall, and which request the meter lets move. "Ecluse.Core.Server.Admission.Meter" applies
these decisions inside its transactions.
-}
module Ecluse.Core.Server.Admission.Budget (
    -- * Charges
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

import Data.IntSet qualified as IntSet

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
    -- ^ The oldest ticket paused on a growth step or waiting on the work of one, if any.
    }
    deriving stock (Eq, Show)

-- | The gate's answer to a new request.
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
    | -- | The step does not fit, but it serves the ticket that holds the overdraw token.
      GrowOnToken
    | -- | The step does not fit, the token is free, and this charge serves the oldest paused ticket.
      GrowTakeToken
    | -- | Pause until memory frees.
      GrowWait
    deriving stock (Eq, Show)

{- | A step that fits proceeds. Past the budget, only work serving the token holder moves, or with a free
token, work serving the oldest paused ticket. @served@: the charging ticket and those waiting on it.
-}
growthDecision :: MeterView -> IntSet -> Int -> GrowthGate
growthDecision view served want
    | fits view want = GrowWithin
    | any (`IntSet.member` served) (mvToken view) = GrowOnToken
    | isNothing (mvToken view) && oldestServed = GrowTakeToken
    | otherwise = GrowWait
  where
    oldestServed = case (fst <$> IntSet.minView served, mvOldestWaiter view) of
        (Just ticket, Just oldest) -> ticket <= oldest
        (Just _, Nothing) -> True
        (Nothing, _) -> False

fits :: MeterView -> Int -> Bool
fits view bytes = mvCharged view + bytes <= mvBudget view
