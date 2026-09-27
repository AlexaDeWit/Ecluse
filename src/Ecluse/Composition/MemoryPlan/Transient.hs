-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The transient memory budget: the live data metadata requests may hold at once, carved from what
the collector can hold without strain.

A busy copying collector keeps about four times its live data, plus the nursery, so the live target
is a quarter of the heap ceiling less the nursery. The idle process and the retained tenants come off
the target first, and what remains is the budget the meter starts from. The sampler may raise it
until charges and the live data it measures outside them reach a third of that heap, two thirds of
the point where copying overflows.
-}
module Ecluse.Composition.MemoryPlan.Transient (
    TransientBudget (..),
    transientBudget,
    renderTransientBudget,
    brakeBounds,
    liveTargetBytes,
    liveCacheShareBytes,

    -- * The starting constants
    idleLiveFloorBytes,
    transientFloorBytes,
    noCeilingTransientBytes,
    liveCacheSharePercent,
    meterStepBytes,
) where

import Ecluse.Core.Server.Admission.Brake (BrakeBounds (..))

-- | The budget the boot hands the meter and the sampler, in bytes of live data.
data TransientBudget = TransientBudget
    { tbLiveTargetBytes :: Maybe Int
    -- ^ The live data the heap holds in the collector's normal regime. 'Nothing' without a ceiling.
    , tbExplainedBytes :: Int
    -- ^ Live data outside the budget: the idle floor and the retained tenants.
    , tbBootBytes :: Int
    -- ^ The budget at boot.
    , tbLiveCeilingBytes :: Maybe Int
    -- ^ The live data the sampler lets charges and the measured remainder reach together.
    , tbFloorBytes :: Int
    -- ^ The least the sampler may shrink the budget to.
    , tbOverflowLiveBytes :: Maybe Int
    -- ^ The live data at which the copying collector overflows the ceiling.
    }
    deriving stock (Eq, Show)

{- | Resolve the budget from the heap ceiling, the capability count, the allocation area and the
retained tenants' bytes. With no ceiling the budget is a large constant the brake alone moves.
-}
transientBudget :: Maybe Int -> Int -> Int -> Int -> TransientBudget
transientBudget heapCeiling capabilities allocArea retained = case heapCeiling of
    Nothing ->
        TransientBudget
            { tbLiveTargetBytes = Nothing
            , tbExplainedBytes = explained
            , tbBootBytes = noCeilingTransientBytes
            , tbLiveCeilingBytes = Nothing
            , tbFloorBytes = transientFloorBytes
            , tbOverflowLiveBytes = Nothing
            }
    Just ceiling' ->
        let copyable = copyableHeap ceiling' capabilities allocArea
            boot = max transientFloorBytes (copyable `div` 4 - explained)
         in TransientBudget
                { tbLiveTargetBytes = Just (copyable `div` 4)
                , tbExplainedBytes = explained
                , tbBootBytes = boot
                , tbLiveCeilingBytes = Just (copyable `div` 3)
                , tbFloorBytes = transientFloorBytes
                , tbOverflowLiveBytes = Just (copyable `div` 2)
                }
  where
    explained = idleLiveFloorBytes + max 0 retained

-- | The boot line for the budget, with the arithmetic an operator needs to check it.
renderTransientBudget :: TransientBudget -> Text
renderTransientBudget budget = case tbLiveTargetBytes budget of
    Nothing ->
        "memory plan: transient budget " <> show (tbBootBytes budget) <> " (built-in default; no heap-ceiling datapoint)"
    Just target ->
        "memory plan: transient budget "
            <> show (tbBootBytes budget)
            <> " (live target "
            <> show target
            <> " less "
            <> show (tbExplainedBytes budget)
            <> " idle and retained; floor "
            <> show (tbFloorBytes budget)
            <> maybe "" (\ceiling' -> "; live ceiling " <> show ceiling') (tbLiveCeilingBytes budget)
            <> ")"

-- | The fixed bounds the sampler's brake steers the budget within.
brakeBounds :: TransientBudget -> BrakeBounds
brakeBounds budget =
    BrakeBounds
        { bbBootBytes = tbBootBytes budget
        , bbFloorBytes = tbFloorBytes budget
        , bbLiveCeilingBytes = tbLiveCeilingBytes budget
        , bbFixedLiveBytes = idleLiveFloorBytes
        , bbExplainedBytes = tbExplainedBytes budget
        , bbOverflowLiveBytes = tbOverflowLiveBytes budget
        , bbGrowFloorBytes = meterStepBytes
        }

-- | The live target L for a heap ceiling: a quarter of the heap the nursery leaves.
liveTargetBytes :: Int -> Int -> Int -> Int
liveTargetBytes ceiling' capabilities allocArea = copyableHeap ceiling' capabilities allocArea `div` 4

-- | The retained cache's share of the live target, so the cache alone cannot push the heap past it.
liveCacheShareBytes :: Int -> Int -> Int -> Int
liveCacheShareBytes ceiling' capabilities allocArea =
    liveTargetBytes ceiling' capabilities allocArea * liveCacheSharePercent `div` 100

-- The nursery sits inside the heap ceiling, so it comes off before the collector's factor applies.
copyableHeap :: Int -> Int -> Int -> Int
copyableHeap ceiling' capabilities allocArea = max 0 (ceiling' - max 1 capabilities * max 0 allocArea)

{- | The live data of a booted, idle process. The load test measures about 3 MiB, and the sampler's
live correction takes any excess off the budget at run time.
-}
idleLiveFloorBytes :: Int
idleLiveFloorBytes = 8 * 1024 * 1024

-- | The smallest budget, so a small pod still moves more than one request's entry step.
transientFloorBytes :: Int
transientFloorBytes = 16 * 1024 * 1024

-- | The budget with no heap ceiling. Nothing bounds the heap, so only the GC brake moves it.
noCeilingTransientBytes :: Int
noCeilingTransientBytes = 1024 * 1024 * 1024

-- | The share of the live target the retained cache may hold.
liveCacheSharePercent :: Int
liveCacheSharePercent = 30

-- | The entry step and the unit a read pays in: one STM transaction per step.
meterStepBytes :: Int
meterStepBytes = 1024 * 1024
