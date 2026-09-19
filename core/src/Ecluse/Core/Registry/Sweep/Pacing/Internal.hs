-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The arithmetic "Ecluse.Core.Registry.Sweep.Pacing" decides a cycle's pace with: the seconds a
cycle has, the share of a scope's capacity it may take, and the seconds a tally of requests costs
at that share.

Importing this module opts out of the public surface's stability promises. It exists so a spec can
pin each step of the derivation against the decision built from it.
-}
module Ecluse.Core.Registry.Sweep.Pacing.Internal (
    cycleAllowance,
    nominalPackagePace,
    budgetFraction,
    ceilingsFor,
    cycleDemand,
    BudgetShortfall (..),
) where

import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Data.Time (NominalDiffTime)

import Ecluse.Core.Registry.Maintenance.Budget (
    QuotaDimension,
    RequestTally,
    StoreBudget (bgCosts, bgQuotas),
    smallestQuota,
    tallyCounts,
 )
import Ecluse.Core.Registry.Sweep.Types (
    SweepPacing (swpBudgetFraction, swpChunkPause, swpChunkSize, swpCyclePause, swpCycleWindow),
    minimumChunkPause,
 )

{- | The seconds one complete cycle has. A name newly covered by an advisory can miss the running
cycle's selection, so the window must cover that cycle, the pause, and the next one.
-}
cycleAllowance :: SweepPacing -> Rational
cycleAllowance pacing = (toRational (swpCycleWindow pacing) - toRational (swpCyclePause pacing)) / 2

{- | The sweep's own nominal package pace, in requests per second. It is the one dial an operator
already has over how hard a cycle leans on a store, so both the derived capacity and the share use it.
-}
nominalPackagePace :: Int -> NominalDiffTime -> Rational
nominalPackagePace chunkSize chunkPause =
    toRational (max 1 chunkSize) / toRational (max minimumChunkPause chunkPause)

{- | The share of a scope's capacity the sweep may take: the configured fraction, else the smaller
of half the capacity and the share the nominal package pace already implies.
-}
budgetFraction :: SweepPacing -> StoreBudget -> Rational
budgetFraction pacing budget = fromMaybe computed (swpBudgetFraction pacing)
  where
    computed = maybe fractionCeiling implied (smallestQuota budget)
    implied smallest
        | smallest <= 0 = fractionCeiling
        | otherwise = min fractionCeiling (pacingPace pacing / smallest)

-- The nominal pace of this pacing's own chunk size and chunk pause.
pacingPace :: SweepPacing -> Rational
pacingPace pacing = nominalPackagePace (swpChunkSize pacing) (swpChunkPause pacing)

-- The largest share of a store's capacity a sweep takes, leaving the rest to the proxy's calls.
fractionCeiling :: Rational
fractionCeiling = 1 % 2

-- | The per-second ceilings one scope holds itself to under that share.
ceilingsFor :: Rational -> StoreBudget -> Map QuotaDimension Rational
ceilingsFor fraction = Map.map (fraction *) . bgQuotas

{- | The seconds a tally of requests takes at those ceilings. Each attempt costs the longest its
own dimensions hold it to, and the cycle makes them one at a time, so the costs add.
-}
cycleDemand :: Map QuotaDimension Rational -> StoreBudget -> RequestTally -> Rational
cycleDemand ceilings budget tally =
    sum [toRational count * requestSeconds kind | (kind, count) <- tallyCounts tally]
  where
    requestSeconds kind =
        foldr
            max
            0
            [ cost / limit
            | (dimension, cost) <- Map.toList (Map.findWithDefault Map.empty kind (bgCosts budget))
            , Just limit <- [Map.lookup dimension ceilings]
            , limit > 0
            ]

-- | Why a cycle cannot be paced inside the window, which the warning names.
data BudgetShortfall
    = -- | The cycle's own work fills the allowance, so no request share reaches the window.
      WorkFillsAllowance
    | -- | The window needs this share of the scope's capacity, above the ceiling in force.
      NeedsFraction Rational
    deriving stock (Eq, Show)
