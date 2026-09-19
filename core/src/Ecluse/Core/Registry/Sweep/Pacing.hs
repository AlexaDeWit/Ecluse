-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Pacing one sweep cycle inside the target cycle window. Every decision here is a pure function
of the last complete cycle's measured request counts and the capacity the store declared.
-}
module Ecluse.Core.Registry.Sweep.Pacing (
    -- * The window and the share of capacity it may use
    defaultCycleWindow,
    cycleAllowance,
    nominalPackagePace,
    derivedCapacity,
    budgetFraction,
    ceilingsFor,
    renderScopeBudget,

    -- * What a cycle's own requests demand
    cycleDemand,

    -- * The decision the next cycle runs under
    PaceDecision (..),
    BudgetShortfall (..),
    decidePace,
    renderPaceDecision,
) where

import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Data.Time (NominalDiffTime)

import Ecluse.Core.Registry.Maintenance.Budget (
    CyclePace,
    QuotaDimension (StoreRequests),
    QuotaOrigin (QuotaDerived),
    QuotaScope,
    RequestTally,
    StoreBudget (bgCosts, bgOrigin, bgQuotas, bgScope),
    budgetDeclared,
    oneRequest,
    paceOf,
    renderQuotaScope,
    renderRates,
    renderStoreBudget,
    requestKinds,
    smallestQuota,
    tallyCounts,
    toHundredths,
 )
import Ecluse.Core.Registry.Sweep.Types (
    SweepPacing (swpBudgetFraction, swpChunkPause, swpChunkSize, swpCyclePause, swpCycleWindow),
    minimumChunkPause,
 )

{- | The window a cycle is paced to finish inside when the configuration names none: three cycle
pauses, so each active cycle is granted the same allowance as the idle interval between them.
-}
defaultCycleWindow :: NominalDiffTime -> NominalDiffTime
defaultCycleWindow cyclePause = 3 * cyclePause

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

{- | The capacity a backend that publishes no quota is taken to have: the nominal package pace, on
the one request dimension. Raising the chunk pause lowers it, and a declared capacity replaces it.
-}
derivedCapacity :: Rational -> StoreBudget -> StoreBudget
derivedCapacity pace budget
    | budgetDeclared budget = budget
    | otherwise = budget{bgQuotas = Map.singleton StoreRequests pace, bgOrigin = QuotaDerived}

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

-- The nominal pace of this pacing's own chunk keys.
pacingPace :: SweepPacing -> Rational
pacingPace pacing = nominalPackagePace (swpChunkSize pacing) (swpChunkPause pacing)

-- | The largest share of a store's capacity a sweep takes, leaving the rest to the proxy's calls.
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

-- | What one scope's next cycle runs at, and what it cannot reach inside the window.
data PaceDecision = PaceDecision
    { pdScope :: QuotaScope
    , pdFraction :: Rational
    -- ^ The share of the scope's capacity this decision held the sweep to.
    , pdPace :: CyclePace
    , pdShortfall :: Maybe BudgetShortfall
    }
    deriving stock (Eq, Show)

{- | Pace one scope's next cycle from the last complete one: its measured requests, and the seconds
that cycle spent outside its own budget waits. With no sample it runs at the ceiling and measures.
-}
decidePace :: SweepPacing -> StoreBudget -> Maybe (RequestTally, Rational) -> PaceDecision
decidePace pacing budget sample
    | not (budgetDeclared budget) = decision 1 Nothing
    | otherwise = maybe (decision 1 Nothing) measured sample
  where
    fraction = budgetFraction pacing budget
    ceilings = ceilingsFor fraction budget

    measured (tally, workSeconds)
        | headroom <= 0 = decision 1 (Just WorkFillsAllowance)
        | demand <= 0 = decision 1 Nothing
        | demand > headroom = decision 1 (Just (NeedsFraction (fraction * demand / headroom)))
        | otherwise = decision (demand / headroom) Nothing
      where
        headroom = cycleAllowance pacing - workSeconds
        demand = cycleDemand ceilings budget tally

    decision share shortfall =
        PaceDecision
            { pdScope = bgScope budget
            , pdFraction = fraction
            , pdPace = paceAt share
            , pdShortfall = shortfall
            }

    -- A share below one stretches every request's own cost by the same factor.
    paceAt share =
        paceOf (Map.fromList [(kind, held (seconds kind / share)) | kind <- requestKinds, seconds kind > 0])
    held = min (paceBound pacing)
    seconds kind = cycleDemand ceilings budget (oneRequest kind)

{- | The longest one request is ever held. A wait past a whole cycle's allowance cannot land that
cycle inside the window, and an unbounded one would overrun the thread delay.
-}
paceBound :: SweepPacing -> Rational
paceBound pacing = max 1 (cycleAllowance pacing)

{- | The warning an unattainable window earns, naming the budget it would need. The sweep runs on
at its ceiling, because a refused cycle leaves the denied version served.
-}
renderPaceDecision :: SweepPacing -> PaceDecision -> Maybe Text
renderPaceDecision pacing decision = shortfall <$> pdShortfall decision
  where
    opening =
        "the sweep cannot finish inside the target cycle window of "
            <> show (swpCycleWindow pacing)
            <> " against "
            <> renderQuotaScope (pdScope decision)
    ceilingClause = ", above the ceiling of " <> share (pdFraction decision) <> " in force"
    closing = ". The sweep runs on at that ceiling"
    shortfall = \case
        WorkFillsAllowance ->
            opening
                <> ": its own work already fills the allowance of "
                <> show (toHundredths (cycleAllowance pacing))
                <> " seconds, so no request budget reaches the window"
                <> closing
        NeedsFraction needed ->
            opening
                <> ": it would need "
                <> share needed
                <> " of the store's request capacity"
                <> ceilingClause
                <> closing
    share value = show (toHundredths value)

{- | What one store's capacity resolved to, for the boot line: where each figure came from, the
share in force, and the per-second ceilings that share yields.
-}
renderScopeBudget :: SweepPacing -> StoreBudget -> Text
renderScopeBudget pacing budget =
    "paced against "
        <> renderQuotaScope (bgScope budget)
        <> ": "
        <> capacity
        <> ", fraction "
        <> show (toHundredths fraction)
        <> " ("
        <> fractionOrigin
        <> "), ceilings "
        <> ceilings
  where
    fraction = budgetFraction pacing budget
    fractionOrigin = if isJust (swpBudgetFraction pacing) then "from configuration" else "computed"
    capacity = case bgOrigin budget of
        QuotaDerived ->
            "capacity derived from dredger.chunkSize "
                <> show (swpChunkSize pacing)
                <> " every "
                <> show (swpChunkPause pacing)
                <> " ("
                <> renderRates (bgQuotas budget)
                <> ")"
        _ -> renderStoreBudget budget
    ceilings
        | Map.null resolved = "none"
        | otherwise = renderRates resolved
    resolved = ceilingsFor fraction budget
