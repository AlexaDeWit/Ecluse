-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Pacing one sweep cycle inside the target cycle window. Every decision here is a pure function
of the last complete cycle's measured request counts and the capacity the store declared.
-}
module Ecluse.Core.Registry.Sweep.Pacing (
    -- * The window and the capacity a cycle is paced against
    defaultCycleWindow,
    nominalPackagePace,
    derivedCapacity,
    renderScopeBudget,

    -- * The decision the next cycle runs under
    PaceDecision (..),
    BudgetShortfall,
    decidePace,
    renderPaceDecision,
) where

import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime)

import Ecluse.Core.Registry.Maintenance.Budget (
    CyclePace,
    QuotaDimension (StoreRequests),
    QuotaOrigin (QuotaDerived),
    QuotaScope,
    RequestTally,
    StoreBudget (bgOrigin, bgQuotas, bgScope),
    budgetDeclared,
    oneRequest,
    paceOf,
    renderQuotaScope,
    renderRates,
    renderStoreBudget,
    requestKinds,
    toHundredths,
 )
import Ecluse.Core.Registry.Sweep.Pacing.Internal (
    BudgetShortfall (NeedsFraction, WorkFillsAllowance),
    budgetFraction,
    ceilingsFor,
    cycleAllowance,
    cycleDemand,
    nominalPackagePace,
 )
import Ecluse.Core.Registry.Sweep.Types (
    SweepPacing (swpBudgetFraction, swpChunkPause, swpChunkSize, swpCycleWindow),
 )

{- | The window a cycle is paced to finish inside when the configuration names none: three cycle
pauses, so each active cycle is granted the same allowance as the idle interval between them.
-}
defaultCycleWindow :: NominalDiffTime -> NominalDiffTime
defaultCycleWindow cyclePause = 3 * cyclePause

{- | The capacity a backend that publishes no quota is taken to have: the nominal package pace, on
the one request dimension. Raising the chunk pause lowers it, and a declared capacity replaces it.
-}
derivedCapacity :: Rational -> StoreBudget -> StoreBudget
derivedCapacity pace budget
    | budgetDeclared budget = budget
    | otherwise = budget{bgQuotas = Map.singleton StoreRequests pace, bgOrigin = QuotaDerived}

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
            , pdPace = paceAtShare pacing budget ceilings share
            , pdShortfall = shortfall
            }

-- A share below one stretches every request's own cost by the same factor.
paceAtShare :: SweepPacing -> StoreBudget -> Map QuotaDimension Rational -> Rational -> CyclePace
paceAtShare pacing budget ceilings share =
    paceOf (Map.fromList [(kind, held (seconds kind / share)) | kind <- requestKinds, seconds kind > 0])
  where
    held = min (paceBound pacing)
    seconds kind = cycleDemand ceilings budget (oneRequest kind)

{- The longest one request is ever held. A wait past a whole cycle's allowance cannot land that
cycle inside the window, and an unbounded one would overrun the thread delay. -}
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
