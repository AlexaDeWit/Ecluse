-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pure sweep pacing from measured requests and declared capacity, with quotas taking priority over the target window.
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

-- | Default window granting each active cycle the same allowance as the pause between cycles.
defaultCycleWindow :: NominalDiffTime -> NominalDiffTime
defaultCycleWindow cyclePause = 3 * cyclePause

-- | Use the nominal package pace where the backend declares no capacity.
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

-- | Pace the next cycle from measured requests and work outside budget waits. With no sample, use the ceiling.
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
    paceOf (Map.fromList [(kind, held baseCost) | kind <- requestKinds, let baseCost = seconds kind, baseCost > 0])
  where
    held baseCost = max baseCost (min (paceBound pacing) (baseCost / share))
    seconds kind = cycleDemand ceilings budget (oneRequest kind)

-- Cap optional slowdown at the cycle allowance. The quota delay still takes priority.
paceBound :: SweepPacing -> Rational
paceBound pacing = max 1 (cycleAllowance pacing)

-- | Warn about an unattainable window while the sweep continues at its request ceiling.
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

-- | Report the resolved capacity, its origin, the share in force, and the per-second ceilings at boot.
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
