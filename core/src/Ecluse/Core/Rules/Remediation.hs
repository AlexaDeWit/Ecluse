-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The remediation fast lane's verdict for each version of one package, from its advisory rows.
An exact fix admits when no advisory still affects it. A partial fix admits only in differential
mode, when the stored scores of the advisories still affecting it sit inside the limits. Scores
compare as shortest round-trip decimals, so a gap of 1.0 holds between 8.1 and 7.1.
-}
module Ecluse.Core.Rules.Remediation (remediationVerdict) where

import Data.Foldable1 qualified as Foldable1
import Data.Scientific (Scientific, fromFloatDigits)

import Ecluse.Core.Cve (AdvisoryRange (arCveId, arEpss, arSeverity), PackageAdvisories, advisoryIdsOf, affecting, fixedAt)
import Ecluse.Core.Rules.Types

-- | Decide each version. The limits convert to decimals once, when the mode is applied.
remediationVerdict :: RemainingAdvisories -> PackageAdvisories -> RuleEvidence -> RuleVerdict
remediationVerdict remaining = classify (partialVerdict remaining)

-- An exact fix that another advisory still affects: the advisories it genuinely fixes and those
-- still affecting it, each with its rows.
data PartialFix = PartialFix
    { pfFixed :: AdvisoryIds
    , pfFixedRows :: [AdvisoryRange]
    , pfRemaining :: AdvisoryIds
    , pfRemainingRows :: [AdvisoryRange]
    }

-- A fixed advisory that another of its ranges still holds is no genuine fix.
classify :: (PartialFix -> RuleVerdict) -> PackageAdvisories -> RuleEvidence -> RuleVerdict
classify onPartial advisories ev = case idsOf (advisoryIdsOf fixedRows) of
    Nothing -> NoDecision FixesNoAdvisory
    Just fixed -> case idsOf openIds of
        Nothing -> Allow (Remediates fixed)
        Just remaining -> case idsOf (advisoryIdsOf genuineRows) of
            Nothing -> NoDecision (FixStillApplies remaining)
            Just genuine -> onPartial (PartialFix genuine genuineRows remaining openRows)
  where
    fixedRows = fixedAt advisories (evVersion ev)
    openRows = affecting advisories (evVersion ev)
    openIds = advisoryIdsOf openRows
    genuineRows = filter ((`notElem` openIds) . arCveId) fixedRows

idsOf :: [Text] -> Maybe AdvisoryIds
idsOf = fmap mkAdvisoryIds . nonEmpty

partialVerdict :: RemainingAdvisories -> PartialFix -> RuleVerdict
partialVerdict = \case
    AbstainOnRemaining -> \partial -> NoDecision (FixesButStillAffected (pfFixed partial) (pfRemaining partial))
    Differential limits ->
        let cvss = scoreCheck Cvss arSeverity (<=) (dlMaxRemainingCvss limits) (dlCvssGap limits)
            epss = scoreCheck Epss arEpss (<) (dlMaxRemainingEpss limits) (dlEpssGap limits)
         in differentialVerdict limits cvss epss

-- One score kind's conditions, with its limits already decimals.
data ScoreCheck = ScoreCheck
    { ckScore :: AdvisoryScore
    , ckRead :: AdvisoryRange -> Maybe Double
    , ckWithinCeiling :: Scientific -> Bool
    , ckGapHolds :: Maybe (Scientific -> Scientific -> Bool)
    }

-- A limit that cannot convert lets nothing pass, so a malformed limit never widens the lane.
scoreCheck :: AdvisoryScore -> (AdvisoryRange -> Maybe Double) -> (Scientific -> Scientific -> Bool) -> Double -> ScoreGap -> ScoreCheck
scoreCheck score readScore withinCeiling limit gap =
    ScoreCheck
        { ckScore = score
        , ckRead = readScore
        , ckWithinCeiling = let bound = decimal limit in \highest -> maybe False (highest `withinCeiling`) bound
        , ckGapHolds = case gap of
            NoGapRequired -> Nothing
            GapOfAtLeast least -> let bound = decimal least in Just (\fixed remaining -> maybe False (fixed - remaining >=) bound)
        }

differentialVerdict :: DifferentialLimits -> ScoreCheck -> ScoreCheck -> PartialFix -> RuleVerdict
differentialVerdict limits cvss epss partial = case nonEmpty (cvssShortfalls <> epssShortfalls) of
    Nothing -> Allow (RemediatesWithinLimits comparison)
    Just shortfalls -> NoDecision (RemediationBeyondLimits comparison (mkLimitShortfalls shortfalls))
  where
    (cvssSides, cvssShortfalls) = checkScore cvss partial
    (epssSides, epssShortfalls) = checkScore epss partial
    comparison = ScoreComparison (pfFixed partial) (pfRemaining partial) cvssSides epssSides limits

-- Maxima stand for each side, so repeated rows cannot change the outcome. A remaining advisory
-- with any unscored row is unscored, so losing a score can never admit a version.
checkScore :: ScoreCheck -> PartialFix -> (ScoreSides, [LimitShortfall])
checkScore check partial =
    (ScoreSides (reading fixedHighest) (reading remainingHighest), unscored <> aboveCeiling <> gap)
  where
    score = ckScore check
    scoreOf = finite <=< ckRead check
    fixedHighest = viaNonEmpty Foldable1.maximum (mapMaybe scoreOf (pfFixedRows partial))
    perAdvisory = [(advisory, traverse scoreOf (filter ((== advisory) . arCveId) (pfRemainingRows partial))) | advisory <- toList (unAdvisoryIds (pfRemaining partial))]
    remainingHighest = viaNonEmpty Foldable1.maximum (concat [scores | (_, Just scores) <- perAdvisory])
    unscored = [RemainingUnscored score (mkAdvisoryIds ids) | Just ids <- [nonEmpty [advisory | (advisory, Nothing) <- perAdvisory]]]
    aboveCeiling = [RemainingAboveCeiling score | Just highest <- [remainingHighest], not (ckWithinCeiling check (fromFloatDigits highest))]
    gap = case (ckGapHolds check, fixedHighest, remainingHighest) of
        (Nothing, _, _) -> []
        (Just _, Nothing, _) -> [FixedUnscored score]
        (Just holds, Just fixed, Just remaining) | not (holds (fromFloatDigits fixed) (fromFloatDigits remaining)) -> [GapBelowMinimum score]
        (Just _, Just _, _) -> []

reading :: Maybe Double -> ScoreReading
reading = maybe Unscored Scored

-- A non-finite score is no score: 'fromFloatDigits' gives no meaningful decimal for one.
finite :: Double -> Maybe Double
finite x = x <$ guard (not (isNaN x || isInfinite x))

decimal :: Double -> Maybe Scientific
decimal = fmap fromFloatDigits . finite
