-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The remediation reference and generators the rules specs share. The reference scans row
pairs and compares integer tenths of CVSS and integer hundred-thousandths of EPSS, where the
engine folds maxima and compares decimals. The generators keep every score on those grids.
-}
module Ecluse.Rules.Remediation.Support (
    referenceRemediation,
    genRemediation,
    genLimits,
    genRows,
    genTenths,
    genEpss,
) where

import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Ecluse.Core.Cve (AdvisoryRange (..))
import Ecluse.Core.Osv.Types (UpperBound (FixedBefore, LastAffected, Unbounded))
import Ecluse.Core.Rules.Types

-- | The verdict for one version, from which rows name it as their fix and which rows hold it.
referenceRemediation :: RemainingAdvisories -> (AdvisoryRange -> Bool) -> (AdvisoryRange -> Bool) -> [AdvisoryRange] -> RuleVerdict
referenceRemediation remaining fixes holds ranges =
    case (idsIn fixedRows, idsIn openRows, idsIn genuineRows) of
        (Nothing, _, _) -> NoDecision FixesNoAdvisory
        (Just fixed, Nothing, _) -> Allow (Remediates fixed)
        (Just _, Just open, Nothing) -> NoDecision (FixStillApplies open)
        (Just _, Just open, Just genuine) -> case remaining of
            AbstainOnRemaining -> NoDecision (FixesButStillAffected genuine open)
            Differential limits -> referenceDifferential limits genuine open genuineRows openRows
  where
    fixedRows = filter fixes ranges
    openRows = filter holds ranges
    genuineRows = [row | row <- fixedRows, arCveId row `notElem` map arCveId openRows]

idsIn :: [AdvisoryRange] -> Maybe AdvisoryIds
idsIn = fmap mkAdvisoryIds . nonEmpty . ordNub . map arCveId

referenceDifferential :: DifferentialLimits -> AdvisoryIds -> AdvisoryIds -> [AdvisoryRange] -> [AdvisoryRange] -> RuleVerdict
referenceDifferential limits genuine open genuineRows openRows =
    case nonEmpty (cvssShortfalls <> epssShortfalls) of
        Nothing -> Allow (RemediatesWithinLimits comparison)
        Just shortfalls -> NoDecision (RemediationBeyondLimits comparison (mkLimitShortfalls shortfalls))
  where
    cvss = Side Cvss arSeverity 10 (\highest -> highest <= tenths (dlMaxRemainingCvss limits)) (dlCvssGap limits)
    epss = Side Epss arEpss 100000 (\highest -> highest < units 100000 (dlMaxRemainingEpss limits)) (dlEpssGap limits)
    tenths = units 10
    (cvssSides, cvssShortfalls) = referenceSide cvss open genuineRows openRows
    (epssSides, epssShortfalls) = referenceSide epss open genuineRows openRows
    comparison = ScoreComparison genuine open cvssSides epssSides limits

data Side = Side
    { sideScore :: AdvisoryScore
    , sideRead :: AdvisoryRange -> Maybe Double
    , sideScale :: Integer
    , sideCeiling :: Integer -> Bool
    , sideGap :: ScoreGap
    }

-- Every remaining row must pass alone, and the gap holds when some fixed row clears every remaining one.
referenceSide :: Side -> AdvisoryIds -> [AdvisoryRange] -> [AdvisoryRange] -> (ScoreSides, [LimitShortfall])
referenceSide side open genuineRows openRows =
    (ScoreSides (highest (scored genuineRows)) (highest remainingScores), unscored <> aboveCeiling <> gap)
  where
    isScored row = maybe False (\score -> not (isNaN score || isInfinite score)) (sideRead side row)
    scored rows = [score | row <- rows, isScored row, Just score <- [sideRead side row]]
    rowsOf advisory = [row | row <- openRows, arCveId row == advisory]
    unscoredIds = [advisory | advisory <- toList (unAdvisoryIds open), not (all isScored (rowsOf advisory))]
    remainingScores = scored [row | row <- openRows, arCveId row `notElem` unscoredIds]
    highest scores = maybe Unscored Scored (listToMaybe (sortBy (flip compare) scores))
    unscored = [RemainingUnscored (sideScore side) (mkAdvisoryIds ids) | Just ids <- [nonEmpty unscoredIds]]
    aboveCeiling = [RemainingAboveCeiling (sideScore side) | not (all (sideCeiling side . scaled) remainingScores)]
    scaled = units (sideScale side)
    gap = case (sideGap side, scored genuineRows, remainingScores) of
        (NoGapRequired, _, _) -> []
        (GapOfAtLeast _, [], _) -> [FixedUnscored (sideScore side)]
        (GapOfAtLeast _, _, []) -> []
        (GapOfAtLeast least, fixed, remaining)
            | any (\f -> all (\r -> scaled f - scaled r >= scaled least) remaining) fixed -> []
            | otherwise -> [GapBelowMinimum (sideScore side)]

units :: Integer -> Double -> Integer
units scale x = round (x * fromInteger scale)

-- | A remediation rule with either unavailability choice and either remaining mode.
genRemediation :: Gen Rule
genRemediation =
    fmap AllowIfRemediatesCve $
        AllowIfRemediatesCveParams
            <$> Gen.element [FailDeny, FailNoDecision]
            <*> Gen.choice [pure AbstainOnRemaining, pure (Differential defaultDifferentialLimits), Differential <$> genLimits]

-- | Limits on the grids, often the permissive or shipped ones, so generated partial fixes pass as well as fail.
genLimits :: Gen DifferentialLimits
genLimits =
    DifferentialLimits
        <$> Gen.frequency [(2, genTenths), (2, pure 10), (1, pure 6.9)]
        <*> Gen.frequency [(2, genEpss), (2, pure 1), (1, pure 0.01)]
        <*> Gen.frequency [(2, pure NoGapRequired), (2, GapOfAtLeast <$> genTenths), (1, pure (GapOfAtLeast 1.0))]
        <*> Gen.frequency [(3, pure NoGapRequired), (1, GapOfAtLeast <$> genEpss)]

-- | Rows over the releases given. Half the lists hold a partial fix of the second release, and most rows fix it.
genRows :: [Text] -> Gen [AdvisoryRange]
genRows pool = do
    core <- Gen.frequency [(1, pure []), (1, partialFix)]
    extra <- Gen.list (Range.linear (if null core then 1 else 0) 5) (genRow pool)
    Gen.shuffle (core <> extra)
  where
    release = fromMaybe "" (pool !!? 1)
    partialFix = sequence [AdvisoryRange "GHSA-a" <$> score genTenths <*> pure (Just "0") <*> pure (FixedBefore release) <*> score genEpss, AdvisoryRange "GHSA-b" <$> score genTenths <*> pure (Just "0") <*> pure Unbounded <*> score genEpss]
    score gen = Gen.frequency [(1, pure Nothing), (12, Just <$> gen)]

genRow :: [Text] -> Gen AdvisoryRange
genRow pool =
    AdvisoryRange
        <$> Gen.element ["GHSA-a", "GHSA-b", "GHSA-c", "MAL-d"]
        <*> Gen.frequency [(1, pure Nothing), (12, Just <$> genTenths)]
        <*> Gen.element (Nothing : map Just ("0" : take 2 pool))
        <*> Gen.frequency [(4, pure (FixedBefore (fromMaybe "" (pool !!? 1)))), (2, FixedBefore <$> Gen.element pool), (2, pure Unbounded), (1, LastAffected <$> Gen.element pool)]
        <*> Gen.frequency [(1, pure Nothing), (12, Just <$> genEpss)]

genTenths :: Gen Double
genTenths = (/ 10) . fromInteger <$> Gen.integral (Range.linear 0 100)

-- | EPSS on the hundred-thousandths grid, often near the shipped one-percent ceiling.
genEpss :: Gen Double
genEpss = (/ 100000) . fromInteger <$> Gen.frequency [(3, Gen.integral (Range.linear 0 2000)), (1, Gen.integral (Range.linear 0 100000))]
