-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The remediation verdict over hand-built advisory rows, and its laws against a naive reference.
module Ecluse.Core.Rules.RemediationSpec (spec) where

import Hedgehog (Gen, forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Cve (AdvisoryRange (..), packageAdvisories)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Osv.Types (UpperBound (FixedBefore, Unbounded))
import Ecluse.Core.Package (mkPackageName)
import Ecluse.Core.Rules.Remediation (remediationVerdict)
import Ecluse.Core.Rules.Types
import Ecluse.Core.Version (mkVersion)
import Ecluse.Rules.Remediation.Support (genEpss, genLimits, genRows, genTenths, referenceRemediation)
import Ecluse.Rules.Support (normaliseVerdictIds)
import Ecluse.Test.Cve (referenceInside)

spec :: Spec
spec = do
    examplesSpec
    boundarySpec
    propertiesSpec

-- | The verdict for one version of @thing@ from these rows.
verdictFor :: RemainingAdvisories -> Ecosystem -> [AdvisoryRange] -> Text -> RuleVerdict
verdictFor remaining eco rows version =
    remediationVerdict remaining (packageAdvisories eco rows) (identityEvidence (mkPackageName eco Nothing "thing") (mkVersion eco version))

shipped :: RemainingAdvisories
shipped = Differential defaultDifferentialLimits

-- | An advisory over @[introduced, fixed)@ with its stored CVSS and EPSS.
advisory :: Text -> Text -> Maybe Text -> Maybe Double -> Maybe Double -> AdvisoryRange
advisory advisoryId introduced fixed cvss = AdvisoryRange advisoryId cvss (Just introduced) (maybe Unbounded FixedBefore fixed)

-- | One ecosystem's spellings: an affected release, the partial fix under test, and a later release.
data Spelling = Spelling
    { spEco :: Ecosystem
    , spBefore :: Text
    , spFix :: Text
    , spLater :: Text
    }

spellings :: [Spelling]
spellings = [Spelling Npm "1.0.0" "2.0.0" "3.0.0", Spelling PyPI "1.0" "2.0" "3.0"]

-- | The severe advisory the release under test fixes.
severe :: Spelling -> Maybe Double -> Maybe Double -> AdvisoryRange
severe sp = advisory "GHSA-severe" "0" (Just (spFix sp))

-- | The minor advisory that still affects the release under test, until the later release.
minor :: Spelling -> Maybe Double -> Maybe Double -> AdvisoryRange
minor sp = advisory "GHSA-minor" (spBefore sp) (Just (spLater sp))

ids :: Text -> [Text] -> AdvisoryIds
ids lead rest = mkAdvisoryIds (lead :| rest)

-- | A partial fix of GHSA-severe that leaves GHSA-minor, with the sides compared.
compared :: ScoreSides -> ScoreSides -> DifferentialLimits -> ScoreComparison
compared = ScoreComparison (ids "GHSA-severe" []) (ids "GHSA-minor" [])

sides :: Maybe Double -> Maybe Double -> ScoreSides
sides fixed remaining = ScoreSides (maybe Unscored Scored fixed) (maybe Unscored Scored remaining)

within :: ScoreSides -> ScoreSides -> DifferentialLimits -> RuleVerdict
within cvss epss limits = Allow (RemediatesWithinLimits (compared cvss epss limits))

beyond :: ScoreSides -> ScoreSides -> DifferentialLimits -> NonEmpty LimitShortfall -> RuleVerdict
beyond cvss epss limits shortfalls = NoDecision (RemediationBeyondLimits (compared cvss epss limits) (mkLimitShortfalls shortfalls))

-- | Each case at the release under test: its label, the mode, the rows, and the verdict.
partialCases :: Spelling -> [(String, RemainingAdvisories, [AdvisoryRange], RuleVerdict)]
partialCases sp =
    [ ("admits a partial fix inside the shipped limits", shipped, [severe sp (Just 9.8) (Just 0.5), minor sp (Just 4.2) (Just 0.005)], within (sides (Just 9.8) (Just 4.2)) (sides (Just 0.5) (Just 0.005)) defaults)
    , ("abstains when the remaining CVSS is above its ceiling", shipped, [severe sp (Just 9.8) (Just 0.5), minor sp (Just 7.0) (Just 0.005)], beyond (sides (Just 9.8) (Just 7.0)) (sides (Just 0.5) (Just 0.005)) defaults (RemainingAboveCeiling Cvss :| []))
    , ("abstains when the remaining EPSS reaches its ceiling", shipped, [severe sp (Just 9.8) (Just 0.5), minor sp (Just 4.2) (Just 0.01)], beyond (sides (Just 9.8) (Just 4.2)) (sides (Just 0.5) (Just 0.01)) defaults (RemainingAboveCeiling Epss :| []))
    , ("abstains on a CVSS gap of 0.9", shipped, [severe sp (Just 7.8) (Just 0.5), minor sp (Just 6.9) (Just 0.005)], beyond (sides (Just 7.8) (Just 6.9)) (sides (Just 0.5) (Just 0.005)) defaults (GapBelowMinimum Cvss :| []))
    , ("abstains when an EPSS difference of 0.04 misses a gap of 0.05", Differential epssGapped, [severe sp (Just 9.8) (Just 0.09), minor sp (Just 4.2) (Just 0.05)], beyond (sides (Just 9.8) (Just 4.2)) (sides (Just 0.09) (Just 0.05)) epssGapped (GapBelowMinimum Epss :| []))
    , ("needs no fixed score when the CVSS gap is off", Differential noCvssGap, [severe sp Nothing Nothing, minor sp (Just 4.2) (Just 0.005)], within (sides Nothing (Just 4.2)) (sides Nothing (Just 0.005)) noCvssGap)
    , ("abstains when no fixed advisory stores the CVSS a gap needs", shipped, [severe sp Nothing (Just 0.5), minor sp (Just 4.2) (Just 0.005)], beyond (sides Nothing (Just 4.2)) (sides (Just 0.5) (Just 0.005)) defaults (FixedUnscored Cvss :| []))
    , ("abstains when a remaining advisory stores no CVSS", shipped, [severe sp (Just 9.8) (Just 0.5), minor sp Nothing (Just 0.005)], beyond (sides (Just 9.8) Nothing) (sides (Just 0.5) (Just 0.005)) defaults (RemainingUnscored Cvss (ids "GHSA-minor" []) :| []))
    , ("abstains when a remaining advisory stores no EPSS", shipped, [severe sp (Just 9.8) (Just 0.5), minor sp (Just 4.2) Nothing], beyond (sides (Just 9.8) (Just 4.2)) (sides (Just 0.5) Nothing) defaults (RemainingUnscored Epss (ids "GHSA-minor" []) :| []))
    , ("takes the highest score where one advisory's rows disagree", shipped, [severe sp (Just 9.8) (Just 0.5), minor sp (Just 4.0) (Just 0.005), advisory "GHSA-minor" "0" (Just (spLater sp)) (Just 7.0) (Just 0.005)], beyond (sides (Just 9.8) (Just 7.0)) (sides (Just 0.5) (Just 0.005)) defaults (RemainingAboveCeiling Cvss :| []))
    , ("counts a remaining advisory with one unscored row as unscored", shipped, [severe sp (Just 9.8) (Just 0.5), minor sp (Just 4.0) (Just 0.005), advisory "GHSA-minor" "0" (Just (spLater sp)) Nothing (Just 0.005)], beyond (sides (Just 9.8) Nothing) (sides (Just 0.5) (Just 0.005)) defaults (RemainingUnscored Cvss (ids "GHSA-minor" []) :| []))
    , ("reads a non-finite stored score as no score", shipped, [severe sp (Just (1 / 0)) (Just 0.5), minor sp (Just (0 / 0)) (Just 0.005)], beyond (sides Nothing Nothing) (sides (Just 0.5) (Just 0.005)) defaults (RemainingUnscored Cvss (ids "GHSA-minor" []) :| [FixedUnscored Cvss]))
    , ("ignores repeated rows", shipped, concat (replicate 3 [severe sp (Just 9.8) (Just 0.5), minor sp (Just 4.2) (Just 0.005)]), within (sides (Just 9.8) (Just 4.2)) (sides (Just 0.5) (Just 0.005)) defaults)
    , ("abstains in abstain mode on a partial fix inside the limits", AbstainOnRemaining, [severe sp (Just 9.8) (Just 0.5), minor sp (Just 4.2) (Just 0.005)], NoDecision (FixesButStillAffected (ids "GHSA-severe" []) (ids "GHSA-minor" [])))
    , ("admits a fully remediated fix with every score missing", shipped, [severe sp Nothing Nothing], Allow (Remediates (ids "GHSA-severe" [])))
    , ("admits a fully remediated fix in abstain mode too", AbstainOnRemaining, [severe sp Nothing Nothing], Allow (Remediates (ids "GHSA-severe" [])))
    , ("names no genuine fix where another range of the advisory still holds the release", shipped, [severe sp (Just 9.8) (Just 0.5), advisory "GHSA-severe" (spFix sp) (Just (spLater sp)) (Just 9.8) (Just 0.5)], NoDecision (FixStillApplies (ids "GHSA-severe" [])))
    , ("reports only genuine fixes beside a multi-range advisory", AbstainOnRemaining, [severe sp (Just 9.8) (Just 0.5), advisory "GHSA-severe" (spFix sp) (Just (spLater sp)) (Just 9.8) (Just 0.5), advisory "GHSA-other" "0" (Just (spFix sp)) (Just 8.0) Nothing], NoDecision (FixesButStillAffected (ids "GHSA-other" []) (ids "GHSA-severe" [])))
    , ("abstains on an unscored remaining MAL record", shipped, [severe sp (Just 9.8) (Just 0.5), advisory "MAL-thing" (spBefore sp) Nothing Nothing Nothing], NoDecision (RemediationBeyondLimits (ScoreComparison (ids "GHSA-severe" []) (ids "MAL-thing" []) (sides (Just 9.8) Nothing) (sides (Just 0.5) Nothing) defaults) (mkLimitShortfalls (RemainingUnscored Cvss (ids "MAL-thing" []) :| [RemainingUnscored Epss (ids "MAL-thing" [])]))))
    ]
  where
    defaults = defaultDifferentialLimits
    epssGapped = defaults{dlMaxRemainingEpss = 0.1, dlEpssGap = GapOfAtLeast 0.05}
    noCvssGap = defaults{dlCvssGap = NoGapRequired}

examplesSpec :: Spec
examplesSpec = describe "remediationVerdict" $
    for_ spellings $ \sp -> describe (show (spEco sp)) $ do
        for_ (partialCases sp) $ \(label, mode, rows, expected) ->
            it label $ verdictFor mode (spEco sp) rows (spFix sp) `shouldBe` expected
        it "names no fix for a release no advisory fixes" $
            verdictFor shipped (spEco sp) [severe sp (Just 9.8) (Just 0.5)] (spBefore sp) `shouldBe` NoDecision FixesNoAdvisory

boundarySpec :: Spec
boundarySpec = describe "decimal boundaries" $ do
    for_ spellings $ \sp -> describe (show (spEco sp)) $ do
        let decide limits fixedCvss remainingCvss fixedEpss remainingEpss =
                verdictFor (Differential limits) (spEco sp) [severe sp (Just fixedCvss) (Just fixedEpss), minor sp (Just remainingCvss) (Just remainingEpss)] (spFix sp)
            admits limits fixedCvss remainingCvss fixedEpss remainingEpss =
                decide limits fixedCvss remainingCvss fixedEpss remainingEpss `shouldBe` within (sides (Just fixedCvss) (Just remainingCvss)) (sides (Just fixedEpss) (Just remainingEpss)) limits
        it "admits a remaining CVSS of 6.9 at a gap of exactly 1.0 from 7.9" $
            admits defaultDifferentialLimits 7.9 6.9 0.5 0.005
        it "admits a CVSS difference of exactly 1.0 between 8.1 and 7.1" $
            admits defaultDifferentialLimits{dlMaxRemainingCvss = 10} 8.1 7.1 0.5 0.005
        it "admits an EPSS difference of exactly 0.05 between 0.12 and 0.07" $
            admits defaultDifferentialLimits{dlMaxRemainingEpss = 1, dlEpssGap = GapOfAtLeast 0.05} 9.8 4.2 0.12 0.07
        it "admits a remaining EPSS of 0.0099 and abstains at 0.01" $ do
            admits defaultDifferentialLimits 9.8 4.2 0.5 0.0099
            decide defaultDifferentialLimits 9.8 4.2 0.5 0.01 `shouldSatisfy` isAbstention

    it "passes the CVSS gap exactly when the tenths differ by at least the gap, across the grid" $ do
        let sp = Spelling Npm "1.0.0" "2.0.0" "3.0.0"
            grid = [0 .. 100] :: [Integer]
            -- Binary subtraction misjudges pairs on this grid, such as 8.1 against 7.1, so the grid can fail.
            binaryMisses = [() | gap <- [1, 5, 10, 25], fixed <- grid, remaining <- grid, (tenths fixed - tenths remaining >= tenths gap) /= (fixed - remaining >= gap)]
            mismatches =
                [ (fixed, remaining, gap)
                | gap <- [1, 5, 10, 25]
                , fixed <- grid
                , remaining <- grid
                , let limits = defaultDifferentialLimits{dlMaxRemainingCvss = 10, dlCvssGap = GapOfAtLeast (tenths gap)}
                      admitted = isAdmission (verdictFor (Differential limits) Npm [severe sp (Just (tenths fixed)) (Just 0), minor sp (Just (tenths remaining)) (Just 0)] "2.0.0")
                , admitted /= (fixed - remaining >= gap)
                ]
        binaryMisses `shouldSatisfy` (not . null)
        mismatches `shouldBe` []
  where
    tenths :: Integer -> Double
    tenths n = fromInteger n / 10

isAdmission, isAbstention :: RuleVerdict -> Bool
isAdmission = \case
    Allow{} -> True
    _ -> False
isAbstention = \case
    NoDecision{} -> True
    _ -> False

propertiesSpec :: Spec
propertiesSpec = describe "properties" . modifyMaxSuccess (const 1000) $ do
    it "agrees with a naive reference that scans row pairs on integer grids" $ hedgehog $ do
        (eco, pool) <- forAll genEcosystem
        rows <- forAll (genRows pool)
        version <- forAll (genVersion pool)
        remaining <- forAll (Gen.choice [pure AbstainOnRemaining, Differential <$> genLimits])
        let production = verdictFor remaining eco rows version
            reference = referenceRemediation remaining ((== FixedBefore version) . arUpperBound) (referenceInside eco version) rows
        H.cover 2 "a partial fix is admitted" (isPartialAdmission production)
        H.cover 10 "a partial fix abstains on a limit" (isBeyondLimits production)
        normaliseVerdictIds production === normaliseVerdictIds reference

    it "never admits under stricter limits what looser limits leave" $ hedgehog $ do
        (eco, pool) <- forAll genEcosystem
        rows <- forAll (genRows pool)
        version <- forAll (genVersion pool)
        loose <- forAll genLimits
        strict <- forAll (genStricter loose)
        let decide limits = verdictFor (Differential limits) eco rows version
        H.cover 4 "the looser limits admit a partial fix" (isPartialAdmission (decide loose))
        when (isAdmission (decide strict)) (H.assert (isAdmission (decide loose)))

    it "admits in abstain mode only full fixes, which every mode admits alike" $ hedgehog $ do
        (eco, pool) <- forAll genEcosystem
        rows <- forAll (genRows pool)
        version <- forAll (genVersion pool)
        limits <- forAll genLimits
        let abstaining = verdictFor AbstainOnRemaining eco rows version
            differential = verdictFor (Differential limits) eco rows version
        H.cover 8 "a full fix" (isFullFix abstaining)
        when (isAdmission abstaining) (differential === abstaining)
        when (isFullFix differential) (abstaining === differential)

    it "never admits after a known score is replaced by none" $ hedgehog $ do
        (eco, pool) <- forAll genEcosystem
        rows <- forAll (genRows pool)
        version <- forAll (genVersion pool)
        limits <- forAll genLimits
        index <- forAll (Gen.int (Range.linear 0 (max 0 (length rows - 1))))
        unscoreEpss <- forAll Gen.bool
        let unscored = [if i == index then forget unscoreEpss row else row | (i, row) <- zip [0 ..] rows]
            decide given = verdictFor (Differential limits) eco given version
        H.cover 4 "the known scores admit a partial fix" (isPartialAdmission (decide rows))
        when (isAdmission (decide unscored)) (H.assert (isAdmission (decide rows)))

    it "ignores row order and repeated rows, up to advisory order" $ hedgehog $ do
        (eco, pool) <- forAll genEcosystem
        rows <- forAll (genRows pool)
        version <- forAll (genVersion pool)
        remaining <- forAll (Gen.choice [pure AbstainOnRemaining, Differential <$> genLimits])
        repeated <- forAll (Gen.subsequence rows)
        reordered <- forAll (Gen.shuffle (rows <> repeated))
        normaliseVerdictIds (verdictFor remaining eco reordered version) === normaliseVerdictIds (verdictFor remaining eco rows version)
  where
    forget epss row = if epss then row{arEpss = Nothing} else row{arSeverity = Nothing}

isPartialAdmission, isBeyondLimits, isFullFix :: RuleVerdict -> Bool
isPartialAdmission = \case
    Allow RemediatesWithinLimits{} -> True
    _ -> False
isBeyondLimits = \case
    NoDecision RemediationBeyondLimits{} -> True
    _ -> False
isFullFix = \case
    Allow Remediates{} -> True
    _ -> False

-- | An ecosystem and canonical releases in it, so a textual fix match is the canonical one.
genEcosystem :: Gen (Ecosystem, [Text])
genEcosystem = Gen.element [(Npm, ["1.0.0", "2.0.0", "3.0.0"]), (PyPI, ["1", "2", "3"])]

-- | The second release is the one most generated rows fix, so partial fixes are common.
genVersion :: [Text] -> Gen Text
genVersion pool = Gen.frequency [(3, Gen.element (take 1 (drop 1 pool))), (1, Gen.element pool)]

-- | Lower ceilings and larger gaps, each on its grid.
genStricter :: DifferentialLimits -> Gen DifferentialLimits
genStricter limits = do
    cvssCut <- genTenths
    epssCut <- genEpss
    DifferentialLimits (max 0 (dlMaxRemainingCvss limits - cvssCut)) (max 0 (dlMaxRemainingEpss limits - epssCut))
        <$> widen genTenths (dlCvssGap limits)
        <*> widen genEpss (dlEpssGap limits)
  where
    widen gen = \case
        NoGapRequired -> Gen.choice [pure NoGapRequired, GapOfAtLeast <$> gen]
        GapOfAtLeast least -> GapOfAtLeast . (least +) <$> gen
