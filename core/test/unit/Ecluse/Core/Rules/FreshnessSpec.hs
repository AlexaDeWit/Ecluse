-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Where a mount's maximum advisory push age comes from, and what a push age permits.
The derivation reads one mount's own rules, so these cases fix both halves of that contract.
-}
module Ecluse.Core.Rules.FreshnessSpec (spec) where

import Data.Time (NominalDiffTime, UTCTime (UTCTime), addUTCTime, fromGregorian, nominalDay)
import Test.Hspec

import Ecluse.Core.Package (mkScope)
import Ecluse.Core.Rules.Freshness (
    AdvisoryAge (advisoryAge, advisoryMaxAge, advisoryPushedAt),
    AdvisoryAgeBasis (AgeBeforeQuarantine, AgeConfigured, AgeFloor),
    AdvisoryFreshness (AdvisoryAging, AdvisoryFresh, AdvisoryStale, AdvisoryUndated),
    AdvisoryPublication (NoGeneration, PublishedAt, UndatedGeneration),
    MaxAdvisoryAge (maxAdvisoryAge, maxAdvisoryAgeBasis),
    ageAlarmStep,
    assessAdvisoryAge,
    maxAdvisoryAgeFor,
 )
import Ecluse.Core.Rules.Types (
    DenyIfCveParams (DenyIfCveParams, dicMinCvss, dicOnUnavailable),
    FailureAlignment (FailDeny),
    Rule (AllowIfOlderThan, AllowScope, DenyIfCve),
 )

now :: UTCTime
now = UTCTime (fromGregorian 2026 9 12) 0

sixDays :: NominalDiffTime
sixDays = 6 * nominalDay

threeDays :: NominalDiffTime
threeDays = 3 * nominalDay

denyCve :: Rule
denyCve = DenyIfCve DenyIfCveParams{dicMinCvss = 7, dicOnUnavailable = FailDeny}

spec :: Spec
spec = do
    derivationSpec
    readingSpec
    alarmSpec

derivationSpec :: Spec
derivationSpec = describe "maxAdvisoryAgeFor" $ do
    it "gives six days for a seven-day quarantine, naming the rule it derived from" $ do
        let limit = maxAdvisoryAgeFor Nothing [AllowIfOlderThan (7 * nominalDay), denyCve]
        maxAdvisoryAge limit `shouldBe` sixDays
        maxAdvisoryAgeBasis limit `shouldBe` AgeBeforeQuarantine (7 * nominalDay)

    it "gives three days when no age rule is active on the mount" $ do
        let limit = maxAdvisoryAgeFor Nothing [denyCve, AllowScope (mkScope "acme")]
        maxAdvisoryAge limit `shouldBe` threeDays
        maxAdvisoryAgeBasis limit `shouldBe` AgeFloor

    it "derives from the earliest admission when two quarantines are active" $ do
        let limit = maxAdvisoryAgeFor Nothing [AllowIfOlderThan (14 * nominalDay), AllowIfOlderThan (7 * nominalDay)]
        maxAdvisoryAge limit `shouldBe` sixDays
        maxAdvisoryAgeBasis limit `shouldBe` AgeBeforeQuarantine (7 * nominalDay)

    it "holds the three-day floor under a two-day quarantine" $ do
        let limit = maxAdvisoryAgeFor Nothing [AllowIfOlderThan (2 * nominalDay)]
        maxAdvisoryAge limit `shouldBe` threeDays
        maxAdvisoryAgeBasis limit `shouldBe` AgeFloor

    it "takes an explicit value above the derived one" $ do
        let limit = maxAdvisoryAgeFor (Just (30 * nominalDay)) [AllowIfOlderThan (7 * nominalDay)]
        maxAdvisoryAge limit `shouldBe` 30 * nominalDay
        maxAdvisoryAgeBasis limit `shouldBe` AgeConfigured

    it "takes an explicit value below the floor" $ do
        let limit = maxAdvisoryAgeFor (Just 3600) [AllowIfOlderThan (7 * nominalDay)]
        maxAdvisoryAge limit `shouldBe` 3600
        maxAdvisoryAgeBasis limit `shouldBe` AgeConfigured

readingSpec :: Spec
readingSpec = describe "assessAdvisoryAge" $ do
    it "reads an age equal to the maximum as still eligible" $
        assessAdvisoryAge sixDayLimit now (PublishedAt (addUTCTime (negate sixDays) now))
            `shouldBe` AdvisoryAging AdvisoryAge{advisoryPushedAt = addUTCTime (negate sixDays) now, advisoryAge = sixDays, advisoryMaxAge = sixDays}

    it "reads one second past the maximum as expired" $
        case assessAdvisoryAge sixDayLimit now (PublishedAt (addUTCTime (negate sixDays - 1) now)) of
            AdvisoryStale observed -> advisoryMaxAge observed `shouldBe` sixDays
            other -> expectationFailure ("expected an expired reading, got " <> show other)

    it "reads a push inside half the maximum as fresh" $
        assessAdvisoryAge sixDayLimit now (PublishedAt (addUTCTime (negate (2 * nominalDay)) now))
            `shouldBe` AdvisoryFresh

    it "reads a push past half the maximum as aging, which is still eligible" $
        case assessAdvisoryAge sixDayLimit now (PublishedAt (addUTCTime (negate (4 * nominalDay)) now)) of
            AdvisoryAging observed -> advisoryAge observed `shouldBe` 4 * nominalDay
            other -> expectationFailure ("expected an aging reading, got " <> show other)

    it "reads nothing serving as fresh, leaving the absent-database path to decide" $
        assessAdvisoryAge sixDayLimit now NoGeneration `shouldBe` AdvisoryFresh

    it "reads a serving generation the store gave no publication time for as ineligible" $
        assessAdvisoryAge sixDayLimit now UndatedGeneration `shouldBe` AdvisoryUndated

alarmSpec :: Spec
alarmSpec = describe "ageAlarmStep" $ do
    it "reports the first crossing and latches" $
        case ageAlarmStep False (aged 4) of
            (latched, Just observed) -> do
                latched `shouldBe` True
                advisoryAge observed `shouldBe` 4 * nominalDay
            other -> expectationFailure ("expected a reported crossing, got " <> show other)

    it "stays silent while latched, including once the push expires" $ do
        snd (ageAlarmStep True (aged 4)) `shouldBe` Nothing
        snd (ageAlarmStep True (aged 9)) `shouldBe` Nothing

    it "re-arms on a push back inside half the maximum" $ do
        ageAlarmStep True (aged 1) `shouldBe` (False, Nothing)
        fst (ageAlarmStep False (aged 4)) `shouldBe` True

    it "reports nothing for an undated generation, which has no age and raises its own alarm" $
        ageAlarmStep False AdvisoryUndated `shouldBe` (False, Nothing)
  where
    aged days = assessAdvisoryAge sixDayLimit now (PublishedAt (addUTCTime (negate (days * nominalDay)) now))

sixDayLimit :: MaxAdvisoryAge
sixDayLimit = maxAdvisoryAgeFor Nothing [AllowIfOlderThan (7 * nominalDay)]
