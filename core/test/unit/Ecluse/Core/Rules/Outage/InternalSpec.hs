-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The outage fold itself: a transition reports once, a continuing outage reports once per
period, and the record of logged admissions is bounded and emptied on recovery.
-}
module Ecluse.Core.Rules.Outage.InternalSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Time (UTCTime, addUTCTime)
import Test.Hspec

import Ecluse.Core.Rules.Outage.Internal
import Ecluse.Rules.Outage.Support (down, ident, isHealthy, period, shape, up)
import Ecluse.Rules.Support (now)

-- | 'stepOutage' under the test period, at 'now' plus the given seconds.
stepAt :: Integer -> SourceHealth -> OutageState -> (OutageState, Maybe OutageReport)
stepAt secs health current = let stepped = stepOutage period (at secs) health current in (osState stepped, osReport stepped)

-- | 'now' plus the given seconds.
at :: Integer -> UTCTime
at secs = addUTCTime (fromInteger secs) now

-- | Fold a timed sequence of readings, collecting every report in order.
run :: [(Integer, SourceHealth)] -> (OutageState, [OutageReport])
run = foldl' step (Healthy, [])
  where
    step (current, reports) (secs, health) =
        let (next, report) = stepAt secs health current
         in (next, reports <> maybeToList report)

spec :: Spec
spec = do
    describe "stepOutage" $ do
        it "stays silent while the source answers" $
            first shape (run [(0, up "DenyIfCve"), (1, up "DenyIfCve"), (2, up "DenyIfEpss")]) `shouldBe` (Nothing, [])

        it "reports the first unavailable evaluation once, not the repeats within the period" $
            snd (run [(0, down "DenyIfCve"), (1, down "DenyIfCve"), (period - 1, down "DenyIfCve")])
                `shouldBe` [OutageBegan "DenyIfCve" "no advisory database loaded"]

        it "reminds once per period while the outage continues, with every rule still unable" $ do
            let (_, reports) = run [(0, down "DenyIfCve"), (5, down "DenyIfEpss"), (period, down "DenyIfCve"), (period + 1, down "DenyIfEpss"), (2 * period, down "DenyIfEpss")]
            reports
                `shouldBe` [ OutageBegan "DenyIfCve" "no advisory database loaded"
                           , OutageContinues now (Map.fromList [("DenyIfCve", "no advisory database loaded"), ("DenyIfEpss", "no advisory database loaded")])
                           , OutageContinues now (Map.fromList [("DenyIfCve", "no advisory database loaded"), ("DenyIfEpss", "no advisory database loaded")])
                           ]

        it "carries each rule's latest cause into the reminder" $ do
            let (_, reports) = run [(0, down "DenyIfCve"), (period, SourceUnavailable "DenyIfCve" "the rule source circuit breaker is open")]
            reports
                `shouldBe` [ OutageBegan "DenyIfCve" "no advisory database loaded"
                           , OutageContinues now (Map.singleton "DenyIfCve" "the rule source circuit breaker is open")
                           ]

        it "recovers only once every rule consults the source again, naming when the outage began" $ do
            let (final, reports) = run [(0, down "DenyIfCve"), (1, down "DenyIfEpss"), (2, up "DenyIfCve"), (3, up "DenyIfEpss")]
            final `shouldSatisfy` isHealthy
            reports `shouldBe` [OutageBegan "DenyIfCve" "no advisory database loaded", OutageRecovered now]

        it "keeps the outage open on a sibling's answer while one rule's breaker still fast-fails" $ do
            -- Two rules share a source but not a breaker, so one can probe successfully while the
            -- other is still cooling. The outage is over only when both answer.
            let (final, reports) = run [(0, down "DenyIfCve"), (1, down "DenyIfEpss"), (2, up "DenyIfEpss"), (3, down "DenyIfCve")]
            reports `shouldBe` [OutageBegan "DenyIfCve" "no advisory database loaded"]
            shape final `shouldBe` Just (now, now, Map.singleton "DenyIfCve" "no advisory database loaded", noLoggedAdmissions)

        it "flags a reading that changes nothing, and every one that does" $ do
            let began = osState (stepOutage period now (down "DenyIfCve") Healthy)
                changedBy secs health = osChanged (stepOutage period (at secs) health began)
            osChanged (stepOutage period now (up "DenyIfCve") Healthy) `shouldBe` False
            changedBy 1 (down "DenyIfCve") `shouldBe` False -- the same rule, the same cause, inside the period
            changedBy 1 (up "DenyIfEpss") `shouldBe` False -- a rule that was never unable
            changedBy 1 (SourceUnavailable "DenyIfCve" "the rule source circuit breaker is open") `shouldBe` True
            changedBy 1 (down "DenyIfEpss") `shouldBe` True
            changedBy period (down "DenyIfCve") `shouldBe` True
            changedBy 1 (up "DenyIfCve") `shouldBe` True

        it "reports a later outage as a fresh beginning" $ do
            let (_, reports) = run [(0, down "DenyIfCve"), (1, up "DenyIfCve"), (2, down "DenyIfCve")]
            reports
                `shouldBe` [ OutageBegan "DenyIfCve" "no advisory database loaded"
                           , OutageRecovered now
                           , OutageBegan "DenyIfCve" "no advisory database loaded"
                           ]

    describe "noteLogged" $ do
        it "logs a new identity once and skips its repeats" $ do
            let (once, logged) = noteLogged 8 (ident "a" "1.0.0" ["DenyIfCve"]) noLoggedAdmissions
                (again, repeated) = noteLogged 8 (ident "a" "1.0.0" ["DenyIfCve"]) once
            (logged, repeated) `shouldBe` (True, False)
            again `shouldBe` once

        it "treats another version, or another skipped rule set, as its own line" $ do
            let (logged, _) = noteLogged 8 (ident "a" "1.0.0" ["DenyIfCve"]) noLoggedAdmissions
            snd (noteLogged 8 (ident "a" "2.0.0" ["DenyIfCve"]) logged) `shouldBe` True
            snd (noteLogged 8 (ident "a" "1.0.0" ["DenyIfCve", "DenyIfEpss"]) logged) `shouldBe` True

        it "evicts the oldest identity first once the record holds the cap" $ do
            let filled = foldl' (\acc v -> fst (noteLogged 3 (ident "a" v ["DenyIfCve"]) acc)) noLoggedAdmissions ["1", "2", "3"]
                (evicted, logged) = noteLogged 3 (ident "a" "4" ["DenyIfCve"]) filled
            logged `shouldBe` True
            snd (noteLogged 3 (ident "a" "1" ["DenyIfCve"]) evicted) `shouldBe` True
            snd (noteLogged 3 (ident "a" "2" ["DenyIfCve"]) evicted) `shouldBe` False

    describe "admissionLogged" $ do
        it "logs every admission on a healthy source and records nothing" $
            first shape (admissionLogged 8 (ident "a" "1.0.0" ["DenyIfCve"]) Healthy) `shouldBe` (Nothing, True)

        it "empties the record on recovery, so the next outage logs the identity again" $ do
            let (during, _) = stepAt 0 (down "DenyIfCve") Healthy
                (noted, logged) = admissionLogged 8 (ident "a" "1.0.0" ["DenyIfCve"]) during
                (recovered, _) = stepAt 1 (up "DenyIfCve") noted
                (relapsed, _) = stepAt 2 (down "DenyIfCve") recovered
            logged `shouldBe` True
            snd (admissionLogged 8 (ident "a" "1.0.0" ["DenyIfCve"]) noted) `shouldBe` False
            recovered `shouldSatisfy` isHealthy
            snd (admissionLogged 8 (ident "a" "1.0.0" ["DenyIfCve"]) relapsed) `shouldBe` True
