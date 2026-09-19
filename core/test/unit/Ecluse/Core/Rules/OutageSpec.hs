-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The bounded outage reporter: a transition reports once, a continuing outage reports once per
period, and a request never reports on its own.
-}
module Ecluse.Core.Rules.OutageSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Test.Hspec

import Ecluse.Core.Rules.Outage
import Ecluse.Core.Rules.Outage.Internal
import Ecluse.Core.Rules.Types (Reason)

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 6 20) 0

-- | The reminder gap under test, in seconds.
period :: (Num a) => a
period = 900

-- | 'stepOutage' under the test period, at @t0@ plus the given seconds.
stepAt :: Integer -> SourceHealth -> OutageState -> (OutageState, Maybe OutageReport)
stepAt secs health current = let stepped = stepOutage period (addUTCTime (fromInteger secs) t0) health current in (osState stepped, osReport stepped)

-- | Fold a timed sequence of readings, collecting every report in order.
run :: [(Integer, SourceHealth)] -> (OutageState, [OutageReport])
run = foldl' step (Healthy, [])
  where
    step (current, reports) (secs, health) =
        let (next, report) = stepAt secs health current
         in (next, reports <> maybeToList report)

{- | The state's observable shape: nothing for a healthy source, else when the outage began, when it
last reported, the rules still unable, and the identities logged in order.
-}
shape :: OutageState -> Maybe (UTCTime, UTCTime, Map Text Reason, LoggedAdmissions)
shape = \case
    Healthy -> Nothing
    Outage ongoing -> Just (ooSince ongoing, ooReportedAt ongoing, ooRules ongoing, ooLogged ongoing)

isHealthy :: OutageState -> Bool
isHealthy = isNothing . shape

down :: Text -> SourceHealth
down rule = SourceUnavailable rule "no advisory database loaded"

up :: Text -> SourceHealth
up = SourceAnswered

ident :: Text -> Text -> [Text] -> AdmissionIdentity
ident package version rules = AdmissionIdentity package version (Set.fromList rules)

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
                           , OutageContinues t0 (Map.fromList [("DenyIfCve", "no advisory database loaded"), ("DenyIfEpss", "no advisory database loaded")])
                           , OutageContinues t0 (Map.fromList [("DenyIfCve", "no advisory database loaded"), ("DenyIfEpss", "no advisory database loaded")])
                           ]

        it "carries each rule's latest cause into the reminder" $ do
            let (_, reports) = run [(0, down "DenyIfCve"), (period, SourceUnavailable "DenyIfCve" "the rule source circuit breaker is open")]
            reports
                `shouldBe` [ OutageBegan "DenyIfCve" "no advisory database loaded"
                           , OutageContinues t0 (Map.singleton "DenyIfCve" "the rule source circuit breaker is open")
                           ]

        it "recovers only once every rule consults the source again, naming when the outage began" $ do
            let (final, reports) = run [(0, down "DenyIfCve"), (1, down "DenyIfEpss"), (2, up "DenyIfCve"), (3, up "DenyIfEpss")]
            final `shouldSatisfy` isHealthy
            reports `shouldBe` [OutageBegan "DenyIfCve" "no advisory database loaded", OutageRecovered t0]

        it "keeps the outage open on a sibling's answer while one rule's breaker still fast-fails" $ do
            -- Two rules share a source but not a breaker, so one can probe successfully while the
            -- other is still cooling. The outage is over only when both answer.
            let (final, reports) = run [(0, down "DenyIfCve"), (1, down "DenyIfEpss"), (2, up "DenyIfEpss"), (3, down "DenyIfCve")]
            reports `shouldBe` [OutageBegan "DenyIfCve" "no advisory database loaded"]
            shape final `shouldBe` Just (t0, t0, Map.singleton "DenyIfCve" "no advisory database loaded", noLoggedAdmissions)

        it "flags a reading that changes nothing, and every one that does" $ do
            let began = osState (stepOutage period t0 (down "DenyIfCve") Healthy)
                changedBy secs health = osChanged (stepOutage period (addUTCTime secs t0) health began)
            osChanged (stepOutage period t0 (up "DenyIfCve") Healthy) `shouldBe` False
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
                           , OutageRecovered t0
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

    describe "sourceReporter" $ do
        it "emits each report through the shared state, and nothing per healthy evaluation" $ do
            (store, _, shared) <- countingStore
            clock <- newIORef t0
            emitted <- newIORef []
            let reporter = sourceReporter period (readIORef clock) store (\r -> modifyIORef' emitted (r :))
            replicateM_ 3 (reportSource reporter (up "DenyIfCve"))
            reportSource reporter (down "DenyIfCve")
            replicateM_ 50 (reportSource reporter (down "DenyIfCve"))
            writeIORef clock (addUTCTime period t0)
            reportSource reporter (down "DenyIfCve")
            reportSource reporter (up "DenyIfCve")
            reverse <$> readIORef emitted
                `shouldReturn` [ OutageBegan "DenyIfCve" "no advisory database loaded"
                               , OutageContinues t0 (Map.singleton "DenyIfCve" "no advisory database loaded")
                               , OutageRecovered t0
                               ]
            readTVarIO shared >>= (`shouldSatisfy` isHealthy)

        it "commits only on a transition or a due reminder, never per evaluation inside the period" $ do
            -- The serve leg keeps evaluating through an outage, so the common path must stay a
            -- read and a pure check rather than a transaction on the shared state.
            (store, commits, _) <- countingStore
            clock <- newIORef t0
            let reporter = sourceReporter period (readIORef clock) store (const pass)
            replicateM_ 10 (reportSource reporter (up "DenyIfCve"))
            readIORef commits `shouldReturn` 0
            reportSource reporter (down "DenyIfCve")
            readIORef commits `shouldReturn` 1
            forM_ [1 .. 200 :: Integer] $ \secs -> do
                writeIORef clock (addUTCTime (fromInteger secs) t0)
                reportSource reporter (down "DenyIfCve")
            readIORef commits `shouldReturn` 1
            -- A second rule joining the outage changes the state, so it commits without a report.
            reportSource reporter (down "DenyIfEpss")
            readIORef commits `shouldReturn` 2
            writeIORef clock (addUTCTime period t0)
            reportSource reporter (down "DenyIfCve")
            readIORef commits `shouldReturn` 3
            reportSource reporter (up "DenyIfCve")
            reportSource reporter (up "DenyIfEpss")
            readIORef commits `shouldReturn` 5

        it "commits an admission identity once, and decides its repeats on the read alone" $ do
            (store, commits, _) <- countingStore
            let reporter = sourceReporter period (pure t0) store (const pass)
            noteAdmission reporter (ident "a" "1.0.0" ["DenyIfCve"]) `shouldReturn` True
            readIORef commits `shouldReturn` 0 -- healthy: logged, nothing to record
            reportSource reporter (down "DenyIfCve")
            noteAdmission reporter (ident "a" "1.0.0" ["DenyIfCve"]) `shouldReturn` True
            replicateM_ 100 (noteAdmission reporter (ident "a" "1.0.0" ["DenyIfCve"]) `shouldReturn` False)
            readIORef commits `shouldReturn` 2

        it "performs no commit for a repeat identity or an unchanged reading over a record at the cap" $ do
            -- The record holds thousands of identities here, so a decision that compared states
            -- would walk them all on the serve path. The count pins that nothing is written.
            (store, commits, _) <- countingStore
            let reporter = sourceReporter period (pure t0) store (const pass)
            reportSource reporter (down "DenyIfCve")
            forM_ [1 .. loggedAdmissionCap] $ \n ->
                noteAdmission reporter (ident "a" (show n) ["DenyIfCve"]) `shouldReturn` True
            readIORef commits `shouldReturn` loggedAdmissionCap + 1
            replicateM_ 50 (noteAdmission reporter (ident "a" (show loggedAdmissionCap) ["DenyIfCve"]) `shouldReturn` False)
            replicateM_ 50 (reportSource reporter (down "DenyIfCve"))
            readIORef commits `shouldReturn` loggedAdmissionCap + 1

-- | The live store wrapped so the spec can count how many folds it committed.
countingStore :: IO (OutageStore, IORef Int, TVar OutageState)
countingStore = do
    shared <- newTVarIO Healthy
    commits <- newIORef (0 :: Int)
    let live = tvarOutageStore shared
    pure (live{commitOutage = \advance -> modifyIORef' commits (+ 1) >> commitOutage live advance}, commits, shared)
