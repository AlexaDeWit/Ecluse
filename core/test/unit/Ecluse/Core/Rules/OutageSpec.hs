-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The bounded outage reporter: a transition reports once, a continuing outage reports once per
period, and a request never reports on its own.
-}
module Ecluse.Core.Rules.OutageSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Test.Hspec

import Ecluse.Core.Rules.Outage

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 6 20) 0

-- | The reminder gap under test, in seconds.
period :: (Num a) => a
period = 900

-- | 'stepOutage' under the test period, at @t0@ plus the given seconds.
stepAt :: Integer -> SourceHealth -> OutageState -> (OutageState, Maybe OutageReport)
stepAt secs = stepOutage period (addUTCTime (fromInteger secs) t0)

-- | Fold a timed sequence of readings, collecting every report in order.
run :: [(Integer, SourceHealth)] -> (OutageState, [OutageReport])
run = foldl' step (Healthy, [])
  where
    step (current, reports) (secs, health) =
        let (next, report) = stepAt secs health current
         in (next, reports <> maybeToList report)

down :: Text -> SourceHealth
down rule = SourceUnavailable rule "no advisory database loaded"

up :: Text -> SourceHealth
up = SourceAnswered

spec :: Spec
spec = do
    describe "stepOutage" $ do
        it "stays silent while the source answers" $
            run [(0, up "DenyIfCve"), (1, up "DenyIfCve"), (2, up "DenyIfEpss")] `shouldBe` (Healthy, [])

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
            final `shouldBe` Healthy
            reports `shouldBe` [OutageBegan "DenyIfCve" "no advisory database loaded", OutageRecovered t0]

        it "keeps the outage open on a sibling's answer while one rule's breaker still fast-fails" $ do
            -- Two rules share a source but not a breaker, so one can probe successfully while the
            -- other is still cooling. The outage is over only when both answer.
            let (final, reports) = run [(0, down "DenyIfCve"), (1, down "DenyIfEpss"), (2, up "DenyIfEpss"), (3, down "DenyIfCve")]
            reports `shouldBe` [OutageBegan "DenyIfCve" "no advisory database loaded"]
            final `shouldBe` Outage (OngoingOutage t0 t0 (Map.singleton "DenyIfCve" "no advisory database loaded"))

        it "reports a later outage as a fresh beginning" $ do
            let (_, reports) = run [(0, down "DenyIfCve"), (1, up "DenyIfCve"), (2, down "DenyIfCve")]
            reports
                `shouldBe` [ OutageBegan "DenyIfCve" "no advisory database loaded"
                           , OutageRecovered t0
                           , OutageBegan "DenyIfCve" "no advisory database loaded"
                           ]

    describe "sourceReporter" $ do
        it "emits each report through the shared state, and nothing per healthy evaluation" $ do
            shared <- newTVarIO Healthy
            clock <- newIORef t0
            emitted <- newIORef []
            let reporter = sourceReporter period (readIORef clock) shared (\r -> modifyIORef' emitted (r :))
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
            readTVarIO shared `shouldReturn` Healthy
