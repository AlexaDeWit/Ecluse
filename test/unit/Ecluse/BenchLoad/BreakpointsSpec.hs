-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.BenchLoad.BreakpointsSpec (spec) where

import Test.Hspec

import Ecluse.BenchLoad.Breakpoints

spec :: Spec
spec = describe "observed interval full-store model" $ do
    let budget = ModelBudget 100 10 100
        listing name start = TraceRead name start (start + 2) 60 Listing True
        run b rs = either (expectationFailure . toString) (const pass) (modelTrace b rs)
    it "counts a concurrent follower separately from a retained hit" $ do
        let result = modelTrace budget [listing "a" 0, listing "a" 1, listing "a" 3]
        fmap (\r -> (mrMisses r, mrCollapsed r, mrHits r)) result `shouldBe` Right (1, 1, 1)
    it "measures listing-to-artifact age and admitted bytes without touching recency" $ do
        let observations = [listing "a" 0, listing "b" 3, (listing "a" 6){trAccess = Artifact}, listing "c" 9, listing "a" 12]
            result = modelTrace budget{mbBytes = 120} observations
        fmap mrReuse result `shouldBe` Right [Reuse "a" Artifact 4 60]
        fmap mrCapacityEvictions result `shouldBe` Right 2
    it "separates TTL expiry, oversized refusal, and capacity eviction" $ do
        fmap mrExpired (modelTrace budget [listing "a" 0, listing "a" 103]) `shouldBe` Right 1
        fmap mrOversized (modelTrace budget{mbBytes = 59} [listing "a" 0]) `shouldBe` Right 1
        fmap mrCapacityEvictions (modelTrace budget [listing "a" 0, listing "b" 3]) `shouldBe` Right 1
    it "does not populate a full entry from an artifact or failed listing" $ do
        let observations = [(listing "a" 0){trAccess = Artifact}, (listing "a" 3){trSuccess = False}, listing "a" 6]
        fmap mrHits (modelTrace budget observations) `shouldBe` Right 0
        fmap mrAdmittedBytes (modelTrace budget observations) `shouldBe` Right 60
    it "keeps a later artifact as an upper-bound opportunity after an earlier artifact miss" $ do
        let observations = [(listing "a" 0){trAccess = Artifact}, listing "a" 3, (listing "a" 6){trAccess = Artifact}]
        fmap mrReuse (modelTrace budget observations) `shouldBe` Right [Reuse "a" Artifact 1 0]
    it "enforces the entry-count bound independently of bytes" $
        fmap (\r -> (mrBytePressureEvictions r, mrCountPressureEvictions r)) (modelTrace budget{mbBytes = 1000, mbEntries = 1} [listing "a" 0, listing "b" 3]) `shouldBe` Right (0, 1)
    it "rejects reversed intervals instead of silently reordering completion" $
        modelTrace budget [(listing "a" 3){trEnd = 2}] `shouldSatisfy` isLeft
    it "accepts zero retention with the other bounds explicit" $
        run budget{mbBytes = 0} [listing "a" 0]
