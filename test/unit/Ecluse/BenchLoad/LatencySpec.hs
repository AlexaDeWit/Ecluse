-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pin the success-only percentiles every latency figure in the report uses.
module Ecluse.BenchLoad.LatencySpec (spec) where

import Data.Aeson (eitherDecodeStrict)
import Test.Hspec

import Ecluse.BenchLoad.Latency (Percentiles (..), isSuccessStatus, noPercentiles, percentiles)

spec :: Spec
spec = do
    describe "percentiles" $ do
        it "takes the nearest rank of seconds and reports milliseconds, in any order" $ do
            let p = percentiles [0.004, 0.001, 0.003, 0.002]
            pP50Ms p `shouldBe` Just 2
            pP90Ms p `shouldBe` Just 4
            pP95Ms p `shouldBe` Just 4
            pP99Ms p `shouldBe` Just 4
            pP999Ms p `shouldBe` Just 4
        it "distinguishes p95 without moving the existing quantiles" $ do
            let observed = percentiles [fromIntegral i / 1000 | i <- [1 .. 100 :: Int]]
            (pP50Ms observed, pP90Ms observed, pP95Ms observed, pP99Ms observed, pP999Ms observed) `shouldBe` (Just 50, Just 90, Just 95, Just 99, Just 100)
        it "keeps p95 unavailable in an older percentile record" $
            eitherDecodeStrict "{\"pP50Ms\":1,\"pP90Ms\":2,\"pP99Ms\":3,\"pP999Ms\":4}" `shouldBe` Right (Percentiles (Just 1) (Just 2) Nothing (Just 3) (Just 4))
        it "reports nothing when nothing succeeded" $
            percentiles [] `shouldBe` noPercentiles
    describe "isSuccessStatus" $
        it "counts 2xx and 3xx, a 304 revalidation included" $ do
            map isSuccessStatus [200, 204, 304, 399] `shouldBe` [True, True, True, True]
            map isSuccessStatus [199, 404, 429, 503] `shouldBe` [False, False, False, False]
