-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Oha first-byte observations keep their actual population and unavailable fields.
module Ecluse.BenchLoad.OhaSpec (spec) where

import Data.Aeson (eitherDecodeStrict)
import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.BenchLoad.Latency (Percentiles (..))
import Ecluse.BenchLoad.Oha (OhaReport (..))
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "Oha first body-byte observations" $ do
    it "retains all-response percentiles without inventing success-only timings" $ do
        report <- expectRight (decode ",\"firstBytePercentiles\":{\"p50\":0.001,\"p90\":0.002,\"p95\":0.003,\"p99\":0.004,\"p99.9\":0.005}")
        ohaFirstBytePercentiles report `shouldBe` Just (Percentiles (Just 1) (Just 2) (Just 3) (Just 4) (Just 5))
        Map.lookup "503" (ohaStatusCounts report) `shouldBe` Just 4
        ohaSuccessLatencies report `shouldBe` []
    it "keeps old, absent and null observations unavailable rather than zero" $
        for_ ["", ",\"firstBytePercentiles\":{}", ",\"firstBytePercentiles\":{\"p50\":null,\"p95\":null,\"p99\":null}"] $ \fields -> do
            report <- expectRight (decode fields)
            ohaFirstBytePercentiles report `shouldBe` Nothing
    it "preserves partial observations without filling missing quantiles" $ do
        report <- expectRight (decode ",\"firstBytePercentiles\":{\"p95\":0.003}")
        ohaFirstBytePercentiles report `shouldBe` Just (Percentiles Nothing Nothing (Just 3) Nothing Nothing)
    it "refuses an invalid observed percentile" $
        decode ",\"firstBytePercentiles\":{\"p95\":-0.001}" `shouldSatisfy` isLeft
    it "retains actual transferred bytes independently of response counts" $ do
        report <- expectRight (bodyBytes "123456")
        ohaTransferredBodyBytes report `shouldBe` Just 123456
        Map.lookup "200" (ohaStatusCounts report) `shouldBe` Just 1
    it "distinguishes observed zero bytes from unavailable bytes" $ do
        zero <- expectRight (bodyBytes "0")
        missing <- expectRight (decode "")
        ohaTransferredBodyBytes zero `shouldBe` Just 0
        ohaTransferredBodyBytes missing `shouldBe` Nothing
    it "rejects negative transferred bytes" $
        bodyBytes "-1" `shouldSatisfy` isLeft
  where
    decode fields = eitherDecodeStrict ("{\"summary\":{\"total\":1},\"statusCodeDistribution\":{\"200\":1,\"503\":4}" <> fields <> "}") :: Either String OhaReport
    bodyBytes bytes = eitherDecodeStrict ("{\"summary\":{\"total\":1,\"totalData\":" <> bytes <> "},\"statusCodeDistribution\":{\"200\":1}}") :: Either String OhaReport
