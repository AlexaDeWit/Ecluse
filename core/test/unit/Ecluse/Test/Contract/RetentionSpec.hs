-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Broken observations must fail the retention contract without running a heap probe.
module Ecluse.Test.Contract.RetentionSpec (spec) where

import Test.Hspec

import Ecluse.Test.Contract.Retention (boundedRetention)

-- | Pin sample order and the strict threshold used by both native readers.
spec :: Spec
spec = describe "boundedRetention" $ do
    it "samples the original repetition counts in order" $ do
        samples <- newIORef []
        boundedRetention 100 (\count -> modifyIORef' samples (<> [count]) >> pure 20)
        readIORef samples `shouldReturn` [2000, 16000]
    it "rejects growth equal to the allowance" $
        boundedRetention 100 (\count -> pure (if count == 2000 then 20 else 120)) `shouldThrow` anyException
    it "rejects growth above the allowance" $
        boundedRetention 100 (\count -> pure (if count == 2000 then 20 else 121)) `shouldThrow` anyException
    it "accepts growth below the allowance" $
        boundedRetention 100 (\count -> pure (if count == 2000 then 20 else 119))
