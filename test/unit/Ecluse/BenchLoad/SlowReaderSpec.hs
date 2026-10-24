-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | A slow reader reads at its rate, reports what it was answered, and counts reads the window cuts short.
module Ecluse.BenchLoad.SlowReaderSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Network.HTTP.Types (status200, status503)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec

import Ecluse.BenchLoad.Harness (Companion (..))
import Ecluse.BenchLoad.Oha (OhaReport (..))
import Ecluse.BenchLoad.SlowReader (SlowRead (..), slowRead, slowReaders)
import Ecluse.Test.Wai (localhost)

spec :: Spec
spec = describe "slow readers" $ do
    it "reads a body no faster than its rate" $
        testWithApplication (pure (answering status200 body)) $ \port -> do
            outcome <- slowRead (512 * 1024) (localhost port)
            outcome `shouldSatisfy` \case
                Answered 200 seconds -> seconds >= 0.4
                _ -> False
    it "reports the status a refusal answered with" $
        testWithApplication (pure (answering status503 "busy")) $ \port -> do
            outcome <- slowRead (512 * 1024) (localhost port)
            outcome `shouldSatisfy` \case
                Answered 503 _ -> True
                _ -> False
    it "counts a read the window cuts short as a deadline abort, not a failure" $
        testWithApplication (pure stalled) $ \port -> do
            report <- companionRun (slowReaders 1 [localhost port]) 1
            ohaStatusCounts report `shouldBe` Map.empty
            ohaErrorCounts report `shouldBe` Map.singleton "aborted due to deadline" 1
            ohaSuccessLatencies report `shouldBe` []
  where
    body = LBS.replicate (256 * 1024) 0x61
    answering status bytes _ respond = respond (responseLBS status [] bytes)
    stalled :: Application
    stalled _ respond = threadDelay 5_000_000 >> respond (responseLBS status200 [] "late")
