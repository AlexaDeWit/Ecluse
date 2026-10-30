-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Generated native worker fixtures retain four actual archives and refuse premature or wrong uploads.
module Ecluse.BenchLoad.RubyGems.WorkerFixtureSpec (spec) where

import Data.Map.Strict qualified as Map
import Network.HTTP.Types (hContentType, status200, status400, status503)
import Network.Wai (defaultRequest, pathInfo, requestHeaders, requestMethod)
import Network.Wai.Test (SRequest (SRequest), SResponse (simpleStatus), runSession, srequest)
import Test.Hspec

import Ecluse.BenchLoad.RubyGems.WorkerFixture
import Ecluse.BenchLoad.WorkerState (WorkerTarget (..))

spec :: Spec
spec = beforeAll (newWorkerFixture 1024) $ beforeWith reset $ describe "native worker fixture" $ do
    it "keeps every independently generated platform archive and original compact specification" $ \fixture -> do
        Map.size (wfFiles fixture) `shouldBe` 4
        Map.size (wfSpecs fixture) `shouldBe` 4
        Map.size (wfRows fixture) `shouldBe` 4
        Map.member fixtureTrigger (wfFiles fixture) `shouldBe` True
    it "refuses bytes that do not match any independently built native archive" $ \fixture -> do
        response <- publish fixture "unrelated bytes"
        simpleStatus response `shouldBe` status400
        readIORef (wtFiles (wfState fixture)) `shouldReturn` Map.empty
    it "refuses a known native archive before durable inventory exists" $ \fixture -> do
        bytes <- trigger fixture
        response <- publish fixture bytes
        simpleStatus response `shouldBe` status400
        readIORef (wtFiles (wfState fixture)) `shouldReturn` Map.empty
    it "retains rejection controls after the upload reaches the native target" $ \fixture -> do
        writeIORef (wtObjects (wfState fixture)) (Map.singleton "inventory" ("revision", "state"))
        writeIORef (wtReject (wfState fixture)) True
        bytes <- trigger fixture
        response <- publish fixture bytes
        simpleStatus response `shouldBe` status503
        readIORef (wtFiles (wfState fixture)) `shouldReturn` Map.empty
        Map.lookup "upload-attempt" <$> readIORef (wtCounts (wfState fixture)) `shouldReturn` Just 1
    it "stores exact native bytes and resets target and durable state together" $ \fixture -> do
        writeIORef (wtObjects (wfState fixture)) (Map.singleton "inventory" ("revision", "state"))
        bytes <- trigger fixture
        response <- publish fixture bytes
        simpleStatus response `shouldBe` status200
        readIORef (wtFiles (wfState fixture)) `shouldReturn` Map.singleton fixtureTrigger bytes
        resetWorkerFixture fixture
        readIORef (wtFiles (wfState fixture)) `shouldReturn` Map.empty
        readIORef (wtObjects (wfState fixture)) `shouldReturn` Map.empty
  where
    reset fixture = resetWorkerFixture fixture >> writeIORef (wtReject (wfState fixture)) False >> pure fixture
    trigger fixture = maybe (fail "the native trigger is absent") pure (Map.lookup fixtureTrigger (wfFiles fixture))
    publish fixture bytes = runSession (srequest (SRequest defaultRequest{requestMethod = "POST", pathInfo = ["api", "v1", "gems"], requestHeaders = [(hContentType, "application/octet-stream")]} (fromStrict bytes))) (targetApp 0 fixture)
