-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The owned Cargo worker fixture keeps its crate archive and refuses premature or wrong uploads.
module Ecluse.BenchLoad.Cargo.WorkerFixtureSpec (spec) where

import Data.Map.Strict qualified as Map
import Network.HTTP.Types (status200, status400, status503)
import Network.Wai (defaultRequest, pathInfo, requestMethod)
import Network.Wai.Test (SRequest (SRequest), SResponse (simpleStatus), runSession, srequest)
import Test.Hspec

import Ecluse.BenchLoad.Cargo.WorkerFixture
import Ecluse.BenchLoad.WorkerState (WorkerTarget (..))
import Ecluse.Test.Registry.Cargo.Fixtures (CargoRelease (releaseArchive), cargoUpload, cargoUploadMetadata, cargoUploadWithMetadata)
import Ecluse.Test.Support (expectRightText)

spec :: Spec
spec = beforeAll (newWorkerFixture 1024) $ beforeWith reset $ describe "native worker fixture" $ do
    it "keeps the owned crate archive under the version trigger" $ \fixture ->
        wfFiles fixture `shouldBe` Map.singleton fixtureTrigger (releaseArchive (wfRelease fixture))
    it "refuses a native frame whose archive differs from the owned crate" $ \fixture -> do
        frame <- expectRightText (cargoUploadWithMetadata (cargoUploadMetadata (wfRelease fixture)) "unrelated bytes")
        storeInventory fixture
        response <- publish fixture frame
        simpleStatus response `shouldBe` status400
        readIORef (wtFiles (wfState fixture)) `shouldReturn` Map.empty
        Map.lookup "upload-attempt" <$> readIORef (wtCounts (wfState fixture)) `shouldReturn` Nothing
    it "refuses the owned crate before durable inventory exists" $ \fixture -> do
        frame <- expectRightText (cargoUpload (wfRelease fixture))
        response <- publish fixture frame
        simpleStatus response `shouldBe` status400
        readIORef (wtFiles (wfState fixture)) `shouldReturn` Map.empty
    it "retains rejection controls after the upload reaches the native target" $ \fixture -> do
        storeInventory fixture
        writeIORef (wtReject (wfState fixture)) True
        frame <- expectRightText (cargoUpload (wfRelease fixture))
        response <- publish fixture frame
        simpleStatus response `shouldBe` status503
        readIORef (wtFiles (wfState fixture)) `shouldReturn` Map.empty
        Map.lookup "upload-attempt" <$> readIORef (wtCounts (wfState fixture)) `shouldReturn` Just 1
    it "stores exact native bytes and resets target and durable state together" $ \fixture -> do
        storeInventory fixture
        frame <- expectRightText (cargoUpload (wfRelease fixture))
        response <- publish fixture frame
        simpleStatus response `shouldBe` status200
        readIORef (wtFiles (wfState fixture)) `shouldReturn` Map.singleton fixtureTrigger (releaseArchive (wfRelease fixture))
        resetWorkerFixture fixture
        readIORef (wtFiles (wfState fixture)) `shouldReturn` Map.empty
        readIORef (wtObjects (wfState fixture)) `shouldReturn` Map.empty
  where
    reset fixture = resetWorkerFixture fixture >> writeIORef (wtReject (wfState fixture)) False >> pure fixture
    storeInventory fixture = writeIORef (wtObjects (wfState fixture)) (Map.singleton "inventory" ("revision", "state"))
    publish fixture bytes = runSession (srequest (SRequest defaultRequest{requestMethod = "PUT", pathInfo = ["api", "v1", "crates", "new"]} (fromStrict bytes))) (targetApp 0 fixture)
