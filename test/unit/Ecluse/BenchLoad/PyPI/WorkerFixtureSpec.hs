-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The PyPI worker fixture keeps four native distributions and refuses premature or wrong uploads.
module Ecluse.BenchLoad.PyPI.WorkerFixtureSpec (spec) where

import Data.Map.Strict qualified as Map
import Network.HTTP.Types (hContentType, status200, status400, status503)
import Network.Wai (defaultRequest, pathInfo, requestHeaders, requestMethod)
import Network.Wai.Test (SRequest (SRequest), SResponse (simpleStatus), runSession, srequest)
import Test.Hspec

import Ecluse.BenchLoad.PyPI.WorkerFixture
import Ecluse.BenchLoad.WorkerState (WorkerTarget (..))
import Ecluse.Test.Package (hexSha256Of)

spec :: Spec
spec = beforeAll (newWorkerFixture 1024) $ beforeWith reset $ describe "native worker fixture" $ do
    it "keeps two wheels and two source distributions, the upload trigger among them" $ \fixture ->
        Map.keys (wfFiles fixture) `shouldMatchList` [fixtureTrigger, "example-1-py2-none-any.whl", "example-1.zip", "example-1.tar.gz"]
    it "refuses bytes that do not match the built distribution" $ \fixture -> do
        storeInventory fixture
        response <- publish fixture (wheelUpload "unrelated bytes")
        simpleStatus response `shouldBe` status400
        readIORef (wtFiles (wfState fixture)) `shouldReturn` Map.empty
        Map.lookup "upload-attempt" <$> readIORef (wtCounts (wfState fixture)) `shouldReturn` Nothing
    it "refuses a known distribution before durable inventory exists" $ \fixture -> do
        bytes <- trigger fixture
        response <- publish fixture (wheelUpload bytes)
        simpleStatus response `shouldBe` status400
        readIORef (wtFiles (wfState fixture)) `shouldReturn` Map.empty
    it "retains rejection controls after the upload reaches the native target" $ \fixture -> do
        storeInventory fixture
        writeIORef (wtReject (wfState fixture)) True
        bytes <- trigger fixture
        response <- publish fixture (wheelUpload bytes)
        simpleStatus response `shouldBe` status503
        readIORef (wtFiles (wfState fixture)) `shouldReturn` Map.empty
        Map.lookup "upload-attempt" <$> readIORef (wtCounts (wfState fixture)) `shouldReturn` Just 1
    it "stores exact native bytes and resets target and durable state together" $ \fixture -> do
        storeInventory fixture
        bytes <- trigger fixture
        response <- publish fixture (wheelUpload bytes)
        simpleStatus response `shouldBe` status200
        readIORef (wtFiles (wfState fixture)) `shouldReturn` Map.singleton fixtureTrigger bytes
        resetWorkerFixture fixture
        readIORef (wtFiles (wfState fixture)) `shouldReturn` Map.empty
        readIORef (wtObjects (wfState fixture)) `shouldReturn` Map.empty
  where
    reset fixture = resetWorkerFixture fixture >> writeIORef (wtReject (wfState fixture)) False >> pure fixture
    storeInventory fixture = writeIORef (wtObjects (wfState fixture)) (Map.singleton "inventory" ("revision", "state"))
    trigger fixture = maybe (fail "the wheel trigger is absent") pure (Map.lookup fixtureTrigger (wfFiles fixture))
    publish fixture body =
        runSession
            (srequest (SRequest defaultRequest{requestMethod = "POST", pathInfo = ["upload"], requestHeaders = [(hContentType, "multipart/form-data; boundary=" <> boundary)]} (fromStrict body)))
            (targetApp 0 fixture)

-- | A twine-shaped upload of the trigger wheel, with metadata that describes the given bytes.
wheelUpload :: ByteString -> ByteString
wheelUpload bytes = foldMap field fields <> content <> "--" <> boundary <> "--\r\n"
  where
    fields =
        [ (":action", "file_upload")
        , ("protocol_version", "1")
        , ("name", "example")
        , ("version", "1")
        , ("metadata_version", "2.4")
        , ("description", "Description.\n")
        , ("sha256_digest", encodeUtf8 (hexSha256Of bytes))
        , ("filetype", "bdist_wheel")
        , ("pyversion", "py3")
        ]
    field (name, value) = "--" <> boundary <> "\r\nContent-Disposition: form-data; name=\"" <> name <> "\"\r\n\r\n" <> value <> "\r\n"
    content =
        "--"
            <> boundary
            <> "\r\nContent-Disposition: form-data; name=\"content\"; filename=\""
            <> encodeUtf8 fixtureTrigger
            <> "\"\r\nContent-Type: application/octet-stream\r\n\r\n"
            <> bytes
            <> "\r\n"

boundary :: ByteString
boundary = "ecluse-pypi-worker-fixture-boundary"
