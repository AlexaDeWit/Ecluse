-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native upload bounds and identity claims use independently framed fixture bytes.
module Ecluse.Core.Registry.Cargo.Publish.BodySpec (spec) where

import Data.Aeson (Value (Object), encode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Builder (byteString, toLazyByteString, word32LE)
import Data.ByteString.Lazy qualified as LBS
import Test.Hspec

import Ecluse.Core.Registry.Cargo.Publish.Body
import Ecluse.Core.Security.Limits (Limits (..), defaultLimits)
import Ecluse.Test.Registry.Cargo.Fixtures (CargoRelease (releaseArchive), cargoUpload, cargoUploadMetadata)
import Ecluse.Test.Registry.Cargo.Publish (cargoPublicationFixture)
import Ecluse.Test.Registry.Cargo.Reference (UploadFrame (UploadFrame), referenceUpload)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "Cargo upload framing" $ do
    it "decodes independently framed native metadata and the exact compressed archive" $ do
        release <- cargoPublicationFixture
        bytes <- expectRight (cargoUpload release)
        decoded <- expectRight (decodeUpload defaultLimits bytes)
        uploadName decoded `shouldBe` "fixture"
        uploadVersion decoded `shouldBe` "1.0.0"
        uploadArchive decoded `shouldBe` releaseArchive release
        uploadMetadataValue decoded `shouldBe` cargoUploadMetadata release
        encoded <- LBS.toStrict <$> expectRight (encodeUpload (uploadMetadataValue decoded) (uploadArchive decoded))
        let expected = case cargoUploadMetadata release of
                Object fields -> fields
                _ -> mempty
        referenceUpload encoded `shouldBe` Right (UploadFrame expected (releaseArchive release))
    it "rejects every incomplete length prefix, truncated section and trailing payload" $ do
        release <- cargoPublicationFixture
        bytes <- expectRight (cargoUpload release)
        for_ [BS.empty, BS.take 1 bytes, BS.take 3 bytes, BS.take 5 bytes, BS.take (BS.length bytes - 1) bytes, bytes <> "trailing", BS.replicate 4 255 <> BS.drop 4 bytes] $ \broken ->
            decodeUpload defaultLimits broken `shouldSatisfy` isLeft
    it "checks request, metadata, archive and nesting bounds" $ do
        release <- cargoPublicationFixture
        bytes <- expectRight (cargoUpload release)
        for_ [defaultLimits{maxPublishRequestBytes = BS.length bytes - 1}, defaultLimits{maxMetadataBytes = 8}, defaultLimits{maxMirrorArtifactBytes = 8}, defaultLimits{maxNestingDepth = 1}] $ \limits ->
            decodeUpload limits bytes `shouldSatisfy` isLeft
    it "rejects duplicate identity keys and an incomplete JSON document" $ do
        release <- cargoPublicationFixture
        for_ ["{\"name\":\"fixture\",\"name\":\"other\"}", "{\"name\":\"fixture\""] $ \json ->
            decodeUpload defaultLimits (frame json <> frame (releaseArchive release)) `shouldSatisfy` isLeft
    it "requires all native identity and metadata fields" $ do
        release <- cargoPublicationFixture
        case cargoUploadMetadata release of
            Object fields -> for_ ["name", "vers", "deps", "features", "authors", "keywords", "categories"] $ \key -> do
                let json = LBS.toStrict (encode (Object (KeyMap.delete key fields)))
                decodeUpload defaultLimits (frame json <> frame (releaseArchive release)) `shouldSatisfy` isLeft
            _ -> expectationFailure "fixture metadata is not an object"
    it "charges decoded metadata before exposing the archive" $ do
        release <- cargoPublicationFixture
        bytes <- expectRight (cargoUpload release)
        paid <- newIORef ([] :: [Int])
        result <- readUpload defaultLimits (\cost -> modifyIORef' paid (cost :)) bytes
        result `shouldSatisfy` isRight
        payments <- readIORef paid
        payments `shouldSatisfy` (\costs -> length costs == 1 && all (> 65536) costs)

frame :: ByteString -> ByteString
frame bytes = LBS.toStrict (toLazyByteString (word32LE (fromIntegral (BS.length bytes)) <> byteString bytes))
