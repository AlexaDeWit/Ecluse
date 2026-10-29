-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native upload bounds and identity claims use independently framed fixture bytes.
module Ecluse.Core.Registry.Cargo.Publish.BodySpec (spec) where

import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.Cargo.Publish.Body
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Security.Limits (Limits (..), defaultLimits)
import Ecluse.Test.Registry.Cargo.Fixtures (CargoRelease (releaseArchive), cargoUpload, cargoUploadFrame, cargoUploadMetadata)
import Ecluse.Test.Registry.Cargo.Publish (cargoPublicationFixture, observeCargoRelay)
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
        uploadMetadata decoded `shouldBe` cargoUploadMetadata release
        observed <- relayMetadata decoded bytes
        observed `shouldBe` cargoUploadMetadata release
        encoded <- LBS.toStrict <$> expectRight (encodeUpload observed (uploadArchive decoded))
        let expected = case cargoUploadMetadata release of
                Object fields -> fields
                _ -> mempty
        referenceUpload encoded `shouldBe` Right (UploadFrame expected (releaseArchive release))
    it "rejects every incomplete length prefix, truncated section and trailing payload" $ do
        release <- cargoPublicationFixture
        bytes <- expectRight (cargoUpload release)
        let cases =
                [(broken, "Cargo upload has a truncated length prefix") | broken <- [BS.empty, BS.take 1 bytes, BS.take 3 bytes]]
                    <> [(broken, "Cargo upload has a truncated section") | broken <- [BS.take 5 bytes, BS.take (BS.length bytes - 1) bytes]]
                    <> [(bytes <> "trailing", "Cargo upload contains trailing bytes"), (BS.replicate 4 255 <> BS.drop 4 bytes, "Cargo upload section exceeds its byte bound")]
        for_ cases $ \(broken, reason) -> do
            decodeUpload defaultLimits broken `shouldBe` Left (ParseError reason)
            paid <- newIORef ([] :: [Int])
            readUpload defaultLimits (\cost -> modifyIORef' paid (cost :)) broken `shouldReturn` Left (ParseError reason)
            readIORef paid `shouldReturn` []
    it "checks request, metadata, archive and nesting bounds" $ do
        release <- cargoPublicationFixture
        bytes <- expectRight (cargoUpload release)
        for_ [defaultLimits{maxPublishRequestBytes = BS.length bytes - 1}, defaultLimits{maxMetadataBytes = 8}, defaultLimits{maxMirrorArtifactBytes = 8}, defaultLimits{maxNestingDepth = 1}] $ \limits ->
            decodeUpload limits bytes `shouldSatisfy` isLeft
    it "accepts each exact section cap and rejects one extra byte after earlier guards pass" $ do
        release <- cargoPublicationFixture
        let metadata = LBS.toStrict (encode (cargoUploadMetadata release))
            archive = releaseArchive release
            bytes = cargoUploadFrame metadata <> cargoUploadFrame archive
            requestLimits = defaultLimits{maxPublishRequestBytes = BS.length bytes}
            metadataLimits = defaultLimits{maxMetadataBytes = BS.length metadata}
            archiveLimits = defaultLimits{maxMirrorArtifactBytes = BS.length archive}
        for_ [requestLimits, metadataLimits, archiveLimits] $ \limits ->
            fmap uploadArchive (decodeUpload limits bytes) `shouldBe` Right archive
        decodeUpload requestLimits (bytes <> " ") `shouldBe` Left (ParseError "Cargo upload exceeds its request byte bound")
        decodeUpload metadataLimits (cargoUploadFrame (metadata <> " ") <> cargoUploadFrame archive) `shouldBe` Left (ParseError "Cargo upload section exceeds its byte bound")
        decodeUpload archiveLimits (cargoUploadFrame metadata <> cargoUploadFrame (archive <> " ")) `shouldBe` Left (ParseError "Cargo upload section exceeds its byte bound")
    it "rejects a fully framed empty archive before reserving metadata" $ do
        release <- cargoPublicationFixture
        let bytes = cargoUploadFrame (LBS.toStrict (encode (cargoUploadMetadata release))) <> cargoUploadFrame BS.empty
        paid <- newIORef ([] :: [Int])
        readUpload defaultLimits (\cost -> modifyIORef' paid (cost :)) bytes `shouldReturn` Left (ParseError "Cargo upload has no archive")
        readIORef paid `shouldReturn` []
    it "rejects duplicate identity keys and an incomplete JSON document" $ do
        release <- cargoPublicationFixture
        for_ ["{\"name\":\"fixture\",\"name\":\"other\"}", "{\"name\":\"fixture\""] $ \json ->
            decodeUpload defaultLimits (cargoUploadFrame json <> cargoUploadFrame (releaseArchive release)) `shouldBe` Left (ParseError "Cargo upload metadata is malformed or exceeds its nesting bound")
        paid <- newIORef ([] :: [Int])
        readUpload defaultLimits (\cost -> modifyIORef' paid (cost :)) (cargoUploadFrame "{" <> cargoUploadFrame (releaseArchive release))
            `shouldReturn` Left (ParseError "Cargo upload metadata is malformed or exceeds its nesting bound")
        readIORef paid `shouldReturn` [65600]
    it "refuses invalid top-level native names and versions with their owned failure category" $ do
        release <- cargoPublicationFixture
        case cargoUploadMetadata release of
            Object fields -> for_ [("name", "invalid/name", "Cargo upload name is invalid"), ("vers", "1.0", "Cargo upload version is invalid")] $ \(key, value, reason) -> do
                let metadata = Object (KeyMap.insert key (String value) fields)
                decodeUpload defaultLimits (cargoUploadFrame (LBS.toStrict (encode metadata)) <> cargoUploadFrame (releaseArchive release))
                    `shouldSatisfy` either (T.isInfixOf reason . parseErrorMessage) (const False)
            _ -> expectationFailure "fixture metadata is not an object"
    it "requires all native identity and metadata fields" $ do
        release <- cargoPublicationFixture
        case cargoUploadMetadata release of
            Object fields -> for_ ["name", "vers", "deps", "features", "authors", "keywords", "categories"] $ \key -> do
                let json = LBS.toStrict (encode (Object (KeyMap.delete key fields)))
                decodeUpload defaultLimits (cargoUploadFrame json <> cargoUploadFrame (releaseArchive release)) `shouldSatisfy` isLeft
            _ -> expectationFailure "fixture metadata is not an object"
    it "rejects malformed array members, dependency fields and optional metadata" $ do
        release <- cargoPublicationFixture
        case cargoUploadMetadata release of
            Object fields -> do
                let invalid =
                        [(key, toJSON [Null]) | key <- ["deps", "authors", "keywords", "categories"]]
                            <> [("features", object ["extra" .= [Number 42]]), ("features", object ["extra" .= Null]), ("badges", object ["ci" .= object ["branch" .= Bool True]])]
                            <> [(key, Number 42) | key <- ["description", "documentation", "homepage", "readme", "readme_file", "license", "license_file", "repository", "links", "rust_version"]]
                            <> [("rust_version", String version) | version <- ["^1.74", "1.74-beta", "1.74.0+build", "01.74", "1.74.0.1"]]
                for_ invalid $ \(key, value) ->
                    decodeUpload defaultLimits (cargoUploadFrame (LBS.toStrict (encode (Object (KeyMap.insert key value fields)))) <> cargoUploadFrame (releaseArchive release)) `shouldSatisfy` isLeft
                for_ ["name", "version_req", "features", "optional", "default_features"] $ \key ->
                    for_ [KeyMap.delete key, KeyMap.insert key Null, KeyMap.insert key (Number 42)] $ \change -> do
                        let metadata = Object (KeyMap.insert "deps" (toJSON [Object (change uploadDependency)]) fields)
                        decodeUpload defaultLimits (cargoUploadFrame (LBS.toStrict (encode metadata)) <> cargoUploadFrame (releaseArchive release)) `shouldSatisfy` isLeft
                for_ [("name", String "invalid/name"), ("explicit_name_in_toml", String "invalid/name"), ("kind", String "unknown"), ("target", Bool True), ("registry", Bool True), ("features", toJSON [Null])] $ \(key, value) -> do
                    let metadata = Object (KeyMap.insert "deps" (toJSON [Object (KeyMap.insert key value uploadDependency)]) fields)
                    decodeUpload defaultLimits (cargoUploadFrame (LBS.toStrict (encode metadata)) <> cargoUploadFrame (releaseArchive release)) `shouldSatisfy` isLeft
            _ -> expectationFailure "fixture metadata is not an object"
    it "preserves omitted and nullable optional fields and unrestricted author text" $ do
        release <- cargoPublicationFixture
        case cargoUploadMetadata release of
            Object fields -> for_ [False, True] $ \includeNulls -> do
                let optionalFields = ["description", "documentation", "homepage", "readme", "readme_file", "license", "license_file", "repository", "links", "badges", "rust_version"]
                    update current key = if includeNulls then KeyMap.insert key Null current else KeyMap.delete key current
                    dependency = if includeNulls then KeyMap.insert "kind" Null uploadDependency else foldr KeyMap.delete uploadDependency ["target", "kind", "registry", "explicit_name_in_toml"]
                    metadata = Object (KeyMap.insert "deps" (toJSON [Object dependency]) (KeyMap.insert "authors" (toJSON ["42", "<untrusted>" :: Text]) (foldl' update fields optionalFields)))
                    bytes = cargoUploadFrame (LBS.toStrict (encode metadata)) <> cargoUploadFrame (releaseArchive release)
                decoded <- expectRight (decodeUpload defaultLimits bytes)
                relayMetadata decoded bytes `shouldReturn` metadata
                uploadArchive decoded `shouldBe` releaseArchive release
            _ -> expectationFailure "fixture metadata is not an object"
    it "accepts each native Rust version component count" $ do
        release <- cargoPublicationFixture
        case cargoUploadMetadata release of
            Object fields -> for_ ["1", "1.74", "1.74.0"] $ \version -> do
                let metadata = Object (KeyMap.insert "rust_version" (String version) fields)
                bytes <- LBS.toStrict <$> expectRight (encodeUpload metadata (releaseArchive release))
                decoded <- expectRight (decodeUpload defaultLimits bytes)
                relayMetadata decoded bytes `shouldReturn` metadata
            _ -> expectationFailure "fixture metadata is not an object"
    it "keeps licence text and additive metadata without server policy restrictions" $ do
        release <- cargoPublicationFixture
        case cargoUploadMetadata release of
            Object fields -> do
                let metadata = Object (KeyMap.insert "license" (String "organisation licence") (KeyMap.insert "future-extension" (object ["retained" .= True]) fields))
                bytes <- LBS.toStrict <$> expectRight (encodeUpload metadata (releaseArchive release))
                decoded <- expectRight (decodeUpload defaultLimits bytes)
                relayMetadata decoded bytes `shouldReturn` metadata
            _ -> expectationFailure "fixture metadata is not an object"
    it "charges decoded metadata before exposing the archive" $ do
        release <- cargoPublicationFixture
        bytes <- expectRight (cargoUpload release)
        paid <- newIORef ([] :: [Int])
        result <- readUpload defaultLimits (\cost -> modifyIORef' paid (cost :)) bytes
        result `shouldSatisfy` isRight
        (_, metadataCost) <- expectRight result
        payments <- readIORef paid
        payments `shouldSatisfy` (\costs -> length costs == 1 && all (> 65536) costs)
        payments `shouldBe` [metadataCost]

relayMetadata :: CargoUploadBody -> ByteString -> IO Value
relayMetadata decoded bytes = do
    UploadFrame metadata archive <- observeCargoRelay loopbackRegistryUrl bytes
    archive `shouldBe` uploadArchive decoded
    pure (Object metadata)

uploadDependency :: KeyMap.KeyMap Value
uploadDependency = KeyMap.fromList [("name", String "dependency"), ("version_req", String "^1"), ("features", toJSON ([] :: [Text])), ("optional", Bool False), ("default_features", Bool True), ("target", Null), ("kind", String "normal"), ("registry", Null), ("explicit_name_in_toml", Null)]
