-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native publication preserves archive bytes and checks configured API authority before writes.
module Ecluse.Core.Registry.Cargo.PublishSpec (spec) where

import Codec.Compression.GZip qualified as GZip
import Data.Aeson (Value (..), encode, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (ResponseHeaders)
import Network.HTTP.Types.Status (Status, status200)
import Test.Hspec

import Ecluse.Core.Credential (bareCredential, mkSecret)
import Ecluse.Core.Package (PackageDetails (pkgName, pkgVersion), PackageInfo (..))
import Ecluse.Core.Registry (PublishFault (PublishSourceUnavailable))
import Ecluse.Core.Registry.Adapter.Capability (AdapterPublish (publishMirrorRequirements, publishRelay), MirrorRequirements (mirrorStateRequirement), MirrorStateRequirement (DurableVersionState), PublishRequestBody (PublishRequestBody))
import Ecluse.Core.Registry.Cargo.Publish
import Ecluse.Core.Registry.Cargo.Publish.Body qualified as Body
import Ecluse.Core.Registry.Metadata (VersionDoc (vdDetails))
import Ecluse.Core.Registry.Origin (originClient)
import Ecluse.Core.Registry.Publish (MirrorTransport (..), PublishCodec (pcNativeComplete), PublishPlan (PublishPlan), newMirrorPublish)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Security.Limits (Limits (maxMetadataBytes), defaultLimits)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (ChargeFactors))
import Ecluse.Test.Deadline (withExchangeClient)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Registry.Archive (tarBytes)
import Ecluse.Test.Registry.Cargo.Fixtures (CargoRelease (releaseArchive, releaseChecksum), cargoIndexRecord, cargoUpload, cargoUploadMetadata, cargoUploadWithMetadata)
import Ecluse.Test.Registry.Cargo.Metadata (cargoTestName)
import Ecluse.Test.Registry.Cargo.Publish (cargoPublicationFixture, cargoResolverAgreements, cargoResolverArchive, cargoResolverContradictions, cargoResolverMetadata, cargoSourceVersion)
import Ecluse.Test.Registry.Cargo.Reference (UploadFrame (..), referenceUpload)
import Ecluse.Test.Registry.Publish (publishOnlyFile)
import Ecluse.Test.Stub (Captured (..), allCaptured, headerValue, stubBaseUrl, withRoutedStub)
import Ecluse.Test.Support (expectRight)
import Ecluse.Test.Wai (selfBaseUrlOf)

spec :: Spec
spec = do
    describe "Cargo native publication" $ do
        it "writes complete archive metadata and exact verified bytes to the approved native API" $
            withRoutedStub (targetReply "/registry") $ \stub -> do
                release <- cargoPublicationFixture
                doc <- cargoSourceVersion (cargoIndexRecord release)
                let details = vdDetails doc
                withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
                    minted <- newIORef (0 :: Int)
                    let mint = modifyIORef' minted (+ 1) $> Just (bareCredential (mkSecret "cargo-token"))
                        transport = MirrorTransport exchanges mint defaultLimits passthroughTracingPort
                        writer = newMirrorPublish transport (loopbackRegistryUrl (stubBaseUrl stub <> "/index")) (loopbackRegistryUrl (stubBaseUrl stub <> "/registry")) codec
                    publishOnlyFile writer doc (PublishPlan (pkgVersion details)) (releaseArchive release) `shouldReturn` Right ()
                    captures <- allCaptured stub
                    map capPath captures `shouldBe` ["/index/config.json", "/registry/api/v1/crates/new"]
                    map (headerValue "Authorization") captures `shouldBe` [Just "cargo-token", Just "cargo-token"]
                    readIORef minted `shouldReturn` 1
                    written <- maybe (fail "missing upload capture") pure (find ((== "PUT") . capMethod) captures)
                    parsed <- expectRight (referenceUpload (capBody written))
                    uploadArchive parsed `shouldBe` releaseArchive release
                    KeyMap.lookup "readme" (uploadMetadata parsed) `shouldBe` Just (String "owned readme")
                    KeyMap.lookup "authors" (uploadMetadata parsed) `shouldBe` Just (Array (fromList [String "Ecluse contributors"]))
                    headerValue "Content-Type" written `shouldBe` Just "application/octet-stream"
        it "publishes modern source omissions with interpreted dependency defaults" $
            withRoutedStub (targetReply "/registry") $ \stub -> do
                original <- cargoPublicationFixture
                let manifest = "[package]\nname='fixture'\nversion='1.0.0'\nrust-version='1.74'\n[dependencies]\ndependency='1'\n"
                archive <- LBS.toStrict . GZip.compress <$> tarBytes [("fixture-1.0.0/Cargo.toml", manifest), ("fixture-1.0.0/src/lib.rs", "pub fn owned() {}\n")]
                let release = original{releaseArchive = archive, releaseChecksum = hexSha256Of archive}
                source <- case cargoIndexRecord release of
                    Object fields -> pure (Object (KeyMap.insert "deps" (Array (fromList [object ["name" .= ("dependency" :: Text), "req" .= ("^1" :: Text), "registry" .= Null]])) (KeyMap.delete "features" fields)))
                    _ -> fail "source fixture is not an object"
                doc <- cargoSourceVersion source
                withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
                    let transport = MirrorTransport exchanges (pure (Just (bareCredential (mkSecret "cargo-token")))) defaultLimits passthroughTracingPort
                        writer = newMirrorPublish transport (loopbackRegistryUrl (stubBaseUrl stub <> "/index")) (loopbackRegistryUrl (stubBaseUrl stub <> "/registry")) codec
                    publishOnlyFile writer doc (PublishPlan (pkgVersion (vdDetails doc))) archive `shouldReturn` Right ()
                    captures <- allCaptured stub
                    map capPath captures `shouldBe` ["/index/config.json", "/registry/api/v1/crates/new"]
                    written <- maybe (fail "missing upload capture") pure (find ((== "PUT") . capMethod) captures)
                    parsed <- expectRight (referenceUpload (capBody written))
                    uploadArchive parsed `shouldBe` archive
                    KeyMap.lookup "features" (uploadMetadata parsed) `shouldBe` Just (object [])
                    KeyMap.lookup "deps" (uploadMetadata parsed) `shouldBe` Just (Array (fromList [object ["name" .= ("dependency" :: Text), "version_req" .= ("^1" :: Text), "features" .= ([] :: [Text]), "optional" .= False, "default_features" .= True, "target" .= Null, "kind" .= ("normal" :: Text), "registry" .= ("https://github.com/rust-lang/crates.io-index" :: Text), "explicit_name_in_toml" .= Null]]))
        it "refuses defaulted mirror links contradictions before destination discovery or upload" $ do
            original <- cargoPublicationFixture
            let manifest = "[package]\nname='fixture'\nversion='1.0.0'\nrust-version='1.74'\nlinks='native_foo'\nbuild='build.rs'\n"
            archive <- LBS.toStrict . GZip.compress <$> tarBytes [("fixture-1.0.0/Cargo.toml", manifest), ("fixture-1.0.0/build.rs", "fn main() {}\n"), ("fixture-1.0.0/src/lib.rs", "pub fn owned() {}\n")]
            let release = original{releaseArchive = archive, releaseChecksum = hexSha256Of archive}
            for_ [(Nothing, False), (Just Null, False), (Just (String "native_bar"), False), (Just (String "native_foo"), True)] $ \(links, agrees) ->
                withRoutedStub (targetReply "/registry") $ \stub -> do
                    source <- case cargoIndexRecord release of
                        Object fields -> pure (Object (maybe (KeyMap.delete "links" fields) (\value -> KeyMap.insert "links" value fields) links))
                        _ -> fail "source fixture is not an object"
                    doc <- cargoSourceVersion source
                    withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
                        let transport = MirrorTransport exchanges (pure (Just (bareCredential (mkSecret "cargo-token")))) defaultLimits passthroughTracingPort
                            writer = newMirrorPublish transport (loopbackRegistryUrl (stubBaseUrl stub <> "/index")) (loopbackRegistryUrl (stubBaseUrl stub <> "/registry")) codec
                        result <- publishOnlyFile writer doc (PublishPlan (pkgVersion (vdDetails doc))) archive
                        if agrees
                            then do
                                result `shouldBe` Right ()
                                captures <- allCaptured stub
                                map capPath captures `shouldBe` ["/index/config.json", "/registry/api/v1/crates/new"]
                            else do
                                result `shouldBe` Left (PublishSourceUnavailable "Cargo manifest and index links declarations disagree")
                                allCaptured stub `shouldReturn` []
        it "completes the final metadata-size decision before any destination request" $ do
            original <- cargoPublicationFixture
            let manifest = "[package]\nname='fixture'\nversion='1.0.0'\nrust-version='1.74'\n"
                limits = defaultLimits{maxMetadataBytes = 2048}
            -- Keep raw archive data within the shared cap, then exceed it through JSON escaping.
            for_ [(BS.replicate 1024 65, True), (BS.replicate 1024 34, False)] $ \(readme, shouldPublish) ->
                withRoutedStub (targetReply "/registry") $ \stub -> do
                    archive <- LBS.toStrict . GZip.compress <$> tarBytes [("fixture-1.0.0/Cargo.toml", manifest), ("fixture-1.0.0/README.md", readme), ("fixture-1.0.0/src/lib.rs", "pub fn owned() {}\n")]
                    let release = original{releaseArchive = archive, releaseChecksum = hexSha256Of archive}
                    doc <- cargoSourceVersion (cargoIndexRecord release)
                    withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
                        let transport = MirrorTransport exchanges (pure (Just (bareCredential (mkSecret "cargo-token")))) limits passthroughTracingPort
                            writer = newMirrorPublish transport (loopbackRegistryUrl (stubBaseUrl stub <> "/index")) (loopbackRegistryUrl (stubBaseUrl stub <> "/registry")) codec
                        result <- publishOnlyFile writer doc (PublishPlan (pkgVersion (vdDetails doc))) archive
                        if shouldPublish
                            then do
                                result `shouldBe` Right ()
                                captures <- allCaptured stub
                                map capPath captures `shouldBe` ["/index/config.json", "/registry/api/v1/crates/new"]
                                written <- maybe (fail "missing upload capture") pure (find ((== "PUT") . capMethod) captures)
                                parsed <- expectRight (referenceUpload (capBody written))
                                KeyMap.lookup "readme" (uploadMetadata parsed) `shouldBe` Just (String (decodeUtf8 readme))
                                uploadArchive parsed `shouldBe` archive
                            else do
                                result `shouldBe` Left (PublishSourceUnavailable "Cargo publication metadata exceeds its byte bound")
                                allCaptured stub `shouldReturn` []
        it "refuses discovery which changes the approved API base path before any upload" $
            withRoutedStub (targetReply "/unapproved") $ \stub -> do
                release <- cargoPublicationFixture
                doc <- cargoSourceVersion (cargoIndexRecord release)
                withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
                    let transport = MirrorTransport exchanges (pure (Just (bareCredential (mkSecret "cargo-token")))) defaultLimits passthroughTracingPort
                        writer = newMirrorPublish transport (loopbackRegistryUrl (stubBaseUrl stub <> "/index")) (loopbackRegistryUrl (stubBaseUrl stub <> "/registry")) codec
                    result <- publishOnlyFile writer doc (PublishPlan (pkgVersion (vdDetails doc))) (releaseArchive release)
                    result `shouldSatisfy` isLeft
                    captures <- allCaptured stub
                    map capPath captures `shouldBe` ["/index/config.json"]
        it "requires durable source state even when a target has one complete archive" $ do
            release <- cargoPublicationFixture
            doc <- cargoSourceVersion (cargoIndexRecord release)
            let details = vdDetails doc
            pcNativeComplete codec (pkgName details) (pkgVersion details) (PackageInfo (pkgName details) mempty mempty []) `shouldBe` False
            mirrorStateRequirement (publishMirrorRequirements (cargoPublish factors)) `shouldBe` DurableVersionState
    describe "Cargo first-party claims" $ do
        it "accepts matching complete upload metadata and archive identity" $ do
            release <- cargoPublicationFixture
            bytes <- expectRight (cargoUpload release)
            uploadClaims defaultLimits (const (pure ())) "https://proxy.test/cargo/private" (PublishRequestBody bytes Nothing) `shouldReturn` Right ["fixture"]
        it "compares complete resolver facts against the retained effective manifest" $ do
            archive <- cargoResolverArchive
            for_ cargoResolverAgreements $ \metadata -> do
                valid <- expectRight (cargoUploadWithMetadata metadata archive)
                uploadClaims defaultLimits (const (pure ())) "https://proxy.test/cargo/private" (PublishRequestBody valid Nothing) `shouldReturn` Right ["fixture"]
            for_ cargoResolverContradictions $ \metadata -> do
                bytes <- expectRight (cargoUploadWithMetadata metadata archive)
                Body.decodeUpload defaultLimits bytes `shouldSatisfy` isRight
                uploadClaims defaultLimits (const (pure ())) "https://proxy.test/cargo/private" (PublishRequestBody bytes Nothing) >>= (`shouldSatisfy` isLeft)
        it "reserves comparison workspace after archive inspection and before accepting resolver claims" $ do
            archive <- cargoResolverArchive
            bytes <- expectRight (cargoUploadWithMetadata cargoResolverMetadata archive)
            paid <- newIORef ([] :: [Int])
            uploadClaims defaultLimits (\cost -> modifyIORef' paid (<> [cost])) "https://proxy.test/cargo/private" (PublishRequestBody bytes Nothing) `shouldReturn` Right ["fixture"]
            payments <- readIORef paid
            case reverse payments of
                comparison : archiveCharge : _ -> comparison `shouldSatisfy` (> archiveCharge)
                _ -> expectationFailure "Cargo upload omitted its archive or comparison reservation"
        it "rejects malformed members before either claims or relay can contact the destination" $
            withRoutedStub (targetReply "/registry") $ \stub -> do
                release <- cargoPublicationFixture
                withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
                    let origin = originClient defaultLimits exchanges (loopbackRegistryUrl (stubBaseUrl stub <> "/registry")) (Just (bareCredential (mkSecret "cargo-token")))
                    case cargoUploadMetadata release of
                        Object fields -> for_ [("deps", Array (fromList [Null])), ("authors", Array (fromList [Number 42]))] $ \(key, value) -> do
                            bytes <- LBS.toStrict <$> expectRight (Body.encodeUpload (Object (KeyMap.insert key value fields)) (releaseArchive release))
                            let body = PublishRequestBody bytes Nothing
                            uploadClaims defaultLimits (const (pure ())) "https://proxy.test/cargo/private" body >>= (`shouldSatisfy` isLeft)
                            publishRelay (cargoPublish factors) origin cargoTestName body >>= (`shouldSatisfy` isLeft)
                        _ -> expectationFailure "fixture upload metadata is not an object"
                    allCaptured stub `shouldReturn` []
        it "rejects different archive name, version and build spelling before relay" $ do
            release <- cargoPublicationFixture
            case cargoUploadMetadata release of
                Object fields -> for_ [("name", "another"), ("vers", "2.0.0"), ("vers", "1.0.0+other")] $ \(key, value) -> do
                    bytes <- LBS.toStrict <$> expectRight (Body.encodeUpload (Object (KeyMap.insert key (String value) fields)) (releaseArchive release))
                    uploadClaims defaultLimits (const (pure ())) "https://proxy.test/cargo/private" (PublishRequestBody bytes Nothing) >>= (`shouldSatisfy` isLeft)
                _ -> expectationFailure "fixture upload metadata is not an object"
    describe "Cargo native response classification" $ do
        it "accepts native success objects and rejects errors even with a success status" $ do
            classifyPublish 200 "{}" `shouldBe` Right ()
            classifyPublish 200 "{\"warnings\":{},\"errors\":[]}" `shouldBe` Right ()
            for_ [(200, "{\"errors\":[{\"detail\":\"rejected\"}]}"), (200, "invalid"), (400, "{\"errors\":[{\"detail\":\"already exists\"}]}"), (409, "{}"), (503, "{}")] $ \(status, body) ->
                classifyPublish status body `shouldSatisfy` isLeft

factors :: ChargeFactors
factors = ChargeFactors 1000 1000 1000

codec :: PublishCodec
codec = cargoPublishCodec factors

targetReply :: Text -> Captured -> (Status, ResponseHeaders, LByteString)
targetReply api captured =
    let root = selfBaseUrlOf (capHeaders captured)
        discovery = object ["dl" .= (root <> "/crates/{crate}/{version}/download"), "api" .= (root <> api), "auth-required" .= True]
     in (status200, [("Content-Type", "application/json")], if capMethod captured == "PUT" then "{}" else encode discovery)
