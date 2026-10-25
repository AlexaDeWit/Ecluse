-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native publication preserves archive bytes and checks configured API authority before writes.
module Ecluse.Core.Registry.Cargo.PublishSpec (spec) where

import Data.Aeson (Value (..), encode, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (ResponseHeaders)
import Network.HTTP.Types.Status (Status, status200)
import Test.Hspec

import Ecluse.Core.Credential (bareCredential, mkSecret)
import Ecluse.Core.Package (PackageDetails (pkgName, pkgVersion), PackageInfo (..))
import Ecluse.Core.Registry.Adapter.Capability (AdapterPublish (publishMirrorRequirements), MirrorRequirements (mirrorStateRequirement), MirrorStateRequirement (DurableVersionState), PublishRequestBody (PublishRequestBody))
import Ecluse.Core.Registry.Cargo.Publish
import Ecluse.Core.Registry.Cargo.Publish.Body qualified as Body
import Ecluse.Core.Registry.Metadata (VersionDoc (vdDetails))
import Ecluse.Core.Registry.Publish (MirrorTransport (..), PublishCodec (pcNativeComplete), PublishPlan (PublishPlan), newMirrorPublish)
import Ecluse.Core.Security.Limits (defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (ChargeFactors))
import Ecluse.Test.Deadline (withExchangeClient)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Registry.Cargo.Fixtures (CargoRelease (releaseArchive), cargoIndexRecord, cargoUpload, cargoUploadMetadata)
import Ecluse.Test.Registry.Cargo.Publish (cargoPublicationFixture, cargoSourceVersion)
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
            uploadClaims defaultLimits (const (pure ())) (PublishRequestBody bytes Nothing) `shouldReturn` Right ["fixture"]
        it "rejects different archive name, version and build spelling before relay" $ do
            release <- cargoPublicationFixture
            case cargoUploadMetadata release of
                Object fields -> for_ [("name", "another"), ("vers", "2.0.0"), ("vers", "1.0.0+other")] $ \(key, value) -> do
                    bytes <- LBS.toStrict <$> expectRight (Body.encodeUpload (Object (KeyMap.insert key (String value) fields)) (releaseArchive release))
                    uploadClaims defaultLimits (const (pure ())) (PublishRequestBody bytes Nothing) >>= (`shouldSatisfy` isLeft)
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
