-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Genuine publication preserves native bytes and keeps caller credentials out of read roles.
module Ecluse.Core.Registry.RubyGems.PublishSpec (spec) where

import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Network.HTTP.Client (Request (method, requestBody, requestHeaders), RequestBody (RequestBodyBS), defaultManagerSettings, newManager)
import Network.HTTP.Types.Status (status200)
import Test.Hspec

import Ecluse.Core.Credential (ClientCredential (ClientCredential), bareCredential, mkSecret)
import Ecluse.Core.Package (PackageDetails (..), PackageInfo (..))
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.Adapter.Capability
import Ecluse.Core.Registry.CachedDocument (rubyGemsCached)
import Ecluse.Core.Registry.Metadata (VersionDoc (..))
import Ecluse.Core.Registry.MirrorUpload
import Ecluse.Core.Registry.Origin (OriginClient (ocChargeFullReadWorkspace), originClient)
import Ecluse.Core.Registry.Publish (MirrorPublish (mpProbeInventory), MirrorTransport (..), PublishCodec (..), PublishPlan (PublishPlan), newMirrorPublish)
import Ecluse.Core.Registry.RubyGems.Credential (rubyCodeArtifactPublishCredential)
import Ecluse.Core.Registry.RubyGems.Document
import Ecluse.Core.Registry.RubyGems.Project (projectName)
import Ecluse.Core.Registry.RubyGems.Projection (projectGemRows)
import Ecluse.Core.Registry.RubyGems.Publish
import Ecluse.Core.Registry.RubyGems.Wire (decodeInfoRow)
import Ecluse.Core.Security (Limits (maxMirrorArtifactBytes), defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (ChargeFactors))
import Ecluse.Server.Admission.Support (refuseCharge)
import Ecluse.Test.Deadline (withExchangeClient, withSweeper)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Registry.Publish (publishOnlyFile)
import Ecluse.Test.Stub (Captured (..), allCaptured, headerValue, stubBaseUrl, withRoutedStub)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "RubyGems publication" $ do
    it "relays the original gem body with the caller's native key and checked OTP" $
        withRoutedStub (const (status200, [], "owned native reply")) $ \target -> do
            archive <- readFileBS "test/fixtures/rubygems/gems/ecluse-fixture-root-1.0.0.gem"
            name <- expectRight (projectName "ecluse-fixture-root")
            extras <- expectRight (publishRequestExtras publisher [("OTP", "opaque-code")])
            withExchangeClient (newManager defaultManagerSettings) $ \client -> do
                let origin = originClient defaultLimits client (loopbackRegistryUrl (stubBaseUrl target <> "/private")) (Just (bareCredential (mkSecret "native-api-key")))
                void (publishRelay publisher origin name (PublishRequestBody archive (Just "application/octet-stream") extras) >>= expectRight)
                allCaptured target >>= \case
                    [written] -> do
                        capMethod written `shouldBe` "POST"
                        capPath written `shouldBe` "/private/api/v1/gems"
                        capBody written `shouldBe` archive
                        headerValue "Authorization" written `shouldBe` Just "native-api-key"
                        headerValue "OTP" written `shouldBe` Just "opaque-code"
                    _ -> expectationFailure "expected exactly one native publication"

    it "relays an authorised body without inspecting the archive a second time" $
        withRoutedStub (const (status200, [], "owned native reply")) $ \target -> do
            archive <- readFileBS "test/fixtures/rubygems/gems/ecluse-fixture-root-1.0.0.gem"
            name <- expectRight (projectName "ecluse-fixture-root")
            withExchangeClient (newManager defaultManagerSettings) $ \client -> do
                let origin = (originClient defaultLimits client (loopbackRegistryUrl (stubBaseUrl target <> "/private")) (Just (bareCredential (mkSecret "native-api-key")))){ocChargeFullReadWorkspace = refuseCharge}
                void (publishRelay publisher origin name (PublishRequestBody archive (Just "application/octet-stream") NoPublishExtras) >>= expectRight)
                map capMethod <$> allCaptured target `shouldReturn` ["POST"]

    it "accepts native-extension builds and refuses malformed or oversized archives" $ do
        archive <- readFileBS "test/fixtures/rubygems/gems/ecluse-fixture-native-1.0.0.gem"
        let body = PublishRequestBody archive Nothing NoPublishExtras
        uploadClaims defaultLimits (const pass) "unused" body `shouldReturn` Right ["ecluse-fixture-native"]
        uploadClaims defaultLimits{maxMirrorArtifactBytes = 1} (const pass) "unused" body >>= (`shouldSatisfy` isLeft)
        uploadClaims defaultLimits (const pass) "unused" (PublishRequestBody "not a native archive" Nothing NoPublishExtras) >>= (`shouldSatisfy` isLeft)

    it "publishes only the admitted digest and preserves verified bytes without an OTP" $ do
        archive <- readFileBS "test/fixtures/rubygems/gems/ecluse-fixture-root-1.0.0.gem"
        doc <- sourceVersion
        let details = vdDetails doc
        case rubyPublishCodec factors of
            PublishCodec{pcPrepareSource, pcPrepareUpload, pcPublishRequest, pcNativeComplete} -> do
                (inventory, source) <- expectRight (pcPrepareSource (pkgName details) (pkgVersion details) doc)
                file <- case inventoryFiles inventory of
                    only :| [] -> pure only
                    _ -> fail "expected one native archive"
                verified <- expectRight (verifyMirrorBytes file archive)
                verifyMirrorBytes file (archive <> "changed") `shouldSatisfy` isLeft
                upload <- withSweeper (\deadlines -> pcPrepareUpload deadlines defaultLimits (const pass) inventory source file verified) >>= expectRight
                request <- expectRight (pcPublishRequest "https://target.test/private" (Just (bareCredential (mkSecret "worker-key"))) (PublishPlan (pkgVersion details)) upload)
                method request `shouldBe` "POST"
                lookup "Authorization" (requestHeaders request) `shouldBe` Just "worker-key"
                lookup "OTP" (requestHeaders request) `shouldBe` Nothing
                case requestBody request of
                    RequestBodyBS bytes -> bytes `shouldBe` archive
                    _ -> expectationFailure "native publication replaced the verified byte body"
                pcNativeComplete (pkgName details) (pkgVersion details) (PackageInfo (pkgName details) (Map.singleton "1.0.0" details) mempty []) `shouldBe` False

    for_ [("declared CodeArtifact roles", rubyPublishCodecWith rubyCodeArtifactPublishCredential factors, "Bearer worker-key"), ("generic roles with username aws", rubyPublishCodec factors, "worker-key")] $ \(label, codec, expectedKey) ->
        it ("uses Basic reads and the selected native writer mapping for " <> label) $ do
            listing <- readFileBS "test/fixtures/rubygems/info/ecluse-fixture-root"
            archive <- readFileBS "test/fixtures/rubygems/gems/ecluse-fixture-root-1.0.0.gem"
            doc <- sourceVersion
            let reply captured = (status200, [("Content-Type", "text/plain")], if capMethod captured == "POST" then "written" else LBS.fromStrict listing)
                details = vdDetails doc
            withRoutedStub reply $ \target ->
                withExchangeClient (newManager defaultManagerSettings) $ \client -> do
                    let credential = ClientCredential (Just "aws") (mkSecret "worker-key")
                        transport = MirrorTransport client (pure (Just credential)) defaultLimits passthroughTracingPort
                        endpoint = loopbackRegistryUrl (stubBaseUrl target <> "/private")
                        writer = newMirrorPublish transport endpoint endpoint codec
                    void (mpProbeInventory writer (const pass) (pkgName details) >>= expectRight)
                    publishOnlyFile writer doc (PublishPlan (pkgVersion details)) archive `shouldReturn` Right ()
                    captures <- allCaptured target
                    case filter ((== "GET") . capMethod) captures of
                        [readRequest] -> do
                            capPath readRequest `shouldBe` "/private/info/ecluse-fixture-root"
                            headerValue "Authorization" readRequest `shouldBe` Just "Basic YXdzOndvcmtlci1rZXk="
                        _ -> expectationFailure "expected one authorised native read"
                    case filter ((== "POST") . capMethod) captures of
                        [writeRequest] -> do
                            capPath writeRequest `shouldBe` "/private/api/v1/gems"
                            headerValue "Authorization" writeRequest `shouldBe` Just expectedKey
                            capBody writeRequest `shouldBe` archive
                        _ -> expectationFailure "expected one authorised native write"

    it "requires durable native state and an independent proof after every publication reply" $ do
        mirrorStateRequirement (publishMirrorRequirements publisher) `shouldBe` DurableVersionState
        classifyPublish 200 "written" `shouldBe` Right ()
        classifyPublish 409 "already present" `shouldSatisfy` isLeft
        classifyPublish 403 "refused" `shouldSatisfy` isLeft

publisher :: AdapterPublish
publisher = rubyPublish factors

factors :: ChargeFactors
factors = ChargeFactors 1000 1000 1000

sourceVersion :: IO VersionDoc
sourceVersion = do
    name <- expectRight (projectName "ecluse-fixture-root")
    bytes <- readFileBS "test/fixtures/rubygems/info/ecluse-fixture-root"
    rows <- traverse (\(position, raw) -> expectRight (decodeInfoRow name (RecordEntry position) raw)) (zip [1 ..] (filter (not . BS8.null) (drop 1 (BS8.lines bytes))))
    info <- expectRight (projectGemRows defaultLimits name "https://public.test" rows)
    details <- maybe (fail "missing native fixture version") pure (Map.lookup "1.0.0" (infoVersions info))
    source <- maybe (fail "missing native fixture document") pure (selectGemVersion (pkgVersion details) (GemDocument name "---\n" rows))
    pure (VersionDoc details (Just (fst rubyGemsCached source)))
