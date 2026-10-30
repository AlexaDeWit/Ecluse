-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Genuine publication preserves native bytes and keeps caller credentials out of read roles.
module Ecluse.Core.Registry.RubyGems.PublishSpec (spec) where

import Codec.Archive.Tar qualified as Tar
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Client (Request (method, requestBody, requestHeaders), RequestBody (RequestBodyBS), defaultManagerSettings, newManager)
import Network.HTTP.Types.Status (status200)
import Test.Hspec

import Ecluse.Core.Credential (ClientCredential (ClientCredential), bareCredential, mkSecret)
import Ecluse.Core.Package (PackageDetails (..), PackageInfo (..))
import Ecluse.Core.Package.Hash (sha256Hex)
import Ecluse.Core.Registry (ParseError (..), PublishBodyFault (PublishMalformed))
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
import Ecluse.Test.Registry.Archive (pax, paxRecord, tarEntries)
import Ecluse.Test.Registry.Publish (anonymousPublishAt, publishOnlyFile)
import Ecluse.Test.Registry.RubyGems.Fixtures (loadFixtureInfo)
import Ecluse.Test.Stub (Captured (..), allCaptured, headerValue, stubBaseUrl, withRoutedStub)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "RubyGems publication" $ do
    it "relays the original gem body with the caller's native key and checked OTP" $
        withRoutedStub (const (status200, [], "owned native reply")) $ \target -> do
            archive <- readFileBS rootArchive
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
            archive <- readFileBS rootArchive
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

    it "refuses an extended outer header as a malformed upload" $ do
        entries <- readFileBS "test/fixtures/rubygems/gems/ecluse-fixture-native-1.0.0.gem" >>= tarEntries . LBS.fromStrict
        global <- pax 'g' (paxRecord "comment=opaque\n")
        let claims written = uploadClaims defaultLimits (const pass) "unused" (PublishRequestBody (LBS.toStrict (Tar.write written)) Nothing NoPublishExtras)
        claims entries `shouldReturn` Right ["ecluse-fixture-native"]
        claims (global : entries) `shouldReturn` Left (PublishMalformed (ParseError "gem envelope carries an extended or prefixed tar header"))

    it "mirrors a public gem whose outer tar carries an extended header" $
        withRoutedStub (const (status200, [], "written")) $ \target -> do
            entries <- readFileBS rootArchive >>= tarEntries . LBS.fromStrict
            global <- pax 'g' (paxRecord "comment=opaque\n")
            let archive = LBS.toStrict (Tar.write (global : entries))
            doc <- sourceVersionOf archive
            withSweeper $ \deadlines -> do
                writer <- anonymousPublishAt deadlines (rubyPublishCodec factors) (loopbackRegistryUrl (stubBaseUrl target <> "/private"))
                publishOnlyFile writer doc (PublishPlan (pkgVersion (vdDetails doc))) archive `shouldReturn` Right ()
            map capBody <$> allCaptured target `shouldReturn` [archive]

    it "publishes only the admitted digest and preserves verified bytes without an OTP" $ do
        archive <- readFileBS rootArchive
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
            archive <- readFileBS rootArchive
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

rootArchive :: FilePath
rootArchive = "test/fixtures/rubygems/gems/ecluse-fixture-root-1.0.0.gem"

sourceVersion :: IO VersionDoc
sourceVersion = readFileBS rootArchive >>= sourceVersionOf

-- | The root fixture's 1.0.0 version, its source row advertising the given archive's SHA-256.
sourceVersionOf :: ByteString -> IO VersionDoc
sourceVersionOf archive = do
    genuine <- sha256Hex <$> readFileBS rootArchive
    (name, rows) <- loadFixtureInfo "ecluse-fixture-root"
    let advertise row = decodeInfoRow name (grEntryKey row) (encodeUtf8 (T.replace genuine (sha256Hex archive) (decodeUtf8 (grSource row))))
    advertised <- traverse (either (fail . show) pure . advertise) rows
    info <- expectRight (projectGemRows defaultLimits name "https://public.test" advertised)
    details <- maybe (fail "missing native fixture version") pure (Map.lookup "1.0.0" (infoVersions info))
    source <- maybe (fail "missing native fixture document") pure (selectGemVersion (pkgVersion details) (GemDocument name "---\n" advertised))
    pure (VersionDoc details (Just (fst rubyGemsCached source)))
