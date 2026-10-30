-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native publication preserves archive bytes and checks configured API authority before writes.
module Ecluse.Core.Registry.Cargo.PublishSpec (spec) where

import Data.Aeson (Value (..), encode, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (ResponseHeaders)
import Network.HTTP.Types.Status (Status, status200, status401, status403, status404, status500)
import Test.Hspec

import Ecluse.Core.Credential (bareCredential, mkSecret)
import Ecluse.Core.Package (PackageDetails (pkgName, pkgVersion), PackageInfo (..))
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry (FetchFault (FetchBoundExceeded, FetchUrlUnformable), ParseError (..), PublishBodyFault (PublishMalformed), PublishError (PublishError), PublishFault (..), UrlFormationError (UnparseableUrl))
import Ecluse.Core.Registry.Adapter.Capability (AdapterPublish (publishMirrorRequirements, publishRelay), MirrorRequirements (mirrorStateRequirement), PublishExtras (NoPublishExtras), PublishRequestBody (PublishRequestBody), durableMirrorDocuments)
import Ecluse.Core.Registry.CachedDocument (cargoCached)
import Ecluse.Core.Registry.Cargo.Document (cargoDocument)
import Ecluse.Core.Registry.Cargo.Publish
import Ecluse.Core.Registry.Cargo.Publish.Body qualified as Body
import Ecluse.Core.Registry.Metadata (VersionDoc (..))
import Ecluse.Core.Registry.Origin (originClient)
import Ecluse.Core.Registry.Publish (MirrorTransport (..), PublishCodec (pcNativeComplete), PublishPlan (PublishPlan), newMirrorPublish)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Security.Limits (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge), Limits (maxMetadataBytes), defaultLimits)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (ChargeFactors))
import Ecluse.Core.Version (renderVersion)
import Ecluse.Test.Deadline (withExchangeClient)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Registry.Cargo.Fixtures (CargoRelease (releaseArchive, releaseChecksum), cargoCrateArchive, cargoIndexRecord, cargoUpload, cargoUploadMetadata, cargoUploadWithMetadata)
import Ecluse.Test.Registry.Cargo.Metadata (cargoTestName)
import Ecluse.Test.Registry.Cargo.Publish (cargoForeignRegistryArchive, cargoForeignRegistryMetadata, cargoForeignRegistryRefusal, cargoPublicationFixture, cargoResolverAgreements, cargoResolverArchive, cargoResolverContradictions, cargoResolverDependencyField, cargoResolverMetadata, cargoSourceVersion)
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
                archive <- cargoCrateArchive "fixture" "1.0.0" [("Cargo.toml", manifest), ("src/lib.rs", "pub fn owned() {}\n")]
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
            archive <- cargoCrateArchive "fixture" "1.0.0" [("Cargo.toml", manifest), ("build.rs", "fn main() {}\n"), ("src/lib.rs", "pub fn owned() {}\n")]
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
                    archive <- cargoCrateArchive "fixture" "1.0.0" [("Cargo.toml", manifest), ("README.md", readme), ("src/lib.rs", "pub fn owned() {}\n")]
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
                    result `shouldBe` Left (PublishRejected (PublishError "the target discovery is malformed or names an unapproved API root"))
                    captures <- allCaptured stub
                    map capPath captures `shouldBe` ["/index/config.json"]
                    map capMethod captures `shouldBe` ["GET"]
        it "classifies absent, malformed, refused and oversized discovery before any PUT" $ do
            release <- cargoPublicationFixture
            doc <- cargoSourceVersion (cargoIndexRecord release)
            let malformed = PublishRejected (PublishError "the target discovery is malformed or names an unapproved API root")
                replies =
                    [(status200, "{\"dl\":\"https://static.crates.io/{crate}\"}", malformed), (status200, "invalid JSON", malformed)]
                        <> [(status, "upstream refusal", PublishRejected (PublishError ("target discovery failed with HTTP status " <> show code))) | (status, code) <- [(status401, 401 :: Int), (status403, 403), (status404, 404), (status500, 500)]]
                        <> [(status200, LBS.replicate 65537 32, PublishFetch (FetchBoundExceeded (BodyTooLarge (MetadataBodyLimit 65536))))]
            for_ replies $ \(status, body, expected) ->
                withRoutedStub (const (status, [], body)) $ \stub ->
                    withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
                        let transport = MirrorTransport exchanges (pure (Just (bareCredential (mkSecret "cargo-token")))) defaultLimits passthroughTracingPort
                            writer = newMirrorPublish transport (loopbackRegistryUrl (stubBaseUrl stub <> "/index")) (loopbackRegistryUrl (stubBaseUrl stub <> "/registry")) codec
                        publishOnlyFile writer doc (PublishPlan (pkgVersion (vdDetails doc))) (releaseArchive release) `shouldReturn` Left expected
                        captures <- allCaptured stub
                        map capPath captures `shouldBe` ["/index/config.json"]
                        map capMethod captures `shouldBe` ["GET"]
        it "refuses checksum-verified archives and selected text faults before destination discovery" $ do
            original <- cargoPublicationFixture
            missingManifest <- cargoCrateArchive "fixture" "1.0.0" [("src/lib.rs", "owned")]
            invalidReadme <- cargoCrateArchive "fixture" "1.0.0" [("README.md", BS.singleton 255), ("Cargo.toml", "[package]\nname='fixture'\nversion='1.0.0'\nrust-version='1.74'\n")]
            for_ [(missingManifest, "Cargo archive lacks its effective Cargo.toml"), (invalidReadme, "Cargo publication text is not UTF-8")] $ \(archive, reason) ->
                withRoutedStub (targetReply "/registry") $ \stub -> do
                    let release = original{releaseArchive = archive, releaseChecksum = hexSha256Of archive}
                    doc <- cargoSourceVersion (cargoIndexRecord release)
                    withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
                        let transport = MirrorTransport exchanges (pure (Just (bareCredential (mkSecret "cargo-token")))) defaultLimits passthroughTracingPort
                            writer = newMirrorPublish transport (loopbackRegistryUrl (stubBaseUrl stub <> "/index")) (loopbackRegistryUrl (stubBaseUrl stub <> "/registry")) codec
                        publishOnlyFile writer doc (PublishPlan (pkgVersion (vdDetails doc))) archive `shouldReturn` Left (PublishSourceUnavailable reason)
                        allCaptured stub `shouldReturn` []
        it "refuses malformed source rows before archive preparation or a destination request" $ do
            release <- cargoPublicationFixture
            doc <- cargoSourceVersion (cargoIndexRecord release)
            case cargoIndexRecord release of
                Object fields -> for_ [(Object (KeyMap.delete "name" fields), "invalid Cargo field: name"), (Object (KeyMap.insert "deps" Null fields), "invalid Cargo field: deps"), (Null, "Cargo requires exactly one paired source row and archive")] $ \(row, reason) ->
                    withRoutedStub (targetReply "/registry") $ \stub ->
                        withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
                            let source = doc{vdRaw = Just (fst cargoCached (cargoDocument [(RecordEntry 7, row)]))}
                                transport = MirrorTransport exchanges (pure (Just (bareCredential (mkSecret "cargo-token")))) defaultLimits passthroughTracingPort
                                writer = newMirrorPublish transport (loopbackRegistryUrl (stubBaseUrl stub <> "/index")) (loopbackRegistryUrl (stubBaseUrl stub <> "/registry")) codec
                            publishOnlyFile writer source (PublishPlan (pkgVersion (vdDetails doc))) (releaseArchive release)
                                `shouldReturn` Left (PublishSourceUnavailable reason)
                            allCaptured stub `shouldReturn` []
                _ -> expectationFailure "fixture source row is not an object"
        it "requires durable source state even when a target has one complete archive" $ do
            release <- cargoPublicationFixture
            doc <- cargoSourceVersion (cargoIndexRecord release)
            let details = vdDetails doc
            pcNativeComplete codec (pkgName details) (pkgVersion details) (PackageInfo (pkgName details) (Map.singleton (renderVersion (pkgVersion details)) details) mempty []) `shouldBe` False
            isJust (durableMirrorDocuments (mirrorStateRequirement (publishMirrorRequirements (cargoPublish factors)))) `shouldBe` True
    describe "Cargo first-party claims" $ do
        it "refuses authorised-name disagreement and query or fragment API roots before sending a credential" $
            withRoutedStub (targetReply "/registry") $ \stub -> do
                release <- cargoPublicationFixture
                bytes <- expectRight (cargoUpload release)
                withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
                    let root = stubBaseUrl stub <> "/registry"
                        at target = originClient defaultLimits exchanges (loopbackRegistryUrl target) (Just (bareCredential (mkSecret "cargo-token")))
                        body = PublishRequestBody bytes Nothing NoPublishExtras
                    for_ [root <> "?query=secret", root <> "#fragment"] $ \target ->
                        publishRelay (cargoPublish factors) (at target) cargoTestName body
                            `shouldReturn` Left (FetchUrlUnformable (UnparseableUrl "Cargo API root must not contain a query or fragment"))
                    case cargoUploadMetadata release of
                        Object fields -> do
                            changed <- LBS.toStrict <$> expectRight (Body.encodeUpload (Object (KeyMap.insert "name" (String "another") fields)) (releaseArchive release))
                            publishRelay (cargoPublish factors) (at root) cargoTestName (PublishRequestBody changed Nothing NoPublishExtras)
                                `shouldReturn` Left (FetchUrlUnformable (UnparseableUrl "Cargo relay name differs from the authorised package"))
                        _ -> expectationFailure "fixture upload metadata is not an object"
                    allCaptured stub `shouldReturn` []
                    void (publishRelay (cargoPublish factors) (at root) cargoTestName body >>= expectRight)
                    captures <- allCaptured stub
                    map capMethod captures `shouldBe` ["PUT"]
                    map capBody captures `shouldBe` [bytes]
                    map (headerValue "Authorization") captures `shouldBe` [Just "cargo-token"]
        it "accepts matching complete upload metadata and archive identity" $ do
            release <- cargoPublicationFixture
            bytes <- expectRight (cargoUpload release)
            uploadClaims defaultLimits (const (pure ())) "https://proxy.test/cargo" (PublishRequestBody bytes Nothing NoPublishExtras) `shouldReturn` Right ["fixture"]
        it "compares complete resolver facts against the retained effective manifest" $ do
            archive <- cargoResolverArchive
            for_ cargoResolverAgreements $ \metadata -> do
                valid <- expectRight (cargoUploadWithMetadata metadata archive)
                uploadClaims defaultLimits (const (pure ())) "https://proxy.test/cargo" (PublishRequestBody valid Nothing NoPublishExtras) `shouldReturn` Right ["fixture"]
            for_ cargoResolverContradictions $ \metadata -> do
                bytes <- expectRight (cargoUploadWithMetadata metadata archive)
                Body.decodeUpload defaultLimits bytes `shouldSatisfy` isRight
                uploadClaims defaultLimits (const (pure ())) "https://proxy.test/cargo" (PublishRequestBody bytes Nothing NoPublishExtras) >>= (`shouldSatisfy` isLeft)
        it "reserves comparison workspace after archive inspection and before accepting resolver claims" $ do
            archive <- cargoResolverArchive
            bytes <- expectRight (cargoUploadWithMetadata cargoResolverMetadata archive)
            paid <- newIORef ([] :: [Int])
            uploadClaims defaultLimits (\cost -> modifyIORef' paid (<> [cost])) "https://proxy.test/cargo" (PublishRequestBody bytes Nothing NoPublishExtras) `shouldReturn` Right ["fixture"]
            payments <- readIORef paid
            case reverse payments of
                comparison : archiveCharge : _ -> comparison `shouldSatisfy` (> archiveCharge)
                _ -> expectationFailure "Cargo upload omitted its archive or comparison reservation"
        it "refuses unstable dependency fields before archive inspection" $ do
            archive <- cargoResolverArchive
            for_ ["artifact", "bindep_target", "lib", "public"] $ \key -> do
                bytes <- expectRight (cargoUploadWithMetadata (cargoResolverDependencyField key (Bool True)) archive)
                paid <- newIORef ([] :: [Int])
                refused <- uploadClaims defaultLimits (\cost -> modifyIORef' paid (<> [cost])) "https://proxy.test/cargo" (PublishRequestBody bytes Nothing NoPublishExtras)
                refused `shouldSatisfy` \case
                    Left (PublishMalformed fault) -> "Cargo upload dependency 'tool' carries an unstable" `T.isInfixOf` parseErrorMessage fault
                    _ -> False
                -- The metadata reservation is the only charge, so the archive was never inspected.
                length <$> readIORef paid `shouldReturn` 1
        it "refuses a dependency on another registry, naming it and the fix" $ do
            archive <- cargoForeignRegistryArchive
            bytes <- expectRight (cargoUploadWithMetadata cargoForeignRegistryMetadata archive)
            uploadClaims defaultLimits (const (pure ())) "https://proxy.test/cargo" (PublishRequestBody bytes Nothing NoPublishExtras)
                `shouldReturn` Left (PublishMalformed (ParseError cargoForeignRegistryRefusal))
        it "rejects malformed members before either claims or relay can contact the destination" $
            withRoutedStub (targetReply "/registry") $ \stub -> do
                release <- cargoPublicationFixture
                withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
                    let origin = originClient defaultLimits exchanges (loopbackRegistryUrl (stubBaseUrl stub <> "/registry")) (Just (bareCredential (mkSecret "cargo-token")))
                    case cargoUploadMetadata release of
                        Object fields -> for_ [("deps", Array (fromList [Null])), ("authors", Array (fromList [Number 42]))] $ \(key, value) -> do
                            bytes <- LBS.toStrict <$> expectRight (Body.encodeUpload (Object (KeyMap.insert key value fields)) (releaseArchive release))
                            let body = PublishRequestBody bytes Nothing NoPublishExtras
                            uploadClaims defaultLimits (const (pure ())) "https://proxy.test/cargo" body >>= (`shouldSatisfy` isLeft)
                            publishRelay (cargoPublish factors) origin cargoTestName body >>= (`shouldSatisfy` isLeft)
                        _ -> expectationFailure "fixture upload metadata is not an object"
                    allCaptured stub `shouldReturn` []
        it "rejects different archive name, version and build spelling before relay" $ do
            release <- cargoPublicationFixture
            case cargoUploadMetadata release of
                Object fields -> for_ [("name", "another"), ("vers", "2.0.0"), ("vers", "1.0.0+other")] $ \(key, value) -> do
                    bytes <- LBS.toStrict <$> expectRight (Body.encodeUpload (Object (KeyMap.insert key (String value) fields)) (releaseArchive release))
                    uploadClaims defaultLimits (const (pure ())) "https://proxy.test/cargo" (PublishRequestBody bytes Nothing NoPublishExtras) >>= (`shouldSatisfy` isLeft)
                _ -> expectationFailure "fixture upload metadata is not an object"
    describe "Cargo native response classification" $ do
        it "accepts native success objects and rejects errors even with a success status" $ do
            classifyPublish 200 "{}" `shouldBe` Right ()
            classifyPublish 200 "{\"warnings\":{},\"errors\":[]}" `shouldBe` Right ()
            for_ [(200, "{\"errors\":[{\"detail\":\"rejected\"}]}"), (200, "invalid"), (200, "[]"), (200, "{\"errors\":{}}"), (200, "{\"errors\":null}"), (400, "{\"errors\":[{\"detail\":\"already exists\"}]}"), (409, "{}"), (503, "{}")] $ \(status, body) ->
                classifyPublish status body `shouldBe` Left (PublishRejected (PublishError ("Cargo publication failed with HTTP status " <> show status)))

factors :: ChargeFactors
factors = ChargeFactors 1000 1000 1000

codec :: PublishCodec
codec = cargoPublishCodec factors

targetReply :: Text -> Captured -> (Status, ResponseHeaders, LByteString)
targetReply api captured =
    let root = selfBaseUrlOf (capHeaders captured)
        discovery = object ["dl" .= (root <> "/crates/{crate}/{version}/download"), "api" .= (root <> api), "auth-required" .= True]
     in (status200, [("Content-Type", "application/json")], if capMethod captured == "PUT" then "{}" else encode discovery)
