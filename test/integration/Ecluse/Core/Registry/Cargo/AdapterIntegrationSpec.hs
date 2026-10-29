-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cargo's sparse HTTP views retain native rows and keep archive reads inside their selected authority.
module Ecluse.Core.Registry.Cargo.AdapterIntegrationSpec (spec) where

import Data.Aeson (Value (..), encode, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.List (lookup)
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian, nominalDay)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (ResponseHeaders, Status, methodGet, methodHead, methodPut, status200, status304, status401, status403, status404, status503)
import Network.Wai (Application)
import Network.Wai qualified as Wai
import Network.Wai.Test (SResponse (..))
import Test.Hspec

import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Mirror.State (ProjectMirrorRecord, StateRead (..), StateTarget, extendProjectRecord, stateTarget)
import Ecluse.Core.Mirror.State.Internal (StateRevision (..), retainVerifiedIdentity)
import Ecluse.Core.Package (Artifact (..), Availability (Available), CodeExecSignal (CodeExecUnknown), HashAlg (SHA256), PackageDetails (..), PackageName, mkHash, mkPackageName)
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.Adapter (RegistryAdapter, adapterFor)
import Ecluse.Core.Registry.Cargo.Document (cargoDocument)
import Ecluse.Core.Registry.MirrorUpload (MirrorReadData (CargoMirrorRead), inventoryFiles, verifyMirrorBytes)
import Ecluse.Core.Registry.MirrorUpload.Internal (restoreMirrorInventory)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Rules.Types (Rule (AllowIfOlderThan))
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission.Meter (MemoryMeter, meterSnapshot, steerMeter, takeLargestCharge)
import Ecluse.Core.Server.Admission.Types (BrakeLevel (Calm), MeterSnapshot (snChargedBytes))
import Ecluse.Core.Server.Context (PackumentDeps (..), ReadSourceSelection (ManagedMirrorAndPublic, PrivateOnly))
import Ecluse.Core.Server.Upstream (MirrorServePlan (NoMirrorWrite))
import Ecluse.Core.Version (mkVersion)
import Ecluse.Runtime.Env (Env (envMemoryMeter))
import Ecluse.Runtime.Server (application, mkServerConfig)
import Ecluse.Server.Pipeline.ServingDriver (cargoServingDriver)
import Ecluse.Server.Pipeline.TestSupport (getPath, getPathWith, newTestEnvWithQueue, requestAt)
import Ecluse.Service (mountBindingFor)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Serving (ServingDriver (servingLayout), ServingLayout (SeparateRegistries), servingContract)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Mirror.State (mirrorViewWith)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Registry.Cargo.Fixtures qualified as Fixture
import Ecluse.Test.Registry.Cargo.Reference qualified as Reference
import Ecluse.Test.Rules (atDefaultPrecedence, inertRuleDeps)
import Ecluse.Test.Server.Mount (serveDepsFor, withPrivateBaseUrl)
import Ecluse.Test.Stub (Captured (..), allCaptured, headerValue, stubLocalhostUrl, withRoutedStub)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    it (caseLabel "integration/cargo/source-layout" "requires distinct public replacement and private alternate identities") $ do
        servingLayout cargoServingDriver `shouldBe` SeparateRegistries
        withCargoProxy defaultReply defaultReply $ \proxy -> do
            roots <- forM ["/cargo/config.json", "/cargo/private/config.json"] $ \path -> do
                response <- getPathWith [("Authorization", "caller")] path (proxyApp proxy)
                simpleStatus response `shouldBe` status200
                (dl, _, _, _) <- expectRight (Reference.referenceDiscovery (toStrict (simpleBody response)))
                pure (T.takeWhile (/= '{') dl)
            roots `shouldBe` ["http://ecluse.test/cargo/crates/", "http://ecluse.test/cargo/private/crates/"]
    servingContract "cargo" cargoServingDriver
    discoverySpec
    servingSpec
    isolationSpec
    admissionSpec

native :: Text -> Text -> IO () -> Spec
native number description = it (caseLabel ("integration/cargo/native-" <> number) description)

discoverySpec :: Spec
discoverySpec = describe "native sparse discovery" $ do
    native "001" "returns local archive locations without reading the public registry" $
        withCargoProxy defaultReply defaultReply $ \proxy -> do
            response <- getPath "/cargo/config.json" (proxyApp proxy)
            simpleStatus response `shouldBe` status200
            (dl, api, required, _) <- expectRight (Reference.referenceDiscovery (toStrict (simpleBody response)))
            dl `shouldBe` "http://ecluse.test/cargo/crates/{crate}/{version}/{crate}-{version}.crate"
            api `shouldBe` Nothing
            required `shouldBe` False
            publicCalls proxy `shouldReturn` []
            privateCalls proxy `shouldReturn` []
    native "008" "checks the edge credential before trusted discovery or data reads" $
        withCargoProxy defaultReply defaultReply $ \proxy -> do
            let secured = proxyWith proxy (\deps -> deps{pdInboundToken = Just (mkSecret "edge")}) (\deps -> deps{pdInboundToken = Just (mkSecret "edge")})
            for_ ["/cargo/config.json", "/cargo/private/config.json", publicIndex, privateIndex, publicArchive, privateArchive] $ \path -> do
                response <- getPath path secured
                simpleStatus response `shouldBe` status401
                lookup "WWW-Authenticate" (simpleHeaders response) `shouldBe` Just "Cargo"
            publicCalls proxy `shouldReturn` []
            privateCalls proxy `shouldReturn` []
    native "011" "preserves a trusted discovery 401 with the native credential challenge" $
        withCargoProxy (const (status401, [], "login")) defaultReply $ \proxy -> do
            response <- getPath "/cargo/private/config.json" (proxyApp proxy)
            simpleStatus response `shouldBe` status401
            lookup "WWW-Authenticate" (simpleHeaders response) `shouldBe` Just "Cargo"
            publicCalls proxy `shouldReturn` []
            map capPath <$> privateCalls proxy `shouldReturn` ["/index/config.json"]
    native "012" "keeps an actual trusted 403 distinct from unavailable discovery" $
        for_ [status403, status503] $ \status ->
            withCargoProxy (const (status, [], "unavailable")) defaultReply $ \proxy -> do
                response <- getPath "/cargo/private/config.json" (proxyApp proxy)
                simpleStatus response `shouldBe` status
                lookup "WWW-Authenticate" (simpleHeaders response) `shouldBe` Nothing
                publicCalls proxy `shouldReturn` []
    native "013" "discovers only the selected trusted source with the current raw token" $
        withCargoProxy defaultReply defaultReply $ \proxy -> do
            response <- getPathWith [("Authorization", "selected-token")] "/cargo/private/config.json" (proxyApp proxy)
            simpleStatus response `shouldBe` status200
            captured <- privateCalls proxy
            map capPath captured `shouldBe` ["/index/config.json"]
            map (headerValue "Authorization") captured `shouldBe` [Just "selected-token"]
            publicCalls proxy `shouldReturn` []
            let selectedMirror = proxyWith proxy (withPrivateBaseUrl (Just (loopbackRegistryUrl (privateBase proxy <> "/index")))) id
            mirror <- getPathWith [("Authorization", "mirror-token")] "/cargo/config.json" selectedMirror
            simpleStatus mirror `shouldBe` status200
            calls <- privateCalls proxy
            map capPath calls `shouldBe` ["/index/config.json", "/index/config.json"]
            map (headerValue "Authorization") calls `shouldBe` [Just "selected-token", Just "mirror-token"]
            publicCalls proxy `shouldReturn` []
    native "014" "copies the selected trusted authentication requirement into local discovery" $
        withCargoProxy authenticatedReply defaultReply $ \proxy -> do
            response <- getPathWith [("Authorization", "caller")] "/cargo/private/config.json" (proxyApp proxy)
            (dl, api, required, _) <- expectRight (Reference.referenceDiscovery (toStrict (simpleBody response)))
            dl `shouldBe` "http://ecluse.test/cargo/private/crates/{crate}/{version}/{crate}-{version}.crate"
            api `shouldBe` Nothing
            required `shouldBe` True
            lookup "Cache-Control" (simpleHeaders response) `shouldBe` Just "no-store"
    native "021" "refuses a download authority that the selected origin did not approve" $
        withCargoProxy unapprovedReply defaultReply $ \proxy -> do
            response <- getPath "/cargo/private/config.json" (proxyApp proxy)
            simpleStatus response `shouldBe` status503
            map capPath <$> privateCalls proxy `shouldReturn` ["/index/config.json"]
            publicCalls proxy `shouldReturn` []

servingSpec :: Spec
servingSpec = describe "native sparse serving" $ do
    native "002" "preserves every installation field in the exact admitted native rows" $
        withCargoProxy defaultReply defaultReply $ \proxy -> do
            response <- getPath publicIndex (proxyApp proxy)
            simpleStatus response `shouldBe` status200
            observedRows response `shouldReturn` [nativeRecord "1.2.3"]
            lookup "Content-Type" (simpleHeaders response) `shouldBe` Just "text/plain"
    native "003" "fetches the checksum-bound bytes through the archive location in local discovery" $
        withCargoProxy defaultReply defaultReply $ \proxy -> do
            discovery <- getPath "/cargo/config.json" (proxyApp proxy)
            (template, _, _, _) <- expectRight (Reference.referenceDiscovery (toStrict (simpleBody discovery)))
            template `shouldBe` "http://ecluse.test/cargo/crates/{crate}/{version}/{crate}-{version}.crate"
            listing <- getPath publicIndex (proxyApp proxy)
            fields <- expectRight . maybeToRight ("missing native row" :: Text) . listToMaybe =<< observedRows listing
            KeyMap.lookup "cksum" fields `shouldBe` Just (String (hexSha256Of archiveBytes))
            archive <- getPath publicArchive (proxyApp proxy)
            simpleStatus archive `shouldBe` status200
            toStrict (simpleBody archive) `shouldBe` archiveBytes
            hexSha256Of (toStrict (simpleBody archive)) `shouldBe` hexSha256Of archiveBytes
    native "004" "keeps metadata validators and archive HEAD coherent with GET" $
        withCargoProxy defaultReply defaultReply $ \proxy -> do
            let app = proxyApp proxy
            initial <- getPath publicIndex app
            tag <- maybe (fail "missing Cargo index validator") pure (lookup "ETag" (simpleHeaders initial))
            for_ [methodGet, methodHead] $ \method -> do
                matched <- requestAt method publicIndex Wai.defaultRequest{Wai.requestHeaders = [("If-None-Match", tag)]} app
                simpleStatus matched `shouldBe` status304
                simpleBody matched `shouldBe` ""
            headed <- requestAt methodHead publicArchive Wai.defaultRequest app
            simpleStatus headed `shouldBe` status200
            simpleBody headed `shouldBe` ""
            lookup "Content-Length" (simpleHeaders headed) `shouldBe` Just (show (BS.length archiveBytes))
    native "005" "checks the complete sparse prefix before either source receives a request" $
        withCargoProxy defaultReply defaultReply $ \proxy -> do
            for_ ["/cargo/th/in/Thing", "/cargo/TH/in/thing", "/cargo/thing", "/cargo/ot/he/thing"] $ \path -> do
                response <- getPath path (proxyApp proxy)
                simpleStatus response `shouldBe` status404
            publicCalls proxy `shouldReturn` []
            privateCalls proxy `shouldReturn` []
    native "006" "quarantines duplicate identities and malformed framing without discarding healthy siblings" $
        withCargoProxy defaultReply (indexReply (row "1.2.3+first" <> "\n{broken}\n" <> row "2.3.4" <> "\n" <> row "1.2.3+second" <> "\n{")) $ \proxy -> do
            response <- getPath publicIndex (proxyApp proxy)
            observedRows response `shouldReturn` [nativeRecord "2.3.4"]
    native "007" "does not emit unsupported schema rows beside an admitted record" $
        withCargoProxy defaultReply (indexReply (row "1.2.3" <> "\n" <> toStrict (encode (Object (KeyMap.insert "v" (Number 3) (nativeRecord "2.3.4")))))) $ \proxy -> do
            response <- getPath publicIndex (proxyApp proxy)
            observedRows response `shouldReturn` [nativeRecord "1.2.3"]
    native "009" "strips a raw Cargo token from all public metadata and archive requests" $
        withCargoProxy defaultReply defaultReply $ \proxy -> do
            for_ [publicIndex, publicArchive] $ \path -> do
                response <- getPathWith [("Authorization", "private-caller-token")] path (proxyApp proxy)
                simpleStatus response `shouldBe` status200
            calls <- publicCalls proxy
            map capPath calls `shouldSatisfy` elem "/index/config.json"
            map capPath calls `shouldSatisfy` elem "/index/th/in/thing"
            map capPath calls `shouldSatisfy` elem "/archives/thing/1.2.3"
            map (headerValue "Authorization") calls `shouldSatisfy` all isNothing
            privateCalls proxy `shouldReturn` []
    native "022" "refuses unsupported methods and conflicting archive coordinates before upstream reads" $
        withCargoProxy defaultReply defaultReply $ \proxy -> do
            response <- requestAt methodPut publicIndex Wai.defaultRequest (proxyApp proxy)
            simpleStatus response `shouldBe` status404
            for_ ["/cargo/crates/thing/1.2.3/other-1.2.3.crate", "/cargo/crates/thing/1.2.3/thing-2.3.4.crate"] $ \path -> do
                refused <- getPath path (proxyApp proxy)
                simpleStatus refused `shouldBe` status404
            publicCalls proxy `shouldReturn` []
            privateCalls proxy `shouldReturn` []

isolationSpec :: Spec
isolationSpec = describe "native source isolation" $ do
    native "010" "reauthorises private reads for each caller after the first caller fills caches" $
        for_ [status401, status403] $ \status ->
            withCargoProxy (callerReply status) defaultReply $ \proxy -> do
                authorised <- getPathWith [("Authorization", "allowed")] privateIndex (proxyApp proxy)
                simpleStatus authorised `shouldBe` status200
                for_ [privateIndex, privateArchive] $ \path -> do
                    refused <- getPathWith [("Authorization", "refused")] path (proxyApp proxy)
                    simpleStatus refused `shouldBe` status
                    lookup "WWW-Authenticate" (simpleHeaders refused) `shouldBe` ("Cargo" <$ guard (status == status401))
                calls <- privateCalls proxy
                map (headerValue "Authorization") calls `shouldSatisfy` elem (Just "refused")
                publicCalls proxy `shouldReturn` []
    native "015" "keeps declared first-party reads off the public replacement and public upstream" $
        withCargoProxy defaultReply defaultReply $ \proxy -> do
            let app = proxyWith proxy (\deps -> deps{pdFirstParty = const True}) (\deps -> deps{pdFirstParty = const True})
            for_ [publicIndex, publicArchive] $ \path -> do
                refused <- getPath path app
                simpleStatus refused `shouldBe` status404
            accepted <- getPath privateIndex app
            simpleStatus accepted `shouldBe` status200
            publicCalls proxy `shouldReturn` []
    native "016" "keeps an arbitrary private-only miss off the public fallback" $
        withCargoProxy (const (status404, [], "")) defaultReply $ \proxy -> do
            for_ [privateIndex, privateArchive] $ \path -> do
                response <- getPath path (proxyApp proxy)
                simpleStatus response `shouldBe` status404
            publicCalls proxy `shouldReturn` []
    native "017" "excludes unmanaged private rows when no project source state exists" $
        withCargoProxy defaultReply (const (status404, [], "")) $ \proxy -> do
            let target = stateTarget (privateBase proxy <> "/index")
            view <- mirrorViewWith target (\_ _ -> pure (Right StateAbsent))
            let app = proxyWith proxy (\deps -> (withPrivateBaseUrl (Just (loopbackRegistryUrl (privateBase proxy <> "/index"))) deps){pdMirrorState = Just view}) id
            listing <- getPath publicIndex app
            simpleStatus listing `shouldBe` status403
            archive <- getPath publicArchive app
            simpleStatus archive `shouldBe` status404
    native "026" "refuses unavailable public evidence without exposing unrecorded private data" $ do
        let unavailableIndex captured
                | capPath captured == "/index/th/in/thing" = (status503, [], "public index unavailable")
                | otherwise = defaultReply captured
        for_ [methodGet, methodHead] $ \method ->
            withCargoProxy defaultReply unavailableIndex $ \proxy -> do
                let target = stateTarget (privateBase proxy <> "/index")
                view <- mirrorViewWith target (\_ _ -> pure (Right StateAbsent))
                let app = proxyWith proxy (\deps -> (withPrivateBaseUrl (Just (loopbackRegistryUrl (privateBase proxy <> "/index"))) deps){pdMirrorState = Just view}) id
                for_ [publicIndex, publicArchive] $ \path -> do
                    response <- requestAt method path Wai.defaultRequest app
                    simpleStatus response `shouldBe` status503
                    let body = toStrict (simpleBody response)
                    BS.isInfixOf "\"vers\"" body `shouldBe` False
                    BS.isInfixOf archiveBytes body `shouldBe` False
                    when (method == methodHead) (simpleBody response `shouldBe` "")
                for_ [privateCalls proxy, publicCalls proxy] $ \captured -> do
                    requestPaths <- map capPath <$> captured
                    requestPaths `shouldContain` ["/index/config.json"]
                    requestPaths `shouldContain` ["/index/th/in/thing"]
                    requestPaths `shouldNotContain` ["/archives/thing/1.2.3"]
    native "018" "serves only completed source versions and excludes an unrecorded target sibling" $
        withCargoProxy (indexReply (row "1.2.3" <> "\n" <> row "2.3.4")) (const (status404, [], "")) $ \proxy -> do
            let target = stateTarget (privateBase proxy <> "/index")
            record <- nativeState target "2.3.4"
            view <- mirrorViewWith target (\_ _ -> pure (Right (StateLoaded (StateRevision target packageName "complete") record)))
            let app = proxyWith proxy (\deps -> (withPrivateBaseUrl (Just (loopbackRegistryUrl (privateBase proxy <> "/index"))) deps){pdMirrorState = Just view}) id
            listing <- getPath publicIndex app
            observedRows listing `shouldReturn` [nativeRecord "2.3.4"]
            archive <- getPath publicArchive app
            simpleStatus archive `shouldBe` status404
    native "019" "refuses a recorded version when the target checksum does not prove completion" $
        withCargoProxy (indexReply (toStrict (encode (Object (KeyMap.insert "cksum" (String (hexSha256Of "wrong target")) (nativeRecord "1.2.3")))))) (const (status404, [], "")) $ \proxy -> do
            let target = stateTarget (privateBase proxy <> "/index")
            record <- nativeState target "1.2.3"
            view <- mirrorViewWith target (\_ _ -> pure (Right (StateLoaded (StateRevision target packageName "incomplete") record)))
            let app = proxyWith proxy (\deps -> (withPrivateBaseUrl (Just (loopbackRegistryUrl (privateBase proxy <> "/index"))) deps){pdMirrorState = Just view}) id
            for_ [publicIndex, publicArchive] $ \path -> do
                response <- getPath path app
                simpleStatus response `shouldBe` status503
    native "020" "partitions private and public validators and rejects a validator from another view" $
        withCargoProxy (indexReply (toStrict (encode (Object (KeyMap.insert "cksum" (String (hexSha256Of "different")) (nativeRecord "1.2.3")))))) defaultReply $ \proxy -> do
            public <- getPath publicIndex (proxyApp proxy)
            private <- getPath privateIndex (proxyApp proxy)
            publicTag <- maybe (fail "missing public validator") pure (lookup "ETag" (simpleHeaders public))
            privateTag <- maybe (fail "missing private validator") pure (lookup "ETag" (simpleHeaders private))
            privateTag `shouldNotBe` publicTag
            crossed <- getPathWith [("If-None-Match", publicTag)] privateIndex (proxyApp proxy)
            simpleStatus crossed `shouldBe` status200
            lookup "ETag" (simpleHeaders crossed) `shouldBe` Just privateTag
            rows <- observedRows crossed
            rows `shouldNotBe` [nativeRecord "1.2.3"]

data CargoProxy = CargoProxy
    { proxyApp :: Application
    , proxyWith :: (PackumentDeps -> PackumentDeps) -> (PackumentDeps -> PackumentDeps) -> Application
    , publicCalls :: IO [Captured]
    , privateCalls :: IO [Captured]
    , privateBase :: Text
    , proxyMemory :: MemoryMeter
    }

type Reply = Captured -> (Status, ResponseHeaders, LByteString)

withCargoProxy :: Reply -> Reply -> (CargoProxy -> IO a) -> IO a
withCargoProxy privateReply publicReply action =
    withRoutedStub privateReply $ \private -> withRoutedStub publicReply $ \public -> withSweeper $ \deadlines -> do
        adapter <- requiredAdapter
        manager <- newManager defaultManagerSettings
        queue <- newTestMemoryQueue
        env <- newTestEnvWithQueue deadlines queue manager
        rules <- prepare inertRuleDeps [atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))]
        let publicDeps =
                (serveDepsFor adapter Nothing (loopbackRegistryUrl (stubLocalhostUrl public <> "/index")) NoMirrorWrite rules (pure now))
                    { pdReadSourceSelection = ManagedMirrorAndPublic
                    , pdMountBaseUrl = "http://ecluse.test/cargo"
                    , pdEgressUrl = Right . loopbackRegistryUrl
                    }
            privateDeps =
                publicDeps
                    { pdReadSourceSelection = PrivateOnly
                    , pdMountBaseUrl = "http://ecluse.test/cargo/private"
                    }
            trusted = withPrivateBaseUrl (Just (loopbackRegistryUrl (stubLocalhostUrl private <> "/index"))) privateDeps
            build configurePublic configurePrivate =
                application (mkServerConfig (maybeToList (mountBindingFor Cargo (configurePublic publicDeps) (Just (configurePrivate trusted)) Nothing))) env
        action (CargoProxy (build id id) build (allCaptured public) (allCaptured private) (stubLocalhostUrl private) (envMemoryMeter env))
  where
    now = UTCTime (fromGregorian 2026 6 20) 0

requiredAdapter :: IO RegistryAdapter
requiredAdapter = maybe (fail "Cargo has no production adapter registration") pure (adapterFor Cargo)

publicIndex, privateIndex, publicArchive, privateArchive :: ByteString
publicIndex = "/cargo/th/in/thing"
privateIndex = "/cargo/private/th/in/thing"
publicArchive = "/cargo/crates/thing/1.2.3/thing-1.2.3.crate"
privateArchive = "/cargo/private/crates/thing/1.2.3/thing-1.2.3.crate"

archiveBytes :: ByteString
archiveBytes = "Owned Cargo metadata fixture bytes"

nativeRecord :: Text -> KeyMap.KeyMap Value
nativeRecord version = case Fixture.cargoIndexRecord (Fixture.minimalCargoRelease "thing" version) of
    Object fields -> KeyMap.union installation fields
    _ -> installation
  where
    installation =
        KeyMap.fromList
            [ ("deps", toListValue [object ["name" .= ("renamed_leaf" :: Text), "package" .= ("native-leaf" :: Text), "req" .= ("^1.0" :: Text), "features" .= ["extra" :: Text], "optional" .= True, "default_features" .= False, "target" .= ("cfg(unix)" :: Text), "kind" .= ("normal" :: Text), "registry" .= ("sparse+https://index.crates.io/" :: Text)]])
            , ("features", object ["default" .= ["feature" :: Text], "feature" .= ([] :: [Text])])
            , ("features2", object ["optional" .= ["dep:renamed_leaf" :: Text, "renamed_leaf?/extra"]])
            , ("rust_version", String "1.74")
            ]
    toListValue = Array . fromList

row :: Text -> ByteString
row = toStrict . encode . Object . nativeRecord

observedRows :: SResponse -> IO [KeyMap.KeyMap Value]
observedRows response = do
    simpleStatus response `shouldBe` status200
    traverse (expectRight . Reference.indexValue) (Reference.referenceIndex (toStrict (simpleBody response)))

defaultReply :: Reply
defaultReply captured
    | capPath captured == "/index/config.json" = (status200, [], toLazy (discoveryBytes (captureBase captured <> "/archives/{crate}/{version}") False))
    | capPath captured == "/index/th/in/thing" = (status200, [], toLazy (row "1.2.3"))
    | capPath captured == "/archives/thing/1.2.3" = (status200, [("Content-Length", show (BS.length archiveBytes))], toLazy archiveBytes)
    | otherwise = (status404, [], "")

captureBase :: Captured -> Text
captureBase captured = "http://" <> maybe "" decodeUtf8 (headerValue "Host" captured)

indexReply :: ByteString -> Reply
indexReply bytes captured
    | capPath captured == "/index/th/in/thing" = (status200, [], toLazy bytes)
    | otherwise = defaultReply captured

callerReply :: Status -> Reply
callerReply refusedStatus captured
    | headerValue "Authorization" captured == Just "allowed" = defaultReply captured
    | otherwise = (refusedStatus, [], "caller refused")

authenticatedReply :: Reply
authenticatedReply captured
    | capPath captured == "/index/config.json" = (status200, [], toLazy (discoveryBytes (captureBase captured <> "/archives/{crate}/{version}") True))
    | otherwise = defaultReply captured

unapprovedReply :: Reply
unapprovedReply _ = (status200, [], "{\"dl\":\"https://unapproved.example.test/{crate}/{version}\"}")

packageName :: PackageName
packageName = mkPackageName Cargo Nothing "thing"

nativeState :: StateTarget -> Text -> IO ProjectMirrorRecord
nativeState target version = do
    checksum <- expectRight (mkHash SHA256 (hexSha256Of archiveBytes))
    let artifact = Artifact (RecordEntry 0) ("thing-" <> version <> ".crate") ("https://static.crates.io/crates/thing/thing-" <> version <> ".crate") [checksum] Nothing Nothing
        details = PackageDetails packageName (mkVersion Cargo version) (Just (UTCTime (fromGregorian 2020 1 1) 0)) CodeExecUnknown Available (artifact :| [])
        source = CargoMirrorRead (cargoDocument [(RecordEntry 0, Object (nativeRecord version))])
    inventory <- expectRight (restoreMirrorInventory details source)
    expected <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
    let file :| _ = inventoryFiles inventory
    verified <- expectRight (verifyMirrorBytes file archiveBytes)
    expectRight (retainVerifiedIdentity (mkVersion Cargo version) verified expected)

discoveryBytes :: Text -> Bool -> ByteString
discoveryBytes dl authenticated = toStrict (encode (object ["dl" .= dl, "auth-required" .= authenticated]))

admissionSpec :: Spec
admissionSpec = describe "Cargo selected admission" $ do
    native "023" "admits private archive selection before discovery or index allocation" $
        withCargoProxy defaultReply defaultReply $ \proxy -> do
            steerMeter (proxyMemory proxy) 0 Calm
            response <- getPath privateArchive (proxyApp proxy)
            simpleStatus response `shouldBe` status503
            privateCalls proxy `shouldReturn` []
            publicCalls proxy `shouldReturn` []
    native "024" "charges growing selected and unrelated rows for public and caller-authorised archives" $
        for_ [publicArchive, privateArchive] $ \path ->
            for_ ["1.2.3", "2.0.0"] $ \version -> do
                peaks <- forM [4096, 2 * 1024 * 1024] $ \size -> do
                    let large = Object (KeyMap.insert "features" (object ["large" .= [T.replicate size "x"]]) (nativeRecord version))
                        body = if version == "1.2.3" then toStrict (encode large) else row "1.2.3" <> "\n" <> toStrict (encode large)
                    withCargoProxy (indexReply body) (indexReply body) $ \proxy -> do
                        response <- getPathWith [("Authorization", "caller-token")] path (proxyApp proxy)
                        simpleStatus response `shouldBe` status200
                        toStrict (simpleBody response) `shouldBe` archiveBytes
                        calls <- if path == privateArchive then privateCalls proxy else publicCalls proxy
                        map (headerValue "Authorization") calls `shouldBe` replicate (length calls) (if path == privateArchive then Just "caller-token" else Nothing)
                        snChargedBytes <$> meterSnapshot (proxyMemory proxy) `shouldReturn` 0
                        peak <- takeLargestCharge (proxyMemory proxy)
                        peak `shouldSatisfy` (>= 12 * size)
                        pure peak
                zipWith (<) peaks (drop 1 peaks) `shouldBe` [True]

    native "025" "keeps caller-authorised PrivateOnly selection independent of managed public state" $
        withCargoProxy defaultReply defaultReply $ \proxy -> do
            stateReads <- newIORef (0 :: Int)
            let target = stateTarget (privateBase proxy <> "/index")
            mirror <- mirrorViewWith target (\_ _ -> modifyIORef' stateReads (+ 1) $> Right StateAbsent)
            let app = proxyWith proxy id (\deps -> deps{pdMirrorState = Just mirror})
            response <- getPathWith [("Authorization", "caller-token")] privateArchive app
            simpleStatus response `shouldBe` status200
            toStrict (simpleBody response) `shouldBe` archiveBytes
            readIORef stateReads `shouldReturn` 0
            publicCalls proxy `shouldReturn` []
