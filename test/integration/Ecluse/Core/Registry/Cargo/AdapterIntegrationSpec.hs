-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cargo's merged sparse registry retains native rows and keeps archive reads inside their selected authority.
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
import Ecluse.Core.Mirror.State (ProjectMirrorRecord, StateRead (..), StateTarget, stateTarget)
import Ecluse.Core.Mirror.State.Internal (StateRevision (..))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Registry.Cargo.Mirror (cargoMirrorDocuments)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Rules.Types (Rule (AllowIfOlderThan))
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission.Meter (MemoryMeter, meterSnapshot, steerMeter, takeLargestCharge)
import Ecluse.Core.Server.Admission.Types (BrakeLevel (Calm), MeterSnapshot (snChargedBytes))
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Core.Server.Upstream (MirrorServePlan (NoMirrorWrite))
import Ecluse.Runtime.Env (Env (envMemoryMeter))
import Ecluse.Server.Pipeline.ServingDriver (cargoServingDriver)
import Ecluse.Server.Pipeline.TestSupport (getPath, getPathWith, newTestEnvWithQueue, proxyApplication, requestAt)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Serving (servingContract)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Mirror.State (cargoMirrorRecord, mirrorViewWith)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Registry.Cargo.Fixtures qualified as Fixture
import Ecluse.Test.Registry.Cargo.Reference qualified as Reference
import Ecluse.Test.Rules (atDefaultPrecedence, inertRuleDeps)
import Ecluse.Test.Server.Mount (requiredAdapter, serveDepsFor, withPrivateBaseUrl)
import Ecluse.Test.Stub (Captured (..), allCaptured, headerValue, stubLocalhostUrl, withRoutedStub)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    it (caseLabel "integration/cargo/source-layout" "serves one merged registry and leaves the removed private prefix unrouted") $
        withCargoProxy defaultReply defaultReply $ \proxy -> do
            response <- getPathWith [("Authorization", "caller")] "/cargo/config.json" (proxyApp proxy)
            simpleStatus response `shouldBe` status200
            (dl, _, _, _) <- expectRight (Reference.referenceDiscovery (toStrict (simpleBody response)))
            T.takeWhile (/= '{') dl `shouldBe` "http://ecluse.test/cargo/crates/"
            removed <- getPathWith [("Authorization", "caller")] "/cargo/private/config.json" (proxyApp proxy)
            simpleStatus removed `shouldBe` status404
            privateCalls proxy `shouldReturn` []
            publicCalls proxy `shouldReturn` []
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
            response <- getPath "/cargo/config.json" (publicOnly proxy)
            simpleStatus response `shouldBe` status200
            (dl, api, required, _) <- expectRight (Reference.referenceDiscovery (toStrict (simpleBody response)))
            dl `shouldBe` "http://ecluse.test/cargo/crates/{crate}/{version}/{crate}-{version}.crate"
            api `shouldBe` Nothing
            required `shouldBe` False
            publicCalls proxy `shouldReturn` []
            privateCalls proxy `shouldReturn` []
    native "008" "checks the edge credential before trusted discovery or data reads" $
        withCargoProxy defaultReply defaultReply $ \proxy -> do
            let secured = proxyWith proxy (\deps -> deps{pdInboundToken = Just (mkSecret "edge")})
            for_ ["/cargo/config.json", indexPath, archivePath] $ \path -> do
                response <- getPath path secured
                simpleStatus response `shouldBe` status401
                lookup "WWW-Authenticate" (simpleHeaders response) `shouldBe` Just "Cargo"
            publicCalls proxy `shouldReturn` []
            privateCalls proxy `shouldReturn` []
    native "011" "answers discovery locally with authentication required when a private upstream is configured" $
        for_ [defaultReply, authenticatedReply, const (status401, [], "login"), const (status403, [], "refused"), unapprovedReply] $ \privateReply ->
            withCargoProxy privateReply defaultReply $ \proxy ->
                for_ [[], [("Authorization", "caller")]] $ \headers -> do
                    response <- getPathWith headers "/cargo/config.json" (proxyApp proxy)
                    expectLocalDiscovery Nothing True response
                    privateCalls proxy `shouldReturn` []
                    publicCalls proxy `shouldReturn` []
    native "012" "keeps discovery available while the private upstream fails" $
        withCargoProxy (const (status503, [], "unavailable")) defaultReply $ \proxy -> do
            response <- getPath "/cargo/config.json" (proxyApp proxy)
            expectLocalDiscovery Nothing True response
            privateCalls proxy `shouldReturn` []
            publicCalls proxy `shouldReturn` []
    native "013" "requires authentication in discovery when only the edge token is configured" $
        withCargoProxy defaultReply defaultReply $ \proxy -> do
            let edgeOnly = proxyWith proxy (\deps -> (withPrivateBaseUrl Nothing deps){pdInboundToken = Just (mkSecret "edge")})
            response <- getPathWith [("Authorization", "edge")] "/cargo/config.json" edgeOnly
            expectLocalDiscovery Nothing True response
            privateCalls proxy `shouldReturn` []
            publicCalls proxy `shouldReturn` []
    native "014" "omits the publication API from discovery when publication is not configured" $
        withCargoProxy authenticatedReply defaultReply $ \proxy -> do
            response <- getPathWith [("Authorization", "caller")] "/cargo/config.json" (proxyApp proxy)
            (_, api, _, _) <- expectRight (Reference.referenceDiscovery (toStrict (simpleBody response)))
            api `shouldBe` Nothing
            privateCalls proxy `shouldReturn` []
    native "021" "answers 404 under the removed private prefix without upstream reads" $
        withCargoProxy defaultReply defaultReply $ \proxy -> do
            for_ ["/cargo/private/config.json", "/cargo/private/th/in/thing", "/cargo/private/crates/thing/1.2.3/thing-1.2.3.crate"] $ \path -> do
                response <- getPathWith [("Authorization", "caller")] path (proxyApp proxy)
                simpleStatus response `shouldBe` status404
            publication <- requestAt methodPut "/cargo/private/api/v1/crates/new" Wai.defaultRequest (proxyApp proxy)
            simpleStatus publication `shouldBe` status404
            privateCalls proxy `shouldReturn` []
            publicCalls proxy `shouldReturn` []

expectLocalDiscovery :: Maybe Text -> Bool -> SResponse -> IO ()
expectLocalDiscovery expectedApi expectedAuth response = do
    simpleStatus response `shouldBe` status200
    (dl, api, required, _) <- expectRight (Reference.referenceDiscovery (toStrict (simpleBody response)))
    dl `shouldBe` "http://ecluse.test/cargo/crates/{crate}/{version}/{crate}-{version}.crate"
    api `shouldBe` expectedApi
    required `shouldBe` expectedAuth
    lookup "Cache-Control" (simpleHeaders response) `shouldBe` Just "no-store"

servingSpec :: Spec
servingSpec = describe "native sparse serving" $ do
    native "002" "preserves every installation field in the exact admitted native rows" $
        withCargoProxy defaultReply defaultReply $ \proxy -> do
            response <- getPath indexPath (publicOnly proxy)
            simpleStatus response `shouldBe` status200
            observedRows response `shouldReturn` [nativeRecord "1.2.3"]
            lookup "Content-Type" (simpleHeaders response) `shouldBe` Just "text/plain"
    native "003" "fetches the checksum-bound bytes through the archive location in local discovery" $
        withCargoProxy defaultReply defaultReply $ \proxy -> do
            let app = publicOnly proxy
            discovery <- getPath "/cargo/config.json" app
            (template, _, _, _) <- expectRight (Reference.referenceDiscovery (toStrict (simpleBody discovery)))
            template `shouldBe` "http://ecluse.test/cargo/crates/{crate}/{version}/{crate}-{version}.crate"
            listing <- getPath indexPath app
            fields <- expectRight . maybeToRight ("missing native row" :: Text) . listToMaybe =<< observedRows listing
            KeyMap.lookup "cksum" fields `shouldBe` Just (String (hexSha256Of archiveBytes))
            archive <- getPath archivePath app
            simpleStatus archive `shouldBe` status200
            toStrict (simpleBody archive) `shouldBe` archiveBytes
            hexSha256Of (toStrict (simpleBody archive)) `shouldBe` hexSha256Of archiveBytes
    native "004" "keeps metadata validators and archive HEAD coherent with GET" $
        withCargoProxy defaultReply defaultReply $ \proxy -> do
            let app = publicOnly proxy
            initial <- getPath indexPath app
            tag <- maybe (fail "missing Cargo index validator") pure (lookup "ETag" (simpleHeaders initial))
            for_ [methodGet, methodHead] $ \method -> do
                matched <- requestAt method indexPath Wai.defaultRequest{Wai.requestHeaders = [("If-None-Match", tag)]} app
                simpleStatus matched `shouldBe` status304
                simpleBody matched `shouldBe` ""
            headed <- requestAt methodHead archivePath Wai.defaultRequest app
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
            response <- getPath indexPath (publicOnly proxy)
            observedRows response `shouldReturn` [nativeRecord "2.3.4"]
    native "007" "does not emit unsupported schema rows beside an admitted record" $
        withCargoProxy defaultReply (indexReply (row "1.2.3" <> "\n" <> toStrict (encode (Object (KeyMap.insert "v" (Number 3) (nativeRecord "2.3.4")))))) $ \proxy -> do
            response <- getPath indexPath (publicOnly proxy)
            observedRows response `shouldReturn` [nativeRecord "1.2.3"]
    native "009" "strips a raw Cargo token from all public metadata and archive requests" $
        withCargoProxy defaultReply defaultReply $ \proxy -> do
            for_ [indexPath, archivePath] $ \path -> do
                response <- getPathWith [("Authorization", "private-caller-token")] path (publicOnly proxy)
                simpleStatus response `shouldBe` status200
            calls <- publicCalls proxy
            map capPath calls `shouldSatisfy` elem "/index/config.json"
            map capPath calls `shouldSatisfy` elem "/index/th/in/thing"
            map capPath calls `shouldSatisfy` elem "/archives/thing/1.2.3"
            map (headerValue "Authorization") calls `shouldSatisfy` all isNothing
            privateCalls proxy `shouldReturn` []
    native "022" "refuses unsupported methods and conflicting archive coordinates before upstream reads" $
        withCargoProxy defaultReply defaultReply $ \proxy -> do
            response <- requestAt methodPut indexPath Wai.defaultRequest (proxyApp proxy)
            simpleStatus response `shouldBe` status404
            for_ ["/cargo/crates/thing/1.2.3/other-1.2.3.crate", "/cargo/crates/thing/1.2.3/thing-2.3.4.crate"] $ \path -> do
                refused <- getPath path (proxyApp proxy)
                simpleStatus refused `shouldBe` status404
            publicCalls proxy `shouldReturn` []
            privateCalls proxy `shouldReturn` []
    native "027" "serves one row when private and public versions differ only in build metadata" $ do
        withCargoProxy (indexReply (row "1.2.3+corp")) (indexReply (row "1.2.3" <> "\n" <> row "1.2.4")) $ \proxy -> do
            response <- getPathWith [("Authorization", "caller")] indexPath (proxyApp proxy)
            rows <- observedRows response
            rows `shouldMatchList` [nativeRecord "1.2.3+corp", nativeRecord "1.2.4"]
        withCargoProxy (indexReply (row "1.2.3")) (indexReply (row "1.2.3+build")) $ \proxy -> do
            response <- getPathWith [("Authorization", "caller")] indexPath (proxyApp proxy)
            observedRows response `shouldReturn` [nativeRecord "1.2.3"]

isolationSpec :: Spec
isolationSpec = describe "native source isolation" $ do
    native "010" "reauthorises private reads for each caller after the first caller fills caches" $
        for_ [status401, status403] $ \status ->
            withCargoProxy (callerReply status) defaultReply $ \proxy -> do
                authorised <- getPathWith [("Authorization", "allowed")] indexPath (proxyApp proxy)
                simpleStatus authorised `shouldBe` status200
                for_ [indexPath, archivePath] $ \path -> do
                    refused <- getPathWith [("Authorization", "refused")] path (proxyApp proxy)
                    simpleStatus refused `shouldBe` status
                    lookup "WWW-Authenticate" (simpleHeaders refused) `shouldBe` ("Cargo" <$ guard (status == status401))
                calls <- privateCalls proxy
                map (headerValue "Authorization") calls `shouldSatisfy` elem (Just "refused")
                publicTokens <- map (headerValue "Authorization") <$> publicCalls proxy
                publicTokens `shouldSatisfy` all isNothing
    native "015" "serves declared first-party names from the private origin alone" $ do
        withCargoProxy defaultReply defaultReply $ \proxy -> do
            let app = proxyWith proxy (\deps -> deps{pdFirstParty = const True})
            listing <- getPathWith [("Authorization", "caller")] indexPath app
            observedRows listing `shouldReturn` [nativeRecord "1.2.3"]
            archive <- getPathWith [("Authorization", "caller")] archivePath app
            simpleStatus archive `shouldBe` status200
            toStrict (simpleBody archive) `shouldBe` archiveBytes
            publicCalls proxy `shouldReturn` []
        withCargoProxy privateMiss defaultReply $ \proxy -> do
            let app = proxyWith proxy (\deps -> deps{pdFirstParty = const True})
            for_ [indexPath, archivePath] $ \path -> do
                refused <- getPathWith [("Authorization", "caller")] path app
                simpleStatus refused `shouldBe` status404
            publicCalls proxy `shouldReturn` []
    native "016" "falls back to admitted public rows and archives after a private miss for an undeclared name" $
        withCargoProxy privateMiss defaultReply $ \proxy -> do
            listing <- getPathWith [("Authorization", "caller")] indexPath (proxyApp proxy)
            observedRows listing `shouldReturn` [nativeRecord "1.2.3"]
            archive <- getPathWith [("Authorization", "caller")] archivePath (proxyApp proxy)
            simpleStatus archive `shouldBe` status200
            toStrict (simpleBody archive) `shouldBe` archiveBytes
            publicCalls proxy >>= (`shouldSatisfy` (not . null))
    native "017" "serves unrecorded private rows as trusted content when no project source state exists" $
        withCargoProxy defaultReply (const (status404, [], "")) $ \proxy -> do
            view <- mirrorViewWith cargoMirrorDocuments (proxyTarget proxy) (\_ _ -> pure (Right StateAbsent))
            let app = proxyWith proxy (\deps -> deps{pdMirrorState = Just view})
            listing <- getPath indexPath app
            observedRows listing `shouldReturn` [nativeRecord "1.2.3"]
            archive <- getPath archivePath app
            simpleStatus archive `shouldBe` status200
            toStrict (simpleBody archive) `shouldBe` archiveBytes
    native "026" "serves unrecorded private rows while public evidence is unavailable" $ do
        let unavailableIndex captured
                | capPath captured == "/index/th/in/thing" = (status503, [], "public index unavailable")
                | otherwise = defaultReply captured
        for_ [methodGet, methodHead] $ \method ->
            withCargoProxy defaultReply unavailableIndex $ \proxy -> do
                view <- mirrorViewWith cargoMirrorDocuments (proxyTarget proxy) (\_ _ -> pure (Right StateAbsent))
                let app = proxyWith proxy (\deps -> deps{pdMirrorState = Just view})
                listing <- requestAt method indexPath Wai.defaultRequest app
                simpleStatus listing `shouldBe` status200
                archive <- requestAt method archivePath Wai.defaultRequest app
                simpleStatus archive `shouldBe` status200
                if method == methodHead
                    then map simpleBody [listing, archive] `shouldBe` ["", ""]
                    else do
                        observedRows listing `shouldReturn` [nativeRecord "1.2.3"]
                        toStrict (simpleBody archive) `shouldBe` archiveBytes
    native "018" "serves a completed managed version beside an unrecorded private sibling" $
        withCargoProxy (indexReply (row "1.2.3" <> "\n" <> row "2.3.4")) (const (status404, [], "")) $ \proxy -> do
            record <- nativeState (proxyTarget proxy) "2.3.4"
            view <- mirrorViewWith cargoMirrorDocuments (proxyTarget proxy) (\_ _ -> pure (Right (StateLoaded (StateRevision (proxyTarget proxy) packageName "complete") record)))
            let app = proxyWith proxy (\deps -> deps{pdMirrorState = Just view})
            listing <- getPath indexPath app
            rows <- observedRows listing
            rows `shouldMatchList` [nativeRecord "1.2.3", nativeRecord "2.3.4"]
            archive <- getPath archivePath app
            simpleStatus archive `shouldBe` status200
            toStrict (simpleBody archive) `shouldBe` archiveBytes
    native "019" "refuses a recorded version when the target checksum does not prove completion" $
        withCargoProxy (indexReply (toStrict (encode (Object (KeyMap.insert "cksum" (String (hexSha256Of "wrong target")) (nativeRecord "1.2.3")))))) (const (status404, [], "")) $ \proxy -> do
            record <- nativeState (proxyTarget proxy) "1.2.3"
            view <- mirrorViewWith cargoMirrorDocuments (proxyTarget proxy) (\_ _ -> pure (Right (StateLoaded (StateRevision (proxyTarget proxy) packageName "incomplete") record)))
            let app = proxyWith proxy (\deps -> deps{pdMirrorState = Just view})
            for_ [indexPath, archivePath] $ \path -> do
                response <- getPath path app
                simpleStatus response `shouldBe` status503

data CargoProxy = CargoProxy
    { proxyApp :: Application
    , proxyWith :: (PackumentDeps -> PackumentDeps) -> Application
    , publicCalls :: IO [Captured]
    , privateCalls :: IO [Captured]
    , privateBase :: Text
    , proxyMemory :: MemoryMeter
    }

type Reply = Captured -> (Status, ResponseHeaders, LByteString)

-- One merged mount: the private stub is the private upstream and the public stub is crates.io.
withCargoProxy :: Reply -> Reply -> (CargoProxy -> IO a) -> IO a
withCargoProxy privateReply publicReply action =
    withRoutedStub privateReply $ \private -> withRoutedStub publicReply $ \public -> withSweeper $ \deadlines -> do
        adapter <- requiredAdapter Cargo
        manager <- newManager defaultManagerSettings
        queue <- newTestMemoryQueue
        env <- newTestEnvWithQueue deadlines queue manager
        rules <- prepare inertRuleDeps [atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))]
        let deps =
                (serveDepsFor adapter (Just (loopbackRegistryUrl (stubLocalhostUrl private <> "/index"))) (loopbackRegistryUrl (stubLocalhostUrl public <> "/index")) NoMirrorWrite rules (pure now))
                    { pdMountBaseUrl = "http://ecluse.test/cargo"
                    , pdEgressUrl = Right . loopbackRegistryUrl
                    }
            build configure = proxyApplication Cargo env (configure deps)
        action (CargoProxy (build id) build (allCaptured public) (allCaptured private) (stubLocalhostUrl private) (envMemoryMeter env))
  where
    now = UTCTime (fromGregorian 2026 6 20) 0

-- The same mount with no private upstream, which reads and discovers like a public-only deployment.
publicOnly :: CargoProxy -> Application
publicOnly proxy = proxyWith proxy (withPrivateBaseUrl Nothing)

-- Durable mirror state is keyed by the private upstream that aggregates the mirror target.
proxyTarget :: CargoProxy -> StateTarget
proxyTarget proxy = stateTarget (privateBase proxy <> "/index")

indexPath, archivePath :: ByteString
indexPath = "/cargo/th/in/thing"
archivePath = "/cargo/crates/thing/1.2.3/thing-1.2.3.crate"

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

-- A settled private miss: discovery succeeds and every record or archive is absent.
privateMiss :: Reply
privateMiss captured
    | capPath captured == "/index/config.json" = defaultReply captured
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
nativeState target version = cargoMirrorRecord target packageName archiveBytes [(0, Object (nativeRecord version))]

discoveryBytes :: Text -> Bool -> ByteString
discoveryBytes dl authenticated = toStrict (encode (object ["dl" .= dl, "auth-required" .= authenticated]))

admissionSpec :: Spec
admissionSpec = describe "Cargo selected admission" $ do
    native "023" "admits private archive selection before discovery or index allocation" $
        withCargoProxy defaultReply defaultReply $ \proxy -> do
            steerMeter (proxyMemory proxy) 0 Calm
            response <- getPath archivePath (proxyApp proxy)
            simpleStatus response `shouldBe` status503
            privateCalls proxy `shouldReturn` []
            publicCalls proxy `shouldReturn` []
    native "024" "charges growing selected and unrelated rows for public and caller-authorised archives" $
        for_ [True, False] $ \privateSelected ->
            for_ ["1.2.3", "2.0.0"] $ \version -> do
                peaks <- forM [4096, 2 * 1024 * 1024] $ \size -> do
                    let large = Object (KeyMap.insert "features" (object ["large" .= [T.replicate size "x"]]) (nativeRecord version))
                        body = if version == "1.2.3" then toStrict (encode large) else row "1.2.3" <> "\n" <> toStrict (encode large)
                        (privateReply, publicReply) = if privateSelected then (indexReply body, defaultReply) else (privateMiss, indexReply body)
                    withCargoProxy privateReply publicReply $ \proxy -> do
                        response <- getPathWith [("Authorization", "caller-token")] archivePath (proxyApp proxy)
                        simpleStatus response `shouldBe` status200
                        toStrict (simpleBody response) `shouldBe` archiveBytes
                        calls <- if privateSelected then privateCalls proxy else publicCalls proxy
                        map (headerValue "Authorization") calls `shouldBe` replicate (length calls) (if privateSelected then Just "caller-token" else Nothing)
                        snChargedBytes <$> meterSnapshot (proxyMemory proxy) `shouldReturn` 0
                        peak <- takeLargestCharge (proxyMemory proxy)
                        peak `shouldSatisfy` (>= 12 * size)
                        pure peak
                zipWith (<) peaks (drop 1 peaks) `shouldBe` [True]
    native "025" "reads no durable mirror state for a declared first-party name" $
        withCargoProxy defaultReply defaultReply $ \proxy -> do
            stateReads <- newIORef (0 :: Int)
            mirror <- mirrorViewWith cargoMirrorDocuments (proxyTarget proxy) (\_ _ -> modifyIORef' stateReads (+ 1) $> Right StateAbsent)
            let app = proxyWith proxy (\deps -> deps{pdFirstParty = const True, pdMirrorState = Just mirror})
            listing <- getPathWith [("Authorization", "caller-token")] indexPath app
            observedRows listing `shouldReturn` [nativeRecord "1.2.3"]
            response <- getPathWith [("Authorization", "caller-token")] archivePath app
            simpleStatus response `shouldBe` status200
            toStrict (simpleBody response) `shouldBe` archiveBytes
            readIORef stateReads `shouldReturn` 0
            publicCalls proxy `shouldReturn` []
