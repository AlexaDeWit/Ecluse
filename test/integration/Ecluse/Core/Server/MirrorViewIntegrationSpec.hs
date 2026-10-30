-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Managed mirrors preserve private authorisation and complete visibility through HTTP routes.
module Ecluse.Core.Server.MirrorViewIntegrationSpec (spec) where

import Data.Aeson (Object, Value (Array, Object, String), eitherDecode, encode, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (dropWhileEnd, lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Status, methodHead, status200, status304, status403, status404, status503)
import Network.Wai (Application, defaultRequest, pathInfo, rawPathInfo, requestHeaders, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Test (SResponse (simpleBody, simpleHeaders, simpleStatus))
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, PyPI, RubyGems), ecosystemName)
import Ecluse.Core.Mirror.Documents (MirrorDocuments)
import Ecluse.Core.Mirror.State
import Ecluse.Core.Mirror.State.Codec (chargedDecodeProjectRecord)
import Ecluse.Core.Mirror.State.Internal (StateRevision (StateRevision), VerifiedCompanionBytes (VerifiedCompanionBytes))
import Ecluse.Core.Package (HashAlg (SHA512), mkPackageName)
import Ecluse.Core.Package.Integrity (mkMinIntegrity)
import Ecluse.Core.Queue (MirrorJob (jobPackage, jobTriggerFilename, jobVersion), MirrorQueue (enqueue))
import Ecluse.Core.Registry.Adapter (RegistryAdapter, adapterFor)
import Ecluse.Core.Registry.Cargo.Mirror (cargoMirrorDocuments)
import Ecluse.Core.Registry.MirrorUpload (MirrorReadData (..), inventoryFiles, inventoryReadData)
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter)
import Ecluse.Core.Registry.PyPI.Document (simpleEnvelope, simpleFiles)
import Ecluse.Core.Registry.PyPI.Mirror (pypiMirrorDocuments)
import Ecluse.Core.Registry.RubyGems.Mirror (rubyGemsMirrorDocuments)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Rules.Types (DenyIfCveParams (DenyIfCveParams), FailureAlignment (FailDeny), Rule (AllowByIdentity, DenyByIdentity, DenyIfCve), RuleVerdict (Deny))
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Cache.Backend.Local (newLocalPool)
import Ecluse.Core.Server.Cache.Types (StoreBudget (StoreBudget))
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Core.Server.MirrorView
import Ecluse.Core.Server.Path (unFilename)
import Ecluse.Core.Server.Upstream (MirrorServePlan (MirrorOnAdmit, NoMirrorWrite))
import Ecluse.Runtime.Server (application, mkServerConfig)
import Ecluse.Server.Pipeline.TestSupport (getPath, getPathWith, newTestEnvWithQueue, requestAt)
import Ecluse.Service (mountBindingFor)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Mirror.RubyGems (RubyStateProbeFixture (..), rubyStateProbeFixture)
import Ecluse.Test.Mirror.State
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Registry.Cargo.Fixtures qualified as Cargo
import Ecluse.Test.Registry.Cargo.Reference qualified as Reference
import Ecluse.Test.Registry.RubyGems.Reference (RubyInfoRow (rowPosition), referenceInfo)
import Ecluse.Test.Rules (atDefaultPrecedence, inertRuleDeps, packageRule, revocation)
import Ecluse.Test.Server.Mount (serveDepsFor, withMirrorPlan)
import Ecluse.Test.Support (expectRight)
import Ecluse.Test.Wai (localhost, lookupAuth, selfBaseUrl)

spec :: Spec
spec = do
    pypiReads
    cargoReads
    rubyGemsReads

pypiReads :: Spec
pypiReads = describe "managed PyPI mirror reads" $ do
    it (caseLabel "integration/mirror-view/native-001" "keeps the admitted project envelope on cold and hot reads through public outage") $ do
        inventory <- mirrorSample [1, 2] False "https://public.test/" "source" >>= withMirrorEnvelope sourceEnvelope
        record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        absent <- testView (pure (Right StateAbsent))
        rules <- prepare inertRuleDeps [atDefaultPrecedence (AllowByIdentity "demo")]
        queue <- newTestMemoryQueue
        control <- withManagedProxyOver (queue, \deps -> deps{pdRules = rules}) absent False (native status404 []) (projectWithEnvelope sourceEnvelope False) $ \app -> do
            response <- getPath "/pypi/simple/demo/" app
            simpleStatus response `shouldBe` status200
            responseEnvelope response `shouldBe` Right (KeyMap.insert "versions" (fromListValue [String "1"]) sourceEnvelope)
            pure (responseEnvelope response)
        present <- testView (pure (Right (StateLoaded revision record)))
        withManagedProxy present False (projectWithEnvelope privateEnvelope False) (native status503 []) $ \app -> do
            replicateM_ 2 $ do
                response <- getPath "/pypi/simple/demo/" app
                simpleStatus response `shouldBe` status200
                responseEnvelope response `shouldBe` control

    it (caseLabel "integration/mirror-view/native-002" "keeps the private project envelope when an unmanaged private version is present") $ do
        inventory <- mirrorSample [1, 2] False "https://public.test/" "source" >>= withMirrorEnvelope sourceEnvelope
        record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        absent <- testView (pure (Right StateAbsent))
        control <- withManagedProxy absent False (projectWithEnvelope privateEnvelope True) (native status503 []) $ \app -> do
            response <- getPath "/pypi/simple/demo/" app
            simpleStatus response `shouldBe` status200
            responseEnvelope response `shouldBe` Right (KeyMap.insert "versions" (fromListValue [String "1", String "2"]) privateEnvelope)
            pure (responseEnvelope response)
        view <- testView (pure (Right (StateLoaded revision record)))
        withManagedProxy view False (projectWithEnvelope privateEnvelope True) (native status503 []) $ \app -> do
            response <- getPath "/pypi/simple/demo/" app
            simpleStatus response `shouldBe` status200
            responseEnvelope response `shouldBe` control

    it (caseLabel "integration/mirror-view/native-003" "refuses private access before consulting retained source fields") $ do
        calls <- newIORef (0 :: Int)
        view <- testView (modifyIORef' calls (+ 1) $> Right StateAbsent)
        withManagedProxy view False (native status403 [1]) (native status200 [1]) $ \app -> do
            response <- getPath "/pypi/simple/demo/" app
            simpleStatus response `shouldBe` status403
            artifact <- getPath artifactPath app
            simpleStatus artifact `shouldBe` status403
            for_ newFilePaths $ \path -> getPath path app >>= \fileResponse -> simpleStatus fileResponse `shouldBe` status403
        readIORef calls `shouldReturn` 0

    it (caseLabel "integration/mirror-view/native-004" "bypasses state entirely for first-party private names") $ do
        calls <- newIORef (0 :: Int)
        view <- testView (modifyIORef' calls (+ 1) $> Left (StateInvalid "unavailable"))
        withManagedProxy view True (native status200 [1]) (native status503 []) $ \app -> do
            response <- getPath "/pypi/simple/demo/" app
            simpleStatus response `shouldBe` status200
            artifact <- getPath artifactPath app
            simpleStatus artifact `shouldBe` status200
        readIORef calls `shouldReturn` 0

    it (caseLabel "integration/mirror-view/native-005" "preserves unmanaged private handling on conclusive project absence") $ do
        view <- testView (pure (Right StateAbsent))
        withManagedProxy view False (native status200 [1]) (native status503 []) $ \app -> do
            response <- getPath "/pypi/simple/demo/" app
            simpleStatus response `shouldBe` status200
            artifact <- getPath artifactPath app
            simpleStatus artifact `shouldBe` status200

    it (caseLabel "integration/mirror-view/native-006" "cannot serve A and B while unread state may retain expected C") $ do
        view <- testView (pure (Left (StateInvalid "state outage")))
        withManagedProxy view False (native status200 [1, 2]) (native status200 [1, 2]) $ \app -> do
            response <- getPath "/pypi/simple/demo/" app
            simpleStatus response `shouldBe` status503
            artifact <- getPath artifactPath app
            simpleStatus artifact `shouldBe` status503
            for_ newFilePaths $ \path -> getPath path app >>= \fileResponse -> simpleStatus fileResponse `shouldBe` status503

    it (caseLabel "integration/mirror-view/native-007" "serves completed artifacts without public reads, policy evaluation, or enqueue after fresh proxy construction") $ do
        inventory <- mirrorSample [1, 2, 3] False "https://public.test/" "original"
        record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        calls <- newIORef (0 :: Int)
        publicCalls <- newIORef (0 :: Int)
        view <- testView (modifyIORef' calls (+ 1) $> Right (StateLoaded revision record))
        let public request respond = modifyIORef' publicCalls (+ 1) >> native status503 [] request respond
        withManagedProxy view False (native status200 [1, 2, 3]) public $ \app -> do
            for_ [1, 2, 3] $ \number -> do
                response <- getPath ("/pypi/simple/demo/" <> encodeUtf8 (mirrorFilename number)) app
                simpleStatus response `shouldBe` status200
                simpleBody response `shouldBe` "wheel"
        readIORef calls `shouldReturn` 3
        readIORef publicCalls `shouldReturn` 0

    it (caseLabel "integration/mirror-view/native-008" "does not reuse a successful caller's authorisation for a refused credential") $ do
        calls <- newIORef (0 :: Int)
        view <- testView (modifyIORef' calls (+ 1) $> Right StateAbsent)
        let private request = native (if lookupAuth (requestHeaders request) == Just "Basic dXNlcjpnb29k" then status200 else status403) [1] request
        withManagedProxy view False private (native status503 []) $ \app -> do
            accepted <- getPathWith [("Authorization", "Basic dXNlcjpnb29k")] artifactPath app
            simpleStatus accepted `shouldBe` status200
            refused <- getPathWith [("Authorization", "Basic dXNlcjpiYWQ=")] artifactPath app
            simpleStatus refused `shouldBe` status403
        readIORef calls `shouldReturn` 1

    it (caseLabel "integration/mirror-view/native-009" "serves retained companions with HEAD and validators after the native target drops the declaration") $ do
        inventory <- mirrorSample [1] True "https://public.test/" "source"
        initial <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        let expected :| _ = inventoryFiles inventory
        verified <- expectRight (verifyCompanionBytes defaultLimits expected "retained metadata")
        let VerifiedCompanionBytes _ reference _ = verified
        record <- expectRight (retainCompanionReference mirrorVersion verified reference initial)
        view <- testViewWith (pure (Right (StateLoaded revision record))) (\_ -> pure (Right "retained metadata"))
        publicCalls <- newIORef (0 :: Int)
        let public request respond = modifyIORef' publicCalls (+ 1) >> native status503 [] request respond
            path = artifactPath <> ".metadata"
        withManagedProxy view False (native status200 [1]) public $ \app -> do
            initialResponse <- getPath path app
            simpleStatus initialResponse `shouldBe` status200
            simpleBody initialResponse `shouldBe` "retained metadata"
            headed <- requestAt methodHead path defaultRequest app
            simpleStatus headed `shouldBe` status200
            simpleBody headed `shouldBe` ""
            etag <- maybe (fail "missing companion validator") pure (lookup "ETag" (simpleHeaders initialResponse))
            unchanged <- getPathWith [("If-None-Match", etag)] path app
            simpleStatus unchanged `shouldBe` status304
        readIORef publicCalls `shouldReturn` 0

    it (caseLabel "integration/mirror-view/native-010" "returns explicit unavailability when a native file disappears") $ do
        inventory <- mirrorSample [1, 2, 3] False "https://public.test/" "original"
        record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        view <- testView (pure (Right (StateLoaded revision record)))
        for_ [status503, status404] $ \publicStatus ->
            withManagedProxy view False (native status200 [1, 2]) (native publicStatus []) $ \app -> do
                response <- getPath "/pypi/simple/demo/" app
                simpleStatus response `shouldBe` status503
                artifact <- getPath artifactPath app
                simpleStatus artifact `shouldBe` status503

    it (caseLabel "integration/mirror-view/native-011" "admits a later filename from public even when an unrecorded private copy exists") $ do
        old <- mirrorSample [1, 2] False "https://public.test/" "original"
        record <- expectRight (extendProjectRecord defaultLimits target old Nothing)
        view <- testView (pure (Right (StateLoaded revision record)))
        rules <- prepare inertRuleDeps [atDefaultPrecedence (AllowByIdentity "demo")]
        queued <- newIORef []
        queue <- newTestMemoryQueue
        let observing = queue{enqueue = \job -> modifyIORef' queued (<> [job]) $> Right ()}
            configure deps = (withMirrorPlan (MirrorOnAdmit (loopbackRegistryUrl "http://localhost:1")) deps){pdRules = rules}
        withManagedProxyOver (observing, configure) view False (nativeWith True "private" status200 [1, 2, 3]) (nativeWith True "public" status200 [1, 2, 3]) $ \app -> do
            hot <- getPath artifactPath app
            simpleStatus hot `shouldBe` status200
            simpleBody hot `shouldBe` "private"
            readIORef queued `shouldReturn` []
            for_ newFilePaths $ \path -> do
                response <- getPath path app
                simpleStatus response `shouldBe` status200
                simpleBody response `shouldBe` "public"
        jobs <- readIORef queued
        map jobPackage jobs `shouldBe` [mirrorPackage]
        map jobVersion jobs `shouldBe` [mirrorVersion]
        map (fmap unFilename . jobTriggerFilename) jobs `shouldBe` [Just (mirrorFilename 3)]
        later <- mirrorSample [2, 3] False "https://public.test/" "later"
        extended <- expectRight (extendProjectRecord defaultLimits target later (Just record))
        fmap (Map.keys . versionFiles) (recordVersion mirrorVersion record) `shouldBe` Just (map mirrorFilename [1, 2])
        fmap (Map.keys . versionFiles) (recordVersion mirrorVersion extended) `shouldBe` Just (map mirrorFilename [1, 2, 3])

    it (caseLabel "integration/mirror-view/native-012" "does not trust an unrecorded private filename when public lacks it") $ do
        old <- mirrorSample [1, 2] False "https://public.test/" "original"
        record <- expectRight (extendProjectRecord defaultLimits target old Nothing)
        view <- testView (pure (Right (StateLoaded revision record)))
        rules <- prepare inertRuleDeps [atDefaultPrecedence (AllowByIdentity "demo")]
        queue <- newTestMemoryQueue
        for_ [nativeWith True "public" status200 [1, 2], native status404 []] $ \public ->
            withManagedProxyOver (queue, \deps -> deps{pdRules = rules}) view False (nativeWith True "private" status200 [1, 2, 3]) public $ \app ->
                for_ newFilePaths $ \path -> getPath path app >>= \fileResponse -> simpleStatus fileResponse `shouldBe` status404

    it (caseLabel "integration/mirror-view/native-013" "denies a later artifact while serving its declared companion") $ do
        old <- mirrorSample [1, 2] False "https://public.test/" "original"
        record <- expectRight (extendProjectRecord defaultLimits target old Nothing)
        view <- testView (pure (Right (StateLoaded revision record)))
        withManagedProxy view False (nativeWith True "private" status200 [1, 2, 3]) (nativeWith True "public" status200 [1, 2, 3]) $ \app -> do
            let path = "/pypi/simple/demo/" <> encodeUtf8 (mirrorFilename 3)
            artifact <- getPath path app
            simpleStatus artifact `shouldBe` status403
            companion <- getPath (path <> ".metadata") app
            simpleStatus companion `shouldBe` status200
            simpleBody companion `shouldBe` "public"

    for_
        [ ("without-public-admission", "without an admitted public version", [DenyByIdentity "demo@1"])
        , ("with-public-sibling", "with an admitted public sibling", [DenyByIdentity "demo@1", AllowByIdentity "demo"])
        ]
        $ \(identifier, label, policy) ->
            it (caseLabel ("integration/mirror-view/native-014/" <> identifier) ("serves the sibling of a denied missing version " <> label)) $ do
                inventory <- mirrorSample [1, 2] False "https://public.test/" "source"
                record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
                view <- testView (pure (Right (StateLoaded revision record)))
                rules <- prepare inertRuleDeps (map atDefaultPrecedence policy)
                queue <- newTestMemoryQueue
                withManagedProxyOver (queue, \deps -> deps{pdRules = rules}) view False (projectVersions sourceEnvelope ["2.0"]) (projectVersions sourceEnvelope ["1.0", "2.0"]) $ \app -> do
                    response <- getPath "/pypi/simple/demo/" app
                    simpleStatus response `shouldBe` status200
                    fmap (KeyMap.lookup "versions") (responseEnvelope response) `shouldBe` Right (Just (fromListValue [String "2"]))
                    sibling <- getPath "/pypi/simple/demo/demo-2.0-1-py3-none-any.whl" app
                    simpleStatus sibling `shouldBe` status200
                    simpleBody sibling `shouldBe` "wheel"
                    denied <- getPath artifactPath app
                    simpleStatus denied `shouldBe` status403

    for_
        [ ("absent-public", "absent public metadata", [DenyByIdentity "demo@1"], native status404 [])
        , ("unavailable-public", "unavailable public metadata", [DenyByIdentity "demo@1"], native status503 [])
        , ("malformed-public", "malformed public metadata", [DenyByIdentity "demo@1"], native status200 [])
        , ("default-denial", "default denial", [], native status200 [1, 2])
        , ("undecidable-policy", "undecidable policy", [DenyIfCve (DenyIfCveParams 8 FailDeny)], native status200 [1, 2])
        , ("incomplete-inventory", "admitted incomplete inventory", [AllowByIdentity "demo"], native status200 [1])
        ]
        $ \(identifier, label, policy, public) ->
            it (caseLabel ("integration/mirror-view/native-015/" <> identifier) ("keeps missing recorded versions unavailable with " <> label)) $ do
                inventory <- mirrorSample [1, 2] False "https://public.test/" "source"
                record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
                view <- testView (pure (Right (StateLoaded revision record)))
                rules <- prepare inertRuleDeps (map atDefaultPrecedence policy)
                queue <- newTestMemoryQueue
                withManagedProxyOver (queue, \deps -> deps{pdRules = rules}) view False (projectVersions sourceEnvelope ["2.0"]) public $ \app -> do
                    response <- getPath "/pypi/simple/demo/" app
                    simpleStatus response `shouldBe` status503

    it (caseLabel "integration/mirror-view/native-016" "requires integrity-admitted evidence before a named denial releases mirror coverage") $ do
        inventory <- mirrorSample [1, 2] False "https://public.test/" "source"
        record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        view <- testView (pure (Right (StateLoaded revision record)))
        rules <- prepare inertRuleDeps [atDefaultPrecedence (DenyByIdentity "demo@1")]
        integrityFloor <- expectRight (mkMinIntegrity SHA512)
        queue <- newTestMemoryQueue
        withManagedProxyOver (queue, \deps -> deps{pdRules = rules, pdMinIntegrity = integrityFloor}) view False (projectVersions sourceEnvelope ["2.0"]) (native status200 [1, 2]) $ \app -> do
            response <- getPath "/pypi/simple/demo/" app
            simpleStatus response `shouldBe` status503

    for_ [("incomplete", [1], 1), ("later-file", [1, 2, 3], 3)] $ \(identifier, privateFiles, number) ->
        it (caseLabel ("integration/mirror-view/native-017/" <> identifier) "serves managed public companions without policy reads or queue writes") $ do
            old <- mirrorSample [1, 2] False "https://public.test/" "original"
            record <- expectRight (extendProjectRecord defaultLimits target old Nothing)
            view <- testView (pure (Right (StateLoaded revision record)))
            advisoryReads <- newIORef (0 :: Int)
            policyReads <- newIORef (0 :: Int)
            queued <- newIORef []
            queue <- newTestMemoryQueue
            let rules = [packageRule "deny-companion-control" 0 FailDeny Nothing (modifyIORef' policyReads (+ 1)) (Deny Nothing revocation)]
                observing = queue{enqueue = \job -> modifyIORef' queued (<> [job]) $> Right ()}
                configure deps =
                    (withMirrorPlan (MirrorOnAdmit (loopbackRegistryUrl "http://localhost:1")) deps)
                        { pdRules = rules
                        , pdAdvisoryEtag = modifyIORef' advisoryReads (+ 1) $> Nothing
                        }
                path = "/pypi/simple/demo/" <> encodeUtf8 (mirrorFilename number)
            withManagedProxyOver (observing, configure) view False (nativeWith True "private" status200 privateFiles) (nativeWith True "public" status200 [1, 2, 3]) $ \app -> do
                artifact <- getPath path app
                simpleStatus artifact `shouldBe` status403
                readIORef advisoryReads >>= (`shouldSatisfy` (> 0))
                readIORef policyReads >>= (`shouldSatisfy` (> 0))
                writeIORef advisoryReads 0
                writeIORef policyReads 0
                companion <- getPath (path <> ".metadata") app
                simpleStatus companion `shouldBe` status200
                simpleBody companion `shouldBe` "public"
                headed <- requestAt methodHead (path <> ".metadata") defaultRequest app
                simpleStatus headed `shouldBe` status200
                simpleBody headed `shouldBe` ""
            readIORef advisoryReads `shouldReturn` 0
            readIORef policyReads `shouldReturn` 0
            readIORef queued `shouldReturn` []

    for_ [("partial", nativeWith True "public" status200 [1]), ("absent", native status404 []), ("unavailable", native status503 [])] $ \(identifier, public) ->
        it (caseLabel ("integration/mirror-view/native-018/" <> identifier) "refuses a companion without complete public coverage") $ do
            inventory <- mirrorSample [1, 2] False "https://public.test/" "original"
            record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
            view <- testView (pure (Right (StateLoaded revision record)))
            withManagedProxy view False (nativeWith True "private" status200 [1]) public $ \app -> do
                response <- getPath (artifactPath <> ".metadata") app
                simpleStatus response `shouldBe` status503

    it (caseLabel "integration/mirror-view/native-021" "refuses a failing private origin before reading retained state, even when public covers the record") $ do
        inventory <- mirrorSample [1, 2] False "https://public.test/" "source"
        record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        calls <- newIORef (0 :: Int)
        view <- testView (modifyIORef' calls (+ 1) $> Right (StateLoaded revision record))
        rules <- prepare inertRuleDeps [atDefaultPrecedence (AllowByIdentity "demo")]
        queue <- newTestMemoryQueue
        withManagedProxyOver (queue{enqueue = \_ -> fail "unexpected mirror enqueue"}, \deps -> deps{pdRules = rules}) view False (native status503 []) (native status200 [1, 2]) $ \app ->
            for_ (["/pypi/simple/demo/", artifactPath] <> newFilePaths) $ \path ->
                getPath path app >>= \response -> simpleStatus response `shouldBe` status503
        readIORef calls `shouldReturn` 0

newFilePaths :: [ByteString]
newFilePaths = [path, path <> ".metadata"]
  where
    path = "/pypi/simple/demo/" <> encodeUtf8 (mirrorFilename 3)

cargoReads :: Spec
cargoReads = describe "managed Cargo mirror reads" $
    it (caseLabel "integration/mirror-view/native-019" "restores retained Cargo rows in original index order on the merged listing during public outage") $ do
        let name = mkPackageName Cargo Nothing "thing"
            release = Cargo.minimalCargoRelease "thing"
            original version = Cargo.cargoIndexRecord (release version)
            crateTarget = stateTarget "https://private.test/cargo"
        record <- cargoMirrorRecord crateTarget name (Cargo.releaseArchive (release "1.9.0")) [(0, original "1.9.0"), (1, original "1.10.0")]
        view <- testViewAt cargoMirrorDocuments crateTarget (pure (Right (StateLoaded (StateRevision crateTarget name "complete") record)))
        adapter <- requiredAdapter Cargo
        withMountProxy Cargo adapter (\deps -> deps{pdEgressUrl = Right . loopbackRegistryUrl}) view (crateIndex (map republished [original "1.10.0", original "1.9.0"])) unavailable $ \app -> do
            response <- getPath "/cargo/th/in/thing" app
            simpleStatus response `shouldBe` status200
            rows <- traverse (expectRight . Reference.indexValue) (Reference.referenceIndex (toStrict (simpleBody response)))
            map Object rows `shouldBe` [original "1.9.0", original "1.10.0"]
  where
    republished = \case
        Object fields -> Object (KeyMap.insert "pubtime" (String "2026-10-01T00:00:00Z") fields)
        other -> other

rubyGemsReads :: Spec
rubyGemsReads = describe "managed RubyGems mirror reads" $
    it (caseLabel "integration/mirror-view/native-020" "restores every retained RubyGems platform row and publication time on /info during public outage") $ do
        fixture <- rubyStateProbeFixture
        let gemTarget = rubyStateTarget fixture
            name = rubyStatePackage fixture
        record <- chargedDecodeProjectRecord (const pass) defaultLimits gemTarget name (rubyStateEncodedRecord fixture) >>= expectRight
        view <- testViewAt rubyGemsMirrorDocuments gemTarget (pure (Right (StateLoaded (StateRevision gemTarget name "complete") record)))
        original <- readFileBS "test/fixtures/rubygems/info/ecluse-fixture-platform"
        let republished = encodeUtf8 (T.replace "created_at:2020-" "created_at:2026-" (decodeUtf8 original))
        adapter <- requiredAdapter RubyGems
        withMountProxy RubyGems adapter id view (gemInfo republished) unavailable $ \app -> do
            response <- getPath "/rubygems/info/ecluse-fixture-platform" app
            simpleStatus response `shouldBe` status200
            served <- either (fail . toString) pure (referenceInfo (toStrict (simpleBody response)))
            expected <- either (fail . toString) pure (referenceInfo original)
            length expected `shouldBe` 4
            map unpositioned served `shouldMatchList` map unpositioned expected
  where
    unpositioned row = row{rowPosition = 0}

sourceEnvelope, privateEnvelope :: Object
sourceEnvelope = KeyMap.fromList [("name", String "demo"), ("meta", object ["api-version" .= ("1.0" :: Text)]), ("project-status", object ["status" .= ("active" :: Text)]), ("alternate-locations", fromListValue [String "https://alternate.test/simple/demo/"])]
privateEnvelope = KeyMap.insert "project-status" (object ["status" .= ("archived" :: Text)]) sourceEnvelope

responseEnvelope :: SResponse -> Either String Object
responseEnvelope response = do
    value <- eitherDecode (simpleBody response)
    case value of
        Object fields -> Right (KeyMap.delete "files" fields)
        _ -> Left "expected project object"

projectWithEnvelope :: Object -> Bool -> Application
projectWithEnvelope envelope extraVersion = projectVersions envelope (["1.0"] <> ["2.0" | extraVersion])

projectVersions :: Object -> [Text] -> Application
projectVersions envelope versions request respond = case dropWhileEnd T.null (pathInfo request) of
    ["simple", _] -> do
        inventory <- mirrorSample [1, 2] False (selfBaseUrl request <> "/files/") "native"
        let files = case inventoryReadData inventory of
                PyPIMirrorRead document -> map snd (simpleFiles document)
                _ -> []
            versioned = [Object (KeyMap.mapWithKey (versionField version) fields) | version <- versions, Object fields <- files]
            body = Object (KeyMap.insert "files" (fromListValue versioned) envelope)
        respond (responseLBS status200 [("Content-Type", "application/vnd.pypi.simple.v1+json")] (encode body))
    _ -> respond (responseLBS status200 [] "wheel")
  where
    versionField version key (String value)
        | key `elem` ["filename", "url"] = String (T.replace "demo-1.0-" ("demo-" <> version <> "-") value)
    versionField _ _ value = value

native :: Status -> [Int] -> Application
native = nativeWith False "wheel"

nativeWith :: Bool -> LByteString -> Status -> [Int] -> Application
nativeWith _ _ status [] _ respond = respond (responseLBS status [] "{}")
nativeWith companion bytes status numbers request respond = case dropWhileEnd T.null (pathInfo request) of
    ["simple", _] -> do
        inventory <- mirrorSample numbers companion (selfBaseUrl request <> "/files/") "native"
        let body = case inventoryReadData inventory of
                PyPIMirrorRead document -> encode (Object (KeyMap.insert "files" (fromListValue (map snd (simpleFiles document))) (simpleEnvelope document)))
                NpmMirrorRead fields -> encode (Object fields)
                CargoMirrorRead _ -> "unsupported fixture source"
                RubyGemsMirrorRead _ -> "unsupported fixture source"
        respond (responseLBS status [("Content-Type", "application/vnd.pypi.simple.v1+json")] body)
    _ -> respond (responseLBS status [] bytes)

-- Cargo discovery and one LF index file at the private upstream's root.
crateIndex :: [Value] -> Application
crateIndex rows request respond = case rawPathInfo request of
    "/config.json" -> respond (responseLBS status200 [] (toLazy (Cargo.cargoViewDiscovery (selfBaseUrl request) Nothing False)))
    "/th/in/thing" -> respond (responseLBS status200 [] (foldMap (\row -> encode row <> "\n") rows))
    _ -> respond (responseLBS status404 [] "")

gemInfo :: ByteString -> Application
gemInfo bytes request respond = case pathInfo request of
    ["info", _] -> respond (responseLBS status200 [] (toLazy bytes))
    _ -> respond (responseLBS status404 [] "")

unavailable :: Application
unavailable _ respond = respond (responseLBS status503 [] "public origin unavailable")

fromListValue :: [Value] -> Value
fromListValue = Array . fromList

artifactPath :: ByteString
artifactPath = "/pypi/simple/demo/" <> encodeUtf8 (mirrorFilename 1)

target :: StateTarget
target = stateTarget "https://private.test/"

revision :: StateRevision
revision = StateRevision target mirrorPackage "one"

testView :: IO (Either MirrorStateFault StateRead) -> IO MirrorView
testView readState = testViewWith readState (\_ -> pure (Left StateMissingCompanion))

testViewWith :: IO (Either MirrorStateFault StateRead) -> (CompanionRef -> IO (Either MirrorStateFault ByteString)) -> IO MirrorView
testViewWith = viewAt pypiMirrorDocuments target

-- A view over another state target that retains no companion bytes.
testViewAt :: MirrorDocuments -> StateTarget -> IO (Either MirrorStateFault StateRead) -> IO MirrorView
testViewAt documents viewTarget readState = viewAt documents viewTarget readState (\_ -> pure (Left StateMissingCompanion))

viewAt :: MirrorDocuments -> StateTarget -> IO (Either MirrorStateFault StateRead) -> (CompanionRef -> IO (Either MirrorStateFault ByteString)) -> IO MirrorView
viewAt documents viewTarget readState companion = do
    pool <- newLocalPool 20 10_000_000
    cache <- newMirrorRetention pool 60 (StoreBudget 0 0)
    let store = MirrorState (\_ _ _ -> readState) (\_ _ _ _ -> pure (Left (StateInvalid "unexpected state write"))) (\_ _ -> pure (Left StateMissingCompanion)) (const companion)
    pure (newMirrorView defaultLimits viewTarget store cache documents)

withManagedProxy :: MirrorView -> Bool -> Application -> Application -> (Application -> IO a) -> IO a
withManagedProxy view firstParty private public action = do
    queue <- newTestMemoryQueue
    withManagedProxyOver (queue{enqueue = \_ -> fail "unexpected mirror enqueue"}, id) view firstParty private public action

withManagedProxyOver :: (MirrorQueue, PackumentDeps -> PackumentDeps) -> MirrorView -> Bool -> Application -> Application -> (Application -> IO a) -> IO a
withManagedProxyOver = withMountOver PyPI pypiAdapter

-- Another ecosystem's mount over a managed view, for undeclared names and with no mirror enqueue.
withMountProxy :: Ecosystem -> RegistryAdapter -> (PackumentDeps -> PackumentDeps) -> MirrorView -> Application -> Application -> (Application -> IO a) -> IO a
withMountProxy eco adapter configure view private public action = do
    queue <- newTestMemoryQueue
    withMountOver eco adapter (queue{enqueue = \_ -> fail "unexpected mirror enqueue"}, configure) view False private public action

withMountOver :: Ecosystem -> RegistryAdapter -> (MirrorQueue, PackumentDeps -> PackumentDeps) -> MirrorView -> Bool -> Application -> Application -> (Application -> IO a) -> IO a
withMountOver eco adapter (queue, configure) view firstParty private public action = do
    manager <- newManager defaultManagerSettings
    testWithApplication (pure private) $ \privatePort ->
        testWithApplication (pure public) $ \publicPort -> withSweeper $ \deadlines -> do
            env <- newTestEnvWithQueue deadlines queue manager
            let deps =
                    (serveDepsFor adapter (Just (loopbackRegistryUrl (localhost privatePort))) (loopbackRegistryUrl (localhost publicPort)) NoMirrorWrite [] (pure (UTCTime (fromGregorian 2026 10 11) 0)))
                        { pdMirrorState = Just view
                        , pdFirstParty = const firstParty
                        , pdMountBaseUrl = "http://ecluse.test/" <> ecosystemName eco
                        }
            action (application (mkServerConfig (maybeToList (mountBindingFor eco (configure deps) Nothing))) env)

requiredAdapter :: Ecosystem -> IO RegistryAdapter
requiredAdapter eco = maybe (fail (toString (ecosystemName eco) <> " has no production adapter registration")) pure (adapterFor eco)
