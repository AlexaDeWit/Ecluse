-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Managed mirrors preserve private authorisation and complete visibility through HTTP routes.
module Ecluse.Core.Server.MirrorViewIntegrationSpec (spec) where

import Data.Aeson (Object, Value (Array, Object, String), eitherDecode, encode, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.List (dropWhileEnd, lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Network.HTTP.Types (Status, methodGet, methodHead, status200, status304, status403, status404, status503)
import Network.Wai (Application, defaultRequest, pathInfo, rawPathInfo, requestHeaders, responseLBS)
import Network.Wai.Test (SResponse (simpleBody, simpleHeaders, simpleStatus))
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, PyPI, RubyGems), ecosystemName)
import Ecluse.Core.Mirror.Documents (MirrorDocuments)
import Ecluse.Core.Mirror.State
import Ecluse.Core.Mirror.State.Codec (chargedDecodeProjectRecord)
import Ecluse.Core.Mirror.State.Format (MirrorStateFormat)
import Ecluse.Core.Mirror.State.Internal (StateRevision (StateRevision), VerifiedCompanionBytes (VerifiedCompanionBytes))
import Ecluse.Core.Package (HashAlg (SHA512), mkPackageName)
import Ecluse.Core.Package.Integrity (mkMinIntegrity)
import Ecluse.Core.Queue (MirrorJob (jobPackage, jobTriggerFilename, jobVersion), MirrorQueue (enqueue))
import Ecluse.Core.Registry.Adapter (RegistryAdapter)
import Ecluse.Core.Registry.Cargo.Mirror (cargoMirrorDocuments, cargoStateFormat)
import Ecluse.Core.Registry.MirrorUpload (MirrorReadData (..), inventoryFiles, inventoryReadData)
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter)
import Ecluse.Core.Registry.PyPI.Document (simpleEnvelope, simpleFiles)
import Ecluse.Core.Registry.PyPI.Mirror (pypiMirrorDocuments, pypiStateFormat)
import Ecluse.Core.Registry.RubyGems.Mirror (rubyGemsMirrorDocuments, rubyGemsStateFormat)
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
import Ecluse.Server.Pipeline.TestSupport (getPath, getPathWith, newTestEnvWithQueue, requestAt, withUpstreams)
import Ecluse.Service (mountBindingFor)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Mirror.RubyGems (RubyStateProbeFixture (..), makeRubyStateProbeFixture, rubyStateProbeFixture)
import Ecluse.Test.Mirror.State
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Registry.Cargo.Fixtures qualified as Cargo
import Ecluse.Test.Registry.Cargo.Reference qualified as Reference
import Ecluse.Test.Registry.RubyGems.Fixtures (RubyRelease (..), loadRubyReleasesFrom, rubyRelease)
import Ecluse.Test.Registry.RubyGems.Reference (RubyInfoRow (rowPosition), referenceInfo)
import Ecluse.Test.Rules (atDefaultPrecedence, inertRuleDeps, packageRule, revocation)
import Ecluse.Test.Server.Mount (requiredAdapter, serveDepsFor, withMirrorPlan)
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
        record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
        absent <- testView (pure (Right StateAbsent))
        rules <- prepare inertRuleDeps [atDefaultPrecedence (AllowByIdentity "demo")]
        queue <- newTestMemoryQueue
        control <- withManagedProxyOver (queue, \deps -> deps{pdRules = rules}) absent False (native status404 []) (projectWithEnvelope sourceEnvelope False) $ \app -> do
            response <- getPath "/pypi/simple/demo/" app
            simpleStatus response `shouldBe` status200
            responseEnvelope response `shouldBe` Right (KeyMap.insert "versions" (fromListValue [String "1"]) sourceEnvelope)
            pure (responseEnvelope response)
        -- The store answers a read that names its revision as unchanged, as the production store does,
        -- so the second read serves the record the first one retained.
        stateReads <- newIORef []
        present <- mirrorViewWith pypiStateFormat pypiMirrorDocuments target $ \_ held -> do
            modifyIORef' stateReads (<> [held])
            pure $ Right $ case held of
                Just seen | seen == revision -> StateUnchanged revision
                _ -> StateLoaded revision record
        withManagedProxy present False (projectWithEnvelope privateEnvelope False) (native status503 []) $ \app -> do
            replicateM_ 2 $ do
                response <- getPath "/pypi/simple/demo/" app
                simpleStatus response `shouldBe` status200
                responseEnvelope response `shouldBe` control
        readIORef stateReads `shouldReturn` [Nothing, Just revision]

    it (caseLabel "integration/mirror-view/native-002" "keeps the private project envelope when an unmanaged private version is present") $ do
        inventory <- mirrorSample [1, 2] False "https://public.test/" "source" >>= withMirrorEnvelope sourceEnvelope
        record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
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
        withManagedProxy view True (nativeWith True "private" status200 [1]) (native status503 []) $ \app -> do
            response <- getPath "/pypi/simple/demo/" app
            simpleStatus response `shouldBe` status200
            for_ [artifactPath, artifactPath <> ".metadata"] $ \path -> do
                served <- getPath path app
                simpleStatus served `shouldBe` status200
                simpleBody served `shouldBe` "private"
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
        record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
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
        initial <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
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
        record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
        view <- testView (pure (Right (StateLoaded revision record)))
        for_ [status503, status404] $ \publicStatus ->
            withManagedProxy view False (native status200 [1, 2]) (native publicStatus []) $ \app -> do
                response <- getPath "/pypi/simple/demo/" app
                simpleStatus response `shouldBe` status503
                artifact <- getPath artifactPath app
                simpleStatus artifact `shouldBe` status503

    it (caseLabel "integration/mirror-view/native-011" "admits a later filename from public even when an unrecorded private copy exists") $ do
        old <- mirrorSample [1, 2] False "https://public.test/" "original"
        record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target old Nothing)
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
        extended <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target later (Just record))
        fmap (Map.keys . versionFiles) (recordVersion mirrorVersion record) `shouldBe` Just (map mirrorFilename [1, 2])
        fmap (Map.keys . versionFiles) (recordVersion mirrorVersion extended) `shouldBe` Just (map mirrorFilename [1, 2, 3])

    it (caseLabel "integration/mirror-view/native-012" "does not trust an unrecorded private filename when public lacks it") $ do
        old <- mirrorSample [1, 2] False "https://public.test/" "original"
        record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target old Nothing)
        view <- testView (pure (Right (StateLoaded revision record)))
        rules <- prepare inertRuleDeps [atDefaultPrecedence (AllowByIdentity "demo")]
        queue <- newTestMemoryQueue
        for_ [nativeWith True "public" status200 [1, 2], native status404 []] $ \public ->
            withManagedProxyOver (queue, \deps -> deps{pdRules = rules}) view False (nativeWith True "private" status200 [1, 2, 3]) public $ \app ->
                for_ newFilePaths $ \path -> getPath path app >>= \fileResponse -> simpleStatus fileResponse `shouldBe` status404

    it (caseLabel "integration/mirror-view/native-013" "denies a later artifact while serving its declared companion") $ do
        old <- mirrorSample [1, 2] False "https://public.test/" "original"
        record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target old Nothing)
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
                record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
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
                record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
                view <- testView (pure (Right (StateLoaded revision record)))
                rules <- prepare inertRuleDeps (map atDefaultPrecedence policy)
                queue <- newTestMemoryQueue
                withManagedProxyOver (queue, \deps -> deps{pdRules = rules}) view False (projectVersions sourceEnvelope ["2.0"]) public $ \app -> do
                    response <- getPath "/pypi/simple/demo/" app
                    simpleStatus response `shouldBe` status503

    it (caseLabel "integration/mirror-view/native-016" "requires integrity-admitted evidence before a named denial releases mirror coverage") $ do
        inventory <- mirrorSample [1, 2] False "https://public.test/" "source"
        record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
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
            record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target old Nothing)
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
            record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
            view <- testView (pure (Right (StateLoaded revision record)))
            withManagedProxy view False (nativeWith True "private" status200 [1]) public $ \app -> do
                response <- getPath (artifactPath <> ".metadata") app
                simpleStatus response `shouldBe` status503

    it (caseLabel "integration/mirror-view/native-021" "refuses a failing private origin before reading retained state, even when public covers the record") $ do
        inventory <- mirrorSample [1, 2] False "https://public.test/" "source"
        record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
        calls <- newIORef (0 :: Int)
        view <- testView (modifyIORef' calls (+ 1) $> Right (StateLoaded revision record))
        rules <- prepare inertRuleDeps [atDefaultPrecedence (AllowByIdentity "demo")]
        queue <- newTestMemoryQueue
        withManagedProxyOver (queue{enqueue = \_ -> fail "unexpected mirror enqueue"}, \deps -> deps{pdRules = rules}) view False (native status503 []) (native status200 [1, 2]) $ \app ->
            for_ (["/pypi/simple/demo/", artifactPath] <> newFilePaths) $ \path ->
                getPath path app >>= \response -> simpleStatus response `shouldBe` status503
        readIORef calls `shouldReturn` 0

    for_ [("absent-public", status404, status404), ("unavailable-public", status503, status503)] $ \(identifier, publicStatus, expected) ->
        it (caseLabel ("integration/mirror-view/native-022/" <> identifier) "answers a recorded project 404 only when both origins answer 404 and no recorded version survives") $ do
            inventory <- mirrorSample [1, 2] False "https://public.test/" "source"
            record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
            view <- testView (pure (Right (StateLoaded revision record)))
            withManagedProxy view False (native status404 []) (native publicStatus []) $ \app ->
                recordedPathAnswers expected "/pypi/simple/demo/" app

    for_ [("absent-public", status404, status404), ("unavailable-public", status503, status503)] $ \(identifier, publicStatus, expected) -> do
        it (caseLabel ("integration/mirror-view/native-025/" <> identifier) "answers a recorded distribution and its companion 404 only when both origins answer 404 for the project") $ do
            inventory <- mirrorSample [1, 2] False "https://public.test/" "source"
            record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
            view <- testView (pure (Right (StateLoaded revision record)))
            withManagedProxy view False (native status404 []) (native publicStatus []) $ \app ->
                for_ [artifactPath, artifactPath <> ".metadata"] $ \path -> recordedPathAnswers expected path app
        it (caseLabel ("integration/mirror-view/native-026/" <> identifier) "answers a recorded distribution 404 only when the private file and the public version are both gone") $ do
            inventory <- mirrorSample [1, 2] False "https://public.test/" "source"
            record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
            view <- testView (pure (Right (StateLoaded revision record)))
            withManagedProxy view False (withoutFiles (native status200 [1, 2])) (native publicStatus []) $
                recordedPathAnswers expected artifactPath

    it (caseLabel "integration/mirror-view/native-030" "answers a recorded distribution and its companion 404 when the public project lacks the version beside a private 404") $ do
        view <- recordedDemoView
        withManagedProxy view False (native status404 []) (projectVersions sourceEnvelope ["2.0"]) $ \app ->
            for_ [artifactPath, artifactPath <> ".metadata"] $ \path -> recordedPathAnswers status404 path app

    for_ [("covered", native status200 [1, 2], status200), ("uncovered", native status200 [1], status503)] $ \(identifier, public, expected) ->
        it (caseLabel ("integration/mirror-view/native-033/" <> identifier) "serves public bytes for a recorded file the private archive lost only under mirror coverage") $ do
            view <- recordedDemoView
            rules <- prepare inertRuleDeps [atDefaultPrecedence (AllowByIdentity "demo")]
            queue <- newTestMemoryQueue
            withManagedProxyOver (queue, \deps -> deps{pdRules = rules}) view False (withoutFiles (native status200 [1, 2])) public $ \app -> do
                recordedPathAnswers expected artifactPath app
                response <- getPath artifactPath app
                (simpleBody response == "wheel") `shouldBe` (expected == status200)

    it (caseLabel "integration/mirror-view/native-034" "keeps a recorded distribution and its companion unavailable when the public index names another project") $ do
        view <- recordedDemoView
        withManagedProxy view False (native status404 []) otherProjectIndex $ \app ->
            for_ [artifactPath, artifactPath <> ".metadata"] $ \path -> recordedPathAnswers status503 path app
  where
    recordedDemoView = do
        inventory <- mirrorSample [1, 2] False "https://public.test/" "source"
        record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
        testView (pure (Right (StateLoaded revision record)))
    otherProjectIndex _ respond =
        respond (responseLBS status200 [("Content-Type", "application/vnd.pypi.simple.v1+json")] (encode (object ["meta" .= object ["api-version" .= ("1.0" :: Text)], "name" .= ("other" :: Text), "files" .= ([] :: [Value])])))

newFilePaths :: [ByteString]
newFilePaths = [path, path <> ".metadata"]
  where
    path = "/pypi/simple/demo/" <> encodeUtf8 (mirrorFilename 3)

cargoReads :: Spec
cargoReads = describe "managed Cargo mirror reads" $ do
    it (caseLabel "integration/mirror-view/native-019" "restores retained Cargo rows in original index order on the merged listing during public outage") $ do
        view <- recordedCrateView
        adapter <- requiredAdapter Cargo
        withMountProxy Cargo adapter (\deps -> deps{pdEgressUrl = Right . loopbackRegistryUrl}) view (crateIndex (map republished [original "1.9.1", original "1.10.0", original "1.9.0"])) unavailable $ \app -> do
            response <- getPath "/cargo/th/in/thing" app
            simpleStatus response `shouldBe` status200
            rows <- traverse (expectRight . Reference.indexValue) (Reference.referenceIndex (toStrict (simpleBody response)))
            map Object rows `shouldBe` [original "1.9.0", original "1.10.0", original "1.9.1"]
    for_ [("absent-public", native status404 [], status404), ("unavailable-public", unavailable, status503)] $ \(identifier, public, expected) ->
        it (caseLabel ("integration/mirror-view/native-023/" <> identifier) "answers a recorded crate 404 only when both origins answer 404 and no recorded version survives") $ do
            view <- recordedCrateView
            adapter <- requiredAdapter Cargo
            withMountProxy Cargo adapter (\deps -> deps{pdEgressUrl = Right . loopbackRegistryUrl}) view crateDiscovery public $
                recordedPathAnswers expected "/cargo/th/in/thing"
    for_ [("absent-public", native status404 [], status404), ("unavailable-public", unavailable, status503)] $ \(identifier, public, expected) -> do
        it (caseLabel ("integration/mirror-view/native-027/" <> identifier) "answers a recorded archive 404 only when both origins answer 404 for the crate") $ do
            view <- recordedCrateView
            adapter <- requiredAdapter Cargo
            withMountProxy Cargo adapter (\deps -> deps{pdEgressUrl = Right . loopbackRegistryUrl}) view crateDiscovery public $
                recordedPathAnswers expected archivePath
        it (caseLabel ("integration/mirror-view/native-028/" <> identifier) "answers a recorded archive 404 only when the private archive and the public version are both gone") $ do
            view <- recordedCrateView
            adapter <- requiredAdapter Cargo
            withMountProxy Cargo adapter (\deps -> deps{pdEgressUrl = Right . loopbackRegistryUrl}) view (crateIndex (map original ["1.9.0", "1.10.0", "1.9.1"])) public $
                recordedPathAnswers expected archivePath
    it (caseLabel "integration/mirror-view/native-031" "answers a recorded archive 404 when the public crate lacks the version beside a private 404") $ do
        view <- recordedCrateView
        adapter <- requiredAdapter Cargo
        withMountProxy Cargo adapter (\deps -> deps{pdEgressUrl = Right . loopbackRegistryUrl}) view crateDiscovery (crateIndex [original "2.0.0"]) $
            recordedPathAnswers status404 archivePath
  where
    archivePath = "/cargo/crates/thing/1.9.0/thing-1.9.0.crate"
    name = mkPackageName Cargo Nothing "thing"
    release = Cargo.minimalCargoRelease "thing"
    original version = Cargo.cargoIndexRecord (release version)
    crateTarget = stateTarget "https://private.test/cargo"
    -- A backport recorded after 1.10.0. The index order differs from version-text order, its
    -- reverse, semver order and the private origin's order.
    recordedCrateView = do
        record <- cargoMirrorRecord crateTarget name (Cargo.releaseArchive (release "1.9.0")) [(0, original "1.9.0"), (1, original "1.10.0"), (2, original "1.9.1")]
        testViewAt cargoStateFormat cargoMirrorDocuments crateTarget (pure (Right (StateLoaded (StateRevision crateTarget name "complete") record)))
    republished = \case
        Object fields -> Object (KeyMap.insert "pubtime" (String "2026-10-01T00:00:00Z") fields)
        other -> other

rubyGemsReads :: Spec
rubyGemsReads = describe "managed RubyGems mirror reads" $ do
    it (caseLabel "integration/mirror-view/native-020" "restores every retained RubyGems platform row and publication time on /info during public outage") $ do
        view <- recordedGemView
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
    for_ [("absent-public", native status404 [], status404), ("unavailable-public", unavailable, status503)] $ \(identifier, public, expected) ->
        it (caseLabel ("integration/mirror-view/native-024/" <> identifier) "answers a recorded gem's /info 404 only when both origins answer 404 and no recorded version survives") $ do
            view <- recordedGemView
            adapter <- requiredAdapter RubyGems
            withMountProxy RubyGems adapter id view (native status404 []) public $
                recordedPathAnswers expected "/rubygems/info/ecluse-fixture-platform"
    for_ [("absent-public", native status404 [], status404), ("unavailable-public", unavailable, status503)] $ \(identifier, public, expected) ->
        it (caseLabel ("integration/mirror-view/native-029/" <> identifier) "answers a recorded gem's archive and compact specification 404 only when both origins answer 404") $ do
            view <- recordedGemView
            adapter <- requiredAdapter RubyGems
            withMountProxy RubyGems adapter id view (native status404 []) public $ \app ->
                for_ ["/rubygems/gems/ecluse-fixture-platform-1.0.0.gem", "/rubygems/quick/Marshal.4.8/ecluse-fixture-platform-1.0.0.gemspec.rz"] $ \path ->
                    recordedPathAnswers expected path app
    it (caseLabel "integration/mirror-view/native-032" "answers a recorded gem's archive and compact specification 404 when the public gem lacks the version beside a private 404") $ do
        view <- recordedGemView
        adapter <- requiredAdapter RubyGems
        withMountProxy RubyGems adapter id view (native status404 []) (gemInfo laterOnlyInfo) $ \app ->
            for_ platformCoordinates $ \path ->
                recordedPathAnswers status404 path app
    for_ [("other-checksum", \release -> T.replace (releaseChecksum release) (T.replicate 64 "b")), ("missing-platform", \_ -> T.unlines . filter (not . T.isPrefixOf "1.0.0-java ") . T.lines)] $ \(identifier, change) ->
        it (caseLabel ("integration/mirror-view/native-035/" <> identifier) "keeps a recorded gem unavailable on every path when the public gem does not cover the record beside a private 404") $ do
            view <- recordedGemView
            release <- fixtureRelease "ecluse-fixture-platform" "1.0.0"
            original <- readFileBS "test/fixtures/rubygems/info/ecluse-fixture-platform"
            rules <- allowing "ecluse-fixture-platform"
            adapter <- requiredAdapter RubyGems
            withMountProxy RubyGems adapter rules view (native status404 []) (gemOrigin (encodeUtf8 (change release (decodeUtf8 original))) [release]) $ \app ->
                for_ (platformCoordinates <> ["/rubygems/info/ecluse-fixture-platform"]) $ \path ->
                    recordedPathAnswers status503 path app
    it (caseLabel "integration/mirror-view/native-036" "keeps a recorded gem's /info unavailable when the public gem lacks the version beside a private 404") $ do
        view <- recordedGemView
        rules <- allowing "ecluse-fixture-platform"
        adapter <- requiredAdapter RubyGems
        withMountProxy RubyGems adapter rules view (native status404 []) (gemInfo laterOnlyInfo) $
            recordedPathAnswers status503 "/rubygems/info/ecluse-fixture-platform"
    for_ [("gone", False, status404), ("covered", True, status200)] $ \(identifier, listed, expected) ->
        it (caseLabel ("integration/mirror-view/native-037/" <> identifier) "checks mirror coverage for the requested gem version only beside a private 404") $ do
            view <- recordedView =<< makeRubyStateProbeFixture "test/fixtures/rubygems" "ecluse-fixture-leaf"
            requested <- fixtureRelease "ecluse-fixture-leaf" "1.0.0"
            uncovered <- fixtureRelease "ecluse-fixture-leaf" "2.0.0"
            -- 2.1.0.pre.1 is gone from the public gem, and its 2.0.0 carries another checksum.
            let info = "---\n" <> unlines ([releaseInfoRow requested | listed] <> [T.replace (releaseChecksum uncovered) (T.replicate 64 "b") (releaseInfoRow uncovered)])
            rules <- allowing "ecluse-fixture-leaf"
            adapter <- requiredAdapter RubyGems
            withMountProxy RubyGems adapter rules view (native status404 []) (gemOrigin (encodeUtf8 info) [requested]) $ \app ->
                for_ [("/rubygems/gems/ecluse-fixture-leaf-1.0.0.gem", releaseArchive requested), ("/rubygems/quick/Marshal.4.8/ecluse-fixture-leaf-1.0.0.gemspec.rz", releaseCompact requested)] $ \(path, bytes) -> do
                    recordedPathAnswers expected path app
                    response <- getPath path app
                    (simpleBody response == toLazy bytes) `shouldBe` listed
    it (caseLabel "integration/mirror-view/native-038" "answers a recorded gem's archive and compact specification 404 beside a public gem that dropped a row and whose other version the gate denies") $ do
        view <- recordedGemView
        adapter <- requiredAdapter RubyGems
        withMountProxy RubyGems adapter id view (native status404 []) (gemInfo (laterOnlyInfo <> "2.1.0 |checksum:invalid\n")) $ \app ->
            for_ platformCoordinates $ \path ->
                recordedPathAnswers status404 path app
  where
    unpositioned row = row{rowPosition = 0}
    platformCoordinates = ["/rubygems/gems/ecluse-fixture-platform-1.0.0.gem", "/rubygems/quick/Marshal.4.8/ecluse-fixture-platform-1.0.0.gemspec.rz"]
    laterOnlyInfo = "---\n2.0.0 |checksum:" <> encodeUtf8 (T.replicate 64 "a") <> "\n"
    allowing name = (\rules deps -> deps{pdRules = rules}) <$> prepare inertRuleDeps [atDefaultPrecedence (AllowByIdentity name)]
    fixtureRelease name version = loadRubyReleasesFrom "test/fixtures/rubygems" >>= \releases -> rubyRelease releases name version "ruby"
    recordedGemView = recordedView =<< rubyStateProbeFixture
    recordedView fixture = do
        let gemTarget = rubyStateTarget fixture
            name = rubyStatePackage fixture
        record <- chargedDecodeProjectRecord rubyGemsStateFormat (const pass) defaultLimits gemTarget name (rubyStateEncodedRecord fixture) >>= expectRight
        testViewAt rubyGemsStateFormat rubyGemsMirrorDocuments gemTarget (pure (Right (StateLoaded (StateRevision gemTarget name "complete") record)))

-- A recorded name's path answers GET and HEAD with this status.
recordedPathAnswers :: Status -> ByteString -> Application -> Expectation
recordedPathAnswers expected path app =
    for_ [methodGet, methodHead] $ \method -> do
        response <- requestAt method path defaultRequest app
        simpleStatus response `shouldBe` expected

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

-- This origin's project index, with every file it lists answering 404.
withoutFiles :: Application -> Application
withoutFiles index request respond = case dropWhileEnd T.null (pathInfo request) of
    ["simple", _] -> index request respond
    _ -> respond (responseLBS status404 [] "")

-- Cargo discovery and one LF index file at the private upstream's root.
crateIndex :: [Value] -> Application
crateIndex rows request respond = case rawPathInfo request of
    "/th/in/thing" -> respond (responseLBS status200 [] (foldMap (\row -> encode row <> "\n") rows))
    _ -> crateDiscovery request respond

-- Cargo discovery at the private upstream's root, which holds no index file.
crateDiscovery :: Application
crateDiscovery request respond = case rawPathInfo request of
    "/config.json" -> respond (responseLBS status200 [] (toLazy (Cargo.cargoViewDiscovery (selfBaseUrl request) Nothing False)))
    _ -> respond (responseLBS status404 [] "")

gemInfo :: ByteString -> Application
gemInfo bytes = gemOrigin bytes []

-- A gem origin with these /info bytes that also serves these releases' archives and compact specifications.
gemOrigin :: ByteString -> [RubyRelease] -> Application
gemOrigin bytes releases request respond = case pathInfo request of
    ["info", _] -> respond (responseLBS status200 [] (toLazy bytes))
    ["gems", file] | Just release <- find ((== file) . releaseFilename) releases -> served (releaseArchive release)
    ["quick", "Marshal.4.8", file] | Just release <- find ((== file) . compactName) releases -> served (releaseCompact release)
    _ -> respond (responseLBS status404 [] "")
  where
    served bytes' = respond (responseLBS status200 [("Content-Length", show (BS.length bytes'))] (toLazy bytes'))
    compactName release = fromMaybe (releaseFilename release) (T.stripSuffix ".gem" (releaseFilename release)) <> ".gemspec.rz"

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
testViewWith = viewAt pypiStateFormat pypiMirrorDocuments target

-- A view over another state target that retains no companion bytes.
testViewAt :: MirrorStateFormat -> MirrorDocuments -> StateTarget -> IO (Either MirrorStateFault StateRead) -> IO MirrorView
testViewAt format documents viewTarget readState = viewAt format documents viewTarget readState (\_ -> pure (Left StateMissingCompanion))

viewAt :: MirrorStateFormat -> MirrorDocuments -> StateTarget -> IO (Either MirrorStateFault StateRead) -> (CompanionRef -> IO (Either MirrorStateFault ByteString)) -> IO MirrorView
viewAt format documents viewTarget readState companion = do
    pool <- newLocalPool 20 10_000_000
    cache <- newMirrorRetention pool 60 (StoreBudget 0 0)
    let store = MirrorState (\_ _ _ -> readState) (\_ _ _ _ -> pure (Left (StateInvalid "unexpected state write"))) (\_ _ -> pure (Left StateMissingCompanion)) (const companion)
    pure (newMirrorView defaultLimits viewTarget store cache format documents)

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
withMountOver eco adapter (queue, configure) view firstParty private public action =
    withUpstreams private public $ \privatePort publicPort deadlines manager -> do
        env <- newTestEnvWithQueue deadlines queue manager
        let deps =
                (serveDepsFor adapter (Just (loopbackRegistryUrl (localhost privatePort))) (loopbackRegistryUrl (localhost publicPort)) NoMirrorWrite [] (pure (UTCTime (fromGregorian 2026 10 11) 0)))
                    { pdMirrorState = Just view
                    , pdFirstParty = const firstParty
                    , pdMountBaseUrl = "http://ecluse.test/" <> ecosystemName eco
                    }
        action (application (mkServerConfig (maybeToList (mountBindingFor eco (configure deps) Nothing))) env)
