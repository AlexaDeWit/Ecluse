-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Exercise core serve handlers through their runtime ports.
Responses and emitted metrics remain observable independently of application wiring.
Admission lifetime cases connect sweep deletions to the next private request.
-}
module Ecluse.Core.Server.PipelineSpec (spec) where

import Data.Aeson (Value (Object, String), eitherDecode, eitherDecodeStrict, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian, nominalDay)
import Katip (LogEnv, closeScribes)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (hContentType, status200, status304, status401, status403, status404, statusCode)

import Ecluse.Core.Credential (ClientCredential (credSecret), bareCredential, mkSecret, unSecret)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (HashAlg (SHA512), PackageName, mkPackageName)
import Ecluse.Core.Package.Integrity (mkMinIntegrity)
import Ecluse.Core.Registry.Maintenance (ConsentVerdict (ConsentWithheld), StoreClass (StorePreserved), StoreFacts (factBackend), StoredVersion (StoredVersion, storedVersion), VersionPresence (VersionServed))
import Ecluse.Core.Registry.Npm.Credential (npmCredential)
import Ecluse.Core.Registry.Npm.Route (
    npmPackumentContract,
    npmPackumentReplies,
    npmRouter,
    npmTarballContract,
    npmTarballReplies,
 )
import Ecluse.Core.Registry.Request (CredentialMapping, credentialMapping)
import Ecluse.Core.Registry.Sweep (sweepCycle)
import Ecluse.Core.Registry.Sweep.Outcome (CycleOutcome (outcomeTally), SweepTally (tallyDeleted, tallyExamined))
import Ecluse.Core.Registry.Sweep.Types (SweepMount (smFirstParty), SweepPacing (swpShape), SweepShape (SweepCandidates, SweepEverything), deletingCache)
import Ecluse.Core.Rules (PreparedRule, RuleDeps (rdSourceReporter), evalRules, prepare)
import Ecluse.Core.Rules.Outage (OutageState (Healthy), SourceHealth (SourceAnswered), SourceReporter (noteAdmission, reportSource), sourceReporter, tvarOutageStore)
import Ecluse.Core.Rules.Types (PrecededRule, Rule (AllowIfOlderThan))
import Ecluse.Core.Rules.Types qualified as Rules
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission (ServeAdmission, newServeAdmission, newServeAdmissionTuned, withServeAdmission)
import Ecluse.Core.Server.Cache (Source (Source), cachedMetadata, newMetadataCache)
import Ecluse.Core.Server.Context (
    Handler,
    MountBinding (..),
    PackumentDeps (..),
    RequestCtx (RequestCtx),
    ServeRuntime (ServeRuntime, srMetadataCache, srMetrics),
    pdPrivateBaseUrl,
    pdPublicBaseUrl,
    runHandler,
 )
import Ecluse.Core.Server.Contract (ResponseContract, responseToWai)
import Ecluse.Core.Server.Pipeline (headTarball, servePackument, serveTarball)
import Ecluse.Core.Server.Pipeline.Publish ()
import Ecluse.Core.Server.Pipeline.Shared (hRetryAfter)
import Ecluse.Core.Server.Upstream (MirrorServePlan (MirrorOnAdmit))
import Ecluse.Core.Telemetry.Metrics (Decision (Admit, Deny, Unavailable), SweepResult (SweepDeleted), SweepTarget (SweepPrivate))
import Ecluse.Core.Telemetry.Record (MetricsPort)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Log (captureStdout, jsonLogEnv, newTestLogEnv)
import Ecluse.Test.Maintenance (FakeStore (fakeMaintenance, readFakeContents, writeFakeContents), FakeStoreConfig (fakeClass, fakeConsent, fakeContents, fakeFacts, fakeManifests), defaultFakeStoreConfig, newFakeStore)
import Ecluse.Test.Package (hexSha1Of, sampleDetails, sampleManifest, sriSha256Of, sriSha512Of, unsafeFilename)
import Ecluse.Test.Port (passthroughTracingPort, recordingDivergenceMetricsPort, recordingMetricsPort)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Registry.Npm (VersionSpec (..), packumentValue, versionSpec, versionValue)
import Ecluse.Test.Rules (admittedBy, atDefaultPrecedence, blockedBy, inertRuleDeps, isUndecidable)
import Ecluse.Test.Server.Cache (defaultCacheConfig)
import Ecluse.Test.Server.Mount (npmServeDeps, withPrivateBaseUrl)
import Ecluse.Test.Sweep (RecordedSweep (recPorts, recTargetResults), recordingPorts, testMount, testPacing, withPrivateCache)
import Network.HTTP.Types.Header (RequestHeaders, hHost)
import Network.Wai (Application, Request (rawPathInfo, requestHeaders), defaultRequest, responseHeaders, responseLBS, responseStatus)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Internal (Response (ResponseBuilder), ResponseReceived (ResponseReceived))
import Test.Hspec
import UnliftIO.Exception (bracket, throwIO)

-- | Pin client responses and metrics, including trusted reads after a policy change.
spec :: Spec
spec = describe "Ecluse.Core.Server.Pipeline (core handlers over a ServeRuntime)" $ do
    admissionLifetimeSpec
    cacheRetentionSpec
    divergenceEvidenceSpec
    skippedCheckAuditSpec
    distTagSpec
    sharedCacheSpec

    for_ [(status200, Admit), (status304, Admit), (status401, Deny), (status403, Deny)] $ \(upstreamStatus, expected) ->
        it ("records private artifact HTTP " <> show (statusCode upstreamStatus) <> " as " <> show expected <> " for GET and HEAD") $
            testWithApplication (pure (\_ respond -> respond (responseLBS upstreamStatus [] "private response"))) $ \port -> do
                (metricsPort, decisions) <- recordingMetricsPort
                rt <- mkRuntime metricsPort
                base <- depsFor 1
                let deps = withPrivateBaseUrl (Just (loopbackRegistryUrl ("http://localhost:" <> show port))) base
                for_ [serveTarball, headTarball] $ \serve -> do
                    response <- captureServe npmTarballContract rt (mountWith deps) (serve npmTarballReplies leftpad (mkVersion Npm "1.0.0") (unsafeFilename "leftpad-1.0.0.tgz") defaultRequest)
                    statusCode (responseStatus response) `shouldBe` if expected == Deny then 403 else statusCode upstreamStatus
                decisions `shouldReturn` [expected, expected]

    it "serves a merged packument and records an admit through the metrics port" $
        testWithApplication (pure upstreamApp) $ \port -> do
            (metricsPort, decisions) <- recordingMetricsPort
            rt <- mkRuntime metricsPort
            deps <- depsFor port
            resp <- captureServe npmPackumentContract rt (mountWith deps) (servePackument npmPackumentReplies leftpad defaultRequest)
            statusCode (responseStatus resp) `shouldBe` 200
            decisions >>= (`shouldBe` [Admit])

    it "records an unavailability and renders 503 when no upstream resolves" $ do
        (metricsPort, decisions) <- recordingMetricsPort
        rt <- mkRuntime metricsPort
        -- 'depsFor 1' points both origins at a closed port, which refuses each fetch.
        deps <- depsFor 1
        resp <- captureServe npmPackumentContract rt (mountWith deps) (servePackument npmPackumentReplies leftpad defaultRequest)
        statusCode (responseStatus resp) `shouldBe` 503
        decisions >>= (`shouldBe` [Unavailable])

    it "admits exactly the credential presentation the mount's ecosystem declares" $ do
        (metricsPort, _decisions) <- recordingMetricsPort
        rt <- mkRuntime metricsPort
        -- Closed upstream ports distinguish edge refusal from an admitted request's fetch failure.
        gated <- gatedDeps
        let serveUnder mapping headers =
                statusCode . responseStatus
                    <$> captureServe
                        npmPackumentContract
                        rt
                        (mountUnder mapping gated)
                        (servePackument npmPackumentReplies leftpad (requestWith headers))
        serveUnder npmCredential [("Authorization", "Bearer " <> edgeToken)] >>= (`shouldBe` 503)
        serveUnder npmCredential [("X-Api-Key", edgeToken)] >>= (`shouldBe` 401)
        serveUnder apiKeyCredential [("X-Api-Key", edgeToken)] >>= (`shouldBe` 503)
        serveUnder apiKeyCredential [("Authorization", "Bearer " <> edgeToken)] >>= (`shouldBe` 401)

    it "serves a gated tarball and records an admit, driving the metrics and tracing ports" $
        testWithApplication (pure upstreamApp) $ \port -> do
            (metricsPort, decisions) <- recordingMetricsPort
            rt <- mkRuntime metricsPort
            deps <- depsFor port
            resp <-
                captureServe
                    npmTarballContract
                    rt
                    (mountWith deps)
                    (serveTarball npmTarballReplies leftpad (mkVersion Npm "1.0.0") (unsafeFilename "leftpad-1.0.0.tgz") defaultRequest)
            statusCode (responseStatus resp) `shouldBe` 200
            decisions >>= (`shouldBe` [Admit])

    it "keeps a first-party packument off the public upstream, answering 503 when the private origin cannot be read" $ do
        (metricsPort, decisions) <- recordingMetricsPort
        rt <- mkRuntime metricsPort
        hits <- newIORef (0 :: Int)
        -- 'depsFor' points the private origin at a closed port, so this name's one authority
        -- never answers and the client is told to retry rather than that the name is gone.
        testWithApplication (pure (countingUpstream hits upstreamApp)) $ \port -> do
            base <- depsFor port
            let serveUnder firstParty =
                    captureServe
                        npmPackumentContract
                        rt
                        (mountWith base{pdFirstParty = firstParty})
                        (servePackument npmPackumentReplies leftpad defaultRequest)
            -- A cold cache makes a zero count prove that the public leg never ran.
            firstParty <- serveUnder (== leftpad)
            statusCode (responseStatus firstParty) `shouldBe` 503
            readIORef hits >>= (`shouldBe` 0)
            decisions >>= (`shouldBe` [Unavailable])
            thirdParty <- serveUnder (/= leftpad)
            statusCode (responseStatus thirdParty) `shouldBe` 200
            readIORef hits >>= (`shouldSatisfy` (> 0))
            decisions >>= (`shouldBe` [Unavailable, Admit])

    it "keeps a first-party artifact off the public upstream, answering 404 after a private miss" $ do
        (metricsPort, decisions) <- recordingMetricsPort
        rt <- mkRuntime metricsPort
        hits <- newIORef (0 :: Int)
        testWithApplication (pure (countingUpstream hits upstreamApp)) $ \port -> do
            base <- depsFor port
            let serveUnder firstParty =
                    captureServe
                        npmTarballContract
                        rt
                        (mountWith base{pdFirstParty = firstParty})
                        (serveTarball npmTarballReplies leftpad (mkVersion Npm "1.0.0") (unsafeFilename "leftpad-1.0.0.tgz") defaultRequest)
            firstParty <- serveUnder (== leftpad)
            statusCode (responseStatus firstParty) `shouldBe` 404
            readIORef hits >>= (`shouldBe` 0)
            decisions >>= (`shouldBe` [Deny])
            thirdParty <- serveUnder (/= leftpad)
            statusCode (responseStatus thirdParty) `shouldBe` 200
            readIORef hits >>= (`shouldSatisfy` (> 0))
            decisions >>= (`shouldBe` [Deny, Admit])

    it "sheds packument work when metadata admission refuses" $ do
        (metricsPort, _decisions) <- recordingMetricsPort
        -- No waiting room makes saturation refuse immediately.
        admission <- newServeAdmissionTuned 1 0 0
        rt <- mkRuntimeWith admission metricsPort
        deps <- depsFor 1
        held <- withServeAdmission (srMetrics rt) admission (captureServe npmPackumentContract rt (mountWith deps) (servePackument npmPackumentReplies leftpad defaultRequest))
        response <- maybe (throwIO MissingFixtureResponse) pure held
        statusCode (responseStatus response) `shouldBe` 503
        (snd <$> find ((== hRetryAfter) . fst) (responseHeaders response)) `shouldBe` Just "1"

    it "releases metadata admission after an admitted operation completes" $
        testWithApplication (pure upstreamApp) $ \port -> do
            (metricsPort, _decisions) <- recordingMetricsPort
            admission <- newServeAdmissionTuned 1 0 0
            rt <- mkRuntimeWith admission metricsPort
            deps <- depsFor port
            saturated <- withServeAdmission (srMetrics rt) admission (captureServe npmPackumentContract rt (mountWith deps) (servePackument npmPackumentReplies leftpad defaultRequest))
            (statusCode . responseStatus <$> saturated) `shouldBe` Just 503
            admitted <- captureServe npmPackumentContract rt (mountWith deps) (servePackument npmPackumentReplies leftpad defaultRequest)
            statusCode (responseStatus admitted) `shouldBe` 200

    it "sheds a tarball miss when its public metadata gate cannot acquire admission" $ do
        (metricsPort, _decisions) <- recordingMetricsPort
        admission <- newServeAdmissionTuned 1 0 0
        rt <- mkRuntimeWith admission metricsPort
        deps <- depsFor 1
        held <-
            withServeAdmission (srMetrics rt) admission $
                captureServe
                    npmTarballContract
                    rt
                    (mountWith deps)
                    (serveTarball npmTarballReplies leftpad (mkVersion Npm "1.0.0") (unsafeFilename "leftpad-1.0.0.tgz") defaultRequest)
        response <- maybe (throwIO MissingFixtureResponse) pure held
        statusCode (responseStatus response) `shouldBe` 503
        (snd <$> find ((== hRetryAfter) . fst) (responseHeaders response)) `shouldBe` Just "1"

    it "does not hold metadata admission around a trusted private tarball stream" $
        testWithApplication (pure upstreamApp) $ \port -> do
            (metricsPort, _decisions) <- recordingMetricsPort
            admission <- newServeAdmission 1
            rt <- mkRuntimeWith admission metricsPort
            deps <- depsFor 1
            let privateDeps = withPrivateBaseUrl (Just (loopbackRegistryUrl ("http://localhost:" <> show port))) deps
            held <-
                withServeAdmission (srMetrics rt) admission $
                    captureServe
                        npmTarballContract
                        rt
                        (mountWith privateDeps)
                        (serveTarball npmTarballReplies leftpad (mkVersion Npm "1.0.0") (unsafeFilename "leftpad-1.0.0.tgz") defaultRequest)
            (statusCode . responseStatus <$> held) `shouldBe` Just 200

divergenceEvidenceSpec :: Spec
divergenceEvidenceSpec = describe "validated divergence evidence across public rules" $ do
    it "keeps the warning and counter after a named public denial with both digests fixed" $
        withConflictOrigins (conflictPublicApp id) divergentPrivateApp $ \rt base divergences publicHits -> do
            denied <- prepare inertRuleDeps (atDefaultPrecedence Rules.DenyInstallTimeExecution : allowPolicy)
            -- The public tag names 2.0.0 while it survives; the deny leaves only the private 1.0.0.
            for_ [(base, "2.0.0", ["1.0.0", "2.0.0"], 1), (base{pdRules = denied}, "1.0.0", ["1.0.0"], 2)] $ \(deps, latest, keys, count) -> do
                logged <- captureStdout $ bracket jsonLogEnv (void . closeScribes) $ \logEnv -> do
                    resp <- captureServeWithLog logEnv npmPackumentContract rt (mountWith deps) (servePackument npmPackumentReplies leftpad defaultRequest)
                    assertPrivatePackument latest keys resp
                assertConflictLog True logged
                divergences `shouldReturn` count
            readIORef publicHits `shouldReturn` 1

    it "retains denied conflict evidence beside another admitted public version" $
        withConflictOrigins (conflictPublicApp (\v -> v{vsHasInstallScript = vsVersion v == "1.0.0"})) divergentPrivateApp $ \rt base divergences _ -> do
            denied <- prepare inertRuleDeps (atDefaultPrecedence Rules.DenyInstallTimeExecution : allowPolicy)
            logged <- captureStdout $ bracket jsonLogEnv (void . closeScribes) $ \logEnv -> do
                resp <- captureServeWithLog logEnv npmPackumentContract rt (mountWith base{pdRules = denied}) (servePackument npmPackumentReplies leftpad defaultRequest)
                assertPrivatePackument "2.0.0" ["1.0.0", "2.0.0"] resp
            assertConflictLog True logged
            divergences `shouldReturn` 1

    for_
        [ ("a malformed digest", \v -> v{vsIntegrity = Just "sha512-invalid"})
        , ("a missing digest", \v -> v{vsIntegrity = Nothing})
        , ("an unsupported digest", \v -> v{vsIntegrity = Just "sha999-AAAA"})
        , ("a below-floor digest", \v -> v{vsIntegrity = Nothing, vsShasum = Just (hexSha1Of artifactBytes)})
        , ("a refused artifact authority", \v -> v{vsTarballUrl = "https://unrelated.invalid/leftpad-1.0.0.tgz"})
        , ("malformed version metadata", \v -> v{vsExtraPairs = ["dist" .= ("invalid" :: Text)]})
        , ("an unshared algorithm", \v -> v{vsIntegrity = Just (sriSha256Of artifactBytes)})
        ]
        $ \(label, change) ->
            it ("does not alarm for " <> label <> " on a denied public copy") $
                withConflictOrigins (conflictPublicApp change) divergentPrivateApp $ \rt base divergences _ -> do
                    denied <- prepare inertRuleDeps (atDefaultPrecedence Rules.DenyInstallTimeExecution : allowPolicy)
                    logged <- captureStdout $ bracket jsonLogEnv (void . closeScribes) $ \logEnv -> do
                        resp <- captureServeWithLog logEnv npmPackumentContract rt (mountWith base{pdRules = denied}) (servePackument npmPackumentReplies leftpad defaultRequest)
                        assertPrivatePackument "1.0.0" ["1.0.0"] resp
                    assertConflictLog False logged
                    divergences `shouldReturn` 0

    it "excludes a denied SHA-256 copy when the public floor requires SHA-512" $
        withConflictOrigins (conflictPublicApp (\v -> v{vsIntegrity = Just (sriSha256Of artifactBytes)})) (privateAppWithIntegrity (sriSha256Of "private bytes")) $ \rt base divergences _ -> do
            floorSpec <- either (fail . toString) pure (mkMinIntegrity SHA512)
            denied <- prepare inertRuleDeps (atDefaultPrecedence Rules.DenyInstallTimeExecution : allowPolicy)
            logged <- captureStdout $ bracket jsonLogEnv (void . closeScribes) $ \logEnv -> do
                resp <- captureServeWithLog logEnv npmPackumentContract rt (mountWith base{pdRules = denied, pdMinIntegrity = floorSpec}) (servePackument npmPackumentReplies leftpad defaultRequest)
                assertTrustedPackument (sriSha256Of "private bytes") "1.0.0" ["1.0.0"] resp
            assertConflictLog False logged
            divergences `shouldReturn` 0

    it "does not fetch public conflict evidence for a first-party name" $
        withConflictOrigins (conflictPublicApp id) divergentPrivateApp $ \rt base divergences publicHits -> do
            resp <- captureServe npmPackumentContract rt (mountWith base{pdFirstParty = (== leftpad)}) (servePackument npmPackumentReplies leftpad defaultRequest)
            assertPrivatePackument "1.0.0" ["1.0.0"] resp
            divergences `shouldReturn` 0
            readIORef publicHits `shouldReturn` 0

    for_ [status401, status403] $ \refusal ->
        it ("keeps private HTTP " <> show (statusCode refusal) <> " authoritative without another public fetch") $
            withConflictOrigins (conflictPublicApp id) (\_ respond -> respond (responseLBS refusal [] "refused")) $ \rt deps divergences publicHits -> do
                resp <- captureServe npmPackumentContract rt (mountWith deps) (servePackument npmPackumentReplies leftpad defaultRequest)
                statusCode (responseStatus resp) `shouldBe` 403
                divergences `shouldReturn` 0
                readIORef publicHits `shouldReturn` 1

skippedCheckAuditSpec :: Spec
skippedCheckAuditSpec = describe "skipped-check evidence at the public artifact gate" $ do
    it "logs the skipped check once, at the artifact admission, and not for the packument that listed it" $
        testWithApplication (pure upstreamApp) $ \port -> do
            (metricsPort, _decisions) <- recordingMetricsPort
            rt <- mkRuntime metricsPort
            deps <- skippingDeps port
            logged <- captureStdout $ bracket jsonLogEnv (void . closeScribes) $ \logEnv -> do
                packument <- captureServeWithLog logEnv npmPackumentContract rt (mountWith deps) (servePackument npmPackumentReplies leftpad defaultRequest)
                statusCode (responseStatus packument) `shouldBe` 200
                tarball <- captureServeWithLog logEnv npmTarballContract rt (mountWith deps) (serveTarball npmTarballReplies leftpad (mkVersion Npm "1.0.0") (unsafeFilename "leftpad-1.0.0.tgz") defaultRequest)
                statusCode (responseStatus tarball) `shouldBe` 200
            let evidence = filter (T.isInfixOf "skipped for unavailability") (lines logged)
            length evidence `shouldBe` 1
            for_ ["\"sev\":\"Warning\"", "\"package\":\"leftpad\"", "\"version\":\"1.0.0\"", "\"rule\":\"DenyIfCve\"", "\"cause\":\"no advisory database loaded\""] $ \field ->
                logged `shouldSatisfy` T.isInfixOf field

    it "logs the line once per admission identity for the life of the outage, and again after recovery" $
        testWithApplication (pure twoVersionUpstream) $ \port -> do
            (metricsPort, _decisions) <- recordingMetricsPort
            rt <- mkRuntime metricsPort
            (reporter, deps) <- observedSkippingDeps port
            twoSkips <- prepare (ruleDepsOf reporter) (atDefaultPrecedence (Rules.DenyIfEpss (Rules.DenyIfEpssParams 0.5 Rules.FailNoDecision)) : skipPolicy)
            let serve logEnv d ver = do
                    tarball <- captureServeWithLog logEnv npmTarballContract rt (mountWith d) (serveTarball npmTarballReplies leftpad (mkVersion Npm ver) (unsafeFilename ("leftpad-" <> ver <> ".tgz")) defaultRequest)
                    statusCode (responseStatus tarball) `shouldBe` 200
                evidenceLines = length . filter (T.isInfixOf "skipped for unavailability") . lines
            logged <- captureStdout $ bracket jsonLogEnv (void . closeScribes) $ \logEnv -> do
                -- Five public serves of one version during one outage: one line.
                replicateM_ 5 (serve logEnv deps "1.0.0")
                -- Another version is its own identity.
                serve logEnv deps "2.0.0"
                -- The same version with another skipped rule set is its own identity too.
                serve logEnv deps{pdRules = twoSkips} "1.0.0"
                replicateM_ 3 (serve logEnv deps "1.0.0")
                -- Recovery, once every rule answers, clears the record, so the next admission
                -- with a skip logs again.
                for_ ["DenyIfCve", "DenyIfEpss"] (reportSource reporter . SourceAnswered)
                serve logEnv deps "1.0.0"
            -- Two skipped rules on one admission are two lines, so the third identity adds two.
            evidenceLines logged `shouldBe` 5
            length (filter (T.isInfixOf "\"version\":\"2.0.0\"") (lines logged)) `shouldBe` 1
            length (filter (T.isInfixOf "\"rule\":\"DenyIfEpss\"") (lines logged)) `shouldBe` 1

    it "logs nothing for a trusted private serve, which runs no rules" $
        testWithApplication (pure upstreamApp) $ \port -> do
            (metricsPort, _decisions) <- recordingMetricsPort
            rt <- mkRuntime metricsPort
            -- The public upstream sits on a closed port, so only the private copy can serve.
            deps <- withPrivateBaseUrl (Just (loopbackRegistryUrl ("http://localhost:" <> show port))) <$> skippingDeps 1
            logged <- captureStdout $ bracket jsonLogEnv (void . closeScribes) $ \logEnv ->
                replicateM_ 3 $ do
                    tarball <- captureServeWithLog logEnv npmTarballContract rt (mountWith deps) (serveTarball npmTarballReplies leftpad (mkVersion Npm "1.0.0") (unsafeFilename "leftpad-1.0.0.tgz") defaultRequest)
                    statusCode (responseStatus tarball) `shouldBe` 200
            logged `shouldSatisfy` (not . T.isInfixOf "skipped for unavailability")

-- The issue's reproduction over the fixtures: the quarantine allow beside an advisory deny set to
-- skip, with no advisory database loaded.
skippingDeps :: Int -> IO PackumentDeps
skippingDeps publicPort = do
    base <- depsFor publicPort
    skipping <- prepare inertRuleDeps skipPolicy
    pure base{pdRules = skipping}

skipPolicy :: [PrecededRule]
skipPolicy = atDefaultPrecedence (Rules.DenyIfCve (Rules.DenyIfCveParams 8.0 Rules.FailNoDecision)) : allowPolicy

-- 'skippingDeps' over a live outage reporter, so the gate's evidence line is bounded as the
-- composition root bounds it, with the reporter handed back so a case can recover the source.
observedSkippingDeps :: Int -> IO (SourceReporter, PackumentDeps)
observedSkippingDeps publicPort = do
    base <- depsFor publicPort
    shared <- newTVarIO Healthy
    let reporter = sourceReporter 900 (pure fixedNow) (tvarOutageStore shared) (const pass)
    skipping <- prepare (ruleDepsOf reporter) skipPolicy
    pure (reporter, base{pdRules = skipping, pdNoteAdmission = noteAdmission reporter})

ruleDepsOf :: SourceReporter -> RuleDeps
ruleDepsOf reporter = inertRuleDeps{rdSourceReporter = reporter}

-- 'upstreamApp' over two old versions, so a case can admit each.
twoVersionUpstream :: Application
twoVersionUpstream req respond =
    case rawPathInfo req of
        "/leftpad" ->
            respond (responseLBS status200 [(hContentType, "application/json")] (encode (privatePackumentOver host (sha512Integrity artifactBytes) ["1.0.0", "2.0.0"] "2.0.0")))
        path
            | "/leftpad/-/leftpad-" `BS.isPrefixOf` path ->
                respond (responseLBS status200 [(hContentType, "application/octet-stream")] (LBS.fromStrict artifactBytes))
        _ -> respond (responseLBS status404 [] "")
  where
    host = maybe "localhost" snd (find ((== hHost) . fst) (requestHeaders req))

distTagSpec :: Spec
distTagSpec = describe "served dist-tags.latest" $ do
    it "serves the greatest admitted version when the public latest is denied" $ do
        -- The public tag names 3.0.0, which the deny holds back before the merge sees it, and
        -- the store's own tag names 1.0.0. The ordering decides, so neither tag is served.
        denied <- prepare inertRuleDeps (atDefaultPrecedence Rules.DenyInstallTimeExecution : allowPolicy)
        let public = publicAppOver ["1.0.0", "2.0.0", "3.0.0"] "3.0.0" (\v -> v{vsHasInstallScript = vsVersion v == "3.0.0"})
        withConflictOrigins public (privateAppOver ["1.0.0"] "1.0.0") $ \rt base _divergences _ -> do
            resp <- captureServe npmPackumentContract rt (mountWith base{pdRules = denied}) (servePackument npmPackumentReplies leftpad defaultRequest)
            fields <- servedFields resp
            KeyMap.lookup "dist-tags" fields `shouldBe` Just (object ["latest" .= ("2.0.0" :: Text)])
            servedKeys fields `shouldBe` Just ["1.0.0", "2.0.0"]

    it "keeps a public latest held on an older version while its target survives" $ do
        -- The public maintainer holds latest at 1.0.0 though 2.0.0 is admitted, and the store
        -- tags 2.0.0. Maintainer intent wins over both the ordering and the store.
        let public = publicAppOver ["1.0.0", "2.0.0"] "1.0.0" id
        withConflictOrigins public (privateAppOver ["1.0.0", "2.0.0"] "2.0.0") $ \rt base _divergences _ -> do
            resp <- captureServe npmPackumentContract rt (mountWith base) (servePackument npmPackumentReplies leftpad defaultRequest)
            fields <- servedFields resp
            KeyMap.lookup "dist-tags" fields `shouldBe` Just (object ["latest" .= ("1.0.0" :: Text)])
            servedKeys fields `shouldBe` Just ["1.0.0", "2.0.0"]

sharedCacheSpec :: Spec
sharedCacheSpec = describe "the shared metadata cache across the two origins" $
    it "keeps the private origin out of the cache and answers the public origin from it" $ do
        privateHits <- newIORef 0
        let public = publicAppOver ["1.0.0", "2.0.0"] "2.0.0" id
            private = countingUpstream privateHits (privateAppOver ["1.0.0"] "1.0.0")
        withConflictOrigins public private $ \rt deps _divergences publicHits -> do
            replicateM_ 2 (captureServe npmPackumentContract rt (mountWith deps) (servePackument npmPackumentReplies leftpad defaultRequest))
            -- The private leg re-reads its upstream per request, so it never answers one caller
            -- from another's authorised read. The public leg reads once and then serves the cache.
            readIORef privateHits `shouldReturn` 2
            readIORef publicHits `shouldReturn` 1
            privateCached <- traverse (cachedUnder rt) (pdPrivateBaseUrl deps)
            privateCached `shouldBe` Just False
            cachedUnder rt (pdPublicBaseUrl deps) `shouldReturn` True

-- Whether the shared cache holds a full-document entry under an origin's own key.
cachedUnder :: ServeRuntime -> RegistryUrl -> IO Bool
cachedUnder rt baseUrl = isJust <$> cachedMetadata (srMetadataCache rt) (Source (registryUrlText baseUrl)) leftpad

withConflictOrigins :: Application -> Application -> (ServeRuntime -> PackumentDeps -> IO Int -> IORef Int -> IO ()) -> IO ()
withConflictOrigins public private action = do
    publicHits <- newIORef 0
    testWithApplication (pure (countingUpstream publicHits public)) $ \publicPort ->
        testWithApplication (pure private) $ \privatePort -> do
            (metrics, divergences) <- recordingDivergenceMetricsPort
            rt <- mkRuntime metrics
            base <- depsFor publicPort
            action rt (withPrivateBaseUrl (Just (loopbackRegistryUrl ("http://localhost:" <> show privatePort))) base) divergences publicHits

assertConflictLog :: Bool -> Text -> Expectation
assertConflictLog expected logged = do
    entries <- traverse (either fail pure . eitherDecodeStrict . encodeUtf8) (T.lines logged)
    let conflicts = filter (T.isInfixOf "cross-upstream integrity divergence" . decodeUtf8 . LBS.toStrict . encode) (entries :: [Value])
    length conflicts `shouldBe` if expected then 1 else 0
    for_ conflicts $ \entry -> case entry of
        Object fields -> do
            KeyMap.lookup "sev" fields `shouldBe` Just (String "Warning")
            let encoded = decodeUtf8 (LBS.toStrict (encode entry))
                expectedMessage =
                    "cross-upstream integrity divergence: the trusted copy of leftpad is served, but a public copy contradicts it on a shared integrity algorithm for 1 version(s): 1.0.0 (trusted {leftpad-1.0.0.tgz sha512:"
                        <> T.drop 7 (sha512Integrity "leftpad artifact bytes (privately tampered)")
                        <> "} vs public {leftpad-1.0.0.tgz sha512:"
                        <> T.drop 7 (sha512Integrity artifactBytes)
                        <> "})"
            KeyMap.lookup "msg" fields `shouldBe` Just (String expectedMessage)
            encoded `shouldSatisfy` T.isInfixOf "\"package\":\"leftpad\""
            encoded `shouldSatisfy` T.isInfixOf "\"versions\":\"1.0.0\""
        _ -> expectationFailure "a structured log entry must be an object"

-- The served document's top-level fields.
servedFields :: Response -> IO (KeyMap.KeyMap Value)
servedFields resp = do
    value <- case resp of
        ResponseBuilder _ _ builder -> either fail pure (eitherDecode (toLazyByteString builder))
        _ -> fail "expected a packument response builder"
    case value of
        Object fields -> pure fields
        _ -> fail "expected a packument object"

-- The served version keys, sorted. 'Nothing' when the document carries no version object.
servedKeys :: KeyMap.KeyMap Value -> Maybe [Text]
servedKeys fields = case KeyMap.lookup "versions" fields of
    Just (Object versions) -> Just (sort (map Key.toText (KeyMap.keys versions)))
    _ -> Nothing

assertPrivatePackument :: Text -> [Text] -> Response -> Expectation
assertPrivatePackument = assertTrustedPackument (sha512Integrity "leftpad artifact bytes (privately tampered)")

assertTrustedPackument :: Text -> Text -> [Text] -> Response -> Expectation
assertTrustedPackument integrity latest keys resp = do
    statusCode (responseStatus resp) `shouldBe` 200
    fields <- servedFields resp
    KeyMap.lookup "dist-tags" fields `shouldBe` Just (object ["latest" .= latest])
    servedKeys fields `shouldBe` Just keys
    case KeyMap.lookup "versions" fields of
        Just (Object versions) ->
            KeyMap.lookup "1.0.0" versions
                `shouldBe` Just (versionValue ((versionSpec "leftpad" "1.0.0" "http://proxy.test/leftpad/-/leftpad-1.0.0.tgz"){vsIntegrity = Just integrity, vsExtraPairs = ["_retained" .= ("private field" :: Text)]}))
        _ -> expectationFailure "expected served version objects"

conflictPublicApp :: (VersionSpec -> VersionSpec) -> Application
conflictPublicApp change = publicAppOver ["1.0.0", "2.0.0"] "2.0.0" (change . withInstallScript)
  where
    withInstallScript v = v{vsHasInstallScript = True}

-- A public document over the given versions, tagged at the named one. Every version predates the
-- age rule, so only a named deny holds one back.
publicAppOver :: [Text] -> Text -> (VersionSpec -> VersionSpec) -> Application
publicAppOver versions latest change req respond =
    respond (responseLBS status200 [(hContentType, "application/json")] (encode document))
  where
    host = maybe "localhost" snd (find ((== hHost) . fst) (requestHeaders req))
    entry ver = versionValue (change ((versionSpec "leftpad" ver ("http://" <> decodeUtf8 host <> "/leftpad/-/leftpad-" <> ver <> ".tgz")){vsIntegrity = Just (sha512Integrity artifactBytes)}))
    document = packumentValue "leftpad" latest [(ver, entry ver) | ver <- versions] [Key.fromText ver .= oldPublishTime | ver <- versions] []

admissionLifetimeSpec :: Spec
admissionLifetimeSpec = describe "admission lifetime after removing an allow" $
    for_ [SweepCandidates, SweepEverything] $ \shape -> describe (show shape) $ do
        it "retains the copy and serves the next private GET when the only allow disappears" $
            checkLifetime shape (LifetimePolicy [] (== Rules.BlockedByDefault []) Retained) Sweepable
        it "removes the copy and denies the next GET when an existing identity deny becomes decisive" $
            checkLifetime shape winningDeny Sweepable
        it "keeps trusting the copy when an unchanged higher-priority allow still beats the deny" $
            checkLifetime
                shape
                (LifetimePolicy [Rules.PrecededRule 600 (Rules.AllowByIdentity "leftpad"), identityDeny] ((== Just "AllowByIdentity") . admittedBy) Retained)
                Sweepable
        it "retains the copy when unavailable advisory evidence wins ahead of the existing deny" $
            checkLifetime
                shape
                (LifetimePolicy [unavailableCveDeny, identityDeny] isUndecidable Retained)
                Sweepable
        it "retains the copy when unavailable advisory evidence is the only remaining rule" $
            checkLifetime
                shape
                (LifetimePolicy [unavailableCveDeny] isUndecidable Retained)
                Sweepable
        for_ [FirstParty, ConsentMissing, TargetPreserved] $ \guard' ->
            it ("retains the denied copy and serves the next private GET under " <> show guard') $
                checkLifetime shape winningDeny guard'
        it "removes a version whose manifest the store no longer serves, so the next private GET is denied" $
            checkLifetime shape winningDeny ManifestMissing
        it "retains a version with no manifest when no exact deny wins, so the next private GET serves it" $
            checkLifetime
                shape
                (LifetimePolicy [unavailableCveDeny] isUndecidable Retained)
                ManifestMissing

{- The cache a next read refills. The trusted private leg serves a hit untouched, so the read puts
the version back whatever the rules say, and the next grouped cycle decides on that state. -}
cacheRetentionSpec :: Spec
cacheRetentionSpec = describe "a private read that retains a version the cycle removed" $
    it "serves the read untouched, removes the copy it restored, and keeps the version policy allows" $ do
        mirror <- newFakeStore (retentionStore "mirror" [deniedVersion, keptVersion])
        cache <- newFakeStore (retentionStore "cache" [])
        rules <- prepare inertRuleDeps [identityDeny]
        let mount = withPrivateCache (deletingCache (fakeMaintenance cache)) (testMount (fakeMaintenance mirror) rules [Rules.prRule identityDeny])
        cleared <- recordingPorts Nothing
        _ <- sweepCycle testPacing (recPorts cleared) [mount]
        storedVersions mirror `shouldReturn` [keptVersion]
        storedVersions cache `shouldReturn` []
        nextPrivateGet (retainingUpstream cache) rules True
        storedVersions cache `shouldReturn` [deniedVersion]
        reconciled <- recordingPorts Nothing
        _ <- sweepCycle testPacing (recPorts reconciled) [mount]
        recTargetResults reconciled >>= (`shouldSatisfy` elem (SweepPrivate, SweepDeleted))
        storedVersions cache `shouldReturn` []
        storedVersions mirror `shouldReturn` [keptVersion]

-- Each store answers under its own backend name, because a grouped cycle keys its inventories on it.
retentionStore :: Text -> [Version] -> FakeStoreConfig
retentionStore backend versions =
    defaultFakeStoreConfig
        { fakeContents = Map.singleton leftpad [StoredVersion item VersionServed Nothing | item <- versions]
        , fakeManifests = Map.singleton leftpad (sampleManifest leftpad [deniedVersion, keptVersion])
        , fakeFacts = (fakeFacts defaultFakeStoreConfig){factBackend = backend}
        }

-- A cache that retains what it serves, the way a read through an upstream relationship refills one.
retainingUpstream :: FakeStore -> Application
retainingUpstream store req respond = do
    writeFakeContents store (Map.singleton leftpad [StoredVersion deniedVersion VersionServed Nothing])
    upstreamApp req respond

storedVersions :: FakeStore -> IO [Version]
storedVersions store = map storedVersion . Map.findWithDefault [] leftpad <$> readFakeContents store

deniedVersion, keptVersion :: Version
deniedVersion = mkVersion Npm "1.0.0"
keptVersion = mkVersion Npm "2.0.0"

data CopyDisposition = Retained | Removed
    deriving stock (Eq)

{- The store's shape for one cycle: what holds a delete back, and whether its metadata read answers.
An unread manifest is evidence rather than a guard, so a rule reading only identity still decides. -}
data LifetimeStore = Sweepable | FirstParty | ConsentMissing | TargetPreserved | ManifestMissing
    deriving stock (Eq, Show)

-- The store states that stop a delete before any rule runs.
stopsBeforeRules :: LifetimeStore -> Bool
stopsBeforeRules = \case
    FirstParty -> True
    ConsentMissing -> True
    TargetPreserved -> True
    Sweepable -> False
    ManifestMissing -> False

data LifetimePolicy = LifetimePolicy
    { lpRules :: [PrecededRule]
    , lpDecision :: Rules.Decision -> Bool
    , lpDisposition :: CopyDisposition
    }

identityDeny :: PrecededRule
identityDeny = atDefaultPrecedence (Rules.DenyByIdentity "leftpad@1.0.0")

unavailableCveDeny :: PrecededRule
unavailableCveDeny = Rules.PrecededRule 600 (Rules.DenyIfCve (Rules.DenyIfCveParams 0 Rules.FailDeny))

winningDeny :: LifetimePolicy
winningDeny = LifetimePolicy [identityDeny] ((== Just "DenyByIdentity") . blockedBy) Removed

checkLifetime :: SweepShape -> LifetimePolicy -> LifetimeStore -> Expectation
checkLifetime shape policy storeState = do
    ctx <- Rules.mkEvalContext (pure fixedNow) (pure Nothing)
    let version = mkVersion Npm "1.0.0"
        details = sampleDetails leftpad version
        initialPolicy = Rules.PrecededRule 700 (Rules.AllowByIdentity "leftpad@1.0.0") : lpRules policy
    initial <- prepare inertRuleDeps initialPolicy
    admittedBy <$> evalRules ctx initial (Rules.completeEvidence details) `shouldReturn` Just "AllowByIdentity"
    store <- newFakeStore (lifetimeStoreConfig storeState)
    preparedAfter <- prepare inertRuleDeps (lpRules policy)
    evalRules ctx preparedAfter (Rules.completeEvidence details) >>= (`shouldSatisfy` lpDecision policy)
    recorded <- recordingPorts Nothing
    let mount =
            (testMount (fakeMaintenance store) preparedAfter (map Rules.prRule (lpRules policy)))
                { smFirstParty = \name -> storeState == FirstParty && name == leftpad
                }
    outcome <- sweepCycle testPacing{swpShape = shape} (recPorts recorded) [mount]
    let retained = stopsBeforeRules storeState || lpDisposition policy == Retained
        expectedVersions = [StoredVersion version VersionServed Nothing | retained]
        examined
            | stopsBeforeRules storeState = 0
            | shape == SweepCandidates && identityDeny `notElem` lpRules policy = 0
            | otherwise = 1
    tallyExamined (outcomeTally outcome) `shouldBe` examined
    tallyDeleted (outcomeTally outcome) `shouldBe` if retained then 0 else 1
    Map.lookup leftpad <$> readFakeContents store `shouldReturn` Just expectedVersions
    nextPrivateGet (storedUpstream store) preparedAfter retained

lifetimeStoreConfig :: LifetimeStore -> FakeStoreConfig
lifetimeStoreConfig storeState = case storeState of
    ConsentMissing -> seeded{fakeConsent = ConsentWithheld "operator withdrew consent"}
    TargetPreserved -> seeded{fakeClass = StorePreserved "the store has an upstream"}
    ManifestMissing -> seeded{fakeManifests = Map.empty}
    _ -> seeded
  where
    version = mkVersion Npm "1.0.0"
    seeded =
        defaultFakeStoreConfig
            { fakeContents = Map.singleton leftpad [StoredVersion version VersionServed Nothing]
            , fakeManifests = Map.singleton leftpad (sampleManifest leftpad [version])
            }

nextPrivateGet :: Application -> [PreparedRule] -> Bool -> Expectation
nextPrivateGet privateUpstream rules retained = do
    publicHits <- newIORef (0 :: Int)
    privateHits <- newIORef (0 :: Int)
    testWithApplication (pure (countingUpstream publicHits upstreamApp)) $ \publicPort ->
        testWithApplication (pure (countingUpstream privateHits privateUpstream)) $ \privatePort -> do
            (metricsPort, decisions) <- recordingMetricsPort
            rt <- mkRuntime metricsPort
            base <- depsFor publicPort
            let deps = withPrivateBaseUrl (Just (loopbackRegistryUrl ("http://localhost:" <> show privatePort))) base{pdRules = rules}
            response <-
                captureServe
                    npmTarballContract
                    rt
                    (mountWith deps)
                    (serveTarball npmTarballReplies leftpad (mkVersion Npm "1.0.0") (unsafeFilename "leftpad-1.0.0.tgz") defaultRequest)
            statusCode (responseStatus response) `shouldBe` if retained then 200 else 403
            decisions `shouldReturn` [if retained then Admit else Deny]
            readIORef privateHits `shouldReturn` 1
            readIORef publicHits `shouldReturn` if retained then 0 else 1

-- The private registry reads the same inventory that the sweep's delete capability mutates.
storedUpstream :: FakeStore -> Application
storedUpstream store req respond = do
    contents <- readFakeContents store
    if StoredVersion (mkVersion Npm "1.0.0") VersionServed Nothing `elem` Map.findWithDefault [] leftpad contents
        then upstreamApp req respond
        else respond (responseLBS status404 [] "")

captureServe :: ResponseContract response -> ServeRuntime -> MountBinding -> ((response -> IO ResponseReceived) -> Handler ResponseReceived) -> IO Response
captureServe contract rt binding mkHandler = do
    logEnv <- newTestLogEnv
    captureServeWithLog logEnv contract rt binding mkHandler

captureServeWithLog :: LogEnv -> ResponseContract response -> ServeRuntime -> MountBinding -> ((response -> IO ResponseReceived) -> Handler ResponseReceived) -> IO Response
captureServeWithLog logEnv contract rt binding mkHandler = do
    captured <- newIORef Nothing
    let respond value = writeIORef captured (Just (responseToWai contract value)) >> pure ResponseReceived
    _ <- runHandler logEnv mempty (RequestCtx rt binding) (mkHandler respond)
    maybe (throwIO MissingFixtureResponse) pure =<< readIORef captured

-- A missing callback result is a fixture failure, never an upstream outcome.
data MissingFixtureResponse = MissingFixtureResponse
    deriving stock (Show)

instance Exception MissingFixtureResponse

mkRuntime :: MetricsPort -> IO ServeRuntime
mkRuntime metricsPort = do
    -- Capacity high enough that this handle never gates. The admission cases wrap
    -- 'withServeAdmission' with their own tuned handle.
    admission <- newServeAdmission 1_000_000
    mkRuntimeWith admission metricsPort

mkRuntimeWith :: ServeAdmission -> MetricsPort -> IO ServeRuntime
mkRuntimeWith admission metricsPort = do
    manager <- newManager defaultManagerSettings
    cache <- newMetadataCache defaultCacheConfig
    queue <- newTestMemoryQueue
    pure (ServeRuntime admission manager manager cache queue metricsPort passthroughTracingPort)

leftpad :: PackageName
leftpad = mkPackageName Npm Nothing "leftpad"

mountWith :: PackumentDeps -> MountBinding
mountWith = mountUnder npmCredential

mountUnder :: CredentialMapping -> PackumentDeps -> MountBinding
mountUnder mapping deps =
    MountBinding
        { bindingPrefix = "npm" :| []
        , bindingRouter = npmRouter
        , bindingCredential = mapping
        , bindingPackumentDeps = deps
        , bindingPublishDeps = Nothing
        }

apiKeyCredential :: CredentialMapping
apiKeyCredential = credentialMapping recoverApiKey "X-Api-Key" (encodeUtf8 . unSecret . credSecret)
  where
    recoverApiKey headers = bareCredential . mkSecret . decodeUtf8 <$> lookup "X-Api-Key" headers

edgeToken :: (IsString s) => s
edgeToken = "edge-token"

-- Closed upstream ports distinguish an edge refusal from a fetch failure.
gatedDeps :: IO PackumentDeps
gatedDeps = do
    base <- depsFor 1
    pure base{pdInboundToken = Just (mkSecret edgeToken)}

requestWith :: RequestHeaders -> Request
requestWith headers = defaultRequest{requestHeaders = headers}

depsFor :: Int -> IO PackumentDeps
depsFor publicPort = do
    prepared <- prepare inertRuleDeps allowPolicy
    pure
        ( npmServeDeps
            (Just (loopbackRegistryUrl "http://localhost:1"))
            (loopbackRegistryUrl ("http://localhost:" <> show publicPort))
            (MirrorOnAdmit (loopbackRegistryUrl "http://mirror.test"))
            prepared
            (pure fixedNow)
        )
            { pdMountBaseUrl = "http://proxy.test"
            , pdEgressUrl = Right . loopbackRegistryUrl
            }

allowPolicy :: [PrecededRule]
allowPolicy = [atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))]

fixedNow :: UTCTime
fixedNow = UTCTime (fromGregorian 2020 1 1) 0

upstreamApp :: Application
upstreamApp req respond =
    case rawPathInfo req of
        "/leftpad" ->
            respond (responseLBS status200 [(hContentType, "application/json")] (encode (packumentFor host)))
        "/leftpad/-/leftpad-1.0.0.tgz" ->
            respond (responseLBS status200 [(hContentType, "application/octet-stream")] (LBS.fromStrict artifactBytes))
        _ -> respond (responseLBS status404 [] "")
  where
    host = maybe "localhost" snd (find ((== hHost) . fst) (requestHeaders req))

countingUpstream :: IORef Int -> Application -> Application
countingUpstream hits app req respond = modifyIORef' hits (+ 1) >> app req respond

artifactBytes :: ByteString
artifactBytes = "leftpad artifact bytes"

packumentWithIntegrity :: ByteString -> Text -> Value
packumentWithIntegrity host integrity = privatePackumentOver host integrity ["1.0.0"] "1.0.0"

-- A private document over the given versions, tagged at the named one.
privatePackumentOver :: ByteString -> Text -> [Text] -> Text -> Value
privatePackumentOver host integrity versions latest =
    packumentValue
        "leftpad"
        latest
        [(ver, entry ver) | ver <- versions]
        [Key.fromText ver .= oldPublishTime | ver <- versions]
        []
  where
    entry ver =
        versionValue
            ( (versionSpec "leftpad" ver ("http://" <> decodeUtf8 host <> "/leftpad/-/leftpad-" <> ver <> ".tgz"))
                { vsIntegrity = Just integrity
                , vsExtraPairs = ["_retained" .= ("private field" :: Text)]
                }
            )

oldPublishTime :: Text
oldPublishTime = "2019-01-01T00:00:00.000Z"

packumentFor :: ByteString -> Value
packumentFor host = packumentWithIntegrity host (sha512Integrity artifactBytes)

sha512Integrity :: ByteString -> Text
sha512Integrity = sriSha512Of

divergentPrivateApp :: Application
divergentPrivateApp = privateAppWithIntegrity (sha512Integrity "leftpad artifact bytes (privately tampered)")

privateAppWithIntegrity :: Text -> Application
privateAppWithIntegrity integrity = privateAppAt integrity ["1.0.0"] "1.0.0"

-- A private upstream serving the given versions under its own latest tag, with digests that
-- agree with the public copy, so nothing diverges.
privateAppOver :: [Text] -> Text -> Application
privateAppOver = privateAppAt (sha512Integrity artifactBytes)

privateAppAt :: Text -> [Text] -> Text -> Application
privateAppAt integrity versions latest req respond =
    case rawPathInfo req of
        "/leftpad" ->
            respond (responseLBS status200 [(hContentType, "application/json")] (encode (privatePackumentOver host integrity versions latest)))
        _ -> respond (responseLBS status404 [] "")
  where
    host = maybe "localhost" snd (find ((== hHost) . fst) (requestHeaders req))
