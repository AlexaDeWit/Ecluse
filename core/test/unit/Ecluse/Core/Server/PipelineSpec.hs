-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Client responses and metrics through core handlers and ecosystem routes.
module Ecluse.Core.Server.PipelineSpec (spec) where

import Data.Aeson (Value (Bool, Object, String), eitherDecode, eitherDecodeStrict, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian, nominalDay)
import Network.HTTP.Types (Method, Status, hContentType, methodGet, methodHead, status200, status206, status302, status304, status400, status401, status403, status404, status429, status500, status503, statusCode)

import Ecluse.Core.Credential (ClientCredential (credSecret), bareCredential, mkSecret, unSecret)
import Ecluse.Core.Cve (AdvisoryRange (AdvisoryRange))
import Ecluse.Core.Cve.Types (DbEtag (DbEtag))
import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Mirror.State (stateTarget)
import Ecluse.Core.Osv.Types (UpperBound (Unbounded))
import Ecluse.Core.Package (HashAlg (SHA512))
import Ecluse.Core.Package.Integrity (mkMinIntegrity)
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (artifactHosts))
import Ecluse.Core.Registry.Maintenance (ConsentVerdict (ConsentWithheld), StoreClass (StorePreserved), StoreFacts (factBackend))
import Ecluse.Core.Registry.Metadata (VersionDoc (vdRaw), VersionRead (vrVersion))
import Ecluse.Core.Registry.Npm.Credential (npmCredential)
import Ecluse.Core.Registry.Npm.Route (npmRouter)
import Ecluse.Core.Registry.Npm.Route.Internal (
    npmPackumentContract,
    npmPackumentReplies,
    npmTarballContract,
    npmTarballReplies,
 )
import Ecluse.Core.Registry.PyPI.Credential (pypiCredential)
import Ecluse.Core.Registry.PyPI.Mirror (pypiMirrorDocuments)
import Ecluse.Core.Registry.PyPI.Route (pypiRouter)
import Ecluse.Core.Registry.Request (CredentialMapping, credentialMapping)
import Ecluse.Core.Registry.Sweep (sweepCycle)
import Ecluse.Core.Registry.Sweep.Outcome (CycleOutcome (outcomeTally), SweepTally (tallyDeleted, tallyExamined))
import Ecluse.Core.Registry.Sweep.Types (SweepMount (smFirstParty), SweepPacing (swpShape), SweepShape (SweepCandidates, SweepEverything), deletingCache)
import Ecluse.Core.Rules (PreparedRule, RuleDeps (rdSourceReporter), prepare)
import Ecluse.Core.Rules.Outage (OutageState (Healthy), SourceHealth (SourceAnswered), SourceReporter (noteAdmission, reportSource), sourceReporter, tvarOutageStore)
import Ecluse.Core.Rules.Types (PrecededRule, Rule (AllowIfOlderThan))
import Ecluse.Core.Rules.Types qualified as Rules
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission (newServeAdmission, newServeAdmissionTuned, withOverflow, withServeAdmission)
import Ecluse.Core.Server.Admission.Meter (MemoryMeter, MeterSettings (..), meterSnapshot, newMemoryMeter, withMemoryEntry)
import Ecluse.Core.Server.Admission.Overflow (newOverflowTuned)
import Ecluse.Core.Server.Admission.Types (MeterSnapshot (snChargedBytes))
import Ecluse.Core.Server.Cache (Source (Source))
import Ecluse.Core.Server.Cache.VersionWeight (weighVersion)
import Ecluse.Core.Server.Context (
    MountBinding (..),
    PackumentDeps (..),
    RequestCtx (RequestCtx),
    ResponseAction (RunPipeline),
    RouteAction (RouteAction),
    ServeRuntime (srMemoryMeter, srMetadataCache, srMetrics, srPublicControl),
    pdPrivateBaseUrl,
    pdPublicBaseUrl,
    runHandler,
 )
import Ecluse.Core.Server.Contract (responseToWai)
import Ecluse.Core.Server.Pipeline (headTarball, servePackument, serveTarball)
import Ecluse.Core.Server.Pipeline.Publish ()
import Ecluse.Core.Server.Pipeline.Shared (hRetryAfter)
import Ecluse.Core.Server.Upstream (MirrorServePlan (MirrorOnAdmit, NoMirrorWrite))
import Ecluse.Core.Telemetry.Metrics (Decision (Admit, Deny, Unavailable), SweepResult (SweepDeleted), SweepTarget (SweepPrivate))
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (mpCacheRequest, mpMemoryAdmissionOverdraw, mpRuleEvalDuration))
import Ecluse.Core.Telemetry.Upstream qualified as PublicRead
import Ecluse.Core.Version (Version)
import Ecluse.Test.Cve (fakeCveLookup)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Log (captureJsonLog, newTestLogEnv)
import Ecluse.Test.Maintenance (
    FakeStore (fakeMaintenance, readFakeContents, writeFakeContents),
    FakeStoreConfig (fakeClass, fakeConsent, fakeFacts, fakeManifests),
    defaultFakeStoreConfig,
    heldVersions,
    newFakeStore,
    seededStoreConfig,
    servedVersion,
    servedVersions,
 )
import Ecluse.Test.Mirror.State (mirrorViewWith)
import Ecluse.Test.Package (hexSha1Of, leftpadName, npmVersion, requestsName, sampleDetails, sampleManifest, sriSha256Of, sriSha512Of, unsafeFilename)
import Ecluse.Test.Port (noopMetricsPort, recordingDivergenceMetricsPort, recordingMetricsPort)
import Ecluse.Test.Registry.Npm (VersionSpec (..), packumentValue, versionSpec, versionValue)
import Ecluse.Test.Registry.Public (PublicFixture (..), newPublicFixture)
import Ecluse.Test.Registry.PyPI (simpleFile, simpleIndex, withFileKeys)
import Ecluse.Test.Rules (admittedBy, atDefaultPrecedence, atPrecedence, blockedBy, evalRules, inertRuleDeps, isUndecidable, servingRuleDeps)
import Ecluse.Test.Server.Cache (cachedMetadata, cachedVersion)
import Ecluse.Test.Server.Handler (MissingFixtureResponse (MissingFixtureResponse), captureServe, captureServeWithLog, responseBytes, testServeRuntime, testServeRuntimeWith)
import Ecluse.Test.Server.Mount (npmServeDeps, pypiServeDeps, withPrivateBaseUrl)
import Ecluse.Test.Sweep (RecordedSweep (recPorts, recTargetResults), recordingPorts, testMount, testPacing, withPrivateCache)
import Ecluse.Test.Wai (countingUpstream, selfBaseUrl)
import Network.HTTP.Types.Header (RequestHeaders, hAcceptEncoding, hConnection, hContentEncoding, hContentRange, hDate, hETag, hHost, hIfNoneMatch)
import Network.Wai (Application, Request (rawPathInfo, requestHeaders, requestMethod), defaultRequest, mapResponseHeaders, responseHeaders, responseLBS, responseStatus)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Internal (Response)
import Network.Wai.Test (SResponse (..), runSession)
import Network.Wai.Test qualified as WaiTest (request)
import Test.Hspec
import UnliftIO (concurrently, mapConcurrently, timeout)
import UnliftIO.Exception (throwIO)

-- | Pin client responses and metrics, including trusted reads after a policy change.
spec :: Spec
spec = describe "Ecluse.Core.Server.Pipeline (core handlers over a ServeRuntime)" $ do
    admissionLifetimeSpec
    memoryAdmissionSpec
    overflowRouteSpec
    cacheRetentionSpec
    divergenceEvidenceSpec
    skippedCheckAuditSpec
    distTagSpec
    sharedCacheSpec
    pypiPrivateArtifactSpec
    publicControlSpec

    for_ [(status200, Admit), (status304, Admit), (status401, Deny), (status403, Deny)] $ \(upstreamStatus, expected) ->
        it ("records private artifact HTTP " <> show (statusCode upstreamStatus) <> " as " <> show expected <> " for GET and HEAD") $
            testWithApplication (pure (\_ respond -> respond (responseLBS upstreamStatus [] "private response"))) $ \port -> withSweeper $ \deadlines -> do
                (metricsPort, decisions) <- recordingMetricsPort
                rt <- testServeRuntime deadlines metricsPort
                base <- depsFor 1
                let deps = withPrivateBaseUrl (Just (loopbackRegistryUrl ("http://localhost:" <> show port))) base
                for_ [serveTarball, headTarball] $ \serve -> do
                    response <- captureServe npmTarballContract rt (mountWith deps) (serve npmTarballReplies leftpadName (npmVersion "1.0.0") (unsafeFilename "leftpad-1.0.0.tgz") defaultRequest)
                    statusCode (responseStatus response) `shouldBe` if expected == Deny then 403 else statusCode upstreamStatus
                decisions `shouldReturn` [expected, expected]

    it "serves a merged packument and records an admit through the metrics port" $
        testWithApplication (pure upstreamApp) $ \port -> withSweeper $ \deadlines -> do
            (metricsPort, decisions) <- recordingMetricsPort
            rt <- testServeRuntime deadlines metricsPort
            deps <- depsFor port
            resp <- captureServe npmPackumentContract rt (mountWith deps) (servePackument npmPackumentReplies leftpadName defaultRequest)
            statusCode (responseStatus resp) `shouldBe` 200
            decisions >>= (`shouldBe` [Admit])

    it "records an unavailability and renders 503 when no upstream resolves" $ withSweeper $ \deadlines -> do
        (metricsPort, decisions) <- recordingMetricsPort
        rt <- testServeRuntime deadlines metricsPort
        deps <- depsFor 1
        resp <- captureServe npmPackumentContract rt (mountWith deps) (servePackument npmPackumentReplies leftpadName defaultRequest)
        statusCode (responseStatus resp) `shouldBe` 503
        decisions >>= (`shouldBe` [Unavailable])

    it "admits exactly the credential presentation the mount's ecosystem declares" $ withSweeper $ \deadlines -> do
        (metricsPort, _decisions) <- recordingMetricsPort
        rt <- testServeRuntime deadlines metricsPort
        -- Closed upstream ports distinguish edge refusal from an admitted request's fetch failure.
        gated <- gatedDeps
        let serveUnder mapping headers =
                statusCode . responseStatus
                    <$> captureServe
                        npmPackumentContract
                        rt
                        (mountUnder mapping gated)
                        (servePackument npmPackumentReplies leftpadName (requestWith headers))
        serveUnder npmCredential [("Authorization", "Bearer " <> edgeToken)] >>= (`shouldBe` 503)
        serveUnder npmCredential [("X-Api-Key", edgeToken)] >>= (`shouldBe` 401)
        serveUnder apiKeyCredential [("X-Api-Key", edgeToken)] >>= (`shouldBe` 503)
        serveUnder apiKeyCredential [("Authorization", "Bearer " <> edgeToken)] >>= (`shouldBe` 401)

    it "refuses public metadata and direct artifacts on MAL evidence with structured audit IDs" $
        testWithApplication (pure upstreamApp) $ \port -> withSweeper $ \deadlines -> do
            rt <- testServeRuntime deadlines noopMetricsPort
            base <- depsFor port
            let source = servingRuleDeps (DbEtag "mal-generation") (fakeCveLookup [("leftpad", AdvisoryRange "MAL-pipeline" Nothing Nothing Unbounded Nothing)])
            rules <- prepare source (atDefaultPrecedence (Rules.DenyIfKnownMalicious Rules.FailDeny) : allowPolicy)
            let deps = withPrivateBaseUrl Nothing base{pdRules = rules}
            metadata <- captureServe npmPackumentContract rt (mountWith deps) (servePackument npmPackumentReplies leftpadName defaultRequest)
            statusCode (responseStatus metadata) `shouldBe` 403
            (_, logged) <- captureJsonLog $ \logEnv -> do
                artifact <- captureServeWithLog logEnv npmTarballContract rt (mountWith deps) (serveTarball npmTarballReplies leftpadName (npmVersion "1.0.0") (unsafeFilename "leftpad-1.0.0.tgz") defaultRequest)
                statusCode (responseStatus artifact) `shouldBe` 403
            for_ ["\"rule\":\"DenyIfKnownMalicious\"", "\"cve\":\"MAL-pipeline\""] $ \field ->
                logged `shouldSatisfy` T.isInfixOf field

    it "serves a gated tarball and records an admit, driving the metrics and tracing ports" $
        testWithApplication (pure upstreamApp) $ \port -> withSweeper $ \deadlines -> do
            (metricsPort, decisions) <- recordingMetricsPort
            rt <- testServeRuntime deadlines metricsPort
            deps <- depsFor port
            resp <-
                captureServe
                    npmTarballContract
                    rt
                    (mountWith deps)
                    (serveTarball npmTarballReplies leftpadName (npmVersion "1.0.0") (unsafeFilename "leftpad-1.0.0.tgz") defaultRequest)
            statusCode (responseStatus resp) `shouldBe` 200
            decisions >>= (`shouldBe` [Admit])

    it "keeps a first-party packument off the public upstream, answering 503 when the private origin cannot be read" $ withSweeper $ \deadlines -> do
        (metricsPort, decisions) <- recordingMetricsPort
        rt <- testServeRuntime deadlines metricsPort
        hits <- newIORef (0 :: Int)
        testWithApplication (pure (countingUpstream hits upstreamApp)) $ \port -> do
            base <- depsFor port
            let serveUnder firstParty =
                    captureServe
                        npmPackumentContract
                        rt
                        (mountWith base{pdFirstParty = firstParty})
                        (servePackument npmPackumentReplies leftpadName defaultRequest)
            -- A cold cache makes a zero count prove that the public leg never ran.
            firstParty <- serveUnder (== leftpadName)
            statusCode (responseStatus firstParty) `shouldBe` 503
            readIORef hits >>= (`shouldBe` 0)
            decisions >>= (`shouldBe` [Unavailable])
            thirdParty <- serveUnder (/= leftpadName)
            statusCode (responseStatus thirdParty) `shouldBe` 200
            readIORef hits >>= (`shouldSatisfy` (> 0))
            decisions >>= (`shouldBe` [Unavailable, Admit])

    it "keeps a first-party artifact off the public upstream, answering 503 when the private port is closed" $ withSweeper $ \deadlines -> do
        (metricsPort, decisions) <- recordingMetricsPort
        rt <- testServeRuntime deadlines metricsPort
        hits <- newIORef (0 :: Int)
        testWithApplication (pure (countingUpstream hits upstreamApp)) $ \port -> do
            base <- depsFor port
            for_ [serveTarball, headTarball] $ \serve -> do
                firstParty <-
                    captureServe
                        npmTarballContract
                        rt
                        (mountWith base{pdFirstParty = (== leftpadName)})
                        (serve npmTarballReplies leftpadName (npmVersion "1.0.0") (unsafeFilename "leftpad-1.0.0.tgz") defaultRequest)
                statusCode (responseStatus firstParty) `shouldBe` 503
                lookup hRetryAfter (responseHeaders firstParty) `shouldBe` Nothing
            readIORef hits `shouldReturn` 0
            decisions `shouldReturn` [Unavailable, Unavailable]

    for_ [(status404, 404, Deny), (status302, 503, Unavailable), (status400, 503, Unavailable), (status429, 503, Unavailable), (status500, 503, Unavailable), (status503, 503, Unavailable)] $ \(privateStatus, expectedStatus, expectedDecision) ->
        it ("keeps first-party artifact HTTP " <> show (statusCode privateStatus) <> " private, answering " <> show expectedStatus <> " for GET and HEAD") $ withSweeper $ \deadlines -> do
            (metricsPort, decisions) <- recordingMetricsPort
            rt <- testServeRuntime deadlines metricsPort
            publicHits <- newIORef (0 :: Int)
            testWithApplication (pure (countingUpstream publicHits upstreamApp)) $ \publicPort ->
                testWithApplication (pure (\_ respond -> respond (responseLBS privateStatus [] "private refusal"))) $ \privatePort -> do
                    base <- depsFor publicPort
                    let deps = withPrivateBaseUrl (Just (loopbackRegistryUrl ("http://localhost:" <> show privatePort))) base{pdFirstParty = (== leftpadName)}
                    for_ [serveTarball, headTarball] $ \serve -> do
                        response <-
                            captureServe
                                npmTarballContract
                                rt
                                (mountWith deps)
                                (serve npmTarballReplies leftpadName (npmVersion "1.0.0") (unsafeFilename "leftpad-1.0.0.tgz") defaultRequest)
                        statusCode (responseStatus response) `shouldBe` expectedStatus
                        lookup hRetryAfter (responseHeaders response) `shouldBe` Nothing
                    readIORef publicHits `shouldReturn` 0
                    decisions `shouldReturn` [expectedDecision, expectedDecision]

    it "keeps the public artifact status, headers and bytes after every private miss" $
        testWithApplication (pure (dated upstreamApp)) $ \publicPort -> withSweeper $ \deadlines -> do
            rt <- testServeRuntime deadlines noopMetricsPort
            base <- depsFor publicPort
            logEnv <- newTestLogEnv
            let fetch deps =
                    runSession (WaiTest.request defaultRequest) $ \req respond ->
                        runHandler
                            logEnv
                            mempty
                            (RequestCtx rt (mountWith deps))
                            (serveTarball npmTarballReplies leftpadName (npmVersion "1.0.0") (unsafeFilename "leftpad-1.0.0.tgz") req (respond . responseToWai npmTarballContract))
            baseline <- fetch (withPrivateBaseUrl Nothing base)
            simpleBody baseline `shouldBe` LBS.fromStrict artifactBytes
            let assertSame response = do
                    simpleStatus response `shouldBe` simpleStatus baseline
                    simpleHeaders response `shouldBe` simpleHeaders baseline
                    simpleBody response `shouldBe` simpleBody baseline
            fetch base >>= assertSame
            for_ [status404, status302, status400, status429, status500, status503] $ \privateStatus ->
                testWithApplication (pure (\_ respond -> respond (responseLBS privateStatus [] "private refusal"))) $ \privatePort ->
                    fetch (withPrivateBaseUrl (Just (loopbackRegistryUrl ("http://localhost:" <> show privatePort))) base) >>= assertSame

    for_ [(200, upstreamApp), (206, partialUpstream)] $ \(expected, upstream) ->
        it ("relays an upstream " <> show expected <> " npm artifact unchanged to a client that accepts gzip, for GET and HEAD") $
            testWithApplication (pure (dated upstream)) $ \publicPort -> withSweeper $ \deadlines -> do
                rt <- testServeRuntime deadlines noopMetricsPort
                deps <- withPrivateBaseUrl Nothing <$> depsFor publicPort
                logEnv <- newTestLogEnv
                for_ [serveTarball, headTarball] $ \serve -> do
                    let fetch headers =
                            runSession (WaiTest.request defaultRequest{requestHeaders = headers}) $ \req respond ->
                                runHandler logEnv mempty (RequestCtx rt (mountWith deps)) (serve npmTarballReplies leftpadName (npmVersion "1.0.0") (unsafeFilename "leftpad-1.0.0.tgz") req (respond . responseToWai npmTarballContract))
                    relaysUncoded expected fetch

    for_ [(200, status200), (206, status206)] $ \(expected, artifactStatus) ->
        it ("relays an upstream " <> show expected <> " PyPI distribution unchanged to a client that accepts gzip, for GET and HEAD") $
            testWithApplication (pure (dated (pypiPublicApp artifactStatus))) $ \publicPort -> withSweeper $ \deadlines -> do
                rt <- testServeRuntime deadlines noopMetricsPort
                prepared <- prepare inertRuleDeps allowPolicy
                let deps = (pypiServeDeps Nothing (loopbackRegistryUrl ("http://localhost:" <> show publicPort)) NoMirrorWrite prepared (pure (UTCTime (fromGregorian 2026 6 20) 0))){pdMountBaseUrl = "http://proxy.test"}
                for_ [methodGet, methodHead] $ \method ->
                    relaysUncoded expected (fetchPypiArtifact rt deps method)

    it "sheds packument work when metadata admission refuses" $ withSweeper $ \deadlines -> do
        (metricsPort, _decisions) <- recordingMetricsPort
        admission <- newServeAdmissionTuned deadlines 1 0 0
        rt <- testServeRuntimeWith deadlines admission metricsPort
        deps <- depsFor 1
        held <- withServeAdmission (srMetrics rt) admission (captureServe npmPackumentContract rt (mountWith deps) (servePackument npmPackumentReplies leftpadName defaultRequest))
        response <- either (const (throwIO MissingFixtureResponse)) pure held
        statusCode (responseStatus response) `shouldBe` 503
        (snd <$> find ((== hRetryAfter) . fst) (responseHeaders response)) `shouldBe` Just "1"

    it "releases metadata admission after an admitted operation completes" $
        testWithApplication (pure upstreamApp) $ \port -> withSweeper $ \deadlines -> do
            (metricsPort, _decisions) <- recordingMetricsPort
            admission <- newServeAdmissionTuned deadlines 1 0 0
            rt <- testServeRuntimeWith deadlines admission metricsPort
            deps <- depsFor port
            saturated <- withServeAdmission (srMetrics rt) admission (captureServe npmPackumentContract rt (mountWith deps) (servePackument npmPackumentReplies leftpadName defaultRequest))
            (statusCode . responseStatus <$> saturated) `shouldBe` Right 503
            admitted <- captureServe npmPackumentContract rt (mountWith deps) (servePackument npmPackumentReplies leftpadName defaultRequest)
            statusCode (responseStatus admitted) `shouldBe` 200

    it "sheds a tarball miss when its public metadata gate cannot acquire admission" $ withSweeper $ \deadlines -> do
        (metricsPort, _decisions) <- recordingMetricsPort
        admission <- newServeAdmissionTuned deadlines 1 0 0
        rt <- testServeRuntimeWith deadlines admission metricsPort
        deps <- depsFor 1
        held <-
            withServeAdmission (srMetrics rt) admission $
                captureServe
                    npmTarballContract
                    rt
                    (mountWith deps)
                    (serveTarball npmTarballReplies leftpadName (npmVersion "1.0.0") (unsafeFilename "leftpad-1.0.0.tgz") defaultRequest)
        response <- either (const (throwIO MissingFixtureResponse)) pure held
        statusCode (responseStatus response) `shouldBe` 503
        (snd <$> find ((== hRetryAfter) . fst) (responseHeaders response)) `shouldBe` Just "1"

    it "does not hold metadata admission around a trusted private tarball stream" $
        testWithApplication (pure upstreamApp) $ \port -> withSweeper $ \deadlines -> do
            (metricsPort, _decisions) <- recordingMetricsPort
            admission <- newServeAdmission deadlines 1
            rt <- testServeRuntimeWith deadlines admission metricsPort
            deps <- depsFor 1
            let privateDeps = withPrivateBaseUrl (Just (loopbackRegistryUrl ("http://localhost:" <> show port))) deps
            held <-
                withServeAdmission (srMetrics rt) admission $
                    captureServe
                        npmTarballContract
                        rt
                        (mountWith privateDeps)
                        (serveTarball npmTarballReplies leftpadName (npmVersion "1.0.0") (unsafeFilename "leftpad-1.0.0.tgz") defaultRequest)
            (statusCode . responseStatus <$> held) `shouldBe` Right 200

    it "releases both gates before public artifact relay starts" $ withSweeper $ \deadlines -> do
        admission <- newServeAdmissionTuned deadlines 1 0 0
        rt <- testServeRuntimeWith deadlines admission noopMetricsPort
        released <- newIORef Nothing
        let upstream req respond = do
                when ("/leftpad/-/" `BS.isPrefixOf` rawPathInfo req) $ do
                    available <- withServeAdmission noopMetricsPort admission (pure ())
                    held <- snChargedBytes <$> meterSnapshot (srMemoryMeter rt)
                    writeIORef released (Just (available, held))
                upstreamApp req respond
        testWithApplication (pure upstream) $ \port -> do
            deps <- depsFor port
            response <-
                captureServe
                    npmTarballContract
                    rt
                    (mountWith deps)
                    (serveTarball npmTarballReplies leftpadName (npmVersion "1.0.0") (unsafeFilename "leftpad-1.0.0.tgz") defaultRequest)
            statusCode (responseStatus response) `shouldBe` 200
            readIORef released `shouldReturn` Just (Right (), 0)

overflowRouteSpec :: Spec
overflowRouteSpec = describe "overflow refusal through ecosystem routes" $
    for_ [("after queue expiry", 1), ("past the queue cap", 0)] $ \(ending, cap) ->
        for_ [("npm", npmRouter, npmCredential, ["leftpad"], ["leftpad", "-", "leftpad-1.0.0.tgz"]), ("PyPI", pypiRouter, pypiCredential, ["simple", "requests"], ["simple", "requests", "requests-1.0.0.tar.gz"])] $ \(ecosystem, router, credential, listingPath, artifactPath) ->
            for_ [("listing", listingPath, False), ("public artifact", artifactPath, False), ("managed private artifact", artifactPath, True)] $ \(route, path, managed) ->
                it (ecosystem <> " " <> route <> " keeps its refusal " <> ending) $ do
                    lookups <- newIORef (0 :: Int)
                    stateReads <- newIORef (0 :: Int)
                    testWithApplication (pure (countingUpstream lookups upstreamApp)) $ \port -> withSweeper $ \deadlines -> do
                        (metrics, decisions) <- recordingMetricsPort
                        gate <- newServeAdmissionTuned deadlines 1 0 0
                        overflow <- newOverflowTuned deadlines cap 0
                        let admission = withOverflow gate overflow
                            origin = loopbackRegistryUrl ("http://localhost:" <> show port)
                        rt <- testServeRuntimeWith deadlines admission metrics
                        prepared <- prepare inertRuleDeps allowPolicy
                        view <- mirrorViewWith pypiMirrorDocuments (stateTarget "http://private.test/") (\_ _ -> modifyIORef' stateReads (+ 1) >> throwIO MissingFixtureResponse)
                        let unexpected = throwIO MissingFixtureResponse
                            base =
                                if ecosystem == "npm"
                                    then npmServeDeps Nothing origin NoMirrorWrite prepared (pure fixedNow)
                                    else pypiServeDeps Nothing origin NoMirrorWrite prepared (pure fixedNow)
                            deps =
                                (withPrivateBaseUrl (if managed then Just origin else Nothing) base)
                                    { pdMirrorState = if managed then Just view else Nothing
                                    }
                            binding = (mountUnder credential deps){bindingRouter = router}
                        case router methodGet [] path of
                            RouteAction contract (RunPipeline _ action) -> do
                                logEnv <- newTestLogEnv
                                held <- withServeAdmission metrics admission $
                                    runSession (WaiTest.request defaultRequest) $ \req respond ->
                                        runHandler logEnv mempty (RequestCtx rt binding) (action req (respond . responseToWai contract))
                                response <- either (const unexpected) pure held
                                simpleStatus response `shouldBe` status503
                                lookup hRetryAfter (simpleHeaders response) `shouldBe` Just "1"
                                lookup hConnection (simpleHeaders response) `shouldBe` Nothing
                                if ecosystem == "npm"
                                    then
                                        (eitherDecode (simpleBody response) :: Either String Value)
                                            `shouldBe` Right (object ["error" .= ("server is busy; retry later" :: Text)])
                                    else simpleBody response `shouldBe` ""
                            _ -> expectationFailure "fixture did not select a pipeline route"
                        decisions `shouldReturn` [Unavailable]
                        readIORef lookups `shouldReturn` 0
                        readIORef stateReads `shouldReturn` 0

memoryAdmissionSpec :: Spec
memoryAdmissionSpec = describe "memory admission on live request paths" $ do
    for_ [("1.0.0", 200), ("9.0.0", 404)] $ \(version, expectedStatus) ->
        it ("serves pinned selected reuse, including absence, with no second lookup and fresh rules: " <> toString version) $ do
            lookupCalls <- newIORef (0 :: Int)
            let upstream req respond = do
                    when (rawPathInfo req == "/leftpad") (modifyIORef' lookupCalls (+ 1))
                    upstreamApp req respond
            testWithApplication (pure upstream) $ \port -> withSweeper $ \deadlines -> do
                rt <- testServeRuntime deadlines noopMetricsPort
                deps <- depsFor port
                let serve rules selected =
                        captureServe
                            npmTarballContract
                            rt
                            (mountWith deps{pdRules = rules})
                            (serveTarball npmTarballReplies leftpadName (npmVersion selected) (unsafeFilename ("leftpad-" <> selected <> ".tgz")) defaultRequest)
                warm <- serve (pdRules deps) version
                statusCode (responseStatus warm) `shouldBe` expectedStatus
                retained <- serve (pdRules deps) version
                statusCode (responseStatus retained) `shouldBe` expectedStatus
                when (expectedStatus == 200) $ do
                    denied <- serve [] version
                    statusCode (responseStatus denied) `shouldBe` 403
                readIORef lookupCalls `shouldReturn` 1

    it "reads both origins for assembled hits and conditional responses" $ do
        privateCalls <- newIORef (0 :: Int)
        publicCalls <- newIORef (0 :: Int)
        testWithApplication (pure (countingUpstream privateCalls upstreamApp)) $ \privatePort ->
            testWithApplication (pure (countingUpstream publicCalls upstreamApp)) $ \publicPort -> withSweeper $ \deadlines -> do
                rt <- testServeRuntime deadlines noopMetricsPort
                base <- depsFor publicPort
                let deps = withPrivateBaseUrl (Just (loopbackRegistryUrl ("http://localhost:" <> show privatePort))) base
                    serve request = captureServe npmPackumentContract rt (mountWith deps) (servePackument npmPackumentReplies leftpadName request)
                initial <- serve defaultRequest
                statusCode (responseStatus initial) `shouldBe` 200
                validator <- maybe (throwIO MissingFixtureResponse) pure (lookup hETag (responseHeaders initial))
                reused <- serve defaultRequest
                statusCode (responseStatus reused) `shouldBe` 200
                unchanged <- serve (requestWith [(hIfNoneMatch, validator)])
                statusCode (responseStatus unchanged) `shouldBe` 304
                readIORef privateCalls `shouldReturn` 3
                readIORef publicCalls `shouldReturn` 3

    it "sheds a listing at a full memory gate before either origin starts" $ do
        lookups <- newIORef (0 :: Int)
        testWithApplication (pure (countingUpstream lookups upstreamApp)) $ \port -> withSweeper $ \deadlines -> do
            (rt, meter) <- fullDoorRuntime deadlines noopMetricsPort
            base <- depsFor port
            let deps = withPrivateBaseUrl (Just (loopbackRegistryUrl ("http://localhost:" <> show port))) base
            held <- withMemoryEntry noopMetricsPort meter $ \_ ->
                captureServe npmPackumentContract rt (mountWith deps) (servePackument npmPackumentReplies leftpadName defaultRequest)
            response <- either (const (throwIO MissingFixtureResponse)) pure held
            statusCode (responseStatus response) `shouldBe` 503
            (snd <$> find ((== hRetryAfter) . fst) (responseHeaders response)) `shouldBe` Just "1"
            readIORef lookups `shouldReturn` 0

    it "sheds a public artifact decision at a full memory gate before any lookup" $ do
        lookups <- newIORef (0 :: Int)
        testWithApplication (pure (countingUpstream lookups upstreamApp)) $ \port -> withSweeper $ \deadlines -> do
            (rt, meter) <- fullDoorRuntime deadlines noopMetricsPort
            deps <- depsFor port
            held <- withMemoryEntry noopMetricsPort meter $ \_ ->
                captureServe
                    npmTarballContract
                    rt
                    (mountWith deps)
                    (serveTarball npmTarballReplies leftpadName (npmVersion "1.0.0") (unsafeFilename "leftpad-1.0.0.tgz") defaultRequest)
            (statusCode . responseStatus <$> held) `shouldBe` Right 503
            readIORef lookups `shouldReturn` 0

    it "charges a public artifact decision for its retained selected facts before evaluation" $
        testWithApplication (pure upstreamApp) $ \port -> withSweeper $ \deadlines -> do
            overdraws <- newIORef (0 :: Int)
            atGate <- newIORef Nothing
            let port' = noopMetricsPort{mpMemoryAdmissionOverdraw = atomicModifyIORef' overdraws (\n -> (n + 1, ()))}
            (rt, meter) <- fullDoorRuntime deadlines port'
            deps <- depsFor port
            let metrics = port'{mpRuleEvalDuration = \_ _ -> meterSnapshot meter >>= writeIORef atGate . Just . snChargedBytes}
                runtime = rt{srMetrics = metrics}
            response <-
                captureServe
                    npmTarballContract
                    runtime
                    (mountWith deps)
                    (serveTarball npmTarballReplies leftpadName (npmVersion "1.0.0") (unsafeFilename "leftpad-1.0.0.tgz") defaultRequest)
            statusCode (responseStatus response) `shouldBe` 200
            selected <- cachedVersion noopMetricsPort (srMetadataCache runtime) (Source (registryUrlText (pdPublicBaseUrl deps))) leftpadName (npmVersion "1.0.0") >>= maybe (fail "public artifact metadata was not retained") pure
            let served = selected{vrVersion = fmap (\document -> document{vdRaw = Nothing}) (vrVersion selected)}
                required = weighVersion served
            required `shouldSatisfy` (> 1)
            readIORef atGate `shouldReturn` Just required
            readIORef overdraws `shouldReturn` 1
            snChargedBytes <$> meterSnapshot meter `shouldReturn` 0

    it "lets a listing larger than the whole budget finish by overdrawing, then returns its charge" $
        testWithApplication (pure upstreamApp) $ \port -> withSweeper $ \deadlines -> do
            overdraws <- newIORef (0 :: Int)
            let port' = noopMetricsPort{mpMemoryAdmissionOverdraw = atomicModifyIORef' overdraws (\n -> (n + 1, ()))}
            (rt, meter) <- fullDoorRuntime deadlines port'
            base <- depsFor port
            let deps = withPrivateBaseUrl (Just (loopbackRegistryUrl ("http://localhost:" <> show port))) base
            response <- captureServe npmPackumentContract rt (mountWith deps) (servePackument npmPackumentReplies leftpadName defaultRequest)
            statusCode (responseStatus response) `shouldBe` 200
            readIORef overdraws >>= (`shouldSatisfy` (> 0))
            snChargedBytes <$> meterSnapshot meter `shouldReturn` 0

divergenceEvidenceSpec :: Spec
divergenceEvidenceSpec = describe "validated divergence evidence across public rules" $ do
    it "keeps the warning and counter after a named public denial with both digests fixed" $
        withConflictOrigins (conflictPublicApp id) divergentPrivateApp $ \rt base divergences publicHits -> do
            denied <- prepare inertRuleDeps (atDefaultPrecedence Rules.DenyInstallTimeExecution : allowPolicy)
            for_ [(base, "2.0.0", ["1.0.0", "2.0.0"], 1), (base{pdRules = denied}, "1.0.0", ["1.0.0"], 2)] $ \(deps, latest, keys, count) -> do
                (_, logged) <- captureJsonLog $ \logEnv -> do
                    resp <- captureServeWithLog logEnv npmPackumentContract rt (mountWith deps) (servePackument npmPackumentReplies leftpadName defaultRequest)
                    assertPrivatePackument deps latest keys resp
                assertConflictLog True logged
                divergences `shouldReturn` count
            readIORef publicHits `shouldReturn` 2

    it "retains denied conflict evidence beside another admitted public version" $
        withConflictOrigins (conflictPublicApp (\v -> v{vsHasInstallScript = vsVersion v == "1.0.0"})) divergentPrivateApp $ \rt base divergences _ -> do
            denied <- prepare inertRuleDeps (atDefaultPrecedence Rules.DenyInstallTimeExecution : allowPolicy)
            (_, logged) <- captureJsonLog $ \logEnv -> do
                resp <- captureServeWithLog logEnv npmPackumentContract rt (mountWith base{pdRules = denied}) (servePackument npmPackumentReplies leftpadName defaultRequest)
                assertPrivatePackument base "2.0.0" ["1.0.0", "2.0.0"] resp
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
                    (_, logged) <- captureJsonLog $ \logEnv -> do
                        resp <- captureServeWithLog logEnv npmPackumentContract rt (mountWith base{pdRules = denied}) (servePackument npmPackumentReplies leftpadName defaultRequest)
                        assertPrivatePackument base "1.0.0" ["1.0.0"] resp
                    assertConflictLog False logged
                    divergences `shouldReturn` 0

    it "excludes a denied SHA-256 copy when the public floor requires SHA-512" $
        withConflictOrigins (conflictPublicApp (\v -> v{vsIntegrity = Just (sriSha256Of artifactBytes)})) (privateAppWithIntegrity (sriSha256Of "private bytes")) $ \rt base divergences _ -> do
            floorSpec <- either (fail . toString) pure (mkMinIntegrity SHA512)
            denied <- prepare inertRuleDeps (atDefaultPrecedence Rules.DenyInstallTimeExecution : allowPolicy)
            (_, logged) <- captureJsonLog $ \logEnv -> do
                resp <- captureServeWithLog logEnv npmPackumentContract rt (mountWith base{pdRules = denied, pdMinIntegrity = floorSpec}) (servePackument npmPackumentReplies leftpadName defaultRequest)
                assertTrustedPackument (sriSha256Of "private bytes") base "1.0.0" ["1.0.0"] resp
            assertConflictLog False logged
            divergences `shouldReturn` 0

    it "does not fetch public conflict evidence for a first-party name" $
        withConflictOrigins (conflictPublicApp id) divergentPrivateApp $ \rt base divergences publicHits -> do
            resp <- captureServe npmPackumentContract rt (mountWith base{pdFirstParty = (== leftpadName)}) (servePackument npmPackumentReplies leftpadName defaultRequest)
            assertPrivatePackument base "1.0.0" ["1.0.0"] resp
            divergences `shouldReturn` 0
            readIORef publicHits `shouldReturn` 0

    for_ [status401, status403] $ \refusal ->
        it ("keeps private HTTP " <> show (statusCode refusal) <> " authoritative without another public fetch") $
            withConflictOrigins (conflictPublicApp id) (\_ respond -> respond (responseLBS refusal [] "refused")) $ \rt deps divergences publicHits -> do
                resp <- captureServe npmPackumentContract rt (mountWith deps) (servePackument npmPackumentReplies leftpadName defaultRequest)
                statusCode (responseStatus resp) `shouldBe` 403
                divergences `shouldReturn` 0
                readIORef publicHits `shouldReturn` 1

skippedCheckAuditSpec :: Spec
skippedCheckAuditSpec = describe "skipped-check evidence at the public artifact gate" $ do
    it "logs the skipped check once, at the artifact admission, and not for the packument that listed it" $
        testWithApplication (pure upstreamApp) $ \port -> withSweeper $ \deadlines -> do
            (metricsPort, _decisions) <- recordingMetricsPort
            rt <- testServeRuntime deadlines metricsPort
            deps <- skippingDeps port
            (_, logged) <- captureJsonLog $ \logEnv -> do
                packument <- captureServeWithLog logEnv npmPackumentContract rt (mountWith deps) (servePackument npmPackumentReplies leftpadName defaultRequest)
                statusCode (responseStatus packument) `shouldBe` 200
                tarball <- captureServeWithLog logEnv npmTarballContract rt (mountWith deps) (serveTarball npmTarballReplies leftpadName (npmVersion "1.0.0") (unsafeFilename "leftpad-1.0.0.tgz") defaultRequest)
                statusCode (responseStatus tarball) `shouldBe` 200
            let evidence = filter (T.isInfixOf "skipped for unavailability") (lines logged)
            length evidence `shouldBe` 1
            for_ ["\"sev\":\"Warning\"", "\"package\":\"leftpad\"", "\"version\":\"1.0.0\"", "\"rule\":\"DenyIfCve\"", "\"cause\":\"no advisory database loaded\""] $ \field ->
                logged `shouldSatisfy` T.isInfixOf field

    it "logs the line once per admission identity for the life of the outage, and again after recovery" $
        testWithApplication (pure twoVersionUpstream) $ \port -> withSweeper $ \deadlines -> do
            (metricsPort, _decisions) <- recordingMetricsPort
            rt <- testServeRuntime deadlines metricsPort
            (reporter, deps) <- observedSkippingDeps port
            twoSkips <- prepare (ruleDepsOf reporter) (atDefaultPrecedence (Rules.DenyIfEpss (Rules.DenyIfEpssParams 0.5 Rules.FailNoDecision)) : skipPolicy)
            let serve logEnv d ver = do
                    tarball <- captureServeWithLog logEnv npmTarballContract rt (mountWith d) (serveTarball npmTarballReplies leftpadName (npmVersion ver) (unsafeFilename ("leftpad-" <> ver <> ".tgz")) defaultRequest)
                    statusCode (responseStatus tarball) `shouldBe` 200
                evidenceLines = length . filter (T.isInfixOf "skipped for unavailability") . lines
            (_, logged) <- captureJsonLog $ \logEnv -> do
                replicateM_ 5 (serve logEnv deps "1.0.0")
                serve logEnv deps "2.0.0"
                serve logEnv deps{pdRules = twoSkips} "1.0.0"
                replicateM_ 3 (serve logEnv deps "1.0.0")
                for_ ["DenyIfCve", "DenyIfEpss"] (reportSource reporter . SourceAnswered)
                serve logEnv deps "1.0.0"
            evidenceLines logged `shouldBe` 5
            length (filter (T.isInfixOf "\"version\":\"2.0.0\"") (lines logged)) `shouldBe` 1
            length (filter (T.isInfixOf "\"rule\":\"DenyIfEpss\"") (lines logged)) `shouldBe` 1

    it "logs nothing for a trusted private serve, which runs no rules" $
        testWithApplication (pure upstreamApp) $ \port -> withSweeper $ \deadlines -> do
            (metricsPort, _decisions) <- recordingMetricsPort
            rt <- testServeRuntime deadlines metricsPort
            deps <- withPrivateBaseUrl (Just (loopbackRegistryUrl ("http://localhost:" <> show port))) <$> skippingDeps 1
            (_, logged) <- captureJsonLog $ \logEnv ->
                replicateM_ 3 $ do
                    tarball <- captureServeWithLog logEnv npmTarballContract rt (mountWith deps) (serveTarball npmTarballReplies leftpadName (npmVersion "1.0.0") (unsafeFilename "leftpad-1.0.0.tgz") defaultRequest)
                    statusCode (responseStatus tarball) `shouldBe` 200
            logged `shouldSatisfy` (not . T.isInfixOf "skipped for unavailability")

skippingDeps :: Int -> IO PackumentDeps
skippingDeps publicPort = do
    base <- depsFor publicPort
    skipping <- prepare inertRuleDeps skipPolicy
    pure base{pdRules = skipping}

skipPolicy :: [PrecededRule]
skipPolicy = atDefaultPrecedence (Rules.DenyIfCve (Rules.DenyIfCveParams 8.0 Rules.FailNoDecision)) : allowPolicy

observedSkippingDeps :: Int -> IO (SourceReporter, PackumentDeps)
observedSkippingDeps publicPort = do
    base <- depsFor publicPort
    shared <- newTVarIO Healthy
    let reporter = sourceReporter 900 (pure fixedNow) (tvarOutageStore shared) (const pass)
    skipping <- prepare (ruleDepsOf reporter) skipPolicy
    pure (reporter, base{pdRules = skipping, pdNoteAdmission = noteAdmission reporter})

ruleDepsOf :: SourceReporter -> RuleDeps
ruleDepsOf reporter = inertRuleDeps{rdSourceReporter = reporter}

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
            resp <- captureServe npmPackumentContract rt (mountWith base{pdRules = denied}) (servePackument npmPackumentReplies leftpadName defaultRequest)
            fields <- servedFields resp
            KeyMap.lookup "dist-tags" fields `shouldBe` Just (object ["latest" .= ("2.0.0" :: Text)])
            servedKeys fields `shouldBe` Just ["1.0.0", "2.0.0"]

    it "keeps a public latest held on an older version while its target survives" $ do
        -- The public maintainer holds latest at 1.0.0 though 2.0.0 is admitted, and the store
        -- tags 2.0.0. Maintainer intent wins over both the ordering and the store.
        let public = publicAppOver ["1.0.0", "2.0.0"] "1.0.0" id
        withConflictOrigins public (privateAppOver ["1.0.0", "2.0.0"] "2.0.0") $ \rt base _divergences _ -> do
            resp <- captureServe npmPackumentContract rt (mountWith base) (servePackument npmPackumentReplies leftpadName defaultRequest)
            fields <- servedFields resp
            KeyMap.lookup "dist-tags" fields `shouldBe` Just (object ["latest" .= ("1.0.0" :: Text)])
            servedKeys fields `shouldBe` Just ["1.0.0", "2.0.0"]

sharedCacheSpec :: Spec
sharedCacheSpec = describe "the shared metadata cache across the two origins" $
    it "re-fetches both origins without retaining full metadata" $ do
        privateHits <- newIORef 0
        let public = publicAppOver ["1.0.0", "2.0.0"] "2.0.0" id
            private = countingUpstream privateHits (privateAppOver ["1.0.0"] "1.0.0")
        withConflictOrigins public private $ \rt deps _divergences publicHits -> do
            replicateM_ 2 (captureServe npmPackumentContract rt (mountWith deps) (servePackument npmPackumentReplies leftpadName defaultRequest))
            -- The private read authorises each request. Full public retention is disabled locally.
            readIORef privateHits `shouldReturn` 2
            readIORef publicHits `shouldReturn` 2
            privateCached <- traverse (cachedUnder rt) (pdPrivateBaseUrl deps)
            privateCached `shouldBe` Just False
            cachedUnder rt (pdPublicBaseUrl deps) `shouldReturn` False

cachedUnder :: ServeRuntime -> RegistryUrl -> IO Bool
cachedUnder rt baseUrl = isJust <$> cachedMetadata noopMetricsPort (srMetadataCache rt) (Source (registryUrlText baseUrl)) leftpadName

withConflictOrigins :: Application -> Application -> (ServeRuntime -> PackumentDeps -> IO Int -> IORef Int -> IO ()) -> IO ()
withConflictOrigins public private action = do
    publicHits <- newIORef 0
    testWithApplication (pure (countingUpstream publicHits public)) $ \publicPort ->
        testWithApplication (pure private) $ \privatePort -> withSweeper $ \deadlines -> do
            (metrics, divergences) <- recordingDivergenceMetricsPort
            rt <- testServeRuntime deadlines metrics
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

servedFields :: Response -> IO (KeyMap.KeyMap Value)
servedFields resp = do
    value <- responseBytes resp >>= either fail pure . eitherDecode
    case value of
        Object fields -> pure fields
        _ -> fail "expected a packument object"

servedKeys :: KeyMap.KeyMap Value -> Maybe [Text]
servedKeys fields = case KeyMap.lookup "versions" fields of
    Just (Object versions) -> Just (sort (map Key.toText (KeyMap.keys versions)))
    _ -> Nothing

assertPrivatePackument :: PackumentDeps -> Text -> [Text] -> Response -> Expectation
assertPrivatePackument = assertTrustedPackument (sha512Integrity "leftpad artifact bytes (privately tampered)")

assertTrustedPackument :: Text -> PackumentDeps -> Text -> [Text] -> Response -> Expectation
assertTrustedPackument integrity deps latest keys resp = do
    source <- maybe (fail "missing private source") (pure . registryUrlText) (pdPrivateBaseUrl deps)
    statusCode (responseStatus resp) `shouldBe` 200
    fields <- servedFields resp
    KeyMap.lookup "dist-tags" fields `shouldBe` Just (object ["latest" .= latest])
    servedKeys fields `shouldBe` Just keys
    KeyMap.lookup "author" fields `shouldBe` Just (String ("See " <> source <> "/leftpad"))
    case KeyMap.lookup "versions" fields of
        Just (Object versions) ->
            KeyMap.lookup "1.0.0" versions
                `shouldBe` Just (versionValue ((versionSpec "leftpad" "1.0.0" "http://proxy.test/leftpad/-/leftpad-1.0.0.tgz"){vsIntegrity = Just integrity, vsExtraPairs = ["dependencies" .= object ["private-marker" .= ("^1.0.0" :: Text)], "author" .= ("See " <> source <> "/leftpad")]}))
        _ -> expectationFailure "expected served version objects"

conflictPublicApp :: (VersionSpec -> VersionSpec) -> Application
conflictPublicApp change = publicAppOver ["1.0.0", "2.0.0"] "2.0.0" (change . withInstallScript)
  where
    withInstallScript v = v{vsHasInstallScript = True}

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
                (LifetimePolicy [atPrecedence 600 (Rules.AllowByIdentity "leftpad"), identityDeny] ((== Just "AllowByIdentity") . admittedBy) Retained)
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

-- A trusted read restores the deleted version before the next grouped sweep.
cacheRetentionSpec :: Spec
cacheRetentionSpec = describe "a private read that retains a version the cycle removed" $
    it "serves the read untouched, removes the copy it restored, and keeps the version policy allows" $ do
        mirror <- newFakeStore (retentionStore "mirror" [deniedVersion, keptVersion])
        cache <- newFakeStore (retentionStore "cache" [])
        rules <- prepare inertRuleDeps [identityDeny]
        let mount = withPrivateCache (deletingCache (fakeMaintenance cache)) (testMount (fakeMaintenance mirror) rules [Rules.prRule identityDeny])
        cleared <- recordingPorts Nothing
        _ <- sweepCycle testPacing (recPorts cleared) [mount]
        heldVersions leftpadName mirror `shouldReturn` [keptVersion]
        heldVersions leftpadName cache `shouldReturn` []
        nextPrivateGet (retainingUpstream cache) rules True
        heldVersions leftpadName cache `shouldReturn` [deniedVersion]
        reconciled <- recordingPorts Nothing
        _ <- sweepCycle testPacing (recPorts reconciled) [mount]
        recTargetResults reconciled >>= (`shouldSatisfy` elem (SweepPrivate, SweepDeleted))
        heldVersions leftpadName cache `shouldReturn` []
        heldVersions leftpadName mirror `shouldReturn` [keptVersion]

-- Each store answers under its own backend name, because a grouped cycle keys its inventories on it.
retentionStore :: Text -> [Version] -> FakeStoreConfig
retentionStore backend versions =
    (seededStoreConfig [(leftpadName, versions)])
        { -- Both stores answer for the pair, so a delete never hides behind an unread manifest.
          fakeManifests = Map.singleton leftpadName (sampleManifest leftpadName [deniedVersion, keptVersion])
        , fakeFacts = (fakeFacts defaultFakeStoreConfig){factBackend = backend}
        }

-- A cache that retains what it serves, the way a read through an upstream relationship refills one.
retainingUpstream :: FakeStore -> Application
retainingUpstream store req respond = do
    writeFakeContents store (Map.singleton leftpadName (servedVersions [deniedVersion]))
    upstreamApp req respond

deniedVersion, keptVersion :: Version
deniedVersion = npmVersion "1.0.0"
keptVersion = npmVersion "2.0.0"

data CopyDisposition = Retained | Removed
    deriving stock (Eq)

-- An unread manifest still lets a rule that reads only identity decide.
data LifetimeStore = Sweepable | FirstParty | ConsentMissing | TargetPreserved | ManifestMissing
    deriving stock (Eq, Show)

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
unavailableCveDeny = atPrecedence 600 (Rules.DenyIfCve (Rules.DenyIfCveParams 0 Rules.FailDeny))

winningDeny :: LifetimePolicy
winningDeny = LifetimePolicy [identityDeny] ((== Just "DenyByIdentity") . blockedBy) Removed

checkLifetime :: SweepShape -> LifetimePolicy -> LifetimeStore -> Expectation
checkLifetime shape policy storeState = do
    ctx <- Rules.mkEvalContext (pure fixedNow) (pure Nothing)
    let version = npmVersion "1.0.0"
        details = sampleDetails leftpadName version
        initialPolicy = atPrecedence 700 (Rules.AllowByIdentity "leftpad@1.0.0") : lpRules policy
    initial <- prepare inertRuleDeps initialPolicy
    admittedBy <$> evalRules ctx initial (Rules.completeEvidence details) `shouldReturn` Just "AllowByIdentity"
    store <- newFakeStore (lifetimeStoreConfig storeState)
    preparedAfter <- prepare inertRuleDeps (lpRules policy)
    evalRules ctx preparedAfter (Rules.completeEvidence details) >>= (`shouldSatisfy` lpDecision policy)
    recorded <- recordingPorts Nothing
    let mount =
            (testMount (fakeMaintenance store) preparedAfter (map Rules.prRule (lpRules policy)))
                { smFirstParty = \name -> storeState == FirstParty && name == leftpadName
                }
    outcome <- sweepCycle testPacing{swpShape = shape} (recPorts recorded) [mount]
    let retained = stopsBeforeRules storeState || lpDisposition policy == Retained
        expectedVersions = [servedVersion version | retained]
        examined
            | stopsBeforeRules storeState = 0
            | shape == SweepCandidates && identityDeny `notElem` lpRules policy = 0
            | otherwise = 1
    tallyExamined (outcomeTally outcome) `shouldBe` examined
    tallyDeleted (outcomeTally outcome) `shouldBe` if retained then 0 else 1
    Map.lookup leftpadName <$> readFakeContents store `shouldReturn` Just expectedVersions
    nextPrivateGet (storedUpstream store) preparedAfter retained

lifetimeStoreConfig :: LifetimeStore -> FakeStoreConfig
lifetimeStoreConfig storeState = case storeState of
    ConsentMissing -> seeded{fakeConsent = ConsentWithheld "operator withdrew consent"}
    TargetPreserved -> seeded{fakeClass = StorePreserved "the store has an upstream"}
    ManifestMissing -> seeded{fakeManifests = Map.empty}
    _ -> seeded
  where
    version = npmVersion "1.0.0"
    seeded = seededStoreConfig [(leftpadName, [version])]

nextPrivateGet :: Application -> [PreparedRule] -> Bool -> Expectation
nextPrivateGet privateUpstream rules retained = do
    publicHits <- newIORef (0 :: Int)
    privateHits <- newIORef (0 :: Int)
    testWithApplication (pure (countingUpstream publicHits upstreamApp)) $ \publicPort ->
        testWithApplication (pure (countingUpstream privateHits privateUpstream)) $ \privatePort -> withSweeper $ \deadlines -> do
            (metricsPort, decisions) <- recordingMetricsPort
            rt <- testServeRuntime deadlines metricsPort
            base <- depsFor publicPort
            let deps = withPrivateBaseUrl (Just (loopbackRegistryUrl ("http://localhost:" <> show privatePort))) base{pdRules = rules}
            response <-
                captureServe
                    npmTarballContract
                    rt
                    (mountWith deps)
                    (serveTarball npmTarballReplies leftpadName (npmVersion "1.0.0") (unsafeFilename "leftpad-1.0.0.tgz") defaultRequest)
            statusCode (responseStatus response) `shouldBe` if retained then 200 else 403
            decisions `shouldReturn` [if retained then Admit else Deny]
            readIORef privateHits `shouldReturn` 1
            readIORef publicHits `shouldReturn` if retained then 0 else 1

-- The private registry reads the same inventory that the sweep's delete capability mutates.
storedUpstream :: FakeStore -> Application
storedUpstream store req respond = do
    contents <- readFakeContents store
    if servedVersion (npmVersion "1.0.0") `elem` Map.findWithDefault [] leftpadName contents
        then upstreamApp req respond
        else respond (responseLBS status404 [] "")

publicControlSpec :: Spec
publicControlSpec = describe "public authority control" $ do
    it "keeps private listing contributions while suppressing repeated public throttles" $ withSweeper $ \deadlines -> do
        fixture <- newPublicFixture
        rt <- testServeRuntime deadlines noopMetricsPort
        publicHits <- newIORef (0 :: Int)
        privateHits <- newIORef (0 :: Int)
        let busy _ respond = respond (responseLBS status429 [(hRetryAfter, "60")] "busy")
        testWithApplication (pure (countingUpstream publicHits busy)) $ \publicPort ->
            testWithApplication (pure (countingUpstream privateHits upstreamApp)) $ \privatePort -> do
                base <- depsFor publicPort
                let deps = withPrivateBaseUrl (Just (loopbackRegistryUrl ("http://localhost:" <> show privatePort))) base
                    runtime = rt{srPublicControl = pfControl fixture}
                replicateM_ 2 $ do
                    response <- captureServe npmPackumentContract runtime (mountWith deps) (servePackument npmPackumentReplies leftpadName defaultRequest)
                    statusCode (responseStatus response) `shouldBe` 200
                readIORef publicHits `shouldReturn` 1
                readIORef privateHits `shouldReturn` 2
                pfRequests fixture `shouldReturn` [("localhost", PublicRead.Listing, PublicRead.ResponseStatus 429)]

    for_ [(serveTarball, "GET"), (headTarball, "HEAD")] $ \(serve, method) ->
        it ("counts public " <> method <> " sends, excluding retained metadata and local cooldown") $ withSweeper $ \deadlines -> do
            fixture <- newPublicFixture
            rt <- testServeRuntime deadlines noopMetricsPort
            hits <- newIORef (0 :: Int)
            let upstream req respond
                    | rawPathInfo req == "/leftpad/-/leftpad-1.0.0.tgz" = respond (responseLBS status429 [(hRetryAfter, "60")] "busy")
                    | otherwise = upstreamApp req respond
            testWithApplication (pure (countingUpstream hits upstream)) $ \publicPort -> do
                deps <- withPrivateBaseUrl Nothing <$> depsFor publicPort
                let fetch = captureServe npmTarballContract rt{srPublicControl = pfControl fixture} (mountWith deps) (serve npmTarballReplies leftpadName (npmVersion "1.0.0") (unsafeFilename "leftpad-1.0.0.tgz") defaultRequest)
                firstResponse <- fetch
                statusCode (responseStatus firstResponse) `shouldBe` 429
                secondResponse <- fetch
                statusCode (responseStatus secondResponse) `shouldBe` 503
                lookup hRetryAfter (responseHeaders secondResponse) `shouldBe` Just "60"
                readIORef hits `shouldReturn` 2
                pfRequests fixture `shouldReturn` [("localhost", PublicRead.SelectedRead, PublicRead.ResponseStatus 200), ("localhost", PublicRead.Artifact, PublicRead.ResponseStatus 429)]

    for_ [methodGet, methodHead] $ \method ->
        it ("counts public companion " <> show method <> " and refuses its next local retry") $
            testWithApplication (pure (pypiPublicAppWith True status429)) $ \publicPort -> withSweeper $ \deadlines -> do
                fixture <- newPublicFixture
                rt <- testServeRuntime deadlines noopMetricsPort
                let deps = (pypiServeDeps Nothing (loopbackRegistryUrl ("http://localhost:" <> show publicPort)) NoMirrorWrite [] (pure fixedNow)){pdMountBaseUrl = "http://proxy.test"}
                    fetch = fetchPypiPath rt{srPublicControl = pfControl fixture} deps method "requests-1.0.0.tar.gz.metadata" []
                firstResponse <- fetch
                statusCode (simpleStatus firstResponse) `shouldBe` 429
                secondResponse <- fetch
                statusCode (simpleStatus secondResponse) `shouldBe` 503
                lookup hRetryAfter (simpleHeaders secondResponse) `shouldBe` Just "60"
                pfRequests fixture `shouldReturn` [("localhost", PublicRead.SelectedRead, PublicRead.ResponseStatus 200), ("localhost", PublicRead.Companion, PublicRead.ResponseStatus 429)]

    it "counts one HTTP send for coalesced listing followers" $ do
        completed <- timeout 5_000_000 $ withSweeper $ \deadlines -> do
            fixture <- newPublicFixture
            started <- newEmptyMVar
            release <- newEmptyMVar
            joined <- newEmptyMVar
            let metrics = noopMetricsPort{mpCacheRequest = \outcome -> when (outcome == Metric.Collapsed) (putMVar joined ())}
                upstream req respond = putMVar started () >> takeMVar release >> upstreamApp req respond
            rt <- testServeRuntime deadlines metrics
            testWithApplication (pure upstream) $ \publicPort -> do
                deps <- withPrivateBaseUrl Nothing <$> depsFor publicPort
                let fetch = captureServe npmPackumentContract rt{srPublicControl = pfControl fixture} (mountWith deps) (servePackument npmPackumentReplies leftpadName defaultRequest)
                (responses, ()) <-
                    concurrently
                        (mapConcurrently (const fetch) [1 .. 8 :: Int])
                        (takeMVar started >> replicateM_ 7 (takeMVar joined) >> putMVar release ())
                for_ responses $ \response -> statusCode (responseStatus response) `shouldBe` 200
                pfRequests fixture `shouldReturn` [("localhost", PublicRead.Listing, PublicRead.ResponseStatus 200)]
        completed `shouldBe` Just ()

pypiPrivateArtifactSpec :: Spec
pypiPrivateArtifactSpec = describe "PyPI first-party private artifacts" $ do
    it "keeps indexed private addressing when the public artifact host list is empty" $ withSweeper $ \deadlines -> do
        rt <- testServeRuntime deadlines noopMetricsPort
        privateHits <- newIORef (0 :: Int)
        testWithApplication (pure (countingUpstream privateHits (pypiPrivateApp status404))) $ \privatePort -> do
            let deps = pypiPrivateDeps privatePort 1
                independent = deps{pdArtifact = (pdArtifact deps){artifactHosts = []}}
            response <- capturePypiArtifact rt independent methodGet
            statusCode (responseStatus response) `shouldBe` 404
            readIORef privateHits `shouldReturn` 2

    it "keeps npm's conventional private path when it declares a public artifact host" $ withSweeper $ \deadlines -> do
        rt <- testServeRuntime deadlines noopMetricsPort
        privateHits <- newIORef (0 :: Int)
        testWithApplication (pure (countingUpstream privateHits upstreamApp)) $ \privatePort -> do
            deps <- depsFor 1
            let privateDeps = withPrivateBaseUrl (Just (loopbackRegistryUrl ("http://localhost:" <> show privatePort))) deps
                independent = privateDeps{pdArtifact = (pdArtifact privateDeps){artifactHosts = ["https://artifacts.example"]}}
            response <- captureServe npmTarballContract rt (mountWith independent) (serveTarball npmTarballReplies leftpadName (npmVersion "1.0.0") (unsafeFilename "leftpad-1.0.0.tgz") defaultRequest)
            statusCode (responseStatus response) `shouldBe` 200
            readIORef privateHits `shouldReturn` 1

    it "answers 503 without a suggested delay when the private origin cannot be read" $ withSweeper $ \deadlines -> do
        (metricsPort, decisions) <- recordingMetricsPort
        rt <- testServeRuntime deadlines metricsPort
        publicHits <- newIORef (0 :: Int)
        testWithApplication (pure (countingUpstream publicHits upstreamApp)) $ \publicPort -> do
            let deps = pypiPrivateDeps 1 publicPort
            for_ [methodGet, methodHead] $ \method -> do
                response <- capturePypiArtifact rt deps method
                statusCode (responseStatus response) `shouldBe` 503
                lookup hRetryAfter (responseHeaders response) `shouldBe` Nothing
            readIORef publicHits `shouldReturn` 0
            decisions `shouldReturn` [Unavailable, Unavailable]

    for_ [(status404, 404, Deny), (status302, 503, Unavailable), (status429, 503, Unavailable), (status500, 503, Unavailable)] $ \(privateStatus, expectedStatus, expectedDecision) ->
        it ("answers " <> show expectedStatus <> " after a private index hit and artifact HTTP " <> show (statusCode privateStatus)) $ withSweeper $ \deadlines -> do
            (metricsPort, decisions) <- recordingMetricsPort
            rt <- testServeRuntime deadlines metricsPort
            publicHits <- newIORef (0 :: Int)
            privateHits <- newIORef (0 :: Int)
            testWithApplication (pure (countingUpstream publicHits upstreamApp)) $ \publicPort ->
                testWithApplication (pure (countingUpstream privateHits (pypiPrivateApp privateStatus))) $ \privatePort -> do
                    for_ [methodGet, methodHead] $ \method -> do
                        response <- capturePypiArtifact rt (pypiPrivateDeps privatePort publicPort) method
                        statusCode (responseStatus response) `shouldBe` expectedStatus
                        lookup hRetryAfter (responseHeaders response) `shouldBe` Nothing
                    readIORef privateHits `shouldReturn` 4
                    readIORef publicHits `shouldReturn` 0
                    decisions `shouldReturn` [expectedDecision, expectedDecision]

pypiPrivateDeps :: Int -> Int -> PackumentDeps
pypiPrivateDeps privatePort publicPort =
    (pypiServeDeps (Just (loopbackRegistryUrl ("http://localhost:" <> show privatePort))) (loopbackRegistryUrl ("http://localhost:" <> show publicPort)) (MirrorOnAdmit (loopbackRegistryUrl "http://mirror.test")) [] (pure fixedNow))
        { pdFirstParty = (== requestsName)
        }

capturePypiArtifact :: ServeRuntime -> PackumentDeps -> Method -> IO Response
capturePypiArtifact rt deps method =
    case pypiRouter method [] ["simple", "requests", "requests-1.0.0.tar.gz"] of
        RouteAction contract (RunPipeline _ action) -> captureServe contract rt binding (action defaultRequest)
        _ -> throwIO MissingFixtureResponse
  where
    binding = (mountUnder pypiCredential deps){bindingPrefix = "pypi" :| [], bindingRouter = pypiRouter}

pypiPrivateApp :: Status -> Application
pypiPrivateApp privateStatus req respond
    | rawPathInfo req == "/simple/requests/" =
        respond (responseLBS status200 [(hContentType, "application/vnd.pypi.simple.v1+json")] (encode index))
    | otherwise = respond (responseLBS privateStatus [] "private refusal")
  where
    index = simpleIndex "requests" [withFileKeys [("url", String (selfBaseUrl req <> "/files/requests-1.0.0.tar.gz"))] (simpleFile "requests-1.0.0.tar.gz")]

-- The zero waiting room makes a held step refuse the next request at once.
fullDoorRuntime :: Deadlines -> MetricsPort -> IO (ServeRuntime, MemoryMeter)
fullDoorRuntime deadlines metricsPort = do
    rt <- testServeRuntime deadlines metricsPort
    meter <- newMemoryMeter deadlines MeterSettings{msBudgetBytes = 1, msStepBytes = 1, msEntryRoom = 0, msEntryWaitMicros = 0}
    pure (rt{srMemoryMeter = meter}, meter)

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

artifactBytes :: ByteString
artifactBytes = "leftpad artifact bytes"

-- Pin the upstream's Date, so two relays of one artifact cannot differ by a clock tick.
dated :: Application -> Application
dated upstream req respond = upstream req (respond . mapResponseHeaders ((hDate, "Thu, 01 Jan 2026 00:00:00 GMT") :))

-- A relay with and without Accept-Encoding gzip: the same status, headers and bytes, and no coding.
relaysUncoded :: Int -> (RequestHeaders -> IO SResponse) -> Expectation
relaysUncoded expected fetch = do
    plain <- fetch []
    offered <- fetch [(hAcceptEncoding, "gzip")]
    statusCode (simpleStatus offered) `shouldBe` expected
    simpleStatus offered `shouldBe` simpleStatus plain
    simpleHeaders offered `shouldBe` simpleHeaders plain
    simpleBody offered `shouldBe` simpleBody plain
    lookup hContentEncoding (simpleHeaders offered) `shouldBe` Nothing

-- A public PyPI origin whose one distribution answers with the given status. A 206 carries a partial body.
pypiPublicApp :: Status -> Application
pypiPublicApp = pypiPublicAppWith False

pypiPublicAppWith :: Bool -> Status -> Application
pypiPublicAppWith companion artifactStatus req respond
    | rawPathInfo req == "/simple/requests/" = respond (responseLBS status200 [(hContentType, "application/vnd.pypi.simple.v1+json")] (encode index))
    | companion && rawPathInfo req == "/files/requests-1.0.0.tar.gz.metadata" = respond (responseLBS artifactStatus [(hRetryAfter, "60")] "metadata")
    | rawPathInfo req == "/files/requests-1.0.0.tar.gz" = respond (responseLBS artifactStatus ((hContentType, "application/octet-stream") : range) (LBS.fromStrict body))
    | otherwise = respond (responseLBS status404 [] "")
  where
    index = simpleIndex "requests" [withFileKeys ([("url", String (selfBaseUrl req <> "/files/requests-1.0.0.tar.gz"))] <> [("core-metadata", Bool True) | companion]) (simpleFile "requests-1.0.0.tar.gz")]
    partial = statusCode artifactStatus == 206
    body = if partial then BS.take 7 artifactBytes else artifactBytes
    range = [(hContentRange, "bytes 0-6/22") | partial]

-- A PyPI distribution request through the mount's router, so a HEAD takes the bodiless contract.
fetchPypiArtifact :: ServeRuntime -> PackumentDeps -> Method -> RequestHeaders -> IO SResponse
fetchPypiArtifact rt deps method = fetchPypiPath rt deps method "requests-1.0.0.tar.gz"

fetchPypiPath :: ServeRuntime -> PackumentDeps -> Method -> Text -> RequestHeaders -> IO SResponse
fetchPypiPath rt deps method file headers =
    case pypiRouter method headers ["simple", "requests", file] of
        RouteAction contract (RunPipeline _ action) -> do
            logEnv <- newTestLogEnv
            runSession (WaiTest.request defaultRequest{requestMethod = method, requestHeaders = headers}) $ \req respond ->
                runHandler logEnv mempty (RequestCtx rt binding) (action req (respond . responseToWai contract))
        _ -> throwIO MissingFixtureResponse
  where
    binding = (mountUnder pypiCredential deps){bindingPrefix = "pypi" :| [], bindingRouter = pypiRouter}

-- An upstream that answers the artifact with a partial reply, as a range-serving store may.
partialUpstream :: Application
partialUpstream req respond
    | rawPathInfo req == "/leftpad/-/leftpad-1.0.0.tgz" =
        respond (responseLBS status206 [(hContentType, "application/octet-stream"), (hContentRange, "bytes 0-6/22")] (LBS.fromStrict (BS.take 7 artifactBytes)))
    | otherwise = upstreamApp req respond

packumentWithIntegrity :: ByteString -> Text -> Value
packumentWithIntegrity host integrity = privatePackumentOver host integrity ["1.0.0"] "1.0.0"

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
                , vsExtraPairs = ["dependencies" .= object ["private-marker" .= ("^1.0.0" :: Text)], "_retained" .= ("private field" :: Text)]
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
