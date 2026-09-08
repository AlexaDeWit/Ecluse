-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Exercise core serve handlers through their runtime ports.
Responses and emitted metrics remain observable independently of application wiring.
Admission lifetime cases connect sweep deletions to the next private request.
-}
module Ecluse.Core.Server.PipelineSpec (spec) where

import Data.Aeson (Value, encode, (.=))
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
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Registry.Maintenance (ConsentVerdict (ConsentWithheld), StoreClass (StorePreserved), StoredVersion (StoredVersion), VersionPresence (VersionServed))
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
import Ecluse.Core.Registry.Sweep.Types (CycleOutcome (outcomeTally), SweepMount (smFirstParty), SweepPacing (swpShape), SweepShape (SweepCandidates, SweepEverything), SweepTally (tallyDeleted, tallyExamined))
import Ecluse.Core.Rules (PreparedRule, evalRules, prepare)
import Ecluse.Core.Rules.Types (PrecededRule, Rule (AllowIfOlderThan))
import Ecluse.Core.Rules.Types qualified as Rules
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission (ServeAdmission, newServeAdmission, newServeAdmissionTuned, withServeAdmission)
import Ecluse.Core.Server.Cache (newMetadataCache)
import Ecluse.Core.Server.Context (
    Handler,
    MountBinding (..),
    PackumentDeps (..),
    RequestCtx (RequestCtx),
    ServeRuntime (ServeRuntime, srMetrics),
    runHandler,
 )
import Ecluse.Core.Server.Contract (ResponseContract, responseToWai)
import Ecluse.Core.Server.Pipeline (headTarball, servePackument, serveTarball)
import Ecluse.Core.Server.Pipeline.Publish ()
import Ecluse.Core.Server.Pipeline.Shared (hRetryAfter)
import Ecluse.Core.Server.Upstream (MirrorServePlan (MirrorOnAdmit))
import Ecluse.Core.Telemetry.Metrics (Decision (Admit, Deny, Unavailable))
import Ecluse.Core.Telemetry.Record (MetricsPort)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Log (captureStdout, jsonLogEnv, newTestLogEnv)
import Ecluse.Test.Maintenance (FakeStore (fakeMaintenance, readFakeContents), FakeStoreConfig (fakeClass, fakeConsent, fakeContents, fakeManifests), defaultFakeStoreConfig, newFakeStore)
import Ecluse.Test.Package (sampleDetails, sampleManifest, sriSha512Of, unsafeFilename)
import Ecluse.Test.Port (passthroughTracingPort, recordingDivergenceMetricsPort, recordingMetricsPort)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Registry.Npm (VersionSpec (vsIntegrity), packumentValue, versionSpec, versionValue)
import Ecluse.Test.Rules (admittedBy, atDefaultPrecedence, blockedBy, inertRuleDeps, isUndecidable)
import Ecluse.Test.Server.Cache (defaultCacheConfig)
import Ecluse.Test.Server.Mount (npmServeDeps, withPrivateBaseUrl)
import Ecluse.Test.Sweep (RecordedSweep (recPorts), recordingPorts, testMount, testPacing)
import Network.HTTP.Types.Header (RequestHeaders, hHost)
import Network.Wai (Application, Request (rawPathInfo, requestHeaders), Response, defaultRequest, responseHeaders, responseLBS, responseStatus)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Internal (ResponseReceived (ResponseReceived))
import Test.Hspec
import UnliftIO.Exception (throwIO)

-- | Pin client responses and metrics, including trusted reads after a policy change.
spec :: Spec
spec = describe "Ecluse.Core.Server.Pipeline (core handlers over a ServeRuntime)" $ do
    admissionLifetimeSpec

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

    it "logs and meters a cross-upstream integrity divergence, still serving the trusted copy" $
        testWithApplication (pure upstreamApp) $ \publicPort ->
            testWithApplication (pure divergentPrivateApp) $ \privatePort -> do
                (metricsPort, divergences) <- recordingDivergenceMetricsPort
                rt <- mkRuntime metricsPort
                baseDeps <- depsFor publicPort
                let deps = withPrivateBaseUrl (Just (loopbackRegistryUrl ("http://localhost:" <> show privatePort))) baseDeps
                logged <- captureStdout $ do
                    logEnv <- jsonLogEnv
                    resp <- captureServeWithLog logEnv npmPackumentContract rt (mountWith deps) (servePackument npmPackumentReplies leftpad defaultRequest)
                    statusCode (responseStatus resp) `shouldBe` 200
                    void (closeScribes logEnv)
                logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Warning\""
                logged `shouldSatisfy` T.isInfixOf "cross-upstream integrity divergence"
                logged `shouldSatisfy` T.isInfixOf (T.drop 7 (sha512Integrity "leftpad artifact bytes (privately tampered)"))
                logged `shouldSatisfy` T.isInfixOf (T.drop 7 (sha512Integrity artifactBytes))
                divergences >>= (`shouldBe` 1)

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

    it "keeps a first-party packument off the public upstream, answering 404 on a private miss" $ do
        (metricsPort, decisions) <- recordingMetricsPort
        rt <- mkRuntime metricsPort
        hits <- newIORef (0 :: Int)
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
            statusCode (responseStatus firstParty) `shouldBe` 404
            readIORef hits >>= (`shouldBe` 0)
            decisions >>= (`shouldBe` [Deny])
            thirdParty <- serveUnder (/= leftpad)
            statusCode (responseStatus thirdParty) `shouldBe` 200
            readIORef hits >>= (`shouldSatisfy` (> 0))
            decisions >>= (`shouldBe` [Deny, Admit])

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

admissionLifetimeSpec :: Spec
admissionLifetimeSpec = describe "admission lifetime after removing an allow" $
    for_ [SweepCandidates, SweepEverything] $ \shape -> describe (show shape) $ do
        it "retains the copy and serves the next private GET when the only allow disappears" $
            checkLifetime shape (LifetimePolicy [] (== Rules.BlockedByDefault []) Retained) Eligible
        it "removes the copy and denies the next GET when an existing identity deny becomes decisive" $
            checkLifetime shape winningDeny Eligible
        it "keeps trusting the copy when an unchanged higher-priority allow still beats the deny" $
            checkLifetime
                shape
                (LifetimePolicy [Rules.PrecededRule 600 (Rules.AllowByIdentity "leftpad"), identityDeny] ((== Just "AllowByIdentity") . admittedBy) Retained)
                Eligible
        it "retains the copy when unavailable advisory evidence wins ahead of the existing deny" $
            checkLifetime
                shape
                (LifetimePolicy [unavailableCveDeny, identityDeny] isUndecidable Retained)
                Eligible
        it "retains the copy when unavailable advisory evidence is the only remaining rule" $
            checkLifetime
                shape
                (LifetimePolicy [unavailableCveDeny] isUndecidable Retained)
                Eligible
        for_ [FirstParty, ConsentMissing, TargetPreserved, ManifestMissing] $ \protection ->
            it ("retains the denied copy and serves the next private GET under " <> show protection) $
                checkLifetime shape winningDeny protection

data CopyDisposition = Retained | Removed
    deriving stock (Eq)

data LifetimeProtection = Eligible | FirstParty | ConsentMissing | TargetPreserved | ManifestMissing
    deriving stock (Eq, Show)

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

checkLifetime :: SweepShape -> LifetimePolicy -> LifetimeProtection -> Expectation
checkLifetime shape policy protection = do
    ctx <- Rules.mkEvalContext (pure fixedNow) (pure Nothing)
    let version = mkVersion Npm "1.0.0"
        details = sampleDetails leftpad version
        initialPolicy = Rules.PrecededRule 700 (Rules.AllowByIdentity "leftpad@1.0.0") : lpRules policy
    initial <- prepare inertRuleDeps initialPolicy
    admittedBy <$> evalRules ctx initial details `shouldReturn` Just "AllowByIdentity"
    store <- newFakeStore (lifetimeStore protection)
    preparedAfter <- prepare inertRuleDeps (lpRules policy)
    evalRules ctx preparedAfter details >>= (`shouldSatisfy` lpDecision policy)
    recorded <- recordingPorts Nothing
    let mount =
            (testMount (fakeMaintenance store) preparedAfter (map Rules.prRule (lpRules policy)))
                { smFirstParty = \name -> protection == FirstParty && name == leftpad
                }
    outcome <- sweepCycle testPacing{swpShape = shape} (recPorts recorded) [mount]
    let retained = protection /= Eligible || lpDisposition policy == Retained
        expectedVersions = [StoredVersion version VersionServed | retained]
        examined = case protection of
            FirstParty -> 0
            ConsentMissing -> 0
            TargetPreserved -> 0
            _ -> if shape == SweepCandidates && identityDeny `notElem` lpRules policy then 0 else 1
    tallyExamined (outcomeTally outcome) `shouldBe` examined
    tallyDeleted (outcomeTally outcome) `shouldBe` if retained then 0 else 1
    Map.lookup leftpad <$> readFakeContents store `shouldReturn` Just expectedVersions
    nextPrivateGet store preparedAfter retained

lifetimeStore :: LifetimeProtection -> FakeStoreConfig
lifetimeStore protection = case protection of
    ConsentMissing -> seeded{fakeConsent = ConsentWithheld "operator withdrew consent"}
    TargetPreserved -> seeded{fakeClass = StorePreserved "the store has an upstream"}
    ManifestMissing -> seeded{fakeManifests = Map.empty}
    _ -> seeded
  where
    version = mkVersion Npm "1.0.0"
    seeded =
        defaultFakeStoreConfig
            { fakeContents = Map.singleton leftpad [StoredVersion version VersionServed]
            , fakeManifests = Map.singleton leftpad (sampleManifest leftpad [version])
            }

nextPrivateGet :: FakeStore -> [PreparedRule] -> Bool -> Expectation
nextPrivateGet store rules retained = do
    publicHits <- newIORef (0 :: Int)
    privateHits <- newIORef (0 :: Int)
    testWithApplication (pure (countingUpstream publicHits upstreamApp)) $ \publicPort ->
        testWithApplication (pure (countingUpstream privateHits (storedUpstream store))) $ \privatePort -> do
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
    if StoredVersion (mkVersion Npm "1.0.0") VersionServed `elem` Map.findWithDefault [] leftpad contents
        then upstreamApp req respond
        else respond (responseLBS status404 [] "")

-- | Run a serve handler over a request runtime and mount, capturing the 'Response' it hands its continuation.
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

-- | A request runtime over the recording metrics port, sharing one no-TLS manager across both legs.
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

-- | An npm mount over the given serve dependencies (or 'Nothing' for the unwired stub).
mountWith :: PackumentDeps -> MountBinding
mountWith = mountUnder npmCredential

-- | An npm mount carrying the given credential presentation.
mountUnder :: CredentialMapping -> PackumentDeps -> MountBinding
mountUnder mapping deps =
    MountBinding
        { bindingPrefix = "npm" :| []
        , bindingRouter = npmRouter
        , bindingCredential = mapping
        , bindingPackumentDeps = deps
        , bindingPublishDeps = Nothing
        }

-- | A presentation that carries a raw token on @X-Api-Key@, a form npm does not present.
apiKeyCredential :: CredentialMapping
apiKeyCredential = credentialMapping recoverApiKey "X-Api-Key" (encodeUtf8 . unSecret . credSecret)
  where
    recoverApiKey headers = bareCredential . mkSecret . decodeUtf8 <$> lookup "X-Api-Key" headers

-- | The token a gated mount requires at its edge, in the form a client presents it.
edgeToken :: (IsString s) => s
edgeToken = "edge-token"

-- | Require the edge token but leave both upstreams unreachable, distinguishing edge refusal from fetch failure.
gatedDeps :: IO PackumentDeps
gatedDeps = do
    base <- depsFor 1
    pure base{pdInboundToken = Just (mkSecret edgeToken)}

-- | A request presenting the given headers, otherwise the WAI default.
requestWith :: RequestHeaders -> Request
requestWith headers = defaultRequest{requestHeaders = headers}

-- | Serve dependencies pointing the public origin at the in-process upstream on @publicPort@ and the private origin at a closed port.
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

-- | A pure rule policy that admits the fixture version.
allowPolicy :: [PrecededRule]
allowPolicy = [atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))]

-- | A fixed wall clock against which the fixture version reads as well-aged.
fixedNow :: UTCTime
fixedNow = UTCTime (fromGregorian 2020 1 1) 0

-- | A minimal npm upstream serving @leftpad@ and its self-hosted tarball.
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

-- | An upstream that counts every request before delegating.
countingUpstream :: IORef Int -> Application -> Application
countingUpstream hits app req respond = modifyIORef' hits (+ 1) >> app req respond

-- | The artifact bytes the upstream serves and the packument's @integrity@ commits to.
artifactBytes :: ByteString
artifactBytes = "leftpad artifact bytes"

-- | Keep artifact location and metadata fixed while varying the asserted integrity.
packumentWithIntegrity :: ByteString -> Text -> Value
packumentWithIntegrity host integrity =
    packumentValue
        "leftpad"
        "1.0.0"
        [
            ( "1.0.0"
            , versionValue
                ( (versionSpec "leftpad" "1.0.0" ("http://" <> decodeUtf8 host <> "/leftpad/-/leftpad-1.0.0.tgz"))
                    { vsIntegrity = Just integrity
                    }
                )
            )
        ]
        ["1.0.0" .= ("2019-01-01T00:00:00.000Z" :: Text)]
        []

-- | The public copy's packument: its integrity is a real SHA-512 over the served bytes.
packumentFor :: ByteString -> Value
packumentFor host = packumentWithIntegrity host (sha512Integrity artifactBytes)

-- | The Subresource-Integrity @sha512-<base64>@ string over the given bytes.
sha512Integrity :: ByteString -> Text
sha512Integrity = sriSha512Of

-- | A private (trusted) upstream whose @leftpad@ 1.0.0 integrity contradicts the public copy on the shared SHA-512 algorithm.
divergentPrivateApp :: Application
divergentPrivateApp req respond =
    case rawPathInfo req of
        "/leftpad" ->
            respond (responseLBS status200 [(hContentType, "application/json")] (encode (packumentForDivergent host)))
        _ -> respond (responseLBS status404 [] "")
  where
    host = maybe "localhost" snd (find ((== hHost) . fst) (requestHeaders req))

-- | Use a different SHA-512 digest that still meets the integrity floor.
packumentForDivergent :: ByteString -> Value
packumentForDivergent host = packumentWithIntegrity host (sha512Integrity "leftpad artifact bytes (privately tampered)")
