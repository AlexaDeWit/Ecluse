{- | Unit cover for the core serve handlers ("Ecluse.Core.Server.Pipeline") driven
__directly__ over a 'ServeRuntime' of test doubles -- no application 'Env', no
OpenTelemetry SDK.

This is the partition's proof that the request pipeline is genuinely core: it constructs
the request runtime from a recording metrics port, a pass-through tracing port, an
in-memory queue, and a real cache and HTTP manager, then runs the handlers through the
core 'runHandler' against a scribe-less @katip@ environment. The handlers serve a merged
packument and a gated tarball, degrade to an unavailability when no upstream resolves,
and stub an unwired mount -- and the recording port confirms the serve decision each path
recorded through the interface. The exhaustive serve-path behaviour (every status, the
credential split, the merge) is covered through the real stack in the integration
suite's @Ecluse.Server.PipelineSpec@; this pins that the handlers run over the ports.
-}
module Ecluse.Server.PipelineSpec (spec) where

import Crypto.Hash (Digest, SHA512, hash)
import Data.Aeson (Value, encode, object, (.=))
import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import Data.ByteString.Lazy qualified as LBS
import Data.Time (UTCTime (UTCTime), fromGregorian, nominalDay)
import Katip (Environment (Environment), Namespace (Namespace), initLogEnv)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (hContentType, status200, status404, statusCode)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Package.Integrity (defaultMinIntegrity, defaultMinTrustedIntegrity)
import Ecluse.Core.Queue (newInMemoryQueue)
import Ecluse.Core.Registry.Npm (NpmClientConfig (..))
import Ecluse.Core.Registry.Npm.Filter (assembleMergedPackument)
import Ecluse.Core.Registry.Npm.Request (artifactRequestByFile, artifactRequestByUrl)
import Ecluse.Core.Registry.Npm.Route qualified as Npm
import Ecluse.Core.Registry.Npm.Serve (npmRenderer)
import Ecluse.Core.Rules (inertRuleDeps, prepare)
import Ecluse.Core.Rules.Types (PrecededRule, Rule (AllowIfOlderThan), atDefaultPrecedence)
import Ecluse.Core.Security (TarballHostPolicy (SameHostAsPackument), defaultLimits, tarballHostGate)
import Ecluse.Core.Server.Admission (ServeAdmission, newServeAdmission, newServeAdmissionTuned, unlimitedServeAdmission, withServeAdmission)
import Ecluse.Core.Server.Cache (defaultCacheConfig, newMetadataCache)
import Ecluse.Core.Server.Context (
    Handler,
    MountBinding (..),
    PackumentDeps (..),
    RequestCtx (RequestCtx),
    ServeRuntime (ServeRuntime, srMetrics),
    runHandler,
 )
import Ecluse.Core.Server.Metadata (newNpmMetadataClient)
import Ecluse.Core.Server.Pipeline (servePackument, serveTarball)
import Ecluse.Core.Server.Pipeline.Publish ()
import Ecluse.Core.Server.Pipeline.Shared (hRetryAfter)
import Ecluse.Core.Server.Route (Filename (Filename))
import Ecluse.Core.Telemetry.Metrics (Decision (Admit, Unavailable))
import Ecluse.Core.Telemetry.Record (MetricsPort)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Port (passthroughTracingPort, recordingMetricsPort)
import Network.HTTP.Types.Header (hHost)
import Network.Wai (Application, Request (rawPathInfo, requestHeaders), Response, defaultRequest, responseHeaders, responseLBS, responseStatus)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Internal (ResponseReceived (ResponseReceived))
import Test.Hspec
import UnliftIO.Exception (throwString)

spec :: Spec
spec = describe "Ecluse.Core.Server.Pipeline (core handlers over a ServeRuntime)" $ do
    it "serves a merged packument and records an admit through the metrics port" $
        testWithApplication (pure upstreamApp) $ \port -> do
            (metricsPort, decisions) <- recordingMetricsPort
            rt <- mkRuntime metricsPort
            deps <- depsFor port
            resp <- captureServe rt (mountWith (Just deps)) (servePackument leftpad defaultRequest)
            statusCode (responseStatus resp) `shouldBe` 200
            decisions >>= (`shouldBe` [Admit])

    it "records an unavailability and renders 503 when no upstream resolves" $ do
        (metricsPort, decisions) <- recordingMetricsPort
        rt <- mkRuntime metricsPort
        -- 'depsFor 1' points both origins at a closed port, so each fetch is refused.
        deps <- depsFor 1
        resp <- captureServe rt (mountWith (Just deps)) (servePackument leftpad defaultRequest)
        statusCode (responseStatus resp) `shouldBe` 503
        decisions >>= (`shouldBe` [Unavailable])

    it "renders 501 for a packument route whose mount has no serve dependencies wired" $ do
        (metricsPort, _decisions) <- recordingMetricsPort
        rt <- mkRuntime metricsPort
        resp <- captureServe rt (mountWith Nothing) (servePackument leftpad defaultRequest)
        statusCode (responseStatus resp) `shouldBe` 501

    it "serves a gated tarball and records an admit, driving the metrics and tracing ports" $
        testWithApplication (pure upstreamApp) $ \port -> do
            (metricsPort, decisions) <- recordingMetricsPort
            rt <- mkRuntime metricsPort
            deps <- depsFor port
            resp <-
                captureServe
                    rt
                    (mountWith (Just deps))
                    (serveTarball leftpad (mkVersion Npm "1.0.0") (Filename "leftpad-1.0.0.tgz") defaultRequest)
            statusCode (responseStatus resp) `shouldBe` 200
            decisions >>= (`shouldBe` [Admit])

    it "sheds packument work when metadata admission refuses" $ do
        (metricsPort, _decisions) <- recordingMetricsPort
        -- A tuned handle with no waiting room, so the saturated attempt is refused
        -- outright: these pipeline tests own the rendering of a refusal (503 +
        -- Retry-After), not the wait semantics (AdmissionSpec owns those).
        admission <- newServeAdmissionTuned 1 0 0
        rt <- mkRuntimeWith admission metricsPort
        deps <- depsFor 1
        held <- withServeAdmission (srMetrics rt) admission (captureServe rt (mountWith (Just deps)) (servePackument leftpad defaultRequest))
        response <- maybe (expectationFailure "failed to acquire the test's outer admission slot" >> throwString "unreachable") pure held
        statusCode (responseStatus response) `shouldBe` 503
        (snd <$> find ((== hRetryAfter) . fst) (responseHeaders response)) `shouldBe` Just "1"

    it "releases metadata admission after an admitted operation completes" $
        testWithApplication (pure upstreamApp) $ \port -> do
            (metricsPort, _decisions) <- recordingMetricsPort
            admission <- newServeAdmissionTuned 1 0 0
            rt <- mkRuntimeWith admission metricsPort
            deps <- depsFor port
            saturated <- withServeAdmission (srMetrics rt) admission (captureServe rt (mountWith (Just deps)) (servePackument leftpad defaultRequest))
            (statusCode . responseStatus <$> saturated) `shouldBe` Just 503
            admitted <- captureServe rt (mountWith (Just deps)) (servePackument leftpad defaultRequest)
            statusCode (responseStatus admitted) `shouldBe` 200

    it "sheds a tarball miss when its public metadata gate cannot acquire admission" $ do
        (metricsPort, _decisions) <- recordingMetricsPort
        admission <- newServeAdmissionTuned 1 0 0
        rt <- mkRuntimeWith admission metricsPort
        deps <- depsFor 1
        held <-
            withServeAdmission (srMetrics rt) admission $
                captureServe
                    rt
                    (mountWith (Just deps))
                    (serveTarball leftpad (mkVersion Npm "1.0.0") (Filename "leftpad-1.0.0.tgz") defaultRequest)
        response <- maybe (expectationFailure "failed to acquire the test's outer admission slot" >> throwString "unreachable") pure held
        statusCode (responseStatus response) `shouldBe` 503
        (snd <$> find ((== hRetryAfter) . fst) (responseHeaders response)) `shouldBe` Just "1"

    it "does not hold metadata admission around a trusted private tarball stream" $
        testWithApplication (pure upstreamApp) $ \port -> do
            (metricsPort, _decisions) <- recordingMetricsPort
            admission <- newServeAdmission 1
            rt <- mkRuntimeWith admission metricsPort
            deps <- depsFor 1
            let privateDeps = deps{pdPrivateBaseUrl = "http://localhost:" <> show port}
            held <-
                withServeAdmission (srMetrics rt) admission $
                    captureServe
                        rt
                        (mountWith (Just privateDeps))
                        (serveTarball leftpad (mkVersion Npm "1.0.0") (Filename "leftpad-1.0.0.tgz") defaultRequest)
            (statusCode . responseStatus <$> held) `shouldBe` Just 200

{- | Run a serve handler over a request runtime and mount, capturing the 'Response' it
hands its continuation. The handler runs through the core 'runHandler' against a
scribe-less @katip@ environment (its warnings have nowhere to go, which is what these
tests want) and an empty initial context (no active span, so no @dd@).
-}
captureServe :: ServeRuntime -> MountBinding -> ((Response -> IO ResponseReceived) -> Handler ResponseReceived) -> IO Response
captureServe rt binding mkHandler = do
    logEnv <- initLogEnv (Namespace ["ecluse"]) (Environment "test")
    captured <- newIORef Nothing
    let respond resp = writeIORef captured (Just resp) >> pure ResponseReceived
    _ <- runHandler logEnv mempty (RequestCtx rt binding) (mkHandler respond)
    maybe (throwString "the handler produced no response") pure =<< readIORef captured

{- | A request runtime over the recording metrics port, the pass-through tracing port,
a real (no-TLS) manager shared by both legs, a fresh cache, and an in-memory queue.
-}
mkRuntime :: MetricsPort -> IO ServeRuntime
mkRuntime metricsPort = do
    mkRuntimeWith unlimitedServeAdmission metricsPort

mkRuntimeWith :: ServeAdmission -> MetricsPort -> IO ServeRuntime
mkRuntimeWith admission metricsPort = do
    manager <- newManager defaultManagerSettings
    cache <- newMetadataCache defaultCacheConfig
    queue <- newInMemoryQueue
    pure (ServeRuntime admission manager manager cache queue metricsPort passthroughTracingPort)

leftpad :: PackageName
leftpad = mkPackageName Npm Nothing "leftpad"

-- | An npm mount over the given serve dependencies (or 'Nothing' for the unwired stub).
mountWith :: Maybe PackumentDeps -> MountBinding
mountWith deps =
    MountBinding
        { bindingPrefix = "npm" :| []
        , bindingClassifier = Npm.classify
        , bindingPackumentDeps = deps
        , bindingPublishDeps = Nothing
        , bindingRenderer = npmRenderer
        }

{- | Serve dependencies pointing the public origin at the in-process upstream on
@publicPort@ and the private origin at a closed port (so the trusted leg always degrades
and the merge serves the public contribution). The loopback stubs are addressed by the
@localhost@ DNS name rather than a bare IP literal, so the internal-range block (which
only recognises a literal) never fires on the artifact leg -- no opt-in is needed.
-}
depsFor :: Int -> IO PackumentDeps
depsFor publicPort = do
    prepared <- prepare inertRuleDeps allowPolicy
    pure
        PackumentDeps
            { pdPrivateBaseUrl = "http://localhost:1"
            , pdPublicBaseUrl = "http://localhost:" <> show publicPort
            , pdMountBaseUrl = "http://proxy.test"
            , pdMirrorTarget = "http://mirror.test"
            , pdRules = prepared
            , pdTarballHostPolicy = SameHostAsPackument
            , pdAdditionalBlockedRanges = []
            , pdTarballHostGate = tarballHostGate "http://localhost:1" ("http://localhost:" <> show publicPort) "http://mirror.test"
            , pdLimits = defaultLimits
            , pdInboundToken = Nothing
            , pdNow = pure fixedNow
            , pdHelp = Nothing
            , pdMinIntegrity = defaultMinIntegrity
            , pdMinTrustedIntegrity = defaultMinTrustedIntegrity
            , pdNewMetadataClient = \t p u c f1 f2 f3 l m t' s -> newNpmMetadataClient t p u c f1 f2 f3 (NpmClientConfig t' m s l)
            , pdBuildArtifactRequestByFile = \_ _ t s -> artifactRequestByFile t s
            , pdBuildArtifactRequestByUrl = \_ _ t s -> artifactRequestByUrl t s
            , pdAssemble = assembleMergedPackument
            }

{- | A pure rule policy that admits the fixture version: the rules engine is
deny-by-default, so an empty policy denies every version ("no rule allowed it"). The
quarantine rule admits a version published more than a week before @now@, which the
fixture (published 2019, @now@ 2020) clears.
-}
allowPolicy :: [PrecededRule]
allowPolicy = [atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))]

-- | A fixed wall clock against which the fixture version reads as well-aged.
fixedNow :: UTCTime
fixedNow = UTCTime (fromGregorian 2020 1 1) 0

{- | A minimal npm upstream: it answers @GET \/leftpad@ with a one-version packument
whose @dist.tarball@ self-hosts on this upstream (the host taken from the request, so
the ephemeral test port is woven in) and carries a real SHA-512 @integrity@ over the
served artifact bytes, and answers that tarball path with the bytes themselves.
-}
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

-- | The artifact bytes the upstream serves and the packument's @integrity@ commits to.
artifactBytes :: ByteString
artifactBytes = "leftpad artifact bytes"

-- | A one-version packument for @leftpad@, its tarball self-hosted on @host@.
packumentFor :: ByteString -> Value
packumentFor host =
    object
        [ "name" .= ("leftpad" :: Text)
        , "dist-tags" .= object ["latest" .= ("1.0.0" :: Text)]
        , "versions"
            .= object
                [ "1.0.0"
                    .= object
                        [ "name" .= ("leftpad" :: Text)
                        , "version" .= ("1.0.0" :: Text)
                        , "dist"
                            .= object
                                [ "tarball" .= ("http://" <> (decodeUtf8 host :: Text) <> "/leftpad/-/leftpad-1.0.0.tgz")
                                , "integrity" .= sha512Integrity artifactBytes
                                ]
                        ]
                ]
        , "time" .= object ["1.0.0" .= ("2019-01-01T00:00:00.000Z" :: Text)]
        ]

-- | The Subresource-Integrity @sha512-<base64>@ string over the given bytes.
sha512Integrity :: ByteString -> Text
sha512Integrity bytes = "sha512-" <> decodeUtf8 (convertToBase Base64 (hash bytes :: Digest SHA512) :: ByteString)
