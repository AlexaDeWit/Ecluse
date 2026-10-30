-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Mirror-pipeline role composition, worker liveness, ecosystem mount bindings, and the data-plane clients and their client spans.
module Ecluse.ServiceSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Time (addUTCTime, getCurrentTime)
import Katip (LogEnv)
import Network.HTTP.Client (HttpException (HttpExceptionRequest), HttpExceptionContent (InternalException), Request, brConsume, defaultManagerSettings, httpNoBody, newManager, parseRequest, responseBody)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp qualified as Warp
import OpenTelemetry.Internal.Logging (getGlobalErrorHandler, setGlobalErrorHandler)
import OpenTelemetry.MeterProvider (SdkMeterEnv)
import Test.Hspec
import UnliftIO (bracket)
import UnliftIO.Exception (try)

import Ecluse.Boot (BootEnv (BootEnv))
import Ecluse.Composition.Credential (initTargetCredentialProviders)
import Ecluse.Composition.Executable (ExecutablePlan (epRoleWiring), MirrorWiring (mwCveSync), RoleWiring (MirrorPipelineWiring), planExecutable)
import Ecluse.Composition.Maintenance (StoreBuilds (StoreBuilds, sbDeleting, sbObserving, sbProbing))
import Ecluse.Composition.Support (expectConfig, expectPlanFor, noCeiling, staticEnvVars)
import Ecluse.Composition.TelemetrySupport (advisoryAgePoints, newAdvisoryHandles, withGlobalRoleTelemetry, withRoleLog, withRoleTelemetry)
import Ecluse.Composition.Types (BootRole (BootMirrorPipeline), MirrorRole (MirrorOnly, ServeAndMirror, ServeOnly))
import Ecluse.Core.BuildIdentity (parseOperatorContact, renderUserAgent)
import Ecluse.Core.Cve.Slot (swapIn)
import Ecluse.Core.Cve.Types (DbEtag (DbEtag))
import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Queue (noMirrorQueue)
import Ecluse.Core.Registry.Adapter (adapterFor)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient)
import Ecluse.Core.Registry.Maintenance.Upstream (noUpstreamMechanism)
import Ecluse.Core.Registry.ResponseWait qualified as ResponseWait
import Ecluse.Core.Telemetry.Metrics (Label (LEcosystem), metricAttributes)
import Ecluse.Core.Worker (
    Liveness (liveHealthy, liveLastPoll),
    WorkerHeartbeat,
    newWorkerHeartbeat,
    recordPoll,
    workerHeartbeatStaleAfter,
 )
import Ecluse.Core.Worker.Liveness (newWorkerHeartbeatWithClock)
import Ecluse.Cve.Sync (CveSyncHandle (csEnv))
import Ecluse.Runtime.Cve.Sync (SyncEnv (syncSlot))
import Ecluse.Runtime.Env (envPrivateClient, envPublicClient)
import Ecluse.Runtime.Server (MountBinding (bindingPrefix))
import Ecluse.Runtime.Telemetry (Telemetry, telemetryDisabled)
import Ecluse.Runtime.Telemetry.Tracing (telemetryWaiMiddleware)
import Ecluse.Runtime.Test.Telemetry (activeSpanContext, histogramCounts, sumPoints)
import Ecluse.Service (ServiceRuntime (svcEnv), mountBindingFor, withServiceRuntime, workerLiveness)
import Ecluse.Test.Cve (fakeCveDb)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Maintenance (FakeStore (fakeMaintenance, fakeObservation), defaultFakeStoreConfig, newFakeStore)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Server.Mount (inertDepsFor, inertPackumentDeps)
import Ecluse.Test.Stub (headerValue, lastCaptured, stubBaseUrl, withStub)
import Ecluse.Test.Stub.Socket (resetAfterPartialBodyOverTcp, withRawServer)
import Ecluse.Test.Support (newTestClock)

stalledHeartbeat :: IO WorkerHeartbeat
stalledHeartbeat = do
    heartbeat <- newWorkerHeartbeat
    now <- getCurrentTime
    recordPoll heartbeat (addUTCTime (negate (workerHeartbeatStaleAfter + 60)) now)
    pure heartbeat

spec :: Spec
spec = do
    advisoryAgeSpec
    dataPlaneSpec
    clientSpanSpec
    outboundIdentitySpec
    describe "workerLiveness, what /livez answers once the spawn decision is derived" $ do
        it "reports a stalled consume loop as not live where the process runs one" $ do
            liveness <- stalledHeartbeat >>= workerLiveness True
            liveHealthy liveness `shouldBe` False

        it "stays live where the process runs no consume loop to stall" $ do
            liveness <- stalledHeartbeat >>= workerLiveness False
            liveHealthy liveness `shouldBe` True
            liveLastPoll liveness `shouldBe` Nothing

        it "carries the last poll instant so an orchestrator can judge staleness itself" $ do
            heartbeat <- newWorkerHeartbeat
            now <- getCurrentTime
            recordPoll heartbeat now
            liveness <- workerLiveness True heartbeat
            liveHealthy liveness `shouldBe` True
            liveLastPoll liveness `shouldBe` Just now

        it "is live before the first poll, because a starting worker is not a stalled one" $ do
            liveness <- newWorkerHeartbeat >>= workerLiveness True
            liveHealthy liveness `shouldBe` True
            liveLastPoll liveness `shouldBe` Nothing

        it "fails expired startup only when the process runs a worker" $ do
            now <- getCurrentTime
            (clock, setClock) <- newTestClock now
            heartbeat <- newWorkerHeartbeatWithClock clock
            setClock (addUTCTime (workerHeartbeatStaleAfter + 1) now)
            running <- workerLiveness True heartbeat
            liveHealthy running `shouldBe` False
            liveLastPoll running `shouldBe` Nothing
            absent <- workerLiveness False heartbeat
            liveHealthy absent `shouldBe` True
            liveLastPoll absent `shouldBe` Nothing

    describe "mountBindingFor, where the ecosystem drives the binding" $ do
        it "resolves npm to a binding whose prefix is derived from the ecosystem (/npm)" $
            (bindingPrefix <$> mountBindingFor Npm inertPackumentDeps Nothing) `shouldBe` Just ("npm" :| [])

        it "resolves PyPI to a binding under its own derived prefix (/pypi)" $
            (bindingPrefix <$> mountBindingFor PyPI inertPackumentDeps Nothing) `shouldBe` Just ("pypi" :| [])

        it "binds Cargo under its own prefix" $ do
            adapter <- maybe (fail "Cargo has no registered adapter") pure (adapterFor Cargo)
            let deps = inertDepsFor adapter
            (bindingPrefix <$> mountBindingFor Cargo deps Nothing) `shouldBe` Just ("cargo" :| [])

        it "binds RubyGems under its own prefix without publication dependencies" $ do
            adapter <- maybe (fail "RubyGems has no registered adapter") pure (adapterFor RubyGems)
            let deps = inertDepsFor adapter
            (bindingPrefix <$> mountBindingFor RubyGems deps Nothing) `shouldBe` Just ("rubygems" :| [])

dataPlaneSpec :: Spec
dataPlaneSpec = describe "the data-plane clients" $ do
    it "type a body read the upstream resets after its 200, with telemetry off" $
        withRoleLog (`typesBodyResetUnder` telemetryDisabled)

    it "type a body read the upstream resets after its 200, under the client span" $
        withGlobalRoleTelemetry (\logEnv telemetry _ -> typesBodyResetUnder logEnv telemetry)

-- A body read through a data-plane client that the upstream resets raises the client's own exception, which the relay and the exchanges catch.
typesBodyResetUnder :: LogEnv -> Telemetry -> IO ()
typesBodyResetUnder logEnv telemetry = withRuntimeClients staticEnvVars logEnv telemetry $ \clients ->
    withRawServer resetAfterPartialBodyOverTcp $ \port -> do
        request <- parseRequest ("http://127.0.0.1:" <> show port <> "/")
        for_ clients $ \client ->
            try (ResponseWait.withResponse client request (brConsume . responseBody)) >>= \case
                Left (HttpExceptionRequest _ (InternalException _)) -> pass
                Left other -> expectationFailure ("expected a typed body-read reset, got " <> show other)
                Right _ -> expectationFailure "a reset body was read whole"

{- Each exchange through a data-plane client runs under one client span that ends with it, so the
in-flight gauge returns to zero and the thread's context is the one it held before. -}
clientSpanSpec :: Spec
clientSpanSpec = describe "the data-plane client span" $ do
    it "counts every exchange once and leaves none in flight" $
        withTracedClients $ \_ meterEnv clients request -> do
            for_ clients $ \client -> replicateM_ exchangesPerClient (exchangeOnce client request)
            let exchanged = fromIntegral (exchangesPerClient * length clients)
            sum . map snd <$> sumPoints "http.client.active_requests" meterEnv `shouldReturn` 0
            sum . map snd <$> sumPoints "http.client.request.count" meterEnv `shouldReturn` exchanged
            sum <$> histogramCounts "http.client.request.duration" meterEnv `shouldReturn` fromIntegral exchanged

    it "leaves the thread's context as it found it" $
        withTracedClients $ \_ _ clients request ->
            for_ clients $ \client -> do
                entered <- activeSpanContext
                exchangeOnce client request
                activeSpanContext `shouldReturn` entered

    it "lets the server span's frame around an exchange restore its context with no diagnostic" $
        withTracedClients $ \telemetry _ clients request -> do
            diagnostics <- newIORef []
            middleware <- telemetryWaiMiddleware telemetry
            caller <- newManager defaultManagerSettings
            withGlobalErrorHandler (\diagnostic -> modifyIORef' diagnostics (diagnostic :)) $
                for_ clients $ \client ->
                    Warp.testWithApplication (pure (middleware (relayingThrough client request))) $ \port ->
                        parseRequest ("http://127.0.0.1:" <> show port <> "/") >>= void . (`httpNoBody` caller)
            readIORef diagnostics `shouldReturn` []

exchangesPerClient :: Int
exchangesPerClient = 5

-- The runtime's data-plane clients under telemetry installed as the boot installs it, and a request to a stub that answers 200.
withTracedClients :: (Telemetry -> SdkMeterEnv -> [ExchangeClient] -> Request -> IO ()) -> IO ()
withTracedClients use =
    withGlobalRoleTelemetry $ \logEnv telemetry meterEnv ->
        withRuntimeClients staticEnvVars logEnv telemetry $ \clients ->
            withStub status200 "ok" $ \stub ->
                parseRequest (toString (stubBaseUrl stub)) >>= use telemetry meterEnv clients

exchangeOnce :: ExchangeClient -> Request -> IO ()
exchangeOnce client request = ResponseWait.withResponse client request (void . brConsume . responseBody)

-- One upstream exchange inside the request, as the artifact relay makes on the request thread.
relayingThrough :: ExchangeClient -> Request -> Application
relayingThrough client request _ respond = exchangeOnce client request >> respond (responseLBS status200 [] "relayed")

-- Route the SDK's own diagnostics to the handler for the length of the action.
withGlobalErrorHandler :: (String -> IO ()) -> IO a -> IO a
withGlobalErrorHandler handler action =
    bracket (getGlobalErrorHandler <* setGlobalErrorHandler handler) setGlobalErrorHandler (const action)

outboundIdentitySpec :: Spec
outboundIdentitySpec = describe "the runtime's outbound registry identity" $
    for_ [Nothing, Just "platform@example.com"] $ \contactText ->
        it ("uses the configured identity in both registry managers: " <> show contactText) $
            withRoleTelemetry $ \logEnv telemetry _ -> do
                contact <- either (fail . toString) pure (traverse parseOperatorContact contactText)
                let expected = renderUserAgent contact
                    environment = maybe [] (\value -> [("ECLUSE_EGRESS__OPERATOR_CONTACT", toString value)]) contactText <> staticEnvVars
                withRuntimeClients environment logEnv telemetry $ \clients ->
                    for_ clients $ \client ->
                        withStub status200 "ok" $ \stub -> do
                            request <- parseRequest (toString (stubBaseUrl stub))
                            fmap mconcat (ResponseWait.withResponse client request (brConsume . responseBody)) `shouldReturn` "ok"
                            captured <- lastCaptured stub
                            headerValue "User-Agent" captured `shouldBe` Just expected

advisoryAgeSpec :: Spec
advisoryAgeSpec = describe "withServiceRuntime advisory database ages" $
    for_ [ServeAndMirror, ServeOnly, MirrorOnly] $ \role ->
        it ("emits configured ecosystem ages for " <> show role) $
            withRoleTelemetry $ \logEnv telemetry meterEnv ->
                withServicePlan role staticEnvVars logEnv telemetry $ \boot plan mirror -> do
                    handles <- newAdvisoryHandles [Npm, PyPI]
                    -- The age gauge needs an installed artifact before observation.
                    for_ handles $ \(_, handle) -> swapIn (syncSlot (csEnv handle)) (DbEtag "installed") Nothing (fakeCveDb [])
                    withServiceRuntime boot plan mirror{mwCveSync = Map.fromList handles} $ \_ -> do
                        points <- advisoryAgePoints meterEnv
                        map fst points `shouldMatchList` map (metricAttributes . pure . LEcosystem) [Npm, PyPI]
                        map snd points `shouldSatisfy` all (>= 0)

-- The role's public and private data-plane clients, as 'withServiceRuntime' builds them.
withRuntimeClients :: [(String, String)] -> LogEnv -> Telemetry -> ([ExchangeClient] -> IO ()) -> IO ()
withRuntimeClients environment logEnv telemetry use =
    withServicePlan ServeAndMirror environment logEnv telemetry $ \boot plan mirror ->
        withServiceRuntime boot plan mirror $ \runtime ->
            use [envPublicClient (svcEnv runtime), envPrivateClient (svcEnv runtime)]

withServicePlan :: MirrorRole -> [(String, String)] -> LogEnv -> Telemetry -> (BootEnv -> ExecutablePlan -> MirrorWiring -> IO ()) -> IO ()
withServicePlan role environment logEnv telemetry use = withSweeper $ \deadlines -> do
    config <- expectConfig environment Nothing
    bootPlan <- expectPlanFor (BootMirrorPipeline role) environment Nothing config noCeiling
    (_, planned) <-
        planExecutable
            logEnv
            passthroughTracingPort
            deadlines
            mountBindingFor
            (\_ _ _ -> pure noMirrorQueue)
            initTargetCredentialProviders
            StoreBuilds
                { sbDeleting = \_ _ _ -> fakeMaintenance <$> newFakeStore defaultFakeStoreConfig
                , sbObserving = \_ _ _ -> fakeObservation <$> newFakeStore defaultFakeStoreConfig
                , sbProbing = \_ _ -> noUpstreamMechanism
                }
            bootPlan
    case planned of
        Right plan
            | MirrorPipelineWiring mirror <- epRoleWiring plan ->
                use (BootEnv config logEnv telemetry bootPlan deadlines) plan mirror
        _ -> expectationFailure "expected a mirror-pipeline role plan"
