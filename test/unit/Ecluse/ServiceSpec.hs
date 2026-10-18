-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Mirror-pipeline role composition, worker liveness, and ecosystem mount bindings.
module Ecluse.ServiceSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Time (addUTCTime, getCurrentTime)
import Katip (LogEnv)
import Network.HTTP.Client (HttpException (HttpExceptionRequest), HttpExceptionContent (InternalException), brConsume, newManager, parseRequest, responseBody, withResponse)
import Test.Hspec
import UnliftIO.Exception (try)

import Ecluse.Boot (BootEnv (BootEnv))
import Ecluse.Composition.Credential (initTargetCredentialProviders)
import Ecluse.Composition.Executable (ExecutablePlan (epRoleWiring), MirrorWiring (mwCveSync), RoleWiring (MirrorPipelineWiring), planExecutable)
import Ecluse.Composition.Maintenance (StoreBuilds (StoreBuilds, sbDeleting, sbObserving, sbProbing))
import Ecluse.Composition.Support (expectConfig, expectPlanFor, noCeiling, staticEnvVars)
import Ecluse.Composition.TelemetrySupport (advisoryAgePoints, newAdvisoryHandles, withRoleTelemetry)
import Ecluse.Composition.Types (BootRole (BootMirrorPipeline), MirrorRole (MirrorOnly, ServeAndMirror, ServeOnly))
import Ecluse.Core.BuildIdentity (parseOperatorContact, renderUserAgent)
import Ecluse.Core.Cve.Slot (swapIn)
import Ecluse.Core.Cve.Types (DbEtag (DbEtag))
import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Queue (noMirrorQueue)
import Ecluse.Core.Registry.Maintenance.Upstream (noUpstreamMechanism)
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
import Ecluse.Runtime.Env (envManager, envPrivateManager)
import Ecluse.Runtime.Server (MountBinding (bindingPrefix))
import Ecluse.Runtime.Telemetry (Telemetry, telemetryDisabled)
import Ecluse.Service (ServiceRuntime (svcEnv), dataPlaneManagerSettings, mountBindingFor, withServiceRuntime, workerLiveness)
import Ecluse.Test.Cve (fakeCveDb)
import Ecluse.Test.Maintenance (FakeStore (fakeMaintenance, fakeObservation), defaultFakeStoreConfig, newFakeStore)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Server.Mount (inertPackumentDeps)
import Ecluse.Test.Stub (headerValue, lastCaptured, stubBaseUrl, withStub)
import Ecluse.Test.Stub.Socket (resetAfterPartialBodyOverTcp, withRawServer)
import Ecluse.Test.Support (newTestClock)
import Network.HTTP.Types.Status (status200)

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

        it "has no binding for an ecosystem with no adapter wired (loud Nothing, not a stub)" $
            (bindingPrefix <$> mountBindingFor RubyGems inertPackumentDeps Nothing) `shouldBe` Nothing

dataPlaneSpec :: Spec
dataPlaneSpec = describe "dataPlaneManagerSettings" $ do
    it "types a body read the upstream resets after its 200, with telemetry off" $
        typesBodyResetUnder telemetryDisabled

    it "types a body read the upstream resets after its 200, under the client span" $
        withRoleTelemetry (\_ telemetry _ -> typesBodyResetUnder telemetry)

-- A body read through a data-plane manager that the upstream resets raises the client's own exception, which the relay and the exchanges catch.
typesBodyResetUnder :: Telemetry -> IO ()
typesBodyResetUnder telemetry = withRawServer resetAfterPartialBodyOverTcp $ \port -> do
    manager <- newManager =<< dataPlaneManagerSettings telemetry
    request <- parseRequest ("http://127.0.0.1:" <> show port <> "/")
    try (withResponse request manager (brConsume . responseBody)) >>= \case
        Left (HttpExceptionRequest _ (InternalException _)) -> pass
        Left other -> expectationFailure ("expected a typed body-read reset, got " <> show other)
        Right _ -> expectationFailure "a reset body was read whole"

outboundIdentitySpec :: Spec
outboundIdentitySpec = describe "the runtime's outbound registry identity" $
    for_ [Nothing, Just "platform@example.com"] $ \contactText ->
        it ("uses the configured identity in both registry managers: " <> show contactText) $
            withRoleTelemetry $ \logEnv telemetry _ -> do
                contact <- either (fail . toString) pure (traverse parseOperatorContact contactText)
                let expected = renderUserAgent contact
                    environment = maybe [] (\value -> [("ECLUSE_EGRESS__OPERATOR_CONTACT", toString value)]) contactText <> staticEnvVars
                withServicePlan ServeAndMirror environment logEnv telemetry $ \boot plan mirror ->
                    withServiceRuntime boot plan mirror $ \runtime ->
                        for_ [envManager (svcEnv runtime), envPrivateManager (svcEnv runtime)] $ \manager ->
                            withStub status200 "ok" $ \stub -> do
                                request <- parseRequest (toString (stubBaseUrl stub))
                                fmap mconcat (withResponse request manager (brConsume . responseBody)) `shouldReturn` "ok"
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

withServicePlan :: MirrorRole -> [(String, String)] -> LogEnv -> Telemetry -> (BootEnv -> ExecutablePlan -> MirrorWiring -> IO ()) -> IO ()
withServicePlan role environment logEnv telemetry use = do
    config <- expectConfig environment Nothing
    bootPlan <- expectPlanFor (BootMirrorPipeline role) environment Nothing config noCeiling
    (_, planned) <-
        planExecutable
            logEnv
            passthroughTracingPort
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
                use (BootEnv config logEnv telemetry bootPlan) plan mirror
        _ -> expectationFailure "expected a mirror-pipeline role plan"
