-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Mirror-pipeline role composition, worker liveness, and ecosystem mount bindings.
module Ecluse.ServiceSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Time (addUTCTime, getCurrentTime)
import Test.Hspec

import Ecluse.Boot (BootEnv (BootEnv))
import Ecluse.Composition.Credential (noCredentialProviders)
import Ecluse.Composition.Executable (ExecutablePlan (epRoleWiring), MirrorWiring (mwCveSync), RoleWiring (MirrorPipelineWiring), planExecutable)
import Ecluse.Composition.Maintenance (StoreBuilds (StoreBuilds, sbDeleting, sbObserving))
import Ecluse.Composition.Support (expectConfig, expectPlanFor, noCeiling, staticEnvVars)
import Ecluse.Composition.TelemetrySupport (advisoryAgePoints, newAdvisoryHandles, withRoleTelemetry)
import Ecluse.Composition.Types (BootRole (BootMirrorPipeline), MirrorRole (MirrorOnly, ServeAndMirror, ServeOnly))
import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Queue (noMirrorQueue)
import Ecluse.Core.Telemetry.Metrics (Label (LEcosystem), metricAttributes)
import Ecluse.Core.Worker (
    Liveness (liveHealthy, liveLastPoll),
    WorkerHeartbeat,
    newWorkerHeartbeat,
    recordPoll,
    workerHeartbeatStaleAfter,
 )
import Ecluse.Core.Worker.Liveness (newWorkerHeartbeatWithClock)
import Ecluse.Runtime.Server (MountBinding (bindingPrefix))
import Ecluse.Service (mountBindingFor, withServiceRuntime, workerLiveness)
import Ecluse.Test.Maintenance (FakeStore (fakeMaintenance, fakeObservation), defaultFakeStoreConfig, newFakeStore)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Server.Mount (inertPackumentDeps)
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
    describe "workerLiveness -- what /livez answers once the spawn decision is derived" $ do
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

    describe "mountBindingFor -- ecosystem drives the binding" $ do
        it "resolves npm to a binding whose prefix is derived from the ecosystem (/npm)" $
            (bindingPrefix <$> mountBindingFor Npm inertPackumentDeps Nothing) `shouldBe` Just ("npm" :| [])

        it "resolves PyPI to a binding under its own derived prefix (/pypi)" $
            (bindingPrefix <$> mountBindingFor PyPI inertPackumentDeps Nothing) `shouldBe` Just ("pypi" :| [])

        it "has no binding for an ecosystem with no adapter wired (loud Nothing, not a stub)" $
            (bindingPrefix <$> mountBindingFor RubyGems inertPackumentDeps Nothing) `shouldBe` Nothing

advisoryAgeSpec :: Spec
advisoryAgeSpec = describe "withServiceRuntime advisory database ages" $
    for_ [ServeAndMirror, ServeOnly, MirrorOnly] $ \role ->
        it ("emits configured ecosystem ages for " <> show role) $
            withRoleTelemetry $ \logEnv telemetry meterEnv -> do
                config <- expectConfig staticEnvVars Nothing
                bootPlan <- expectPlanFor (BootMirrorPipeline role) staticEnvVars Nothing config noCeiling
                planned <-
                    planExecutable
                        logEnv
                        passthroughTracingPort
                        mountBindingFor
                        (\_ _ _ -> pure noMirrorQueue)
                        (\_ _ -> pure (Right noCredentialProviders))
                        StoreBuilds
                            { sbDeleting = \_ _ _ -> fakeMaintenance <$> newFakeStore defaultFakeStoreConfig
                            , sbObserving = \_ _ _ -> fakeObservation <$> newFakeStore defaultFakeStoreConfig
                            }
                        bootPlan
                case planned of
                    Right plan | MirrorPipelineWiring mirror <- epRoleWiring plan -> do
                        handles <- newAdvisoryHandles [Npm, PyPI]
                        let boot = BootEnv config logEnv telemetry bootPlan
                        withServiceRuntime boot plan mirror{mwCveSync = Map.fromList handles} $ \_ -> do
                            points <- advisoryAgePoints meterEnv
                            map fst points `shouldMatchList` map (metricAttributes . pure . LEcosystem) [Npm, PyPI]
                            map snd points `shouldSatisfy` all (>= 0)
                    _ -> expectationFailure "expected a mirror-pipeline role plan"
