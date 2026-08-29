-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.ServiceSpec (spec) where

import Data.Time (addUTCTime, getCurrentTime)
import Test.Hspec

import Ecluse.Boot (BootAborted (BootAborted), BootEnv (..))
import Ecluse.Composition.MirrorRole (MirrorRole (ServeOnly))
import Ecluse.Composition.Plan (resolveBootPlan)
import Ecluse.Composition.Support (expectConfig, fdLimit, noCeiling, staticEnvVars, withoutQueueUrl)
import Ecluse.Config (Config)
import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Worker (
    Liveness (liveHealthy, liveLastPoll),
    WorkerHeartbeat,
    newWorkerHeartbeat,
    recordPoll,
    workerHeartbeatStaleAfter,
 )
import Ecluse.Runtime.Server (MountBinding (bindingPrefix))
import Ecluse.Runtime.Telemetry (telemetryDisabled)
import Ecluse.Service (mountBindingFor, withServiceRuntime, workerLiveness)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Server.Mount (inertPackumentDeps)

{- | A heartbeat whose last poll is older than 'workerHeartbeatStaleAfter': a consume loop
that stopped advancing, which is what the worker arm of @\/livez@ exists to catch.
-}
stalledHeartbeat :: IO WorkerHeartbeat
stalledHeartbeat = do
    heartbeat <- newWorkerHeartbeat
    now <- getCurrentTime
    recordPoll heartbeat (addUTCTime (negate (workerHeartbeatStaleAfter + 60)) now)
    pure heartbeat

{- | A 'BootEnv' over a resolved config, as @withBootEnv@ would hand one to a role. The runtime
posture is the pinned fixture rather than this machine's, so nothing here re-execs the binary.
-}
testBootEnv :: [(String, String)] -> Config -> IO BootEnv
testBootEnv envVars config = do
    logEnv <- newTestLogEnv
    bootPlan <-
        either
            (\errs -> fail ("boot plan refused: " <> show errs))
            pure
            (snd (resolveBootPlan envVars Nothing config noCeiling fdLimit))
    pure
        BootEnv
            { beConfig = config
            , beS3Endpoint = Nothing
            , beLogEnv = logEnv
            , beTelemetry = telemetryDisabled
            , beBootPlan = bootPlan
            }

spec :: Spec
spec = do
    describe "withServiceRuntime -- the role refusal is applied before any wiring" $
        it "refuses --no-worker over the in-memory queue rather than assembling the role" $ do
            -- The config is otherwise complete, so nothing downstream would refuse: dropping the
            -- role guard would let this boot the whole runtime and return normally.
            let envVars = withoutQueueUrl staticEnvVars
            config <- expectConfig envVars Nothing
            bootEnv <- testBootEnv envVars config
            withServiceRuntime ServeOnly bootEnv (const pass) `shouldThrow` (== BootAborted)

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

    describe "mountBindingFor -- ecosystem drives the binding" $ do
        it "resolves npm to a binding whose prefix is derived from the ecosystem (/npm)" $
            (bindingPrefix <$> mountBindingFor Npm inertPackumentDeps Nothing) `shouldBe` Just ("npm" :| [])

        it "has no binding for an ecosystem with no adapter wired (loud Nothing, not a stub)" $ do
            (bindingPrefix <$> mountBindingFor PyPI inertPackumentDeps Nothing) `shouldBe` Nothing
            (bindingPrefix <$> mountBindingFor RubyGems inertPackumentDeps Nothing) `shouldBe` Nothing
