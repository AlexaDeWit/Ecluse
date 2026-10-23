-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared test-support library for Écluse's runtime-dependent suites.

This internal library holds the test environment helpers that depend on the runtime
tier (specifically 'Env' and its fields). The tier partition keeps them out of the
pure core 'ecluse-test-support'.
-}
module Ecluse.Runtime.Test.Support (
    newTestEnv,
    newTestEnvWith,
    newTestEnvLogging,
) where

import Katip (LogEnv)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)

import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Queue (MirrorQueue)
import Ecluse.Runtime.Env (Env, newEnvWithAdmission, newWorkerHeartbeat)
import Ecluse.Runtime.Telemetry (Telemetry, telemetryDisabled)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Server.Cache (defaultCacheConfig, newMetadataCache)
import Ecluse.Test.Support (testMemoryMeter, testServeAdmission)

{- | The default test environment over deadlines the caller sweeps through
'Ecluse.Test.Deadline.withSweeper': the memory queue, a standard HTTP manager, and telemetry disabled.
-}
newTestEnv :: Deadlines -> IO Env
newTestEnv deadlines = do
    queue <- newTestMemoryQueue
    manager <- newManager defaultManagerSettings
    newTestEnvWith deadlines queue (manager, manager) telemetryDisabled

-- | 'newTestEnv' over the given queue, managers, and telemetry handle. Everything else takes its default.
newTestEnvWith :: Deadlines -> MirrorQueue -> (Manager, Manager) -> Telemetry -> IO Env
newTestEnvWith deadlines queue managers telemetry = do
    logEnv <- newTestLogEnv
    newTestEnvLogging deadlines logEnv queue managers telemetry

-- | 'newTestEnvWith' over a caller-supplied log environment, so a test can capture what the serve path logged.
newTestEnvLogging :: Deadlines -> LogEnv -> MirrorQueue -> (Manager, Manager) -> Telemetry -> IO Env
newTestEnvLogging deadlines logEnv queue (manager, privateManager) telemetry = do
    metadataCache <- newMetadataCache defaultCacheConfig
    heartbeat <- newWorkerHeartbeat
    admission <- testServeAdmission
    meter <- testMemoryMeter
    newEnvWithAdmission admission meter queue manager privateManager deadlines metadataCache logEnv telemetry heartbeat
