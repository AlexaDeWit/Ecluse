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
) where

import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)

import Ecluse.Core.Queue (MirrorQueue)
import Ecluse.Core.Server.Cache (newMetadataCache)
import Ecluse.Runtime.Env (Env, newEnvWithAdmission, newWorkerHeartbeat)
import Ecluse.Runtime.Telemetry (Telemetry, telemetryDisabled)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Server.Cache (defaultCacheConfig)
import Ecluse.Test.Support (testServeAdmission)

{- | The default test environment: the memory queue, a standard HTTP manager, and
telemetry disabled.
-}
newTestEnv :: IO Env
newTestEnv = do
    queue <- newTestMemoryQueue
    manager <- newManager defaultManagerSettings
    newTestEnvWith queue (manager, manager) telemetryDisabled

{- | Build a test environment over the given queue, managers, and telemetry handle. Everything
else takes its default.
-}
newTestEnvWith :: MirrorQueue -> (Manager, Manager) -> Telemetry -> IO Env
newTestEnvWith queue (manager, privateManager) telemetry = do
    metadataCache <- newMetadataCache defaultCacheConfig
    logEnv <- newTestLogEnv
    heartbeat <- newWorkerHeartbeat
    admission <- testServeAdmission
    newEnvWithAdmission admission queue manager privateManager metadataCache logEnv telemetry heartbeat
