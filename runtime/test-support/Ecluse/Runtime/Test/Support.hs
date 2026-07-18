-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared test-support library for Écluse's runtime-dependent suites.

This internal library hosts test environment helpers that depend on the runtime tier
(specifically 'Env' and its fields), which cannot live in the pure core 'ecluse-test-support'
due to the tier partition.
-}
module Ecluse.Runtime.Test.Support (
    newTestEnv,
    newTestEnvWith,
    newTestLogEnv,
) where

import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)

import Ecluse.Core.Queue (MirrorQueue)
import Ecluse.Core.Server.Cache (newMetadataCache)
import Ecluse.Runtime.Env (Env, newEnvWithAdmission, newWorkerHeartbeat)
import Ecluse.Runtime.Telemetry (Telemetry, telemetryDisabled)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Server.Cache (defaultCacheConfig)
import Ecluse.Test.Support (newTestLogEnv, testServeAdmission)

{- | Default test environment using standard memory queue, standard http manager,
and disabled telemetry.
-}
newTestEnv :: IO Env
newTestEnv = do
    queue <- newTestMemoryQueue
    manager <- newManager defaultManagerSettings
    newTestEnvWith queue (manager, manager) telemetryDisabled

{- | Parameterized test environment constructor. Default values are used for
the log environment, heartbeat, serve admission, and metadata cache.
-}
newTestEnvWith :: MirrorQueue -> (Manager, Manager) -> Telemetry -> IO Env
newTestEnvWith queue (manager, privateManager) telemetry = do
    metadataCache <- newMetadataCache defaultCacheConfig
    logEnv <- newTestLogEnv
    heartbeat <- newWorkerHeartbeat
    admission <- testServeAdmission
    newEnvWithAdmission admission queue manager privateManager metadataCache logEnv telemetry heartbeat
