-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The dedicated mirror-worker role, for a fleet scaled on queue depth apart from the front
door. 'runMirror' runs 'Ecluse.Service.runWorker', the same consume-loop entry the proxy role
embeds, over the same 'Ecluse.Service.ServiceRuntime', beside the advisory-sync tasks its
policy re-evaluation reads. It serves only the health probes, so an orchestrator can judge the
pod without the process holding a proxy's request surface.
-}
module Ecluse.Mirror (
    runMirror,
    mirrorServerConfig,
) where

import Katip (Severity (InfoS))
import UnliftIO (concurrently_)
import UnliftIO.Async (mapConcurrently_)

import Ecluse.Boot (probeServerConfig)
import Ecluse.Config (AppConfig)
import Ecluse.Core.Worker (Liveness)
import Ecluse.Runtime.Env (envLogEnv)
import Ecluse.Runtime.Log (moduleLog)
import Ecluse.Runtime.Server (
    ServerConfig (scCheckLive, scCheckReady, scPort),
    probeOnlyApplication,
    raceServerAgainstLoop,
    runWarp,
 )
import Ecluse.Service (ServiceRuntime (..), runWorker)

{- | Run the mirror worker alone. The consume loop and the sync tasks never return, so the
probe server's graceful return on shutdown must cancel them.
-}
runMirror :: ServiceRuntime -> IO ()
runMirror runtime = do
    let env = svcEnv runtime
        cfg = mirrorServerConfig (svcAppConfig runtime) (svcCheckReady runtime) (svcCheckLive runtime)
    moduleLog (envLogEnv env) "Ecluse.Mirror" InfoS ("Mirror worker starting up, health probes on port " <> show (scPort cfg))
    raceServerAgainstLoop
        (runWarp cfg probeOnlyApplication)
        (concurrently_ (runWorker (svcWorkerPolicies runtime) env) (mapConcurrently_ id (svcSyncTasks runtime)))

{- | The dedicated worker's health surface: no mount, the shared @server.port@, and the
consume-loop heartbeat behind @\/livez@ so a stalled worker fails its own liveness check.
-}
mirrorServerConfig :: AppConfig -> IO Bool -> IO Liveness -> ServerConfig
mirrorServerConfig appConfig checkReady checkLive =
    (probeServerConfig appConfig){scCheckReady = checkReady, scCheckLive = checkLive}
