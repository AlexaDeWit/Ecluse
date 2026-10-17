-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The proxy listener runs beside the role's worker and advisory-sync tasks.
"Ecluse.Service" supplies its mounts and probes.
-}
module Ecluse.Proxy (
    runProxy,
    runServer,
) where

import UnliftIO (concurrently_, race_)
import UnliftIO.Async (mapConcurrently_)

import Ecluse.Boot (applyServerSettings)
import Ecluse.Config (AppConfig (cfgServer))
import Ecluse.Proxy.Internal (warpExceptionHook)
import Ecluse.Runtime.Env (Env, envLogEnv)
import Ecluse.Runtime.Server (
    ServerConfig (scCheckLive, scCheckReady, scOnException),
    mkServerConfig,
 )
import Ecluse.Runtime.Server qualified as Server
import Ecluse.Service (ServiceRuntime (..), runWorker)

-- | Run the listener and cancel its background tasks when the HTTP drain ends.
runProxy :: ServiceRuntime -> IO ()
runProxy runtime =
    -- The background tasks never return, so the race cancels them at shutdown. A dropped job
    -- re-enqueues on the next demand and a cancelled sync resumes on next boot.
    case svcMirrorDrain runtime of
        Nothing -> race_ frontDoor (mapConcurrently_ id tasks)
        Just drain -> race_ frontDoor (concurrently_ drain (mapConcurrently_ id tasks))
  where
    -- The memory sampler always runs, so the list is never empty and the race never ends at once.
    tasks = svcMemorySampler runtime : svcSyncTasks runtime
    env = svcEnv runtime
    serverConfig = proxyServerConfig runtime
    frontDoor
        -- The worker loop never returns, so the server's graceful return must cancel it,
        -- never wait on it.
        | svcRunsWorker runtime =
            Server.raceServerAgainstLoop (runServer serverConfig env) (runWorker (svcWorkerPolicies runtime) env)
        | otherwise = runServer serverConfig env

proxyServerConfig :: ServiceRuntime -> ServerConfig
proxyServerConfig runtime =
    (applyServerSettings (cfgServer (svcAppConfig runtime)) (mkServerConfig (svcBindings runtime)))
        { scCheckReady = svcCheckReady runtime
        , scCheckLive = svcCheckLive runtime
        , scOnException = warpExceptionHook (envLogEnv (svcEnv runtime))
        }

-- | Run the proxy listener with the mounted adapters and shared process resources.
runServer :: ServerConfig -> Env -> IO ()
runServer cfg env = Server.runWarp (envLogEnv env) Server.proxyListener cfg (`Server.tracedApplication` env)
