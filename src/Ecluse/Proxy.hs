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

import Katip (LogEnv, Severity (ErrorS), SimpleLogPayload, katipAddContext, logFM, runKatipContextT, sl)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import UnliftIO (concurrently_, race_)
import UnliftIO.Async (mapConcurrently_)

import Ecluse.Boot (applyServerSettings)
import Ecluse.Config (AppConfig (cfgServer))
import Ecluse.Core.Text (displayExceptionT)
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
    case (svcMirrorDrain runtime, svcSyncTasks runtime) of
        -- Racing the front door against an empty task list would cancel it instantly.
        (Nothing, []) -> frontDoor
        (Nothing, tasks) -> race_ frontDoor (mapConcurrently_ id tasks)
        (Just drain, tasks) -> race_ frontDoor (concurrently_ drain (mapConcurrently_ id tasks))
  where
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
runServer cfg env = Server.runWarp cfg (`Server.tracedApplication` env)

{- Warp's exception hook over the process logger. 'Warp.defaultShouldDisplayException'
filters routine client disconnects, so an aborted download does not spam the log. -}
warpExceptionHook :: LogEnv -> Maybe Wai.Request -> SomeException -> IO ()
warpExceptionHook logEnv mRequest err =
    when (Warp.defaultShouldDisplayException err) $
        runKatipContextT logEnv (mempty :: SimpleLogPayload) "server" $
            katipAddContext payload $
                logFM ErrorS "a fault escaped to the server (a post-commit teardown, or warp's own connection handling)"
  where
    payload =
        sl "path" (maybe ("unknown" :: Text) (decodeUtf8 . Wai.rawPathInfo) mRequest)
            <> sl "detail" (displayExceptionT err)
