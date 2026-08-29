-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The proxy role's front door over the shared assembly. 'runProxy' takes the
'ServiceRuntime' "Ecluse.Service" built, derives the 'ServerConfig' from it, and runs the
listener beside the role's background tasks.

Whether the mirror worker runs here is the role's decision, not this module's: under
@--no-worker@ the process still enqueues, and a separate "Ecluse.Mirror" fleet drains
the queue.
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

import Ecluse.Config (
    AppConfig (cfgServer),
    ServerSettings (srvPort, srvShutdownDrainTimeout),
 )
import Ecluse.Core.Text (displayExceptionT)
import Ecluse.Runtime.Env (Env, envLogEnv)
import Ecluse.Runtime.Server (
    ServerConfig (scCheckLive, scCheckReady, scDrainTimeout, scOnException, scPort),
    ShutdownDrainTimeout (ShutdownDrainTimeout),
    mkServerConfig,
 )
import Ecluse.Runtime.Server qualified as Server
import Ecluse.Service (ServiceRuntime (..), runWorker)

{- | Run the proxy role: the HTTP front door, the embedded mirror worker where the role keeps
one, the enqueue-buffer drain, and the advisory-sync tasks.
-}
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

{- The front door's config: the served mounts on the configured port, with the role's
readiness and liveness arms and warp's exception hook over the process logger. -}
proxyServerConfig :: ServiceRuntime -> ServerConfig
proxyServerConfig runtime =
    (mkServerConfig (svcBindings runtime))
        { scPort = srvPort serverSettings
        , scDrainTimeout = ShutdownDrainTimeout (srvShutdownDrainTimeout serverSettings)
        , scCheckReady = svcCheckReady runtime
        , scCheckLive = svcCheckLive runtime
        , scOnException = warpExceptionHook (envLogEnv (svcEnv runtime))
        }
  where
    serverSettings = cfgServer (svcAppConfig runtime)

{- | Run the proxy's HTTP front door over the composition-root 'Env' with the config-derived
'ServerConfig'. The bindings carry each adapter's serve surface, so the web layer stays neutral.
-}
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
