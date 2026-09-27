-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The proxy a load scenario measures. It starts as @ecluse proxy@ does, through
'Ecluse.Startup.runWith', so the runtime posture and memory plan come from its own cgroup and
configuration. Two things differ from a pod: the https upstreams the configuration names are dialled
over plain HTTP on loopback, where the stubs listen, and a loopback control listener beside the
start-up reports RTS statistics for the measured window.
-}
module Ecluse.BenchLoad.ProxyServe (runServeProxy) where

import Data.Aeson (encode)
import Data.List (lookup)
import Data.Streaming.Network (bindPortTCP)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Network.HTTP.Types (hContentType, status200, status404)
import Network.Socket (Socket, close, setCloseOnExecIfNeeded, withFdSocket)
import Network.Wai (Application, pathInfo, queryString, responseLBS)
import Network.Wai.Handler.Warp qualified as Warp
import UnliftIO (bracket, tryIO)
import UnliftIO.Async (link, withAsync)

import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.RtsProbe (requireRtsStats, snapshotAfter)
import Ecluse.BenchLoad.RtsWindow (Collection (MajorCollection, MinorCollection), collectionName)
import Ecluse.CLI (AppCommand (RunService))
import Ecluse.Composition (ResolveAdapter)
import Ecluse.Composition.Types (MirrorRole (ServeAndMirror))
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (artifactHosts))
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Context (PackumentDeps (..), pdMirror, pdPrivateBaseUrl, pdPublicBaseUrl)
import Ecluse.Core.Server.Upstream (mountUpstreams)
import Ecluse.Core.Text (displayExceptionT)
import Ecluse.Service (mountBindingFor)
import Ecluse.Startup (runWith)

{- | Boot and serve until SIGTERM, exiting as @ecluse proxy@ would. The start-up keeps the main
thread, and a control listener failure reaches it through 'link'.
-}
runServeProxy :: IO ()
runServeProxy = do
    requireRtsStats "the proxy"
    controlPort <- controlPortFromEnv
    clock <- clockFromEnv
    bracket (bindControl controlPort) close $ \listening -> do
        -- The start-up may re-exec this binary, so the socket must close on exec.
        withFdSocket listening setCloseOnExecIfNeeded
        withAsync (Warp.runSettingsSocket Warp.defaultSettings listening control) $ \listener -> do
            link listener
            runWith (loopbackAdapter clock) (RunService ServeAndMirror)

controlPortFromEnv :: IO Int
controlPortFromEnv = do
    raw <- lookupEnv "BENCH_PROXY_CONTROL_PORT"
    maybe (benchFail "bench-load: BENCH_PROXY_CONTROL_PORT must name the RTS control port") pure (readMaybe =<< raw)

bindControl :: Int -> IO Socket
bindControl port = tryIO (bindPortTCP port "127.0.0.1") >>= either refuse pure
  where
    refuse err = benchFail ("bench-load: BENCH_PROXY_CONTROL_PORT " <> show port <> " cannot be bound: " <> displayExceptionT err)

clockFromEnv :: IO (Maybe UTCTime)
clockFromEnv =
    lookupEnv "BENCH_PROXY_NOW" >>= \case
        Nothing -> pure Nothing
        Just raw -> maybe (benchFail "bench-load: BENCH_PROXY_NOW must be an ISO8601 UTC time") (pure . Just) (iso8601ParseM raw)

-- The production resolver over deps whose upstreams answer on loopback without TLS.
loopbackAdapter :: Maybe UTCTime -> ResolveAdapter
loopbackAdapter clock ecosystem deps = mountBindingFor ecosystem (overLoopback clock deps)

overLoopback :: Maybe UTCTime -> PackumentDeps -> PackumentDeps
overLoopback clock deps =
    deps
        { pdUpstreams =
            mountUpstreams
                (artifactHosts (pdArtifact deps))
                (plainHttp <$> pdPrivateBaseUrl deps)
                (plainHttp (pdPublicBaseUrl deps))
                (pdMirror deps)
        , pdNow = maybe (pdNow deps) pure clock
        }

plainHttp :: RegistryUrl -> RegistryUrl
plainHttp url = loopbackRegistryUrl (maybe text ("http://" <>) (T.stripPrefix "https://" text))
  where
    text = registryUrlText url

-- @GET /rts?gc=major@ reports the counters after a major collection, and any other query after a minor one.
control :: Application
control request respond = case pathInfo request of
    ["rts"] -> do
        snapshot <-
            snapshotAfter $
                if lookup "gc" (queryString request) == Just (Just (encodeUtf8 (collectionName MajorCollection)))
                    then MajorCollection
                    else MinorCollection
        respond (responseLBS status200 [(hContentType, "application/json")] (encode snapshot))
    _ -> respond (responseLBS status404 [] "")
