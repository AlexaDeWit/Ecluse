-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The proxy a load scenario measures. It boots the way @ecluse proxy@ does, so the runtime
posture and memory plan come from its own cgroup and configuration. Two things differ from a pod:
the https upstreams the configuration names are dialled over plain HTTP on loopback, where the
stubs listen, and a loopback control listener reports RTS statistics for the measured window.
-}
module Ecluse.BenchLoad.ProxyServe (runServeProxy) where

import Data.Aeson (encode)
import Data.List (lookup)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Network.HTTP.Types (hContentType, status200, status404)
import Network.Wai (Application, pathInfo, queryString, responseLBS)
import Network.Wai.Handler.Warp qualified as Warp
import UnliftIO (race_)

import Ecluse.BenchLoad.RtsProbe (snapshotAfter)
import Ecluse.BenchLoad.RtsWindow (Collection (MajorCollection, MinorCollection), collectionName)
import Ecluse.Boot (BootEnv (beBootPlan, beLogEnv, beTelemetry), buildMirrorQueue, logBootWarning, orExit, refuseBoot, withBootEnv)
import Ecluse.Composition (ResolveAdapter)
import Ecluse.Composition.BootError (renderAdvisory, renderBootErrors)
import Ecluse.Composition.Credential (initTargetCredentialProviders)
import Ecluse.Composition.Executable (ExecutablePlan (epRoleWiring), RoleWiring (MirrorPipelineWiring), planExecutable)
import Ecluse.Composition.Maintenance (storeBuilds)
import Ecluse.Composition.Types (BootRole (BootMirrorPipeline), MirrorRole (ServeAndMirror))
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (artifactHosts))
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Context (PackumentDeps (..), pdMirror, pdPrivateBaseUrl, pdPublicBaseUrl)
import Ecluse.Core.Server.Upstream (mountUpstreams)
import Ecluse.Internal (ProcessOutcome (ShutdownRequested), exitCodeFor, exitReasonFor, superviseProcess)
import Ecluse.Proxy (runProxy)
import Ecluse.Runtime.Telemetry.Tracing (tracingPortOf)
import Ecluse.Service (mountBindingFor, withServiceRuntime)

{- | Boot and serve until SIGTERM, exiting as @ecluse@ would. It repeats the mirror-pipeline arm of
"Ecluse", whose entry point fixes the adapter resolver that the loopback rewrite replaces.
-}
runServeProxy :: IO ()
runServeProxy = do
    outcome <- superviseProcess (withBootEnv (BootMirrorPipeline ServeAndMirror) serveMeasured)
    traverse_ (TIO.hPutStrLn stderr) (exitReasonFor outcome)
    exitWith (exitCodeFor outcome)

serveMeasured :: BootEnv -> IO ProcessOutcome
serveMeasured bootEnv = do
    controlPort <- controlPortFromEnv
    clock <- clockFromEnv
    let logEnv = beLogEnv bootEnv
    (advisories, planned) <-
        planExecutable
            logEnv
            (tracingPortOf (beTelemetry bootEnv))
            (loopbackAdapter clock)
            buildMirrorQueue
            initTargetCredentialProviders
            storeBuilds
            (beBootPlan bootEnv)
    traverse_ (logBootWarning logEnv . renderAdvisory) advisories
    plan <- orExit renderBootErrors planned
    -- Keep this arm in step with startPlannedRole in src/Ecluse.hs.
    case epRoleWiring plan of
        MirrorPipelineWiring mirror ->
            ShutdownRequested <$ withServiceRuntime bootEnv plan mirror (\runtime -> race_ (runProxy runtime) (serveControl controlPort))
        _ -> refuseBoot "bench-load: the proxy role planned no mirror pipeline"

controlPortFromEnv :: IO Int
controlPortFromEnv = do
    raw <- lookupEnv "BENCH_PROXY_CONTROL_PORT"
    maybe (refuseBoot "bench-load: BENCH_PROXY_CONTROL_PORT must name the RTS control port") pure (readMaybe =<< raw)

clockFromEnv :: IO (Maybe UTCTime)
clockFromEnv =
    lookupEnv "BENCH_PROXY_NOW" >>= \case
        Nothing -> pure Nothing
        Just raw -> maybe (refuseBoot "bench-load: BENCH_PROXY_NOW must be an ISO8601 UTC time") (pure . Just) (iso8601ParseM raw)

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

serveControl :: Int -> IO ()
serveControl port = Warp.runSettings (Warp.setHost "127.0.0.1" (Warp.setPort port Warp.defaultSettings)) control

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
