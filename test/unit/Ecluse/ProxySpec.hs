-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Production proxy composition through startup advisory failure and recovery.
module Ecluse.ProxySpec (spec) where

import Data.Map.Strict qualified as Map
import Katip (LogEnv)
import Network.HTTP.Client (HttpException, Manager, Response, defaultManagerSettings, httpNoBody, newManager, parseRequest, responseStatus)
import Network.HTTP.Types (statusCode)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import UnliftIO.Async (link, poll, withAsync)
import UnliftIO.Exception (try)

import Ecluse.Boot (BootEnv (BootEnv))
import Ecluse.Composition.Credential (initTargetCredentialProviders)
import Ecluse.Composition.Executable (ExecutablePlan (epRoleWiring), MirrorWiring (mwCveSync), RoleWiring (MirrorPipelineWiring), planExecutable)
import Ecluse.Composition.Maintenance (StoreBuilds (StoreBuilds))
import Ecluse.Composition.Support (expectConfig, expectPlanFor, noCeiling, publicGateEnv)
import Ecluse.Composition.TelemetrySupport (newAdvisoryHandles)
import Ecluse.Composition.Types (BootRole (BootMirrorPipeline), MirrorRole (ServeOnly))
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Fault (TransportCause (TransportUnreachable), transportFault)
import Ecluse.Core.Queue (noMirrorQueue)
import Ecluse.Core.Registry.Maintenance.Upstream (noUpstreamMechanism)
import Ecluse.Cve.Sync (CveSyncHandle (csClock, csEnv, csReady), cveSyncTasks)
import Ecluse.Proxy (runProxy)
import Ecluse.Runtime.Cve.Sync.Internal (CveFetch (..), OsvDbFetchFault (OsvDbTransport), SyncEnv (..), SyncSchedule (SyncSchedule))
import Ecluse.Runtime.Env (envMetrics)
import Ecluse.Runtime.Telemetry (telemetryDisabled)
import Ecluse.Runtime.Test.Cve (fetchServingAt, headOnlyFetch)
import Ecluse.Service (ServiceRuntime (svcEnv, svcSyncTasks), mountBindingFor, withServiceRuntime)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Log (captureJsonLog)
import Ecluse.Test.Osv (mkDbWithWrongEpoch, mkMinimalValidDbWithMeta)
import Ecluse.Test.Poll (awaitUntil, pollUntil)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Wai (freePort)

spec :: Spec
spec = describe "runProxy advisory startup lifecycle" $
    for_ ["unavailable S3", "missing object", "rejected artifact"] $ \failure ->
        it ("keeps the listener alive through " <> failure <> " and recovers without restart") $
            withSystemTempDirectory "proxy-startup" $ \dir -> do
                port <- freePort
                manager <- newManager defaultManagerSettings
                handles <- Map.fromList <$> newAdvisoryHandles [Npm]
                handle <- maybe (fail "missing npm handle") pure (Map.lookup Npm handles)
                atomically (writeTVar (csReady handle) False)
                pushedAt <- csClock handle
                recovering <- newTVarIO False
                attempts <- newTVarIO (0 :: Int)
                let initial = case failure of
                        "unavailable S3" -> headOnlyFetch (Left (OsvDbTransport (transportFault TransportUnreachable "S3 unavailable")))
                        "missing object" -> headOnlyFetch (Right Nothing)
                        _ -> fetchServingAt (Just pushedAt) (Just "rejected") mkDbWithWrongEpoch
                    recovered = fetchServingAt (Just pushedAt) (Just "recovered") (\dest -> mkMinimalValidDbWithMeta dest "leftpad" [])
                    choose = readTVarIO recovering <&> \ready -> if ready then recovered else initial
                    fetch =
                        CveFetch
                            { fetchHead = atomically (modifyTVar' attempts (+ 1)) >> (choose >>= fetchHead)
                            , fetchDownload = \dest -> choose >>= (`fetchDownload` dest)
                            }
                    plan = Map.map (\h -> h{csEnv = (csEnv h){syncFetch = fetch, syncDbPath = dir </> "npm.db"}}) handles
                void $ captureJsonLog $ \logEnv ->
                    withProxyRuntime logEnv port plan $ \runtime -> do
                        let tasks = cveSyncTasks logEnv (envMetrics (svcEnv runtime)) telemetryDisabled (SyncSchedule [1_000, 1_000] 20_000 900_000_000) plan
                        withAsync (runProxy runtime{svcSyncTasks = tasks}) $ \proxy -> do
                            link proxy
                            pollUntil 200 25_000 (== Just 200) (proxyStatus manager port "/livez") `shouldReturn` Just 200
                            awaitUntil 5_000_000 1_000 ((>= 4) <$> readTVarIO attempts) `shouldReturn` True
                            proxyStatus manager port "/readyz" `shouldReturn` Just 503
                            proxyStatus manager port "/npm/-/ping" `shouldReturn` Just 200
                            poll proxy >>= (`shouldSatisfy` isNothing)
                            atomically (writeTVar recovering True)
                            pollUntil 200 25_000 (== Just 200) (proxyStatus manager port "/readyz") `shouldReturn` Just 200
                            proxyStatus manager port "/livez" `shouldReturn` Just 200
                            proxyStatus manager port "/npm/-/ping" `shouldReturn` Just 200
                            poll proxy >>= (`shouldSatisfy` isNothing)

withProxyRuntime :: LogEnv -> Int -> Map.Map Ecosystem CveSyncHandle -> (ServiceRuntime -> IO ()) -> IO ()
withProxyRuntime logEnv port handles use = do
    let vars = ("ECLUSE_SERVER__PORT", show port) : filter ((/= "ECLUSE_SERVER__PORT") . fst) publicGateEnv
    config <- expectConfig vars Nothing
    bootPlan <- expectPlanFor (BootMirrorPipeline ServeOnly) vars Nothing config noCeiling
    withSweeper $ \deadlines -> do
        (_, outcome) <- planExecutable logEnv passthroughTracingPort deadlines mountBindingFor (\_ _ _ -> pure noMirrorQueue) initTargetCredentialProviders unusedStores bootPlan
        case outcome of
            Right plan
                | MirrorPipelineWiring mirror <- epRoleWiring plan ->
                    withServiceRuntime (BootEnv config logEnv telemetryDisabled bootPlan deadlines) plan mirror{mwCveSync = handles} use
            other -> expectationFailure ("expected the serve-only plan, got " <> either show (const "another role") other)
  where
    unusedStores = StoreBuilds (\_ _ _ -> fail "serve-only must not build a deleting store") (\_ _ _ -> fail "serve-only must not build an observing store") (\_ _ -> noUpstreamMechanism)

proxyStatus :: Manager -> Int -> Text -> IO (Maybe Int)
proxyStatus manager port path = do
    request <- parseRequest (toString ("http://127.0.0.1:" <> show port <> path))
    response <- try (httpNoBody request manager) :: IO (Either HttpException (Response ()))
    pure (either (const Nothing) (Just . statusCode . responseStatus) response)
