-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared loopback proxy wiring for ecosystem load fixtures.
HTTP preflights reject a wrong response before the measured window starts.
-}
module Ecluse.BenchLoad.Fixture (
    withProxyOverStubs,
    longCacheTtl,
    defaultCacheEntries,
    artifactBytes,
    loadCorpusBodies,
    selfHosted,
    primeETag,
    fetchChecked,
    benchNow,
) where

import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime, UTCTime (UTCTime), fromGregorian)
import GHC.Conc (getNumCapabilities)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (Header, Status, status200, status304)
import Network.HTTP.Types.Header (hETag, hIfNoneMatch)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (testWithApplication)

import Ecluse (mountBindingFor)
import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Harness (LoadKnobs (..))
import Ecluse.Composition.Sizing (connectionPoolSettings, openFileSoftLimit, resolvePrivateConnections, resolvePublicConnections, resolveServeAdmission)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Queue.Memory (defaultMemoryQueueConfig, newBoundedInMemoryQueue)
import Ecluse.Core.Server.Admission (newServeAdmission)
import Ecluse.Core.Server.Cache (CacheConfig (..), StoreBudget (..), newMetadataCache)
import Ecluse.Core.Server.Context (PackumentDeps)
import Ecluse.Core.Worker (newWorkerHeartbeat)
import Ecluse.Runtime.Env (newEnvWithAdmission)
import Ecluse.Runtime.Server (application, mkServerConfig)
import Ecluse.Runtime.Telemetry (telemetryDisabled)
import Ecluse.Test.Corpus (CorpusPackage (cpPath), cpName)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Server.Cache (defaultCacheConfig)
import Ecluse.Test.Wai (rebaseAuthority)

-- | Boot a composed proxy with production pool sizing over the supplied ecosystem stubs.
withProxyOverStubs :: Ecosystem -> (Int -> Int -> IO PackumentDeps) -> LoadKnobs -> NominalDiffTime -> Int -> Application -> Application -> (Int -> [Text]) -> ([Text] -> IO a) -> IO a
withProxyOverStubs ecosystem depsFor knobs ttl maxEntries privateApp publicApp mkMix body = do
    capabilities <- getNumCapabilities
    fdLimit <- openFileSoftLimit
    let admissionCapacity = fst (resolveServeAdmission (lkServeMaxInFlight knobs) capabilities)
        privateConnections = fst (resolvePrivateConnections (lkPrivateConnectionsPerHost knobs) fdLimit)
        publicConnections = fst (resolvePublicConnections (lkPublicConnectionsPerHost knobs) fdLimit)
    testWithApplication (pure privateApp) $ \privatePort ->
        testWithApplication (pure publicApp) $ \publicPort -> do
            publicManager <- newManager (connectionPoolSettings publicConnections defaultManagerSettings)
            privateManager <- newManager (connectionPoolSettings privateConnections defaultManagerSettings)
            admission <- newServeAdmission admissionCapacity
            cache <- newMetadataCache (benchCacheConfig ttl (max 1 maxEntries))
            logEnv <- newTestLogEnv
            heartbeat <- newWorkerHeartbeat
            -- No worker drains this production-sized queue. At capacity, it sheds new jobs.
            queue <-
                newBoundedInMemoryQueue
                    (defaultMemoryQueueConfig 50_000)
                    (\n -> putTextLn ("bench serve stack: bounded in-memory mirror queue at cap. Running dropped-job total: " <> show n))
            env <- newEnvWithAdmission admission queue publicManager privateManager cache logEnv telemetryDisabled heartbeat
            deps <- depsFor privatePort publicPort
            let cfg = mkServerConfig (maybeToList (mountBindingFor ecosystem deps Nothing))
            testWithApplication (pure (application cfg env)) $ \proxyPort ->
                body (mkMix proxyPort)

-- | Keep entries alive throughout warm-up and measurement, leaving eviction as the tested axis.
longCacheTtl :: NominalDiffTime
longCacheTtl = 3600

-- | The production metadata cache's full-store entry bound.
defaultCacheEntries :: Int
defaultCacheEntries = sbMaxEntries (cacheFullBudget defaultCacheConfig)

benchCacheConfig :: NominalDiffTime -> Int -> CacheConfig
benchCacheConfig ttl maxEntries =
    defaultCacheConfig
        { cacheTtl = ttl
        , cacheFullBudget = capEntries (cacheFullBudget defaultCacheConfig)
        , cacheVersionBudget = capEntries (cacheVersionBudget defaultCacheConfig)
        , cacheAssembledBudget = capEntries (cacheAssembledBudget defaultCacheConfig)
        }
  where
    capEntries budget = budget{sbMaxEntries = maxEntries}

-- | A payload-sized body shared by artifact relays and integrity verification.
artifactBytes :: Int -> LByteString
artifactBytes size = LBS.replicate (fromIntegral (max 1 size)) 0x61

-- | Read the selected corpus, refusing empty captures before starting load.
loadCorpusBodies :: [CorpusPackage] -> IO (Map Text LByteString)
loadCorpusBodies packages = Map.fromList <$> traverse load packages
  where
    load cp = do
        bytes <- readFileLBS (cpPath cp)
        when (LBS.null bytes) (benchFail ("bench-load: corpus capture is empty: " <> toText (cpPath cp)))
        pure (cpName cp, bytes)

-- | Rebase captured artifact URLs once per stub, outside repeated metadata responses.
selfHosted :: Text -> IORef (Map Text LByteString) -> Text -> Map Text LByteString -> IO (Map Text LByteString)
selfHosted capturedAuthority rewritten authority bodies =
    readIORef rewritten >>= \case
        cached | not (Map.null cached) -> pure cached
        _ -> do
            let served = Map.map (rebaseAuthority capturedAuthority authority) bodies
            writeIORef rewritten served
            pure served

-- | Require a successful priming GET followed by an empty 304 for its served validator.
primeETag :: Text -> IO Text
primeETag url = do
    response <- fetchChecked status200 [] url
    tag <- maybe (benchFail "revalidate-not-modified: the priming GET returned no ETag") pure (List.lookup hETag (HTTP.responseHeaders response))
    conditional <- fetchChecked status304 [(hIfNoneMatch, tag)] url
    unless (LBS.null (HTTP.responseBody conditional)) (benchFail "revalidate-not-modified: the 304 response carried a body")
    pure (decodeUtf8 tag)

-- | Fetch without redirects and fail if the fixture serves an unexpected status.
fetchChecked :: Status -> [Header] -> Text -> IO (HTTP.Response LByteString)
fetchChecked expected headers url = do
    manager <- newManager defaultManagerSettings
    request <- HTTP.parseRequest (toString url)
    response <- HTTP.httpLbs request{HTTP.requestHeaders = headers, HTTP.redirectCount = 0, HTTP.checkResponse = \_ _ -> pass} manager
    unless (HTTP.responseStatus response == expected) $
        benchFail ("bench-load preflight " <> url <> ": expected " <> show expected <> ", got " <> show (HTTP.responseStatus response))
    pure response

-- | A fixed clock keeps age admission independent of the day the benchmark runs.
benchNow :: UTCTime
benchNow = UTCTime (fromGregorian 2026 6 1) 0
