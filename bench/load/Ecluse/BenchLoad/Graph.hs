-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Real installer experiment commands for capture, proxy service, and interval analysis.
The Bash driver owns clients and process limits. Runtime figures remain informational.
-}
module Ecluse.BenchLoad.Graph (runGraph) where

import Control.Concurrent (threadDelay)
import Data.Aeson (eitherDecode, encode, object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Format.ISO8601 (iso8601ParseM)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getNumCapabilities)
import GHC.Stats (GCDetails (gcdetails_live_bytes), RTSStats (..), getRTSStats, getRTSStatsEnabled)
import Network.HTTP.Types (statusCode)
import Network.Wai (Middleware, pathInfo, rawPathInfo, requestHeaders, requestMethod, responseHeaders, responseStatus)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.Mem (performMajorGC)
import UnliftIO.Exception (onException)
import UnliftIO.MVar (withMVar)

import Ecluse.BenchLoad.Breakpoints
import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Fixture (withExternalProxy)
import Ecluse.BenchLoad.GraphRegistry (capturePath, graphRegistry, packageKey, traceHeaders)
import Ecluse.BenchLoad.Harness (LoadKnobs (lkServeMaxInFlight), loadKnobsFromEnv)
import Ecluse.BenchLoad.Valkey (Valkey, ValkeyConfig (..), externalFetch, withValkey)
import Ecluse.Composition.Sizing (resolveServeAdmission)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (..))
import Ecluse.Core.Registry.Npm.Metadata (newNpmMetadataReadsWithFetch)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Security (Limits (maxMetadataBytes), defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Cache (CacheConfig (..), StoreBudget (..))
import Ecluse.Core.Server.Cache.Store (RemovalCause (..), ReuseKind (..), StoreEvent (..))
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Core.Server.Upstream (MirrorServePlan (NoMirrorWrite))
import Ecluse.Runtime.Test.Telemetry (sumPoints, withTestTelemetry)
import Ecluse.Test.Corpus (permissiveAgeRules)
import Ecluse.Test.Rules (inertRuleDeps)
import Ecluse.Test.Server.Cache (defaultCacheConfig)
import Ecluse.Test.Server.Mount (npmServeDeps)
import Ecluse.Test.Wai (localhost)

-- | Graph commands never enter the legacy duration-driven load scenarios.
runGraph :: [String] -> IO ()
runGraph = \case
    ["origin", mode, root, port, latency] | mode `elem` ["capture", "frozen"] -> do
        createDirectoryIfMissing True root
        listen <- number port
        delay <- number latency
        traceHttp <- (/= Just "0") <$> lookupEnv "GRAPH_TRACE_HTTP"
        observe <- if traceHttp then observeHttp root (root </> (mode <> "-http.jsonl")) else pure id
        origin <- graphRegistry (mode == "capture") root delay
        runSettings (setHost "127.0.0.1" (setPort listen defaultSettings)) (observe origin)
    ["proxy", root, output, upstream, bytes, entries, ttl, clock, bodyLimit] -> do
        capacity <- number bytes
        count <- number entries
        seconds <- number ttl
        limit <- number bodyLimit
        port <- number upstream
        now <- maybe (benchFail "invalid policy clock") pure (iso8601ParseM clock)
        when (count < 1 || limit < 1) (benchFail "entry and body bounds must be positive")
        createDirectoryIfMissing True output
        rules <- prepare inertRuleDeps permissiveAgeRules
        knobs <- loadKnobsFromEnv
        traceHttp <- (/= Just "0") <$> lookupEnv "GRAPH_TRACE_HTTP"
        observe <- if traceHttp then observeHttp root (output </> "http.jsonl") else pure id
        traceCache <- (== Just "1") <$> lookupEnv "GRAPH_CACHE_EVENTS"
        cacheObserver <- if traceCache then Just <$> observeStore (output </> "cache-events.jsonl") else pure Nothing
        capabilities <- getNumCapabilities
        let base = npmServeDeps (Just (loopbackRegistryUrl (localhost port <> "/private-miss"))) (loopbackRegistryUrl (localhost port)) NoMirrorWrite rules (pure now)
            deps external proxyPort =
                base
                    { pdEgressUrl = Right . loopbackRegistryUrl
                    , pdMountBaseUrl = localhost proxyPort <> "/npm"
                    , pdLimits = defaultLimits{maxMetadataBytes = limit}
                    , pdMetadata = case external of
                        Nothing -> pdMetadata base
                        Just client -> (pdMetadata base){metadataNewReads = newNpmMetadataReadsWithFetch (externalFetch client)}
                    }
            config = defaultCacheConfig{cacheFullBudget = StoreBudget count capacity, cacheTtl = fromIntegral seconds}
        withOptionalValkey (localhost port) output seconds $ \external ->
            withTestTelemetry $ \telemetry meter ->
                withExternalProxy Npm (deps external) knobs config telemetry cacheObserver observe $ \proxyPort -> do
                    before <- getRTSStats
                    writeFileText (output </> "ready") (localhost proxyPort <> "/npm/")
                    awaitStop (output </> "stop")
                    after <- getRTSStats
                    performMajorGC
                    final <- getRTSStats
                    readFileBS "/proc/self/status" >>= writeFileBS (output </> "proc-status.txt")
                    counters <- forM metricNames $ \metric -> do
                        points <- sumPoints metric meter
                        pure (metric, [(show attrs :: Text, value) | (attrs, value) <- points])
                    LBS.writeFile (output </> "proxy.json") $
                        encode $
                            object
                                [ "fullBytes" .= capacity
                                , "effectiveFullBytes" .= max 1 capacity
                                , "fullEntries" .= count
                                , "versionBytes" .= sbMaxBytes (cacheVersionBudget config)
                                , "versionEntries" .= sbMaxEntries (cacheVersionBudget config)
                                , "assembledBytes" .= sbMaxBytes (cacheAssembledBudget config)
                                , "assembledEntries" .= sbMaxEntries (cacheAssembledBudget config)
                                , "capabilities" .= capabilities
                                , "admissionSlots" .= fst (resolveServeAdmission (lkServeMaxInFlight knobs) capabilities)
                                , "httpObservation" .= traceHttp
                                , "cacheObservation" .= traceCache
                                , "ttlSeconds" .= seconds
                                , "bodyLimit" .= limit
                                , "policyClock" .= clock
                                , "allocationBytes" .= (allocated_bytes after - allocated_bytes before)
                                , "gcCount" .= (gcs after - gcs before)
                                , "gcElapsedNs" .= (gc_elapsed_ns after - gc_elapsed_ns before)
                                , "rtsPeakLiveBytes" .= max_live_bytes final
                                , "postGcBytes" .= gcdetails_live_bytes (gc final)
                                , "metrics" .= counters
                                ]
    ["model", input, output, ttl] -> do
        observations <- either (benchFail . toText) pure . eitherDecode =<< readFileLBS input
        ttlMicros <- number ttl
        let weights = Map.fromList [(trKey r, trWeight r) | r <- observations]
            working = sum (Map.elems weights)
            ratios = [0.25, 0.5, 0.9, 1, 1.1, 2, 4, 8] :: [Double]
        cells <- forM ratios $ \ratio -> do
            let budget = ModelBudget (ceiling (fromIntegral working / ratio)) (max 1 (Map.size weights + 1)) (fromIntegral ttlMicros)
            result <- either benchFail pure (modelTrace budget observations)
            pure (object ["workingToCapacityRatio" .= ratio, "budget" .= budget, "result" .= result])
        LBS.writeFile output (encode (object ["kind" .= ("interval model, not measured cache events" :: Text), "artifactHits" .= ("upper-bound opportunities before version-store masking, compare measured full_hits" :: Text), "workingBytes" .= working, "weights" .= weights, "cells" .= cells]))
    _ -> benchFail "usage: bench-load graph origin|proxy|model (see docs/cache-breakpoints.md)"

withOptionalValkey :: Text -> FilePath -> Int -> (Maybe Valkey -> IO a) -> IO a
withOptionalValkey source output ttl use =
    lookupEnv "GRAPH_VALKEY_PORT" >>= \case
        Nothing -> use Nothing
        Just raw -> do
            port <- number raw
            namespace <- maybe (benchFail "GRAPH_CACHE_NAMESPACE is required for external retention") (pure . toText) =<< lookupEnv "GRAPH_CACHE_NAMESPACE"
            timeoutMicros <- maybe (pure 100_000) number =<< lookupEnv "GRAPH_VALKEY_TIMEOUT_US"
            traceCommands <- (== Just "1") <$> lookupEnv "GRAPH_CACHE_EVENTS"
            let config = ValkeyConfig port 16 timeoutMicros (ttl * 1000) ("v1:" <> namespace) source (output </> "valkey.jsonl") traceCommands
            withValkey config (use . Just)

observeStore :: FilePath -> IO (StoreEvent Text -> IO ())
observeStore destination = do
    lock <- newMVar ()
    pure $ \event -> do
        now <- getMonotonicTimeNSec
        let fields = case event of
                Inserted key weight inserted expiry -> ["kind" .= ("insert" :: Text), "key" .= key, "bytes" .= weight, "insertNs" .= inserted, "expiryNs" .= expiry]
                Reused key kind weight expiry -> ["kind" .= (if kind == ResolvedHit then "listing-hit" else "artifact-full-hit" :: Text), "key" .= key, "bytes" .= weight, "expiryNs" .= expiry]
                Removed key cause weight expiry ->
                    ["kind" .= ("remove" :: Text), "key" .= key, "bytes" .= weight, "expiryNs" .= expiry] <> case cause of
                        Expiry -> ["cause" .= ("expiry" :: Text)]
                        Capacity bytes entries -> ["cause" .= ("capacity" :: Text), "bytePressure" .= bytes, "countPressure" .= entries]
                Rejected key weight -> ["kind" .= ("refused" :: Text), "key" .= key, "bytes" .= weight]
                Joined key -> ["kind" .= ("collapse-attempt" :: Text), "key" .= key]
        withMVar lock (\() -> LBS.appendFile destination (encode (object ("observedNs" .= now : fields)) <> "\n"))

number :: String -> IO Int
number raw = case readMaybe raw of
    Just value | value >= 0 -> pure value
    _ -> benchFail ("invalid nonnegative integer: " <> toText raw)

awaitStop :: FilePath -> IO ()
awaitStop path = do
    enabled <- getRTSStatsEnabled
    unless enabled (benchFail "graph proxy requires +RTS -T")
    stopped <- doesFileExist path
    unless stopped (threadDelay 100_000 >> awaitStop path)

observeHttp :: FilePath -> FilePath -> IO Middleware
observeHttp root destination = do
    lock <- newMVar ()
    epoch <- getMonotonicTimeNSec
    pure $ \application request respond -> do
        start <- getMonotonicTimeNSec
        let parts = pathInfo request
            key = T.intercalate "/" (case parts of "npm" : rest -> rest; _ -> parts)
            record :: Maybe Int -> [(Text, Text)] -> IO ()
            record status headers = do
                end <- getMonotonicTimeNSec
                let weightFile = capturePath root (packageKey key) <> ".weight"
                exists <- doesFileExist weightFile
                weight <- if exists then either (benchFail . toText) pure . eitherDecode =<< readFileLBS weightFile else pure (Nothing :: Maybe Int)
                let row =
                        object
                            [ "key" .= key
                            , "package" .= packageKey key
                            , "method" .= (decodeUtf8 (requestMethod request) :: Text)
                            , "path" .= (decodeUtf8 (rawPathInfo request) :: Text)
                            , "requestHeaders" .= traceHeaders (requestHeaders request)
                            , "responseHeaders" .= headers
                            , "startMicros" .= ((start - epoch) `div` 1000)
                            , "endMicros" .= ((end - epoch) `div` 1000)
                            , "status" .= status
                            , "accountedBytes" .= weight
                            ]
                withMVar lock (\() -> LBS.appendFile destination (encode row <> "\n"))
        application
            request
            ( \response -> do
                result <- respond response
                record (Just (statusCode (responseStatus response))) (traceHeaders (responseHeaders response))
                pure result
            )
            `onException` record Nothing [("failure", "transport exception")]

metricNames :: [Text]
metricNames =
    [ "ecluse.metadata_cache.requests"
    , "ecluse.metadata_cache.version.requests"
    , "ecluse.metadata_cache.version.full_hits"
    , "ecluse.metadata_cache.assembled.requests"
    , "ecluse.metadata_cache.refused"
    ]
