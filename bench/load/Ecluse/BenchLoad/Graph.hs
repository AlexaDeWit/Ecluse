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
import Ecluse.BenchLoad.GraphRegistry (capturePath, graphRegistry, packageKey)
import Ecluse.BenchLoad.Harness (loadKnobsFromEnv)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Security (Limits (maxMetadataBytes), defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Cache (CacheConfig (..), StoreBudget (..))
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
        observe <- observeHttp root (root </> (mode <> "-http.jsonl"))
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
        observe <- observeHttp root (output </> "http.jsonl")
        let deps proxyPort =
                (npmServeDeps (Just (loopbackRegistryUrl (localhost port <> "/private-miss"))) (loopbackRegistryUrl (localhost port)) NoMirrorWrite rules (pure now))
                    { pdEgressUrl = Right . loopbackRegistryUrl
                    , pdMountBaseUrl = localhost proxyPort <> "/npm"
                    , pdLimits = defaultLimits{maxMetadataBytes = limit}
                    }
            config = defaultCacheConfig{cacheFullBudget = StoreBudget count capacity, cacheTtl = fromIntegral seconds}
        withTestTelemetry $ \telemetry meter ->
            withExternalProxy Npm deps knobs config telemetry observe $ \proxyPort -> do
                before <- getRTSStats
                writeFileText (output </> "ready") (localhost proxyPort <> "/npm/")
                awaitStop (output </> "stop")
                after <- getRTSStats
                performMajorGC
                final <- getRTSStats
                readFileText "/proc/self/status" >>= writeFileText (output </> "proc-status.txt")
                counters <- forM metricNames $ \metric -> do
                    points <- sumPoints metric meter
                    pure (metric, [(show attrs :: Text, value) | (attrs, value) <- points])
                LBS.writeFile (output </> "proxy.json") $
                    encode $
                        object
                            [ "fullBytes" .= capacity
                            , "fullEntries" .= count
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
        reads <- either (benchFail . toText) pure . eitherDecode =<< readFileLBS input
        ttlMicros <- number ttl
        let weights = Map.fromList [(trKey r, trWeight r) | r <- reads]
            working = sum (Map.elems weights)
            ratios = [0.25, 0.5, 0.9, 1, 1.1, 2, 4, 8] :: [Double]
        cells <- forM ratios $ \ratio -> do
            let budget = ModelBudget (ceiling (fromIntegral working / ratio)) (max 1 (Map.size weights + 1)) (fromIntegral ttlMicros)
            result <- either benchFail pure (modelTrace budget reads)
            pure (object ["workingToCapacityRatio" .= ratio, "budget" .= budget, "result" .= result])
        LBS.writeFile output (encode (object ["kind" .= ("interval model, not measured cache events" :: Text), "workingBytes" .= working, "weights" .= weights, "cells" .= cells]))
    _ -> benchFail "usage: bench-load graph origin|proxy|model (see docs/cache-breakpoints.md)"

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
                            , "requestHeaders" .= (show (requestHeaders request) :: Text)
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
                record (Just (statusCode (responseStatus response))) (show (responseHeaders response) :: Text)
                pure result
            )
            `onException` record Nothing "transport exception"

metricNames :: [Text]
metricNames =
    [ "ecluse.metadata_cache.requests"
    , "ecluse.metadata_cache.version.requests"
    , "ecluse.metadata_cache.version.full_hits"
    , "ecluse.metadata_cache.assembled.requests"
    , "ecluse.metadata_cache.refused"
    ]
