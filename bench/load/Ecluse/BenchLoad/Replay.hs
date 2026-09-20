-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Finite HTTP replay. Each client consumes its trace once, without a warm-up pass.
module Ecluse.BenchLoad.Replay (Replay (..), runReplay) where

import Control.Concurrent (threadDelay)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import GHC.Clock (getMonotonicTime)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (statusCode)
import UnliftIO.Async (mapConcurrently)
import UnliftIO.Exception (tryAny)

import Ecluse.BenchLoad.Oha (OhaReport (..))
import Ecluse.BenchLoad.Patterns (ClientTrace (..), RequestTrace (..))

-- | The URL mapping can expand a listing into a listing followed by a selected-version read.
data Replay = Replay
    { replayTrace :: RequestTrace
    , replayUrls :: Text -> [Text]
    , replayEvidence :: IO Text
    }

-- | Complete every scheduled request. Percentiles include successful responses only.
runReplay :: Replay -> IO OhaReport
runReplay replay = do
    manager <- HTTP.newManager HTTP.defaultManagerSettings
    start <- getMonotonicTime
    results <- concat <$> mapConcurrently (runClient manager start) (rtClients (replayTrace replay))
    end <- getMonotonicTime
    let elapsed = max 1e-9 (end - start)
        successes = sort [latency | (Right status, latency) <- results, status >= 200 && status < 400]
        statuses = Map.fromListWith (+) [(show status, 1) | (Right status, _) <- results]
        failures = length [() | (Left _, _) <- results]
        quantile q = successes !!? max 0 (ceiling (q * fromIntegral (length successes)) - 1)
    pure
        OhaReport
            { ohaRequestsPerSec = fromIntegral (length successes) / elapsed
            , ohaSuccessRate = fromIntegral (length successes) / fromIntegral (max 1 (length results))
            , ohaElapsedSeconds = elapsed
            , ohaP50 = quantile (0.5 :: Double)
            , ohaP90 = quantile 0.9
            , ohaP99 = quantile 0.99
            , ohaP999 = quantile 0.999
            , ohaStatusCounts = statuses
            , ohaErrorCounts = if failures == 0 then mempty else Map.singleton "transport failure" failures
            }
  where
    runClient manager start client = do
        waitUntil (start + fromIntegral (ctStartMicros client) / 1_000_000)
        concat
            <$> forM
                (zip [0 :: Int ..] (ctNames client))
                ( \(index, name) -> do
                    waitUntil (start + fromIntegral (ctStartMicros client + index * ctIntervalMicros client) / 1_000_000)
                    traverse (fetch manager) (replayUrls replay name)
                )
    fetch manager url = do
        request <- HTTP.parseRequest (toString url)
        start <- getMonotonicTime
        result <- tryAny (HTTP.httpLbs request{HTTP.redirectCount = 0, HTTP.checkResponse = \_ _ -> pass} manager)
        for_ result (evaluate . LBS.length . HTTP.responseBody)
        end <- getMonotonicTime
        pure (statusCode . HTTP.responseStatus <$> result, end - start)

waitUntil :: Double -> IO ()
waitUntil deadline = do
    now <- getMonotonicTime
    when (deadline > now) (threadDelay (ceiling ((deadline - now) * 1_000_000)))
