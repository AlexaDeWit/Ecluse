-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Finite HTTP replay with one deadline covering scheduled arrivals and active reads.
module Ecluse.BenchLoad.Replay (Replay (..), ReplayReport (..), runReplay) where

import Control.Concurrent (threadDelay)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import GHC.Clock (getMonotonicTime)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (statusCode)
import UnliftIO (evaluate, timeout)
import UnliftIO.Async (mapConcurrently_)
import UnliftIO.Exception (mask, tryAny)

import Ecluse.BenchLoad.Oha (OhaReport (..))
import Ecluse.BenchLoad.PatternReport (ReplayTotals (..))
import Ecluse.BenchLoad.Patterns (ClientTrace (..), RequestTrace (..))

-- | URL expansion preserves listing-before-selected-version order within each client.
data Replay = Replay
    { replayTrace :: RequestTrace
    , replayDeadlineMicros :: Int
    , replayUrls :: Text -> [Text]
    , replayEvidence :: IO Text
    }

-- | HTTP distribution and scheduled-work accounting describe the same bounded window.
data ReplayReport = ReplayReport
    { replayHttp :: OhaReport
    , replayTotals :: ReplayTotals
    }
    deriving stock (Show)

-- | Cancel all clients at the deadline. External cancellation propagates to the caller.
runReplay :: Replay -> IO ReplayReport
runReplay replay = do
    manager <- HTTP.newManager HTTP.defaultManagerSettings
    results <- newIORef []
    start <- getMonotonicTime
    _ <- timeout (replayDeadlineMicros replay) (mapConcurrently_ (runClient manager results start replay) clients)
    end <- getMonotonicTime
    completed <- readIORef results
    pure (summarise scheduled (max 1e-9 (end - start)) completed)
  where
    clients = rtClients (replayTrace replay)
    scheduled = sum [length (replayUrls replay name) | client <- clients, name <- ctNames client]

runClient :: HTTP.Manager -> IORef [(Maybe Int, Double)] -> Double -> Replay -> ClientTrace -> IO ()
runClient manager results start replay client = do
    waitUntil (start + fromIntegral (ctStartMicros client) / 1_000_000)
    for_ (zip [0 :: Int ..] (ctNames client)) $ \(index, name) -> do
        waitUntil (start + fromIntegral (ctStartMicros client + index * ctIntervalMicros client) / 1_000_000)
        traverse_ (fetch manager results) (replayUrls replay name)

fetch :: HTTP.Manager -> IORef [(Maybe Int, Double)] -> Text -> IO ()
fetch manager results url = mask $ \restore -> do
    start <- getMonotonicTime
    result <- restore $ tryAny $ do
        request <- HTTP.parseRequest (toString url)
        response <- HTTP.httpLbs request{HTTP.redirectCount = 0, HTTP.checkResponse = \_ _ -> pass} manager
        _ <- evaluate (LBS.length (HTTP.responseBody response))
        evaluate (statusCode (HTTP.responseStatus response))
    end <- getMonotonicTime
    outcome <- evaluate (rightToMaybe result)
    latency <- evaluate (end - start)
    atomicModifyIORef' results (\completed -> ((outcome, latency) : completed, ()))

summarise :: Int -> Double -> [(Maybe Int, Double)] -> ReplayReport
summarise scheduled elapsed results = ReplayReport http totals
  where
    successes = sort [latency | (Just status, latency) <- results, status >= 200 && status < 400]
    statuses = Map.fromListWith (+) [(show status, 1) | (Just status, _) <- results]
    failed = length [() | (Nothing, _) <- results]
    completed = sum (Map.elems statuses)
    refused = sum [Map.findWithDefault 0 status statuses | status <- ["429", "503"]]
    quantile :: Double -> Maybe Double
    quantile q = successes !!? max 0 (ceiling (q * fromIntegral (length successes)) - 1)
    totals =
        ReplayTotals
            scheduled
            completed
            (length successes)
            refused
            (completed - length successes - refused)
            failed
            (scheduled - length results)
            elapsed
    http =
        OhaReport
            { ohaRequestsPerSec = fromIntegral (length successes) / elapsed
            , ohaSuccessRate = fromIntegral (length successes) / fromIntegral (max 1 scheduled)
            , ohaElapsedSeconds = elapsed
            , ohaP50 = quantile (0.5 :: Double)
            , ohaP90 = quantile 0.9
            , ohaP99 = quantile 0.99
            , ohaP999 = quantile 0.999
            , ohaStatusCounts = statuses
            , ohaErrorCounts = if failed == 0 then mempty else Map.singleton "transport failure" failed
            }

waitUntil :: Double -> IO ()
waitUntil deadline = do
    now <- getMonotonicTime
    when (deadline > now) (threadDelay (ceiling ((deadline - now) * 1_000_000)))
