-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.E2E.Harness.Proxy (
    proxyStatus,
    proxyGet,
    proxyHead,
    proxyPut,

    -- * Logs
    proxyContainerLogs,
    awaitProxyLog,
    awaitCollectorLog,
    hasPopulatedTraceId,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isDigit)
import Data.List (lookup)
import Data.Text qualified as T
import Network.HTTP.Client (
    Request (method),
    brConsume,
    httpLbs,
    parseRequest,
    responseBody,
    responseHeaders,
    responseStatus,
    withResponse,
 )
import Network.HTTP.Types (hContentLength, statusCode)
import System.Process.Typed (proc, readProcess)
import UnliftIO (handleAny)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.E2E.Harness.Types

-- | The HTTP status of a @GET@ to a proxy path (e.g. @\/npm\/e2e-allow@).
proxyStatus :: E2E -> Text -> IO Int
proxyStatus e2e path = fst <$> proxyGet e2e path

-- | @GET@ a proxy path, returning the status and body.
proxyGet :: E2E -> Text -> IO (Int, LByteString)
proxyGet e2e path = do
    req <- parseRequest (toString (e2eBaseUrl e2e <> path))
    resp <- httpLbs req (e2eManager e2e)
    pure (statusCode (responseStatus resp), responseBody resp)

{- | @HEAD@ a proxy path, returning the status, the declared @Content-Length@ (if
any), and how many body bytes actually arrived -- so a test can assert a @HEAD@ does
not stream a body.
-}
proxyHead :: E2E -> Text -> IO (Int, Maybe Int, Int)
proxyHead e2e path = do
    base <- parseRequest (toString (e2eBaseUrl e2e <> path))
    let req = base{method = "HEAD"}
    withResponse req (e2eManager e2e) $ \resp -> do
        chunks <- brConsume (responseBody resp)
        let declared = do
                raw <- lookup hContentLength (responseHeaders resp)
                readMaybe (toString (decodeUtf8 raw :: Text))
        pure (statusCode (responseStatus resp), declared, sum (map BS.length chunks))

{- | @PUT@ a proxy path with an empty body, returning the status -- the raw publish probe.
A publish on a mount with __no__ publication target configured is refused (@405@) before
the request body is read, so an empty @PUT@ is enough to assert the opt-in posture without
driving the @npm@ CLI.
-}
proxyPut :: E2E -> Text -> IO Int
proxyPut e2e path = do
    base <- parseRequest (toString (e2eBaseUrl e2e <> path))
    resp <- httpLbs base{method = "PUT"} (e2eManager e2e)
    pure (statusCode (responseStatus resp))

{- | The proxy container's combined stdout+stderr as docker has captured it so far -- the
JSONL stream the proxy writes (@ECLUSE_LOG_FORMAT=json@), so a test can assert the proxy
logs at all (the stdout\/stderr property) and inspect the @dd@ object on its lines.
-}
proxyContainerLogs :: E2E -> IO Text
proxyContainerLogs = containerLogs . e2eProxyContainer

{- | Poll the proxy's own log stream until the predicate holds, or the attempts lapse --
for an assertion that has to await an asynchronous line (e.g. the worker's
@mirrored artifact published@, or a throttled telemetry export-error warning).
-}
awaitProxyLog :: E2E -> (Text -> Bool) -> Int -> IO Bool
awaitProxyLog e2e = awaitContainerLog (e2eProxyContainer e2e)

{- | Poll the OTLP collector's debug-exporter output until the predicate holds. Fails
loudly if the environment was booted without a collector (a scenario wiring error: only
a @ecCollector = True@ environment has one to read).
-}
awaitCollectorLog :: E2E -> (Text -> Bool) -> Int -> IO Bool
awaitCollectorLog e2e matches attempts =
    case e2eCollectorContainer e2e of
        Nothing -> fail "awaitCollectorLog: this environment was booted without a collector"
        Just coll -> awaitContainerLog coll matches attempts

-- Poll a container's logs until the predicate holds, up to @attempts@ times at ~250ms.
awaitContainerLog :: String -> (Text -> Bool) -> Int -> IO Bool
awaitContainerLog cname matches = go
  where
    go n
        | n <= 0 = pure False
        | otherwise = do
            logs <- containerLogs cname
            if matches logs then pure True else threadDelay 250000 >> go (n - 1)

-- A container's combined stdout+stderr so far ('docker logs'); empty on any docker
-- error (e.g. the container does not exist yet, mid image-pull).
containerLogs :: String -> IO Text
containerLogs cname =
    handleAny (\_ -> pure "") $ do
        (_, out, err) <- readProcess (proc "docker" ["logs", cname])
        pure (decodeUtf8 (LBS.toStrict out) <> decodeUtf8 (LBS.toStrict err))

{- | Whether any @dd@ object across the given log text carries a __populated__ (digit-
leading) @trace_id@ -- the active-span correlation, present only when telemetry is on and
a span is in scope. Split on the @"trace_id":"@ prefix and require a value that begins
with a digit, so an absent or empty id does not satisfy it.
-}
hasPopulatedTraceId :: Text -> Bool
hasPopulatedTraceId logs =
    any leadsWithDigit (drop 1 (T.splitOn "\"trace_id\":\"" logs))
  where
    leadsWithDigit seg = maybe False (isDigit . fst) (T.uncons seg)
