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

{- | @HEAD@ a proxy path. It returns the status, the declared @Content-Length@, and how many body
bytes actually arrived, so a test can assert that a @HEAD@ streams no body.
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

{- | @PUT@ a proxy path with an empty body, returning the status. A mount with __no__ publication
target refuses the publish with @405@ before it reads the body, so an empty @PUT@ proves the opt-in
posture without driving the @npm@ CLI.
-}
proxyPut :: E2E -> Text -> IO Int
proxyPut e2e path = do
    base <- parseRequest (toString (e2eBaseUrl e2e <> path))
    resp <- httpLbs base{method = "PUT"} (e2eManager e2e)
    pure (statusCode (responseStatus resp))

{- | The proxy container's combined stdout and stderr: the JSONL stream it writes under
@ECLUSE_OBSERVABILITY__LOG_FORMAT=json@.
-}
proxyContainerLogs :: E2E -> IO Text
proxyContainerLogs = containerLogs . e2eProxyContainer

{- | Poll the proxy's own log stream until the predicate holds, or the attempts lapse. Use it for an
assertion that must await an asynchronous line.
-}
awaitProxyLog :: E2E -> (Text -> Bool) -> Int -> IO Bool
awaitProxyLog e2e = awaitContainerLog (e2eProxyContainer e2e)

{- | Poll the OTLP collector's debug-exporter output until the predicate holds. It fails loudly when
the environment booted without a collector, which only @ecCollector = True@ provides.
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

-- A container's combined stdout+stderr so far ('docker logs'). Empty on any docker
-- error, such as the container not existing yet, mid image-pull.
containerLogs :: String -> IO Text
containerLogs cname =
    handleAny (\_ -> pure "") $ do
        (_, out, err) <- readProcess (proc "docker" ["logs", cname])
        pure (decodeUtf8 (LBS.toStrict out) <> decodeUtf8 (LBS.toStrict err))

{- | Whether any @dd@ object in the log text carries a __populated__ @trace_id@. The value must
begin with a digit, so an absent or empty id does not satisfy it.
-}
hasPopulatedTraceId :: Text -> Bool
hasPopulatedTraceId logs =
    any leadsWithDigit (drop 1 (T.splitOn "\"trace_id\":\"" logs))
  where
    leadsWithDigit seg = maybe False (isDigit . fst) (T.uncons seg)
