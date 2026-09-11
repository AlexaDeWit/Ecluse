-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.E2E.Harness.Proxy (
    proxyStatus,
    proxyGet,
    proxyHead,
    proxyPut,
    shouldSucceedThroughProxy,

    -- * Logs
    proxyContainerLogs,
    awaitProxyLog,
    awaitCollectorLog,
    hasPopulatedTraceId,
) where

import Data.ByteString qualified as BS
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
import System.Exit (ExitCode (ExitSuccess))
import Test.Hspec (expectationFailure)

import Ecluse.E2E.Harness.Docker (awaitContainerLog, containerLogs)
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

{- | 'shouldSucceed' with the proxy's own JSONL tail, because a refusal reaches a client as a
bare status whose reason exists only in that log.
-}
shouldSucceedThroughProxy :: E2E -> ClientResult -> IO ClientResult
shouldSucceedThroughProxy e2e res = case crExit res of
    ExitSuccess -> pure res
    _ -> do
        logs <- proxyContainerLogs e2e
        expectationFailure (toString (clientRefusal res logs))
        pure res

-- The client's own output, then the proxy lines that decided the status it saw.
clientRefusal :: ClientResult -> Text -> Text
clientRefusal res logs =
    crCommand res
        <> " failed!\nSTDOUT:\n"
        <> crStdout res
        <> "\nSTDERR:\n"
        <> crStderr res
        <> "\nLast "
        <> show logTailLines
        <> " proxy log lines:\n"
        <> T.intercalate "\n" (reverse (take logTailLines (reverse (lines logs))))

logTailLines :: Int
logTailLines = 50

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

{- | Whether any @dd@ object in the log text carries a __populated__ @trace_id@. The value must
begin with a digit, so an absent or empty id does not satisfy it.
-}
hasPopulatedTraceId :: Text -> Bool
hasPopulatedTraceId logs =
    any leadsWithDigit (drop 1 (T.splitOn "\"trace_id\":\"" logs))
  where
    leadsWithDigit seg = maybe False (isDigit . fst) (T.uncons seg)
