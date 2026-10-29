-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Proxy lifecycle, requests and logs for native client scenarios.
module Ecluse.E2E.Harness.Proxy (
    withClientProxyWith,
    proxyStatus,
    proxyGet,
    proxyGetCoded,
    proxyHead,
    proxyPut,
    proxyRequest,
    shouldSucceedThroughProxy,

    -- * Logs
    proxyContainerLogs,
    mirrorContainerLogs,
    logTail,
    logTailLines,
    awaitProxyLog,
    mirrorVersionComplete,
    hasPopulatedTraceId,
) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Network.HTTP.Client (
    Request (decompress, method, requestBody, requestHeaders),
    RequestBody (RequestBodyBS),
    brConsume,
    httpLbs,
    parseRequest,
    responseBody,
    responseHeaders,
    responseStatus,
    withResponse,
 )
import Network.HTTP.Types (Method, RequestHeaders, statusCode)
import Network.HTTP.Types.Header (hAcceptEncoding)
import System.Exit (ExitCode (ExitSuccess))
import Test.Hspec (expectationFailure)

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, RubyGems))
import Ecluse.E2E.Harness.Client (clientReport)
import Ecluse.E2E.Harness.Docker (awaitContainerLog, containerLogs, withE2EWith)
import Ecluse.E2E.Harness.NativeEdge (nativeTraffic, observedProxy, withObservedNativeProxy)
import Ecluse.E2E.Harness.Types
import Ecluse.Test.Contract.Serving qualified as Serving
import Ecluse.Test.Contract.Telemetry (hasPopulatedTraceId)
import Ecluse.Test.Log (lineMessage)

-- | Native clients use the observed TLS edge and the real product listener.
withClientProxyWith :: E2EConfig -> (E2E -> IO a) -> GlobalDataPlane -> IO a
withClientProxyWith config action plane
    | any (`elem` topologyMounts (gdpTopology plane)) [Cargo, RubyGems] =
        withObservedNativeProxy config plane (\observed -> action (observedProxy observed) <* nativeTraffic observed)
    | otherwise = withE2EWith config action plane

-- | The HTTP status of a @GET@ to a proxy path (e.g. @\/npm\/e2e-allow@).
proxyStatus :: E2E -> Text -> IO Int
proxyStatus e2e path = fst <$> proxyGet e2e path

-- | @GET@ a proxy path, returning the status and body.
proxyGet :: E2E -> Text -> IO (Int, LByteString)
proxyGet e2e path = do
    req <- parseRequest (toString (e2eBaseUrl e2e <> path))
    resp <- httpLbs req (e2eManager e2e)
    pure (statusCode (responseStatus resp), responseBody resp)

-- | Observe the raw HEAD response, including any forbidden body bytes.
proxyHead :: E2E -> Text -> IO Serving.ServingResponse
proxyHead = proxyRaw "HEAD"

-- | Observe the raw GET response of a client that accepts gzip, with any content coding left on the body.
proxyGetCoded :: E2E -> Text -> IO Serving.ServingResponse
proxyGetCoded = proxyRaw "GET"

proxyRaw :: Method -> E2E -> Text -> IO Serving.ServingResponse
proxyRaw verb = proxyRequest verb [(hAcceptEncoding, "gzip")] ""

-- | Send one explicit native request and retain its exact response bytes and headers.
proxyRequest :: Method -> RequestHeaders -> ByteString -> E2E -> Text -> IO Serving.ServingResponse
proxyRequest verb headers body e2e path = do
    base <- parseRequest (toString (e2eBaseUrl e2e <> path))
    let request = base{method = verb, decompress = const False, requestHeaders = headers, requestBody = RequestBodyBS body}
    withResponse request (e2eManager e2e) $ \resp -> do
        chunks <- brConsume (responseBody resp)
        pure
            Serving.ServingResponse
                { Serving.responseStatus = statusCode (responseStatus resp)
                , Serving.responseBody = LBS.fromChunks chunks
                , Serving.responseHeaders = responseHeaders resp
                , Serving.responseVersions = []
                , Serving.responseDigests = mempty
                }

{- | An empty @PUT@ proves that a mount without a publication target refuses with @405@ before
reading the body.
-}
proxyPut :: E2E -> Text -> IO Int
proxyPut e2e path = do
    base <- parseRequest (toString (e2eBaseUrl e2e <> path))
    resp <- httpLbs base{method = "PUT"} (e2eManager e2e)
    pure (statusCode (responseStatus resp))

{- | 'shouldSucceed' with the proxy's and the mirror store's own log tails, because a refusal
reaches a client as a bare status whose reason exists only in those logs.
-}
shouldSucceedThroughProxy :: E2E -> ClientResult -> IO ClientResult
shouldSucceedThroughProxy e2e res = case crExit res of
    ExitSuccess -> pure res
    _ -> do
        proxyLog <- proxyContainerLogs e2e
        mirrorLog <- mirrorContainerLogs e2e
        expectationFailure (toString (clientRefusal res proxyLog mirrorLog))
        pure res

-- The client's own output, then the two logs that decided the status it saw.
clientRefusal :: ClientResult -> Text -> Text -> Text
clientRefusal res proxyLog mirrorLog =
    clientReport res "failed"
        <> tailSection "proxy" proxyLog
        <> tailSection "mirror store" mirrorLog

tailSection :: Text -> Text -> Text
tailSection label logs =
    "\nLast " <> show logTailLines <> " " <> label <> " log lines:\n" <> logTail logTailLines logs

-- | The last @n@ lines of a container's log, for a failure whose reason lives only there.
logTail :: Int -> Text -> Text
logTail n = T.intercalate "\n" . reverse . take n . reverse . lines

-- | How many log lines a failure carries: enough for the deciding request, short enough to read.
logTailLines :: Int
logTailLines = 50

{- | The proxy container's combined stdout and stderr: the JSONL stream it writes under
@ECLUSE_OBSERVABILITY__LOG_FORMAT=json@.
-}
proxyContainerLogs :: E2E -> IO Text
proxyContainerLogs = containerLogs . e2eProxyContainer

-- | The mirror store's own stdout, which carries a publish or render error the proxy cannot see.
mirrorContainerLogs :: E2E -> IO Text
mirrorContainerLogs e2e = maybe (pure "no package store in this topology") (containerLogs . serviceContainer) (e2eStore e2e)

{- | Poll the proxy's own log stream until the predicate holds, or the attempts lapse. Use it
for an assertion that must await an asynchronous line.
-}
awaitProxyLog :: E2E -> (Text -> Bool) -> Int -> IO Bool
awaitProxyLog e2e = awaitContainerLog (e2eProxyContainer e2e)

-- | Match the worker's structured completion message for one exact native package identity.
mirrorVersionComplete :: Text -> Text -> Text -> Bool
mirrorVersionComplete name version = elem ("mirror version complete: " <> name <> "@" <> version) . mapMaybe lineMessage . T.lines
