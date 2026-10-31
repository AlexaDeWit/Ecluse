-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Relay outcomes, response lifetimes, and bounded streaming.
module Ecluse.Core.Server.StreamSpec (spec) where

import Prelude hiding (get)

import Control.Exception (AsyncException (ThreadKilled))
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import Data.Text qualified as T
import GHC.Clock (getMonotonicTime)
import GHC.Conc (BlockReason (BlockedOnException), ThreadStatus (ThreadBlocked), threadStatus, throwTo)
import Network.HTTP.Client (
    defaultManagerSettings,
    httpLbs,
    newManager,
    parseRequest,
    responseBody,
 )
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (Method, Status, methodGet, methodHead, status200, status206, status304, status404, status502, status503, statusCode, statusIsSuccessful)
import Network.HTTP.Types.Header (HeaderName, hContentType, hETag)
import Network.Socket.ByteString (recv, sendAll)
import Network.Wai (Application, Response, ResponseReceived, responseLBS, responseRaw, responseStream)
import Network.Wai.Handler.Warp (testWithApplication)
import System.Timeout qualified as Timeout
import Test.Hspec
import UnliftIO (concurrently)
import UnliftIO.Async (asyncThreadId, waitCatch, withAsync)
import UnliftIO.Concurrent (threadDelay, yield)
import UnliftIO.Exception (mask, tryAny, withException)

import Ecluse.Core.Fault (RetryAfter (RetryAfter), TransportCause (TransportUnreachable), TransportFault (tfCause))
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient (ecManager, ecTrace))
import Ecluse.Core.Security (Limits (progressFloor), ProgressFloor, defaultLimits, mkProgressFloor)
import Ecluse.Core.Server.Conditional (isNotModified)
import Ecluse.Core.Server.Pipeline.Tarball.Relay (relayUnjudged)
import Ecluse.Core.Server.Stream (RelayResponder (RelayResponder), UpstreamBody (NoBody, StreamBody), UpstreamBodyFailed (UpstreamBodyFailed), UpstreamResult (..), pumpBody, withUpstreamResult)
import Ecluse.Test.Deadline (withExchangeClient)
import Ecluse.Test.Registry.JsonStream (sliceChunks)
import Ecluse.Test.Stub (requestSecrets, signedRequest)
import Ecluse.Test.Stub.Socket (awaitRequestHead, resetAfterPartialBody, trustingTlsManager, withRawServer, withTlsServer)
import Ecluse.Test.Wai (pacedBody)

spec :: Spec
spec = do
    describe "pumpBody, constant memory and backpressure" $ do
        it "holds at most one chunk in flight regardless of body size (constant memory)" $ do
            let chunks = replicate 256 (BS.replicate 4096 0x61)
                total = length chunks
            sink <- newEmptyMVar -- producer -> consumer handoff
            ack <- newEmptyMVar -- consumer -> producer acknowledgement
            src <- newSource chunks
            let write builder = do
                    putMVar sink (builtBytes builder)
                    takeMVar ack -- block until the consumer takes it
                flush = pure ()
                consume acc n
                    | n >= total = pure (reverse acc)
                    | otherwise = do
                        c <- takeMVar sink
                        modifyIORef' (srcOutstanding src) (subtract 1)
                        putMVar ack () -- release the producer's write
                        consume (c : acc) (n + 1)
            (_, collected) <- concurrently (pumpBody (srcNext src) write flush) (consume [] 0)
            mconcat collected `shouldBe` mconcat chunks -- all bytes, in order
            readIORef (srcHighWater src) `shouldReturn` 1 -- only ever one resident
        it "writes every chunk in order and stops at the empty terminator" $ do
            let chunks = ["alpha", "beta", "gamma"]
            src <- newSource chunks
            out <- newIORef []
            pumpBody (srcNext src) (\b -> modifyIORef' out (builtBytes b :)) (pure ())
            (reverse <$> readIORef out) `shouldReturn` chunks
            readIORef (srcProduced src) `shouldReturn` 3 -- the terminator pull produces nothing
        it "writes nothing and flushes nothing for an empty body" $ do
            src <- newSource []
            writes <- newIORef (0 :: Int)
            flushes <- newIORef (0 :: Int)
            pumpBody (srcNext src) (const (modifyIORef' writes (+ 1))) (modifyIORef' flushes (+ 1))
            readIORef writes `shouldReturn` 0
            readIORef flushes `shouldReturn` 0
        it "flushes the first chunk only, coalescing the rest in the sink's buffer" $ do
            -- One explicit flush pushes the status, headers, and opening bytes out promptly (time
            -- to first byte). Flushing every chunk would pay a socket send per upstream read.
            src <- newSource ["alpha", "beta", "gamma"]
            flushes <- newIORef (0 :: Int)
            pumpBody (srcNext src) (const (pure ())) (modifyIORef' flushes (+ 1))
            readIORef flushes `shouldReturn` 1

    describe "withUpstreamResult, response acquisition cancellation" $
        it "closes an acquired response before propagating cancellation during trace bookkeeping" $ do
            reached <- newEmptyMVar
            released <- newIORef False
            closed <- newEmptyMVar
            retained <- newIORef Nothing
            let peer connection = do
                    awaitRequestHead connection ""
                    sendAll connection "HTTP/1.1 200 OK\r\nContent-Length: 100\r\n\r\nx"
                    recv connection 1 >>= putMVar closed
                traced request open = do
                    response <- mask $ \restore -> do
                        acquired <- restore (open request)
                        writeIORef retained (Just acquired)
                        putMVar reached ()
                        let spin = readIORef released >>= \done -> unless done (yield >> spin)
                        spin
                        pure acquired
                    -- Offer a scheduling point after bookkeeping, before ownership returns.
                    yield
                    pure response
                unused = RelayResponder (\_ _ _ -> expectationFailure "cancelled relay reached responder") (\_ _ -> expectationFailure "cancelled relay reached responder")
            withRawServer peer $ \port ->
                withExchangeClient (newManager defaultManagerSettings) $ \client -> do
                    request <- parseRequest ("http://127.0.0.1:" <> show port <> "/")
                    withAsync (withUpstreamResult client{ecTrace = traced} (progressFloor defaultLimits) request StreamBody statusIsSuccessful relayUnjudged unused) $ \relay -> do
                        takeMVar reached
                        withAsync (throwTo (asyncThreadId relay) ThreadKilled) $ \thrower -> do
                            let awaitPending =
                                    threadStatus (asyncThreadId thrower) >>= \case
                                        ThreadBlocked BlockedOnException -> pure ()
                                        _ -> threadDelay 1000 >> awaitPending
                            queuedException <- Timeout.timeout 5_000_000 awaitPending
                            writeIORef released True
                            queuedException `shouldBe` Just ()
                            outcome <- waitCatch relay
                            either fromException (const Nothing) outcome `shouldBe` Just ThreadKilled
                            Timeout.timeout 5_000_000 (takeMVar closed) `shouldReturn` Just ""
                            isJust <$> readIORef retained `shouldReturn` True

    describe "withUpstreamResult, a large body end to end over an in-process upstream" $
        it "relays a large body through with the upstream status" $ do
            let bigBody = BS.replicate (4 * 1024 * 1024) 0x7a
            resp <- throughProxy (upstreamApp bigBody) conditionalProxy
            statusCode (HTTP.responseStatus resp) `shouldBe` 206
            toStrict (responseBody resp) `shouldBe` bigBody

    describe "withUpstreamResult, a conditional relay (hit, miss, open failure)" $ do
        it "relays the body AND the upstream content headers when the status passes accept" $ do
            resp <- throughProxy headeredUpstream conditionalProxy
            toStrict (responseBody resp) `shouldBe` "the-bytes"
            headerOf hContentType resp `shouldBe` Just "application/octet-stream"

        it "retains the rejected upstream status without relaying its body" $ do
            resp <- throughProxy missingUpstream conditionalProxy
            statusCode (HTTP.responseStatus resp) `shouldBe` 404
            responseBody resp `shouldBe` "REJECTED"

        it "distinguishes a failed connection from an upstream rejection" $ do
            resp <- throughDeadUpstream conditionalProxy
            statusCode (HTTP.responseStatus resp) `shouldBe` 502
            responseBody resp `shouldBe` "TRANSPORT-FAILURE"

        it "relays an upstream 304 as a bodiless 304, forwarding its validator (the pass-through conditional relay)" $ do
            resp <- throughProxy notModifiedUpstream notModifiedProxy
            statusCode (HTTP.responseStatus resp) `shouldBe` 304
            responseBody resp `shouldBe` ""
            headerOf hETag resp `shouldBe` Just "\"v1\""

    describe "withUpstreamResult, the progress floor with no cap" $
        it "aborts a relay whose upstream stops mid-body within about one window" $ do
            progress <- either (fail . show) pure (mkProgressFloor 30 0.5 1024)
            started <- getMonotonicTime
            outcome <- tryAny (throughProxy (pacedBody [(0, "partial"), (20_000_000, "rest")]) (relayProxy progress StreamBody statusIsSuccessful methodGet))
            finished <- getMonotonicTime
            outcome `shouldSatisfy` isLeft
            (finished - started) `shouldSatisfy` (\seconds -> seconds >= 0.5 && seconds < 10)

    describe "withUpstreamResult, an upstream body that breaks after the commit" $ do
        it "raises a typed fault that never carries the upstream request" $ do
            escaped <- newIORef Nothing
            void (tryAny (throughProxy truncatedUpstream (recordingSignedRelay id escaped)))
            readIORef escaped >>= \case
                Just err -> do
                    fromException err `shouldSatisfy` \case
                        Just (UpstreamBodyFailed _) -> True
                        Nothing -> False
                    for_ requestSecrets $ \secret -> toText (displayException err) `shouldSatisfy` (not . T.isInfixOf secret)
                Nothing -> expectationFailure "no fault escaped the committed relay"

        it "raises a typed unreachable fault when a TLS upstream resets mid-body" $ do
            escaped <- newIORef Nothing
            withExchangeClient trustingTlsManager $ \client -> do
                void (tryAny (withTlsServer resetAfterPartialBody (askProxy (ecManager client) . recordingSignedRelay overTls escaped client)))
                readIORef escaped >>= \case
                    Just err -> do
                        fmap (\(UpstreamBodyFailed fault) -> tfCause fault) (fromException err) `shouldBe` Just TransportUnreachable
                        for_ requestSecrets $ \secret -> toText (displayException err) `shouldSatisfy` (not . T.isInfixOf secret)
                    Nothing -> expectationFailure "no fault escaped the committed relay"

    describe "withUpstreamResult, a bodiless relay (HEAD, no pump)" $
        it "relays the upstream status and content headers with no body on a hit" $ do
            resp <- throughProxy headLengthUpstream probeProxy
            responseBody resp `shouldBe` ""
            headerOf hContentType resp `shouldBe` Just "application/octet-stream"

-- The proxy under test, over the client it relays with and the upstream's port.
type ProxyApp = ExchangeClient -> Int -> Application

-- Each uncommitted outcome gets a distinct response so the tests retain its cause.
relayProxy :: ProgressFloor -> UpstreamBody -> (Status -> Bool) -> Method -> ProxyApp
relayProxy progress body accept method client upPort _req respond = do
    upstream <- parseRequest ("http://127.0.0.1:" <> show upPort <> "/")
    outcome <-
        withUpstreamResult
            client
            progress
            upstream{HTTP.method = method}
            body
            accept
            relayUnjudged
            (waiRelayResponder respond)
    answerOutcome respond outcome

-- A relay of a signed upstream URL that records whatever escapes it, so a case reads what a committed failure carries.
recordingSignedRelay :: (HTTP.Request -> HTTP.Request) -> IORef (Maybe SomeException) -> ProxyApp
recordingSignedRelay scheme escaped client upPort _req respond = do
    upstream <- (\request -> scheme request{HTTP.port = upPort}) <$> signedRequest
    outcome <-
        withUpstreamResult client (progressFloor defaultLimits) upstream StreamBody statusIsSuccessful relayUnjudged (waiRelayResponder respond)
            `withException` record
    answerOutcome respond outcome
  where
    -- A socket error renders lazily. An async exception that interrupts that render stays in the error and raises at every later read.
    -- Rendering here runs before the proxy rethrows the failure, so the case's teardown of the proxy cannot interrupt it.
    record err = evaluateNF_ (displayException err) >> writeIORef escaped (Just err)

overTls :: HTTP.Request -> HTTP.Request
overTls request = request{HTTP.secure = True}

-- An upstream that promises twenty bytes, sends five, and closes, so the relay fails after it commits.
truncatedUpstream :: Application
truncatedUpstream _req respond =
    respond (responseRaw (\_ send -> send "HTTP/1.1 200 OK\r\nContent-Length: 20\r\nConnection: close\r\n\r\nshort") (responseLBS status404 [] ""))

conditionalProxy :: ProxyApp
conditionalProxy = relayProxy (progressFloor defaultLimits) StreamBody statusIsSuccessful methodGet

-- The artifact relay's own accept predicate: a 2xx or a 304.
notModifiedProxy :: ProxyApp
notModifiedProxy = relayProxy (progressFloor defaultLimits) StreamBody (\status -> statusIsSuccessful status || isNotModified status) methodGet

-- The probe: a HEAD upstream, answered with no body, so a hit is observable as the headers alone.
probeProxy :: ProxyApp
probeProxy = relayProxy (progressFloor defaultLimits) NoBody statusIsSuccessful methodHead

-- One request through the proxy under test, standing in front of this upstream.
throughProxy :: Application -> ProxyApp -> IO (HTTP.Response LByteString)
throughProxy upstream proxy = do
    withExchangeClient (newManager defaultManagerSettings) $ \client -> do
        testWithApplication (pure upstream) (askProxy (ecManager client) . proxy client)

-- 'throughProxy' against a port bound only long enough to learn a free one, then released, so
-- the proxy's open fails.
throughDeadUpstream :: ProxyApp -> IO (HTTP.Response LByteString)
throughDeadUpstream proxy = do
    withExchangeClient (newManager defaultManagerSettings) $ \client -> do
        deadPort <- testWithApplication (pure missingUpstream) pure
        askProxy (ecManager client) (proxy client deadPort)

-- One request at the proxy's root, over the manager the proxy itself relays with.
askProxy :: HTTP.Manager -> Application -> IO (HTTP.Response LByteString)
askProxy manager app =
    testWithApplication (pure app) $ \proxyPort -> do
        request <- parseRequest ("http://127.0.0.1:" <> show proxyPort <> "/")
        httpLbs request manager

-- The relayed value of one response header, which the client must receive unchanged.
headerOf :: HeaderName -> HTTP.Response body -> Maybe ByteString
headerOf name = fmap snd . find ((== name) . fst) . HTTP.responseHeaders

-- An upstream that streams a fixed body back in 64 KiB chunks, under a 2xx the proxy never
-- answers itself, so a relay that committed its own status rather than upstream's would fail.
upstreamApp :: ByteString -> Application
upstreamApp body _req respond =
    respond (responseStream status206 [] (\write flush -> writeChunks write flush (sliceChunks 65536 body)))

headeredUpstream :: Application
headeredUpstream _req respond =
    respond (responseLBS status200 [(hContentType, "application/octet-stream")] "the-bytes")

missingUpstream :: Application
missingUpstream _req respond = respond (responseLBS status404 [] "not found")

notModifiedUpstream :: Application
notModifiedUpstream _req respond =
    respond (responseLBS status304 [(hETag, "\"v1\"")] "")

headLengthUpstream :: Application
headLengthUpstream _req respond =
    respond (responseLBS status200 [(hContentType, "application/octet-stream")] "")

answerOutcome :: (Response -> IO ResponseReceived) -> UpstreamResult ((), ResponseReceived) -> IO ResponseReceived
answerOutcome respond = \case
    UpstreamRelayed (_, received) -> pure received
    UpstreamRejected status _ -> respond (responseLBS status [] "REJECTED")
    UpstreamTransportFailure -> respond (responseLBS status502 [] "TRANSPORT-FAILURE")
    UpstreamCooling (RetryAfter seconds) -> respond (responseLBS status503 [("Retry-After", show seconds)] "COOLDOWN")

writeChunks :: (Builder -> IO ()) -> IO () -> [ByteString] -> IO ()
writeChunks _ _ [] = pure ()
writeChunks write flush (c : cs) = write (byteString c) >> flush >> writeChunks write flush cs

waiRelayResponder :: (Response -> IO ResponseReceived) -> RelayResponder ResponseReceived
waiRelayResponder respond =
    RelayResponder
        (\status headers body -> respond (responseStream status headers body))
        (\status headers -> respond (responseLBS status headers ""))

-- A chunk source on the http-client BodyReader contract: the next chunk on each pull, then an
-- empty ByteString once exhausted. It records the outstanding high-water mark.
data Source = Source
    { srcNext :: IO ByteString
    , srcProduced :: IORef Int
    , srcOutstanding :: IORef Int
    , srcHighWater :: IORef Int
    }

newSource :: [ByteString] -> IO Source
newSource chunks = do
    remaining <- newIORef chunks
    produced <- newIORef 0
    outstanding <- newIORef 0
    highWater <- newIORef 0
    let next = do
            cs <- readIORef remaining
            case cs of
                [] -> pure BS.empty
                (c : rest) -> do
                    writeIORef remaining rest
                    modifyIORef' produced (+ 1)
                    n <- atomicModifyIORef' outstanding (\o -> (o + 1, o + 1))
                    modifyIORef' highWater (max n)
                    pure c
    pure Source{srcNext = next, srcProduced = produced, srcOutstanding = outstanding, srcHighWater = highWater}

builtBytes :: Builder -> ByteString
builtBytes = toStrict . toLazyByteString
