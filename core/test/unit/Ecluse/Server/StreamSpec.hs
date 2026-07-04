module Ecluse.Server.StreamSpec (spec) where

import Prelude hiding (get)

import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import Network.HTTP.Client (
    defaultManagerSettings,
    httpLbs,
    newManager,
    parseRequest,
    responseBody,
 )
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (methodHead, status200, status304, status404, statusCode, statusIsSuccessful)
import Network.HTTP.Types.Header (hContentType, hETag)
import Network.Wai (Application, responseLBS, responseStream)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
import UnliftIO (concurrently)

import Ecluse.Core.Server.Conditional (isNotModified)
import Ecluse.Core.Server.Stream (probeUpstreamWhen, pumpBody, streamUpstream, streamUpstreamWhen)

{- | A chunk source over a fixed list of chunks: each pull returns the next chunk
and an empty 'ByteString' once exhausted (the @http-client@ @BodyReader@
contract). It records the high-water mark of chunks produced-but-not-yet-consumed
-- the residency the backpressure assertion turns on.
-}
data Source = Source
    { srcNext :: IO ByteString
    , srcProduced :: IORef Int
    , srcOutstanding :: IORef Int
    , srcHighWater :: IORef Int
    }

-- | Build a chunk 'Source' over the given chunks.
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

-- | Render a one-builder write back to the strict bytes it carried.
builtBytes :: Builder -> ByteString
builtBytes = toStrict . toLazyByteString

spec :: Spec
spec = do
    describe "pumpBody -- constant memory and backpressure" $ do
        it "holds at most one chunk in flight regardless of body size (constant memory)" $ do
            -- A 256-chunk body through a synchronous rendezvous: each 'write' blocks
            -- until the consumer has taken the chunk AND acked, so the handoff is
            -- fully complete before the pump loops back to read the next chunk. The
            -- producer therefore can never run ahead -- the outstanding high-water
            -- mark is exactly 1 no matter how large the body. Were the pump to
            -- buffer the whole body, the high-water would track the 256-chunk count;
            -- staying at 1 is the constant-memory property, and the rendezvous block
            -- is the backpressure ('write' pacing the read).
            let chunks = replicate 256 (BS.replicate 4096 0x61)
                total = length chunks
            sink <- newEmptyMVar -- producer -> consumer handoff
            ack <- newEmptyMVar -- consumer -> producer acknowledgement
            src <- newSource chunks
            let write builder = do
                    putMVar sink (builtBytes builder) -- offer the chunk
                    takeMVar ack -- block until the consumer has taken it
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
            readIORef (srcProduced src) `shouldReturn` 3 -- 3 chunks + the terminator pull
        it "writes nothing for an empty body" $ do
            src <- newSource []
            out <- newIORef (0 :: Int)
            pumpBody (srcNext src) (const (modifyIORef' out (+ 1))) (pure ())
            readIORef out `shouldReturn` 0
        it "flushes the first chunk only, coalescing the rest in the sink's buffer" $ do
            -- One explicit flush pushes the status, headers, and opening bytes out
            -- promptly (time to first byte); flushing every chunk would instead pay
            -- a socket send per upstream read at relay byte rates. The sink sends as
            -- its output buffer fills and flushes the tail at stream end, so later
            -- chunks need no explicit flush to arrive.
            src <- newSource ["alpha", "beta", "gamma"]
            flushes <- newIORef (0 :: Int)
            pumpBody (srcNext src) (const (pure ())) (modifyIORef' flushes (+ 1))
            readIORef flushes `shouldReturn` 1
        it "does not flush an empty body" $ do
            src <- newSource []
            flushes <- newIORef (0 :: Int)
            pumpBody (srcNext src) (const (pure ())) (modifyIORef' flushes (+ 1))
            readIORef flushes `shouldReturn` 0

    describe "streamUpstream -- end to end over an in-process upstream" $
        it "relays a large body through with the upstream status" $ do
            -- A 4 MiB body streamed from an in-process Warp upstream, through the
            -- proxy's streamUpstream, and pulled back by a real client. It must
            -- arrive intact via the full http-client wiring; the pump test above is
            -- the unit proof that this path never buffers the body whole.
            let bigBody = BS.replicate (4 * 1024 * 1024) 0x7a
            manager <- newManager defaultManagerSettings
            testWithApplication (pure (upstreamApp bigBody)) $ \upPort ->
                testWithApplication (pure (proxyApp manager upPort)) $ \proxyPort -> do
                    req <- parseRequest ("http://127.0.0.1:" <> show proxyPort <> "/")
                    resp <- httpLbs req manager
                    toStrict (responseBody resp) `shouldBe` bigBody

    describe "streamUpstreamWhen -- conditional relay (hit / miss / open-failure)" $ do
        it "relays the body AND the upstream content headers when the status passes accept" $ do
            -- On a passing status the body streams through and the relay forwards the
            -- upstream's content headers (the client verifies dist.integrity over the
            -- relayed bytes and headers). The hit is observable as the upstream body
            -- and its Content-Type reaching the real client.
            manager <- newManager defaultManagerSettings
            testWithApplication (pure headeredUpstream) $ \upPort ->
                testWithApplication (pure (conditionalProxy manager upPort)) $ \proxyPort -> do
                    req <- parseRequest ("http://127.0.0.1:" <> show proxyPort <> "/")
                    resp <- httpLbs req manager
                    toStrict (responseBody resp) `shouldBe` "the-bytes"
                    (snd <$> find ((== hContentType) . fst) (HTTP.responseHeaders resp))
                        `shouldBe` Just "application/octet-stream"

        it "returns a clean miss (the fall-through marker) when the status fails accept" $ do
            -- A 404 fails the success predicate: no response is committed, so the
            -- proxy falls through and answers its own marker -- proving the helper
            -- reported the recoverable miss rather than relaying the upstream body.
            manager <- newManager defaultManagerSettings
            testWithApplication (pure missingUpstream) $ \upPort ->
                testWithApplication (pure (conditionalProxy manager upPort)) $ \proxyPort -> do
                    req <- parseRequest ("http://127.0.0.1:" <> show proxyPort <> "/")
                    resp <- httpLbs req manager
                    responseBody resp `shouldBe` fellThroughMarker

        it "returns a clean miss when the upstream connection cannot be opened" $ do
            -- The upstream port is bound only long enough to learn a free port, then
            -- released, so opening the connection fails. That failure is the
            -- recoverable phase: the helper reports a miss (Nothing) and the proxy
            -- falls through, never committing a response from a connection it could
            -- not open.
            manager <- newManager defaultManagerSettings
            deadPort <- testWithApplication (pure missingUpstream) pure
            testWithApplication (pure (conditionalProxy manager deadPort)) $ \proxyPort -> do
                req <- parseRequest ("http://127.0.0.1:" <> show proxyPort <> "/")
                resp <- httpLbs req manager
                responseBody resp `shouldBe` fellThroughMarker

        it "relays an upstream 304 as a bodiless 304, forwarding its validator (the pass-through conditional relay)" $ do
            -- A 304 passes the artifact relay's accept predicate (a 2xx OR a 304), and
            -- is relayed straight back as a BODILESS 304 with the upstream's validator
            -- (ETag) forwarded -- never pumped as a streamed body. This is the cheap
            -- conditional-GET freshness check: the client gets a 304, not the artifact.
            manager <- newManager defaultManagerSettings
            testWithApplication (pure notModifiedUpstream) $ \upPort ->
                testWithApplication (pure (notModifiedProxy manager upPort)) $ \proxyPort -> do
                    req <- parseRequest ("http://127.0.0.1:" <> show proxyPort <> "/")
                    resp <- httpLbs req manager
                    statusCode (HTTP.responseStatus resp) `shouldBe` 304
                    responseBody resp `shouldBe` ""
                    (snd <$> find ((== hETag) . fst) (HTTP.responseHeaders resp))
                        `shouldBe` Just "\"v1\""

    describe "probeUpstreamWhen -- bodiless relay (HEAD, no pump)" $ do
        it "relays the upstream status and content headers with no body on a hit" $ do
            -- A HEAD probe through the helper: the client gets the upstream's status
            -- and content headers (here a Content-Type) but an EMPTY body -- the body is
            -- never pumped (the amplification a HEAD must never trigger).
            manager <- newManager defaultManagerSettings
            testWithApplication (pure headLengthUpstream) $ \upPort ->
                testWithApplication (pure (probeProxy manager upPort)) $ \proxyPort -> do
                    req <- parseRequest ("http://127.0.0.1:" <> show proxyPort <> "/")
                    resp <- httpLbs req manager
                    -- No body relayed (the pump never ran).
                    responseBody resp `shouldBe` ""
                    -- The content headers the matching GET would carry are relayed.
                    (snd <$> find ((== hContentType) . fst) (HTTP.responseHeaders resp))
                        `shouldBe` Just "application/octet-stream"

        it "returns a clean miss when the status fails accept" $ do
            -- A 404 fails the success predicate: no response is committed, so the proxy
            -- falls through to its marker rather than relaying anything.
            manager <- newManager defaultManagerSettings
            testWithApplication (pure missingUpstream) $ \upPort ->
                testWithApplication (pure (probeProxy manager upPort)) $ \proxyPort -> do
                    req <- parseRequest ("http://127.0.0.1:" <> show proxyPort <> "/")
                    resp <- httpLbs req manager
                    responseBody resp `shouldBe` fellThroughMarker

        it "returns a clean miss when the upstream connection cannot be opened" $ do
            -- The open is the recoverable phase here too: a failed connection is a
            -- miss, never a committed bodiless reply.
            manager <- newManager defaultManagerSettings
            deadPort <- testWithApplication (pure missingUpstream) pure
            testWithApplication (pure (probeProxy manager deadPort)) $ \proxyPort -> do
                req <- parseRequest ("http://127.0.0.1:" <> show proxyPort <> "/")
                resp <- httpLbs req manager
                responseBody resp `shouldBe` fellThroughMarker
  where
    -- An upstream that streams a fixed body back in 64 KiB chunks.
    upstreamApp :: ByteString -> Application
    upstreamApp body _req respond =
        respond (responseStream status200 [] (\write flush -> writeChunks write flush (chunk 65536 body)))

    -- The proxy: open the upstream and stream it through, relaying status+headers.
    proxyApp :: HTTP.Manager -> Int -> Application
    proxyApp manager upPort _req respond = do
        upReq <- parseRequest ("http://127.0.0.1:" <> show upPort <> "/")
        streamUpstream manager upReq (,) respond

    -- An upstream that answers 200 with a body and a content header to relay.
    headeredUpstream :: Application
    headeredUpstream _req respond =
        respond (responseLBS status200 [(hContentType, "application/octet-stream")] "the-bytes")

    -- An upstream that always 404s -- the conditional relay's recoverable miss.
    missingUpstream :: Application
    missingUpstream _req respond = respond (responseLBS status404 [] "not found")

    -- An upstream that answers a bodiless 304 with a validator (an ETag) -- the
    -- not-modified case of the pass-through conditional relay.
    notModifiedUpstream :: Application
    notModifiedUpstream _req respond =
        respond (responseLBS status304 [(hETag, "\"v1\"")] "")

    {- The artifact relay's conditional proxy: accept a 2xx OR a 304 (the predicate the
    serve path's artifact relay uses) and relay it through; a miss answers the
    fall-through marker. So an upstream 304 is observable as a relayed bodiless 304. -}
    notModifiedProxy :: HTTP.Manager -> Int -> Application
    notModifiedProxy manager upPort _req respond = do
        upReq <- parseRequest ("http://127.0.0.1:" <> show upPort <> "/")
        outcome <- streamUpstreamWhen manager upReq (\s -> statusIsSuccessful s || isNotModified s) (,) respond
        case outcome of
            Just received -> pure received
            Nothing -> respond (responseLBS status200 [] fellThroughMarker)

    {- An upstream that answers a content header (a Content-Type) with no body, as a
    HEAD reply does, so the probe relay is seen to forward the content metadata a GET
    would carry while pumping nothing. -}
    headLengthUpstream :: Application
    headLengthUpstream _req respond =
        respond (responseLBS status200 [(hContentType, "application/octet-stream")] "")

    {- The probe proxy under test: mark the upstream request a HEAD and relay it
    bodiless on a successful status, else answer the fall-through marker. So a hit is
    observable as the relayed headers with an empty body, a miss as the marker. -}
    probeProxy :: HTTP.Manager -> Int -> Application
    probeProxy manager upPort _req respond = do
        upReq <- parseRequest ("http://127.0.0.1:" <> show upPort <> "/")
        outcome <- probeUpstreamWhen manager upReq{HTTP.method = methodHead} statusIsSuccessful (,) respond
        case outcome of
            Just received -> pure received
            Nothing -> respond (responseLBS status200 [] fellThroughMarker)

    {- The proxy under test: relay only a successful upstream status, otherwise
    answer the fall-through marker. So a hit is observable as the upstream body, and
    a miss (rejected status, or a connection that could not be opened) as the marker. -}
    conditionalProxy :: HTTP.Manager -> Int -> Application
    conditionalProxy manager upPort _req respond = do
        upReq <- parseRequest ("http://127.0.0.1:" <> show upPort <> "/")
        outcome <- streamUpstreamWhen manager upReq statusIsSuccessful (,) respond
        case outcome of
            Just received -> pure received
            Nothing -> respond (responseLBS status200 [] fellThroughMarker)

    -- The body a proxy answers on a conditional-relay miss (no upstream relay
    -- occurred), distinct from any upstream body so a miss is unambiguous.
    fellThroughMarker :: LByteString
    fellThroughMarker = "FELL-THROUGH"

    writeChunks :: (Builder -> IO ()) -> IO () -> [ByteString] -> IO ()
    writeChunks _ _ [] = pure ()
    writeChunks write flush (c : cs) = write (byteString c) >> flush >> writeChunks write flush cs

    chunk :: Int -> ByteString -> [ByteString]
    chunk n bs
        | BS.null bs = []
        | otherwise = let (h, t) = BS.splitAt n bs in h : chunk n t
