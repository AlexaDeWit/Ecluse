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
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseStream)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
import UnliftIO (concurrently)

import Ecluse.Server.Stream (pumpBody, streamUpstream)

{- | A chunk source over a fixed list of chunks: each pull returns the next chunk
and an empty 'ByteString' once exhausted (the @http-client@ @BodyReader@
contract). It records the high-water mark of chunks produced-but-not-yet-consumed
— the residency the backpressure assertion turns on.
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
    describe "pumpBody — constant memory and backpressure" $ do
        it "holds at most one chunk in flight regardless of body size (constant memory)" $ do
            -- A 256-chunk body through a synchronous rendezvous: each 'write' blocks
            -- until the consumer has taken the chunk AND acked, so the handoff is
            -- fully complete before the pump loops back to read the next chunk. The
            -- producer therefore can never run ahead — the outstanding high-water
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

    describe "streamUpstream — end to end over an in-process upstream" $
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

    writeChunks :: (Builder -> IO ()) -> IO () -> [ByteString] -> IO ()
    writeChunks _ _ [] = pure ()
    writeChunks write flush (c : cs) = write (byteString c) >> flush >> writeChunks write flush cs

    chunk :: Int -> ByteString -> [ByteString]
    chunk n bs
        | BS.null bs = []
        | otherwise = let (h, t) = BS.splitAt n bs in h : chunk n t
