-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The watchdog stops a transfer only after a window of waiting without the floor's bytes, in
either direction, never spins between waits, and a metered upload hands over the same bytes it
wraps under the same declared length.
-}
module Ecluse.Core.Registry.ProgressSpec (spec) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder (byteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Time (NominalDiffTime)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client (GivesPopper, RequestBody (..))
import System.CPUTime (getCPUTime)
import Test.Hspec
import UnliftIO.Async (async, wait)
import UnliftIO.Concurrent (threadDelay, yield)
import UnliftIO.Exception (tryAny)

import Ecluse.Core.Registry.Progress (meteredReader, meteredUpload, watched, watchedRaising)
import Ecluse.Core.Security (ProgressFloor, mkProgressFloor)
import Ecluse.Test.Poll (awaitUntil)

spec :: Spec
spec = do
    describe "watched -- the response body" $ do
        it "stops the transfer once a read waits a whole window" $ do
            progress <- floorOf 0.3 1024
            watched progress (\watch -> meteredReader watch (threadDelay 20_000_000 $> "late")) `shouldReturn` Nothing

        it "does not count the consumer's own time between reads" $ do
            progress <- floorOf 0.3 1_000_000
            chunks <- newIORef ["one", "two", "three", ""]
            let next = atomicModifyIORef' chunks (\case [] -> ([], ""); c : cs -> (cs, c))
            watched progress (\watch -> replicateM 4 (threadDelay 200_000 >> meteredReader watch next))
                `shouldReturn` Just ["one", "two", "three", ""]

    describe "watched -- the request body" $
        it "stops the transfer once handing slices over waits a whole window without the floor's bytes" $ do
            progress <- floorOf 0.3 1_000_000
            -- Each pause stands in for the connection taking that long to write the slice before.
            watched progress (\watch -> drainWith 200_000 (meteredUpload watch (RequestBodyBS (BS.replicate 400_000 0x61))))
                `shouldReturn` Nothing

    describe "watchedRaising" $
        it "aborts the transfer with an exception, for a response already committed" $ do
            progress <- floorOf 0.3 1024
            outcome <- tryAny (watchedRaising progress (\watch -> meteredReader watch (threadDelay 20_000_000 $> "late")))
            outcome `shouldSatisfy` isLeft

    describe "the watchdog between waits" $
        it "sleeps rather than spinning once a wait closes just short of the window" $ do
            -- 25 watchdogs each left 15 µs of budget. Spinning on it wakes each one about a
            -- thousand times a second. Sleeping a sixteenth of the window wakes each ten times.
            idleCpu <- idleCpuAfterNearMisses 25
            idleCpu `shouldSatisfy` (< 50)

    describe "meteredUpload" $ do
        let body = BS.pack [fromIntegral (i `mod` 256) | i <- [0 .. 199_999 :: Int]]
            size = fromIntegral (BS.length body)
        for_
            [ ("a strict body", RequestBodyBS body, Just size)
            , ("a lazy body", RequestBodyLBS (LBS.fromChunks [BS.take 70_000 body, BS.drop 70_000 body]), Just size)
            , ("a builder", RequestBodyBuilder size (byteString body), Just size)
            , ("a stream", RequestBodyStream size (givesOnce body), Just size)
            , ("a chunked stream", RequestBodyStreamChunked (givesOnce body), Nothing)
            , ("an IO body", RequestBodyIO (pure (RequestBodyBS body)), Just size)
            ]
            $ \(label, original, declared) ->
                it ("hands over the same bytes in slices of at most 64 KiB under the same declared length, for " <> label) $ do
                    progress <- floorOf 30 1
                    outcome <- watched progress $ \watch -> do
                        let metered = meteredUpload watch original
                        (,) <$> declaredLength metered <*> drainWith 0 metered
                    case outcome of
                        Nothing -> expectationFailure "the transfer fell below its floor"
                        Just (length', slices) -> do
                            length' `shouldBe` declared
                            mconcat slices `shouldBe` body
                            slices `shouldSatisfy` all ((<= 64 * 1024) . BS.length)

        it "leaves a body with no bytes as it is" $ do
            progress <- floorOf 30 1
            watched progress (\watch -> drainWith 0 (meteredUpload watch (RequestBodyLBS ""))) `shouldReturn` Just [""]

{- The process CPU milliseconds over an idle second, while this many transfers each hold a wait
open to 15 µs short of a 1.6 s window and then sit idle with no wait open. -}
idleCpuAfterNearMisses :: Int -> IO Integer
idleCpuAfterNearMisses count = do
    progress <- floorOf 1.6 1_000_000
    closed <- newIORef (0 :: Int)
    release <- newEmptyMVar
    transfers <- replicateM count . async . watched progress $ \watch -> do
        _ <- meteredReader watch (holdUntilShortOf 1_600_000 15 $> "x")
        atomicModifyIORef' closed (\n -> (n + 1, ()))
        readMVar release
    void (awaitUntil 10_000_000 10_000 ((== count) <$> readIORef closed))
    idleStart <- getCPUTime
    threadDelay 1_000_000
    idleEnd <- getCPUTime
    putMVar release ()
    traverse_ wait transfers
    pure ((idleEnd - idleStart) `div` 1_000_000_000)

-- Block until a span in microseconds is within a margin of passing: a timer for the bulk, a spin
-- for the rest, which a timer's resolution cannot reach.
holdUntilShortOf :: Word64 -> Word64 -> IO ()
holdUntilShortOf spanMicros marginMicros = do
    start <- getMonotonicTimeNSec
    threadDelay (fromIntegral (spanMicros - 20_000))
    spinUntil (start + (spanMicros - marginMicros) * 1_000)

spinUntil :: Word64 -> IO ()
spinUntil deadline = do
    now <- getMonotonicTimeNSec
    when (now < deadline) (yield >> spinUntil deadline)

declaredLength :: RequestBody -> IO (Maybe Int64)
declaredLength = \case
    RequestBodyStream size _ -> pure (Just size)
    RequestBodyIO io -> io >>= declaredLength
    _ -> pure Nothing

floorOf :: NominalDiffTime -> Int -> IO ProgressFloor
floorOf window minBytes = either (fail . show) pure (mkProgressFloor 60 window minBytes)

givesOnce :: ByteString -> GivesPopper ()
givesOnce bytes needsPopper = do
    remaining <- newIORef bytes
    needsPopper (atomicModifyIORef' remaining (BS.empty,))

-- Every slice a body hands over, pausing before each pull after the first.
drainWith :: Int -> RequestBody -> IO [ByteString]
drainWith pause = \case
    RequestBodyStream _ gives -> drained gives
    RequestBodyStreamChunked gives -> drained gives
    RequestBodyIO io -> io >>= drainWith pause
    RequestBodyLBS bytes -> pure [LBS.toStrict bytes]
    RequestBodyBS bytes -> pure [bytes]
    RequestBodyBuilder _ _ -> pure []
  where
    drained :: GivesPopper () -> IO [ByteString]
    drained gives = do
        collected <- newIORef []
        gives $ \popper ->
            let go opening = do
                    unless opening (threadDelay pause)
                    slice <- popper
                    unless (BS.null slice) (modifyIORef' collected (slice :) >> go False)
             in go True
        reverse <$> readIORef collected
