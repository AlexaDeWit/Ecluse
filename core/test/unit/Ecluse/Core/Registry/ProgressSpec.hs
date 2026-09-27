-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The watchdog raises in the transfer's thread only after a window of waiting without the
floor's bytes, in either direction, and a metered upload hands over the same bytes it wraps.
-}
module Ecluse.Core.Registry.ProgressSpec (spec) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder (byteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Time (NominalDiffTime)
import Network.HTTP.Client (GivesPopper, RequestBody (..))
import Test.Hspec
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (try)

import Ecluse.Core.Registry.Progress (BelowProgressFloor (BelowProgressFloor), meteredReader, meteredUpload, watched)
import Ecluse.Core.Security (ProgressFloor, mkProgressFloor)

spec :: Spec
spec = do
    describe "watched -- the response body" $ do
        it "raises in the transfer once a read waits a whole window" $ do
            progress <- floorOf 0.3 1024
            outcome <- try (watched progress (\watch -> meteredReader watch (threadDelay 20_000_000 $> "late")))
            outcome `shouldBe` Left BelowProgressFloor

        it "does not count the consumer's own time between reads" $ do
            progress <- floorOf 0.3 1_000_000
            chunks <- newIORef ["one", "two", "three", ""]
            let next = atomicModifyIORef' chunks (\case [] -> ([], ""); c : cs -> (cs, c))
            watched progress (\watch -> replicateM 4 (threadDelay 200_000 >> meteredReader watch next))
                `shouldReturn` ["one", "two", "three", ""]

    describe "watched -- the request body" $
        it "raises once handing slices over waits a whole window without the floor's bytes" $ do
            progress <- floorOf 0.3 1_000_000
            -- Each pause stands in for the connection taking that long to write the slice before.
            outcome <- try (watched progress (\watch -> drainWith 200_000 (meteredUpload watch (RequestBodyBS (BS.replicate 400_000 0x61)))))
            outcome `shouldBe` Left BelowProgressFloor

    describe "meteredUpload" $ do
        let body = BS.pack [fromIntegral (i `mod` 256) | i <- [0 .. 199_999 :: Int]]
        for_
            [ ("a strict body", RequestBodyBS body)
            , ("a lazy body", RequestBodyLBS (LBS.fromChunks [BS.take 70_000 body, BS.drop 70_000 body]))
            , ("a builder", RequestBodyBuilder (fromIntegral (BS.length body)) (byteString body))
            , ("a stream", RequestBodyStream (fromIntegral (BS.length body)) (givesOnce body))
            , ("a chunked stream", RequestBodyStreamChunked (givesOnce body))
            , ("an IO body", RequestBodyIO (pure (RequestBodyBS body)))
            ]
            $ \(label, original) ->
                it ("hands over the same bytes in slices of at most 64 KiB, for " <> label) $ do
                    progress <- floorOf 30 1
                    slices <- watched progress (\watch -> drainWith 0 (meteredUpload watch original))
                    mconcat slices `shouldBe` body
                    slices `shouldSatisfy` all ((<= 64 * 1024) . BS.length)

        it "leaves a body with no bytes as it is" $ do
            progress <- floorOf 30 1
            slices <- watched progress (\watch -> drainWith 0 (meteredUpload watch (RequestBodyLBS "")))
            slices `shouldBe` [""]

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
