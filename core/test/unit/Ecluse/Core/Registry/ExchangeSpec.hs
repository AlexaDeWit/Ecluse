-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Destructive requests are not replayed, digesting reads cover every consumed chunk, an exchange
below the progress floor in either direction fails with the transport timeout on a closed
connection, and only a serve-path exchange is capped.
-}
module Ecluse.Core.Registry.ExchangeSpec (spec) where

import Data.Aeson (Value (String))
import Data.ByteString qualified as BS
import Data.JsonStream.Parser qualified as J
import Data.Time (NominalDiffTime)
import GHC.Clock (getMonotonicTime)
import Network.HTTP.Client (Manager, Request (method, requestBody), RequestBody (RequestBodyBS), defaultManagerSettings, httpLbs, newManager, parseRequest)
import Network.HTTP.Types (status200)
import Network.Wai (Application, pathInfo, responseLBS, responseRaw)
import Network.Wai.Handler.Warp (defaultSettings, setOnOpen, testWithApplication, testWithApplicationSettings)
import Test.Hspec
import UnliftIO.Async (async, wait)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (tryAny)

import Ecluse.Core.Fault (TransportCause (TransportTimeout), TransportFault (tfCause))
import Ecluse.Core.Registry (BodyOutcome (SuccessBody), FetchFault (FetchTransport))
import Ecluse.Core.Registry.Exchange (boundedExchange, digestingRead, singleAttemptSettings, withSuccessBody, withinServeCap)
import Ecluse.Core.Registry.JsonStream (StreamResult (..), readJsonStream, retainedValue)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge), ProgressFloor, boundedRead, mkProgressFloor)
import Ecluse.Core.Server.Cache.Store (SingleFlight, newSingleFlightWithBackend, resolveSingleFlight)
import Ecluse.Core.Snapshot (ContentDigest)
import Ecluse.Test.Poll (awaitUntil)
import Ecluse.Test.Snapshot (digestOf)
import Ecluse.Test.Support (expectRightIO)
import Ecluse.Test.Wai (countingUpstream, pacedBody)

spec :: Spec
spec = do
    singleAttemptSpec
    digestingReadSpec
    floorSpec

singleAttemptSpec :: Spec
singleAttemptSpec = describe "singleAttemptSettings" $
    it "returns a lost destructive response without resending on a fresh connection" $ do
        requests <- newIORef (0 :: Int)
        let application _ respond = do
                attempt <- atomicModifyIORef' requests (\n -> (n + 1, n + 1))
                respond $
                    if attempt == 1
                        then responseLBS status200 [] "warm connection"
                        else responseRaw (\_ _ -> pure ()) (responseLBS status200 [] "")
        testWithApplication (pure application) $ \port -> do
            manager <- newManager (singleAttemptSettings defaultManagerSettings)
            request <- parseRequest ("http://127.0.0.1:" <> show port <> "/")
            void (httpLbs request manager)
            result <- tryAny (httpLbs request{method = "DELETE"} manager)
            result `shouldSatisfy` isLeft
            readIORef requests `shouldReturn` 2

digestingReadSpec :: Spec
digestingReadSpec = describe "digestingRead" $ do
    it "digests every chunk at every split, including bytes after the extracted value" $ do
        let body = "{\"keep\":\"yes\",\"ignored\":[1,2,3]} trailing bytes"
        forM_ [1 .. BS.length body - 1] $ \position -> do
            (streamed, digest) <- expectRightIO (readDigested (MetadataBodyLimit 1024) [BS.take position body, BS.drop position body])
            streamValue streamed `shouldBe` Right (Just (String "yes"))
            streamBytes streamed `shouldBe` BS.length body
            digest `shouldBe` digestOf body

    it "digests omitted fields even when the extracted value stays equal" $ do
        (firstRead, firstDigest) <- expectRightIO (readDigested (MetadataBodyLimit 1024) ["{\"keep\":\"yes\",\"ignored\":\"one\"}"])
        (secondRead, secondDigest) <- expectRightIO (readDigested (MetadataBodyLimit 1024) ["{\"keep\":\"yes\",\"ignored\":\"two\"}"])
        streamValue firstRead `shouldBe` streamValue secondRead
        firstDigest `shouldNotBe` secondDigest

    it "returns the consumer's refusal without a digest" $
        readDigested (MetadataBodyLimit 2) ["{}", "x"] `shouldReturn` Left (BodyTooLarge (MetadataBodyLimit 2))

readDigested :: BodyLimit -> [ByteString] -> IO (Either LimitError (StreamResult (Maybe Value), ContentDigest))
readDigested bound chunks = do
    remaining <- newIORef chunks
    let next = atomicModifyIORef' remaining $ \case
            [] -> ([], BS.empty)
            chunk : rest -> (rest, chunk)
    digestingRead (readJsonStream bound (J.objectWithKey "keep" (retainedValue 1)) (\_ value -> Right (Just value)) Nothing) next

-- Each case runs a real Warp stub, because the floor bounds transfers on a live socket. Every
-- bound on elapsed time leaves a wide margin for a loaded runner.
floorSpec :: Spec
floorSpec = describe "the progress floor and the serve-path cap" $ do
    it "fails a body that stops after its headers within about one window" $ do
        progress <- floorOf 30 0.5 1024
        (outcome, elapsed) <- timedExchange id progress (pacedBody [(0, "{\"a\":"), (stallMicros, "1}")])
        timeoutCause outcome `shouldBe` Just TransportTimeout
        elapsed `shouldSatisfy` between 0.5 10

    it "fails a trickle below the floor within about one window" $ do
        progress <- floorOf 30 1 65536
        -- 100 bytes every 50 ms: bytes keep arriving, at 2 KB a second against a 64 KiB floor.
        (outcome, elapsed) <- timedExchange id progress (pacedBody (replicate 400 (50_000, BS.replicate 100 0x61)))
        timeoutCause outcome `shouldBe` Just TransportTimeout
        elapsed `shouldSatisfy` between 1 10

    it "completes a healthy body above the floor that takes several windows, byte for byte" $ do
        progress <- floorOf 30 1 1000
        let body = steadyBody 16
        (outcome, elapsed) <- timedExchange id progress (pacedBody body)
        outcome `shouldBe` Right (mconcat (map snd body))
        elapsed `shouldSatisfy` (> 2)

    it "completes a worker-path transfer that outlives the serve cap, which the worker never takes" $ do
        progress <- floorOf 2 1 1000
        let body = steadyBody 24
        (outcome, elapsed) <- timedExchange id progress (pacedBody body)
        outcome `shouldBe` Right (mconcat (map snd body))
        elapsed `shouldSatisfy` (> 2)

    it "fails a serve-path exchange at the cap while its body still arrives above the floor" $ do
        progress <- floorOf 2 1 1000
        (outcome, elapsed) <- timedExchange (withinServeCap progress id) progress (pacedBody (steadyBody 80))
        timeoutCause outcome `shouldBe` Just TransportTimeout
        elapsed `shouldSatisfy` between 2 8

    it "fails an upload the target stops reading within about one window, with no cap" $ do
        progress <- floorOf 30 1 1048576
        -- The target never reads, so once the socket buffers fill the upload moves no bytes.
        let silentTarget _ respond = threadDelay stallMicros >> respond (responseLBS status200 [] "")
        withStub silentTarget $ \manager url -> do
            request <- parseRequest url
            let upload = request{method = "PUT", requestBody = RequestBodyBS (BS.replicate (32 * 1024 * 1024) 0x61)}
            started <- getMonotonicTime
            outcome <- exchangeBody manager progress upload
            finished <- getMonotonicTime
            timeoutCause outcome `shouldBe` Just TransportTimeout
            (finished - started) `shouldSatisfy` between 1 10

    it "closes a connection that missed the floor instead of returning it to the pool" $ do
        progress <- floorOf 30 0.5 65536
        opened <- newIORef (0 :: Int)
        let settings = setOnOpen (\_ -> atomicModifyIORef' opened (\n -> (n + 1, True))) defaultSettings
            application request = case pathInfo request of
                ["trickle"] -> pacedBody (replicate 400 (50_000, "x")) request
                _ -> pacedBody [(0, "{}")] request
        testWithApplicationSettings settings (pure application) $ \port -> do
            manager <- newManager defaultManagerSettings
            let fetch path = parseRequest ("http://127.0.0.1:" <> show port <> "/" <> path) >>= exchangeBody manager progress
            replicateM_ 2 (fetch "healthy" `shouldReturn` Right "{}")
            readIORef opened `shouldReturn` 1
            (timeoutCause <$> fetch "trickle") `shouldReturn` Just TransportTimeout
            readIORef opened `shouldReturn` 1
            fetch "healthy" `shouldReturn` Right "{}"
            readIORef opened `shouldReturn` 2

    it "hands single-flight followers the leader's fault without waiting out the stall" $ do
        progress <- floorOf 30 1 1024
        hits <- newIORef (0 :: Int)
        withStub (countingUpstream hits (pacedBody [(0, "{\"a\":"), (stallMicros, "1}")])) $ \manager url -> do
            flight <- newSingleFlightWithBackend Nothing :: IO (SingleFlight FetchFault Text ByteString)
            request <- parseRequest url
            let run = resolveSingleFlight (const pass) (const pass) pass flight "stalled" (exchangeBody manager progress request)
            started <- getMonotonicTime
            leader <- async run
            awaitUntil 5_000_000 10_000 ((> 0) <$> readIORef hits) `shouldReturn` True
            followers <- traverse (const (async run)) [1 .. 3 :: Int]
            outcomes <- traverse wait (leader : followers)
            finished <- getMonotonicTime
            map timeoutCause outcomes `shouldBe` replicate 4 (Just TransportTimeout)
            (finished - started) `shouldSatisfy` (< 10)
            readIORef hits `shouldReturn` 1

    it "does not count the consumer's own time between reads against the window" $ do
        progress <- floorOf 30 0.3 1_000_000
        let chunks = ["first", "second"]
        withStub (pacedBody [(0, chunk) | chunk <- chunks]) $ \manager url -> do
            request <- parseRequest url
            let slowConsumer readChunk = boundedRead (MetadataBodyLimit 1024) (threadDelay 500_000 >> readChunk)
            withSuccessBody manager progress slowConsumer request
                `shouldReturn` Right (SuccessBody 200 (BS.length (mconcat chunks), mconcat chunks))

-- A stall long enough that finishing before it proves the floor fired.
stallMicros :: Int
stallMicros = 20_000_000

-- 2 KB every 150 ms: each chunk alone clears a 1000-byte floor, far inside a 1 s window.
steadyBody :: Int -> [(Int, ByteString)]
steadyBody count = [(150_000, BS.replicate 2000 (0x61 + fromIntegral (i `mod` 26))) | i <- [0 .. count - 1]]

between :: Double -> Double -> Double -> Bool
between low high seconds = seconds >= low && seconds < high

-- A floor from a serve-path cap, a window, and a byte count.
floorOf :: NominalDiffTime -> NominalDiffTime -> Int -> IO ProgressFloor
floorOf serveCap window minBytes = either (fail . show) pure (mkProgressFloor serveCap window minBytes)

withStub :: Application -> (Manager -> String -> IO a) -> IO a
withStub application action =
    testWithApplication (pure application) $ \port -> do
        manager <- newManager defaultManagerSettings
        action manager ("http://127.0.0.1:" <> show port <> "/")

exchangeBody :: Manager -> ProgressFloor -> Request -> IO (Either FetchFault ByteString)
exchangeBody manager progress = boundedExchange (\_ _ body -> body) manager progress (MetadataBodyLimit 1_000_000)

-- One exchange against the stub, run through the given wrapper, with the seconds it took.
timedExchange ::
    (IO (Either FetchFault ByteString) -> IO (Either FetchFault ByteString)) ->
    ProgressFloor ->
    Application ->
    IO (Either FetchFault ByteString, Double)
timedExchange wrap progress application =
    withStub application $ \manager url -> do
        request <- parseRequest url
        started <- getMonotonicTime
        outcome <- wrap (exchangeBody manager progress request)
        finished <- getMonotonicTime
        pure (outcome, finished - started)

timeoutCause :: Either FetchFault a -> Maybe TransportCause
timeoutCause = \case
    Left (FetchTransport fault) -> Just (tfCause fault)
    _ -> Nothing
