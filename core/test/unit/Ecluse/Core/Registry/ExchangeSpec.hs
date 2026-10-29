-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Destructive requests are not replayed, digesting reads cover every consumed chunk, an exchange
below the progress floor in either direction fails with the transport timeout on a closed
connection, and only a serve-path exchange is capped.
-}
module Ecluse.Core.Registry.ExchangeSpec (spec) where

import Crypto.Hash (hashInit, hashUpdates)
import Data.Aeson (Value (String))
import Data.ByteString qualified as BS
import Data.JsonStream.Parser qualified as J
import Data.Text qualified as T
import Data.Time (NominalDiffTime)
import GHC.Clock (getMonotonicTime)
import Hedgehog (Gen, cover, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Network.HTTP.Client (Request (method, requestBody), RequestBody (RequestBodyBS, RequestBodyStreamChunked), defaultManagerSettings, httpLbs, newManager, parseRequest)
import Network.HTTP.Types (status200)
import Network.Wai (Application, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, setOnOpen, testWithApplication, testWithApplicationSettings)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)
import UnliftIO.Async (async, wait)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Fault (TransportCause (TransportTimeout, TransportUnreachable), TransportFault (tfCause, tfDetail))
import Ecluse.Core.Registry (BodyOutcome (SuccessBody), FetchFault (FetchTransport))
import Ecluse.Core.Registry.Exchange (accumulatingCharge, accumulatingChargeFrom, boundedChargedRead, boundedExchange, boundedIncrementalRead, digestingRead, newSingleAttemptManager, singleAttemptSettings, withSuccessBody, withinServeCap)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient (ecDeadlines))
import Ecluse.Core.Registry.JsonStream (StreamResult (..), readJsonStream, retainedValue)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge), ProgressFloor, boundedRead, mkProgressFloor)
import Ecluse.Core.Server.Cache.Store (SingleFlight, newSingleFlightWithBackend, resolveSingleFlight)
import Ecluse.Core.Server.MemoryModel (bufferedChunkCharge)
import Ecluse.Core.Snapshot (ContentDigest, digestFromContext)
import Ecluse.Test.Deadline (withExchangeClient)
import Ecluse.Test.Poll (awaitUntil)
import Ecluse.Test.Registry.JsonStream (heldChunks)
import Ecluse.Test.Snapshot (digestOf)
import Ecluse.Test.Stub.Retry (requestsAfterLostAnswer)
import Ecluse.Test.Stub.Socket (resetAfterPartialBody, trustingTlsManager, withResetAfterHeadThenFull, withTlsServer)
import Ecluse.Test.Support (between, expectRightIO)
import Ecluse.Test.Wai (countingUpstream, pacedBody)

spec :: Spec
spec = do
    chargeSpec
    singleAttemptSpec
    digestingReadSpec
    floorSpec
    bodyResetSpec

chargeSpec :: Spec
chargeSpec = describe "cumulative charging" $ do
    it "retains the seed and charges zero additions" $ do
        paid <- newIORef []
        (add, total) <- accumulatingChargeFrom 19 (\n -> modifyIORef' paid (<> [n]))
        readIORef paid `shouldReturn` []
        mapM_ add [0, 3, 5, 0]
        readIORef paid `shouldReturn` [19, 22, 27, 27]
        total `shouldReturn` 27

    it "saturates additions beyond Int without wrapping" $ do
        paid <- newIORef []
        (add, total) <- accumulatingChargeFrom (toInteger (maxBound :: Int) - 1) (\n -> modifyIORef' paid (<> [n]))
        mapM_ add [2, maxBound]
        readIORef paid `shouldReturn` [maxBound, maxBound]
        total `shouldReturn` maxBound

    it "retains the addition when the charge callback fails" $ do
        (add, total) <- accumulatingChargeFrom 7 (const (throwIO ChargeRefused))
        add 4 `shouldThrow` (== ChargeRefused)
        total `shouldReturn` 11

    it "keeps the zero-seeded wrapper's nonpositive suppression" $ do
        paid <- newIORef []
        add <- accumulatingCharge (\n -> modifyIORef' paid (<> [n]))
        mapM_ add [0, -3, 4, 0, 2]
        readIORef paid `shouldReturn` [4, 6]

    it "pays buffered chunks as running totals or as increments, by reader" $ do
        let three = bufferedChunkCharge 3
            five = bufferedChunkCharge 5
        for_ [(boundedChargedRead, [three, three + five]), (boundedIncrementalRead, [three, five])] $ \(boundedReader, expected) -> do
            paid <- newIORef []
            next <- heldChunks ["abc", "defgh"]
            boundedReader (\n -> modifyIORef' paid (<> [n])) (MetadataBodyLimit 64) next `shouldReturn` Right (8, "abcdefgh")
            readIORef paid `shouldReturn` expected

data ChargeRefused = ChargeRefused
    deriving stock (Eq, Show)

instance Exception ChargeRefused

singleAttemptSpec :: Spec
singleAttemptSpec = do
    describe "singleAttemptSettings" $
        it "returns a lost destructive response without resending on a fresh connection" $
            (newManager (singleAttemptSettings defaultManagerSettings) >>= requestsAfterLostAnswer) `shouldReturn` 2

    describe "newSingleAttemptManager" $
        it "returns a lost destructive response over the TLS settings without resending on a fresh connection" $
            (newSingleAttemptManager >>= requestsAfterLostAnswer) `shouldReturn` 2

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

    describe "properties" $
        modifyMaxSuccess (const 500) . it "digests the chunks the consumer read as the pure SHA-256 fold does, empty chunks included" $
            hedgehog $ do
                chunks <- forAll (Gen.list (Range.linear 0 24) genChunk)
                count <- forAll (Gen.int (Range.constant 0 (length chunks)))
                let consumed = take count chunks
                cover 20 "an empty chunk is read" (any BS.null consumed)
                cover 20 "a chunk is left unread" (count < length chunks)
                digest <- liftIO (digestOfReads count chunks)
                digest === digestFromContext (hashUpdates hashInit consumed)

-- Mostly chunks that end inside a 64-byte SHA-256 block, some empty, a few across many blocks.
genChunk :: Gen ByteString
genChunk = Gen.frequency [(2, pure BS.empty), (6, Gen.bytes (Range.linear 1 200)), (1, Gen.bytes (Range.exponential 1 4096))]

-- The digest of a consumer that reads exactly the first @count@ chunks.
digestOfReads :: Int -> [ByteString] -> IO ContentDigest
digestOfReads count chunks = do
    next <- heldChunks chunks
    digestingRead (\readChunk -> Right @Void () <$ replicateM_ count readChunk) next >>= either absurd (pure . snd)

readDigested :: BodyLimit -> [ByteString] -> IO (Either LimitError (StreamResult (Maybe Value), ContentDigest))
readDigested bound chunks = do
    next <- heldChunks chunks
    digestingRead (readJsonStream bound (J.objectWithKey "keep" (retainedValue 1)) (\_ value -> Right (Just value)) Nothing) next

-- Each case runs a real Warp stub, because the floor bounds transfers on a live socket. Every
-- bound on elapsed time leaves a wide margin for a loaded runner.
floorSpec :: Spec
floorSpec = describe "the progress floor and the serve-path cap" $ do
    it "fails a body that stops after its headers within about one window" $ do
        progress <- floorOf 30 0.5 1024
        (outcome, elapsed) <- timedExchange (const id) progress (pacedBody [(0, "{\"a\":"), (stallMicros, "1}")])
        transportCause outcome `shouldBe` Just TransportTimeout
        elapsed `shouldSatisfy` between 0.5 10

    it "fails a trickle below the floor within about one window" $ do
        progress <- floorOf 30 1 65536
        -- 100 bytes every 50 ms: bytes keep arriving, at 2 KB a second against a 64 KiB floor.
        (outcome, elapsed) <- timedExchange (const id) progress (pacedBody (replicate 400 (50_000, BS.replicate 100 0x61)))
        transportCause outcome `shouldBe` Just TransportTimeout
        faultDetail outcome `shouldBe` Just "the exchange moved fewer than 65536 body bytes, counting both directions together, in a 1-second progress window"
        elapsed `shouldSatisfy` between 1 10

    it "completes a healthy body above the floor that takes several windows, byte for byte" $ do
        progress <- floorOf 30 1 1000
        let body = steadyBody 16
        (outcome, elapsed) <- timedExchange (const id) progress (pacedBody body)
        outcome `shouldBe` Right (mconcat (map snd body))
        elapsed `shouldSatisfy` (> 2)

    it "completes a worker-path transfer that outlives the serve cap, which the worker never takes" $ do
        progress <- floorOf 2 1 1000
        let body = steadyBody 24
        (outcome, elapsed) <- timedExchange (const id) progress (pacedBody body)
        outcome `shouldBe` Right (mconcat (map snd body))
        elapsed `shouldSatisfy` (> 2)

    it "fails a serve-path exchange at the cap while its body still arrives above the floor" $ do
        progress <- floorOf 2 1 1000
        (outcome, elapsed) <- timedExchange (\deadlines -> withinServeCap deadlines progress id) progress (pacedBody (steadyBody 80))
        transportCause outcome `shouldBe` Just TransportTimeout
        faultDetail outcome `shouldBe` Just "the upstream exchange outlived its 2-second serve-path cap"
        elapsed `shouldSatisfy` between 2 8

    it "fails an upload the target stops reading within about one window, with no cap" $ do
        progress <- floorOf 30 1 1048576
        -- The target never reads, so once the socket buffers fill the upload moves no bytes.
        let silentTarget _ respond = threadDelay stallMicros >> respond (responseLBS status200 [] "")
        withStub silentTarget $ \client url -> do
            request <- parseRequest url
            let upload = request{method = "PUT", requestBody = RequestBodyBS (BS.replicate (32 * 1024 * 1024) 0x61)}
            started <- getMonotonicTime
            outcome <- exchangeBody client progress upload
            finished <- getMonotonicTime
            transportCause outcome `shouldBe` Just TransportTimeout
            (finished - started) `shouldSatisfy` between 1 10

    it "ends a retry's stalled reconnect after a failed upload slice with the floor, which no wait bounds" $ do
        progress <- floorOf 30 0.5 1024
        reset <- newEmptyMVar
        withResetAfterHeadThenFull reset $ \port -> do
            manager <- newManager defaultManagerSettings
            request <- parseRequest ("http://127.0.0.1:" <> show port <> "/")
            void (httpLbs request manager)
            withExchangeClient (pure manager) $ \client -> do
                -- The slice waits for the reset to land, so the slice fails to send and the head did not.
                let afterReset needsPopper = do
                        sent <- newIORef False
                        needsPopper $ readIORef sent >>= bool (readMVar reset >> threadDelay 200_000 >> writeIORef sent True $> "data") (pure BS.empty)
                    upload = request{method = "PUT", requestBody = RequestBodyStreamChunked afterReset}
                started <- getMonotonicTime
                outcome <- exchangeBody client progress upload
                finished <- getMonotonicTime
                transportCause outcome `shouldBe` Just TransportTimeout
                faultDetail outcome `shouldSatisfy` maybe False ("progress window" `T.isInfixOf`)
                (finished - started) `shouldSatisfy` between 0.5 8

    it "closes a connection that missed the floor instead of returning it to the pool" $ do
        progress <- floorOf 30 0.5 65536
        opened <- newIORef (0 :: Int)
        let settings = setOnOpen (\_ -> atomicModifyIORef' opened (\n -> (n + 1, True))) defaultSettings
            application request = case pathInfo request of
                ["trickle"] -> pacedBody (replicate 400 (50_000, "x")) request
                _ -> pacedBody [(0, "{}")] request
        testWithApplicationSettings settings (pure application) $ \port ->
            withExchangeClient (newManager defaultManagerSettings) $ \client -> do
                let fetch path = parseRequest ("http://127.0.0.1:" <> show port <> "/" <> path) >>= exchangeBody client progress
                replicateM_ 2 (fetch "healthy" `shouldReturn` Right "{}")
                readIORef opened `shouldReturn` 1
                (transportCause <$> fetch "trickle") `shouldReturn` Just TransportTimeout
                readIORef opened `shouldReturn` 1
                fetch "healthy" `shouldReturn` Right "{}"
                readIORef opened `shouldReturn` 2

    it "hands single-flight followers the leader's fault without waiting out the stall" $ do
        -- A 3 s window leaves a slow runner time to join every follower before the leader fails.
        progress <- floorOf 30 3 1024
        hits <- newIORef (0 :: Int)
        withStub (countingUpstream hits (pacedBody [(0, "{\"a\":"), (stallMicros, "1}")])) $ \client url -> do
            flight <- newSingleFlightWithBackend Nothing :: IO (SingleFlight FetchFault Text ByteString)
            request <- parseRequest url
            let run = resolveSingleFlight (const pass) (const pass) pass flight "stalled" (exchangeBody client progress request)
            started <- getMonotonicTime
            leader <- async run
            awaitUntil 5_000_000 10_000 ((> 0) <$> readIORef hits) `shouldReturn` True
            followers <- traverse (const (async run)) [1 .. 3 :: Int]
            outcomes <- traverse wait (leader : followers)
            finished <- getMonotonicTime
            map transportCause outcomes `shouldBe` replicate 4 (Just TransportTimeout)
            (finished - started) `shouldSatisfy` (< 10)
            readIORef hits `shouldReturn` 1

    it "does not count the consumer's own time between reads against the window" $ do
        progress <- floorOf 30 0.3 1_000_000
        let chunks = ["first", "second"]
        withStub (pacedBody [(0, chunk) | chunk <- chunks]) $ \client url -> do
            request <- parseRequest url
            let slowConsumer readChunk = boundedRead (MetadataBodyLimit 1024) (threadDelay 500_000 >> readChunk)
            withSuccessBody client progress slowConsumer request
                `shouldReturn` Right (SuccessBody 200 (BS.length (mconcat chunks), mconcat chunks))

bodyResetSpec :: Spec
bodyResetSpec = describe "a TLS body the peer resets mid-read" $
    it "is an unreachable transport fault after the 200 arrived, at the host:port that served it" $
        withTlsServer resetAfterPartialBody $ \port ->
            withExchangeClient trustingTlsManager $ \client -> do
                progress <- floorOf 30 1 1
                bodyStarted <- newIORef False
                let consume readChunk = writeIORef bodyStarted True >> boundedRead (MetadataBodyLimit 1024) readChunk
                outcome <- withSuccessBody client progress consume =<< parseRequest ("https://127.0.0.1:" <> show port <> "/")
                readIORef bodyStarted `shouldReturn` True
                transportCause outcome `shouldBe` Just TransportUnreachable
                faultDetail outcome `shouldSatisfy` maybe False (T.isPrefixOf ("127.0.0.1:" <> show port <> ": "))

-- A stall long enough that finishing before it proves the floor fired.
stallMicros :: Int
stallMicros = 20_000_000

-- 2 KB every 150 ms: each chunk alone clears a 1000-byte floor, far inside a 1 s window.
steadyBody :: Int -> [(Int, ByteString)]
steadyBody count = [(150_000, BS.replicate 2000 (0x61 + fromIntegral (i `mod` 26))) | i <- [0 .. count - 1]]

-- A floor from a serve-path cap, a window, and a byte count.
floorOf :: NominalDiffTime -> NominalDiffTime -> Int -> IO ProgressFloor
floorOf serveCap window minBytes = either (fail . show) pure (mkProgressFloor serveCap window minBytes)

withStub :: Application -> (ExchangeClient -> String -> IO a) -> IO a
withStub application action =
    testWithApplication (pure application) $ \port -> do
        withExchangeClient (newManager defaultManagerSettings) (\client -> action client ("http://127.0.0.1:" <> show port <> "/"))

exchangeBody :: ExchangeClient -> ProgressFloor -> Request -> IO (Either FetchFault ByteString)
exchangeBody client progress = boundedExchange (\_ _ body -> body) client progress (MetadataBodyLimit 1_000_000)

-- One exchange against the stub, run through the given wrapper, with the seconds it took.
timedExchange ::
    (Deadlines -> IO (Either FetchFault ByteString) -> IO (Either FetchFault ByteString)) ->
    ProgressFloor ->
    Application ->
    IO (Either FetchFault ByteString, Double)
timedExchange wrap progress application =
    withStub application $ \client url -> do
        request <- parseRequest url
        started <- getMonotonicTime
        outcome <- wrap (ecDeadlines client) (exchangeBody client progress request)
        finished <- getMonotonicTime
        pure (outcome, finished - started)

transportCause :: Either FetchFault a -> Maybe TransportCause
transportCause = \case
    Left (FetchTransport fault) -> Just (tfCause fault)
    _ -> Nothing

faultDetail :: Either FetchFault a -> Maybe Text
faultDetail = \case
    Left (FetchTransport fault) -> Just (tfDetail fault)
    _ -> Nothing
