-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Destructive requests are not replayed, digesting reads cover every consumed chunk, and a
silent or endless upstream body fails with the transport timeout.
-}
module Ecluse.Core.Registry.ExchangeSpec (spec) where

import Data.Aeson (Value (String))
import Data.ByteString qualified as BS
import Data.ByteString.Builder (byteString)
import Data.JsonStream.Parser qualified as J
import Data.Time (NominalDiffTime)
import GHC.Clock (getMonotonicTime)
import Network.HTTP.Client (Manager, Request (method), defaultManagerSettings, httpLbs, newManager, parseRequest)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS, responseRaw, responseStream)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
import UnliftIO.Async (async, wait)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (tryAny)

import Ecluse.Core.Fault (TransportCause (TransportTimeout), TransportFault (tfCause))
import Ecluse.Core.Registry (BodyOutcome (SuccessBody), FetchFault (FetchTransport))
import Ecluse.Core.Registry.Exchange (boundedExchange, digestingRead, singleAttemptSettings, withSuccessBody)
import Ecluse.Core.Registry.JsonStream (StreamResult (..), readJsonStream, retainedValue)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), ExchangeDeadline, LimitError (BodyTooLarge), boundedRead, mkExchangeDeadline)
import Ecluse.Core.Server.Cache.Store (SingleFlight, newSingleFlightWithBackend, resolveSingleFlight)
import Ecluse.Core.Snapshot (ContentDigest)
import Ecluse.Test.Snapshot (digestOf)
import Ecluse.Test.Support (expectRightIO)

spec :: Spec
spec = do
    singleAttemptSpec
    digestingReadSpec
    deadlineSpec

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

-- Each case runs a real Warp stub, because the deadlines bound reads on a live socket.
deadlineSpec :: Spec
deadlineSpec = describe "exchange deadlines" $ do
    it "fails a body that stalls after its headers with the transport timeout, inside the idle interval" $ do
        deadline <- deadlineOf 30 0.3
        (outcome, elapsed) <- timedExchange deadline (paced [(0, "{\"a\":"), (stallMicros, "1}")])
        timeoutCause outcome `shouldBe` Just TransportTimeout
        elapsed `shouldSatisfy` (\seconds -> seconds >= 0.3 && seconds < 10)

    it "hands single-flight followers the leader's timeout without waiting out the stall" $ do
        deadline <- deadlineOf 30 1
        hits <- newIORef (0 :: Int)
        withStub (counted hits (paced [(0, "{\"a\":"), (stallMicros, "1}")])) $ \manager url -> do
            flight <- newSingleFlightWithBackend Nothing :: IO (SingleFlight FetchFault Text ByteString)
            request <- parseRequest url
            let run = resolveSingleFlight (const pass) (const pass) pass flight "stalled" (exchangeBody manager deadline request)
            started <- getMonotonicTime
            leader <- async run
            waitForHit hits
            followers <- traverse (const (async run)) [1 .. 3 :: Int]
            outcomes <- traverse wait (leader : followers)
            finished <- getMonotonicTime
            map timeoutCause outcomes `shouldBe` replicate 4 (Just TransportTimeout)
            (finished - started) `shouldSatisfy` (< 10)
            readIORef hits `shouldReturn` 1

    it "completes a body that trickles inside the idle interval, byte for byte" $ do
        deadline <- deadlineOf 30 0.5
        let chunks = ["{\"versions\":", "[1,", "2,", "3]", "}"]
        (outcome, _) <- timedExchange deadline (paced [(100_000, chunk) | chunk <- chunks])
        outcome `shouldBe` Right (mconcat chunks)

    it "does not count the consumer's own time between reads as silence" $ do
        deadline <- deadlineOf 30 0.3
        let chunks = ["first", "second"]
        withStub (paced [(0, chunk) | chunk <- chunks]) $ \manager url -> do
            request <- parseRequest url
            let slowConsumer readChunk = boundedRead (MetadataBodyLimit 1024) (threadDelay 500_000 >> readChunk)
            withSuccessBody manager deadline slowConsumer request
                `shouldReturn` Right (SuccessBody 200 (BS.length (mconcat chunks), mconcat chunks))

    it "caps a body that keeps trickling past the whole-exchange deadline" $ do
        deadline <- deadlineOf 1.5 0.5
        (outcome, elapsed) <- timedExchange deadline (paced (replicate 100 (100_000, "x")))
        timeoutCause outcome `shouldBe` Just TransportTimeout
        elapsed `shouldSatisfy` (\seconds -> seconds >= 1 && seconds < 5)

-- A stall long enough that finishing before it proves a deadline fired.
stallMicros :: Int
stallMicros = 20_000_000

deadlineOf :: NominalDiffTime -> NominalDiffTime -> IO ExchangeDeadline
deadlineOf requestTimeout idle =
    maybe (fail "the test deadline must be positive and below its request timeout") pure (mkExchangeDeadline requestTimeout idle)

-- A 200 whose body arrives as chunks, each after its own pause. The first pause follows the headers.
paced :: [(Int, ByteString)] -> Application
paced chunks _ respond =
    respond . responseStream status200 [] $ \write flush ->
        for_ chunks $ \(pause, chunk) -> threadDelay pause >> write (byteString chunk) >> flush

counted :: IORef Int -> Application -> Application
counted hits application request respond =
    atomicModifyIORef' hits (\n -> (n + 1, ())) >> application request respond

waitForHit :: IORef Int -> IO ()
waitForHit hits = do
    seen <- readIORef hits
    when (seen == 0) (threadDelay 10_000 >> waitForHit hits)

withStub :: Application -> (Manager -> String -> IO a) -> IO a
withStub application action =
    testWithApplication (pure application) $ \port -> do
        manager <- newManager defaultManagerSettings
        action manager ("http://127.0.0.1:" <> show port <> "/")

exchangeBody :: Manager -> ExchangeDeadline -> Request -> IO (Either FetchFault ByteString)
exchangeBody manager deadline = boundedExchange (\_ _ body -> body) manager deadline (MetadataBodyLimit 1_000_000)

-- One exchange against the stub, with the seconds it took.
timedExchange :: ExchangeDeadline -> Application -> IO (Either FetchFault ByteString, Double)
timedExchange deadline application =
    withStub application $ \manager url -> do
        request <- parseRequest url
        started <- getMonotonicTime
        outcome <- exchangeBody manager deadline request
        finished <- getMonotonicTime
        pure (outcome, finished - started)

timeoutCause :: Either FetchFault a -> Maybe TransportCause
timeoutCause = \case
    Left (FetchTransport fault) -> Just (tfCause fault)
    _ -> Nothing
