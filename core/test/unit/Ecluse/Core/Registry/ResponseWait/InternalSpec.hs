-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The response wait gives the connection the whole wait and the headers what is left, counted
from the end of the upload, and fails each phase with the fault http-client's own timer gives.
-}
module Ecluse.Core.Registry.ResponseWait.InternalSpec (spec) where

import Data.ByteString qualified as BS
import GHC.Clock (getMonotonicTime)
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Network.HTTP.Client (
    BodyReader,
    HttpException (HttpExceptionRequest),
    HttpExceptionContent,
    Manager,
    Request (method, requestBody, responseTimeout),
    RequestBody (RequestBodyStream, RequestBodyStreamChunked),
    Response,
    brConsume,
    defaultManagerSettings,
    newManager,
    parseRequest,
    responseBody,
    responseClose,
    responseOpen,
    responseStatus,
    responseTimeoutMicro,
 )
import Network.HTTP.Types (status200, statusCode)
import Network.Wai (Application, getRequestBodyChunk, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, setOnOpen, testWithApplication, testWithApplicationSettings)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (bracket, try)

import Ecluse.Core.Deadline (newDeadlines)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient (ExchangeClient))
import Ecluse.Core.Registry.ResponseWait.Internal (ResponseWait (..), connectedAt, openResponseAfter, uploadedAt, waitFault)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Stub.Socket (trustingTlsManager, withRawServer)

spec :: Spec
spec = do
    transitionSpec
    openSpec

transitionSpec :: Spec
transitionSpec = describe "the transitions" $ do
    it "leaves the headers what the connection left of the wait" $
        hedgehog $ do
            deadline <- forAll (Gen.word64 (Range.linear 1 60_000_000_000))
            now <- forAll (Gen.word64 (Range.linear 0 (deadline - 1)))
            first named (connectedAt 30 now (Connecting deadline)) === Right (deadline - now)

    it "fails a connection made at or after its deadline with the connect timeout" $ do
        first named (connectedAt 30 50 (Connecting 50)) `shouldBe` Left "ConnectionTimeout"
        first named (connectedAt 30 51 (Connecting 50)) `shouldBe` Left "ConnectionTimeout"

    it "starts the whole wait again on a second connection, as http-client's retry does" $ do
        first named (connectedAt 30 900 (Uploading 7)) `shouldBe` Right 30
        first named (connectedAt 30 900 (Answering 12)) `shouldBe` Right 30

    it "starts the headers' deadline at the end of the upload" $ do
        uploadedAt 100 (Uploading 30) `shouldBe` Answering 130
        uploadedAt 100 (Answering 40) `shouldBe` Answering 40

    it "names the timeout of the phase that ran out, and none before its deadline" $ do
        named <$> waitFault 50 (Connecting 50) `shouldBe` Just "ConnectionTimeout"
        named <$> waitFault 50 (Answering 50) `shouldBe` Just "ResponseTimeout"
        named <$> waitFault 49 (Connecting 50) `shouldBe` Nothing
        named <$> waitFault 49 (Answering 50) `shouldBe` Nothing
        named <$> waitFault maxBound (Uploading 1) `shouldBe` Nothing

-- Each case runs a real loopback peer, with a one or two second wait in place of the 30 s one. Every
-- bound on elapsed time leaves a wide margin for a loaded runner.
openSpec :: Spec
openSpec = describe "openResponseAfter" $ do
    it "opens a response that answers within the wait" $
        testWithApplication (pure (\_ respond -> respond (responseLBS status200 [] "ok"))) $ \port -> do
            manager <- newManager defaultManagerSettings
            (outcome, _) <- opened 1 manager =<< parseRequest ("http://127.0.0.1:" <> show port <> "/")
            outcome `shouldBe` Right 200

    it "fails a response whose headers never come with the response timeout, as http-client's own timer does" $
        testWithApplication (pure (\_ respond -> threadDelay 20_000_000 >> respond (responseLBS status200 [] ""))) $ \port -> do
            manager <- newManager defaultManagerSettings
            request <- parseRequest ("http://127.0.0.1:" <> show port <> "/")
            (outcome, elapsed) <- opened 1 manager request
            outcome `shouldBe` Left "ResponseTimeout"
            elapsed `shouldSatisfy` between 1 4
            fst <$> openedByClient manager request `shouldReturn` outcome

    it "fails a connection whose handshake never completes with the connect timeout, as http-client's own timer does" $
        withRawServer (\_ -> threadDelay 20_000_000) $ \port -> do
            manager <- trustingTlsManager
            request <- parseRequest ("https://127.0.0.1:" <> show port <> "/")
            (outcome, elapsed) <- opened 1 manager request
            outcome `shouldBe` Left "ConnectionTimeout"
            elapsed `shouldSatisfy` between 1 4
            fst <$> openedByClient manager request `shouldReturn` outcome

    it "counts the wait for the headers from the end of an upload longer than the wait" $
        testWithApplication (pure (readingAt 16_000_000)) $ \port -> do
            manager <- newManager defaultManagerSettings
            request <- parseRequest ("http://127.0.0.1:" <> show port <> "/")
            -- 64 MiB at 16 MB a second outlasts the socket buffers by seconds.
            let chunk = BS.replicate (64 * 1024) 0x61
                upload = request{method = "PUT", requestBody = RequestBodyStream (64 * 1024 * 1024) (givesTimes 1024 chunk)}
            (outcome, elapsed) <- opened 2 manager upload
            outcome `shouldBe` Right 200
            elapsed `shouldSatisfy` (> 2)

    it "counts the wait for the headers from the end of a chunked upload too" $
        testWithApplication (pure (readingAt 16_000_000)) $ \port -> do
            manager <- newManager defaultManagerSettings
            request <- parseRequest ("http://127.0.0.1:" <> show port <> "/")
            let chunk = BS.replicate (64 * 1024) 0x61
                upload = request{method = "PUT", requestBody = RequestBodyStreamChunked (givesTimes 1024 chunk)}
            (outcome, elapsed) <- opened 2 manager upload
            outcome `shouldBe` Right 200
            elapsed `shouldSatisfy` (> 2)

    it "fails a connection that arrives after its deadline with the connect timeout, before the sweeper can" $
        testWithApplication (pure (\_ respond -> respond (responseLBS status200 [] "ok"))) $ \port -> do
            manager <- newManager defaultManagerSettings
            request <- parseRequest ("http://127.0.0.1:" <> show port <> "/")
            -- Nothing sweeps these deadlines, so only the check as the connection arrives can fail it.
            deadlines <- newDeadlines
            outcome <- timedOpen (openResponseAfter 1 (ExchangeClient manager deadlines) request)
            fst outcome `shouldBe` Left "ConnectionTimeout"

    it "closes a connection whose headers missed the wait instead of returning it to the pool" $ do
        connections <- newIORef (0 :: Int)
        let settings = setOnOpen (\_ -> atomicModifyIORef' connections (\n -> (n + 1, True))) defaultSettings
            application request respond = case pathInfo request of
                ["withheld"] -> threadDelay 20_000_000 >> respond (responseLBS status200 [] "late")
                _ -> respond (responseLBS status200 [] "ok")
        testWithApplicationSettings settings (pure application) $ \port -> do
            manager <- newManager defaultManagerSettings
            let at path = parseRequest ("http://127.0.0.1:" <> show port <> "/" <> path)
            -- Each body is read through, so a healthy connection goes back to the pool.
            (readThrough manager =<< at "first") `shouldReturn` Right 200
            readIORef connections `shouldReturn` 1
            (readThrough manager =<< at "withheld") `shouldReturn` Left "ResponseTimeout"
            (readThrough manager =<< at "after") `shouldReturn` Right 200
            readIORef connections `shouldReturn` 2

-- Open under a wait of whole seconds, then close: the status, or the timeout's name, and the seconds taken.
opened :: Word64 -> Manager -> Request -> IO (Either Text Int, Double)
opened seconds manager request =
    withSweeper $ \deadlines -> timedOpen (openResponseAfter (seconds * 1_000_000_000) (ExchangeClient manager deadlines) request)

-- The same through http-client's own one-second response timeout, for reference.
openedByClient :: Manager -> Request -> IO (Either Text Int, Double)
openedByClient manager request = timedOpen (responseOpen request{responseTimeout = responseTimeoutMicro 1_000_000} manager)

-- Open under a one-second wait and read the body through before closing.
readThrough :: Manager -> Request -> IO (Either Text Int)
readThrough manager request =
    withSweeper $ \deadlines ->
        first failure
            <$> try (bracket (openResponseAfter 1_000_000_000 (ExchangeClient manager deadlines) request) responseClose (\response -> statusCode (responseStatus response) <$ brConsume (responseBody response)))

timedOpen :: IO (Response BodyReader) -> IO (Either Text Int, Double)
timedOpen open = do
    started <- getMonotonicTime
    outcome <- try (bracket open responseClose (pure . statusCode . responseStatus))
    finished <- getMonotonicTime
    pure (first failure outcome, finished - started)

failure :: HttpException -> Text
failure = \case
    HttpExceptionRequest _ content -> named content
    other -> show other

named :: HttpExceptionContent -> Text
named = show

-- Read the request body at a fixed rate in bytes a second, then answer.
readingAt :: Double -> Application
readingAt rate request respond = do
    started <- getMonotonicTime
    let pace total = do
            chunk <- getRequestBodyChunk request
            unless (BS.null chunk) $ do
                let consumed = total + BS.length chunk
                ahead <- (fromIntegral consumed / rate -) . subtract started <$> getMonotonicTime
                when (ahead > 0) (threadDelay (round (ahead * 1_000_000)))
                pace consumed
    pace (0 :: Int)
    respond (responseLBS status200 [] "")

-- Hand over the same chunk this many times.
givesTimes :: Int -> ByteString -> (IO ByteString -> IO ()) -> IO ()
givesTimes count chunk needsPopper = do
    remaining <- newIORef count
    needsPopper $ atomicModifyIORef' remaining $ \left -> if left <= 0 then (0, BS.empty) else (left - 1, chunk)

between :: Double -> Double -> Double -> Bool
between low high seconds = seconds >= low && seconds < high
