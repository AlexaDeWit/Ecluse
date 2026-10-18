-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | HTTP attempt outcomes preserve response ownership, retry behaviour, and cancellation.
module Ecluse.Core.Registry.Exchange.ObserveSpec (spec) where

import Control.Exception (AsyncException (ThreadKilled), throwIO)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import GHC.IO.Exception (IOErrorType (ResourceVanished))
import Network.HTTP.Client (HttpException (HttpExceptionRequest), HttpExceptionContent (ResponseTimeout), Manager, Request (method, requestBody), RequestBody (RequestBodyBS, RequestBodyIO, RequestBodyStream), brConsume, defaultManagerSettings, httpLbs, newManager, parseRequest, responseBody, responseStatus, withResponse)
import Network.HTTP.Types (methodGet, methodHead, status200, status429)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import System.IO.Error (ioeGetErrorType)
import Test.Hspec
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Core.Registry.Exchange.Observe (ExchangeOutcome (..), observeExchange)
import Ecluse.Core.Registry.Progress (meteredUpload, watched)
import Ecluse.Core.Security (mkProgressFloor)
import Ecluse.Test.Stub.Retry (withLostAnswer)
import Ecluse.Test.Stub.Socket (resetAfterPartialBodyOverTcp, withRawServer)
import Ecluse.Test.Wai (localhost)

spec :: Spec
spec = describe "observeExchange" $ do
    it "records raw response status before the body consumer and only once" $ do
        events <- newIORef []
        request <- parseRequest "http://localhost/"
        observeExchange (append events) request $ \modified headers -> do
            beginRequest modified
            headers status429
            readIORef events `shouldReturn` [ResponseStatus 429]
            headers status200
        readIORef events `shouldReturn` [ResponseStatus 429]

    it "records a pre-notification transport failure and preserves the exception" $ do
        events <- newIORef []
        request <- parseRequest "http://localhost/"
        observeExchange (append events) request (\_ _ -> throwIO (HttpExceptionRequest request ResponseTimeout))
            `shouldThrow` isResponseTimeout
        readIORef events `shouldReturn` [TransportFailure]

    it "preserves cancellation before notification without recording a send" $ do
        events <- newIORef []
        request <- parseRequest "http://localhost/"
        observeExchange (append events) request (\_ _ -> throwIO ThreadKilled)
            `shouldThrow` (== ThreadKilled)
        readIORef events `shouldReturn` []

    it "records cancellation after notification and preserves the original exception" $ do
        events <- newIORef []
        request <- parseRequest "http://localhost/"
        observeExchange (append events) request (\modified _ -> beginRequest modified >> throwIO ThreadKilled)
            `shouldThrow` (== ThreadKilled)
        readIORef events `shouldReturn` [Cancelled]

    it "keeps the response status when cancellation follows headers" $ do
        events <- newIORef []
        request <- parseRequest "http://localhost/"
        observeExchange (append events) request (\modified headers -> beginRequest modified >> headers status200 >> throwIO ThreadKilled)
            `shouldThrow` (== ThreadKilled)
        readIORef events `shouldReturn` [ResponseStatus 200]

    it "preserves unrelated synchronous exceptions without inventing a transport failure" $ do
        events <- newIORef []
        request <- parseRequest "http://localhost/"
        observeExchange (append events) request (\modified _ -> beginRequest modified >> throwIO ConsumerStopped)
            `shouldThrow` (== ConsumerStopped)
        readIORef events `shouldReturn` []

    it "preserves empty GET and HEAD methods, framing, and bodies" $ do
        received <- newIORef []
        events <- newIORef []
        let application request respond = do
                body <- Wai.strictRequestBody request
                let headers = Wai.requestHeaders request
                append received (Wai.requestMethod request, lookup "Content-Length" headers, lookup "Transfer-Encoding" headers, body)
                respond (Wai.responseLBS status200 [("Connection", "close")] "")
        testWithApplication (pure application) $ \port -> do
            manager <- newManager defaultManagerSettings
            request <- parseRequest (toString (localhost port))
            for_ [methodGet, methodHead] $ \verb -> do
                void (httpLbs request{method = verb} manager)
                void (observedBody manager (append events) request{method = verb})
        readIORef received >>= \case
            [getBefore, getObserved, headBefore, headObserved] -> do
                getObserved `shouldBe` getBefore
                headObserved `shouldBe` headBefore
            other -> expectationFailure ("expected four requests, received " <> show other)
        readIORef events `shouldReturn` [ResponseStatus 200, ResponseStatus 200]

    it "preserves upload bytes and progress metering through attempt notification" $ do
        events <- newIORef []
        let bytes = BS.replicate 200_000 0x61
            application request respond = Wai.strictRequestBody request >>= respond . Wai.responseLBS status200 []
        progress <- either (fail . show) pure (mkProgressFloor 60 30 1)
        testWithApplication (pure application) $ \port -> do
            manager <- newManager defaultManagerSettings
            request <- parseRequest (toString (localhost port))
            watched
                progress
                ( \watch ->
                    observedBody manager (append events) request{method = "POST", requestBody = meteredUpload watch (RequestBodyBS bytes)}
                )
                `shouldReturn` Just (LBS.fromStrict bytes)
        readIORef events `shouldReturn` [ResponseStatus 200]

    it "records a stalled upload once and preserves the watchdog result" $ do
        events <- newIORef []
        progress <- either (fail . show) pure (mkProgressFloor 60 0.3 1024)
        request <- parseRequest "http://localhost/"
        let upload = request{requestBody = RequestBodyBS (BS.replicate 2048 0x61)}
        watched
            progress
            ( \watch ->
                observeExchange (append events) upload $ \modified _ ->
                    case meteredUpload watch (requestBody modified) of
                        RequestBodyIO prepare ->
                            prepare >>= \case
                                RequestBodyStream _ gives -> gives $ \next -> do
                                    void next
                                    threadDelay 2_000_000
                                _ -> expectationFailure "expected the metered upload stream"
                        _ -> expectationFailure "expected attempt notification"
            )
            `shouldReturn` Nothing
        readIORef events `shouldReturn` [TransportFailure]

    it "records the failed reused-connection attempt and its successful retry" $ do
        events <- newIORef []
        manager <- newManager defaultManagerSettings
        withLostAnswer manager (observedBody manager (append events)) `shouldReturn` 3
        readIORef events `shouldReturn` [TransportFailure, ResponseStatus 200]

    it "records only the received status when the body later fails" $ do
        events <- newIORef []
        withRawServer resetAfterPartialBodyOverTcp $ \port -> do
            manager <- newManager defaultManagerSettings
            request <- parseRequest (toString (localhost port))
            observedBody manager (append events) request `shouldThrow` ((== ResourceVanished) . ioeGetErrorType)
        readIORef events `shouldReturn` [ResponseStatus 200]

observedBody :: Manager -> (ExchangeOutcome -> IO ()) -> Request -> IO LBS.ByteString
observedBody manager record request = observeExchange record request $ \modified headers ->
    withResponse modified manager $ \response -> do
        headers (responseStatus response)
        LBS.fromChunks <$> brConsume (responseBody response)

beginRequest :: Request -> IO ()
beginRequest request = case requestBody request of
    RequestBodyIO action -> void action
    _ -> expectationFailure "the observer did not install attempt notification"

append :: IORef [a] -> a -> IO ()
append events event = modifyIORef' events (<> [event])

isResponseTimeout :: HttpException -> Bool
isResponseTimeout = \case
    HttpExceptionRequest _ ResponseTimeout -> True
    _ -> False

data ConsumerStopped = ConsumerStopped
    deriving stock (Eq, Show)

instance Exception ConsumerStopped
