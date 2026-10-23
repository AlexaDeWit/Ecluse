-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | A reused connection whose next request loses its response before headers arrive.
module Ecluse.Test.Stub.Retry (
    requestsAfterLostAnswer,
    withLostAnswer,
    withLostAnswerPaced,
) where

import Network.HTTP.Client (Manager, Request (method), httpLbs, parseRequest)
import Network.HTTP.Types (status200)
import Network.Wai (responseLBS, responseRaw)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec (shouldSatisfy)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (tryAny)

import Ecluse.Test.Wai (localhost)

-- | Count the warm request and the unanswered destructive request without allowing a retry.
requestsAfterLostAnswer :: Manager -> IO Int
requestsAfterLostAnswer manager = withLostAnswer manager $ \request -> do
    result <- tryAny (httpLbs request{method = "DELETE"} manager)
    result `shouldSatisfy` isLeft

-- | Warm the connection, drop its next answer, then let subsequent requests succeed.
withLostAnswer :: Manager -> (Request -> IO a) -> IO Int
withLostAnswer = withLostAnswerPaced 0 0

-- | 'withLostAnswer', holding the dropped answer and then the first answer after it for these microseconds.
withLostAnswerPaced :: Int -> Int -> Manager -> (Request -> IO a) -> IO Int
withLostAnswerPaced holdDropped holdRetry manager action = do
    requests <- newIORef (0 :: Int)
    let application _ respond = do
            attempt <- atomicModifyIORef' requests (\n -> (n + 1, n + 1))
            when (attempt == 2) (threadDelay holdDropped)
            when (attempt == 3) (threadDelay holdRetry)
            respond $
                if attempt == 2
                    then responseRaw (\_ _ -> pure ()) (responseLBS status200 [] "")
                    else responseLBS status200 [] "warm connection"
    testWithApplication (pure application) $ \port -> do
        request <- parseRequest (toString (localhost port))
        void (httpLbs request manager)
        void (action request)
        readIORef requests
