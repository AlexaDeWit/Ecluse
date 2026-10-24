-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Request deadlines and protocol-specific transport middleware.
module Ecluse.Runtime.Server.MiddlewareSpec (spec) where

import GHC.Clock (getMonotonicTime)
import Network.HTTP.Client (
    HttpException (HttpExceptionRequest),
    Manager,
    defaultManagerSettings,
    httpLbs,
    managerResponseTimeout,
    newManager,
    parseRequest,
    responseBody,
    responseStatus,
    responseTimeoutNone,
 )
import Network.HTTP.Types (hConnection, http20, status200, statusCode)
import Network.Wai (Application, Middleware, defaultRequest, httpVersion, responseLBS, responseStream)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Internal (ResponseReceived (ResponseReceived))
import Network.Wai.Middleware.Timeout qualified as WaiExtra
import Test.Hspec
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (try)

import Ecluse.Runtime.Server.Middleware (closeResponseConnections, requestTimeout)
import Ecluse.Test.Deadline (withSweeper)

spec :: Spec
spec = do
    requestTimeoutSpec
    describe "closeResponseConnections" $
        it "passes HTTP/2 responses through without a close exception" $ do
            sent <- newIORef (0 :: Int)
            let respond _ = modifyIORef' sent (+ 1) $> ResponseReceived
                app _ reply = reply (responseLBS status200 [(hConnection, "close")] "ok")
            _ <- closeResponseConnections app defaultRequest{httpVersion = http20} respond
            readIORef sent `shouldReturn` 1

requestTimeoutSpec :: Spec
requestTimeoutSpec = describe "requestTimeout" $ do
    it "answers an empty 503 once a request outlives its seconds" $
        withSweeper $ \deadlines -> do
            (outcome, elapsed) <- served (requestTimeout deadlines 1) stalling
            outcome `shouldBe` "503 \"\""
            elapsed `shouldSatisfy` \seconds -> seconds >= 1 && seconds < 4

    it "passes a request that finishes in time through unchanged" $
        withSweeper $ \deadlines ->
            fst <$> served (requestTimeout deadlines 1) prompt `shouldReturn` "200 \"ok\""

    it "ends a response that has already begun as wai-extra's timeout does" $
        withSweeper $ \deadlines -> do
            (ours, _) <- served (requestTimeout deadlines 1) beginsThenStalls
            (theirs, _) <- served (WaiExtra.timeout 1) beginsThenStalls
            ours `shouldBe` theirs

served :: Middleware -> Application -> IO (Text, Double)
served middleware app =
    testWithApplication (pure (middleware app)) $ \port -> do
        manager <- newManager defaultManagerSettings{managerResponseTimeout = responseTimeoutNone}
        started <- getMonotonicTime
        outcome <- fetched manager port
        finished <- getMonotonicTime
        pure (outcome, finished - started)

fetched :: Manager -> Int -> IO Text
fetched manager port = do
    request <- parseRequest ("http://127.0.0.1:" <> show port <> "/")
    try (httpLbs request manager) <&> \case
        Right response -> show (statusCode (responseStatus response)) <> " " <> show (responseBody response)
        Left (HttpExceptionRequest _ content) -> toText (takeWhile (/= ' ') (show content))
        Left other -> show other

stalling :: Application
stalling _ respond = threadDelay 20_000_000 >> respond (responseLBS status200 [] "late")

prompt :: Application
prompt _ respond = respond (responseLBS status200 [] "ok")

beginsThenStalls :: Application
beginsThenStalls _ respond = respond (responseStream status200 [] (\write flush -> write "partial" >> flush >> threadDelay 20_000_000))
