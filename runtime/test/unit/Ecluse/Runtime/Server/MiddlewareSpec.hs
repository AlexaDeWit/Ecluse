-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The request timeout answers a stalled request with an empty 503 and passes a prompt one through, as wai-extra's did.
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
import Network.HTTP.Types (status200, statusCode)
import Network.Wai (Application, Middleware, responseLBS, responseStream)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Middleware.Timeout qualified as WaiExtra
import Test.Hspec
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (try)

import Ecluse.Runtime.Server.Middleware (requestTimeout)
import Ecluse.Test.Deadline (withSweeper)

spec :: Spec
spec = describe "requestTimeout" $ do
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

-- One request through the middleware: the status and body, or the client's fault, and the seconds taken.
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
