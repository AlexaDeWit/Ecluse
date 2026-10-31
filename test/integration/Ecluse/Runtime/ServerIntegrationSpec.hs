-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The production listener drains in-flight work on @SIGTERM@, and the fixture cleans up after failures.
module Ecluse.Runtime.ServerIntegrationSpec (spec) where

import Data.List (lookup)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Network.HTTP.Client (
    Manager,
    defaultManagerSettings,
    httpLbs,
    newManager,
    parseRequest,
    requestHeaders,
    responseBody,
    responseHeaders,
    responseStatus,
 )
import Network.HTTP.Types (HeaderName, hConnection, status200, status503, statusCode)
import Network.Socket qualified as Socket
import Network.Wai (Application, defaultRequest, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Test (SResponse (simpleStatus), request, runSession, setPath)
import Test.Hspec
import UnliftIO.Async (Async, poll, race, wait, withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (bracket, finally, throwIO, try)
import UnliftIO.Timeout (timeout)

import Ecluse.Runtime.Server.Internal (
    ServerConfig (scCheckLive, scCheckReady, scDrain, scDrainTimeout),
    ShutdownDrainTimeout (ShutdownDrainTimeout),
    mkServerConfig,
    probeApplication,
    proxyListener,
    serverMiddleware,
 )
import Ecluse.Runtime.Test.Server (raiseShutdownSignal, withProductionListener)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

-- | The runner runs one example at a time, so a process-wide @SIGTERM@ reaches only the listener under test.
spec :: Spec
spec = describe "graceful shutdown -- drain in-flight work" $ do
    it (caseLabel "integration/server-shutdown/native-001" "completes an in-flight request after the socket is closed, then the server stops") $ do
        arrived <- newEmptyMVar
        release <- newEmptyMVar
        let app :: Application
            app _req respond = do
                putMVar arrived ()
                takeMVar release
                respond (responseLBS status200 [] "served")

        withListener app $ \port serverThread served -> do
            manager <- newManager defaultManagerSettings
            flip finally (void (tryPutMVar release ())) $ withAsync (getResponse manager port []) $ \inflight -> do
                awaitFixture "handler arrival" (wait inflight) (takeMVar arrived)
                readyBefore <- getResponseAt manager port "/readyz" []
                status readyBefore `shouldBe` 200
                raiseShutdownSignal
                awaitFixture "listening socket closure" (wait serverThread) (awaitRefused port)
                -- The held request keeps the drain open after the socket has closed.
                timeout 1_000_000 (wait serverThread) >>= (`shouldBe` Nothing)
                -- warp closes idle connections once the socket closes, so readiness is asked of the
                -- application it serves, in process.
                readyDuring <- runSession (request (setPath defaultRequest "/readyz")) served
                simpleStatus readyDuring `shouldBe` status503
                putMVar release ()
                result <- timeout 5_000_000 (wait inflight)
                result `shouldBe` Just (200, Just "close", "served")
                stopped <- timeout 5_000_000 (wait serverThread)
                stopped `shouldBe` Just ()

    it (caseLabel "integration/server-shutdown/native-002" "returns promptly when the socket is closed with no work in flight") $ do
        let app :: Application
            app _req respond = respond (responseLBS status200 [] "served")

        withListener app $ \port serverThread _ -> do
            manager <- newManager defaultManagerSettings
            -- The client closes its connection, so none is open when the signal arrives.
            (beforeStatus, _, beforeBody) <- getResponse manager port [(hConnection, "close")]
            (beforeStatus, beforeBody) `shouldBe` (200, "served")

            raiseShutdownSignal
            stopped <- timeout 5_000_000 (wait serverThread)
            stopped `shouldBe` Just ()

            afterStop <- try (getResponse manager port []) :: IO (Either SomeException (Int, Maybe ByteString, LByteString))
            afterStop `shouldSatisfy` isLeft

    it (caseLabel "integration/server-shutdown/native-003" "joins the listener and client when the fixture body fails") $ do
        arrived <- newEmptyMVar
        release <- newEmptyMVar
        resources <- newEmptyMVar
        let app _ respond = putMVar arrived () >> takeMVar release >> respond (responseLBS status200 [] "served")
        withListener
            app
            ( \port serverThread _ -> do
                manager <- newManager defaultManagerSettings
                flip finally (void (tryPutMVar release ())) $ withAsync (getResponse manager port []) $ \client -> do
                    putMVar resources (port, serverThread, client)
                    awaitFixture "handler arrival" (wait client) (takeMVar arrived)
                    throwIO (TestContractEscape "forced fixture failure")
            )
            `shouldThrow` (\(TestContractEscape message) -> message == "forced fixture failure")
        (port, serverThread, client) <- takeMVar resources
        poll serverThread >>= (`shouldSatisfy` isJust)
        poll client >>= (`shouldSatisfy` isJust)
        manager <- newManager defaultManagerSettings
        afterStop <- try (getResponse manager port []) :: IO (Either SomeException (Int, Maybe ByteString, LByteString))
        afterStop `shouldSatisfy` isLeft

    it (caseLabel "integration/server-shutdown/native-004" "reports a client failure before handler arrival") $ do
        arrived <- newEmptyMVar :: IO (MVar ())
        withListener (\_ respond -> respond (responseLBS status200 [] "served")) $ \_ _ _ ->
            withAsync (throwIO (TestContractEscape "forced client failure") :: IO ()) $ \client ->
                awaitFixture "handler arrival" (wait client) (takeMVar arrived)
                    `shouldThrow` (\(TestContractEscape message) -> message == "forced client failure")

{- The production listener under the production middleware, serving @app@ beside the production
health probes, with the application it serves. The drain is long enough that only a finished
request ends it.
-}
withListener :: Application -> (Port -> Async () -> Application -> IO a) -> IO a
withListener app action =
    withSweeper $ \deadlines -> do
        built <- newEmptyMVar
        let serve live = do
                let served = serverMiddleware deadlines live (withProbes live)
                putMVar built served
                pure served
        withProductionListener proxyListener cfg serve $ \port server -> readMVar built >>= action port server
  where
    cfg = (mkServerConfig []){scDrainTimeout = ShutdownDrainTimeout 30}
    withProbes live received
        | pathInfo received == ["readyz"] = probeApplication (scDrain live) (scCheckReady live) (scCheckLive live) received
        | otherwise = app received

awaitFixture :: Text -> IO a -> IO b -> IO b
awaitFixture label terminated ready = do
    result <- timeout 5_000_000 (race terminated ready)
    case result of
        Just (Right value) -> pure value
        Just (Left _) -> throwIO (TestContractEscape (label <> ": participant stopped before readiness"))
        Nothing -> throwIO (TestContractEscape (label <> ": timed out"))

-- Return once a fresh connection to the port is refused, which happens only after the listening socket closes.
awaitRefused :: Port -> IO ()
awaitRefused port = do
    accepted <- try (bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close connect)
    case accepted of
        Left (_ :: SomeException) -> pass
        Right () -> threadDelay 50_000 >> awaitRefused port
  where
    connect client = Socket.connect client (Socket.SockAddrInet (fromIntegral port) (Socket.tupleToHostAddress (127, 0, 0, 1)))

getResponse :: Manager -> Port -> [(HeaderName, ByteString)] -> IO (Int, Maybe ByteString, LByteString)
getResponse manager port = getResponseAt manager port "/"

getResponseAt :: Manager -> Port -> String -> [(HeaderName, ByteString)] -> IO (Int, Maybe ByteString, LByteString)
getResponseAt manager port path headers = do
    base <- parseRequest ("http://127.0.0.1:" <> show port <> path)
    response <- httpLbs base{requestHeaders = headers <> requestHeaders base} manager
    pure (statusCode (responseStatus response), lookup hConnection (responseHeaders response), responseBody response)

status :: (Int, Maybe ByteString, LByteString) -> Int
status (code, _, _) = code
