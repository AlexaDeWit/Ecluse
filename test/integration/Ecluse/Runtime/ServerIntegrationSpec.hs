-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The production listener drains in-flight work on @SIGTERM@ and refuses new connections.
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
import Network.HTTP.Types (HeaderName, hConnection, status200, statusCode)
import Network.Socket qualified as Socket
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (Port)
import Test.Hspec
import UnliftIO.Async (Async, race, wait, withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (bracket, finally, throwIO, try)
import UnliftIO.Timeout (timeout)

import Ecluse.Runtime.Server.Internal (
    ServerConfig (scDrainTimeout),
    ShutdownDrainTimeout (ShutdownDrainTimeout),
    mkServerConfig,
    proxyListener,
    serverMiddleware,
 )
import Ecluse.Runtime.Test.Server (raiseShutdownSignal, withProductionListener)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

-- | The runner runs one example at a time, so a process-wide @SIGTERM@ reaches only the listener under test.
spec :: Spec
spec = describe "graceful shutdown -- drain in-flight work" $ do
    it (caseLabel "integration/server-shutdown/native-001" "refuses new connections and finishes an in-flight request with Connection: close, then the server stops") $ do
        arrived <- newEmptyMVar
        release <- newEmptyMVar
        let app :: Application
            app _req respond = do
                putMVar arrived ()
                takeMVar release
                respond (responseLBS status200 [] "served")

        withListener app $ \port serverThread -> do
            manager <- newManager defaultManagerSettings
            flip finally (void (tryPutMVar release ())) $ withAsync (getResponse manager port []) $ \inflight -> do
                awaitFixture "handler arrival" (wait inflight) (takeMVar arrived)
                raiseShutdownSignal
                refused <- isJust <$> timeout 5_000_000 (awaitRefused port)
                heldOpen <- isNothing <$> timeout 1_000_000 (wait serverThread)
                putMVar release ()
                finished <- timeout 5_000_000 (wait inflight)
                stopped <- isJust <$> timeout 5_000_000 (wait serverThread)
                -- One comparison, so a failure shows every observation at once.
                (refused, heldOpen, finished, stopped) `shouldBe` (True, True, Just (200, Just "close", "served"), True)

    it (caseLabel "integration/server-shutdown/native-002" "returns promptly when the socket is closed with no work in flight") $ do
        let app :: Application
            app _req respond = respond (responseLBS status200 [] "served")

        withListener app $ \port serverThread -> do
            manager <- newManager defaultManagerSettings
            -- The client closes its connection, so none is open when the signal arrives.
            (beforeStatus, _, beforeBody) <- getResponse manager port [(hConnection, "close")]
            (beforeStatus, beforeBody) `shouldBe` (200, "served")

            raiseShutdownSignal
            stopped <- timeout 5_000_000 (wait serverThread)
            stopped `shouldBe` Just ()

            afterStop <- try (getResponse manager port []) :: IO (Either SomeException (Int, Maybe ByteString, LByteString))
            afterStop `shouldSatisfy` isLeft

-- The production listener and middleware over @app@, with a drain long enough that only a finished request ends it.
withListener :: Application -> (Port -> Async () -> IO a) -> IO a
withListener app action =
    withSweeper $ \deadlines ->
        withProductionListener proxyListener cfg (const (pure (serverMiddleware deadlines app))) action
  where
    cfg = (mkServerConfig []){scDrainTimeout = ShutdownDrainTimeout 30}

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
getResponse manager port headers = do
    base <- parseRequest ("http://127.0.0.1:" <> show port <> "/")
    response <- httpLbs base{requestHeaders = headers <> requestHeaders base} manager
    pure (statusCode (responseStatus response), lookup hConnection (responseHeaders response), responseBody response)
