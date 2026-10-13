-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Held loopback listeners prove graceful draining and fixture cleanup.
module Ecluse.Runtime.ServerIntegrationSpec (spec) where

import Ecluse.Test.Contract.Catalogue (caseLabel)
import Network.HTTP.Client (
    Manager,
    defaultManagerSettings,
    httpLbs,
    newManager,
    parseRequest,
    requestHeaders,
    responseBody,
    responseStatus,
 )
import Network.HTTP.Types (hConnection, status200, statusCode)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (
    Port,
    defaultSettings,
    openFreePort,
    runSettingsSocket,
    setGracefulShutdownTimeout,
    setInstallShutdownHandler,
 )
import Test.Hspec
import UnliftIO.Async (Async, poll, race, wait, withAsync)
import UnliftIO.Exception (bracket, finally, throwIO, try)
import UnliftIO.Timeout (timeout)

import Ecluse.Test.Support (TestContractEscape (TestContractEscape))
import Network.Socket (close)

-- | Direct socket closure avoids process-global signal delivery in the runner.
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

        withListener app 30 $ \port closeSocket serverThread -> do
            manager <- newManager defaultManagerSettings
            flip finally (void (tryPutMVar release ())) $ withAsync (getStatusBody manager port) $ \inflight -> do
                awaitFixture "handler arrival" (wait inflight) (takeMVar arrived)
                closeSocket
                stillServing <- poll serverThread
                stillServing `shouldSatisfy` isNothing
                putMVar release ()
                result <- timeout 5_000_000 (wait inflight)
                result `shouldBe` Just (200, "served")
                stopped <- timeout 5_000_000 (wait serverThread)
                stopped `shouldBe` Just ()

    it (caseLabel "integration/server-shutdown/native-002" "returns promptly when the socket is closed with no work in flight") $ do
        let app :: Application
            app _req respond = respond (responseLBS status200 [] "served")

        withListener app 30 $ \port closeSocket serverThread -> do
            manager <- newManager defaultManagerSettings
            beforeClose <- getStatusBody manager port
            beforeClose `shouldBe` (200, "served")

            closeSocket
            stopped <- timeout 5_000_000 (wait serverThread)
            stopped `shouldBe` Just ()

            afterStop <- try (getStatusBody manager port) :: IO (Either SomeException (Int, LByteString))
            afterStop `shouldSatisfy` isLeft

    it (caseLabel "integration/server-shutdown/native-003" "joins the listener and client when the fixture body fails") $ do
        arrived <- newEmptyMVar
        release <- newEmptyMVar
        resources <- newEmptyMVar
        let app _ respond = putMVar arrived () >> takeMVar release >> respond (responseLBS status200 [] "served")
        withListener
            app
            30
            ( \port _ serverThread -> do
                manager <- newManager defaultManagerSettings
                flip finally (void (tryPutMVar release ())) $ withAsync (getStatusBody manager port) $ \client -> do
                    putMVar resources (port, serverThread, client)
                    awaitFixture "handler arrival" (wait client) (takeMVar arrived)
                    throwIO (TestContractEscape "forced fixture failure")
            )
            `shouldThrow` (\(TestContractEscape message) -> message == "forced fixture failure")
        (port, serverThread, client) <- takeMVar resources
        poll serverThread >>= (`shouldSatisfy` isJust)
        poll client >>= (`shouldSatisfy` isJust)
        manager <- newManager defaultManagerSettings
        afterStop <- try (getStatusBody manager port) :: IO (Either SomeException (Int, LByteString))
        afterStop `shouldSatisfy` isLeft

    it (caseLabel "integration/server-shutdown/native-004" "reports a client failure before handler arrival") $ do
        arrived <- newEmptyMVar :: IO (MVar ())
        withListener (\_ respond -> respond (responseLBS status200 [] "served")) 30 $ \_ _ _ ->
            withAsync (throwIO (TestContractEscape "forced client failure") :: IO ()) $ \client ->
                awaitFixture "handler arrival" (wait client) (takeMVar arrived)
                    `shouldThrow` (\(TestContractEscape message) -> message == "forced client failure")

withListener :: Application -> Int -> (Port -> IO () -> Async () -> IO a) -> IO a
withListener app drainTimeoutSeconds action =
    bracket openFreePort (close . snd) $ \(port, socket) -> do
        closeSocketVar <- newEmptyMVar
        let settings =
                setGracefulShutdownTimeout (Just drainTimeoutSeconds)
                    . setInstallShutdownHandler (putMVar closeSocketVar)
                    $ defaultSettings
        withAsync (runSettingsSocket settings socket app) $ \serverThread -> do
            closeSocket <- awaitFixture "listener readiness" (wait serverThread) (takeMVar closeSocketVar)
            action port closeSocket serverThread

awaitFixture :: Text -> IO a -> IO b -> IO b
awaitFixture label terminated ready = do
    result <- timeout 5_000_000 (race terminated ready)
    case result of
        Just (Right value) -> pure value
        Just (Left _) -> throwIO (TestContractEscape (label <> ": participant stopped before readiness"))
        Nothing -> throwIO (TestContractEscape (label <> ": timed out"))

getStatusBody :: Manager -> Port -> IO (Int, LByteString)
getStatusBody manager port = do
    base <- parseRequest ("http://127.0.0.1:" <> show port <> "/")
    let request = base{requestHeaders = (hConnection, "close") : requestHeaders base}
    response <- httpLbs request manager
    pure (statusCode (responseStatus response), responseBody response)
