-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Server.GracefulShutdownSpec (spec) where

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
import Network.Socket (close)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (
    Port,
    defaultSettings,
    openFreePort,
    runSettings,
    setGracefulShutdownTimeout,
    setInstallShutdownHandler,
    setPort,
 )
import Test.Hspec
import UnliftIO.Async (Async, async, poll, wait)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (try)
import UnliftIO.Timeout (timeout)

{- | The graceful-shutdown drain, driven against a real Warp listener on loopback. Closing
the listen socket stops new connections, but Warp still waits for an in-flight request to
finish. These tests close the socket directly, because signal delivery is process-global and
would race the test runner.
-}
spec :: Spec
spec = describe "graceful shutdown -- drain in-flight work" $ do
    it "completes an in-flight request after the socket is closed, then the server stops" $ do
        -- A handler that blocks on a release barrier. The test can then hold a request
        -- in-flight across the socket close, the rollover window the drain guards.
        arrived <- newEmptyMVar
        release <- newEmptyMVar
        let app :: Application
            app _req respond = do
                putMVar arrived ()
                takeMVar release
                respond (responseLBS status200 [] "served")

        withListener app 30 $ \port closeSocket serverThread -> do
            manager <- newManager defaultManagerSettings
            -- Fire the slow request. It reaches the handler and blocks there.
            inflight <- async (getStatusBody manager port)
            takeMVar arrived

            -- Begin the drain: close the listen socket (the signal handler's act).
            closeSocket

            -- The server must NOT have returned yet: it waits on the in-flight
            -- request, which is still parked on the barrier.
            stillServing <- poll serverThread
            stillServing `shouldSatisfy` isNothing

            -- Release the handler. The in-flight request completes with its body
            -- intact (no mid-request cut-off), and only then does the server stop.
            putMVar release ()
            result <- timeout 5_000_000 (wait inflight)
            result `shouldBe` Just (200, "served")

            stopped <- timeout 5_000_000 (wait serverThread)
            stopped `shouldBe` Just ()

    it "returns promptly when the socket is closed with no work in flight" $ do
        let app :: Application
            app _req respond = respond (responseLBS status200 [] "served")

        withListener app 30 $ \port closeSocket serverThread -> do
            manager <- newManager defaultManagerSettings
            -- Before the close, the listener serves.
            beforeClose <- getStatusBody manager port
            beforeClose `shouldBe` (200, "served")

            -- With nothing in flight the drain waits for nothing. The server stops well inside the
            -- 30s graceful window rather than blocking out the whole timeout.
            closeSocket
            stopped <- timeout 5_000_000 (wait serverThread)
            stopped `shouldBe` Just ()

            -- And it served before stopping (not refused from the start).
            afterStop <- try (getStatusBody manager port) :: IO (Either SomeException (Int, LByteString))
            afterStop `shouldSatisfy` isLeft

{- Run an 'Application' on a free loopback port with the graceful-shutdown settings
'Ecluse.Server.runServer' uses. The install handler captures Warp's @closeSocket@ into an
MVar rather than an OS signal handler, so the test triggers the drain deterministically.
-}
withListener ::
    Application ->
    Int ->
    (Port -> IO () -> Async () -> IO a) ->
    IO a
withListener app drainTimeoutSeconds k = do
    -- Release the discovered port so Warp opens and owns the listen socket that the captured
    -- @closeSocket@ then closes, the way 'Ecluse.Server.runServer' wires it.
    port <- freePort
    closeSocketVar <- newEmptyMVar
    let settings =
            setPort port
                . setGracefulShutdownTimeout (Just drainTimeoutSeconds)
                . setInstallShutdownHandler (putMVar closeSocketVar)
                $ defaultSettings
    serverThread <- async (runSettings settings app)
    -- The install handler runs as Warp starts. Await the captured close action, then
    -- give the listener a beat to begin accepting before the test connects.
    closeSocket <- takeMVar closeSocketVar
    threadDelay 200_000
    k port closeSocket serverThread

-- Open a free port and release it at once, leaving the number for Warp to bind. A brief
-- race with another process is tolerable for a loopback test.
freePort :: IO Port
freePort = do
    (port, sock) <- openFreePort
    close sock
    pure port

{- Issue a GET to the loopback listener. The request carries @Connection: close@, as a
response from a draining instance does, so no keep-alive socket holds the drain open.
-}
getStatusBody :: Manager -> Port -> IO (Int, LByteString)
getStatusBody manager port = do
    base <- parseRequest ("http://127.0.0.1:" <> show port <> "/")
    let request = base{requestHeaders = (hConnection, "close") : requestHeaders base}
    response <- httpLbs request manager
    pure (statusCode (responseStatus response), responseBody response)
