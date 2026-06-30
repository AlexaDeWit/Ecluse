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

{- | The graceful-shutdown drain, exercised against a real Warp listener on
loopback. These assert the lifecycle guarantee the going-away design rests on: when
the listen socket is closed (what the @SIGTERM@\/@SIGINT@ handler does in
'Ecluse.Server.runServer'), Warp stops accepting new connections yet __waits for an
in-flight request to finish__ before the server returns -- so a request mid-flight
during a rollover is never cut off.

The drain is driven by closing the socket directly rather than by delivering an OS
signal: signal delivery is process-global and would race the test runner. The signal
handler's body -- raise the drain flag, then run Warp's @closeSocket@ -- is what these
tests stand in for; the flag-raising and readiness\/header effects are asserted
socket-free in "Ecluse.ServerSpec".
-}
spec :: Spec
spec = describe "graceful shutdown -- drain in-flight work" $ do
    it "completes an in-flight request after the socket is closed, then the server stops" $ do
        -- A handler that blocks on a release barrier, so we can hold a request
        -- in-flight across the socket close -- the rollover window the drain guards.
        arrived <- newEmptyMVar
        release <- newEmptyMVar
        let app :: Application
            app _req respond = do
                putMVar arrived ()
                takeMVar release
                respond (responseLBS status200 [] "served")

        withListener app 30 $ \port closeSocket serverThread -> do
            manager <- newManager defaultManagerSettings
            -- Fire the slow request; it reaches the handler and blocks there.
            inflight <- async (getStatusBody manager port)
            takeMVar arrived

            -- Begin the drain: close the listen socket (the signal handler's act).
            closeSocket

            -- The server must NOT have returned yet -- it is waiting on the
            -- in-flight request, which is still parked on the barrier.
            stillServing <- poll serverThread
            stillServing `shouldSatisfy` isNothing

            -- Release the handler; the in-flight request completes with its body
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

            -- Close the socket: with nothing in flight, the drain has nothing to
            -- wait for, so the server stops well inside the 30s graceful window --
            -- it does not block out the whole timeout.
            closeSocket
            stopped <- timeout 5_000_000 (wait serverThread)
            stopped `shouldBe` Just ()

            -- And it served before stopping (not refused from the start).
            afterStop <- try (getStatusBody manager port) :: IO (Either SomeException (Int, LByteString))
            afterStop `shouldSatisfy` isLeft

{- Run an 'Application' on a free loopback port with the same graceful-shutdown
settings 'Ecluse.Server.runServer' uses -- a bounded 'setGracefulShutdownTimeout' and
a 'setInstallShutdownHandler' -- and hand the test the port, a @closeSocket@ action
that begins the drain, and the server's 'Async' so it can observe when the server
returns. The shutdown handler captures Warp's @closeSocket@ into an MVar rather than
installing an OS signal handler, so the drain is triggered deterministically.
-}
withListener ::
    Application ->
    Int ->
    (Port -> IO () -> Async () -> IO a) ->
    IO a
withListener app drainTimeoutSeconds k = do
    -- Discover a free port, then release it so Warp can open and own its own listen
    -- socket -- so the @closeSocket@ the install handler captures closes the very
    -- socket Warp's accept loop holds, the way 'Ecluse.Server.runServer' is wired.
    port <- freePort
    closeSocketVar <- newEmptyMVar
    let settings =
            setPort port
                . setGracefulShutdownTimeout (Just drainTimeoutSeconds)
                . setInstallShutdownHandler (putMVar closeSocketVar)
                $ defaultSettings
    serverThread <- async (runSettings settings app)
    -- The install handler runs as Warp starts; await the captured close action,
    -- then give the listener a beat to begin accepting before the test connects.
    closeSocket <- takeMVar closeSocketVar
    threadDelay 200_000
    k port closeSocket serverThread

-- A port no listener is currently bound to: open a free one and immediately
-- release it, leaving the number for Warp to bind. (A brief race with another
-- process is tolerable for a loopback test.)
freePort :: IO Port
freePort = do
    (port, sock) <- openFreePort
    close sock
    pure port

{- Issue a GET to the loopback listener and return its status code and body. The
request carries @Connection: close@ -- as a response from a draining instance does
in production -- so the connection is not held open in a keep-alive pool past the
response; the graceful drain then completes once the in-flight request returns
rather than waiting on an idle socket.
-}
getStatusBody :: Manager -> Port -> IO (Int, LByteString)
getStatusBody manager port = do
    base <- parseRequest ("http://127.0.0.1:" <> show port <> "/")
    let request = base{requestHeaders = (hConnection, "close") : requestHeaders base}
    response <- httpLbs request manager
    pure (statusCode (responseStatus response), responseBody response)
