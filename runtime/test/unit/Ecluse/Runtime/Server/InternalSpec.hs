-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | HTTP/1 connection closure through the production listener and its exception observer.
module Ecluse.Runtime.Server.InternalSpec (spec) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as LBS
import Data.Char (toLower)
import Data.Text qualified as T
import Katip (closeScribes)
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Types (Method, ResponseHeaders, hConnection, hContentLength, methodGet, methodHead, status200, status204, status500)
import Network.Socket qualified as Socket
import Network.Socket.ByteString (recv, sendAll)
import Network.Wai (Application, Request, responseLBS, responseStream)
import System.Posix.Signals qualified as Posix
import Test.Hspec
import UnliftIO (timeout)
import UnliftIO.Async (link, withAsync)
import UnliftIO.Exception (bracket, throwIO, withException)

import Ecluse.Core.Server.Admission (newServeAdmissionTuned, withOverflow, withServeAdmission)
import Ecluse.Core.Server.Admission.Overflow (newOverflowTuned)
import Ecluse.Core.Server.Pipeline.Shared (shedMessage, shedStatus, withMetadataAdmission)
import Ecluse.Runtime.Server.Internal (ServerConfig (scOnException, scPort), listeningPrefix, mkServerConfig, runWarp)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Log (memoryLogEnv)
import Ecluse.Test.Poll (pollUntil)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Server.Handler (testServeRuntimeWith)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

spec :: Spec
spec = around_ (timeout 5_000_000 >=> (`shouldBe` Just ())) $ describe "runWarp HTTP/1 connection lifetime" $ do
    for_ ["HTTP/1.0", "HTTP/1.1"] $ \version ->
        for_ [methodGet, methodHead] $ \method ->
            it ("closes after the complete past-cap 503 for " <> B8.unpack version <> " " <> B8.unpack method) $
                withPastCapResponse $ \headers -> do
                    wire <- pipelined version method (bufferedShed headers)
                    assertShedHeaders version wire
                    let (_, body) = BS.breakSubstring "\r\n\r\n" wire
                    BS.drop 4 body `shouldBe` if method == methodHead then "" else encodeUtf8 shedMessage
                    countResponses wire `shouldBe` 1

    it "finishes a streamed closing response before EOF" $ do
        let app _ respond = respond (responseStream shedStatus [(hConnection, "close")] (\write _ -> write "busy"))
        wire <- pipelined "HTTP/1.1" methodGet app
        countResponses wire `shouldBe` 1
        wire `shouldSatisfy` BS.isSuffixOf "4\r\nbusy\r\n0\r\n\r\n"

    it "closes a bodyless response with a mixed-case close token" $ do
        let app _ respond = respond (responseLBS status204 [(hConnection, "keep-alive, Close")] "")
        wire <- pipelined "HTTP/1.1" methodGet app
        countResponses wire `shouldBe` 1
        wire `shouldSatisfy` BS.isPrefixOf "HTTP/1.1 204 "
        BS.drop 4 (snd (BS.breakSubstring "\r\n\r\n" wire)) `shouldBe` ""

    it "processes the second request when the first response permits reuse" $ do
        wire <- pipelined "HTTP/1.1" methodGet (\_ respond -> respond (responseLBS status200 [] "ok"))
        countResponses wire `shouldBe` 2

    it "lets the inner application finish without observing a close as an error" $ do
        finished <- newIORef False
        innerErrors <- newIORef (0 :: Int)
        warpErrors <- newIORef ([] :: [Text])
        let app request respond =
                ( do
                    received <- bufferedShed [(hConnection, "close")] request respond
                    writeIORef finished True
                    pure received
                )
                    `withException` \(_ :: SomeException) -> modifyIORef' innerErrors (+ 1)
        wire <- pipelinedObserved (recordException warpErrors) "HTTP/1.1" methodGet app
        countResponses wire `shouldBe` 1
        readIORef finished `shouldReturn` True
        readIORef innerErrors `shouldReturn` 0
        readIORef warpErrors `shouldReturn` []

    it "observes a real failure after a closing response instead of replacing it" $ do
        observed <- newIORef ([] :: [Text])
        let app request respond = do
                _ <- bufferedShed [(hConnection, "close")] request respond
                -- Warp's observer owns application failures after a committed response.
                throwIO TransportFailure
        wire <- pipelinedObserved (recordException observed) "HTTP/1.1" methodGet app
        countResponses wire `shouldBe` 1
        readIORef observed `shouldReturn` ["TransportFailure"]

    it "keeps a pre-response application failure visible to Warp" $ do
        observed <- newIORef ([] :: [Text])
        -- The fixture drives Warp's exception response and observer.
        let app _ _ = throwIO TransportFailure
        errors <- withRealWarp (recordException observed) app $ \port -> do
            manager <- HttpClient.newManager HttpClient.defaultManagerSettings
            request <- HttpClient.parseRequest ("http://127.0.0.1:" <> show port <> "/")
            response <- HttpClient.httpLbs request manager
            HttpClient.responseStatus response `shouldBe` status500
            pollUntil 200 5_000 (not . null) (readIORef observed)
        errors `shouldSatisfy` (not . null)
        errors `shouldSatisfy` all (== "TransportFailure")

recordException :: IORef [Text] -> Maybe Request -> SomeException -> IO ()
recordException observed _ exception = modifyIORef' observed (show exception :)

data TransportFailure = TransportFailure
    deriving stock (Show)

instance Exception TransportFailure

bufferedShed :: ResponseHeaders -> Application
bufferedShed headers _ respond =
    respond (responseLBS shedStatus ((hContentLength, show (BS.length body)) : headers) (LBS.fromStrict body))
  where
    body = encodeUtf8 shedMessage

assertShedHeaders :: ByteString -> ByteString -> Expectation
assertShedHeaders version wire = do
    wire `shouldSatisfy` BS.isPrefixOf (version <> " 503 ")
    let folded = B8.map toLower wire
    folded `shouldSatisfy` BS.isInfixOf "\r\nretry-after: 1\r\n"
    folded `shouldSatisfy` BS.isInfixOf "\r\nconnection: close\r\n"

withPastCapResponse :: (ResponseHeaders -> IO ()) -> IO ()
withPastCapResponse action = withSweeper $ \deadlines -> do
    overflow <- newOverflowTuned deadlines 0 500_000
    admission <- (`withOverflow` overflow) <$> newServeAdmissionTuned deadlines 1 0 0
    runtime <- testServeRuntimeWith deadlines admission noopMetricsPort
    entered <- newEmptyMVar
    blocked <- newEmptyMVar :: IO (MVar ())
    let hold = withServeAdmission noopMetricsPort admission (putMVar entered () >> takeMVar blocked)
    withAsync hold $ \holder -> do
        link holder
        takeMVar entered
        withMetadataAdmission runtime action (const (expectationFailure "past-cap request entered")) pure

-- The second request asks for close, so the reuse control also ends with EOF.
pipelined :: ByteString -> Method -> Application -> IO ByteString
pipelined = pipelinedObserved (\_ _ -> pass)

pipelinedObserved :: (Maybe Request -> SomeException -> IO ()) -> ByteString -> Method -> Application -> IO ByteString
pipelinedObserved observe version method app =
    withRealWarp observe app $ \port ->
        bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \client -> do
            Socket.connect client (Socket.SockAddrInet (fromIntegral port) (Socket.tupleToHostAddress (127, 0, 0, 1)))
            sendAll client (method <> " / " <> version <> "\r\nHost: localhost\r\nConnection: keep-alive\r\n\r\nGET / " <> version <> "\r\nHost: localhost\r\nConnection: close\r\n\r\n")
            receiveAll client

withRealWarp :: (Maybe Request -> SomeException -> IO ()) -> Application -> (Int -> IO a) -> IO a
withRealWarp observe app action = withSavedSignalHandlers $
    bracket memoryLogEnv (void . closeScribes . fst) $ \(logEnv, messages) -> do
        let cfg = (mkServerConfig []){scPort = 0, scOnException = observe}
        withAsync (runWarp logEnv "transport-test" cfg (const (pure app))) $ \server -> do
            link server
            port <- pollUntil 200 5_000 isJust (loggedPort <$> messages)
            maybe (throwIO (TestContractEscape "listener did not report its port")) action port
  where
    loggedPort = listToMaybe . mapMaybe (readMaybe . toString <=< T.stripPrefix (listeningPrefix "transport-test"))

-- runWarp installs process-wide handlers. Restore the suite's handlers on every exit.
withSavedSignalHandlers :: IO a -> IO a
withSavedSignalHandlers action = bracket save restore (const action)
  where
    save = forM [Posix.sigTERM, Posix.sigINT] $ \signal ->
        (signal,) <$> Posix.installHandler signal Posix.Ignore Nothing
    restore = traverse_ (\(signal, handler) -> void (Posix.installHandler signal handler Nothing))

receiveAll :: Socket.Socket -> IO ByteString
receiveAll client = do
    chunk <- recv client 4096
    if BS.null chunk then pure "" else (chunk <>) <$> receiveAll client

countResponses :: ByteString -> Int
countResponses bytes =
    case BS.breakSubstring "HTTP/1." bytes of
        (_, rest) | BS.null rest -> 0
        (_, rest) -> 1 + countResponses (BS.drop 7 rest)
