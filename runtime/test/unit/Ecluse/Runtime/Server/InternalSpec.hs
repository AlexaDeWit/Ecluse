-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Connection reuse for admission refusals through the production listener.
module Ecluse.Runtime.Server.InternalSpec (spec) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as LBS
import Data.Char (toLower)
import Data.List (lookup)
import Network.HPACK qualified as HPACK
import Network.HTTP.Semantics (tokenFoldedKey)
import Network.HTTP.Types (HeaderName, Method, ResponseHeaders, hContentLength, methodGet, methodHead, status200)
import Network.HTTP2.Client qualified as H2
import Network.Socket qualified as Socket
import Network.Socket.ByteString (recv, sendAll)
import Network.Wai (Application, responseLBS, responseStream)
import Test.Hspec
import UnliftIO (timeout)
import UnliftIO.Async (link, withAsync)
import UnliftIO.Exception (bracket, throwIO)

import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), boundedRead)
import Ecluse.Core.Server.Admission (newServeAdmissionTuned, withOverflow, withServeAdmission)
import Ecluse.Core.Server.Admission.Overflow (newOverflowTuned)
import Ecluse.Core.Server.Context (ServeRuntime)
import Ecluse.Core.Server.Pipeline.Shared (shedMessage, shedStatus, withMetadataAdmission)
import Ecluse.Runtime.Server.Internal (mkServerConfig)
import Ecluse.Runtime.Test.Server (withProductionListener)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Server.Handler (testServeRuntimeWith)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

spec :: Spec
spec = around_ (timeout 5_000_000 >=> (`shouldBe` Just ())) $ describe "runWarp connection lifetime" $ do
    for_ ["HTTP/1.0", "HTTP/1.1"] $ \version ->
        for_ [methodGet, methodHead] $ \method ->
            it ("reuses one connection for two complete past-cap 503 responses with " <> B8.unpack version <> " " <> B8.unpack method) $
                withSaturatedAdmission $ \runtime -> do
                    wire <- pipelined version method (pastCapApplication runtime)
                    let (firstHeaders, firstRest) = BS.breakSubstring "\r\n\r\n" wire
                        firstBody = if method == methodHead then "" else encodeUtf8 shedMessage
                        (body, followingResponse) = BS.splitAt (BS.length firstBody) (BS.drop 4 firstRest)
                        (secondHeaders, secondRest) = BS.breakSubstring "\r\n\r\n" followingResponse
                    assertShedHeaders version firstHeaders
                    body `shouldBe` firstBody
                    assertShedHeaders version secondHeaders
                    BS.drop 4 secondRest `shouldBe` encodeUtf8 shedMessage
                    countResponses wire `shouldBe` 2

    it "keeps one HTTP/2 connection open for two complete past-cap 503 streams while admission stays full" $
        withSaturatedAdmission $ \runtime -> do
            withRealWarp (pastCapApplication runtime) $ \port ->
                withLoopbackSocket port $ \client ->
                    bracket (H2.allocSimpleConfig client 4096) H2.freeSimpleConfig $ \config ->
                        H2.run H2.defaultClientConfig config $ \sendRequest _aux ->
                            replicateM_ 2 $
                                sendRequest (H2.requestNoBody methodGet "/" []) $ \response -> do
                                    H2.responseStatus response `shouldBe` Just shedStatus
                                    let headers = [(tokenFoldedKey token, value) | (token, value) <- fst (H2.responseHeaders response)]
                                    lookup "retry-after" headers `shouldBe` Just "1"
                                    lookup "connection" headers `shouldBe` Nothing
                                    body <- boundedRead (MetadataBodyLimit 1024) (H2.getResponseBodyChunk response)
                                    body `shouldBe` Right (BS.length (encodeUtf8 shedMessage), encodeUtf8 shedMessage)

    it "finishes two streamed refusals on one reusable connection" $
        withPastCapResponse $ \headers -> do
            let app _ respond = respond (responseStream shedStatus headers (\write _ -> write "busy"))
            wire <- pipelined "HTTP/1.1" methodGet app
            countResponses wire `shouldBe` 2
            wire `shouldSatisfy` BS.isInfixOf "4\r\nbusy\r\n0\r\n\r\nHTTP/1.1 503 "
            wire `shouldSatisfy` BS.isSuffixOf "4\r\nbusy\r\n0\r\n\r\n"
            B8.map toLower wire `shouldSatisfy` (not . BS.isInfixOf "\r\nconnection:")

    it "processes the second ordinary request on the same connection" $ do
        wire <- pipelined "HTTP/1.1" methodGet (\_ respond -> respond (responseLBS status200 [] "ok"))
        countResponses wire `shouldBe` 2

    it "enforces decoded header bounds and reuses the HTTP/2 connection after a refusal" $ do
        entered <- newIORef (0 :: Int)
        let app _ respond = do
                atomicModifyIORef'_ entered (+ 1)
                respond (responseLBS status200 [] "ok")
        withRealWarp app $ \port ->
            withLoopbackSocket port $ \client ->
                bracket (H2.allocSimpleConfig client 4096) H2.freeSimpleConfig $ \config ->
                    H2.run H2.defaultClientConfig config $ \sendRequest _aux -> do
                        let request size = H2.requestNoBody methodGet "/" (replicate 80 ("x-padding", BS.replicate size 97))
                            accept response = do
                                H2.responseStatus response `shouldBe` Just status200
                                boundedRead (MetadataBodyLimit 1024) (H2.getResponseBodyChunk response)
                                    `shouldReturn` Right (2, "ok")
                        sendRequest (request 700) accept
                        sendRequest (request 800) accept `shouldThrow` \case
                            H2.StreamResetIsReceived H2.ProtocolError _ -> True
                            _ -> False
                        sendRequest (H2.requestNoBody methodGet "/" []) accept
        readIORef entered `shouldReturn` 2

    describe "decoded HTTP/2 header bounds" $ do
        it "uses a finite default header limit" $
            H2.maxHeaderListSize H2.defaultSettings `shouldBe` Just 65536

        it "accepts fields exactly at the decoded limit" $
            decodeBoundedHeaders (Just 72) [("x-a", "1"), ("x-b", "2")]
                `shouldReturn` [("x-a", "1"), ("x-b", "2")]

        it "refuses fields one octet above the decoded limit" $
            decodeBoundedHeaders (Just 71) [("x-a", "1"), ("x-b", "2")]
                `shouldThrow` (== HPACK.TooLargeHeader)

        it "counts pseudo-header names and field overhead" $
            decodeBoundedHeaders (Just 37) [(":path", "/")]
                `shouldThrow` (== HPACK.TooLargeHeader)

        it "refuses a non-empty block at a zero limit" $
            decodeBoundedHeaders (Just 0) [("x-a", "1")]
                `shouldThrow` (== HPACK.TooLargeHeader)

        it "accepts empty trailers at a zero limit" $
            decodeBoundedHeaders (Just 0) [] `shouldReturn` []

        it "joins ordinary repeated Cookie fields within the limit" $
            decodeBoundedHeaders (Just 82) [("cookie", "a=1"), ("cookie", "b=2")]
                `shouldReturn` [("cookie", "a=1; b=2")]

        it "retains ordinary repeated fields in order" $
            decodeBoundedHeaders (Just 72) [("x-a", "1"), ("x-a", "2")]
                `shouldReturn` [("x-a", "1"), ("x-a", "2")]

        it "retains the existing field-count bound" $
            decodeBoundedHeaders Nothing (replicate 203 ("x-a", "1"))
                `shouldThrow` (== HPACK.TooLargeHeader)

        it "keeps the decoder table current after a bound refusal" $
            HPACK.withDynamicTableForEncoding 4096 $ \encoder ->
                HPACK.withDynamicTableForDecoding 4096 4096 $ \decoder -> do
                    HPACK.setHeaderListSizeLimit (Just 36) decoder
                    initialBlock <- HPACK.encodeHeader HPACK.defaultEncodeStrategy 4096 encoder [("x-a", "1"), ("x-b", "2")]
                    HPACK.decodeTokenHeader decoder initialBlock `shouldThrow` (== HPACK.TooLargeHeader)
                    followingBlock <- HPACK.encodeHeader HPACK.defaultEncodeStrategy 4096 encoder [("x-b", "2")]
                    (fields, _) <- HPACK.decodeTokenHeader decoder followingBlock
                    [(tokenFoldedKey token, value) | (token, value) <- fields] `shouldBe` [("x-b", "2")]

decodeBoundedHeaders :: Maybe Int -> [(HeaderName, ByteString)] -> IO [(ByteString, ByteString)]
decodeBoundedHeaders limit fields =
    HPACK.withDynamicTableForEncoding 4096 $ \encoder ->
        HPACK.withDynamicTableForDecoding 4096 4096 $ \decoder -> do
            HPACK.setHeaderListSizeLimit limit decoder
            block <- HPACK.encodeHeader HPACK.defaultEncodeStrategy 4096 encoder fields
            (decoded, _) <- HPACK.decodeTokenHeader decoder block
            pure [(tokenFoldedKey token, value) | (token, value) <- decoded]

pastCapApplication :: ServeRuntime -> Application
pastCapApplication runtime request respond =
    withMetadataAdmission
        runtime
        (\headers -> bufferedShed headers request respond)
        -- An unexpected admission invalidates this refusal fixture.
        (const (throwIO (TestContractEscape "past-cap request entered")))
        pure

bufferedShed :: ResponseHeaders -> Application
bufferedShed headers _ respond =
    respond (responseLBS shedStatus ((hContentLength, show (BS.length body)) : headers) (LBS.fromStrict body))
  where
    body = encodeUtf8 shedMessage

assertShedHeaders :: ByteString -> ByteString -> Expectation
assertShedHeaders version wire = do
    wire `shouldSatisfy` BS.isPrefixOf (version <> " 503 ")
    let folded = B8.map toLower (wire <> "\r\n")
    folded `shouldSatisfy` BS.isInfixOf "\r\nretry-after: 1\r\n"
    folded `shouldSatisfy` (not . BS.isInfixOf "\r\nconnection:")

withPastCapResponse :: (ResponseHeaders -> IO ()) -> IO ()
withPastCapResponse action = withSaturatedAdmission $ \runtime ->
    withMetadataAdmission runtime action (const (expectationFailure "past-cap request entered")) pure

withSaturatedAdmission :: (ServeRuntime -> IO ()) -> IO ()
withSaturatedAdmission action = withSweeper $ \deadlines -> do
    overflow <- newOverflowTuned deadlines 0 500_000
    admission <- (`withOverflow` overflow) <$> newServeAdmissionTuned deadlines 1 0 0
    runtime <- testServeRuntimeWith deadlines admission noopMetricsPort
    entered <- newEmptyMVar
    blocked <- newEmptyMVar :: IO (MVar ())
    let hold = withServeAdmission noopMetricsPort admission (putMVar entered () >> takeMVar blocked)
    withAsync hold $ \holder -> do
        link holder
        takeMVar entered
        action runtime

-- A write half-close ends the fixture without asking either response to close the connection.
pipelined :: ByteString -> Method -> Application -> IO ByteString
pipelined version method app =
    withRealWarp app $ \port ->
        withLoopbackSocket port $ \client -> do
            sendAll client (method <> " / " <> version <> "\r\nHost: localhost\r\nConnection: keep-alive\r\n\r\nGET / " <> version <> "\r\nHost: localhost\r\nConnection: keep-alive\r\n\r\n")
            Socket.shutdown client Socket.ShutdownSend
            receiveAll client

withLoopbackSocket :: Int -> (Socket.Socket -> IO a) -> IO a
withLoopbackSocket port action =
    bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \client -> do
        Socket.connect client (Socket.SockAddrInet (fromIntegral port) (Socket.tupleToHostAddress (127, 0, 0, 1)))
        action client

withRealWarp :: Application -> (Int -> IO a) -> IO a
withRealWarp app action = withProductionListener "transport-test" (mkServerConfig []) (const (pure app)) (const . action)

receiveAll :: Socket.Socket -> IO ByteString
receiveAll client = do
    chunk <- recv client 4096
    if BS.null chunk then pure "" else (chunk <>) <$> receiveAll client

countResponses :: ByteString -> Int
countResponses bytes =
    case BS.breakSubstring "HTTP/1." bytes of
        (_, rest) | BS.null rest -> 0
        (_, rest) -> 1 + countResponses (BS.drop 7 rest)
