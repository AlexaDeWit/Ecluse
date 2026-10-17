-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Client exceptions classified and rendered, over plain HTTP and through the TLS manager.
module Ecluse.Core.Fault.HttpSpec (spec) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Foreign.C.Error (Errno, eCONNRESET, eINVAL, eTIMEDOUT, errnoToIOError)
import Network.Connection (HostNotResolved (HostNotResolved))
import Network.HTTP.Client (
    HttpException (HttpExceptionRequest, InvalidUrlException),
    HttpExceptionContent (ConnectionClosed, ConnectionFailure, ConnectionTimeout, InternalException, NoResponseDataReceived, ResponseTimeout),
    ManagerSettings (managerModifyResponse),
    Request (secure),
    brConsume,
    brRead,
    defaultManagerSettings,
    defaultRequest,
    newManager,
    parseRequest,
    responseBody,
    responseStatus,
    withResponse,
 )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (status200, status404, status503)
import Network.Socket (ShutdownCmd (ShutdownSend), Socket, shutdown)
import Network.Socket.ByteString (recv, sendAll)
import Network.TLS qualified as TLS
import System.IO.Error (IOError)
import Test.Hspec
import UnliftIO.Exception (throwIO, try, tryAny)

import Ecluse.Core.Fault (
    HttpFault (HttpStatus),
    TransportCause (TransportProtocol, TransportTimeout, TransportTls, TransportUnreachable),
    TransportFault (tfCause, tfDetail),
    renderTransportCause,
    transportRetryable,
 )
import Ecluse.Core.Fault.Http (classifyTransport, displayFaultT, httpFault, typedBodyReads, typedTlsManagerSettings)
import Ecluse.Core.Text (displayExceptionT)
import Ecluse.Test.Fault (httpTransportCause)
import Ecluse.Test.Stub (closedAuthority, failedFetch, redirectSecrets, redirectedStatusException, refusedFetch, requestSecrets, signedRequest, stubBaseUrl, withStub)
import Ecluse.Test.Stub.Socket (resetAfterPartialBodyOverTcp, resetOnClose, withRawServer)

-- | A typed fault outside the HTTP client.
data OtherFault = OtherFault
    deriving stock (Show)

instance Exception OtherFault

spec :: Spec
spec = do
    describe "httpFault" $ do
        it "keeps the status the redirect target answered" $ do
            (_, err) <- redirectedStatusException status503
            httpFault err `shouldBe` HttpStatus 503

        it "keeps the transport fault when nothing answered" $ do
            refused <- refusedFetch =<< signedRequest
            httpTransportCause (httpFault refused) `shouldBe` Just TransportUnreachable

    describe "displayFaultT" $ do
        it "renders a status by its code at the dialled host:port, never the request or the response" $ do
            (authority, err) <- redirectedStatusException status404
            let rendered = displayFaultT (toException err)
            rendered `shouldBe` authority <> ": HTTP 404"
            for_ redirectSecrets $ \secret -> rendered `shouldSatisfy` (not . T.isInfixOf secret)

        it "renders a refused connection by the client's reason at the dialled host:port, then its cause, never the request" $ do
            refused <- refusedFetch =<< signedRequest
            let rendered = displayFaultT (toException refused)
            rendered `shouldSatisfy` T.isPrefixOf (closedAuthority <> ": ConnectionFailure: ")
            rendered `shouldSatisfy` T.isSuffixOf (" (" <> renderTransportCause TransportUnreachable <> ")")
            rendered `shouldSatisfy` (not . T.isInfixOf "\n")
            for_ requestSecrets $ \secret -> rendered `shouldSatisfy` (not . T.isInfixOf secret)

        it "renders any other exception as displayExceptionT does today" $
            displayFaultT (toException OtherFault) `shouldBe` displayExceptionT (toException OtherFault)

    describe "classifyTransport" $ do
        it "details a timeout by the dialled host:port and the client's reason, never the request" $ do
            request <- signedRequest
            let fault = classifyTransport (HttpExceptionRequest request ResponseTimeout)
            tfCause fault `shouldBe` TransportTimeout
            tfDetail fault `shouldBe` closedAuthority <> ": ResponseTimeout"

        it "details a TLS refusal by its own reason on one line, without GHC's context" $ do
            request <- signedRequest
            let fault = classifyTransport (HttpExceptionRequest request (InternalException (toException (TLS.HandshakeFailed (TLS.Error_Misc "refused")))))
            tfCause fault `shouldBe` TransportTls
            tfDetail fault `shouldBe` closedAuthority <> ": InternalException: HandshakeFailed (Error_Misc \"refused\")"

        it "details a redirected status by its code at the dialled host:port, never the response" $ do
            (authority, err) <- redirectedStatusException status503
            tfDetail (classifyTransport err) `shouldBe` authority <> ": StatusCodeException: HTTP 503"

        it "keeps a refused connection's own reason, and none of the request's path, query or headers" $ do
            fault <- classifyTransport <$> (refusedFetch =<< signedRequest)
            tfDetail fault `shouldSatisfy` T.isPrefixOf (closedAuthority <> ": ConnectionFailure: ")
            tfDetail fault `shouldSatisfy` (not . T.isInfixOf "\n")
            for_ requestSecrets $ \secret -> tfDetail fault `shouldSatisfy` (not . T.isInfixOf secret)

    describe "classifyTransport through the TLS manager" $ do
        it "classes a refused HTTPS connection as unreachable, as plain HTTP is, on one line and without the request" $ do
            request <- signedRequest
            fault <- classifyTransport <$> failedFetch tlsManagerSettings request{secure = True}
            tfCause fault `shouldBe` TransportUnreachable
            tfDetail fault `shouldSatisfy` T.isPrefixOf (closedAuthority <> ": InternalException: HostCannotConnect ")
            tfDetail fault `shouldSatisfy` (not . T.isInfixOf "\n")
            for_ requestSecrets $ \secret -> tfDetail fault `shouldSatisfy` (not . T.isInfixOf secret)

        it "classes the connector's empty address list as unreachable" $ do
            request <- signedRequest
            tfCause (classifyTransport (HttpExceptionRequest request (InternalException (toException (HostNotResolved "registry.invalid")))))
                `shouldBe` TransportUnreachable

        it "keeps a real TLS handshake failure as a TLS fault" $
            withRawServer answerNonTls $ \port -> do
                fault <- httpsFetchFault port
                tfCause fault `shouldBe` TransportTls

        it "classes a peer that closes before the handshake answer as unreachable, as plain HTTP is" $
            withRawServer hangUp $ \port -> do
                fault <- httpsFetchFault port
                tfCause fault `shouldBe` TransportUnreachable
                tfDetail fault `shouldBe` "127.0.0.1:" <> show port <> ": InternalException: HandshakeFailed Error_EOF"

        it "classes a peer that resets the handshake as unreachable" $
            withRawServer resetConnection $ \port -> do
                fault <- httpsFetchFault port
                tfCause fault `shouldBe` TransportUnreachable
                tfDetail fault `shouldSatisfy` T.isPrefixOf ("127.0.0.1:" <> show port <> ": InternalException: HandshakeFailed (Error_Misc ")

    transportFaultSpec
    bodyReadSpec

-- | 'classifyTransport' folds each @http-client@ exception shape onto the bounded 'TransportCause'.
transportFaultSpec :: Spec
transportFaultSpec = describe "transport faults as values" $ do
    it "classifies timeouts as TransportTimeout" $ do
        causeOf (HttpExceptionRequest defaultRequest ConnectionTimeout) `shouldBe` TransportTimeout
        causeOf (HttpExceptionRequest defaultRequest ResponseTimeout) `shouldBe` TransportTimeout

    it "classifies a socket that timed out after connect over plain HTTP as TransportTimeout" $
        causeOf (HttpExceptionRequest defaultRequest (InternalException (toException (socketError eTIMEDOUT)))) `shouldBe` TransportTimeout

    it "classifies a socket that timed out in the TLS handshake as TransportTimeout" $
        causeOf (HttpExceptionRequest defaultRequest (InternalException (handshakeSocketError eTIMEDOUT))) `shouldBe` TransportTimeout

    it "classifies connection failures and resets as TransportUnreachable" $ do
        causeOf (HttpExceptionRequest defaultRequest (ConnectionFailure (toException OtherFault))) `shouldBe` TransportUnreachable
        causeOf (HttpExceptionRequest defaultRequest ConnectionClosed) `shouldBe` TransportUnreachable
        causeOf (HttpExceptionRequest defaultRequest NoResponseDataReceived) `shouldBe` TransportUnreachable
        causeOf (HttpExceptionRequest defaultRequest (InternalException (toException (socketError eCONNRESET)))) `shouldBe` TransportUnreachable
        causeOf (HttpExceptionRequest defaultRequest (InternalException (handshakeSocketError eCONNRESET))) `shouldBe` TransportUnreachable

    it "classifies a wrapped TLS exception as TransportTls" $ do
        let handshake = toException (TLS.HandshakeFailed (TLS.Error_Misc "handshake refused"))
            certificate = toException (TLS.HandshakeFailed (TLS.Error_Protocol "certificate has unknown CA" TLS.UnknownCa))
        causeOf (HttpExceptionRequest defaultRequest (InternalException handshake)) `shouldBe` TransportTls
        causeOf (HttpExceptionRequest defaultRequest (InternalException certificate)) `shouldBe` TransportTls
        causeOf (HttpExceptionRequest defaultRequest (InternalException (handshakeSocketError eINVAL))) `shouldBe` TransportTls
        -- Only a socket error type between base's delimiters, and only inside Error_Misc, reads as a socket cause.
        causeOf (HttpExceptionRequest defaultRequest (InternalException (toException (TLS.HandshakeFailed (TLS.Error_Misc "handshake timeout"))))) `shouldBe` TransportTls
        causeOf (HttpExceptionRequest defaultRequest (InternalException (toException (TLS.HandshakeFailed (TLS.Error_Protocol "recv: timeout (Connection timed out)" TLS.HandshakeFailure))))) `shouldBe` TransportTls

    it "classifies every other client fault as TransportProtocol" $ do
        -- The closed catch-all keeps the sum total over whatever http-client reports.
        causeOf (HttpExceptionRequest defaultRequest (InternalException (toException OtherFault))) `shouldBe` TransportProtocol
        causeOf (HttpExceptionRequest defaultRequest (InternalException (toException (socketError eINVAL)))) `shouldBe` TransportProtocol
        causeOf (InvalidUrlException "::" "bad") `shouldBe` TransportProtocol

    it "retries a timeout and an unreachable peer, and nothing else" $ do
        -- One table decides transience for every classifyTransport consumer, so no
        -- caller re-derives it from the client library's constructors.
        map transportRetryable [TransportTimeout, TransportUnreachable] `shouldBe` [True, True]
        map transportRetryable [TransportTls, TransportProtocol] `shouldBe` [False, False]
  where
    causeOf = tfCause . classifyTransport

-- | 'typedBodyReads' raises a socket or TLS failure in a body read as the client's own exception, which 'classifyTransport' types.
bodyReadSpec :: Spec
bodyReadSpec = describe "typedBodyReads" $ do
    it "types a plain-HTTP reset after the status arrived as unreachable, under the settings every manager starts from" $
        withRawServer resetAfterPartialBodyOverTcp $ \port -> do
            manager <- newManager typedTlsManagerSettings
            request <- parseRequest ("http://127.0.0.1:" <> show port <> "/feed")
            status <- newIORef Nothing
            outcome <- try (withResponse request manager (\response -> writeIORef status (Just (responseStatus response)) >> brConsume (responseBody response)))
            readIORef status `shouldReturn` Just status200
            either (Just . tfCause . classifyTransport) (const Nothing) outcome `shouldBe` Just TransportUnreachable

    it "types a socket timeout in a body read as a timeout, and a TLS alert as TLS" $ do
        causes <- traverse (fmap typedCause . typedReadOf) [toException (socketError eTIMEDOUT), fatalAlert]
        causes `shouldBe` [Just TransportTimeout, Just TransportTls]

    it "leaves any other exception from a body read as it is" $
        typedReadOf (toException OtherFault) >>= (`shouldSatisfy` isJust . fromException @OtherFault)
  where
    typedCause = fmap (tfCause . classifyTransport) . fromException
    fatalAlert = toException (TLS.Terminated True "received fatal error: BadRecordMac" (TLS.Error_Protocol "remote side fatal error" TLS.BadRecordMac))

-- What a body read raises through 'typedBodyReads' when the read itself raises this exception.
typedReadOf :: SomeException -> IO SomeException
typedReadOf err = withStub status200 "body" $ \stub -> do
    manager <- newManager (typedBodyReads defaultManagerSettings{managerModifyResponse = pure . (throwIO err <$)})
    request <- parseRequest (toString (stubBaseUrl stub))
    tryAny (withResponse request manager (brRead . responseBody)) >>= either pure (const (fail "the body read succeeded"))

-- The fault an HTTPS fetch of a local port raises through the TLS manager.
httpsFetchFault :: Int -> IO TransportFault
httpsFetchFault port = classifyTransport <$> (failedFetch tlsManagerSettings =<< parseRequest ("https://127.0.0.1:" <> show port <> "/feed"))

-- The IOException network raises for a socket call that failed with this errno.
socketError :: Errno -> IOError
socketError errno = errnoToIOError "Network.Socket.recvBuf" errno Nothing Nothing

-- The handshake failure tls raises when a socket call fails with this errno: it keeps only the shown error.
handshakeSocketError :: Errno -> SomeException
handshakeSocketError errno = toException (TLS.HandshakeFailed (TLS.Error_Misc (show (socketError errno))))

answerNonTls :: Socket -> IO ()
answerNonTls conn = sendAll conn "HTTP/1.1 400 not TLS\r\n\r\n" >> drain conn

-- Await the ClientHello, then send FIN and read until the client hangs up, so the close sends no RST.
hangUp :: Socket -> IO ()
hangUp conn = recv conn 4096 >> shutdown conn ShutdownSend >> drain conn

-- Await the ClientHello, so the reset lands in the handshake and not in connect.
resetConnection :: Socket -> IO ()
resetConnection conn = recv conn 4096 >> resetOnClose conn

drain :: Socket -> IO ()
drain conn = recv conn 4096 >>= \chunk -> unless (BS.null chunk) (drain conn)
