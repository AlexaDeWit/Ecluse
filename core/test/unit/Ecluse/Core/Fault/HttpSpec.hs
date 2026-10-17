-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Client exceptions classified and rendered, over plain HTTP and through the TLS manager.
module Ecluse.Core.Fault.HttpSpec (spec) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Network.Connection (HostNotResolved (HostNotResolved))
import Network.HTTP.Client (HttpException (HttpExceptionRequest), HttpExceptionContent (InternalException, ResponseTimeout), Request (secure), parseRequest)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (status404, status503)
import Network.Socket (accept, close)
import Network.Socket.ByteString (recv, sendAll)
import Network.TLS qualified as TLS
import Network.Wai.Handler.Warp (openFreePort)
import Test.Hspec
import UnliftIO.Async (withAsync)
import UnliftIO.Exception (bracket, finally)

import Ecluse.Core.Fault (HttpFault (HttpStatus), TransportCause (TransportTimeout, TransportTls, TransportUnreachable), TransportFault (tfCause, tfDetail), renderTransportCause)
import Ecluse.Core.Fault.Http (classifyTransport, displayFaultT, httpFault)
import Ecluse.Core.Text (displayExceptionT)
import Ecluse.Test.Fault (httpTransportCause)
import Ecluse.Test.Stub (failedFetch, redirectSecrets, redirectedStatusException, refusedFetch, requestSecrets, signedRequest)

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
            rendered `shouldSatisfy` T.isPrefixOf "127.0.0.1:1: ConnectionFailure: "
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
            tfDetail fault `shouldBe` "127.0.0.1:1: ResponseTimeout"

        it "details a TLS refusal by its own reason on one line, without GHC's context" $ do
            request <- signedRequest
            let fault = classifyTransport (HttpExceptionRequest request (InternalException (toException (TLS.HandshakeFailed (TLS.Error_Misc "refused")))))
            tfCause fault `shouldBe` TransportTls
            tfDetail fault `shouldBe` "127.0.0.1:1: InternalException: HandshakeFailed (Error_Misc \"refused\")"

        it "details a redirected status by its code at the dialled host:port, never the response" $ do
            (authority, err) <- redirectedStatusException status503
            tfDetail (classifyTransport err) `shouldBe` authority <> ": StatusCodeException: HTTP 503"

        it "keeps a refused connection's own reason, and none of the request's path, query or headers" $ do
            fault <- classifyTransport <$> (refusedFetch =<< signedRequest)
            tfDetail fault `shouldSatisfy` T.isPrefixOf "127.0.0.1:1: ConnectionFailure: "
            tfDetail fault `shouldSatisfy` (not . T.isInfixOf "\n")
            for_ requestSecrets $ \secret -> tfDetail fault `shouldSatisfy` (not . T.isInfixOf secret)

    describe "classifyTransport through the TLS manager" $ do
        it "classes a refused HTTPS connection as unreachable, as plain HTTP is, on one line and without the request" $ do
            request <- signedRequest
            fault <- classifyTransport <$> failedFetch tlsManagerSettings request{secure = True}
            tfCause fault `shouldBe` TransportUnreachable
            tfDetail fault `shouldSatisfy` T.isPrefixOf "127.0.0.1:1: InternalException: HostCannotConnect "
            tfDetail fault `shouldSatisfy` (not . T.isInfixOf "\n")
            for_ requestSecrets $ \secret -> tfDetail fault `shouldSatisfy` (not . T.isInfixOf secret)

        it "classes an unresolvable HTTPS host as unreachable, as plain HTTP is" $ do
            fault <- classifyTransport <$> (failedFetch tlsManagerSettings =<< parseRequest "https://ecluse-unresolvable.invalid/feed")
            tfCause fault `shouldBe` TransportUnreachable

        it "classes the connector's empty address list as unreachable" $ do
            request <- signedRequest
            tfCause (classifyTransport (HttpExceptionRequest request (InternalException (toException (HostNotResolved "registry.invalid")))))
                `shouldBe` TransportUnreachable

        it "keeps a real TLS handshake failure as a TLS fault" $
            withNonTlsServer $ \port -> do
                fault <- classifyTransport <$> (failedFetch tlsManagerSettings =<< parseRequest ("https://127.0.0.1:" <> show port <> "/feed"))
                tfCause fault `shouldBe` TransportTls

-- Answer every connection with bytes that are not a TLS record, then read until the client hangs up.
withNonTlsServer :: (Int -> IO a) -> IO a
withNonTlsServer action = bracket openFreePort (close . snd) $ \(port, listener) ->
    withAsync (forever (accept listener >>= \(conn, _) -> (sendAll conn "HTTP/1.1 400 not TLS\r\n\r\n" >> drain conn) `finally` close conn)) $ \_ ->
        action port
  where
    drain conn = recv conn 4096 >>= \chunk -> unless (BS.null chunk) (drain conn)
