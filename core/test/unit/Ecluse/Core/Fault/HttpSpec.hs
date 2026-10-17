-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Fault.HttpSpec (spec) where

import Data.Text qualified as T
import Network.HTTP.Client (HttpException (HttpExceptionRequest), HttpExceptionContent (ResponseTimeout))
import Network.HTTP.Types.Status (status404, status503)
import Test.Hspec

import Ecluse.Core.Fault (HttpFault (HttpStatus), TransportCause (TransportTimeout, TransportUnreachable), TransportFault (tfCause, tfDetail), renderTransportCause)
import Ecluse.Core.Fault.Http (classifyTransport, displayFaultT, httpFault)
import Ecluse.Core.Text (displayExceptionT)
import Ecluse.Test.Fault (httpTransportCause)
import Ecluse.Test.Stub (redirectSecrets, redirectedStatusException, refusedFetch, requestSecrets, signedRequest)

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

        it "details a redirected status by its code at the dialled host:port, never the response" $ do
            (authority, err) <- redirectedStatusException status503
            tfDetail (classifyTransport err) `shouldBe` authority <> ": StatusCodeException: HTTP 503"

        it "keeps a refused connection's own reason, and none of the request's path, query or headers" $ do
            fault <- classifyTransport <$> (refusedFetch =<< signedRequest)
            tfDetail fault `shouldSatisfy` T.isPrefixOf "127.0.0.1:1: ConnectionFailure: "
            tfDetail fault `shouldSatisfy` (not . T.isInfixOf "\n")
            for_ requestSecrets $ \secret -> tfDetail fault `shouldSatisfy` (not . T.isInfixOf secret)
