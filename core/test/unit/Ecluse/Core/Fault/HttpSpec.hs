-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Fault.HttpSpec (spec) where

import Data.Text qualified as T
import Network.HTTP.Client (HttpException (HttpExceptionRequest), HttpExceptionContent (ResponseTimeout))
import Network.HTTP.Types.Status (status404, status503)
import Test.Hspec

import Ecluse.Core.Fault (TransportCause (TransportTimeout, TransportUnreachable), TransportFault (tfCause, tfDetail), renderTransportCause)
import Ecluse.Core.Fault.Http (HttpFault (HttpStatus, HttpTransport), classifyTransport, displayFaultT, httpFault)
import Ecluse.Test.Stub (redirectSecrets, redirectedStatusException, refusedFetch, requestSecrets, signedRequest)

-- | A typed fault outside the HTTP client, which renders as its own 'displayException'.
data OtherFault = OtherFault
    deriving stock (Show)

instance Exception OtherFault where
    displayException OtherFault = "another fault"

spec :: Spec
spec = do
    describe "httpFault" $ do
        it "keeps the status the redirect target answered" $ do
            (_, err) <- redirectedStatusException status503
            httpFault err `shouldBe` HttpStatus 503

        it "keeps the transport cause when nothing answered" $ do
            refused <- refusedFetch =<< signedRequest
            httpFault refused `shouldBe` HttpTransport TransportUnreachable

    describe "displayFaultT" $ do
        it "renders a status by its code at the dialled host:port, never the request or the response" $ do
            (authority, err) <- redirectedStatusException status404
            let rendered = displayFaultT (toException err)
            rendered `shouldBe` authority <> ": HTTP 404"
            for_ redirectSecrets $ \secret -> rendered `shouldSatisfy` (not . T.isInfixOf secret)

        it "renders a refused connection by its classified cause at the dialled host:port" $ do
            refused <- refusedFetch =<< signedRequest
            displayFaultT (toException refused) `shouldBe` "127.0.0.1:1: " <> renderTransportCause TransportUnreachable

        it "renders any other exception as its own displayException" $
            displayFaultT (toException OtherFault) `shouldBe` "another fault"

    describe "classifyTransport" $ do
        it "details a timeout by the dialled host:port and the client's cause, never the request" $ do
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
            for_ requestSecrets $ \secret -> tfDetail fault `shouldSatisfy` (not . T.isInfixOf secret)
