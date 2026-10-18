-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Which escaping exceptions the span net records by a typed cause, and which it leaves to the library.
module Ecluse.Runtime.Telemetry.EscapeSpec (spec) where

import Amazonka qualified as AWS
import Data.Text qualified as T
import Network.HTTP.Client (HttpException (HttpExceptionRequest), HttpExceptionContent (InvalidProxyEnvironmentVariable, ResponseTimeout))
import Network.HTTP.Client.Internal (throwHttp)
import OpenTelemetry.Trace (TracerProviderOptions (tracerProviderOptionsExceptionHandlers), emptyTracerProviderOptions)
import OpenTelemetry.Trace.Core (ExceptionClassification (IgnoredException), ExceptionResponse (ExceptionResponse))
import Test.Hspec
import UnliftIO.Exception (tryAny)

import Ecluse.Core.Fault (TransportCause (TransportTimeout), renderTransportCause)
import Ecluse.Core.Fault.Http (displayFaultT)
import Ecluse.Runtime.Telemetry.Escape (escapedRequestCause, withEscapeHandler)
import Ecluse.Test.Stub (requestSecrets, signedRequest)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

spec :: Spec
spec = do
    describe "escapedRequestCause" $ do
        it "renders an HTTP exception through its typed cause, never its request" $ do
            timedOut <- toException . (`HttpExceptionRequest` ResponseTimeout) <$> signedRequest
            escapedRequestCause timedOut `shouldBe` Just (displayFaultT timedOut)
            for_ requestSecrets $ \secret -> displayFaultT timedOut `shouldSatisfy` (not . T.isInfixOf secret)

        it "renders an amazonka transport error through its classified fault, never its request" $ do
            timedOut <- (`HttpExceptionRequest` ResponseTimeout) <$> signedRequest
            escapedRequestCause (toException (AWS.TransportError timedOut))
                `shouldBe` Just ("127.0.0.1:1: ResponseTimeout (" <> renderTransportCause TransportTimeout <> ")")

        it "renders a client fault raised outside a request through its typed cause, never its proxy value, and the library ignores it" $ do
            raised <- tryAny (throwHttp (InvalidProxyEnvironmentVariable "https_proxy" "https://user:proxy-secret@proxy.example:3128") :: IO ())
            case raised of
                Left wrapper -> do
                    escapedRequestCause wrapper `shouldBe` Just (displayFaultT wrapper)
                    displayFaultT wrapper `shouldNotSatisfy` T.isInfixOf "proxy-secret"
                    ignored wrapper `shouldBe` True
                Right () -> expectationFailure "throwHttp returned"

        it "leaves any other exception to the library" $
            escapedRequestCause (toException (TestContractEscape "contract broken")) `shouldBe` Nothing

    describe "withEscapeHandler" $
        it "tells the library to ignore a request-carrying exception, and no other" $ do
            timedOut <- toException . (`HttpExceptionRequest` ResponseTimeout) <$> signedRequest
            ignored timedOut `shouldBe` True
            ignored (toException (TestContractEscape "contract broken")) `shouldBe` False

-- Whether the handlers 'withEscapeHandler' installs classify the exception as ignored.
ignored :: SomeException -> Bool
ignored err = case asum (map ($ err) (tracerProviderOptionsExceptionHandlers (withEscapeHandler emptyTracerProviderOptions))) of
    Just (ExceptionResponse IgnoredException _) -> True
    _ -> False
