-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The transport classification a fetch reports its faults by, and the npm origin fixture's wiring.
module Ecluse.Core.Registry.NpmSpec (spec) where

import Network.HTTP.Client (
    HttpException (HttpExceptionRequest, InvalidUrlException),
    HttpExceptionContent (
        ConnectionClosed,
        ConnectionFailure,
        ConnectionTimeout,
        InternalException,
        NoResponseDataReceived,
        ResponseTimeout
    ),
    defaultManagerSettings,
    defaultRequest,
    newManager,
 )
import Network.TLS qualified as TLS
import Test.Hspec (Spec, describe, it, shouldBe)
import UnliftIO (evaluate)

import Ecluse.Core.Fault (
    TransportCause (TransportProtocol, TransportTimeout, TransportTls, TransportUnreachable),
    TransportFault (tfCause),
    transportRetryable,
 )
import Ecluse.Core.Fault.Http (classifyTransport)
import Ecluse.Core.Registry.Origin (OriginClient (..))
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress (mkRegistryUrl, registryUrlText)
import Ecluse.Test.Registry.Npm (defaultNpmConfig, publicRegistryBaseUrl)

spec :: Spec
spec = do
    transportFaultSpec
    configAndWiringSpec

-- | 'classifyTransport' folds each @http-client@ exception shape onto the bounded 'TransportCause'.
transportFaultSpec :: Spec
transportFaultSpec = describe "transport faults as values" $ do
    it "classifies timeouts as TransportTimeout" $ do
        causeOf (HttpExceptionRequest defaultRequest ConnectionTimeout) `shouldBe` TransportTimeout
        causeOf (HttpExceptionRequest defaultRequest ResponseTimeout) `shouldBe` TransportTimeout

    it "classifies connection failures and resets as TransportUnreachable" $ do
        causeOf (HttpExceptionRequest defaultRequest (ConnectionFailure (toException FakeInnerFault))) `shouldBe` TransportUnreachable
        causeOf (HttpExceptionRequest defaultRequest ConnectionClosed) `shouldBe` TransportUnreachable
        -- A peer that hung up before the first response byte never reached a protocol
        -- exchange, so it reads as unreachable rather than as a protocol fault.
        causeOf (HttpExceptionRequest defaultRequest NoResponseDataReceived) `shouldBe` TransportUnreachable

    it "classifies a wrapped TLS exception as TransportTls" $ do
        let handshake = toException (TLS.HandshakeFailed (TLS.Error_Misc "handshake refused"))
        causeOf (HttpExceptionRequest defaultRequest (InternalException handshake)) `shouldBe` TransportTls

    it "classifies every other client fault as TransportProtocol" $ do
        -- The closed catch-all keeps the sum total over whatever http-client reports.
        causeOf (HttpExceptionRequest defaultRequest (InternalException (toException FakeInnerFault))) `shouldBe` TransportProtocol
        causeOf (InvalidUrlException "::" "bad") `shouldBe` TransportProtocol

    it "retries a timeout and an unreachable peer, and nothing else" $ do
        -- One table decides transience for every classifyTransport consumer, so no
        -- caller re-derives it from the client library's constructors.
        map transportRetryable [TransportTimeout, TransportUnreachable] `shouldBe` [True, True]
        map transportRetryable [TransportTls, TransportProtocol] `shouldBe` [False, False]
  where
    causeOf = tfCause . classifyTransport

configAndWiringSpec :: Spec
configAndWiringSpec = describe "config wiring" $ do
    it "defaultNpmConfig targets the public registry anonymously over the given manager" $ do
        manager <- newManager defaultManagerSettings
        -- The production https-only former accepts the public registry, so this fixture needs
        -- no loopback opt-in to reach it.
        base <- either (fail . toString) pure (mkRegistryUrl publicRegistryBaseUrl)
        let config = defaultNpmConfig base manager
        registryUrlText (ocBaseUrl config) `shouldBe` publicRegistryBaseUrl
        isJust (ocToken config) `shouldBe` False
        -- The secure-default bounds apply to an anonymous public fetch out of the box. A
        -- deployment overrides them per its budget.
        ocLimits config `shouldBe` defaultLimits
        -- A 'Manager' is opaque (no Eq/Show), so forcing it to WHNF is the
        -- assertion that the field carries the manager we passed, not a bottom.
        _ <- evaluate (ocManager config)
        pure ()

-- | Classification must inspect the wrapped exception type rather than its rendered text.
data FakeInnerFault = FakeInnerFault
    deriving stock (Show)

instance Exception FakeInnerFault
