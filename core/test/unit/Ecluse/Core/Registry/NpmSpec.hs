-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Registry.NpmSpec (spec) where

import Codec.Compression.GZip qualified as GZip
import Data.ByteString qualified as BS
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
import Network.HTTP.Types.Header (hContentEncoding)
import Network.HTTP.Types.Status (status200, status401, status403, statusCode)
import Network.TLS qualified as TLS
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import UnliftIO (evaluate)

import Ecluse.Core.Fault (
    TransportCause (TransportProtocol, TransportTimeout, TransportTls, TransportUnreachable),
    TransportFault (tfCause),
    transportRetryable,
 )
import Ecluse.Core.Fault.Http (classifyTransport)
import Ecluse.Core.Registry (
    FetchFault (FetchBoundExceeded, FetchUrlUnformable),
    UrlFormationError (EmptyBaseUrl),
 )

import Ecluse.Core.Registry.Metadata (Manifest (manifestBodyBytes, manifestDigest), MetadataError (..))
import Ecluse.Core.Registry.Npm.Metadata (fetchNpmManifest)
import Ecluse.Core.Registry.Origin (OriginClient (..))
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge), defaultLimits, maxMetadataBytes)
import Ecluse.Core.Security.Egress (mkRegistryUrl, registryUrlText)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Snapshot (digestOf)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Registry (isBoundExceededFetch, isTransportFetch)
import Ecluse.Test.Registry.Npm (defaultNpmConfig, isOdd, publicRegistryBaseUrl)

import Ecluse.Test.Stub (
    stubConfig,
    withStub,
    withStubHeaders,
 )

spec :: Spec
spec = do
    boundedBodySpec
    transportFaultSpec
    configAndWiringSpec

-- | The shipping metadata reader applies source byte bounds before projecting streamed fields.
boundedBodySpec :: Spec
boundedBodySpec = describe "bounded metadata body read" $ do
    for_ [status401, status403] $ \upstreamStatus ->
        it ("retains HTTP " <> show (statusCode upstreamStatus) <> " before an oversized error body") $
            withStub upstreamStatus (toLazy oversizedBody) $ \stub -> do
                base <- stubConfig loopbackRegistryUrl stub
                let config = base{ocLimits = defaultLimits{maxMetadataBytes = 64}}
                outcome <- fetchNpmManifest passthroughTracingPort config isOdd
                void outcome `shouldBe` Left (MetadataAuthorisationFailure (statusCode upstreamStatus))

    it "refuses an over-cap body fail-closed as a FetchBoundExceeded value" $
        withStub status200 (toLazy oversizedBody) $ \stub -> do
            base <- stubConfig loopbackRegistryUrl stub
            let config = base{ocLimits = defaultLimits{maxMetadataBytes = 64}}
            outcome <- fetchNpmManifest passthroughTracingPort config isOdd
            void outcome `shouldBe` Left (MetadataFetch (FetchBoundExceeded (BodyTooLarge (MetadataBodyLimit 64))))

    it "digests the complete source body within maxMetadataBytes" $
        withStub status200 "{\"name\":\"is-odd\"}" $ \stub -> do
            base <- stubConfig loopbackRegistryUrl stub
            let config = base{ocLimits = defaultLimits{maxMetadataBytes = 64}}
            resp <- fetchNpmManifest passthroughTracingPort config isOdd
            fmap manifestDigest resp `shouldBe` Right (digestOf "{\"name\":\"is-odd\"}")

    it "reports decompressed bytes for an accepted gzip body" $
        withStubHeaders status200 [(hContentEncoding, "gzip")] (GZip.compress (toLazy oversizedBody)) $ \stub -> do
            base <- stubConfig loopbackRegistryUrl stub
            let config = base{ocLimits = defaultLimits{maxMetadataBytes = BS.length oversizedBody}}
            resp <- fetchNpmManifest passthroughTracingPort config isOdd
            fmap manifestBodyBytes resp `shouldBe` Right (BS.length oversizedBody)
            fmap manifestDigest resp `shouldBe` Right (digestOf oversizedBody)

    it "bounds DECOMPRESSED size: a small gzip body that inflates past the cap is refused" $
        -- The size cap must cover decompressed bytes, including expansion from a gzip bomb.
        withStubHeaders status200 [(hContentEncoding, "gzip")] (toLazy gzippedOversizedBody) $ \stub -> do
            base <- stubConfig loopbackRegistryUrl stub
            let config = base{ocLimits = defaultLimits{maxMetadataBytes = 1024}}
            -- Sanity: the compressed body is under the cap, so only the
            -- decompressed-size bound can explain a refusal.
            BS.length gzippedOversizedBody `shouldSatisfy` (< 1024)
            outcome <- fetchNpmManifest passthroughTracingPort config isOdd
            fetchOutcome outcome `shouldSatisfy` isBoundExceededFetch

    it "reports an empty base URL as a FetchUrlUnformable value, never thrown" $ do
        -- The read-path URL-formation fault is a value (mirroring the write path's
        -- PublishFetch), not a thrown UrlFormationError laundered by a broad catch.
        manager <- newManager defaultManagerSettings
        let config = defaultNpmConfig (loopbackRegistryUrl "") manager
        outcome <- fetchNpmManifest passthroughTracingPort config isOdd
        void outcome `shouldBe` Left (MetadataFetch (FetchUrlUnformable EmptyBaseUrl))

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

    it "reports a refused connection as a FetchTransport value, never thrown" $ do
        -- Port 1 on the loopback is privileged and unbound, so the kernel refuses the
        -- connect. It is the one live-transport case a unit test can drive determinately.
        manager <- newManager defaultManagerSettings
        let config = defaultNpmConfig (loopbackRegistryUrl "http://127.0.0.1:1") manager
        outcome <- fetchNpmManifest passthroughTracingPort config isOdd
        fetchOutcome outcome `shouldSatisfy` isTransportFetch
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

-- A body larger than the tight 64-byte cap the bounded-body test sets.
oversizedBody :: ByteString
oversizedBody = "{\"name\":\"is-odd\",\"_padding\":\"" <> BS.replicate 256 0x78 <> "\"}"

-- | Keep compressed bytes below the cap while their expanded body exceeds it.
gzippedOversizedBody :: ByteString
gzippedOversizedBody =
    toStrict (GZip.compress (toLazy ("{\"name\":\"is-odd\",\"_padding\":\"" <> BS.replicate 65536 0x78 <> "\"}")))

-- | Classification must inspect the wrapped exception type rather than its rendered text.
data FakeInnerFault = FakeInnerFault
    deriving stock (Show)

instance Exception FakeInnerFault

fetchOutcome :: Either MetadataError a -> Either FetchFault ()
fetchOutcome = \case
    Left (MetadataFetch fault) -> Left fault
    _ -> Right ()
