-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Registry.NpmSpec (spec) where

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
import Network.HTTP.Types.Status (status200)
import Network.TLS qualified as TLS
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import UnliftIO (evaluate)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Fault (
    TransportCause (TransportProtocol, TransportTimeout, TransportTls, TransportUnreachable),
    TransportFault (tfCause),
 )
import Ecluse.Core.Fault.Http (classifyTransport)
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Registry (
    FetchFault (FetchBoundExceeded, FetchTransport, FetchUrlUnformable),
    RegistryResponse (..),
    UrlFormationError (EmptyBaseUrl),
 )

import Ecluse.Core.Registry.Npm (
    NpmClientConfig (..),
    fetchMetadataFormBounded,
 )
import Ecluse.Core.Registry.Npm.Request (MetadataForm (Full), noValidators)
import Ecluse.Core.Security (defaultLimits, maxBodyBytes)
import Ecluse.Test.Registry.Npm (defaultNpmConfig, publicRegistryBaseUrl)

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

{- | The metadata fetch reads the upstream body through 'boundedRead' against the
config's 'npmLimits', so a body past 'maxBodyBytes' is refused fail-closed as a
'FetchBoundExceeded' __value__ rather than buffered whole, while a body within budget
is returned verbatim. This is the body-size half of invariant 4 at the @http-client@
boundary; the version-count and nesting-depth halves are enforced in the serve
pipeline's decode step (asserted through the request path in
"Ecluse.Server.PipelineSpec").
-}
boundedBodySpec :: Spec
boundedBodySpec = describe "bounded metadata body read" $ do
    it "refuses an over-cap body fail-closed as a FetchBoundExceeded value" $
        -- The stub serves a body larger than the tight cap; the bounded read must
        -- report the breach (never a truncated RegistryResponse), and it reports it
        -- as a value the serve adapter threads into a MetadataError with no
        -- throw-then-catch round-trip.
        withStub status200 (toLazy oversizedBody) $ \stub -> do
            base <- stubConfig stub
            let config = base{npmLimits = defaultLimits{maxBodyBytes = 64}}
            outcome <- fetchMetadataFormBounded config Full noValidators isOdd
            outcome `shouldSatisfy` isBoundExceeded

    it "returns a body that is within maxBodyBytes verbatim" $
        -- A body the cap admits is read whole and returned unchanged -- no false refusal.
        withStub status200 "{\"name\":\"is-odd\"}" $ \stub -> do
            base <- stubConfig stub
            let config = base{npmLimits = defaultLimits{maxBodyBytes = 64}}
            resp <- fetchMetadataFormBounded config Full noValidators isOdd
            fmap responseBody resp `shouldBe` Right "{\"name\":\"is-odd\"}"

    it "bounds DECOMPRESSED size: a small gzip body that inflates past the cap is refused" $
        -- The load-bearing security property: the metadata request advertises
        -- @Accept-Encoding: gzip@ and http-client decompresses transparently, so the
        -- cap must bound the inflated bytes, not the wire size. The stub serves a gzip
        -- body whose COMPRESSED size is well under the cap but whose DECOMPRESSED size
        -- is well over it; the bounded read must still refuse fail-closed. This guards
        -- against a future change silently moving the cap to compressed bytes (which a
        -- gzip bomb would then walk straight through).
        withStubHeaders status200 [(hContentEncoding, "gzip")] (toLazy gzippedOversizedBody) $ \stub -> do
            base <- stubConfig stub
            let config = base{npmLimits = defaultLimits{maxBodyBytes = 1024}}
            -- Sanity: the compressed body really is under the cap, so only the
            -- decompressed-size bound can explain a refusal.
            BS.length gzippedOversizedBody `shouldSatisfy` (< 1024)
            outcome <- fetchMetadataFormBounded config Full noValidators isOdd
            outcome `shouldSatisfy` isBoundExceeded

    it "reports an empty base URL as a FetchUrlUnformable value, never thrown" $ do
        -- The read-path URL-formation fault is a value (mirroring the write path's
        -- PublishUrlUnformable), not a thrown UrlFormationError laundered by a broad catch.
        manager <- newManager defaultManagerSettings
        let config = (defaultNpmConfig manager){npmBaseUrl = ""}
        outcome <- fetchMetadataFormBounded config Full noValidators isOdd
        outcome `shouldBe` Left (FetchUrlUnformable EmptyBaseUrl)

{- | The transport half of the typed fetch channel: 'classifyTransport' folds each
@http-client@ exception shape onto the bounded 'TransportCause' the logs and metrics
read, and the bounded fetch reports a live transport failure as a 'FetchTransport'
value rather than an escaping exception.
-}
transportFaultSpec :: Spec
transportFaultSpec = describe "transport faults as values" $ do
    it "classifies timeouts as TransportTimeout" $ do
        causeOf (HttpExceptionRequest defaultRequest ConnectionTimeout) `shouldBe` TransportTimeout
        causeOf (HttpExceptionRequest defaultRequest ResponseTimeout) `shouldBe` TransportTimeout

    it "classifies connection failures and resets as TransportUnreachable" $ do
        causeOf (HttpExceptionRequest defaultRequest (ConnectionFailure (toException FakeInnerFault))) `shouldBe` TransportUnreachable
        causeOf (HttpExceptionRequest defaultRequest ConnectionClosed) `shouldBe` TransportUnreachable

    it "classifies a wrapped TLS exception as TransportTls" $ do
        let handshake = toException (TLS.HandshakeFailed (TLS.Error_Misc "handshake refused"))
        causeOf (HttpExceptionRequest defaultRequest (InternalException handshake)) `shouldBe` TransportTls

    it "classifies every other client fault as TransportProtocol" $ do
        -- A non-TLS internal exception, a protocol-level fault, and an unparseable
        -- URL all land in the closed catch-all, so the sum stays total over
        -- whatever http-client reports.
        causeOf (HttpExceptionRequest defaultRequest (InternalException (toException FakeInnerFault))) `shouldBe` TransportProtocol
        causeOf (HttpExceptionRequest defaultRequest NoResponseDataReceived) `shouldBe` TransportProtocol
        causeOf (InvalidUrlException "::" "bad") `shouldBe` TransportProtocol

    it "reports a refused connection as a FetchTransport value, never thrown" $ do
        -- Port 1 on the loopback is privileged and unbound, so the connect is
        -- refused: the one live-transport case a unit test can drive determinately.
        manager <- newManager defaultManagerSettings
        let config = (defaultNpmConfig manager){npmBaseUrl = "http://127.0.0.1:1"}
        outcome <- fetchMetadataFormBounded config Full noValidators isOdd
        outcome `shouldSatisfy` isTransportFault
  where
    causeOf = tfCause . classifyTransport

configAndWiringSpec :: Spec
configAndWiringSpec = describe "config wiring" $ do
    it "defaultNpmConfig targets the public registry anonymously over the given manager" $ do
        manager <- newManager defaultManagerSettings
        let config = defaultNpmConfig manager
        npmBaseUrl config `shouldBe` publicRegistryBaseUrl
        isJust (npmToken config) `shouldBe` False
        -- The secure-default response bounds are carried, so an anonymous public
        -- fetch is bounded out of the box (a deployment overrides per its budget).
        npmLimits config `shouldBe` defaultLimits
        -- A 'Manager' is opaque (no Eq/Show), so forcing it to WHNF is the
        -- assertion that the field carries the manager we passed, not a bottom.
        _ <- evaluate (npmManager config)
        pure ()

isOdd :: PackageName
isOdd = mkPackageName Npm Nothing "is-odd"

-- A body comfortably larger than the tight 64-byte cap the bounded-body test sets.
oversizedBody :: ByteString
oversizedBody = "{\"name\":\"is-odd\",\"_padding\":\"" <> BS.replicate 256 0x78 <> "\"}"

{- | A gzip-compressed JSON body whose __decompressed__ size (a long run of one byte,
~64 KiB) far exceeds the 1 KiB cap the gzip test sets, while its __compressed__ size
stays well under it (a long single-byte run deflates tiny). Serving this under
@Content-Encoding: gzip@ proves the bounded read measures inflated, not wire, bytes.
-}
gzippedOversizedBody :: ByteString
gzippedOversizedBody =
    toStrict (GZip.compress (toLazy ("{\"name\":\"is-odd\",\"_padding\":\"" <> BS.replicate 65536 0x78 <> "\"}")))

{- | A typed stand-in for a client library's wrapped inner exception: 'ConnectionFailure'
and 'InternalException' carry a 'SomeException', and the classification must read the
wrapper's type (TLS or not), never the inner rendering.
-}
data FakeInnerFault = FakeInnerFault
    deriving stock (Show)

instance Exception FakeInnerFault

-- | Whether a bounded fetch returned the response-bound breach as a value.
isBoundExceeded :: Either FetchFault RegistryResponse -> Bool
isBoundExceeded = \case
    Left (FetchBoundExceeded _) -> True
    _ -> False

-- | Whether a bounded fetch returned a transport failure as a value.
isTransportFault :: Either FetchFault RegistryResponse -> Bool
isTransportFault = \case
    Left (FetchTransport _) -> True
    _ -> False
