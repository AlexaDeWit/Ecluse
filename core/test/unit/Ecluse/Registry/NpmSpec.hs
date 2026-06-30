module Ecluse.Registry.NpmSpec (spec) where

import Codec.Compression.GZip qualified as GZip
import Data.ByteString qualified as BS
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types.Header (hContentEncoding)
import Network.HTTP.Types.Status (status200)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import UnliftIO (evaluate, try)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Registry (
    RegistryClient (parsePackageInfo, parseVersionDetails, parseVersionList),
    RegistryResponse (..),
 )

import Ecluse.Core.Registry.Npm (
    NpmClientConfig (..),
    defaultNpmConfig,
    fetchMetadataForm,
    newNpmClient,
    publicRegistryBaseUrl,
 )
import Ecluse.Core.Registry.Npm.Request (MetadataForm (Full), noValidators)
import Ecluse.Core.Security (defaultLimits, maxBodyBytes)
import Ecluse.Core.Version (Version, mkVersion)

import Ecluse.Test.Stub (
    stubConfig,
    withStub,
    withStubHeaders,
 )

spec :: Spec
spec = do
    boundedBodySpec
    configAndWiringSpec

{- | The metadata fetch reads the upstream body through 'boundedRead' against the
config's 'npmLimits', so a body past 'maxBodyBytes' is aborted fail-closed (an 'IO'
exception) rather than buffered whole, while a body within budget is returned
verbatim. This is the body-size half of invariant 4 at the @http-client@ boundary;
the version-count and nesting-depth halves are enforced in the serve pipeline's
decode step (asserted through the request path in
"Ecluse.Server.PipelineSpec").
-}
boundedBodySpec :: Spec
boundedBodySpec = describe "bounded metadata body read" $ do
    it "aborts fail-closed when the upstream body exceeds maxBodyBytes" $
        -- The stub serves a body larger than the tight cap; the bounded read must raise
        -- rather than return a (truncated) RegistryResponse.
        withStub status200 (toLazy oversizedBody) $ \stub -> do
            base <- stubConfig stub
            let config = base{npmLimits = defaultLimits{maxBodyBytes = 64}}
            outcome <- try (fetchMetadataForm config Full noValidators isOdd)
            outcome `shouldSatisfy` threw

    it "returns a body that is within maxBodyBytes verbatim" $
        -- A body the cap admits is read whole and returned unchanged -- no false refusal.
        withStub status200 "{\"name\":\"is-odd\"}" $ \stub -> do
            base <- stubConfig stub
            let config = base{npmLimits = defaultLimits{maxBodyBytes = 64}}
            resp <- fetchMetadataForm config Full noValidators isOdd
            responseBody resp `shouldBe` "{\"name\":\"is-odd\"}"

    it "bounds DECOMPRESSED size: a small gzip body that inflates past the cap aborts" $
        -- The load-bearing security property: the metadata request advertises
        -- @Accept-Encoding: gzip@ and http-client decompresses transparently, so the
        -- cap must bound the inflated bytes, not the wire size. The stub serves a gzip
        -- body whose COMPRESSED size is well under the cap but whose DECOMPRESSED size
        -- is well over it; the bounded read must still abort fail-closed. This guards
        -- against a future change silently moving the cap to compressed bytes (which a
        -- gzip bomb would then walk straight through).
        withStubHeaders status200 [(hContentEncoding, "gzip")] (toLazy gzippedOversizedBody) $ \stub -> do
            base <- stubConfig stub
            let config = base{npmLimits = defaultLimits{maxBodyBytes = 1024}}
            -- Sanity: the compressed body really is under the cap, so only the
            -- decompressed-size bound can explain a refusal.
            BS.length gzippedOversizedBody `shouldSatisfy` (< 1024)
            outcome <- try (fetchMetadataForm config Full noValidators isOdd)
            outcome `shouldSatisfy` threw

configAndWiringSpec :: Spec
configAndWiringSpec = describe "config and handle wiring" $ do
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

    it "wires the parse* projections into the handle's pure fields" $ do
        manager <- newManager defaultManagerSettings
        client <- newNpmClient (defaultNpmConfig manager)
        -- A minimal packument projects through the fields the client installed,
        -- proving each pure projection is reachable via the assembled handle.
        let resp = RegistryResponse "{\"name\":\"is-odd\"}"
        case parsePackageInfo client isOdd resp of
            Left err -> fail ("expected a successful projection, got: " <> show err)
            Right _info -> pure ()
        -- No versions in this body, so the version list is empty and a
        -- per-version lookup is absent -- both reach the wired field.
        parseVersionList client resp `shouldBe` Right []
        parseVersionDetails client resp v1 `shouldSatisfy` isLeft

isOdd :: PackageName
isOdd = mkPackageName Npm Nothing "is-odd"

v1 :: Version
v1 = mkVersion Npm "1.0.0"

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

threw :: Either SomeException RegistryResponse -> Bool
threw = isLeft
