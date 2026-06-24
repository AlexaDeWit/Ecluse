module Ecluse.Registry.NpmSpec (spec) where

import Codec.Compression.GZip qualified as GZip
import Control.Exception (try)
import Data.ByteString qualified as BS
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.Text qualified as T
import Network.HTTP.Client (
    Request (decompress),
    defaultManagerSettings,
    newManager,
 )
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types (Header, hContentEncoding)
import Network.HTTP.Types.Status (
    Status,
    status200,
    status404,
    status409,
    status500,
 )
import Network.Wai (
    Application,
    rawPathInfo,
    requestHeaders,
    requestMethod,
    responseLBS,
    strictRequestBody,
 )
import Network.Wai.Handler.Warp (Port, testWithApplication)
import Test.Hspec (
    Spec,
    around,
    describe,
    it,
    shouldBe,
    shouldNotSatisfy,
    shouldReturn,
    shouldSatisfy,
 )
import UnliftIO (evaluate)

import Ecluse.Credential (mkSecret)
import Ecluse.Ecosystem (Ecosystem (Npm))
import Ecluse.Package (PackageInfo (infoName), PackageName, mkPackageName, mkScope, renderPackageName)
import Ecluse.Registry (
    PublishError (publishErrorMessage),
    PublishFault (PublishRejected, PublishUrlUnformable),
    RegistryClient (
        fetchArtifact,
        fetchMetadata,
        parsePackageInfo,
        parseVersionDetails,
        parseVersionList,
        publishArtifact
    ),
    RegistryResponse (RegistryResponse, responseBody),
    UrlFormationError (EmptyBaseUrl, UnparseableUrl),
 )
import Ecluse.Security (Limits (maxBodyBytes), defaultLimits)

import Ecluse.Registry.Npm (
    MetadataForm (Abbreviated, Full),
    NpmClientConfig (..),
    Validators (..),
    artifactFileUrl,
    artifactRequest,
    artifactRequestByFile,
    artifactRequestByUrl,
    defaultNpmConfig,
    fetchMetadataForm,
    metadataAccept,
    metadataRequest,
    newNpmClient,
    noValidators,
    publicRegistryBaseUrl,
    publishRequest,
 )
import Ecluse.Version (Version, mkVersion)

{- | Request-shaping tests for the npm data plane. They drive 'newNpmClient' and
the exposed request builders against an __in-process WAI stub__ standing in for a
registry, so the wire requests Écluse emits — and the way it classifies the
responses — are asserted without any network.

The cases pin the protocol details the slice calls out: the @Accept@ /
@Accept-Encoding@ content negotiation, the scoped-package @%2F@ path encoding, the
injected bearer token, relayed conditional-GET validators, the non-decompressing
artifact request (so a tarball streams byte-for-byte), and — the subtle one —
__idempotent publish on HTTP 409__, where a re-published immutable version is
success, not an error.
-}
spec :: Spec
spec = do
    requestShapingSpec
    boundedBodySpec
    pathEncodingSpec
    authSpec
    artifactSpec
    publishSpec
    urlFailureSpec
    configAndWiringSpec

-- ── a recording WAI stub ──────────────────────────────────────────────────────

{- | What the stub captured from the request it last served: enough to assert the
method, path, and headers Écluse sent.
-}
data Captured = Captured
    { capMethod :: ByteString
    , capPath :: ByteString
    , capHeaders :: [(CI ByteString, ByteString)]
    , capBody :: ByteString
    }
    deriving stock (Eq, Show)

{- | A running stub: the ephemeral 'Port' it listens on and the slot holding the
most recent 'Captured' request (the handle talks to @127.0.0.1:port@).
-}
data Stub = Stub
    { stubPort :: Port
    , stubCaptured :: IORef (Maybe Captured)
    }

-- | The base URL of a running stub.
stubBaseUrl :: Stub -> Text
stubBaseUrl stub = "http://127.0.0.1:" <> show (stubPort stub)

-- | The request the stub last captured (or fail loudly if it served none).
lastCaptured :: Stub -> IO Captured
lastCaptured stub =
    readIORef (stubCaptured stub)
        >>= maybe (fail "stub served no request") pure

{- | Run an action against a stub that records each request and answers every one
with a fixed status and body. @testWithApplication@ binds a free port for the
action's duration, so the test never collides on a fixed port.
-}
withStub :: Status -> LByteString -> (Stub -> IO a) -> IO a
withStub status = withStubHeaders status []

{- | 'withStub' with extra response headers — e.g. @Content-Encoding: gzip@ so the
@http-client@ body reader decompresses the served bytes, letting a test assert the
bounded read bounds /decompressed/ size rather than wire size.
-}
withStubHeaders :: Status -> [Header] -> LByteString -> (Stub -> IO a) -> IO a
withStubHeaders status extraHeaders body action = do
    captured <- newIORef Nothing
    let app :: Application
        app waiReq respond = do
            bodyBytes <- strictRequestBody waiReq
            let cap =
                    Captured
                        { capMethod = requestMethod waiReq
                        , capPath = rawPathInfo waiReq
                        , capHeaders = requestHeaders waiReq
                        , capBody = toStrict bodyBytes
                        }
            atomicModifyIORef' captured (const (Just cap, ()))
            respond (responseLBS status extraHeaders body)
    testWithApplication (pure app) $ \port ->
        action Stub{stubPort = port, stubCaptured = captured}

-- | A config pointed at a stub, anonymous, sharing a no-TLS manager.
stubConfig :: Stub -> IO NpmClientConfig
stubConfig stub = do
    manager <- newManager defaultManagerSettings
    pure
        NpmClientConfig
            { npmBaseUrl = stubBaseUrl stub
            , npmManager = manager
            , npmToken = Nothing
            , npmLimits = defaultLimits
            }

-- | Look up a header (case-insensitively) in a captured request.
headerValue :: ByteString -> Captured -> Maybe ByteString
headerValue name cap = snd <$> find ((== CI.mk name) . fst) (capHeaders cap)

-- ── content negotiation ──────────────────────────────────────────────────────

requestShapingSpec :: Spec
requestShapingSpec =
    around (withStub status200 "{}") $
        describe "fetchMetadata request shaping" $ do
            it "the handle's fetchMetadata requests the abbreviated form with gzip" $ \stub -> do
                config <- stubConfig stub
                client <- newNpmClient config
                _ <- fetchMetadata client isOdd
                cap <- lastCaptured stub
                capMethod cap `shouldBe` "GET"
                headerValue "Accept" cap `shouldBe` Just (metadataAccept Abbreviated)
                headerValue "Accept-Encoding" cap `shouldBe` Just "gzip"

            it "fetchMetadataForm Full requests the full packument (Accept: application/json)" $ \stub -> do
                config <- stubConfig stub
                _ <- fetchMetadataForm config Full noValidators isOdd
                cap <- lastCaptured stub
                headerValue "Accept" cap `shouldBe` Just (metadataAccept Full)

            it "returns the upstream body verbatim" $ \stub -> do
                config <- stubConfig stub
                client <- newNpmClient config
                resp <- fetchMetadata client isOdd
                responseBody resp `shouldBe` "{}"

            it "relays conditional-GET validators when present" $ \stub -> do
                config <- stubConfig stub
                let validators =
                        Validators
                            { validatorIfNoneMatch = Just "\"etag-123\""
                            , validatorIfModifiedSince = Just "Wed, 21 Oct 2015 07:28:00 GMT"
                            }
                _ <- fetchMetadataForm config Abbreviated validators isOdd
                cap <- lastCaptured stub
                headerValue "If-None-Match" cap `shouldBe` Just "\"etag-123\""
                headerValue "If-Modified-Since" cap `shouldBe` Just "Wed, 21 Oct 2015 07:28:00 GMT"

            it "sends no conditional-GET validators by default" $ \stub -> do
                config <- stubConfig stub
                _ <- fetchMetadataForm config Abbreviated noValidators isOdd
                cap <- lastCaptured stub
                headerValue "If-None-Match" cap `shouldBe` Nothing
                headerValue "If-Modified-Since" cap `shouldBe` Nothing

-- ── bounded body read (response bound, security.md invariant 4) ───────────────

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
        -- A body the cap admits is read whole and returned unchanged — no false refusal.
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

-- ── scoped-name path encoding ─────────────────────────────────────────────────

pathEncodingSpec :: Spec
pathEncodingSpec =
    around (withStub status200 "{}") $
        describe "scoped-package path encoding" $ do
            it "percent-encodes the scope separator of a scoped name (@scope%2Fname)" $ \stub -> do
                config <- stubConfig stub
                _ <- fetchMetadataForm config Abbreviated noValidators babelCodeFrame
                cap <- lastCaptured stub
                capPath cap `shouldBe` "/@babel%2Fcode-frame"

            it "leaves an unscoped name unencoded" $ \stub -> do
                config <- stubConfig stub
                _ <- fetchMetadataForm config Abbreviated noValidators isOdd
                cap <- lastCaptured stub
                capPath cap `shouldBe` "/is-odd"

            it "does not encode the leading @ of a scoped name" $ \stub -> do
                config <- stubConfig stub
                _ <- fetchMetadataForm config Abbreviated noValidators babelCodeFrame
                cap <- lastCaptured stub
                BS.isPrefixOf "/@babel" (capPath cap) `shouldBe` True

            it "re-encodes a literal '%' in a once-decoded name so a live escape never reaches the upstream" $ \stub -> do
                -- The defect's concrete vector: '/npm/foo%252e%252e%252fbar' is
                -- WAI-decoded once to the single segment 'foo%2e%2e%2fbar', which
                -- passes 'isSafeComponent' (no literal '/'). The data plane must
                -- re-encode the '%' so the upstream receives '%25…', never a live
                -- '%2e%2e%2f' a decode-and-normalise CDN can resolve to traversal.
                config <- stubConfig stub
                _ <- fetchMetadataForm config Abbreviated noValidators onceDecodedTraversal
                cap <- lastCaptured stub
                capPath cap `shouldBe` "/foo%252e%252e%252fbar"

-- ── auth ──────────────────────────────────────────────────────────────────────

authSpec :: Spec
authSpec =
    around (withStub status200 "{}") $
        describe "bearer-token attachment" $ do
            it "attaches an injected token as a Bearer Authorization header" $ \stub -> do
                base <- stubConfig stub
                let config = base{npmToken = Just (mkSecret "tok-abc")}
                _ <- fetchMetadataForm config Abbreviated noValidators isOdd
                cap <- lastCaptured stub
                headerValue "Authorization" cap `shouldBe` Just "Bearer tok-abc"

            it "sends no Authorization header when no token is injected" $ \stub -> do
                config <- stubConfig stub
                _ <- fetchMetadataForm config Abbreviated noValidators isOdd
                cap <- lastCaptured stub
                headerValue "Authorization" cap `shouldBe` Nothing

-- ── artifacts ──────────────────────────────────────────────────────────────────

artifactSpec :: Spec
artifactSpec = describe "fetchArtifact / artifactRequest" $ do
    around (withStub status200 "tarball-bytes") $ do
        it "addresses the version's tarball under the package's /-/ path" $ \stub -> do
            config <- stubConfig stub
            client <- newNpmClient config
            _ <- fetchArtifact client isOdd v1
            cap <- lastCaptured stub
            capPath cap `shouldBe` "/is-odd/-/is-odd-1.0.0.tgz"

        it "uses the %2F-encoded path but the scope-free tarball filename for a scoped name" $ \stub -> do
            config <- stubConfig stub
            client <- newNpmClient config
            _ <- fetchArtifact client babelCodeFrame v1
            cap <- lastCaptured stub
            capPath cap `shouldBe` "/@babel%2Fcode-frame/-/code-frame-1.0.0.tgz"

        it "returns the upstream artifact bytes (buffered handle field)" $ \stub -> do
            config <- stubConfig stub
            client <- newNpmClient config
            resp <- fetchArtifact client isOdd v1
            responseBody resp `shouldBe` "tarball-bytes"

    it "marks the artifact request non-decompressing so a .tgz streams byte-for-byte" $ do
        manager <- newManager defaultManagerSettings
        let config = NpmClientConfig{npmBaseUrl = "https://reg.test", npmManager = manager, npmToken = Nothing, npmLimits = defaultLimits}
        case artifactRequest config isOdd v1 of
            Left err -> fail ("artifactRequest failed: " <> show err)
            Right req -> do
                -- 'decompress' is the predicate http-client consults per MIME
                -- type; a tarball must never be gunzipped, so it must be False
                -- for every type.
                decompress req "application/gzip" `shouldBe` False
                -- ...and it advertises no Accept-Encoding: requesting a transport
                -- encoding we refuse to decode could yield a doubly-gzipped body
                -- that breaks the tarball's dist.integrity.
                Client.requestHeaders req `shouldNotSatisfy` any ((== "accept-encoding") . fst)

    around (withStub status200 "tarball-bytes") $
        it "artifactRequestByFile fetches by the preserved filename, non-decompressing" $ \stub -> do
            config <- stubConfig stub
            -- The serve path fetches an artifact by the exact on-the-wire filename the
            -- client requested (not one rebuilt from the coordinate). The %2F-encoded
            -- package path is used, the verbatim filename trails @/-/@, and the request
            -- is non-decompressing for the same reason as 'artifactRequest'.
            case artifactRequestByFile config babelCodeFrame "code-frame-7.0.0.tgz" of
                Left err -> fail ("artifactRequestByFile failed: " <> show err)
                Right req -> do
                    Client.path req `shouldBe` "/@babel%2Fcode-frame/-/code-frame-7.0.0.tgz"
                    decompress req "application/gzip" `shouldBe` False
                    Client.requestHeaders req `shouldNotSatisfy` any ((== "accept-encoding") . fst)

    it "artifactFileUrl builds the preserved-filename URL under the package /-/ path" $ do
        -- The mirror job records the public artifact location by its on-the-wire
        -- filename; the URL is @{base}/{encoded-pkg}/-/{filename}@ verbatim.
        artifactFileUrl "https://reg.test" babelCodeFrame "code-frame-7.0.0.tgz"
            `shouldBe` Right "https://reg.test/@babel%2Fcode-frame/-/code-frame-7.0.0.tgz"

    it "artifactRequestByUrl addresses the authoritative URL verbatim (host, path, non-decompressing)" $ do
        -- The serve path honours the packument's dist.tarball location: a cross-host
        -- URL with a non-/-/ path and a signed-style query is fetched exactly as given,
        -- not reconstructed. The base URL in the config is irrelevant (the URL is
        -- absolute); the request is non-decompressing like the other artifact fetches.
        manager <- newManager defaultManagerSettings
        let config = NpmClientConfig{npmBaseUrl = "https://reg.test", npmManager = manager, npmToken = Nothing, npmLimits = defaultLimits}
            url = "https://cdn.example.net/files/abc/code-frame-7.0.0.tgz?sig=deadbeef"
        case artifactRequestByUrl config url of
            Left err -> fail ("artifactRequestByUrl failed: " <> show err)
            Right req -> do
                Client.host req `shouldBe` "cdn.example.net"
                Client.path req `shouldBe` "/files/abc/code-frame-7.0.0.tgz"
                Client.queryString req `shouldBe` "?sig=deadbeef"
                decompress req "application/gzip" `shouldBe` False
                Client.requestHeaders req `shouldNotSatisfy` any ((== "accept-encoding") . fst)

    it "artifactRequestByUrl attaches an injected bearer token (the private-leg credential posture)" $ do
        manager <- newManager defaultManagerSettings
        let config = NpmClientConfig{npmBaseUrl = "https://reg.test", npmManager = manager, npmToken = Just (mkSecret "tok-xyz"), npmLimits = defaultLimits}
        case artifactRequestByUrl config "https://private.reg/files/thing.tgz" of
            Left err -> fail ("artifactRequestByUrl failed: " <> show err)
            Right req ->
                find ((== "Authorization") . fst) (Client.requestHeaders req)
                    `shouldBe` Just ("Authorization", "Bearer tok-xyz")

-- ── publish ─────────────────────────────────────────────────────────────────────

publishSpec :: Spec
publishSpec = describe "publishArtifact idempotency" $ do
    it "PUTs the publish document to the package path" $
        withStub status200 "{}" $ \stub -> do
            config <- stubConfig stub
            client <- newNpmClient config
            _ <- publishArtifact client isOdd v1 publishDoc
            cap <- lastCaptured stub
            capMethod cap `shouldBe` "PUT"
            capPath cap `shouldBe` "/is-odd"
            capBody cap `shouldBe` publishDoc

    it "treats a 2xx as success" $
        withStub status200 "{}" $ \stub -> do
            config <- stubConfig stub
            client <- newNpmClient config
            publishArtifact client isOdd v1 publishDoc `shouldReturn` Right ()

    it "treats a 409 Conflict as idempotent success (the immutable version is already present)" $
        withStub status409 "{\"error\":\"version already exists\"}" $ \stub -> do
            config <- stubConfig stub
            client <- newNpmClient config
            publishArtifact client isOdd v1 publishDoc `shouldReturn` Right ()

    it "reports a 404 as a publish error naming the status (so the mirror job is retried)" $
        withStub status404 "{\"error\":\"Not found\"}" $ \stub -> do
            config <- stubConfig stub
            client <- newNpmClient config
            outcome <- publishArtifact client isOdd v1 publishDoc
            -- Force the error message so the failure carries the status it saw.
            leftMessage outcome `shouldSatisfy` maybe False (T.isInfixOf "404")

    it "reports a 500 as a publish error" $
        withStub status500 "boom" $ \stub -> do
            config <- stubConfig stub
            client <- newNpmClient config
            outcome <- publishArtifact client isOdd v1 publishDoc
            outcome `shouldSatisfy` isLeft

-- ── URL-formation failures ──────────────────────────────────────────────────────

urlFailureSpec :: Spec
urlFailureSpec = describe "URL-formation failures" $ do
    it "metadataRequest refuses an empty base URL as a UrlFormationError, not a publish error" $ do
        manager <- newManager defaultManagerSettings
        let config = NpmClientConfig{npmBaseUrl = "", npmManager = manager, npmToken = Nothing, npmLimits = defaultLimits}
        -- A read-path (fetch) URL fault is a 'UrlFormationError' — the whole point
        -- of the type split: it is never reported as a 'PublishError'.
        metadataRequest config Abbreviated noValidators isOdd `shouldSatisfy` urlErrorWas EmptyBaseUrl

    it "artifactRequest refuses an empty base URL as a UrlFormationError" $ do
        manager <- newManager defaultManagerSettings
        let config = NpmClientConfig{npmBaseUrl = "", npmManager = manager, npmToken = Nothing, npmLimits = defaultLimits}
        artifactRequest config isOdd v1 `shouldSatisfy` urlErrorWas EmptyBaseUrl

    it "artifactRequestByFile refuses an empty base URL as a UrlFormationError" $ do
        manager <- newManager defaultManagerSettings
        let config = NpmClientConfig{npmBaseUrl = "", npmManager = manager, npmToken = Nothing, npmLimits = defaultLimits}
        artifactRequestByFile config isOdd "is-odd-1.0.0.tgz" `shouldSatisfy` urlErrorWas EmptyBaseUrl

    it "artifactFileUrl refuses an empty base URL as a UrlFormationError" $ do
        artifactFileUrl "" isOdd "is-odd-1.0.0.tgz" `shouldSatisfy` urlErrorWas EmptyBaseUrl

    it "artifactRequestByUrl refuses an unparseable URL as a UrlFormationError" $ do
        manager <- newManager defaultManagerSettings
        let config = NpmClientConfig{npmBaseUrl = "https://reg.test", npmManager = manager, npmToken = Nothing, npmLimits = defaultLimits}
        artifactRequestByUrl config "not a url with spaces" `shouldSatisfy` urlErrorWas (UnparseableUrl "not a url with spaces")

    it "publishRequest refuses an empty base URL as a UrlFormationError" $ do
        manager <- newManager defaultManagerSettings
        let config = NpmClientConfig{npmBaseUrl = "", npmManager = manager, npmToken = Nothing, npmLimits = defaultLimits}
        publishRequest config isOdd publishDoc `shouldSatisfy` urlErrorWas EmptyBaseUrl

    it "reports an unformable URL as a non-retriable PublishUrlUnformable value (a config fault to drop, not thrown and not a retriable rejection)" $ do
        manager <- newManager defaultManagerSettings
        client <- newNpmClient NpmClientConfig{npmBaseUrl = "", npmManager = manager, npmToken = Nothing, npmLimits = defaultLimits}
        outcome <- publishArtifact client isOdd v1 publishDoc
        outcome `shouldBe` Left (PublishUrlUnformable EmptyBaseUrl)

    it "fetchMetadata raises the typed UrlFormationError on an unformable URL (catchable by type, not a stringly exception)" $ do
        manager <- newManager defaultManagerSettings
        client <- newNpmClient NpmClientConfig{npmBaseUrl = "", npmManager = manager, npmToken = Nothing, npmLimits = defaultLimits}
        outcome <- try (fetchMetadata client isOdd)
        outcome `shouldBe` (Left EmptyBaseUrl :: Either UrlFormationError RegistryResponse)

    it "fetchArtifact raises the typed UrlFormationError on an unformable URL" $ do
        manager <- newManager defaultManagerSettings
        client <- newNpmClient NpmClientConfig{npmBaseUrl = "", npmManager = manager, npmToken = Nothing, npmLimits = defaultLimits}
        outcome <- try (fetchArtifact client isOdd v1)
        outcome `shouldBe` (Left EmptyBaseUrl :: Either UrlFormationError RegistryResponse)

    it "reports a non-empty but unparseable base URL as UnparseableUrl" $ do
        manager <- newManager defaultManagerSettings
        -- Passes the empty-base guard but is not a parseable URL (no scheme,
        -- embedded spaces), so http-client's parser rejects it. The fault names
        -- the offending URL it could not parse.
        let config = NpmClientConfig{npmBaseUrl = "not a url", npmManager = manager, npmToken = Nothing, npmLimits = defaultLimits}
        metadataRequest config Abbreviated noValidators isOdd `shouldSatisfy` isUnparseable

    it "builds a metadata request against a well-formed base URL" $ do
        manager <- newManager defaultManagerSettings
        let config = NpmClientConfig{npmBaseUrl = "https://reg.test/", npmManager = manager, npmToken = Nothing, npmLimits = defaultLimits}
        -- A trailing slash on the base must not double the join.
        metadataRequest config Abbreviated noValidators isOdd `shouldNotSatisfy` isLeft

-- ── config and handle wiring ─────────────────────────────────────────────────

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

    it "MetadataForm and Validators have usable Eq/Show (the public contract)" $ do
        -- Exercises the derived instances callers (and test output) rely on.
        (Abbreviated == Full) `shouldBe` False
        show Full `shouldBe` ("Full" :: Text)
        noValidators `shouldBe` Validators{validatorIfNoneMatch = Nothing, validatorIfModifiedSince = Nothing}

    it "wires the parse* projections into the handle's pure fields" $ do
        manager <- newManager defaultManagerSettings
        client <- newNpmClient (defaultNpmConfig manager)
        -- A minimal packument projects through the fields the client installed,
        -- proving each pure projection is reachable via the assembled handle.
        let resp = RegistryResponse "{\"name\":\"is-odd\"}"
        case parsePackageInfo client resp of
            Left err -> fail ("expected a successful projection, got: " <> show err)
            Right info -> renderPackageName (infoName info) `shouldBe` "is-odd"
        -- No versions in this body, so the version list is empty and a
        -- per-version lookup is absent — both reach the wired field.
        parseVersionList client resp `shouldBe` Right []
        parseVersionDetails client resp v1 `shouldSatisfy` isLeft

-- ── fixtures ──────────────────────────────────────────────────────────────────

isOdd :: PackageName
isOdd = mkPackageName Npm Nothing "is-odd"

babelCodeFrame :: PackageName
babelCodeFrame = mkPackageName Npm (Just (mkScope "babel")) "code-frame"

{- | A package name as it arrives once-decoded from the request path: the original
@\/npm\/foo%252e%252e%252fbar@ is WAI-decoded once to the single segment
@foo%2e%2e%2fbar@. It carries no literal @\'\/\'@, so the agnostic component gate
accepts it; the data plane must re-encode the @\'%\'@ when building the upstream URL.
-}
onceDecodedTraversal :: PackageName
onceDecodedTraversal = mkPackageName Npm Nothing "foo%2e%2e%2fbar"

v1 :: Version
v1 = mkVersion Npm "1.0.0"

-- | A stand-in publish document; the body bytes are what we assert, not its shape.
publishDoc :: ByteString
publishDoc = "{\"_id\":\"is-odd\",\"name\":\"is-odd\"}"

{- | The (forced) error message of a publish 'Left', or 'Nothing' on a 'Right'.
Forcing the message exercises the error-construction path.
-}
leftMessage :: Either PublishFault a -> Maybe Text
leftMessage outcome = case outcome of
    Left (PublishRejected err) -> Just (publishErrorMessage err)
    Left (PublishUrlUnformable _) -> Nothing
    Right _ -> Nothing

{- | Whether a request-builder result is the expected 'UrlFormationError'. The
typed equality is the assertion that a URL fault is reported as a
'UrlFormationError' (the read\/write-shared type), never as a 'PublishError'.
-}
urlErrorWas :: UrlFormationError -> Either UrlFormationError a -> Bool
urlErrorWas expected = either (== expected) (const False)

-- | Whether a request-builder result is an 'UnparseableUrl' (regardless of the URL).
isUnparseable :: Either UrlFormationError a -> Bool
isUnparseable = either matchUnparseable (const False)
  where
    matchUnparseable (UnparseableUrl _) = True
    matchUnparseable _ = False

{- | Whether a @try@'d fetch raised rather than returning a response — the assertion
the bounded-body tests make when an over-budget body aborts the read fail-closed
(the metadata fetch throws a 'ResponseBoundExceeded').
-}
threw :: Either SomeException RegistryResponse -> Bool
threw = isLeft
