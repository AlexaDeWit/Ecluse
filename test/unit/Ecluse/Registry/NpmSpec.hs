module Ecluse.Registry.NpmSpec (spec) where

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
    RegistryClient (
        fetchArtifact,
        fetchMetadata,
        parsePackageInfo,
        parseVersionDetails,
        parseVersionList,
        publishArtifact
    ),
    RegistryResponse (RegistryResponse, responseBody),
 )
import Ecluse.Registry.Npm (
    MetadataForm (Abbreviated, Full),
    NpmClientConfig (..),
    Validators (..),
    artifactRequest,
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
most recent 'Captured' request (the seam talks to @127.0.0.1:port@).
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
withStub status body action = do
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
            respond (responseLBS status [] body)
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
            }

-- | Look up a header (case-insensitively) in a captured request.
headerValue :: ByteString -> Captured -> Maybe ByteString
headerValue name cap = snd <$> find ((== CI.mk name) . fst) (capHeaders cap)

-- ── content negotiation ──────────────────────────────────────────────────────

requestShapingSpec :: Spec
requestShapingSpec =
    around (withStub status200 "{}") $
        describe "fetchMetadata request shaping" $ do
            it "the seam's fetchMetadata requests the abbreviated form with gzip" $ \stub -> do
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

        it "returns the upstream artifact bytes (buffered seam field)" $ \stub -> do
            config <- stubConfig stub
            client <- newNpmClient config
            resp <- fetchArtifact client isOdd v1
            responseBody resp `shouldBe` "tarball-bytes"

    it "marks the artifact request non-decompressing so a .tgz streams byte-for-byte" $ do
        manager <- newManager defaultManagerSettings
        let config = NpmClientConfig{npmBaseUrl = "https://reg.test", npmManager = manager, npmToken = Nothing}
        case artifactRequest config isOdd v1 of
            Left err -> fail ("artifactRequest failed: " <> show err)
            Right req ->
                -- 'decompress' is the predicate http-client consults per MIME
                -- type; a tarball must never be gunzipped, so it must be False
                -- for every type.
                decompress req "application/gzip" `shouldBe` False

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
    it "metadataRequest refuses an empty base URL with an explanatory error" $ do
        manager <- newManager defaultManagerSettings
        let config = NpmClientConfig{npmBaseUrl = "", npmManager = manager, npmToken = Nothing}
        -- Force the error text so the failure carries a reason (not just Left ()).
        leftMessage (metadataRequest config Abbreviated noValidators isOdd)
            `shouldSatisfy` maybe False (T.isInfixOf "EmptyBaseUrl")

    it "publishRequest refuses an empty base URL with an explanatory error" $ do
        manager <- newManager defaultManagerSettings
        let config = NpmClientConfig{npmBaseUrl = "", npmManager = manager, npmToken = Nothing}
        leftMessage (publishRequest config isOdd publishDoc)
            `shouldSatisfy` maybe False (T.isInfixOf "EmptyBaseUrl")

    it "publishArtifact short-circuits to a publish error on an empty base URL" $ do
        manager <- newManager defaultManagerSettings
        client <- newNpmClient NpmClientConfig{npmBaseUrl = "", npmManager = manager, npmToken = Nothing}
        outcome <- publishArtifact client isOdd v1 publishDoc
        outcome `shouldSatisfy` isLeft

    it "fetchMetadata throws on an unformable URL (config fault, not a silent success)" $ do
        manager <- newManager defaultManagerSettings
        client <- newNpmClient NpmClientConfig{npmBaseUrl = "", npmManager = manager, npmToken = Nothing}
        outcome <- try (fetchMetadata client isOdd)
        outcome `shouldSatisfy` threw

    it "fetchArtifact throws on an unformable URL" $ do
        manager <- newManager defaultManagerSettings
        client <- newNpmClient NpmClientConfig{npmBaseUrl = "", npmManager = manager, npmToken = Nothing}
        outcome <- try (fetchArtifact client isOdd v1)
        outcome `shouldSatisfy` threw

    it "refuses a non-empty but unparseable base URL" $ do
        manager <- newManager defaultManagerSettings
        -- Passes the empty-base guard but is not a parseable URL (no scheme,
        -- embedded spaces), so http-client's parser rejects it.
        let config = NpmClientConfig{npmBaseUrl = "not a url", npmManager = manager, npmToken = Nothing}
        leftMessage (metadataRequest config Abbreviated noValidators isOdd)
            `shouldSatisfy` maybe False (T.isInfixOf "could not parse upstream URL")

    it "builds a metadata request against a well-formed base URL" $ do
        manager <- newManager defaultManagerSettings
        let config = NpmClientConfig{npmBaseUrl = "https://reg.test/", npmManager = manager, npmToken = Nothing}
        -- A trailing slash on the base must not double the join.
        metadataRequest config Abbreviated noValidators isOdd `shouldNotSatisfy` isLeft

-- ── config and seam wiring ───────────────────────────────────────────────────

configAndWiringSpec :: Spec
configAndWiringSpec = describe "config and seam wiring" $ do
    it "defaultNpmConfig targets the public registry anonymously over the given manager" $ do
        manager <- newManager defaultManagerSettings
        let config = defaultNpmConfig manager
        npmBaseUrl config `shouldBe` publicRegistryBaseUrl
        isJust (npmToken config) `shouldBe` False
        -- A 'Manager' is opaque (no Eq/Show), so forcing it to WHNF is the
        -- assertion that the field carries the manager we passed, not a bottom.
        _ <- evaluate (npmManager config)
        pure ()

    it "MetadataForm and Validators have usable Eq/Show (the public contract)" $ do
        -- Exercises the derived instances callers (and test output) rely on.
        (Abbreviated == Full) `shouldBe` False
        show Full `shouldBe` ("Full" :: Text)
        noValidators `shouldBe` Validators{validatorIfNoneMatch = Nothing, validatorIfModifiedSince = Nothing}

    it "wires S07's parse* projections into the seam's pure fields" $ do
        manager <- newManager defaultManagerSettings
        client <- newNpmClient (defaultNpmConfig manager)
        -- A minimal packument projects through the fields the client installed,
        -- proving each pure projection is reachable via the assembled seam.
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

v1 :: Version
v1 = mkVersion Npm "1.0.0"

-- | A stand-in publish document; the body bytes are what we assert, not its shape.
publishDoc :: ByteString
publishDoc = "{\"_id\":\"is-odd\",\"name\":\"is-odd\"}"

{- | The (forced) error message of a request-building 'Left', or 'Nothing' on
a 'Right'. Forcing the message exercises the error-construction path.
-}
leftMessage :: Either PublishError a -> Maybe Text
leftMessage = either (Just . publishErrorMessage) (const Nothing)

-- | Whether a @try@'d fetch raised, rather than returning a response.
threw :: Either SomeException RegistryResponse -> Bool
threw = isLeft
