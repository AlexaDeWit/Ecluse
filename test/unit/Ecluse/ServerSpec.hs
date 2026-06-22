module Ecluse.ServerSpec (spec) where

import Prelude hiding (get)

import Katip (Environment (Environment), Namespace (Namespace), initLogEnv)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Types (methodPost, status200, statusCode)
import Network.Wai (
    Application,
    Request (requestBodyLength, requestMethod),
    RequestBodyLength (ChunkedBody),
    consumeRequestBodyStrict,
    defaultRequest,
    responseLBS,
 )
import Network.Wai.Test (SRequest (SRequest), runSession, setPath, simpleStatus, srequest)
import Test.Hspec
import Test.Hspec.Wai
import UnliftIO.Exception (throwString)

import Ecluse.Credential (AuthToken (..), CredentialProvider, mkSecret, staticProvider)
import Ecluse.Env (Env, newEnv)
import Ecluse.Queue (newInMemoryQueue)
import Ecluse.Registry (ParseError (..), RegistryClient (..))
import Ecluse.Server (
    Mount (..),
    RequestSizeLimit (..),
    ServerConfig (..),
    application,
    defaultServerConfig,
    rootMount,
    serverMiddleware,
 )
import Ecluse.Server.Cache (defaultCacheConfig, newMetadataCache)

{- | A registry-handle double whose effectful fields are never invoked: the S12
web layer only routes, classifies, and renders — it never fetches — so a handle
that refuses loudly is enough to assemble an 'Env'. If a route reached an
effectful field, the refusal would surface the leak.
-}
fakeRegistry :: RegistryClient
fakeRegistry =
    RegistryClient
        { fetchMetadata = const unused
        , fetchArtifact = \_ _ -> unused
        , publishArtifact = \_ _ _ -> unused
        , parsePackageInfo = const (Left unusedParse)
        , parseVersionDetails = \_ _ -> Left unusedParse
        , parseVersionList = const (Left unusedParse)
        }
  where
    unused :: IO a
    unused = throwString "fakeRegistry: the web layer must not fetch in S12"

    unusedParse :: ParseError
    unusedParse = ParseError "fakeRegistry: the web layer must not parse in S12"

-- | A credential-handle double: a fixed, non-expiring token, never read here.
fakeCredentials :: CredentialProvider
fakeCredentials = staticProvider AuthToken{authSecret = mkSecret "server-spec", authExpiresAt = Nothing}

-- | A manager with no TLS and no connection opened on construction.
newTestManager :: IO Manager
newTestManager = newManager defaultManagerSettings

-- | Assemble an 'Env' from the handle doubles, touching no network.
newTestEnv :: IO Env
newTestEnv = do
    queue <- newInMemoryQueue
    manager <- newTestManager
    metadataCache <- newMetadataCache defaultCacheConfig
    logEnv <- initLogEnv (Namespace ["ecluse"]) (Environment "test")
    newEnv fakeRegistry queue fakeCredentials manager metadataCache logEnv

-- | The 'application' under the default server config (a single root mount).
rootApp :: IO Application
rootApp = application defaultServerConfig <$> newTestEnv

{- | The 'application' under a single @\/npm@ mount, so prefix-strip dispatch can
be asserted against a non-root prefix.
-}
npmMountApp :: IO Application
npmMountApp =
    application defaultServerConfig{scMounts = [Mount{mountPrefix = ["npm"]}]} <$> newTestEnv

{- | The server middleware stack wrapping a body-reading application under a tiny
request-body cap. The size-limit middleware rejects an over-cap body only once a
handler reads it, so the inner app strictly consumes the body — exercising the
cap that the no-body meta-route handlers never trip. The 'realIp' and 'timeout'
middleware are part of the same stack.
-}
cappedApp :: Application
cappedApp = serverMiddleware defaultServerConfig{scSizeLimit = RequestSizeLimit 8} echoBody
  where
    echoBody :: Application
    echoBody req respond = do
        _ <- consumeRequestBodyStrict req
        respond (responseLBS status200 [] "read")

{- | Drive a POST with the given body through an 'Application' and return its
status code. The request is marked 'ChunkedBody' (rather than a known length) so
the size-limit middleware applies its streaming byte-count check as the body is
read — the path @hspec-wai@'s @request@, which fixes @requestBodyLength@ at a
known zero, cannot reach. (@srequest@ supplies the body chunks from the
'LByteString'.)
-}
statusForBody :: Application -> LByteString -> IO Int
statusForBody app body = do
    let req =
            (setPath defaultRequest{requestMethod = methodPost} "/")
                { requestBodyLength = ChunkedBody
                }
    response <- runSession (srequest (SRequest req body)) app
    pure (statusCode (simpleStatus response))

spec :: Spec
spec = do
    describe "meta-routes" $
        with rootApp $ do
            it "answers /-/ping locally with 200 {}" $
                get "/-/ping" `shouldRespondWith` "{}"{matchStatus = 200}

            it "answers /-/v1/search with 501 (search is not an install path)" $
                get "/-/v1/search" `shouldRespondWith` 501

            it "answers /livez with 200" $
                get "/livez" `shouldRespondWith` 200

            it "answers /readyz with 200" $
                get "/readyz" `shouldRespondWith` 200

            it "keeps /livez and /readyz distinct routes" $ do
                get "/livez" `shouldRespondWith` 200
                get "/readyz" `shouldRespondWith` 200

    describe "dispatch — root mount" $
        with rootApp $ do
            it "recognises a packument route but does not yet serve it (501, not a fake 200)" $
                get "/is-odd" `shouldRespondWith` 501

            it "recognises a tarball route but does not yet serve it (501)" $
                get "/is-odd/-/is-odd-3.0.1.tgz" `shouldRespondWith` 501

            it "404s an unrecognised path (deny by default)" $
                get "/is-odd/3.0.1" `shouldRespondWith` 404

            it "404s an unknown /-/… meta-route" $
                get "/-/whoami" `shouldRespondWith` 404

            it "404s a hostile traversal path rather than routing it" $
                -- @%2F@ decodes to one segment carrying a slash; the router denies it.
                get "/foo%2Fbar" `shouldRespondWith` 404

    describe "dispatch — /npm mount (prefix strip)" $
        with npmMountApp $ do
            it "strips the mount prefix and classifies the remainder" $
                get "/npm/is-odd" `shouldRespondWith` 501

            it "accepts the bare mount prefix with a trailing slash" $
                -- @/npm/@ strips to the empty ecosystem path → Unsupported → 404.
                get "/npm/" `shouldRespondWith` 404

            it "routes a meta-route under the mount" $
                get "/npm/-/ping" `shouldRespondWith` "{}"{matchStatus = 200}

            it "404s a path outside the mount prefix" $
                -- No mount matches @/pypi/...@, so it falls through to deny-by-default.
                get "/pypi/is-odd" `shouldRespondWith` 404

    describe "middleware — request size limit" $ do
        it "rejects a request body over the cap with 413" $
            -- The body exceeds the 8-byte cap; reading it trips the size-limit
            -- middleware, which answers 413 rather than letting the handler buffer
            -- an unbounded body.
            statusForBody cappedApp "this body is well over eight bytes"
                `shouldReturn` 413

        it "passes a request whose body is within the cap through to the handler" $
            statusForBody cappedApp "tiny" `shouldReturn` 200

    describe "defaultServerConfig" $ do
        it "listens on the conventional npm proxy port" $
            scPort defaultServerConfig `shouldBe` 4873

        it "serves a single root mount" $
            scMounts defaultServerConfig `shouldBe` [rootMount]

        it "the root mount has no prefix" $
            mountPrefix rootMount `shouldBe` []

        it "caps the request body at 25 MiB by default" $
            scSizeLimit defaultServerConfig `shouldBe` RequestSizeLimit (25 * 1024 * 1024)
