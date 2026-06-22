module Ecluse.EnvSpec (spec) where

import Data.Text qualified as T
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Test.Hspec
import UnliftIO (bracket, evaluate, try)
import UnliftIO.Exception (StringException, throwString)

import Ecluse (runServer, runWorker, unconfiguredCredentials, unconfiguredRegistry)
import Ecluse.App (App, runApp)
import Ecluse.Credential (
    AuthToken (..),
    CredentialProvider,
    currentToken,
    mkSecret,
    staticProvider,
    unSecret,
 )
import Ecluse.Ecosystem (Ecosystem (..))
import Ecluse.Env (Env (..), newEnv, withEnv)
import Ecluse.Package (PackageName, mkPackageName)
import Ecluse.Queue (MirrorJob (..), enqueue, msgJob, newInMemoryQueue, receive)
import Ecluse.Registry (ParseError (..), RegistryClient (..), RegistryResponse (..))
import Ecluse.Version (Version, mkVersion)

{- | A registry-seam double: the @parse*@ fields return fixed pure results and
the effectful fields are never invoked by these tests, so they refuse loudly if
they ever are. This lets an 'Env' be assembled without a real backend or any
network.
-}
fakeRegistry :: RegistryClient
fakeRegistry =
    RegistryClient
        { fetchMetadata = const unused
        , fetchArtifact = \_ _ -> unused
        , publishArtifact = \_ _ _ -> unused
        , parsePackageInfo = const (Left parseStub)
        , parseVersionDetails = \_ _ -> Left parseStub
        , parseVersionList = \(RegistryResponse body) -> Left (ParseError (decodeUtf8 body))
        }
  where
    unused :: IO a
    unused = throwString "fakeRegistry: effectful field not used in this test"

    parseStub :: ParseError
    parseStub = ParseError "fake"

-- | A credential-seam double: a fixed, non-expiring token.
fakeCredentials :: CredentialProvider
fakeCredentials = staticProvider AuthToken{authSecret = mkSecret "env-spec-token", authExpiresAt = Nothing}

{- | A manager built from 'defaultManagerSettings' (no TLS, no connection opened
on construction), so assembling an 'Env' touches no network.
-}
newTestManager :: IO Manager
newTestManager = newManager defaultManagerSettings

-- | Assemble an 'Env' from the doubles above and a no-network manager.
newTestEnv :: IO Env
newTestEnv = do
    queue <- newInMemoryQueue
    manager <- newTestManager
    newEnv fakeRegistry queue fakeCredentials manager

-- | A sample job for round-tripping the queue seam held in an 'Env'.
sampleJob :: MirrorJob
sampleJob =
    MirrorJob
        { jobPackage = pkg
        , jobVersion = ver
        , jobArtifactUrl = "https://public.test/thing/-/thing-1.0.0.tgz"
        , jobMirrorTarget = "https://mirror.test/thing/-/thing-1.0.0.tgz"
        }

-- | A sample package name and version, for the registry-seam assertions.
pkg :: PackageName
pkg = mkPackageName Npm Nothing "thing"

ver :: Version
ver = mkVersion Npm "1.0.0"

spec :: Spec
spec = do
    describe "newEnv" $ do
        it "assembles an Env from injected seam doubles, with no network" $ do
            -- Construction must not touch the network: it only gathers the seams
            -- and the manager. A clean return is the assertion.
            env <- newTestEnv
            currentTok <- currentToken' env
            currentTok `shouldBe` "env-spec-token"

        it "wires the credential seam through unchanged" $ do
            env <- newTestEnv
            tok <- currentTokenSecret env
            unSecret (authSecret tok) `shouldBe` "env-spec-token"

        it "wires the registry seam through unchanged (a pure parse field round-trips)" $ do
            -- The registry double's 'parseVersionList' echoes the response body as
            -- the parse error, so reaching it through 'envRegistry' proves the
            -- exact seam we injected is the one stored.
            env <- newTestEnv
            let result = parseVersionList (envRegistry env) (RegistryResponse "echo-me")
            result `shouldBe` Left (ParseError "echo-me")

        it "wires the queue seam through (a job enqueued via Env is received via Env)" $ do
            env <- newTestEnv
            enqueue (envQueue env) sampleJob
            msgs <- receive (envQueue env)
            map msgJob msgs `shouldBe` [sampleJob]

        it "exposes the shared HTTP manager it was built with" $ do
            -- A 'Manager' is opaque (no 'Eq'\/'Show' and no network-free
            -- observable), so the assertion is that the accessor yields the wired
            -- resource rather than a bottom: reaching 'envManager' and forcing it
            -- to weak-head normal form succeeds without throwing.
            env <- newTestEnv
            _ <- evaluate (envManager env)
            pure ()

    describe "withEnv" $ do
        it "runs the body against the assembled Env and returns its result" $ do
            queue <- newInMemoryQueue
            manager <- newTestManager
            result <- withEnv fakeRegistry queue fakeCredentials manager $ \env ->
                currentToken' env
            result `shouldBe` "env-spec-token"

        it "propagates an exception thrown in the body (bracketed teardown re-raises)" $ do
            queue <- newInMemoryQueue
            manager <- newTestManager
            let body :: Env -> IO ()
                body _ = throwString "boom"
            outcome <- try (withEnv fakeRegistry queue fakeCredentials manager body)
            case outcome of
                Left (_ :: StringException) -> pure ()
                Right () -> expectationFailure "expected the body's exception to propagate"

    describe "split-ready services" $ do
        it "runServer over an Env returns (the stub serves nothing yet)" $ do
            env <- newTestEnv
            runServer env `shouldReturn` ()

        it "runWorker over an Env returns (the stub consumes nothing yet)" $ do
            env <- newTestEnv
            runWorker env `shouldReturn` ()

    describe "App / runApp" $ do
        it "reads the Env through the reader and runs effects in IO" $ do
            -- A round-trip through the orchestration monad: the action reaches a
            -- seam via the reader ('asks') and uses it in 'IO' ('liftIO'), then
            -- 'runApp' discharges it against the composition root.
            env <- newTestEnv
            result <- runApp env readToken
            result `shouldBe` "env-spec-token"

        it "lifts bracket into the reader (MonadUnliftIO)" $ do
            -- The reason 'App' adopts @unliftio@: @bracket@ must run in 'App', not
            -- only 'IO'. The acquire result threads through to the body.
            env <- newTestEnv
            result <- runApp env (bracket (pure "resource") (const (pure ())) pure)
            result `shouldBe` ("resource" :: Text)

        it "composes through its Functor and Applicative instances" $ do
            -- Exercises the derived 'Functor'\/'Applicative' over a real reader
            -- action (not a bare 'pure'): map a function over the token read, and
            -- combine two reads applicatively.
            env <- newTestEnv
            mapped <- runApp env (fmap T.length readToken)
            mapped `shouldBe` T.length "env-spec-token"
            combined <- runApp env (liftA2 (\a b -> T.length a + T.length b) readToken readToken)
            combined `shouldBe` 2 * T.length "env-spec-token"

    describe "unconfiguredRegistry" $ do
        it "refuses every effectful call loudly rather than fabricating a result" $ do
            -- A no-backend seam must not silently return a fake success: every
            -- effectful op throws, so a misconfiguration fails fast instead of
            -- serving phantom data or reporting a publish that never happened.
            shouldRefuse (fetchMetadata unconfiguredRegistry pkg)
            shouldRefuse (fetchArtifact unconfiguredRegistry pkg ver)
            shouldRefuse (publishArtifact unconfiguredRegistry pkg ver "bytes")

        it "fails every parse with an explanatory error" $ do
            let resp = RegistryResponse "anything"
                expected :: Either ParseError b
                expected = Left (ParseError "no registry backend configured")
            parsePackageInfo unconfiguredRegistry resp `shouldBe` expected
            parseVersionDetails unconfiguredRegistry resp ver `shouldBe` expected
            parseVersionList unconfiguredRegistry resp `shouldBe` expected

    describe "unconfiguredCredentials" $ do
        it "mints no usable token (empty secret, no expiry)" $ do
            tok <- currentToken unconfiguredCredentials
            unSecret (authSecret tok) `shouldBe` ""
            authExpiresAt tok `shouldBe` Nothing
  where
    currentToken' :: Env -> IO Text
    currentToken' env = unSecret . authSecret <$> currentTokenSecret env

    currentTokenSecret :: Env -> IO AuthToken
    currentTokenSecret env = currentToken (envCredentials env)

    -- An 'App' action that reaches the credential seam through the reader and
    -- uses it in IO, exercising the monad's MonadReader + MonadIO instances.
    readToken :: App Text
    readToken = do
        provider <- asks envCredentials
        tok <- liftIO (currentToken provider)
        pure (unSecret (authSecret tok))

    -- Assert an effectful seam call throws rather than returning a value.
    shouldRefuse :: IO a -> Expectation
    shouldRefuse act = do
        outcome <- try act
        case outcome of
            Left (_ :: SomeException) -> pure ()
            Right _ -> expectationFailure "expected the unconfigured seam to refuse"
