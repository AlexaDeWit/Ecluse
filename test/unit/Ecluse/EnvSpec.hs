module Ecluse.EnvSpec (spec) where

import Data.Text qualified as T
import Katip (Environment (Environment), LogEnv, Namespace (Namespace), initLogEnv)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Test.Hspec
import UnliftIO (bracket, evaluate, timeout, try)
import UnliftIO.Exception (StringException, throwString)

import Ecluse (npmServerConfig, runServer, runWorker, unconfiguredCredentials, unconfiguredRegistry)
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
import Ecluse.Env (Env (..), newEnv, newWorkerHeartbeat, withEnv)
import Ecluse.Package (Hash (..), HashAlg (..), PackageName, mkPackageName)
import Ecluse.Queue (MirrorArtifact (..), MirrorJob (..), enqueue, msgJob, newInMemoryQueue, receive)
import Ecluse.Registry (ParseError (..), RegistryClient (..), RegistryResponse (..))
import Ecluse.Server (scPort)
import Ecluse.Server.Cache (MetadataCache, defaultCacheConfig, newMetadataCache)
import Ecluse.Telemetry (telemetryDisabled, telemetryMeterProvider, telemetryTracerProvider)
import Ecluse.Version (Version, mkVersion)

{- | A registry-handle double: the @parse*@ fields return fixed pure results and
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

-- | A credential-handle double: a fixed, non-expiring token.
fakeCredentials :: CredentialProvider
fakeCredentials = staticProvider AuthToken{authSecret = mkSecret "env-spec-token", authExpiresAt = Nothing}

{- | A manager built from 'defaultManagerSettings' (no TLS, no connection opened
on construction), so assembling an 'Env' touches no network.
-}
newTestManager :: IO Manager
newTestManager = newManager defaultManagerSettings

{- | A scribe-free 'LogEnv' double: a @katip@ environment with no scribe attached,
so assembling an 'Env' opens no handle and writes nothing to stdout.
-}
newTestLogEnv :: IO LogEnv
newTestLogEnv = initLogEnv (Namespace ["ecluse"]) (Environment "test")

-- | A metadata cache on the default config (touches no network).
newTestCache :: IO MetadataCache
newTestCache = newMetadataCache defaultCacheConfig

{- | Assemble an 'Env' from the doubles above, a no-network manager, a metadata
cache, and a scribe-free 'LogEnv'.
-}
newTestEnv :: IO Env
newTestEnv = do
    queue <- newInMemoryQueue
    manager <- newTestManager
    metadataCache <- newTestCache
    logEnv <- newTestLogEnv
    heartbeat <- newWorkerHeartbeat
    newEnv fakeRegistry queue fakeCredentials manager manager metadataCache logEnv telemetryDisabled heartbeat

-- | A sample job for round-tripping the queue handle held in an 'Env'.
sampleJob :: MirrorJob
sampleJob =
    MirrorJob
        { jobPackage = pkg
        , jobVersion = ver
        , jobArtifactUrl = "https://public.test/thing/-/thing-1.0.0.tgz"
        , jobMirrorTarget = "https://mirror.test/thing/-/thing-1.0.0.tgz"
        , jobArtifact =
            MirrorArtifact
                { maFilename = "thing-1.0.0.tgz"
                , maHashes = Hash SHA1 "abc" :| []
                , maSize = Just 42
                }
        }

-- | A sample package name and version, for the registry-handle assertions.
pkg :: PackageName
pkg = mkPackageName Npm Nothing "thing"

ver :: Version
ver = mkVersion Npm "1.0.0"

spec :: Spec
spec = do
    describe "newEnv" $ do
        it "assembles an Env from injected handle doubles, with no network" $ do
            -- Construction must not touch the network: it only gathers the handles
            -- and the manager. A clean return is the assertion.
            env <- newTestEnv
            currentTok <- currentToken' env
            currentTok `shouldBe` "env-spec-token"

        it "wires the credential handle through unchanged" $ do
            env <- newTestEnv
            tok <- currentTokenSecret env
            unSecret (authSecret tok) `shouldBe` "env-spec-token"

        it "wires the registry handle through unchanged (a pure parse field round-trips)" $ do
            -- The registry double's 'parseVersionList' echoes the response body as
            -- the parse error, so reaching it through 'envRegistry' proves the
            -- exact handle we injected is the one stored.
            env <- newTestEnv
            let result = parseVersionList (envRegistry env) (RegistryResponse "echo-me")
            result `shouldBe` Left (ParseError "echo-me")

        it "wires the queue handle through (a job enqueued via Env is received via Env)" $ do
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

        it "exposes the trusted private-origin manager it was built with" $ do
            -- The second data-plane manager (the trusted private origin, exempt from
            -- the resolved-IP recheck) is likewise opaque, so forcing the accessor
            -- to weak-head normal form is the assertion that it is wired.
            env <- newTestEnv
            _ <- evaluate (envPrivateManager env)
            pure ()

        it "exposes the LogEnv it was built with" $ do
            -- A 'LogEnv' is likewise opaque, so reaching 'envLogEnv' and forcing it
            -- to weak-head normal form is the assertion: the wired logging
            -- environment is the one stored.
            env <- newTestEnv
            _ <- evaluate (envLogEnv env)
            pure ()

        it "wires the telemetry handle through (the off-by-default no-op)" $ do
            -- The default substrate is disabled, so the handle stored in the 'Env'
            -- exposes no providers — telemetry is genuinely inert, not merely
            -- unsampled. A 'TracerProvider' has no 'Show', so each is checked
            -- through 'isNothing' rather than a printing matcher.
            env <- newTestEnv
            isNothing (telemetryTracerProvider (envTelemetry env)) `shouldBe` True
            isNothing (telemetryMeterProvider (envTelemetry env)) `shouldBe` True

    describe "withEnv" $ do
        it "runs the body against the assembled Env and returns its result" $ do
            queue <- newInMemoryQueue
            manager <- newTestManager
            metadataCache <- newTestCache
            logEnv <- newTestLogEnv
            heartbeat <- newWorkerHeartbeat
            result <- withEnv fakeRegistry queue fakeCredentials manager manager metadataCache logEnv telemetryDisabled heartbeat $ \env ->
                currentToken' env
            result `shouldBe` "env-spec-token"

        it "propagates an exception thrown in the body (bracketed teardown re-raises)" $ do
            queue <- newInMemoryQueue
            manager <- newTestManager
            metadataCache <- newTestCache
            logEnv <- newTestLogEnv
            heartbeat <- newWorkerHeartbeat
            let body :: Env -> IO ()
                body _ = throwString "boom"
            outcome <- try (withEnv fakeRegistry queue fakeCredentials manager manager metadataCache logEnv telemetryDisabled heartbeat body)
            case outcome of
                Left (_ :: StringException) -> pure ()
                Right () -> expectationFailure "expected the body's exception to propagate"

    describe "split-ready services" $ do
        it "runServer over a ServerConfig and Env serves (blocks) rather than returning" $ do
            -- The server is now a real blocking listener: started under a short
            -- timeout it keeps serving until cancelled, so 'timeout' yields
            -- 'Nothing'. (The routing/meta/middleware behaviour itself is asserted
            -- socket-free in "Ecluse.ServerSpec".) 'scPort = 0' binds an OS-assigned
            -- ephemeral port, so the test never races a fixed port already in use.
            env <- newTestEnv
            timeout 100000 (runServer (npmServerConfig{scPort = 0}) env) `shouldReturn` Nothing

        it "runWorker over an Env serves (blocks polling) rather than returning" $ do
            -- The worker is a continuous consume loop: started under a short timeout
            -- it keeps long-polling the (empty in-memory) queue until cancelled, so
            -- 'timeout' yields 'Nothing'. The loop logic itself is asserted
            -- socket-free in "Ecluse.WorkerSpec".
            env <- newTestEnv
            timeout 100000 (runWorker env) `shouldReturn` Nothing

    describe "App / runApp" $ do
        it "reads the Env through the reader and runs effects in IO" $ do
            -- A round-trip through the orchestration monad: the action reaches a
            -- handle via the reader ('asks') and uses it in 'IO' ('liftIO'), then
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
            -- A no-backend handle must not silently return a fake success: every
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

    -- An 'App' action that reaches the credential handle through the reader and
    -- uses it in IO, exercising the monad's MonadReader + MonadIO instances.
    readToken :: App Text
    readToken = do
        provider <- asks envCredentials
        tok <- liftIO (currentToken provider)
        pure (unSecret (authSecret tok))

    -- Assert an effectful handle call throws rather than returning a value.
    shouldRefuse :: IO a -> Expectation
    shouldRefuse act = do
        outcome <- try act
        case outcome of
            Left (_ :: SomeException) -> pure ()
            Right _ -> expectationFailure "expected the unconfigured handle to refuse"
