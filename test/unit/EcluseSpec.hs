module EcluseSpec (spec) where

import Prelude hiding (get)

import Katip (Environment (Environment), Namespace (Namespace), initLogEnv)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai
import UnliftIO (timeout, try)
import UnliftIO.Exception (throwString)

import System.Environment (setEnv, unsetEnv)

import Ecluse (BootAborted (..), mountBindingFor, npmServerConfig, orExit, run)
import Ecluse.Credential (AuthToken (..), CredentialProvider, mkSecret, staticProvider)
import Ecluse.Ecosystem (Ecosystem (..))
import Ecluse.Env (Env, newEnv, newWorkerHeartbeat)
import Ecluse.Queue (newInMemoryQueue)
import Ecluse.Registry (ParseError (..), RegistryClient (..))
import Ecluse.Server (MountBinding (..), application)
import Ecluse.Server.Cache (defaultCacheConfig, newMetadataCache)
import Ecluse.Telemetry (telemetryDisabled)

{- | A registry-handle double whose effectful fields are never invoked — the
composition-root routing assertions below only route, classify, and render.
-}
fakeRegistry :: RegistryClient
fakeRegistry =
    RegistryClient
        { fetchMetadata = const unused
        , fetchArtifact = \_ _ -> unused
        , publishArtifact = \_ _ _ -> unused
        , parsePackageInfo = \_ _ -> Left unusedParse
        , parseVersionDetails = \_ _ -> Left unusedParse
        , parseVersionList = const (Left unusedParse)
        }
  where
    unused :: IO a
    unused = throwString "fakeRegistry: composition-root routing must not fetch"

    unusedParse :: ParseError
    unusedParse = ParseError "fakeRegistry: composition-root routing must not parse"

-- | A credential-handle double: a fixed, non-expiring token, never read here.
fakeCredentials :: CredentialProvider
fakeCredentials = staticProvider AuthToken{authSecret = mkSecret "ecluse-spec", authExpiresAt = Nothing}

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
    heartbeat <- newWorkerHeartbeat
    newEnv fakeRegistry queue fakeCredentials manager manager metadataCache logEnv telemetryDisabled heartbeat

{- | The composed npm front door ('npmServerConfig') as a WAI 'Application', driven
in-process — so the actual mount the composition root wires is exercised, no socket.
-}
npmApp :: IO Application
npmApp = application npmServerConfig <$> newTestEnv

{- | A valid minimal environment for the config-driven 'run': the three required
URLs, a static mirror-target token so the env single-mount's @static@ credential
reference resolves, and an ephemeral port so the brief blocking listen does not
collide with the conventional default. The document-loading path is exercised
separately by layering @PROXY_CONFIG@ on top of this base. It deliberately omits
@AWS_REGION@, so it is also the fixture for the queue-region fail-fast case.
-}
runEnv :: [(String, String)]
runEnv =
    [ ("PRIVATE_UPSTREAM_URL", "https://private.example.test")
    , ("MIRROR_TARGET_URL", "https://mirror.example.test")
    , ("MIRROR_QUEUE_URL", "https://sqs.example.test/q")
    , ("MIRROR_TARGET_TOKEN", "mirror-write-token")
    , ("PROXY_PORT", "0")
    ]

{- | 'runEnv' extended with the AWS settings the default @sqs@ mirror-queue backend
needs to be built: a region, and throwaway credentials in the environment so
@amazonka@'s discovery resolves from env vars (no instance-metadata round-trip) and
'Ecluse.Queue.Sqs.newSqsQueue' constructs without touching the network. The bogus
queue URL is never reached on the boot-and-serve path — the worker's failing polls
are caught by its own supervision — so this stays hermetic.
-}
awsRunEnv :: [(String, String)]
awsRunEnv =
    [ ("AWS_REGION", "us-east-1")
    , ("AWS_ACCESS_KEY_ID", "test")
    , ("AWS_SECRET_ACCESS_KEY", "test")
    ]
        <> runEnv

spec :: Spec
spec = do
    -- The umbrella module is the composition root the @ecluse@ executable calls
    -- into. It lives in the library (not app/Main.hs) so it is exercised here
    -- rather than only through the binary, and stays linked into the unit suite
    -- where scripts/coverage.sh can see it. 'run' parses configuration, validates
    -- it, and starts the blocking server, so over a valid minimal env it keeps
    -- serving under a short timeout — the liveness check that the config-driven
    -- root wires up and starts without throwing. The static mirror-target token is
    -- supplied so the env single-mount's @static@ credential reference resolves.
    describe "run" $ do
        it "boots from the environment layer alone (no document) and serves" $ do
            unsetEnv "PROXY_CONFIG"
            traverse_ (uncurry setEnv) awsRunEnv
            outcome <- timeout 100000 run
            traverse_ (unsetEnv . fst) awsRunEnv
            outcome `shouldBe` Nothing

        it "boots with an inline PROXY_CONFIG document and serves" $ do
            traverse_ (uncurry setEnv) awsRunEnv
            setEnv "PROXY_CONFIG" "{\"rules\":{}}"
            outcome <- timeout 100000 run
            unsetEnv "PROXY_CONFIG"
            traverse_ (unsetEnv . fst) awsRunEnv
            outcome `shouldBe` Nothing

        it "aborts fast at boot when the mirror-queue backend is not built (pubsub)" $ do
            -- The GCP arm is recognised but unbuilt, so the composition root refuses
            -- to start rather than silently falling back to a different queue.
            unsetEnv "PROXY_CONFIG"
            traverse_ (uncurry setEnv) awsRunEnv
            setEnv "MIRROR_QUEUE_PROVIDER" "pubsub"
            outcome <- try (timeout 100000 run) :: IO (Either BootAborted (Maybe ()))
            unsetEnv "MIRROR_QUEUE_PROVIDER"
            traverse_ (unsetEnv . fst) awsRunEnv
            outcome `shouldBe` Left BootAborted

        it "boots under the in-memory mirror-queue backend (no AWS settings) and serves" $ do
            -- The explicit memory backend needs no cloud queue: it boots from the base
            -- env alone — no AWS_REGION or credentials — emitting its loud non-durable
            -- boot warning and constructing the bounded in-memory queue. The idle
            -- worker simply parks on the empty queue rather than hot-looping.
            unsetEnv "PROXY_CONFIG"
            unsetEnv "AWS_REGION"
            traverse_ (uncurry setEnv) runEnv
            setEnv "MIRROR_QUEUE_PROVIDER" "memory"
            outcome <- timeout 100000 run
            unsetEnv "MIRROR_QUEUE_PROVIDER"
            traverse_ (unsetEnv . fst) runEnv
            outcome `shouldBe` Nothing

        it "aborts fast at boot when the sqs backend has no AWS_REGION" $ do
            -- The default sqs backend needs a region to be scoped to; absent, the
            -- composition root fails fast rather than building an unscoped queue.
            unsetEnv "PROXY_CONFIG"
            -- Clear AWS_REGION explicitly so a sibling case (run under a randomized
            -- order) cannot leak it into this missing-region fixture.
            unsetEnv "AWS_REGION"
            traverse_ (uncurry setEnv) runEnv
            outcome <- try (timeout 100000 run) :: IO (Either BootAborted (Maybe ()))
            traverse_ (unsetEnv . fst) runEnv
            outcome `shouldBe` Left BootAborted

        it "aborts fast at boot when the gcp-artifact-registry credential provider is selected (not built)" $ do
            unsetEnv "PROXY_CONFIG"
            traverse_ (uncurry setEnv) awsRunEnv
            setEnv "MIRROR_TARGET_CREDENTIAL_PROVIDER" "gcp-artifact-registry"
            outcome <- try (timeout 100000 run) :: IO (Either BootAborted (Maybe ()))
            unsetEnv "MIRROR_TARGET_CREDENTIAL_PROVIDER"
            traverse_ (unsetEnv . fst) awsRunEnv
            outcome `shouldBe` Left BootAborted

        it "aborts fast at boot when codeartifact is selected but its domain cannot be resolved" $ do
            -- The CodeArtifact inputs resolve by neither an explicit key nor the
            -- (non-CodeArtifact) mirror URL host, so boot fails before any AWS mint.
            unsetEnv "PROXY_CONFIG"
            traverse_ (uncurry setEnv) awsRunEnv
            setEnv "MIRROR_TARGET_CREDENTIAL_PROVIDER" "codeartifact"
            outcome <- try (timeout 100000 run) :: IO (Either BootAborted (Maybe ()))
            unsetEnv "MIRROR_TARGET_CREDENTIAL_PROVIDER"
            traverse_ (unsetEnv . fst) awsRunEnv
            outcome `shouldBe` Left BootAborted

    describe "orExit (boot fail-fast)" $ do
        it "yields the value on a Right (a passing boot phase)" $
            orExit (const "unused") (Right 7 :: Either () Int) `shouldReturn` 7

        it "reports the failure and aborts the boot on a Left" $ do
            outcome <- try (orExit (const "boot rejected") (Left ()) :: IO ()) :: IO (Either BootAborted ())
            case outcome of
                Left BootAborted -> pure ()
                Right () -> expectationFailure "expected the boot to abort"

    describe "npmServerConfig — the composed npm front door" $
        -- Drive the real composition the composition root wires (npmServerConfig),
        -- not a copy: this exercises the npm mount end to end through dispatch.
        with npmApp $ do
            it "mounts npm at /npm (answers /npm/-/ping locally with 200 {})" $
                get "/npm/-/ping" `shouldRespondWith` "{}"{matchStatus = 200}

            it "recognises an npm packument route under the mount (501; serve deps unwired)" $
                -- Reaching the Packument route forces the mount's classifier *and* its
                -- (unwired) packument deps, then renders the 501 through its renderer.
                get "/npm/is-odd" `shouldRespondWith` 501

            it "does NOT mount npm at the root — /-/ping there is the neutral 404" $
                get "/-/ping" `shouldRespondWith` "Not Found\n"{matchStatus = 404}

            it "renders an unmounted prefix as a neutral text/plain 404" $
                get "/pypi/is-odd" `shouldRespondWith` "Not Found\n"{matchStatus = 404}

    describe "mountBindingFor — ecosystem drives the binding" $ do
        it "resolves npm to a binding whose prefix is derived from the ecosystem (/npm)" $
            -- The path prefix is derived from the ecosystem, never configured, so
            -- the npm binding is served under its own /npm prefix.
            (bindingPrefix <$> mountBindingFor Npm Nothing) `shouldBe` Just ("npm" :| [])

        it "has no binding for an ecosystem with no adapter wired (loud Nothing, not a stub)" $ do
            -- PyPI and RubyGems have no registry client or renderer yet, so resolving
            -- one is a Nothing the caller must handle — never a silently half-wired mount.
            (bindingPrefix <$> mountBindingFor PyPI Nothing) `shouldBe` Nothing
            (bindingPrefix <$> mountBindingFor RubyGems Nothing) `shouldBe` Nothing
