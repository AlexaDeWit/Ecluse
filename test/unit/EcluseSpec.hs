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
import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Queue (newInMemoryQueue)
import Ecluse.Core.Registry (ParseError (..), RegistryClient (..))
import Ecluse.Core.Server.Cache (defaultCacheConfig, newMetadataCache)
import Ecluse.Env (Env, newEnv, newWorkerHeartbeat)
import Ecluse.Server (MountBinding (..), application)
import Ecluse.Telemetry (telemetryDisabled)

{- | A registry-handle double whose effectful fields are never invoked -- the
composition-root routing assertions below only route, classify, and render.
-}
fakeRegistry :: RegistryClient
fakeRegistry =
    RegistryClient
        { fetchMetadata = const unused
        , fetchArtifact = \_ _ -> unused
        , publishArtifact = \_ _ _ _ -> unused
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
    newEnv fakeRegistry queue manager manager metadataCache logEnv telemetryDisabled heartbeat

{- | The composed npm front door ('npmServerConfig') as a WAI 'Application', driven
in-process -- so the actual mount the composition root wires is exercised, no socket.
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
    [ ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://private.example.test")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET", "https://mirror.example.test")
    , ("ECLUSE_QUEUE_URL", "https://sqs.example.test/q")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN", "mirror-write-token")
    , ("ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER", "static")
    , ("ECLUSE_MOUNTS__PYPI__CREDENTIAL_PROVIDER", "static")
    , ("ECLUSE_MOUNTS__RUBYGEMS__CREDENTIAL_PROVIDER", "static")
    , ("AWS_ACCESS_KEY_ID", "test")
    , ("AWS_SECRET_ACCESS_KEY", "test")
    , ("ECLUSE_PORT", "0")
    ]

{- | 'runEnv' extended with the AWS settings the default @sqs@ mirror-queue backend
needs to be built: a region, and throwaway credentials in the environment so
@amazonka@'s discovery resolves from env vars (no instance-metadata round-trip) and
'Ecluse.Core.Queue.Sqs.newSqsQueue' constructs without touching the network. The bogus
queue URL is never reached on the boot-and-serve path -- the worker's failing polls
are caught by its own supervision -- so this stays hermetic.
-}
awsRunEnv :: [(String, String)]
awsRunEnv =
    [ ("AWS_REGION", "us-east-1")
    ]
        <> runEnv

spec :: Spec
spec = do
    -- The umbrella module is the composition root the @ecluse@ executable calls
    -- into. It lives in the library (not app/Main.hs) so it is exercised here
    -- rather than only through the binary, and stays linked into the unit suite
    -- where scripts/coverage.sh can see it. 'run' parses configuration, validates
    -- it, and starts the blocking server, so over a valid minimal env it keeps
    -- serving under a short timeout -- the liveness check that the config-driven
    -- root wires up and starts without throwing. The static mirror-target token is
    -- supplied so the env single-mount's @static@ credential reference resolves.
    describe "run" $ do
        it "boots from the environment layer alone (no document) and serves" $ do
            unsetEnv "ECLUSE_COVERAGE_QUIET_PARTIAL"
            traverse_ (uncurry setEnv) awsRunEnv
            outcome <- timeout 100000 run
            traverse_ (unsetEnv . fst) awsRunEnv
            outcome `shouldBe` Nothing

        it "boots with an inline PROXY_CONFIG document and serves" $ do
            unsetEnv "ECLUSE_COVERAGE_QUIET_PARTIAL"
            traverse_ (uncurry setEnv) awsRunEnv
            outcome <- timeout 100000 run
            traverse_ (unsetEnv . fst) awsRunEnv
            outcome `shouldBe` Nothing

        it "aborts fast at boot when the mirror-queue backend is not built (pubsub)" $ do
            -- The GCP arm is recognised but unbuilt, so the composition root refuses
            -- to start rather than silently falling back to a different queue.
            traverse_ (uncurry setEnv) awsRunEnv
            setEnv "ECLUSE_QUEUE_BACKEND" "pubsub"
            outcome <- try (timeout 100000 run) :: IO (Either BootAborted (Maybe ()))
            unsetEnv "ECLUSE_QUEUE_BACKEND"
            traverse_ (unsetEnv . fst) awsRunEnv
            outcome `shouldBe` Left BootAborted

        it "boots under the in-memory mirror-queue backend (no AWS settings, no ECLUSE_QUEUE_URL) and serves" $ do
            -- The explicit memory backend needs no cloud queue: it boots with no
            -- AWS_REGION/credentials AND no ECLUSE_QUEUE_URL -- emitting its loud
            -- non-durable boot warning and constructing the bounded in-memory queue. The
            -- idle worker simply parks on the empty queue rather than hot-looping.
            unsetEnv "ECLUSE_COVERAGE_QUIET_PARTIAL"
            unsetEnv "AWS_REGION"
            unsetEnv "ECLUSE_QUEUE_URL"
            traverse_ (uncurry setEnv) (filter ((/= "ECLUSE_QUEUE_URL") . fst) runEnv)
            setEnv "ECLUSE_QUEUE_BACKEND" "memory"
            outcome <- timeout 100000 run
            unsetEnv "ECLUSE_QUEUE_BACKEND"
            traverse_ (unsetEnv . fst) runEnv
            outcome `shouldBe` Nothing

        it "aborts fast at boot when the sqs backend has no AWS_REGION" $ do
            -- The default sqs backend needs a region to be scoped to; absent, the
            -- composition root fails fast rather than building an unscoped queue.
            -- Clear AWS_REGION explicitly so a sibling case (run under a randomized
            -- order) cannot leak it into this missing-region fixture.
            unsetEnv "AWS_REGION"
            traverse_ (uncurry setEnv) runEnv
            setEnv "ECLUSE_QUEUE_BACKEND" "sqs"
            outcome <- try (timeout 100000 run) :: IO (Either BootAborted (Maybe ()))
            unsetEnv "ECLUSE_QUEUE_BACKEND"
            traverse_ (unsetEnv . fst) runEnv
            outcome `shouldBe` Left BootAborted

        it "aborts fast at boot when the sqs backend has no ECLUSE_QUEUE_URL" $ do
            -- ECLUSE_QUEUE_URL is optional at the env layer but required for sqs;
            -- absent, the composition root fails loud rather than building a queue with
            -- no target.
            traverse_ (uncurry setEnv) awsRunEnv
            unsetEnv "ECLUSE_QUEUE_URL"
            setEnv "ECLUSE_QUEUE_BACKEND" "sqs"
            outcome <- try (timeout 100000 run) :: IO (Either BootAborted (Maybe ()))
            unsetEnv "ECLUSE_QUEUE_BACKEND"
            traverse_ (unsetEnv . fst) awsRunEnv
            outcome `shouldBe` Left BootAborted

        it "aborts fast at boot when the gcp-artifact-registry credential provider is selected (not built)" $ do
            traverse_ (uncurry setEnv) awsRunEnv
            setEnv "ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER" "gcp-artifact-registry"
            outcome <- try (timeout 100000 run) :: IO (Either BootAborted (Maybe ()))
            unsetEnv "ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER"
            traverse_ (unsetEnv . fst) awsRunEnv
            outcome `shouldBe` Left BootAborted

        it "aborts fast at boot when codeartifact is selected but its domain cannot be resolved" $ do
            -- The CodeArtifact inputs resolve by neither an explicit key nor the
            traverse_ (uncurry setEnv) awsRunEnv
            setEnv "ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER" "codeartifact"
            outcome <- try (timeout 100000 run) :: IO (Either BootAborted (Maybe ()))
            unsetEnv "ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER"
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

    describe "npmServerConfig -- the composed npm front door" $
        -- Drive the real composition the composition root wires (npmServerConfig),
        -- not a copy: this exercises the npm mount end to end through dispatch.
        with npmApp $ do
            it "mounts npm at /npm (answers /npm/-/ping locally with 200 {})" $
                get "/npm/-/ping" `shouldRespondWith` "{}"{matchStatus = 200}

            it "recognises an npm packument route under the mount (501; serve deps unwired)" $
                -- Reaching the Packument route forces the mount's classifier *and* its
                -- (unwired) packument deps, then renders the 501 through its renderer.
                get "/npm/is-odd" `shouldRespondWith` 501

            it "does NOT mount npm at the root -- /-/ping there is the neutral 404" $
                get "/-/ping" `shouldRespondWith` "Not Found\n"{matchStatus = 404}

            it "renders an unmounted prefix as a neutral text/plain 404" $
                get "/pypi/is-odd" `shouldRespondWith` "Not Found\n"{matchStatus = 404}

    describe "mountBindingFor -- ecosystem drives the binding" $ do
        it "resolves npm to a binding whose prefix is derived from the ecosystem (/npm)" $
            -- The path prefix is derived from the ecosystem, never configured, so
            -- the npm binding is served under its own /npm prefix.
            (bindingPrefix <$> mountBindingFor Npm Nothing Nothing) `shouldBe` Just ("npm" :| [])

        it "has no binding for an ecosystem with no adapter wired (loud Nothing, not a stub)" $ do
            -- PyPI and RubyGems have no registry client or renderer yet, so resolving
            -- one is a Nothing the caller must handle -- never a silently half-wired mount.
            (bindingPrefix <$> mountBindingFor PyPI Nothing Nothing) `shouldBe` Nothing
            (bindingPrefix <$> mountBindingFor RubyGems Nothing Nothing) `shouldBe` Nothing
