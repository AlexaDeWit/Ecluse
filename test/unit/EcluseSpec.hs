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
        , parsePackageInfo = const (Left unusedParse)
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
separately by layering @PROXY_CONFIG@ on top of this base.
-}
runEnv :: [(String, String)]
runEnv =
    [ ("PRIVATE_UPSTREAM_URL", "https://private.example.test")
    , ("MIRROR_TARGET_URL", "https://mirror.example.test")
    , ("MIRROR_QUEUE_URL", "https://sqs.example.test/q")
    , ("MIRROR_TARGET_TOKEN", "mirror-write-token")
    , ("PROXY_PORT", "0")
    ]

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
            traverse_ (uncurry setEnv) runEnv
            outcome <- timeout 100000 run
            traverse_ (unsetEnv . fst) runEnv
            outcome `shouldBe` Nothing

        it "boots with an inline PROXY_CONFIG document and serves" $ do
            traverse_ (uncurry setEnv) runEnv
            setEnv "PROXY_CONFIG" "{\"rules\":{}}"
            outcome <- timeout 100000 run
            unsetEnv "PROXY_CONFIG"
            traverse_ (unsetEnv . fst) runEnv
            outcome `shouldBe` Nothing

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
