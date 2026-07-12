-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.EnvSpec (spec) where

import Katip (Environment (Environment), LogEnv, Namespace (Namespace), initLogEnv)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Test.Hspec
import UnliftIO (evaluate, timeout, try)
import UnliftIO.Exception (StringException, throwString)

import Ecluse (mountBindingFor, runServer, runWorker)
import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Queue (MirrorJob (..), enqueue, msgJob, receive)
import Ecluse.Core.Server.Cache (CacheConfig (..), MetadataCache, newMetadataCache)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Runtime.Env (Env (..), newEnvWithAdmission, newWorkerHeartbeat, withEnvWithAdmission)
import Ecluse.Runtime.Server (ServerConfig, mkServerConfig, scPort)
import Ecluse.Runtime.Telemetry (telemetryDisabled, telemetryMeterProvider, telemetryTracerProvider)
import Ecluse.Test.Package (unsafeRegistryUrl)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Support (testServeAdmission)

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

{- | A cache config for the assembly tests: a short TTL over modest entry and byte
budgets. The exact tunables are immaterial here (no eviction is exercised); the
fixture is local so the spec depends on no particular default-config export.
-}
testCacheConfig :: CacheConfig
testCacheConfig =
    CacheConfig
        { cacheTtl = 60
        , cacheMaxEntries = 1024
        , cacheMaxBytes = 256 * 1024 * 1024
        }

-- | A metadata cache on the local test config (touches no network).
newTestCache :: IO MetadataCache
newTestCache = newMetadataCache testCacheConfig

{- | The npm front door the split-ready server test drives: a single npm mount with
no packument-serve or publish dependencies, assembled through the public binding
resolver exactly as the composition root would ('mountBindingFor' over npm).
-}
npmTestConfig :: ServerConfig
npmTestConfig = mkServerConfig (maybeToList (mountBindingFor Npm Nothing Nothing))

{- | Assemble an 'Env' from the doubles above, a no-network manager, a metadata
cache, and a scribe-free 'LogEnv'.
-}
newTestEnv :: IO Env
newTestEnv = do
    queue <- newTestMemoryQueue
    manager <- newTestManager
    metadataCache <- newTestCache
    logEnv <- newTestLogEnv
    heartbeat <- newWorkerHeartbeat
    admission <- testServeAdmission
    newEnvWithAdmission admission queue manager manager metadataCache logEnv telemetryDisabled heartbeat

-- | A sample job for round-tripping the queue handle held in an 'Env'.
sampleJob :: MirrorJob
sampleJob =
    MirrorJob
        { jobPackage = pkg
        , jobVersion = ver
        , jobArtifactUrl = unsafeRegistryUrl "https://public.test/thing/-/thing-1.0.0.tgz"
        , jobArtifactFilename = "thing-1.0.0.tgz"
        , jobTraceContext = Nothing
        }

-- | A sample package name and version, for the registry-handle assertions.
pkg :: PackageName
pkg = mkPackageName Npm Nothing "thing"

ver :: Version
ver = mkVersion Npm "1.0.0"

spec :: Spec
spec = do
    describe "newEnvWithAdmission" $ do
        it "assembles an Env from injected handle doubles, with no network" $ do
            -- Construction must not touch the network: it only gathers the handles
            -- and the manager. A clean return is the assertion.
            _env <- newTestEnv
            pure ()

        it "wires the queue handle through (a job enqueued via Env is received via Env)" $ do
            env <- newTestEnv
            enqueue (envQueue env) sampleJob >>= (`shouldBe` Right ())
            msgs <- receive (envQueue env)
            fmap (map msgJob) msgs `shouldBe` Right [sampleJob]

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
            -- exposes no providers -- telemetry is genuinely inert, not merely
            -- unsampled. A 'TracerProvider' has no 'Show', so each is checked
            -- through 'isNothing' rather than a printing matcher.
            env <- newTestEnv
            isNothing (telemetryTracerProvider (envTelemetry env)) `shouldBe` True
            isNothing (telemetryMeterProvider (envTelemetry env)) `shouldBe` True

    describe "withEnvWithAdmission" $ do
        it "runs the body against the assembled Env and returns its result" $ do
            queue <- newTestMemoryQueue
            manager <- newTestManager
            metadataCache <- newTestCache
            logEnv <- newTestLogEnv
            heartbeat <- newWorkerHeartbeat
            admission <- testServeAdmission
            withEnvWithAdmission admission queue manager manager metadataCache logEnv telemetryDisabled heartbeat (\_ -> pure ())

        it "propagates an exception thrown in the body (the Env scopes the action, nothing swallows it)" $ do
            queue <- newTestMemoryQueue
            manager <- newTestManager
            metadataCache <- newTestCache
            logEnv <- newTestLogEnv
            heartbeat <- newWorkerHeartbeat
            admission <- testServeAdmission
            let body :: Env -> IO ()
                body _ = throwString "boom"
            outcome <- try (withEnvWithAdmission admission queue manager manager metadataCache logEnv telemetryDisabled heartbeat body)
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
            timeout 100000 (runServer (npmTestConfig{scPort = 0}) env) `shouldReturn` Nothing

        it "runWorker over an Env serves (blocks polling) rather than returning" $ do
            -- The worker is a continuous consume loop: started under a short timeout
            -- it keeps long-polling the (empty in-memory) queue until cancelled, so
            -- 'timeout' yields 'Nothing'. The loop logic itself is asserted
            -- socket-free in "Ecluse.WorkerSpec".
            env <- newTestEnv
            -- No re-evaluation policies are needed here: the queue is empty, so the loop
            -- only ever long-polls (no job to re-evaluate), which is what this asserts.
            timeout 100000 (runWorker mempty env) `shouldReturn` Nothing
