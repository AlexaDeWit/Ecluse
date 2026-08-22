-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.EnvSpec (spec) where

import Network.HTTP.Client (defaultManagerSettings, newManager)
import Test.Hspec
import UnliftIO (evaluate, timeout, try)
import UnliftIO.Exception (StringException, throwString)

import Ecluse (mountBindingFor, runServer, runWorker)
import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Queue (MirrorJob (..), enqueue, msgJob, receive)
import Ecluse.Core.Server.Cache (newMetadataCache)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Runtime.Env (Env (..), newWorkerHeartbeat, withEnvWithAdmission)
import Ecluse.Runtime.Server (ServerConfig, mkServerConfig, scPort)
import Ecluse.Runtime.Telemetry (telemetryDisabled, telemetryMeterProvider, telemetryTracerProvider)
import Ecluse.Runtime.Test.Support (newTestEnv)
import Ecluse.Test.Package (unsafeRegistryUrl)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Server.Cache (defaultCacheConfig)
import Ecluse.Test.Server.Mount (inertPackumentDeps)
import Ecluse.Test.Support (newTestLogEnv, testServeAdmission)

{- | A single npm mount with inert packument-serve dependencies and no publish target,
resolved the way the composition root resolves it.
-}
npmTestConfig :: ServerConfig
npmTestConfig = mkServerConfig (maybeToList (mountBindingFor Npm inertPackumentDeps Nothing))

-- | A sample job for round-tripping the queue handle held in an 'Env'.
sampleJob :: MirrorJob
sampleJob =
    MirrorJob
        { jobPackage = pkg
        , jobVersion = ver
        , jobArtifactUrl = unsafeRegistryUrl "https://registry.example.test/thing/-/thing-1.0.0.tgz"
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
            -- A 'Manager' has no 'Eq'\/'Show' and no network-free observable, so forcing
            -- 'envManager' to weak-head normal form without a bottom is the assertion.
            env <- newTestEnv
            _ <- evaluate (envManager env)
            pure ()

        it "exposes the trusted private-origin manager it was built with" $ do
            -- The trusted private-origin manager is likewise opaque, so forcing the accessor
            -- to weak-head normal form is the assertion.
            env <- newTestEnv
            _ <- evaluate (envPrivateManager env)
            pure ()

        it "exposes the LogEnv it was built with" $ do
            -- A 'LogEnv' is likewise opaque, so forcing 'envLogEnv' to weak-head normal form
            -- is the assertion.
            env <- newTestEnv
            _ <- evaluate (envLogEnv env)
            pure ()

        it "wires the telemetry handle through (the off-by-default no-op)" $ do
            -- The default substrate is off, so the handle exposes no providers: telemetry is
            -- inert, not unsampled. A 'TracerProvider' has no 'Show', hence 'isNothing'.
            env <- newTestEnv
            isNothing (telemetryTracerProvider (envTelemetry env)) `shouldBe` True
            isNothing (telemetryMeterProvider (envTelemetry env)) `shouldBe` True

    describe "withEnvWithAdmission" $ do
        it "runs the body against the assembled Env and returns its result" $ do
            queue <- newTestMemoryQueue
            manager <- newManager defaultManagerSettings
            metadataCache <- newMetadataCache defaultCacheConfig
            logEnv <- newTestLogEnv
            heartbeat <- newWorkerHeartbeat
            admission <- testServeAdmission
            withEnvWithAdmission admission queue manager manager metadataCache logEnv telemetryDisabled heartbeat (\_ -> pure ())

        it "propagates an exception thrown in the body (the Env scopes the action, nothing swallows it)" $ do
            queue <- newTestMemoryQueue
            manager <- newManager defaultManagerSettings
            metadataCache <- newMetadataCache defaultCacheConfig
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
            -- The listener blocks until cancelled, so 'timeout' yields 'Nothing'. 'scPort = 0'
            -- binds an OS-assigned ephemeral port, so the test never races a fixed port in use.
            env <- newTestEnv
            timeout 100000 (runServer (npmTestConfig{scPort = 0}) env) `shouldReturn` Nothing

        it "runWorker over an Env serves (blocks polling) rather than returning" $ do
            -- The consume loop long-polls the empty in-memory queue until cancelled, so
            -- 'timeout' yields 'Nothing'.
            env <- newTestEnv
            -- The empty queue needs no re-evaluation policies: the loop only ever
            -- long-polls, with no job to re-evaluate, which is what this asserts.
            timeout 100000 (runWorker mempty env) `shouldReturn` Nothing
