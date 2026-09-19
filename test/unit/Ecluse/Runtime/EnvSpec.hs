-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Runtime.EnvSpec (spec) where

import Network.HTTP.Client (defaultManagerSettings, newManager)
import Test.Hspec
import UnliftIO (evaluate, throwIO, timeout, try)

import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Queue (enqueue, msgJob, receive)
import Ecluse.Core.Server.Cache (newMetadataCache)
import Ecluse.Proxy (runServer)
import Ecluse.Runtime.Env (Env (..), newWorkerHeartbeat, withEnvWithAdmission)
import Ecluse.Runtime.Server (ServerConfig, mkServerConfig, scPort)
import Ecluse.Runtime.Telemetry (telemetryDisabled, telemetryMeterProvider, telemetryTracerProvider)
import Ecluse.Runtime.Test.Support (newTestEnv)
import Ecluse.Service (mountBindingFor, runWorker)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Queue (newTestMemoryQueue, sampleJob)
import Ecluse.Test.Server.Cache (defaultCacheConfig)
import Ecluse.Test.Server.Mount (inertPackumentDeps)
import Ecluse.Test.Support (testServeAdmission)

{- | A single npm mount with inert packument-serve dependencies and no publish target,
resolved the way the composition root resolves it.
-}
npmTestConfig :: ServerConfig
npmTestConfig = mkServerConfig (maybeToList (mountBindingFor Npm inertPackumentDeps Nothing))

-- | The body's own fault, so the assertion names the exception the body raised.
data BodyEscape = BodyEscape
    deriving stock (Eq, Show)

instance Exception BodyEscape

{- | The handles the 'Env' carries with no 'Eq', no 'Show', and no network-free observable.
Forcing each accessor to weak-head normal form without a bottom is all a case can assert.
-}
opaqueHandles :: [(String, Env -> IO ())]
opaqueHandles =
    [ ("shared HTTP manager", void . evaluate . envManager)
    , ("trusted private-origin manager", void . evaluate . envPrivateManager)
    , ("LogEnv", void . evaluate . envLogEnv)
    ]

spec :: Spec
spec = do
    describe "newEnvWithAdmission" $ do
        it "wires the queue handle through (a job enqueued via Env is received via Env)" $ do
            env <- newTestEnv
            enqueue (envQueue env) sampleJob >>= (`shouldBe` Right ())
            msgs <- receive (envQueue env)
            fmap (map msgJob) msgs `shouldBe` Right [sampleJob]

        for_ opaqueHandles $ \(handle, forceHandle) ->
            it ("exposes the " <> handle <> " it was built with") $ do
                env <- newTestEnv
                forceHandle env

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
                body _ = throwIO BodyEscape
            outcome <- try (withEnvWithAdmission admission queue manager manager metadataCache logEnv telemetryDisabled heartbeat body)
            outcome `shouldBe` Left BodyEscape

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
