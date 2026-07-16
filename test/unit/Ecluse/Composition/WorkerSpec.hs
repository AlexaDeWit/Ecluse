-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.WorkerSpec (spec) where

import Data.Map.Strict qualified as Map
import Katip (Environment (Environment), Namespace (Namespace), initLogEnv)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Test.Hspec

import Ecluse (mountBindingFor)
import Ecluse.Composition (PublishTarget, planMounts, planPublishTargets)
import Ecluse.Composition.Support (expectConfig, expectProviders, fixedNow, staticEnvVars)
import Ecluse.Composition.Worker (workerPoliciesFor)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Server.Cache (newMetadataCache)
import Ecluse.Core.Server.Context (MountBinding (bindingPackumentDeps), PackumentDeps (pdMinIntegrity))
import Ecluse.Core.Worker (WorkerPolicy (wpMinIntegrity, wpNow))
import Ecluse.Runtime.Env (Env, newEnvWithAdmission, newWorkerHeartbeat)
import Ecluse.Runtime.Telemetry (telemetryDisabled)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Rules (inertRuleDeps)
import Ecluse.Test.Server.Cache (defaultCacheConfig)
import Ecluse.Test.Support (testServeAdmission)

{- | Tests for the composition root's worker bundle construction: the served mounts,
the resolved publish targets, and the adapter registry in; the per-ecosystem
'Ecluse.Core.Worker.WorkerPolicies' out. Construction only, no network: every
bundle field is a closure the worker applies later, so these pins assert what is
wired (and what deliberately is not), never a live fetch or publish.
-}
spec :: Spec
spec = describe "workerPoliciesFor (config plus adapters in, WorkerPolicies out)" $ do
    it "builds one bundle per served mount, keyed by its ecosystem" $ do
        (env, bindings, targets) <- composedFixtures
        Map.keys (workerPoliciesFor env bindings targets) `shouldBe` [Npm]

    it "reuses the mount's own serve-side policy inputs on the bundle" $ do
        -- The floor (and every sibling input) is the mount's own 'PackumentDeps'
        -- value, so the ingest decision cannot diverge from the serve decision; the
        -- injected clock rides through likewise.
        (env, bindings, targets) <- composedFixtures
        deps <- case bindings of
            [binding] -> pure (bindingPackumentDeps binding)
            _ -> fail "expected exactly one served binding"
        case Map.lookup Npm (workerPoliciesFor env bindings targets) of
            Nothing -> expectationFailure "expected an npm bundle"
            Just policy -> do
                wpMinIntegrity policy `shouldBe` pdMinIntegrity deps
                now <- wpNow policy
                now `shouldBe` fixedNow

    it "contributes no bundle for an ecosystem without a resolved publish target" $ do
        -- The bundle is whole or absent: without a publish target there is no
        -- mirror write to marry, so no half-wired bundle exists and a job for the
        -- ecosystem is fail-closed at the worker rather than publishing nowhere.
        (env, bindings, _) <- composedFixtures
        Map.keys (workerPoliciesFor env bindings []) `shouldBe` []

-- The composed inputs the production boot path derives: the served bindings and
-- publish targets from the static single-mount environment, and an Env over
-- no-network doubles.
composedFixtures :: IO (Env, [MountBinding], [PublishTarget])
composedFixtures = do
    config <- expectConfig staticEnvVars Nothing
    providers <- expectProviders config
    bindings <-
        planMounts mountBindingFor (pure fixedNow) (const inertRuleDeps) providers config
            >>= either (\errs -> fail ("unexpected boot errors: " <> show errs)) pure
    targets <-
        either (\errs -> fail ("unexpected publish-target errors: " <> show errs)) pure (planPublishTargets providers config)
    env <- newTestEnv
    pure (env, bindings, targets)

newTestEnv :: IO Env
newTestEnv = do
    queue <- newTestMemoryQueue
    manager <- newManager defaultManagerSettings
    metadataCache <- newMetadataCache defaultCacheConfig
    logEnv <- initLogEnv (Namespace ["ecluse"]) (Environment "test")
    heartbeat <- newWorkerHeartbeat
    admission <- testServeAdmission
    newEnvWithAdmission admission queue manager manager metadataCache logEnv telemetryDisabled heartbeat
