-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.WorkerSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse (mountBindingFor)
import Ecluse.Composition (PublishTarget (ptEcosystem), planMounts, planPublishTargets)
import Ecluse.Composition.Support (expectConfig, expectProviders, expectValidated, fixedNow, staticEnvVars, testLimits)
import Ecluse.Composition.Worker (mirrorTransportFor, workerPoliciesFor)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Registry.Publish (MirrorTransport (ptLimits))
import Ecluse.Core.Security (Limits (maxBodyBytes), defaultLimits)
import Ecluse.Core.Server.Context (MountBinding (bindingPackumentDeps), PackumentDeps (pdLimits, pdMinIntegrity))
import Ecluse.Core.Worker (WorkerPolicy (wpArtifactLimits, wpMinIntegrity, wpNow))
import Ecluse.Runtime.Env (Env)
import Ecluse.Runtime.Test.Support (newTestEnv)
import Ecluse.Test.Rules (inertRuleDeps)

{- | Tests for the composition root's worker bundle construction. Construction only, no network:
every bundle field is a closure the worker applies later.
-}
spec :: Spec
spec = describe "workerPoliciesFor (config plus adapters in, WorkerPolicies out)" $ do
    it "builds one bundle per served mount, keyed by its ecosystem" $ do
        (env, bindings, targets) <- composedFixtures
        Map.keys (workerPoliciesFor env bindings targets testArtifactCap) `shouldBe` [Npm]

    it "reuses the mount's own serve-side policy inputs on the bundle" $ do
        -- The floor and every sibling input is the mount's own 'PackumentDeps' value, so the
        -- ingest decision cannot diverge from the serve decision.
        (env, bindings, targets) <- composedFixtures
        deps <- case bindings of
            [binding] -> pure (bindingPackumentDeps binding)
            _ -> fail "expected exactly one served binding"
        case Map.lookup Npm (workerPoliciesFor env bindings targets testArtifactCap) of
            Nothing -> expectationFailure "expected an npm bundle"
            Just policy -> do
                wpMinIntegrity policy `shouldBe` pdMinIntegrity deps
                now <- wpNow policy
                now `shouldBe` fixedNow

    it "contributes no bundle for an ecosystem without a resolved publish target" $ do
        -- The bundle is whole or absent: without a publish target there is no mirror
        -- write to marry, so no half-wired bundle exists. A job for the ecosystem then
        -- fails closed at the worker rather than publishing nowhere.
        (env, bindings, _) <- composedFixtures
        Map.keys (workerPoliciesFor env bindings [] testArtifactCap) `shouldBe` []

    it "sizes the bundle's artifact fetch cap from the supplied plan value" $ do
        -- The per-artifact byte cap comes from the memory plan's mirror-artifact tenant, not a
        -- hard-coded constant.
        (env, bindings, targets) <- composedFixtures
        case Map.lookup Npm (workerPoliciesFor env bindings targets testArtifactCap) of
            Nothing -> expectationFailure "expected an npm bundle"
            Just policy -> maxBodyBytes (wpArtifactLimits policy) `shouldBe` testArtifactCap

    it "reads the mirror presence probe under the mount's plan-resolved response bound, not the metadata-path default (issue #851)" $ do
        -- The probe honours the same plan-resolved response bound as every other metadata read on
        -- the mount, so an oversized mirror packument cannot silently defeat duplicate suppression.
        (env, bindings, targets) <- composedFixturesWith probeLimits
        deps <- case bindings of
            [binding] -> pure (bindingPackumentDeps binding)
            _ -> fail "expected exactly one served binding"
        target <- case find ((== Npm) . ptEcosystem) targets of
            Just t -> pure t
            Nothing -> fail "expected an npm publish target"
        let transport = mirrorTransportFor env deps target
        ptLimits transport `shouldBe` pdLimits deps
        ptLimits transport `shouldNotBe` defaultLimits

-- A distinctive artifact fetch cap, so the thread-through assertion pins the exact
-- value the composition root would pass rather than any incidental default.
testArtifactCap :: Int
testArtifactCap = 40 * 1024 * 1024

-- A distinctive plan-resolved response bound, below the shipped default, so the
-- probe-bound pin fails were the wiring to revert to the metadata-path default.
probeLimits :: Limits
probeLimits = defaultLimits{maxBodyBytes = 3 * 1024 * 1024}

-- The composed inputs the production boot path derives, over no-network doubles.
composedFixtures :: IO (Env, [MountBinding], [PublishTarget])
composedFixtures = composedFixturesWith testLimits

-- 'composedFixtures' with an explicit resolved 'Limits', so a test can pin that a plan-resolved
-- bound, not the shipped default, reaches the wiring.
composedFixturesWith :: Limits -> IO (Env, [MountBinding], [PublishTarget])
composedFixturesWith limits = do
    config <- expectConfig staticEnvVars Nothing
    providers <- expectProviders config
    plan <- expectValidated config
    bindings <-
        planMounts mountBindingFor (pure fixedNow) (const inertRuleDeps) providers limits Nothing plan
            >>= either (\errs -> fail ("unexpected boot errors: " <> show errs)) pure
    targets <-
        either (\errs -> fail ("unexpected publish-target errors: " <> show errs)) pure (planPublishTargets providers plan)
    env <- newTestEnv
    pure (env, bindings, targets)
