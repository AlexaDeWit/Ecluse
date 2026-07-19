-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.WorkerSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse (mountBindingFor)
import Ecluse.Composition (PublishTarget (ptEcosystem), planMounts, planPublishTargets)
import Ecluse.Composition.Support (expectConfig, expectProviders, fixedNow, staticEnvVars, testLimits)
import Ecluse.Composition.Worker (mirrorTransportFor, workerPoliciesFor)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Registry.Publish (MirrorTransport (ptLimits))
import Ecluse.Core.Security (Limits (maxBodyBytes), defaultLimits)
import Ecluse.Core.Server.Context (MountBinding (bindingPackumentDeps), PackumentDeps (pdLimits, pdMinIntegrity))
import Ecluse.Core.Worker (WorkerPolicy (wpArtifactLimits, wpMinIntegrity, wpNow))
import Ecluse.Runtime.Env (Env)
import Ecluse.Runtime.Test.Support (newTestEnv)
import Ecluse.Test.Rules (inertRuleDeps)

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
        Map.keys (workerPoliciesFor env bindings targets testArtifactCap) `shouldBe` [Npm]

    it "reuses the mount's own serve-side policy inputs on the bundle" $ do
        -- The floor (and every sibling input) is the mount's own 'PackumentDeps'
        -- value, so the ingest decision cannot diverge from the serve decision; the
        -- injected clock rides through likewise.
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
        -- The bundle is whole or absent: without a publish target there is no
        -- mirror write to marry, so no half-wired bundle exists and a job for the
        -- ecosystem is fail-closed at the worker rather than publishing nowhere.
        (env, bindings, _) <- composedFixtures
        Map.keys (workerPoliciesFor env bindings [] testArtifactCap) `shouldBe` []

    it "sizes the bundle's artifact fetch cap from the supplied plan value" $ do
        -- The worker's per-artifact byte cap is threaded from the memory plan's
        -- mirror-artifact tenant (issue #846), not a hard-coded constant: the value
        -- the composition root passes is exactly the fetch bound each bundle carries.
        (env, bindings, targets) <- composedFixtures
        case Map.lookup Npm (workerPoliciesFor env bindings targets testArtifactCap) of
            Nothing -> expectationFailure "expected an npm bundle"
            Just policy -> maxBodyBytes (wpArtifactLimits policy) `shouldBe` testArtifactCap

    it "reads the mirror presence probe under the mount's plan-resolved response bound, not the metadata-path default (issue #851)" $ do
        -- The probe must honour the same boot-computed, operator-overridable response
        -- bound every other metadata read on the mount does ('pdLimits'), so a mirror
        -- packument larger than the shipped default cannot silently defeat duplicate
        -- suppression. A distinctive plan bound (below the default) pins that the wired
        -- transport tracks the plan value rather than the shipped constant: were the
        -- probe re-pinned to 'defaultLimits', 'shouldNotBe' would catch it.
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

-- The composed inputs the production boot path derives: the served bindings and
-- publish targets from the static single-mount environment, and an Env over
-- no-network doubles.
composedFixtures :: IO (Env, [MountBinding], [PublishTarget])
composedFixtures = composedFixturesWith testLimits

-- 'composedFixtures' with an explicit resolved 'Limits', so a test can pin that a
-- distinctive plan-resolved bound (not the shipped default) reaches the wiring it
-- exercises.
composedFixturesWith :: Limits -> IO (Env, [MountBinding], [PublishTarget])
composedFixturesWith limits = do
    config <- expectConfig staticEnvVars Nothing
    providers <- expectProviders config
    bindings <-
        planMounts mountBindingFor (pure fixedNow) (const inertRuleDeps) providers limits Nothing config
            >>= either (\errs -> fail ("unexpected boot errors: " <> show errs)) pure
    targets <-
        either (\errs -> fail ("unexpected publish-target errors: " <> show errs)) pure (planPublishTargets providers config)
    env <- newTestEnv
    pure (env, bindings, targets)
