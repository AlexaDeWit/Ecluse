-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.ExecutableSpec (spec) where

import Test.Hspec

import Ecluse.Composition (
    BootWiring (bwBindings, bwPublishTargets),
    PublishTarget (ptEcosystem),
    ResolveAdapter,
 )
import Ecluse.Composition.BootError (BootError (MissingAdapter))
import Ecluse.Composition.Executable (ExecutablePlan (epCveSync, epWiring), planExecutable)
import Ecluse.Composition.Support (expectConfig, expectPlan, noCeiling, staticEnvVars)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Server.Context (MountBinding (bindingPrefix))
import Ecluse.Service (mountBindingFor)
import Ecluse.Test.Log (newTestLogEnv)

{- | Tests the boot's effectful planning phase. Every refusal a live environment can settle is
spent here, so a yielded plan is what the assembly builds from and nothing there rejects one.
-}
spec :: Spec
spec = describe "planExecutable" $ do
    it "yields the mounts and the publish targets a mirror-pipeline role assembles from" $ do
        plan <- expectExecutable mountBindingFor
        map bindingPrefix (bwBindings (epWiring plan)) `shouldBe` [pure "npm"]
        map ptEcosystem (bwPublishTargets (epWiring plan)) `shouldBe` [Npm]
        -- No advisory store is configured, so the map is empty and readiness is ungated.
        null (epCveSync plan) `shouldBe` True

    it "refuses, and yields no plan, where a cleared mount resolves to no binding" $ do
        -- The refusal this phase raises without a cloud. The injected resolver stands in for a
        -- build shipping no adapter, which is what makes the wiring, not the pure pass, refuse.
        outcome <- planFor (\_ _ _ -> Nothing)
        case outcome of
            Left errs -> errs `shouldBe` [MissingAdapter Npm]
            Right _ -> expectationFailure "expected the planning phase to refuse"

-- | Plan a boot over 'staticEnvVars' through the given adapter resolver.
planFor :: ResolveAdapter -> IO (Either [BootError] ExecutablePlan)
planFor resolveAdapter = do
    config <- expectConfig staticEnvVars Nothing
    bootPlan <- expectPlan staticEnvVars Nothing config noCeiling
    logEnv <- newTestLogEnv
    planExecutable logEnv resolveAdapter bootPlan

-- | 'planFor', failing the test on a refusal.
expectExecutable :: ResolveAdapter -> IO ExecutablePlan
expectExecutable resolveAdapter =
    planFor resolveAdapter >>= either (\errs -> fail ("planning refused: " <> show errs)) pure
