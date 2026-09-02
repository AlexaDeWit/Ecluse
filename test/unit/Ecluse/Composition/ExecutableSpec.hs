-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.ExecutableSpec (spec) where

import Data.Text qualified as T
import Test.Hspec
import UnliftIO (throwString)

import Ecluse.Composition (
    BootWiring (bwBindings, bwPublishTargets),
    PublishTarget (ptEcosystem),
    ResolveAdapter,
 )
import Ecluse.Composition.BootError (BootError (MirrorQueueUnavailable, MissingAdapter))
import Ecluse.Composition.Executable (
    BuildMirrorQueue,
    ExecutablePlan (epBootPlan, epRoleWiring),
    MirrorWiring (mwBootWiring, mwCveSync, mwRole),
    RoleWiring (MirrorPipelineWiring, PilotWiring, StorePrunerWiring),
    planExecutable,
 )
import Ecluse.Composition.Plan (BootPlan (bpRole))
import Ecluse.Composition.Support (expectConfig, expectPlanFor, noCeiling, staticEnvVars)
import Ecluse.Composition.Types (
    BootRole (BootMirrorPipeline, BootStorePruner, BootWithoutPipeline),
    MirrorRole (ServeAndMirror),
 )
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Queue (noMirrorQueue)
import Ecluse.Core.Server.Context (MountBinding (bindingPrefix))
import Ecluse.Service (mountBindingFor)
import Ecluse.Test.Log (newTestLogEnv)

{- | Tests the boot's effectful planning phase. Every role plans through it, and every refusal a
live environment can settle is spent there, so a yielded plan is one nothing downstream rejects.
-}
spec :: Spec
spec = describe "planExecutable" $ do
    it "yields the mounts and the publish targets a mirror-pipeline role assembles from" $ do
        plan <- expectExecutable (BootMirrorPipeline ServeAndMirror) mountBindingFor inertQueue
        mirror <- expectMirrorWiring plan
        mwRole mirror `shouldBe` ServeAndMirror
        map bindingPrefix (bwBindings (mwBootWiring mirror)) `shouldBe` [pure "npm"]
        map ptEcosystem (bwPublishTargets (mwBootWiring mirror)) `shouldBe` [Npm]
        -- No advisory store is configured, so the map is empty and readiness is ungated.
        null (mwCveSync mirror) `shouldBe` True

    it "refuses, and yields no plan, where a cleared mount resolves to no binding" $ do
        -- The refusal this phase raises without a cloud. The injected resolver stands in for a
        -- build shipping no adapter, which is what makes the wiring, not the pure pass, refuse.
        outcome <- planFor (BootMirrorPipeline ServeAndMirror) (\_ _ _ -> Nothing) inertQueue
        case outcome of
            Right _ -> expectationFailure "expected the planning phase to refuse"
            Left errs -> errs `shouldBe` [MissingAdapter Npm]

    it "refuses a mirror-queue backend the live environment cannot build" $ do
        -- The backend dials its provider at boot. Before this phase owned it the throw escaped
        -- past the gate into the assembly, which claims nothing there can refuse.
        outcome <- planFor (BootMirrorPipeline ServeAndMirror) mountBindingFor refusingQueue
        case outcome of
            Right _ -> expectationFailure "expected the planning phase to refuse"
            Left [MirrorQueueUnavailable detail] -> detail `shouldSatisfy` T.isInfixOf "no credentials"
            Left errs -> expectationFailure ("expected one queue refusal, got: " <> show errs)

    it "reports the queue refusal and the wiring refusal from one run" $ do
        -- The two refusable steps accumulate, so an operator fixes both before the next boot
        -- rather than meeting the second one only once the first is gone.
        outcome <- planFor (BootMirrorPipeline ServeAndMirror) (\_ _ _ -> Nothing) refusingQueue
        case outcome of
            Right _ -> expectationFailure "expected the planning phase to refuse"
            Left [MirrorQueueUnavailable _, MissingAdapter Npm] -> pass
            Left errs -> expectationFailure ("expected both refusals in one list, got: " <> show errs)

    it "plans the store pruner and the pilot through the same phase, each on its own arm" $ do
        -- Neither arm resolves an adapter or builds a queue, so ports that refuse outright leave
        -- both roles clearing exactly as they do with working ones. The gate still stands ahead
        -- of them, which is where a refusal either role later needs is spent.
        pruner <- expectExecutable BootStorePruner (\_ _ _ -> Nothing) refusingQueue
        plannedArm (epRoleWiring pruner) `shouldBe` "store pruner"
        bpRole (epBootPlan pruner) `shouldBe` BootStorePruner
        pilot <- expectExecutable BootWithoutPipeline (\_ _ _ -> Nothing) refusingQueue
        plannedArm (epRoleWiring pilot) `shouldBe` "pilot"
        bpRole (epBootPlan pilot) `shouldBe` BootWithoutPipeline

-- | Which arm of the phase a plan came back through, so an assertion names it rather than a shape.
plannedArm :: RoleWiring -> Text
plannedArm = \case
    MirrorPipelineWiring _ -> "mirror pipeline"
    StorePrunerWiring -> "store pruner"
    PilotWiring -> "pilot"

-- | A queue builder that allocates nothing, for the arms whose refusals are elsewhere.
inertQueue :: BuildMirrorQueue
inertQueue _ _ _ = pure noMirrorQueue

{- | A queue builder that throws as @amazonka@ does when it discovers no credentials, which is the
live call this phase now folds into a refusal.
-}
refusingQueue :: BuildMirrorQueue
refusingQueue _ _ _ = throwString "no credentials"

-- | Plan a boot over 'staticEnvVars' for one role, through the given ports.
planFor :: BootRole -> ResolveAdapter -> BuildMirrorQueue -> IO (Either [BootError] ExecutablePlan)
planFor role resolveAdapter buildQueue = do
    config <- expectConfig staticEnvVars Nothing
    bootPlan <- expectPlanFor role staticEnvVars Nothing config noCeiling
    logEnv <- newTestLogEnv
    planExecutable logEnv resolveAdapter buildQueue bootPlan

-- | 'planFor', failing the test on a refusal.
expectExecutable :: BootRole -> ResolveAdapter -> BuildMirrorQueue -> IO ExecutablePlan
expectExecutable role resolveAdapter buildQueue =
    planFor role resolveAdapter buildQueue
        >>= either (\errs -> fail ("planning refused: " <> show errs)) pure

-- | The mirror-pipeline arm of a plan, failing the test on any other arm.
expectMirrorWiring :: ExecutablePlan -> IO MirrorWiring
expectMirrorWiring plan = case epRoleWiring plan of
    MirrorPipelineWiring mirror -> pure mirror
    other -> fail ("expected the mirror-pipeline arm, got the " <> toString (plannedArm other) <> " arm")
