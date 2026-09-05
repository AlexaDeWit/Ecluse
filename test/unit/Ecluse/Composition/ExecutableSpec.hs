-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.ExecutableSpec (spec) where

import Data.Text qualified as T
import Test.Hspec
import UnliftIO.Exception (throwIO)

import Ecluse.Composition (
    BootWiring (bwBindings, bwPublishTargets),
    PublishTarget (ptEcosystem),
    ResolveAdapter,
 )
import Ecluse.Composition.BootError (
    BootError (
        AdvisorySyncUnavailable,
        MirrorQueueUnavailable,
        MissingAdapter,
        StoreMaintenanceUnavailable,
        StorePrunerWithoutSweep
    ),
    StoreMaintenanceReason (ClientBuildFailed),
 )
import Ecluse.Composition.Executable (
    BuildMirrorQueue,
    ExecutablePlan (epBootPlan, epRoleWiring),
    MirrorWiring (mwBootWiring, mwCveSync, mwRole),
    RoleWiring (MirrorPipelineWiring, PilotWiring),
    planExecutable,
 )
import Ecluse.Composition.Maintenance (BuildStoreMaintenance)
import Ecluse.Composition.Plan (BootPlan (bpRole))
import Ecluse.Composition.Support (codeArtifactEnvVars, expectConfig, expectPlanFor, noCeiling, overrideEnv, staticEnvVars)
import Ecluse.Composition.Types (
    BootRole (BootMirrorPipeline, BootStorePruner, BootWithoutPipeline),
    MirrorRole (ServeAndMirror),
 )
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Queue (noMirrorQueue)
import Ecluse.Core.Server.Context (MountBinding (bindingPrefix))
import Ecluse.Service (mountBindingFor)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Maintenance (FakeStore (fakeMaintenance), defaultFakeStoreConfig, newFakeStore)

{- | Tests the boot's effectful planning phase. Every role plans through it, and every refusal a
live environment can settle is spent there, so a yielded plan is one nothing downstream rejects.
-}
spec :: Spec
spec = describe "planExecutable" $ do
    it "yields the mounts and the publish targets a mirror-pipeline role assembles from" $ do
        plan <- expectExecutable (BootMirrorPipeline ServeAndMirror) mountBindingFor inertQueue inertStore
        mirror <- expectMirrorWiring plan
        mwRole mirror `shouldBe` ServeAndMirror
        map bindingPrefix (bwBindings (mwBootWiring mirror)) `shouldBe` [pure "npm"]
        map ptEcosystem (bwPublishTargets (mwBootWiring mirror)) `shouldBe` [Npm]
        -- No advisory store is configured, so the map is empty and readiness is ungated.
        null (mwCveSync mirror) `shouldBe` True

    it "refuses, and yields no plan, where a cleared mount resolves to no binding" $ do
        -- The refusal this phase raises without a cloud. The injected resolver stands in for a
        -- build shipping no adapter, which is what makes the wiring, not the pure pass, refuse.
        outcome <- planFor (BootMirrorPipeline ServeAndMirror) (\_ _ _ -> Nothing) inertQueue inertStore
        case outcome of
            Right _ -> expectationFailure "expected the planning phase to refuse"
            Left errs -> errs `shouldBe` [MissingAdapter Npm]

    it "refuses a mirror-queue backend the live environment cannot build" $ do
        -- The backend dials its provider at boot, so a throw there is a refusal at the gate and
        -- never a fault inside an assembly that claims nothing can refuse.
        outcome <- planFor (BootMirrorPipeline ServeAndMirror) mountBindingFor refusingQueue inertStore
        case outcome of
            Right _ -> expectationFailure "expected the planning phase to refuse"
            Left [MirrorQueueUnavailable detail] -> detail `shouldSatisfy` T.isInfixOf "NoCredentials"
            Left errs -> expectationFailure ("expected one queue refusal, got: " <> show errs)

    it "reports the queue refusal and the wiring refusal from one run" $ do
        -- The two refusable steps accumulate, so an operator fixes both before the next boot
        -- rather than meeting the second one only once the first is gone.
        outcome <- planFor (BootMirrorPipeline ServeAndMirror) (\_ _ _ -> Nothing) refusingQueue inertStore
        case outcome of
            Right _ -> expectationFailure "expected the planning phase to refuse"
            Left [MirrorQueueUnavailable _, MissingAdapter Npm] -> pass
            Left errs -> expectationFailure ("expected both refusals in one list, got: " <> show errs)

    it "refuses an advisory sync the live environment cannot prepare" $ do
        -- The sync creates its data directory and discovers the advisory store's credentials, and
        -- it runs a step ahead of the queue build, so a throw here would exit 1 past this gate.
        outcome <- planWith unwritableAdvisoryEnv (BootMirrorPipeline ServeAndMirror) mountBindingFor inertQueue inertStore
        case outcome of
            Right _ -> expectationFailure "expected the planning phase to refuse"
            Left [AdvisorySyncUnavailable detail] -> detail `shouldSatisfy` T.isInfixOf advisoryDataDir
            Left errs -> expectationFailure ("expected one advisory-sync refusal, got: " <> show errs)

    it "reports the advisory refusal beside the queue and wiring refusals from one run" $ do
        -- The advisory sync accumulates with the other two rather than short-circuiting them,
        -- which is what keeps one launch reporting every problem an operator must fix.
        outcome <- planWith unwritableAdvisoryEnv (BootMirrorPipeline ServeAndMirror) (\_ _ _ -> Nothing) refusingQueue inertStore
        case outcome of
            Right _ -> expectationFailure "expected the planning phase to refuse"
            Left [AdvisorySyncUnavailable _, MirrorQueueUnavailable _, MissingAdapter Npm] -> pass
            Left errs -> expectationFailure ("expected all three refusals in one list, got: " <> show errs)

    it "refuses the store pruner for want of a sweep, though every store and handle clears" $ do
        -- This build carries no sweep, so a started Dredger would hold a deleting identity and
        -- delete nothing. Its stores clear and its handles build, and the role still refuses.
        outcome <- planWith codeArtifactEnvVars BootStorePruner (\_ _ _ -> Nothing) refusingQueue inertStore
        case outcome of
            Right _ -> expectationFailure "expected the store pruner arm to refuse"
            Left errs -> errs `shouldBe` [StorePrunerWithoutSweep]

    it "reports a store maintenance client the live environment cannot build ahead of that refusal" $ do
        -- The client discovers an AWS identity when it is built, so an environment with none
        -- refuses here rather than failing the Dredger's first call against the store.
        outcome <- planWith codeArtifactEnvVars BootStorePruner (\_ _ _ -> Nothing) refusingQueue refusingStore
        case outcome of
            Right _ -> expectationFailure "expected the planning phase to refuse"
            Left [StoreMaintenanceUnavailable Npm (ClientBuildFailed detail), StorePrunerWithoutSweep] ->
                detail `shouldSatisfy` T.isInfixOf "NoCredentials"
            Left errs -> expectationFailure ("expected the handle refusal then the sweep refusal, got: " <> show errs)

    it "plans the pilot through the same phase, on its own arm" $ do
        -- Nothing here needs a live environment, so ports that refuse outright leave the role
        -- clearing exactly as working ones do. The gate still stands ahead of it, which is
        -- where a refusal it later needs is spent.
        pilot <- expectExecutable BootWithoutPipeline (\_ _ _ -> Nothing) refusingQueue refusingStore
        plannedArm (epRoleWiring pilot) `shouldBe` "pilot"
        bpRole (epBootPlan pilot) `shouldBe` BootWithoutPipeline

-- | Which arm of the phase a plan came back through, so an assertion names it rather than a shape.
plannedArm :: RoleWiring -> Text
plannedArm = \case
    MirrorPipelineWiring _ -> "mirror pipeline"
    PilotWiring -> "pilot"

-- | A queue builder that allocates nothing, for the arms whose refusals are elsewhere.
inertQueue :: BuildMirrorQueue
inertQueue _ _ _ = pure noMirrorQueue

{- | A queue builder that throws as @amazonka@ does when it discovers no credentials, the live
call this phase folds into a refusal.
-}
refusingQueue :: BuildMirrorQueue
refusingQueue _ _ _ = throwIO NoCredentials

-- | A store builder that hands out the in-memory fake, so the pruner's arm reaches no cloud.
inertStore :: BuildStoreMaintenance
inertStore _ _ = fakeMaintenance <$> newFakeStore defaultFakeStoreConfig

-- | A store builder that throws as @amazonka@ does when it discovers no credentials.
refusingStore :: BuildStoreMaintenance
refusingStore _ _ = throwIO NoCredentials

-- | The typed stand-in for amazonka's credential-discovery failure.
data NoCredentials = NoCredentials
    deriving stock (Show)

instance Exception NoCredentials

{- | An advisory store over a data directory under a path that is not a directory, so preparing
the sync throws where every host behaves alike, before it reaches a credential chain.
-}
unwritableAdvisoryEnv :: [(String, String)]
unwritableAdvisoryEnv =
    overrideEnv "ECLUSE_ADVISORIES__DATA_DIR" advisoryDataDir $
        overrideEnv "ECLUSE_ADVISORIES__URL" "s3://advisories.example.test/ecluse" staticEnvVars

-- | The unwritable data directory 'unwritableAdvisoryEnv' points at, which its refusal names.
advisoryDataDir :: (IsString s) => s
advisoryDataDir = "/dev/null/ecluse-advisories"

-- | Plan a boot over 'staticEnvVars' for one role, through the given ports.
planFor :: BootRole -> ResolveAdapter -> BuildMirrorQueue -> BuildStoreMaintenance -> IO (Either [BootError] ExecutablePlan)
planFor = planWith staticEnvVars

-- | 'planFor' over a named environment layer, for a refusal 'staticEnvVars' cannot reach.
planWith :: [(String, String)] -> BootRole -> ResolveAdapter -> BuildMirrorQueue -> BuildStoreMaintenance -> IO (Either [BootError] ExecutablePlan)
planWith envVars role resolveAdapter buildQueue buildStore = do
    config <- expectConfig envVars Nothing
    bootPlan <- expectPlanFor role envVars Nothing config noCeiling
    logEnv <- newTestLogEnv
    planExecutable logEnv resolveAdapter buildQueue buildStore bootPlan

-- | 'planFor', failing the test on a refusal.
expectExecutable :: BootRole -> ResolveAdapter -> BuildMirrorQueue -> BuildStoreMaintenance -> IO ExecutablePlan
expectExecutable = expectExecutableWith staticEnvVars

-- | 'planWith', failing the test on a refusal.
expectExecutableWith :: [(String, String)] -> BootRole -> ResolveAdapter -> BuildMirrorQueue -> BuildStoreMaintenance -> IO ExecutablePlan
expectExecutableWith envVars role resolveAdapter buildQueue buildStore =
    planWith envVars role resolveAdapter buildQueue buildStore
        >>= either (\errs -> fail ("planning refused: " <> show errs)) pure

-- | The mirror-pipeline arm of a plan, failing the test on any other arm.
expectMirrorWiring :: ExecutablePlan -> IO MirrorWiring
expectMirrorWiring plan = case epRoleWiring plan of
    MirrorPipelineWiring mirror -> pure mirror
    other -> fail ("expected the mirror-pipeline arm, got the " <> toString (plannedArm other) <> " arm")
