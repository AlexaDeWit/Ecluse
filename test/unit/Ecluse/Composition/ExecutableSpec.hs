-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.ExecutableSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Environment (setEnv)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import UnliftIO.Exception (throwIO)

import Ecluse.Composition (
    BootWiring (bwBindings, bwPublishTargets),
    PublishTarget (ptEcosystem),
    ResolveAdapter,
 )
import Ecluse.Composition.BootError (
    Advisory (PrivateUpstreamUndecided),
    BootError (
        AdvisorySyncUnavailable,
        CodeArtifactMintFailed,
        MirrorQueueUnavailable,
        MissingAdapter,
        PilotWithoutEcosystem,
        PrivateUpstreamUnsafe,
        StoreMaintenanceUnavailable
    ),
    StoreMaintenanceReason (ClientBuildFailed, PrivateCacheUnavailable),
 )
import Ecluse.Composition.Credential (initTargetCredentialProviders, noCredentialProviders)
import Ecluse.Composition.Executable (
    BuildCredentials,
    BuildMirrorQueue,
    ExecutablePlan (epBootPlan, epRoleWiring),
    MirrorWiring (mwBootWiring, mwCveSync, mwRole),
    PrunerWiring (pwCveSync, pwMounts),
    RoleWiring (MirrorPipelineWiring, PilotWiring, StorePrunerWiring),
    planExecutable,
 )
import Ecluse.Composition.Maintenance (ClearedBackend (cbUrl), StoreBuilds (StoreBuilds, sbDeleting, sbObserving, sbProbing))
import Ecluse.Composition.Plan (BootPlan (bpRole))
import Ecluse.Composition.Support (codeArtifactEnvVars, expectConfig, expectPlanFor, noCeiling, overrideEnv, staticEnvVars, withObservablePrivate)
import Ecluse.Composition.Types (
    BootRole (BootMirrorPipeline, BootStorePreview, BootStorePruner, BootWithoutPipeline),
    MirrorRole (MirrorOnly, ServeAndMirror, ServeOnly),
 )
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Osv.Schema (EpssRequirement (EpssRequired))
import Ecluse.Core.Queue (noMirrorQueue)
import Ecluse.Core.Registry.Maintenance (StoredVersion (StoredVersion), VersionPresence (VersionServed))
import Ecluse.Core.Registry.Maintenance.Upstream (
    ExternalConnection (ExternalConnection),
    RepositoryName (RepositoryName),
    UndecidabilityReason (NoMechanism),
    UnsafeReason (ConfigurationEvidence),
    UpstreamSafety (Safe, Undecidable, Unsafe),
    noUpstreamMechanism,
 )
import Ecluse.Core.Registry.Sweep (sweepCycle)
import Ecluse.Core.Registry.Sweep.Types (CycleOutcome (outcomePrerequisites, outcomeTally), SweepMount (smEcosystem), SweepTally (tallyDeleted))
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Server.Context (MountBinding (bindingPrefix))
import Ecluse.Core.Version (mkVersion)
import Ecluse.Cve.Sync (CveSyncHandle (csEnv))
import Ecluse.Pilot.Plan (ExportLoopPlan (ExportIdle, ExportTo))
import Ecluse.Runtime.Cve.Sync (SyncEnv (syncEpssRequirement))
import Ecluse.Service (mountBindingFor)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Maintenance (FakeStore (fakeMaintenance, fakeObservation, readFakeContents), FakeStoreConfig (fakeContents, fakeManifests, fakeUpstream), defaultFakeStoreConfig, newFakeStore)
import Ecluse.Test.Package (sampleManifest, unscopedNpm)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Sweep (RecordedSweep (recPorts), previewingReport, recordingPortsUnder, testPacing)

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

    it "qualifies advisory consumers in both the mirror and Dredger plans" $
        withSystemTempDirectory "epss-role-plan" $ \dir -> do
            for_ [("AWS_ACCESS_KEY_ID", "test"), ("AWS_SECRET_ACCESS_KEY", "test"), ("AWS_REGION", "us-east-1")] $ uncurry setEnv
            for_ [(BootMirrorPipeline ServeAndMirror, staticEnvVars), (BootStorePruner, codeArtifactEnvVars)] $ \(role, mountEnv) -> do
                let envVars =
                        overrideEnv "ECLUSE_ADVISORIES__DATA_DIR" dir $
                            overrideEnv "ECLUSE_ADVISORIES__URL" advisoryStoreUrl $
                                overrideEnv "ECLUSE_RULES" "{\"risk\":{\"type\":\"DenyIfEpss\",\"minEpss\":0.5}}" mountEnv
                plan <- expectExecutableWith envVars role mountBindingFor inertQueue inertStore
                handles <- case epRoleWiring plan of
                    MirrorPipelineWiring mirror -> pure (mwCveSync mirror)
                    StorePrunerWiring pruner -> pure (pwCveSync pruner)
                    other -> fail ("expected an advisory consumer, got " <> toString (plannedArm other))
                Map.map (syncEpssRequirement . csEnv) handles `shouldBe` Map.singleton Npm EpssRequired

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

    for_ [ServeAndMirror, MirrorOnly] $ \role ->
        it ("refuses a mirror-write mint the live environment refuses, under " <> show role) $ do
            -- Both roles write to the mirror store, so a bad identity refuses here rather than
            -- on the first publish an admitted version asks for.
            outcome <- planUnder staticEnvVars (BootMirrorPipeline role) mountBindingFor inertQueue refusingCredentials inertStore
            case outcome of
                Right _ -> expectationFailure "expected the planning phase to refuse"
                Left errs -> errs `shouldBe` [CodeArtifactMintFailed ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET" :| []) "no identity answered"]

    it "mints nothing on a serve-only boot, and plans it no publish target" $ do
        -- A build that refuses every mint still yields the plan, because the serve-only role
        -- calls none: its identity needs no rights over the mirror store.
        outcome <- planUnder staticEnvVars (BootMirrorPipeline ServeOnly) mountBindingFor inertQueue refusingCredentials inertStore
        plan <- either (\errs -> fail ("planning refused: " <> show errs)) pure outcome
        mirror <- expectMirrorWiring plan
        map bindingPrefix (bwBindings (mwBootWiring mirror)) `shouldBe` [pure "npm"]
        map ptEcosystem (bwPublishTargets (mwBootWiring mirror)) `shouldBe` []

    it "plans the store pruner a sweepable mount per cleared store" $ do
        -- The build carries a sweep, so the arm yields the plan rather than refusing: one mount
        -- per store the pass cleared, carrying what decides for it.
        pruner <- expectExecutableWith codeArtifactEnvVars BootStorePruner (\_ _ _ -> Nothing) refusingQueue inertStore
        plannedArm (epRoleWiring pruner) `shouldBe` "store pruner"
        case epRoleWiring pruner of
            StorePrunerWiring wiring -> map smEcosystem (pwMounts wiring) `shouldBe` [Npm]
            other -> expectationFailure ("expected the store pruner arm, got the " <> toString (plannedArm other) <> " arm")

    it "plans the preview role through the observing build alone" $ do
        -- The role picks its own build, so a preview's boot never runs the one holding a delete.
        preview <-
            expectExecutableWith (withObservablePrivate codeArtifactEnvVars) BootStorePreview (\_ _ _ -> Nothing) refusingQueue observingOnly
        case epRoleWiring preview of
            StorePrunerWiring wiring -> map smEcosystem (pwMounts wiring) `shouldBe` [Npm]
            other -> expectationFailure ("expected the store pruner arm, got the " <> toString (plannedArm other) <> " arm")

    it "builds both observing targets and drives their real grouped preview without a deleting builder" $ do
        mirror <- previewFixture ["1.0.0", "3.0.0"]
        cache <- previewFixture ["2.0.0", "3.0.0"]
        builds <- newIORef []
        let env = overrideEnv "ECLUSE_RULES" "{\"revoke-preview\":{\"type\":\"DenyByIdentity\",\"identity\":\"left-pad\"}}" (withObservablePrivate codeArtifactEnvVars)
            onlyReads =
                observingOnly
                    { sbObserving = \_ _ backend -> do
                        let url = registryUrlText (cbUrl backend)
                        modifyIORef' builds (url :)
                        pure (fakeObservation (if url == "https://private.example.test" then cache else mirror))
                    }
        executable <- expectExecutableWith env BootStorePreview (\_ _ _ -> Nothing) refusingQueue onlyReads
        beforeMirror <- readFakeContents mirror
        beforeCache <- readFakeContents cache
        recorded <- recordingPortsUnder previewingReport Nothing
        case epRoleWiring executable of
            StorePrunerWiring wiring -> do
                outcome <- sweepCycle testPacing (recPorts recorded) (pwMounts wiring)
                tallyDeleted (outcomeTally outcome) `shouldBe` 3
                length (outcomePrerequisites outcome) `shouldBe` 2
            _ -> expectationFailure "expected the preview role"
        built <- readIORef builds
        length built `shouldBe` 2
        length (ordNub built) `shouldBe` 2
        readFakeContents mirror `shouldReturn` beforeMirror
        readFakeContents cache `shouldReturn` beforeCache

    it "reports a store maintenance client the live environment cannot build" $ do
        -- The client discovers an AWS identity when it is built, so an environment with none
        -- refuses here rather than failing the Dredger's first call against the store.
        outcome <- planWith codeArtifactEnvVars BootStorePruner (\_ _ _ -> Nothing) refusingQueue refusingStore
        case outcome of
            Right _ -> expectationFailure "expected the planning phase to refuse"
            Left [StoreMaintenanceUnavailable Npm (ClientBuildFailed detail), StoreMaintenanceUnavailable Npm (PrivateCacheUnavailable _)] ->
                detail `shouldSatisfy` T.isInfixOf "NoCredentials"
            Left errs -> expectationFailure ("expected the handle refusal, got: " <> show errs)

    for_ [False, True] $ \mirrorFails ->
        it ("classifies private observation construction failure and accumulates mirror failure: " <> show mirrorFails) $ do
            let builds =
                    observingOnly
                        { sbObserving = \ports limits backend ->
                            if mirrorFails || registryUrlText (cbUrl backend) == "https://private.example.test"
                                then sbObserving refusingStore ports limits backend
                                else sbObserving observingOnly ports limits backend
                        }
            outcome <- planWith (withObservablePrivate codeArtifactEnvVars) BootStorePreview (\_ _ _ -> Nothing) refusingQueue builds
            case (mirrorFails, outcome) of
                {- The mirror store was built and its cache was not, so the boot names the cache
                it could not build and the mirror target it will not sweep without one. -}
                (False, Left [StoreMaintenanceUnavailable Npm (PrivateCacheUnavailable unpaired), StoreMaintenanceUnavailable Npm (PrivateCacheUnavailable detail)]) -> do
                    unpaired `shouldBe` "no private cache was cleared to sweep beside this mirror target"
                    detail `shouldSatisfy` T.isPrefixOf "client build failed: NoCredentials"
                (True, Left [StoreMaintenanceUnavailable Npm (ClientBuildFailed mirrorDetail), StoreMaintenanceUnavailable Npm (PrivateCacheUnavailable privateDetail)]) -> do
                    mirrorDetail `shouldSatisfy` T.isPrefixOf "NoCredentials"
                    privateDetail `shouldSatisfy` T.isPrefixOf "client build failed: NoCredentials"
                (_, Right _) -> expectationFailure "expected observation construction to refuse"
                (_, Left errors) -> expectationFailure ("expected the exact target refusals in order, got: " <> show errors)

    it "reports a mirror-write mint the live environment refuses" $ do
        -- The Dredger reads and deletes through the mirror write's own credential, so it mints
        -- at boot exactly as the proxy does, and an identity that cannot answer refuses here.
        outcome <- planUnder codeArtifactEnvVars BootStorePruner (\_ _ _ -> Nothing) refusingQueue refusingCredentials inertStore
        case outcome of
            Right _ -> expectationFailure "expected the planning phase to refuse"
            Left errs -> errs `shouldBe` [CodeArtifactMintFailed ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET" :| []) "no identity answered"]

    it "reports a refused mint and a store client it cannot build together" $ do
        -- All three refusable steps accumulate, so one launch names every problem.
        outcome <- planUnder codeArtifactEnvVars BootStorePruner (\_ _ _ -> Nothing) refusingQueue refusingCredentials refusingStore
        case outcome of
            Right _ -> expectationFailure "expected the planning phase to refuse"
            Left [CodeArtifactMintFailed _ _, StoreMaintenanceUnavailable Npm (ClientBuildFailed _), StoreMaintenanceUnavailable Npm (PrivateCacheUnavailable _)] -> pass
            Left errs -> expectationFailure ("expected the mint and the handle refusal, got: " <> show errs)

    it "plans the pilot through the same phase, on its own arm" $ do
        -- Nothing here needs a live environment, so ports that refuse outright leave the role
        -- clearing as working ones do. The gate ahead of it is where the Pilot's refusal is spent.
        pilot <- expectExecutable BootWithoutPipeline (\_ _ _ -> Nothing) refusingQueue refusingStore
        plannedArm (epRoleWiring pilot) `shouldBe` "pilot"
        bpRole (epBootPlan pilot) `shouldBe` BootWithoutPipeline
        -- No advisory store is configured here, so the export loop idles.
        expectPilotPlan pilot >>= (`shouldBe` ExportIdle)

    it "carries the vetted mounts the pilot compiles an artifact for" $ do
        -- The same list the advisory sync reads, so the Pilot publishes to the key each
        -- ecosystem's sync polls rather than to npm's alone.
        pilot <- expectExecutableWith advisoryStoreEnv BootWithoutPipeline mountBindingFor inertQueue inertStore
        plan <- expectPilotPlan pilot
        case plan of
            ExportTo _ ecosystems -> ecosystems `shouldBe` Npm :| []
            ExportIdle -> expectationFailure "expected a configured store to turn the export loop on"

    it "refuses a pilot with an advisory store and no mount to compile for" $ do
        -- A role with no coherent runtime behaviour gets no runtime: the store is configured,
        -- so every cycle would publish nothing at all.
        outcome <- planWith unmountedAdvisoryEnv BootWithoutPipeline mountBindingFor inertQueue inertStore
        case outcome of
            Right _ -> expectationFailure "expected the pilot arm to refuse"
            Left errs -> errs `shouldBe` [PilotWithoutEcosystem]

    for_ [ServeAndMirror, ServeOnly] $ \role -> do
        it ("refuses " <> show role <> ", whose private upstream admits public content") $ do
            -- A repository that aggregates a public registry serves public packages as trusted
            -- private content, which is the one topology the request path cannot tell apart.
            (advisories, outcome) <- probedPlan role (Unsafe publicConnection)
            advisories `shouldBe` []
            case outcome of
                Right _ -> expectationFailure "expected the private upstream to refuse the role"
                Left errs -> errs `shouldBe` [PrivateUpstreamUnsafe Npm publicConnection]

        it ("advises " <> show role <> ", and boots it, where the backend settled nothing") $ do
            (advisories, outcome) <- probedPlan role (Undecidable NoMechanism)
            advisories `shouldBe` [PrivateUpstreamUndecided Npm NoMechanism]
            outcome `shouldSatisfy` isRight

        it ("says nothing to " <> show role <> " about a private upstream that aggregates nothing") $ do
            (advisories, outcome) <- probedPlan role Safe
            advisories `shouldBe` []
            outcome `shouldSatisfy` isRight

    it "reads no private upstream on the mirror worker or the pilot, which serve no client from one" $
        for_ [BootMirrorPipeline MirrorOnly, BootWithoutPipeline] $ \role -> do
            (advisories, outcome) <- reportWith staticEnvVars role mountBindingFor inertQueue (neverProbing inertStore)
            advisories `shouldBe` []
            outcome `shouldSatisfy` isRight

    it "refuses the preview through the private handle it already holds" $ do
        -- The preview builds an observation for the private cache, so it asks that handle rather
        -- than building a second client for the same repository.
        (_, outcome) <-
            reportWith (withObservablePrivate codeArtifactEnvVars) BootStorePreview (\_ _ _ -> Nothing) refusingQueue (privateAnswering (Unsafe publicConnection))
        case outcome of
            Right _ -> expectationFailure "expected the private upstream to refuse the preview"
            Left errs -> errs `shouldBe` [PrivateUpstreamUnsafe Npm publicConnection]

-- | Plan a mirror-pipeline role over a probe that answers the same way for every mount.
probedPlan :: MirrorRole -> UpstreamSafety -> IO ([Advisory], Either [BootError] ExecutablePlan)
probedPlan role answer =
    reportWith staticEnvVars (BootMirrorPipeline role) mountBindingFor inertQueue (probing answer inertStore)

-- | Which arm of the phase a plan came back through, so an assertion names it rather than a shape.
plannedArm :: RoleWiring -> Text
plannedArm = \case
    MirrorPipelineWiring _ -> "mirror pipeline"
    StorePrunerWiring _ -> "store pruner"
    PilotWiring _ -> "pilot"

-- | A queue builder that allocates nothing, for the arms whose refusals are elsewhere.
inertQueue :: BuildMirrorQueue
inertQueue _ _ _ = pure noMirrorQueue

{- | A queue builder that throws as @amazonka@ does when it discovers no credentials, the live
call this phase folds into a refusal.
-}
refusingQueue :: BuildMirrorQueue
refusingQueue _ _ _ = throwIO NoCredentials

-- | Store builds that hand out the in-memory fake, so the pruner's arms reach no cloud.
inertStore :: StoreBuilds
inertStore =
    StoreBuilds
        { sbDeleting = \_ _ _ -> fakeMaintenance <$> newFakeStore defaultFakeStoreConfig
        , sbObserving = \_ _ _ -> fakeObservation <$> newFakeStore defaultFakeStoreConfig
        , sbProbing = \_ _ -> noUpstreamMechanism
        }

-- | Store builds that throw as @amazonka@ does when it discovers no credentials.
refusingStore :: StoreBuilds
refusingStore =
    StoreBuilds
        { sbDeleting = \_ _ _ -> throwIO NoCredentials
        , sbObserving = \_ _ _ -> throwIO NoCredentials
        , sbProbing = \_ _ -> noUpstreamMechanism
        }

-- | Store builds whose deleting arm fails the case, so only a preview's own build can answer.
observingOnly :: StoreBuilds
observingOnly =
    inertStore{sbDeleting = \_ _ _ -> fail "a preview must not build the deleting handle"}

-- | Store builds whose probe answers the same way for every mount that has a private upstream.
probing :: UpstreamSafety -> StoreBuilds -> StoreBuilds
probing answer builds = builds{sbProbing = \_ _ -> pure answer}

-- | Store builds whose probe fails the case, for the roles that must never read a private upstream.
neverProbing :: StoreBuilds -> StoreBuilds
neverProbing builds = builds{sbProbing = \_ _ -> fail "this role must not read the private upstream"}

-- | Observing builds whose private cache answers the given verdict about what it aggregates.
privateAnswering :: UpstreamSafety -> StoreBuilds
privateAnswering answer =
    observingOnly
        { sbObserving = \_ _ backend ->
            fakeObservation
                <$> newFakeStore
                    defaultFakeStoreConfig
                        { fakeUpstream = if registryUrlText (cbUrl backend) == privateUpstreamUrl then answer else Safe
                        }
        }

-- | The private upstream the composition fixtures declare.
privateUpstreamUrl :: Text
privateUpstreamUrl = "https://private.example.test"

-- | The evidence a backend reports when a repository in the chain connects to a public registry.
publicConnection :: UnsafeReason
publicConnection = ConfigurationEvidence (RepositoryName "shared") (ExternalConnection "public:npmjs")

-- | A credential build that mints nothing, so a case reaches no cloud.
inertCredentials :: BuildCredentials
inertCredentials _ _ = pure (Right noCredentialProviders)

-- | A credential build that refuses, as a mint against an identity that cannot answer does.
refusingCredentials :: BuildCredentials
refusingCredentials _ _ = pure (Left [CodeArtifactMintFailed ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET" :| []) "no identity answered"])

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

-- | The shipped mount over a configured advisory store, so the pilot's arm has work to plan.
advisoryStoreEnv :: [(String, String)]
advisoryStoreEnv = overrideEnv "ECLUSE_ADVISORIES__URL" advisoryStoreUrl staticEnvVars

{- | An advisory store with no mount declared under it. The proxy would serve nothing and the
Pilot would compile nothing, which is the pilot arm's own refusal.
-}
unmountedAdvisoryEnv :: [(String, String)]
unmountedAdvisoryEnv = [("ECLUSE_ADVISORIES__URL", advisoryStoreUrl)]

advisoryStoreUrl :: String
advisoryStoreUrl = "s3://advisories.example.test/ecluse"

-- | Plan a boot over 'staticEnvVars' for one role, through the given ports.
planFor :: BootRole -> ResolveAdapter -> BuildMirrorQueue -> StoreBuilds -> IO (Either [BootError] ExecutablePlan)
planFor = planWith staticEnvVars

-- | 'planFor' over a named environment layer, for a refusal 'staticEnvVars' cannot reach.
planWith :: [(String, String)] -> BootRole -> ResolveAdapter -> BuildMirrorQueue -> StoreBuilds -> IO (Either [BootError] ExecutablePlan)
planWith envVars role resolveAdapter buildQueue = planUnder envVars role resolveAdapter buildQueue (defaultCredentialsFor role)

-- | 'planWith', keeping the advisories the phase logged beside the outcome it settled.
reportWith :: [(String, String)] -> BootRole -> ResolveAdapter -> BuildMirrorQueue -> StoreBuilds -> IO ([Advisory], Either [BootError] ExecutablePlan)
reportWith envVars role resolveAdapter buildQueue = reportUnder envVars role resolveAdapter buildQueue (defaultCredentialsFor role)

{- | The credential build a case takes by default: the production one for the mirror pipeline,
whose static fixture mints without a cloud, and an inert one for the store roles' CodeArtifact one.
-}
defaultCredentialsFor :: BootRole -> BuildCredentials
defaultCredentialsFor = \case
    BootMirrorPipeline _ -> initTargetCredentialProviders
    _ -> inertCredentials

-- | 'planWith' over a chosen credential build, for the deleting role's own mint.
planUnder ::
    [(String, String)] ->
    BootRole ->
    ResolveAdapter ->
    BuildMirrorQueue ->
    BuildCredentials ->
    StoreBuilds ->
    IO (Either [BootError] ExecutablePlan)
planUnder envVars role resolveAdapter buildQueue buildCredentials buildStore =
    snd <$> reportUnder envVars role resolveAdapter buildQueue buildCredentials buildStore

-- | 'planUnder', keeping the advisories the phase logged beside the outcome it settled.
reportUnder ::
    [(String, String)] ->
    BootRole ->
    ResolveAdapter ->
    BuildMirrorQueue ->
    BuildCredentials ->
    StoreBuilds ->
    IO ([Advisory], Either [BootError] ExecutablePlan)
reportUnder envVars role resolveAdapter buildQueue buildCredentials buildStore = do
    config <- expectConfig envVars Nothing
    bootPlan <- expectPlanFor role envVars Nothing config noCeiling
    logEnv <- newTestLogEnv
    planExecutable logEnv passthroughTracingPort resolveAdapter buildQueue buildCredentials buildStore bootPlan

-- | 'planFor', failing the test on a refusal.
expectExecutable :: BootRole -> ResolveAdapter -> BuildMirrorQueue -> StoreBuilds -> IO ExecutablePlan
expectExecutable = expectExecutableWith staticEnvVars

-- | 'planWith', failing the test on a refusal.
expectExecutableWith :: [(String, String)] -> BootRole -> ResolveAdapter -> BuildMirrorQueue -> StoreBuilds -> IO ExecutablePlan
expectExecutableWith envVars role resolveAdapter buildQueue buildStore =
    planWith envVars role resolveAdapter buildQueue buildStore
        >>= either (\errs -> fail ("planning refused: " <> show errs)) pure

-- | The pilot arm's export loop, failing the test on any other arm.
expectPilotPlan :: ExecutablePlan -> IO ExportLoopPlan
expectPilotPlan plan = case epRoleWiring plan of
    PilotWiring exportPlan -> pure exportPlan
    other -> fail ("expected the pilot arm, got the " <> toString (plannedArm other) <> " arm")

-- | The mirror-pipeline arm of a plan, failing the test on any other arm.
expectMirrorWiring :: ExecutablePlan -> IO MirrorWiring
expectMirrorWiring plan = case epRoleWiring plan of
    MirrorPipelineWiring mirror -> pure mirror
    other -> fail ("expected the mirror-pipeline arm, got the " <> toString (plannedArm other) <> " arm")

previewFixture :: [Text] -> IO FakeStore
previewFixture rawVersions =
    newFakeStore
        defaultFakeStoreConfig
            { fakeContents = Map.singleton name [StoredVersion version VersionServed Nothing | version <- versions]
            , fakeManifests = Map.singleton name (sampleManifest name versions)
            }
  where
    name = unscopedNpm "left-pad"
    versions = map (mkVersion Npm) rawVersions
