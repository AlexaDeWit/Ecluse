-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Dredger role composition, companion tasks, halt latching, and preview behaviour.
module Ecluse.DredgerSpec (spec) where

import Control.Exception qualified as Exception
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Katip (closeScribes)
import OpenTelemetry.MeterProvider (SdkMeterEnv)
import Test.Hspec
import UnliftIO (bracket, throwIO)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Boot (BootEnv (..))
import Ecluse.Composition.Credential (noCredentialProviders)
import Ecluse.Composition.Executable (
    ExecutablePlan (epRoleWiring),
    PrunerWiring (pwCveSync, pwMounts),
    RoleWiring (StorePrunerWiring),
    planExecutable,
 )
import Ecluse.Composition.Maintenance (StoreBuilds (StoreBuilds, sbDeleting, sbObserving))
import Ecluse.Composition.Support (codeArtifactEnvVars, expectConfig, expectPlanFor, noCeiling, withObservablePrivate)
import Ecluse.Composition.TelemetrySupport (advisoryAgePoints, newAdvisoryHandles, withRoleTelemetry)
import Ecluse.Composition.Types (BootRole (BootStorePreview, BootStorePruner))
import Ecluse.Config (AppConfig (cfgServer), Config (configApp), ServerSettings (srvPort))
import Ecluse.Core.Cve (DbEtag (DbEtag))
import Ecluse.Core.Cve.Slot (swapIn)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Package (PackageName, mkPackageName, renderPackageName)
import Ecluse.Core.Queue (noMirrorQueue)
import Ecluse.Core.Registry.Maintenance (
    ConsentVerdict (ConsentWithheld),
    StoredVersion (StoredVersion),
    VersionPresence (VersionServed),
 )
import Ecluse.Core.Registry.Sweep (sweepCycle)
import Ecluse.Core.Registry.Sweep.Types (
    CycleHalt,
    CycleOutcome (outcomeHalt, outcomePrerequisites, outcomeTally),
    PrerequisiteStatus (PrerequisiteUnmet),
    SweepMount (smStore),
    SweepPacing (swpDeletionCap),
    SweepReport (reportCapHalts, reportRemoval),
    SweepTally (tallyDeleted),
    TargetPrerequisites (tpConsent),
    walkMarkerOf,
 )
import Ecluse.Core.Rules.Types (Rule (DenyByIdentity))
import Ecluse.Core.Server.Readiness (
    MountReadiness (MountAwaitingFirstSync, MountReady),
    Readiness (Latched),
    mountReadiness,
    routable,
 )
import Ecluse.Core.Telemetry.Metrics (Label (LEcosystem), SweepResult (SweepDeleted, SweepExamined, SweepWouldDelete), metricAttributes)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Cve.Sync (CveSyncHandle (..))
import Ecluse.Dredger (dredgerReady, latchedStep, runDredger, withSyncTasks)
import Ecluse.Dredger.Plan (DredgerOptions (DredgerOptions), SweepMode (SweepDeletes, SweepPreviews), SweepRepetition (SweepOnce), sweepReportFor)
import Ecluse.Runtime.Cve.Sync (SyncEnv (syncSlot))
import Ecluse.Test.Cve (fakeCveDb)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Maintenance (
    FakeStore (fakeMaintenance, fakeObservation, readFakeContents),
    FakeStoreConfig (..),
    defaultFakeStoreConfig,
    newFakeStore,
 )
import Ecluse.Test.Package (sampleManifest)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Rules (denyRule)
import Ecluse.Test.Sweep (RecordedSweep (..), recordingPorts, testMount, testPacing)

spec :: Spec
spec = do
    companionSpec
    latchSpec
    probeSpec
    previewSpec
    advisoryAgeSpec

-- An empty sync plan must not cancel the sweep before it does any work.
companionSpec :: Spec
companionSpec = describe "withSyncTasks" $ do
    it "lets the sweep finish when there is no sync task to run at all" $ do
        swept <- newIORef False
        withSyncTasks [] (threadDelay 1000 >> writeIORef swept True)
        readIORef swept `shouldReturn` True

    it "lets the sweep finish when a sync task ends before it does" $ do
        swept <- newIORef False
        withSyncTasks [pass] (threadDelay 1000 >> writeIORef swept True)
        readIORef swept `shouldReturn` True

    it "brings the run down when a sync task faults, rather than sweeping on without it" $ do
        completed <- newIORef False
        outcome <-
            Exception.try $
                withSyncTasks [throwIO (SyncGaveUp "the advisory sync gave up")] (threadDelay 200000 >> writeIORef completed True)
        outcome `shouldSatisfy` faulted
        readIORef completed `shouldReturn` False

{- The cap is a breaker, so it stops the Dredger for the life of the process. Nothing clears it,
and the process stays up, because exiting would restart into the same poisoned generation. -}
latchSpec :: Spec
latchSpec = describe "a latched halt" $ do
    it "runs no further cycle, so the store it did not reach is untouched" $ do
        -- The first cycle fills a cap of one and latches. Two more steps then run nothing, so the
        -- second package is still served and was never even examined.
        (store, rec', _) <- stepped 3
        remaining <- held store
        length remaining `shouldBe` 1
        counted <- recResults rec'
        counted `shouldBe` [SweepExamined, SweepDeleted]

    it "repeats its own line at each cycle interval, so nothing halts in silence" $ do
        (_, rec', _) <- stepped 3
        errors <- recErrors rec'
        length (filter (T.isInfixOf "the mirror sweep is halted and runs no cycle") errors) `shouldBe` 2

    it "waits the cycle pause before repeating, rather than spinning on the halt" $ do
        (_, rec', _) <- stepped 3
        recDelays rec' `shouldReturn` 3

{- A latch closes readiness for good and leaves liveness alone: an orchestrator that restarted the
pod would start sweeping the same generation that filled the cap. -}
probeSpec :: Spec
probeSpec = describe "the health surface under a latch" $ do
    it "answers ready while the advisory sync has landed and nothing has latched" $
        routable <$> dredgerReady (pure synced) (pure Nothing) `shouldReturn` True

    it "reports the latch itself once a halt latched, whatever the sync says" $ do
        (_, _, latched) <- stepped 1
        halt <- readIORef latched
        halt `shouldSatisfy` isJust
        dredgerReady (pure synced) (readIORef latched) `shouldReturn` Latched

    it "answers unready before the advisory sync has landed, latch or no latch" $
        routable <$> dredgerReady (pure awaiting) (pure Nothing) `shouldReturn` False
  where
    synced = mountReadiness (Map.singleton Npm MountReady)
    awaiting = mountReadiness (Map.singleton Npm MountAwaitingFirstSync)

{- The composition root gives the preview role mounts whose execution counts, so a dry run is not
a branch the loop takes but a capability it was never given. -}
previewSpec :: Spec
previewSpec = describe "the preview role's wiring" $ do
    it "counts a removal as would-delete, and lets the cap only log" $ do
        let report = sweepReportFor SweepPreviews
        reportRemoval report `shouldBe` SweepWouldDelete
        reportCapHalts report `shouldBe` False

    it "sweeps a store with no consent, reports it, and deletes nothing" $ do
        store <- newFakeStore seededConfig{fakeConsent = ConsentWithheld "attach it"}
        pruner <- plannedPruner BootStorePreview (observingOver store)
        seeded <- readFakeContents store
        rec' <- recordingPorts generation
        outcome <- sweepCycle testPacing (recPorts rec') (pwMounts pruner)
        outcomeHalt outcome `shouldBe` Nothing
        map tpConsent (outcomePrerequisites outcome) `shouldBe` replicate 2 (PrerequisiteUnmet "attach it")
        tallyDeleted (outcomeTally outcome) `shouldBe` 0
        readFakeContents store `shouldReturn` seeded

    it "holds no walk marker on any mount it was given" $ do
        store <- newFakeStore seededConfig
        pruner <- plannedPruner BootStorePreview (observingOver store)
        map (isNothing . walkMarkerOf . smStore) (pwMounts pruner) `shouldBe` [True]

{- The pruner wiring one boot role settles, over a store this spec drives. Only the build the role
names runs, so the other one reports being reached. -}
plannedPruner :: BootRole -> StoreBuilds -> IO PrunerWiring
plannedPruner role builds =
    bracket newTestLogEnv (void . closeScribes) $ \logEnv -> do
        config <- expectConfig (withObservablePrivate codeArtifactEnvVars) Nothing
        bootPlan <- expectPlanFor role (withObservablePrivate codeArtifactEnvVars) Nothing config noCeiling
        planned <-
            planExecutable
                logEnv
                passthroughTracingPort
                (\_ _ _ -> Nothing)
                (\_ _ _ -> pure noMirrorQueue)
                (\_ _ -> pure (Right noCredentialProviders))
                builds
                bootPlan
        case epRoleWiring <$> planned of
            Right (StorePrunerWiring pruner) -> pure pruner
            _ -> fail "expected the Dredger role plan"

-- The observing build alone answers, so a preview's boot running the other one fails the case.
observingOver :: FakeStore -> StoreBuilds
observingOver store =
    StoreBuilds
        { sbDeleting = \_ _ _ -> fail "a preview must not build the deleting handle"
        , sbObserving = \_ _ _ -> pure (fakeObservation store)
        }

advisoryAgeSpec :: Spec
advisoryAgeSpec = describe "runDredger advisory database ages" $
    it "emits each configured ecosystem and observes generation swaps through its registered callbacks" $
        withDredgerAges $ \meterEnv handles -> do
            let install handle etag = swapIn (syncSlot (csEnv handle)) (DbEtag etag) Nothing (fakeCveDb [])
            for_ handles $ \(_, handle) -> install handle "first-generation"
            threadDelay 1_100_000
            initialPoints <- advisoryAgePoints meterEnv
            map fst initialPoints `shouldMatchList` map (metricAttributes . pure . LEcosystem) [Npm, PyPI]
            map snd initialPoints `shouldSatisfy` all (>= 1)
            for_ handles $ \(eco, handle) ->
                when (eco == Npm) (install handle "next-generation")
            swappedPoints <- advisoryAgePoints meterEnv
            map fst swappedPoints `shouldMatchList` map fst initialPoints
            let ages eco points = [age | (attrs, age) <- points, attrs == metricAttributes [LEcosystem eco]]
            case (ages Npm swappedPoints, ages PyPI swappedPoints, ages PyPI initialPoints) of
                ([npmAge], [pypiAge], [oldPypiAge]) -> do
                    npmAge `shouldSatisfy` (< pypiAge)
                    pypiAge `shouldSatisfy` (>= oldPypiAge)
                _ -> expectationFailure "expected one age per configured ecosystem"

withDredgerAges :: (SdkMeterEnv -> [(Ecosystem, CveSyncHandle)] -> IO ()) -> IO ()
withDredgerAges use = withRoleTelemetry $ \logEnv telemetry meterEnv -> do
    config <- expectConfig (withObservablePrivate codeArtifactEnvVars) Nothing
    bootPlan <- expectPlanFor BootStorePruner codeArtifactEnvVars Nothing config noCeiling
    store <- newFakeStore defaultFakeStoreConfig
    planned <-
        planExecutable
            logEnv
            passthroughTracingPort
            (\_ _ _ -> Nothing)
            (\_ _ _ -> pure noMirrorQueue)
            (\_ _ -> pure (Right noCredentialProviders))
            StoreBuilds
                { sbDeleting = \_ _ _ -> pure (fakeMaintenance store)
                , sbObserving = \_ _ _ -> pure (fakeObservation store)
                }
            bootPlan
    case epRoleWiring <$> planned of
        Right (StorePrunerWiring pruner) -> do
            handles <- newAdvisoryHandles [Npm, PyPI]
            let app = configApp config
                ephemeral = config{configApp = app{cfgServer = (cfgServer app){srvPort = 0}}}
                boot = BootEnv ephemeral logEnv telemetry bootPlan
            runDredger boot (DredgerOptions SweepDeletes SweepOnce) pruner{pwCveSync = Map.fromList handles}
                `shouldReturn` Nothing
            use meterEnv handles
        _ -> expectationFailure "expected the Dredger role plan"

stepped :: Int -> IO (FakeStore, RecordedSweep, IORef (Maybe CycleHalt))
stepped steps = do
    store <- newFakeStore seededConfig
    rec' <- recordingPorts generation
    latched <- newIORef Nothing
    let mount = testMount (fakeMaintenance store) [denyRule] (map (DenyByIdentity . renderPackageName) seededNames)
    replicateM_ steps (latchedStep cappedPacing (recPorts rec') [mount] latched)
    pure (store, rec', latched)

seededConfig :: FakeStoreConfig
seededConfig =
    defaultFakeStoreConfig
        { fakeContents = Map.fromList [(name, [StoredVersion (version "1.0.0") VersionServed]) | name <- names]
        , fakeManifests = Map.fromList [(name, sampleManifest name [version "1.0.0"]) | name <- names]
        }
  where
    names = seededNames

seededNames :: [PackageName]
seededNames = [packageName "left-pad", packageName "lodash"]

-- A cap of one, so the first cycle fills it and latches with the second package untouched.
cappedPacing :: SweepPacing
cappedPacing = testPacing{swpDeletionCap = 1}

held :: FakeStore -> IO [Version]
held store = concatMap (map storedVersionOf) . Map.elems <$> readFakeContents store
  where
    storedVersionOf (StoredVersion v _) = v

generation :: Maybe DbEtag
generation = Just (DbEtag "etag-1")

packageName :: Text -> PackageName
packageName = mkPackageName Npm Nothing

version :: Text -> Version
version = mkVersion Npm

-- The linked companion rethrows asynchronously, so the assertion uses the base exception perimeter.
faulted :: Either SomeException () -> Bool
faulted = isLeft

-- A typed fault a spec throws from a sync task, so the case names what it simulated.
newtype SyncGaveUp = SyncGaveUp Text
    deriving stock (Show)

instance Exception SyncGaveUp
