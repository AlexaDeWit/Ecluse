-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Grouped deletion over backend-owned batches. Every attempt rechecks current policy and
local inventory, and associated targets share one logical charge before their first attempt.
-}
module Ecluse.Core.Registry.Sweep.Deletion (deleteGroup, Selection (..)) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T

import Ecluse.Core.Cve (DbEtag)
import Ecluse.Core.Fault (RetryAfter (RetryAfter))
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Maintenance
import Ecluse.Core.Registry.Sweep.Group (boundedVersions)
import Ecluse.Core.Registry.Sweep.Types
import Ecluse.Core.Telemetry.Metrics (SweepResult (SweepGuardSkipped), SweepTarget (SweepMirror))
import Ecluse.Core.Version (Version, renderVersion)

-- | A current named denial carries the generation credited when the logical cap fills.
data Selection = Selection
    { selVersion :: Version
    , selMessage :: Text
    , selGeneration :: Maybe DbEtag
    }

type SelectStored = Bool -> SweepMount -> [StoredVersion] -> IO [Selection]

data DeletionRun = DeletionRun
    { runPacing :: SweepPacing
    , runPorts :: SweepPorts
    , runCounters :: SweepState
    , runMount :: SweepMount
    , runName :: PackageName
    , runSelect :: SelectStored
    , runStores :: [SweepStore]
    , runCharged :: IORef (Set Text)
    , runCapGeneration :: IORef (Maybe DbEtag)
    , runHalt :: IORef (Maybe CycleHalt)
    }

-- | Reassess each target independently and finish charged cache work before returning a cap halt.
deleteGroup :: SweepPacing -> SweepPorts -> SweepState -> SweepMount -> PackageName -> SelectStored -> (SweepPorts -> (Version, VersionOutcome) -> IO ()) -> [(SweepStore, [StoredVersion])] -> IO (Maybe CycleHalt)
deleteGroup pacing ports counters mount name select report locations = do
    run <- DeletionRun pacing ports counters mount name select (map fst locations) <$> newIORef Set.empty <*> newIORef Nothing <*> newIORef Nothing
    traverse_ (deleteLocation run report) locations
    issued <- readIORef (stIssued counters)
    halt <- readIORef (runHalt run)
    generation <- readIORef (runCapGeneration run)
    pure (if issued >= swpDeletionCap pacing then Just (HaltDeletionCap (swpDeletionCap pacing) issued generation) else halt)

deleteLocation :: DeletionRun -> (SweepPorts -> (Version, VersionOutcome) -> IO ()) -> (SweepStore, [StoredVersion]) -> IO ()
deleteLocation run report (store, initial) = case ssExecute store of
    SweepCounts -> pure ()
    SweepRemoves deletion -> do
        selected <- runSelect run True (atStore run store) initial
        let checks = DeleteGuard (checkBatch run store) (retryDelete run store)
        offered <- withinAllowance run store (map selVersion selected)
        outcomes <- dlDeleteVersions deletion checks (runName run) offered
        traverse_ (report (labelled run store)) outcomes
        confirm run store (mapMaybe confirmationVersion outcomes)

confirmationVersion :: (Version, VersionOutcome) -> Maybe Version
confirmationVersion (version, outcome) = case outcome of
    VersionRemoved -> Just version
    VersionUncertain _ -> Just version
    VersionRemoving _ -> Nothing
    VersionRefused _ -> Nothing
    VersionUnreached _ -> Nothing

withinAllowance :: DeletionRun -> SweepStore -> [Version] -> IO [Version]
withinAllowance run store selected = do
    charged <- readIORef (runCharged run)
    issued <- readIORef (stIssued (runCounters run))
    halt <- readIORef (runHalt run)
    let existing version = Set.member (renderVersion version) charged
        allowance = if isJust halt then 0 else max 0 (swpDeletionCap (runPacing run) - issued)
        fresh = take allowance (filter (not . existing) selected)
        offered = filter (\version -> existing version || version `elem` fresh) selected
    traverse_ (const (record (labelled run store) (runCounters run) SweepGuardSkipped)) (filter (`notElem` offered) selected)
    pure offered

checkBatch :: DeletionRun -> SweepStore -> DeletePhase -> [Version] -> IO (Either StoreFault [Version])
checkBatch run store phase proposed =
    currentGroup run >>= \case
        Left (target, fault) -> refuse run target fault
        Right inventories ->
            permission store >>= \case
                Left fault -> refuse run (backend store) fault
                Right () -> assessBatch run store phase proposed inventories

assessBatch :: DeletionRun -> SweepStore -> DeletePhase -> [Version] -> Map Text [StoredVersion] -> IO (Either StoreFault [Version])
assessBatch run store phase proposed inventories = do
    let before = inventory store inventories
        source = smStore (runMount run)
    decisions <- runSelect run False (atStore run store) before
    sourceDenied <- if isSource run store then pure [] else map selVersion <$> runSelect run False (atStore run source) (inventory source inventories)
    currentGroup run >>= \case
        Left (target, fault) -> refuse run target fault
        Right checked ->
            permission store >>= \case
                Left fault -> refuse run (backend store) fault
                Right () -> do
                    let allowed = [version | version <- proposed, version `elem` map selVersion decisions, version `notElem` sourceDenied, unchanged version before (inventory store checked), isSource run store || entry version (inventory source inventories) == entry version (inventory source checked)]
                    admitted <- if phase == BeforeDelete then charge run store (filter ((`elem` allowed) . selVersion) decisions) else pure allowed
                    when (phase == BeforeDelete) $
                        traverse_ (auditInfo (sweepAudit (labelled run store)) . selMessage) (filter ((`elem` admitted) . selVersion) decisions)
                    pure (Right admitted)

currentGroup :: DeletionRun -> IO (Either (Text, StoreFault) (Map Text [StoredVersion]))
currentGroup run = do
    inventories <- traverse readOne (runStores run)
    pure $ do
        readable <- sequence inventories
        bounded <- first (combined,) (boundedVersions (ssVersionLimit (smStore (runMount run))) readable)
        pure (Map.fromList [(backend store, versions) | (store, versions) <- bounded])
  where
    readOne store = first (backend store,) . fmap (store,) <$> obEnumerateVersions (ssObserve store) (runName run)
    combined = T.intercalate " and " (map backend (runStores run)) <> " (combined inventory)"

permission :: SweepStore -> IO (Either StoreFault ())
permission store = do
    consent <- obVerifyConsent (ssObserve store)
    classified <- obClassifyStore (ssObserve store)
    pure $ do
        marker <- consent
        case marker of
            ConsentWithheld descriptor -> Left (protocolFault descriptor)
            ConsentGranted -> pure ()
        kind <- classified
        case kind of
            StorePreserved why -> Left (protocolFault why)
            StoreDestroyable -> pure ()

charge :: DeletionRun -> SweepStore -> [Selection] -> IO [Version]
charge run store selections = do
    let selected = map selVersion selections
    halted <- readIORef (runHalt run)
    held <- readIORef (runCharged run)
    issued <- readIORef (stIssued (runCounters run))
    let fresh = filter ((`Set.notMember` held) . renderVersion) selected
        admitted = take (if isJust halted then 0 else max 0 (swpDeletionCap (runPacing run) - issued)) fresh
        permitted = filter (\version -> Set.member (renderVersion version) held || version `elem` admitted) selected
    modifyIORef' (runCharged run) (<> Set.fromList (map renderVersion admitted))
    modifyIORef' (stIssued (runCounters run)) (+ length admitted)
    when (issued < swpDeletionCap (runPacing run) && issued + length admitted >= swpDeletionCap (runPacing run)) $
        writeIORef (runCapGeneration run) (listToMaybe (reverse admitted) >>= \version -> selGeneration =<< find ((== version) . selVersion) selections)
    traverse_ (const (record (labelled run store) (runCounters run) SweepGuardSkipped)) (filter (`notElem` permitted) selected)
    pure permitted

refuse :: DeletionRun -> Text -> StoreFault -> IO (Either StoreFault a)
refuse run target fault = do
    writeIORef (runHalt run) (Just (HaltStoreFault (smEcosystem (runMount run)) target (renderStoreFault fault)))
    pure (Left fault)

retryDelete :: DeletionRun -> SweepStore -> StoreFault -> IO Bool
retryDelete run store fault = case faultRetry fault of
    RetryFutile -> pure False
    RetryWorthwhile -> pause (swpChunkPause (runPacing run))
    RetryDelayed (RetryAfter seconds) -> pause (fromIntegral seconds)
  where
    pause delay = do
        auditWarn (sweepAudit (labelled run store)) ("reassessing an uncertain deletion after " <> renderStoreFault fault)
        sweepDelay (runPorts run) delay
        pure True

confirm :: DeletionRun -> SweepStore -> [Version] -> IO ()
confirm run store versions =
    unless (null versions) $
        currentGroup run >>= \case
            Left (target, fault) -> void (refuse run target fault)
            Right inventories -> do
                let actual = inventory store inventories
                denied <- map selVersion <$> runSelect run False (atStore run store) actual
                let residual = [storedVersion item | item <- actual, storedPresence item == VersionServed, storedVersion item `elem` versions, storedVersion item `elem` denied]
                unless (null residual) $ do
                    let detail = "cleanup remains incomplete for versions " <> show (map renderVersion residual)
                    auditError (sweepAudit (labelled run store)) detail
                    void (refuse run (backend store) (protocolFault detail))

atStore :: DeletionRun -> SweepStore -> SweepMount
atStore run store = (runMount run){smStore = store}

backend :: SweepStore -> Text
backend = factBackend . obFacts . ssObserve

isSource :: DeletionRun -> SweepStore -> Bool
isSource run store = sweepTargetOf (runMount run) (ssObserve store) == SweepMirror

inventory :: SweepStore -> Map Text [StoredVersion] -> [StoredVersion]
inventory store = Map.findWithDefault [] (backend store)

entry :: Version -> [StoredVersion] -> Maybe StoredVersion
entry version = find ((== version) . storedVersion)

unchanged :: Version -> [StoredVersion] -> [StoredVersion] -> Bool
unchanged version before after = maybe False (`elem` after) (entry version before)

labelled :: DeletionRun -> SweepStore -> SweepPorts
labelled run store = locatedPorts (runMount run) (ssObserve store) (runPorts run)
