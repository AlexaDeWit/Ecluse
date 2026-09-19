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

type ReportOutcome = SweepPorts -> (Version, VersionOutcome) -> IO ()

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
deleteGroup ::
    SweepPacing ->
    SweepPorts ->
    SweepState ->
    SweepMount ->
    PackageName ->
    SelectStored ->
    ReportOutcome ->
    [(SweepStore, [StoredVersion])] ->
    IO (Maybe CycleHalt)
deleteGroup pacing ports counters mount name select report locations = do
    run <- DeletionRun pacing ports counters mount name select (map fst locations) <$> newIORef Set.empty <*> newIORef Nothing <*> newIORef Nothing
    traverse_ (deleteLocation run report) locations
    issued <- readIORef (stIssued counters)
    halt <- readIORef (runHalt run)
    generation <- readIORef (runCapGeneration run)
    pure (if issued >= swpDeletionCap pacing then Just (HaltDeletionCap (swpDeletionCap pacing) issued generation) else halt)

deleteLocation :: DeletionRun -> ReportOutcome -> (SweepStore, [StoredVersion]) -> IO ()
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

checkBatch :: DeletionRun -> SweepStore -> DeletePhase -> [Version] -> IO (Either StoreFault [Version])
checkBatch run store phase proposed = withCurrentPermission run store (assessBatch run store phase proposed)

{- Read the group's inventory and the store's standing permissions, refusing on either fault. The
inventory the continuation receives is the one this read produced, never an earlier one. -}
withCurrentPermission ::
    DeletionRun ->
    SweepStore ->
    (Map Text [StoredVersion] -> IO (Either StoreFault a)) ->
    IO (Either StoreFault a)
withCurrentPermission run store continue =
    currentGroup run >>= \case
        Left (target, fault) -> refuse run target fault
        Right inventories ->
            permission store >>= \case
                Left fault -> refuse run (backend store) fault
                Right () -> continue inventories

{- The second read is deliberate: a decision and a permission taken before the batch was proposed
say nothing about the store now, and a delete is permanent. -}
assessBatch :: DeletionRun -> SweepStore -> DeletePhase -> [Version] -> Map Text [StoredVersion] -> IO (Either StoreFault [Version])
assessBatch run store phase proposed inventories = do
    decisions <- runSelect run False (atStore run store) before
    denied <-
        if atSource
            then pure Nothing
            else Just . map selVersion <$> runSelect run False (atStore run source) sourceBefore
    withCurrentPermission run store $ \checked -> do
        let sourceView = (\names -> SourceView names sourceBefore (inventory source checked)) <$> denied
            allowed = filter (admissible (map selVersion decisions) before (inventory store checked) sourceView) proposed
        admitReassessed run store phase decisions allowed
  where
    before = inventory store inventories
    source = smStore (runMount run)
    sourceBefore = inventory source inventories
    atSource = isSource run store

-- The source store's own view of the batch, held only where the store being assessed is a cache.
data SourceView = SourceView
    { svDenied :: [Version]
    , svBefore :: [StoredVersion]
    , svAfter :: [StoredVersion]
    }

-- A proposed version stands only while every store that decided it still reads as it did.
admissible :: [Version] -> [StoredVersion] -> [StoredVersion] -> Maybe SourceView -> Version -> Bool
admissible decided before after source version =
    version `elem` decided
        && unchanged version before after
        && all (admittedAtSource version) source

admittedAtSource :: Version -> SourceView -> Bool
admittedAtSource version view =
    version `notElem` svDenied view && entry version (svBefore view) == entry version (svAfter view)

-- Only the pre-delete phase charges the cap and announces, so a retry's recheck does neither twice.
admitReassessed :: DeletionRun -> SweepStore -> DeletePhase -> [Selection] -> [Version] -> IO (Either StoreFault [Version])
admitReassessed run store phase decisions allowed = do
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

-- What a remaining allowance covers, out of the versions one attempt selected.
data Allowance = Allowance
    { alwFresh :: [Version]
    -- ^ Versions this attempt is the first to charge for.
    , alwPermitted :: [Version]
    , alwWithheld :: [Version]
    }

{- An already-charged version passes whatever the allowance is, because the charge was taken before
its first attempt and a reassessment must not spend the cap on it twice. -}
splitByAllowance :: Int -> Set Text -> [Version] -> Allowance
splitByAllowance allowance charged selected =
    Allowance{alwFresh = fresh, alwPermitted = permitted, alwWithheld = filter (`notElem` permitted) selected}
  where
    held version = Set.member (renderVersion version) charged
    fresh = take allowance (filter (not . held) selected)
    permitted = filter (\version -> held version || version `elem` fresh) selected

-- A latched halt charges nothing further, and no cycle hands over more than its own cap.
remainingAllowance :: DeletionRun -> Maybe CycleHalt -> Int -> Int
remainingAllowance run halt issued
    | isJust halt = 0
    | otherwise = max 0 (swpDeletionCap (runPacing run) - issued)

withinAllowance :: DeletionRun -> SweepStore -> [Version] -> IO [Version]
withinAllowance run store selected = do
    charged <- readIORef (runCharged run)
    issued <- readIORef (stIssued (runCounters run))
    halt <- readIORef (runHalt run)
    let split = splitByAllowance (remainingAllowance run halt issued) charged selected
    countWithheld run store (alwWithheld split)
    pure (alwPermitted split)

charge :: DeletionRun -> SweepStore -> [Selection] -> IO [Version]
charge run store selections = do
    let selected = map selVersion selections
    halted <- readIORef (runHalt run)
    held <- readIORef (runCharged run)
    issued <- readIORef (stIssued (runCounters run))
    let split = splitByAllowance (remainingAllowance run halted issued) held selected
        admitted = alwFresh split
    modifyIORef' (runCharged run) (<> Set.fromList (map renderVersion admitted))
    modifyIORef' (stIssued (runCounters run)) (+ length admitted)
    when (issued < cap && issued + length admitted >= cap) $
        writeIORef (runCapGeneration run) (generationOf selections admitted)
    countWithheld run store (alwWithheld split)
    pure (alwPermitted split)
  where
    cap = swpDeletionCap (runPacing run)

-- The generation the cap is credited to is the one the charge that filled it was decided under.
generationOf :: [Selection] -> [Version] -> Maybe DbEtag
generationOf selections admitted =
    listToMaybe (reverse admitted) >>= \version -> selGeneration =<< find ((== version) . selVersion) selections

countWithheld :: DeletionRun -> SweepStore -> [Version] -> IO ()
countWithheld run store withheld =
    traverse_ (const (record (labelled run store) (runCounters run) SweepGuardSkipped)) withheld

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
            Right inventories -> reportResidual run store versions (inventory store inventories)

-- A version the backend accepted a delete for, still served and still denied, halts the cycle.
reportResidual :: DeletionRun -> SweepStore -> [Version] -> [StoredVersion] -> IO ()
reportResidual run store versions actual = do
    denied <- map selVersion <$> runSelect run False (atStore run store) actual
    let residual =
            [ storedVersion item
            | item <- actual
            , storedPresence item == VersionServed
            , storedVersion item `elem` versions
            , storedVersion item `elem` denied
            ]
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
