-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The Dredger's cycle over every mount's mirror store and the private cache it is paired with.
The two inventories are joined bucket by bucket through "Ecluse.Core.Registry.Sweep.Walk", and a
full walk resumes from the stored bucket cursor.
-}
module Ecluse.Core.Registry.Sweep (
    sweepCycle,
    paceAtCeiling,
    storeBudgets,
    withStoreRetry,
) where

import Data.List (lookup)
import Data.Map.Strict qualified as Map

import Ecluse.Core.Ecosystem (ecosystemName)
import Ecluse.Core.Fault (RetryAfter (RetryAfter))
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Maintenance (
    ConsentVerdict (ConsentGranted, ConsentWithheld),
    RetryAdvice (RetryDelayed, RetryFutile, RetryWorthwhile),
    StoreClass (StoreDestroyable, StorePreserved),
    StoreCursor (clearCursor, readCursor, writeCursor),
    StoreFacts (factBackend, factBudget, factNameAlphabet),
    StoreFault (faultRetry),
    StoreObservation (obClassifyStore, obEnumerateVersions, obFacts, obVerifyConsent),
    StoredVersion,
 )
import Ecluse.Core.Registry.Maintenance.Budget (
    BudgetPort (budgetClose, budgetOpen, budgetPaced),
    CycleCost (ccRequests, ccWorkSeconds),
    StoreBudget (bgScope),
    narrowestBudget,
    renderQuotaScope,
    renderRequestTally,
 )
import Ecluse.Core.Registry.Maintenance.NameSpace (
    NameAlphabet,
    NamePrefix,
    renderNamePrefix,
 )
import Ecluse.Core.Registry.Sweep.Candidates (CandidateSet, candidateSet, inCandidates)
import Ecluse.Core.Registry.Sweep.Group (boundedVersions, collectGroupBucket, groupAlphabet)
import Ecluse.Core.Registry.Sweep.Outcome (
    CycleHalt (HaltBucketUnsplittable, HaltConsentWithheld, HaltStoreFault, HaltStorePreserved),
    CycleOutcome (CycleOutcome, outcomeEvidence, outcomeHalt, outcomePrerequisites, outcomeTally),
    PrerequisiteStatus (PrerequisiteMet, PrerequisiteUnmet, PrerequisiteUnread),
    TargetPrerequisites (TargetPrerequisites, tpBackend, tpClassification, tpConsent, tpEcosystem),
    evidenceComplete,
    prerequisitesMet,
    renderCycleHalt,
    renderEvidenceGaps,
    renderPrerequisites,
    renderStoreFault,
    renderTally,
    storeSubject,
    unloadedGeneration,
 )
import Ecluse.Core.Registry.Sweep.Pacing (PaceDecision (pdPace, pdScope), decidePace, renderPaceDecision)
import Ecluse.Core.Registry.Sweep.Package (previewPackageGroup, sweepPackageGroup)
import Ecluse.Core.Registry.Sweep.Types (
    SweepAudit (auditError, auditInfo, auditWarn),
    SweepExecution (SweepCounts, SweepRemoves),
    SweepMount (smConfigured, smEcosystem, smProjectName, smRuleDeps, smStore),
    SweepPacing (swpChunkPause, swpChunkSize, swpShape),
    SweepPorts (sweepAudit, sweepBudget, sweepDelay, sweepNow),
    SweepShape (SweepEverything),
    SweepState,
    SweepStore (ssExecute, ssObserve, ssVersionLimit),
    countingAt,
    newSweepState,
    privateStore,
    recordGap,
    recordPrerequisites,
    stChunkProgress,
    stEvidence,
    stPrerequisites,
    stTally,
    walkMarkerOf,
 )
import Ecluse.Core.Registry.Sweep.Walk (
    BucketNames (BucketFaulted, BucketOverflowed, BucketRead, BucketUnsplittable),
    resumeAfter,
    walkBuckets,
 )
import Ecluse.Core.Rules (RuleDeps (rdWithCveLookup))
import Ecluse.Core.Rules.Types (EvalContext, mkEvalContext, readsAdvisories)

{- | Run one cycle: every mount's store in turn. A halt ends the whole cycle, because every reason
for one is a fact about the deployment rather than about one package.
-}
sweepCycle :: SweepPacing -> SweepPorts -> [SweepMount] -> IO CycleOutcome
sweepCycle pacing ports mounts = do
    budgetOpen (sweepBudget ports)
    counters <- newSweepState
    halt <- stepUntilHalt (sweepMount pacing ports counters) mounts
    outcome <-
        CycleOutcome halt
            <$> readIORef (stTally counters)
            <*> (reverse <$> readIORef (stPrerequisites counters))
            <*> readIORef (stEvidence counters)
    reportCycle ports outcome
    paceNextCycle pacing ports mounts outcome
    pure outcome

{- | Hold every scope to its ceiling before any cycle has measured one. A Dredger whose every
cycle halts never reaches the measured decision, so this is where its rate comes from.
-}
paceAtCeiling :: SweepPacing -> SweepPorts -> [SweepMount] -> IO ()
paceAtCeiling pacing ports mounts =
    budgetPaced (sweepBudget ports) (Map.fromList [(pdScope decision, pdPace decision) | decision <- decisions])
  where
    decisions = [decidePace pacing budget Nothing | budget <- storeBudgets mounts]

{- Pace the next cycle from what this one measured. A halted cycle read part of the store, so its
counts are discarded rather than allowed to replace a complete sample's pace. -}
paceNextCycle :: SweepPacing -> SweepPorts -> [SweepMount] -> CycleOutcome -> IO ()
paceNextCycle pacing ports mounts outcome = do
    cost <- budgetClose (sweepBudget ports)
    unless (isJust (outcomeHalt outcome)) $ do
        traverse_ (auditInfo (sweepAudit ports) . renderMeasured) (Map.toAscList (ccRequests cost))
        let decisions = [decidePace pacing budget (sampleOf cost budget) | budget <- storeBudgets mounts]
        traverse_ (traverse_ (auditWarn (sweepAudit ports)) . renderPaceDecision pacing) decisions
        budgetPaced (sweepBudget ports) (Map.fromList [(pdScope decision, pdPace decision) | decision <- decisions])
  where
    sampleOf cost budget = (,ccWorkSeconds cost) <$> Map.lookup (bgScope budget) (ccRequests cost)
    renderMeasured (scope, tally) =
        "this cycle asked " <> renderQuotaScope scope <> " for " <> renderRequestTally tally

{- | Each distinct capacity pool the cycle's stores share. Two stores that landed in one pool are
paced by the narrower of what each claims, never by whichever the fold read last.
-}
storeBudgets :: [SweepMount] -> [StoreBudget]
storeBudgets mounts =
    Map.elems (Map.fromListWith narrowestBudget [(bgScope budget, budget) | mount <- mounts, budget <- budgetsOf mount])
  where
    budgetsOf mount =
        [ factBudget (obFacts (ssObserve store))
        | store <- [smStore mount, privateStore (smStore mount)]
        ]

{- What a real sweep of each target still needs, above the counts, then what the cycle did and what
it could not read. The two never merge: a complete count is not a permission to delete. -}
reportCycle :: SweepPorts -> CycleOutcome -> IO ()
reportCycle ports outcome = do
    traverse_ (announcePrerequisites ports) (outcomePrerequisites outcome)
    case outcomeHalt outcome of
        Nothing -> auditInfo (sweepAudit ports) ("mirror sweep cycle complete: " <> closing)
        Just reason ->
            auditError (sweepAudit ports) ("mirror sweep cycle halted: " <> renderCycleHalt reason <> "; " <> closing)
  where
    gaps = outcomeEvidence outcome
    closing
        | evidenceComplete gaps = renderTally (outcomeTally outcome)
        | otherwise = renderTally (outcomeTally outcome) <> "; counted from partial evidence: " <> renderEvidenceGaps gaps

-- A target a real sweep would stop on warns. One it would pass reports as routine.
announcePrerequisites :: SweepPorts -> TargetPrerequisites -> IO ()
announcePrerequisites ports target =
    severity (sweepAudit ports) (renderPrerequisites target)
  where
    severity = if prerequisitesMet target then auditInfo else auditWarn

{- Walk the steps until one halts. The mounts, the buckets, and the packages of a chunk all fold
this way, so a halt ends the cycle from wherever it is raised. -}
stepUntilHalt :: (a -> IO (Maybe CycleHalt)) -> [a] -> IO (Maybe CycleHalt)
stepUntilHalt step = go
  where
    go [] = pure Nothing
    go (x : xs) = step x >>= maybe (go xs) (pure . Just)

firstHalt :: [IO (Maybe CycleHalt)] -> IO (Maybe CycleHalt)
firstHalt = stepUntilHalt id

{- One mount: its two standing permissions, then a walk over it. Both are read at every cycle
start, because an operator revokes either and a store can be recreated as a different kind. -}
sweepMount :: SweepPacing -> SweepPorts -> SweepState -> SweepMount -> IO (Maybe CycleHalt)
sweepMount pacing ports counters mount = case ssExecute (smStore mount) of
    SweepRemoves _ -> firstHalt (map (clearedToDelete pacing ports) targets <> [walk])
    SweepCounts -> do
        traverse_ (notePrerequisites pacing ports counters) targets
        walk
  where
    targets = [mount, atPrivateCache mount]
    walk = walkStore pacing ports counters mount

-- The same mount seen at its private cache, whose permissions and inventory are its own.
atPrivateCache :: SweepMount -> SweepMount
atPrivateCache mount = mount{smStore = privateStore (smStore mount)}

{- The two standing permissions a delete needs: the operator's own marker, and whether deleting
from this store destroys anything. Both halts name the backend that raised them. -}
clearedToDelete :: SweepPacing -> SweepPorts -> SweepMount -> IO (Maybe CycleHalt)
clearedToDelete pacing ports mount = do
    consent <- withStoreRetry pacing ports mount (obVerifyConsent (observed mount))
    classified <- withStoreRetry pacing ports mount (obClassifyStore (observed mount))
    pure $ case (consent, classified) of
        (Left halt, _) -> Just halt
        (_, Left halt) -> Just halt
        (Right (ConsentWithheld descriptor), _) -> Just (HaltConsentWithheld eco backend descriptor)
        (_, Right (StorePreserved why)) -> Just (HaltStorePreserved eco backend why)
        (Right ConsentGranted, Right StoreDestroyable) -> Nothing
  where
    eco = smEcosystem mount
    backend = backendOf mount

{- The same two reads, kept as findings. A preview stops for neither, so a permission it could not
read is a finding as well rather than an end to the enumeration. -}
notePrerequisites :: SweepPacing -> SweepPorts -> SweepState -> SweepMount -> IO ()
notePrerequisites pacing ports counters mount = do
    consent <- withStoreRetry pacing ports mount (obVerifyConsent (observed mount))
    classified <- withStoreRetry pacing ports mount (obClassifyStore (observed mount))
    recordPrerequisites
        counters
        TargetPrerequisites
            { tpEcosystem = smEcosystem mount
            , tpBackend = backendOf mount
            , tpConsent = either unreadable consentStatus consent
            , tpClassification = either unreadable classStatus classified
            }
  where
    unreadable = PrerequisiteUnread . renderCycleHalt

    consentStatus = \case
        ConsentGranted -> PrerequisiteMet
        ConsentWithheld descriptor -> PrerequisiteUnmet descriptor

    classStatus = \case
        StoreDestroyable -> PrerequisiteMet
        StorePreserved why -> PrerequisiteUnmet why

{- Walk this mount's two stores as one inventory, in the shape the configuration selected. A
full walk is a superset of a candidate cycle, so nothing runs beside it. -}
walkStore :: SweepPacing -> SweepPorts -> SweepState -> SweepMount -> IO (Maybe CycleHalt)
walkStore pacing ports counters mount = do
    reportAdvisoryHalf ports counters mount
    walkGroup pacing ports counters mount (privateStore (smStore mount))

{- What every step of one mount's paired walk closes over: the two stores, the pacing and ports
the steps run under, and the name a halt reports both backends under. -}
data GroupWalk = GroupWalk
    { gwPacing :: SweepPacing
    , gwPorts :: SweepPorts
    , gwCounters :: SweepState
    , gwMount :: SweepMount
    , gwCache :: SweepStore
    , gwCombined :: Text
    }

walkGroup :: SweepPacing -> SweepPorts -> SweepState -> SweepMount -> SweepStore -> IO (Maybe CycleHalt)
walkGroup pacing ports counters mount cache = do
    resume <- if resumable then readWalkCursor pacing ports mount else pure (Right Nothing)
    case resume of
        Left halt -> pure (Just halt)
        Right cursor -> go cursor (resumeAfter cursor (walkBuckets alphabet))
  where
    walk =
        GroupWalk
            { gwPacing = pacing
            , gwPorts = ports
            , gwCounters = counters
            , gwMount = mount
            , gwCache = cache
            , gwCombined = backendOf mount <> " and " <> factBackend (obFacts (ssObserve cache)) <> " (combined inventory)"
            }
    alphabet = groupAlphabet (observed mount) (ssObserve cache)
    resumable = swpShape pacing == SweepEverything && alphabet == alphabetOf mount
    marker action = if resumable then onCursor pacing ports mount action else pure Nothing
    go _ [] = marker clearCursor
    go resume (prefix : rest) =
        collectGroupBucket alphabet prefix (observed mount) (ssObserve cache) >>= \case
            BucketFaulted (store, fault) -> pure (Just (storeHalt (locatedMount mount store) fault))
            BucketUnsplittable -> pure (Just (HaltBucketUnsplittable (smEcosystem mount) (gwCombined walk) (renderNamePrefix prefix)))
            BucketOverflowed narrower -> go resume (resumeAfter resume (toList narrower) <> rest)
            BucketRead names ->
                withCandidates
                    ports
                    mount
                    ( \candidates ctx ->
                        stepUntilHalt (sweepOneName walk ctx) (filter (wanted candidates . fst) names)
                    )
                    >>= maybe (marker (`writeCursor` prefix) >>= maybe (go resume rest) (pure . Just)) (pure . Just)
    wanted candidates name = swpShape pacing == SweepEverything || inCandidates candidates name

{- One name of a bucket: the chunk pause falls here, before the enumeration reads, so a pause
never lands between the two locations of a single name. -}
sweepOneName :: GroupWalk -> EvalContext -> (PackageName, [Bool]) -> IO (Maybe CycleHalt)
sweepOneName walk ctx (name, slots) = do
    paceName (gwPacing walk) (gwPorts walk) (gwCounters walk)
    readGroupVersions walk name slots >>= \case
        Left halt -> pure (Just halt)
        Right versions -> case boundedVersions (ssVersionLimit (smStore (gwMount walk))) versions of
            Left fault ->
                pure (Just (HaltStoreFault (smEcosystem (gwMount walk)) (gwCombined walk) (renderStoreFault fault)))
            Right bounded -> groupOutcome walk ctx name bounded

-- What each of a name's locations holds, in slot order. The first store fault halts the cycle.
readGroupVersions :: GroupWalk -> PackageName -> [Bool] -> IO (Either CycleHalt [(SweepStore, [StoredVersion])])
readGroupVersions walk name slots = sequence <$> traverse readOne locations
  where
    locations = map (\slot -> if slot then gwCache walk else smStore (gwMount walk)) slots
    readOne store =
        fmap (store,)
            <$> withStoreRetry
                (gwPacing walk)
                (gwPorts walk)
                (locatedMount (gwMount walk) (ssObserve store))
                (obEnumerateVersions (ssObserve store) name)

-- Count or remove one name's joined inventory, as the mount's execution mode decides.
groupOutcome :: GroupWalk -> EvalContext -> PackageName -> [(SweepStore, [StoredVersion])] -> IO (Maybe CycleHalt)
groupOutcome walk ctx name bounded = case ssExecute (smStore mount) of
    SweepCounts -> previewPackageGroup pacing ports counters mount ctx name (map (first ssObserve) bounded)
    SweepRemoves _ ->
        sweepPackageGroup pacing ports counters mount name [(smStore mount, held (smStore mount)), (gwCache walk, held (gwCache walk))]
  where
    pacing = gwPacing walk
    ports = gwPorts walk
    counters = gwCounters walk
    mount = gwMount walk
    -- Read with `lookup`, so two stores under one backend name both take the first one's
    -- inventory. A Map would take the last instead, which is a different set of versions.
    held store = fromMaybe [] (lookup (backendName store) [(backendName located, versions) | (located, versions) <- bounded])
    backendName = factBackend . obFacts . ssObserve

locatedMount :: SweepMount -> StoreObservation -> SweepMount
locatedMount mount store = mount{smStore = countingAt (smStore mount) store}

{- One bucket's candidates under a pinned lookup. Later rule evaluations acquire their own lookups.
A generation swapped mid-bucket defers a name it newly covers by one cycle. -}
withCandidates :: SweepPorts -> SweepMount -> (CandidateSet -> EvalContext -> IO a) -> IO a
withCandidates ports mount act =
    rdWithCveLookup (smRuleDeps mount) $ \mLookup -> do
        candidates <- candidateSet (smProjectName mount) (smConfigured mount) (snd <$> mLookup)
        ctx <- mkEvalContext (sweepNow ports) (pure (fst <$> mLookup))
        act candidates ctx

{- Say once per mount whose rules read advisories and no generation is loaded. Only the identity
half then sweeps, so that rule set decided on less than it names, which is the gap recorded here. -}
reportAdvisoryHalf :: SweepPorts -> SweepState -> SweepMount -> IO ()
reportAdvisoryHalf ports counters mount =
    when (any readsAdvisories (smConfigured mount)) $
        rdWithCveLookup (smRuleDeps mount) $ \mLookup ->
            whenNothing_ mLookup $ do
                recordGap counters unloadedGeneration
                auditError
                    (sweepAudit ports)
                    ( "no advisory database generation is loaded for the "
                        <> ecosystemName (smEcosystem mount)
                        <> " mount, so this cycle sweeps only the names an identity deny pins"
                    )

paceName :: SweepPacing -> SweepPorts -> SweepState -> IO ()
paceName pacing ports counters = do
    progress <- readIORef (stChunkProgress counters)
    when (progress >= max 1 (swpChunkSize pacing)) $ do
        sweepDelay ports (swpChunkPause pacing)
        writeIORef (stChunkProgress counters) 0
    modifyIORef' (stChunkProgress counters) (+ 1)

{- The bucket the last run of this walk completed. A store with nowhere to keep one resumes from
the first bucket every cycle, which is a value here rather than a branch. -}
readWalkCursor :: SweepPacing -> SweepPorts -> SweepMount -> IO (Either CycleHalt (Maybe NamePrefix))
readWalkCursor pacing ports mount = case walkMarkerOf (smStore mount) of
    Nothing -> pure (Right Nothing)
    Just cursor -> withStoreRetry pacing ports mount (readCursor cursor)

{- Run one cursor write, where this run holds a cursor to write. A store with nowhere to keep one
records nothing, and a preview holds none at all. -}
onCursor :: SweepPacing -> SweepPorts -> SweepMount -> (StoreCursor -> IO (Either StoreFault ())) -> IO (Maybe CycleHalt)
onCursor pacing ports mount write =
    maybe (pure Nothing) recorded (walkMarkerOf (smStore mount))
  where
    recorded cursor = leftToMaybe <$> withStoreRetry pacing ports mount (write cursor)

storeHalt :: SweepMount -> StoreFault -> CycleHalt
storeHalt mount fault = HaltStoreFault (smEcosystem mount) (backendOf mount) (renderStoreFault fault)

observed :: SweepMount -> StoreObservation
observed = ssObserve . smStore

backendOf :: SweepMount -> Text
backendOf = factBackend . obFacts . observed

alphabetOf :: SweepMount -> NameAlphabet
alphabetOf = factNameAlphabet . obFacts . observed

{- | One store call, retried once after the wait its own fault advises. A fault that survives that
wait halts the cycle, which the next cycle re-attempts after the cycle pause.
-}
withStoreRetry :: SweepPacing -> SweepPorts -> SweepMount -> IO (Either StoreFault a) -> IO (Either CycleHalt a)
withStoreRetry pacing ports mount call =
    call >>= \case
        Right answered -> pure (Right answered)
        Left fault -> case faultRetry fault of
            RetryFutile -> pure (Left (storeHalt mount fault))
            RetryWorthwhile -> again (swpChunkPause pacing) fault
            RetryDelayed (RetryAfter seconds) -> again (fromIntegral seconds) fault
  where
    -- A retry that clears leaves the cycle running, so it warns rather than reporting a fault the
    -- operator has to act on. Only the halt after a failed retry is an error.
    again delay fault = do
        auditWarn
            (sweepAudit ports)
            ( "retrying a call against "
                <> storeSubject (smEcosystem mount) (backendOf mount)
                <> " after "
                <> renderStoreFault fault
            )
        sweepDelay ports delay
        first (storeHalt mount) <$> call
