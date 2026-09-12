-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The Dredger's cycle over every mount's mirror store.
Candidate listing streams pages. Full walks resume from stored bucket cursors through
"Ecluse.Core.Registry.Sweep.Walk". Both share cycle pacing and deletion limits.
-}
module Ecluse.Core.Registry.Sweep (
    sweepCycle,
    withStoreRetry,
) where

import Data.Conduit (ConduitT, await, fuseBothMaybe, runConduit)

import Ecluse.Core.Ecosystem (ecosystemName)
import Ecluse.Core.Fault (RetryAfter (RetryAfter))
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Maintenance (
    ConsentVerdict (ConsentGranted, ConsentWithheld),
    NameAlphabet,
    NamePrefix,
    RetryAdvice (RetryDelayed, RetryFutile, RetryWorthwhile),
    StoreClass (StoreDestroyable, StorePreserved),
    StoreCursor (clearCursor, readCursor, writeCursor),
    StoreFacts (factBackend, factNameAlphabet),
    StoreFault (faultRetry),
    StoreObservation (obClassifyStore, obEnumerateVersions, obFacts, obListPackagesIn, obVerifyConsent),
    renderNamePrefix,
 )
import Ecluse.Core.Registry.Sweep.Candidates (CandidateSet, candidateSet, inCandidates)
import Ecluse.Core.Registry.Sweep.Package (sweepPackage)
import Ecluse.Core.Registry.Sweep.Types (
    CycleHalt (HaltBucketUnsplittable, HaltConsentWithheld, HaltStoreFault, HaltStorePreserved),
    CycleOutcome (CycleOutcome, outcomeEvidence, outcomeHalt, outcomePrerequisites, outcomeTally),
    PrerequisiteStatus (PrerequisiteMet, PrerequisiteUnmet, PrerequisiteUnread),
    SweepAudit (auditError, auditInfo, auditWarn),
    SweepExecution (SweepCounts, SweepRemoves),
    SweepMount (smConfigured, smEcosystem, smProjectName, smRuleDeps, smStore),
    SweepPacing (swpChunkPause, swpChunkSize, swpShape),
    SweepPorts (sweepAdvisoryEtag, sweepAudit, sweepDelay, sweepNow),
    SweepShape (SweepCandidates, SweepEverything),
    SweepState,
    SweepStore (ssExecute, ssObserve),
    TargetPrerequisites (TargetPrerequisites, tpBackend, tpClassification, tpConsent, tpEcosystem),
    evidenceComplete,
    newSweepState,
    prerequisitesMet,
    recordGap,
    recordPrerequisites,
    renderCycleHalt,
    renderEvidenceGaps,
    renderPrerequisites,
    renderStoreFault,
    renderTally,
    stChunkProgress,
    stEvidence,
    stPrerequisites,
    stTally,
    unloadedGeneration,
    walkMarkerOf,
 )
import Ecluse.Core.Registry.Sweep.Walk (
    BucketNames (BucketFaulted, BucketOverflowed, BucketRead, BucketUnsplittable),
    collectBucket,
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
    counters <- newSweepState
    halt <- stepUntilHalt (sweepMount pacing ports counters) mounts
    outcome <-
        CycleOutcome halt
            <$> readIORef (stTally counters)
            <*> (reverse <$> readIORef (stPrerequisites counters))
            <*> readIORef (stEvidence counters)
    reportCycle ports outcome
    pure outcome

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
    SweepRemoves _ -> firstHalt [clearedToDelete pacing ports mount, walk]
    SweepCounts -> notePrerequisites pacing ports counters mount >> walk
  where
    walk = walkStore pacing ports counters mount

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

{- Walk this mount's store in the shape the configuration selected. A full walk is a superset of
a candidate cycle, so nothing runs beside it. -}
walkStore :: SweepPacing -> SweepPorts -> SweepState -> SweepMount -> IO (Maybe CycleHalt)
walkStore pacing ports counters mount = do
    reportAdvisoryHalf ports counters mount
    case swpShape pacing of
        SweepCandidates -> candidateCycle pacing ports counters mount
        SweepEverything -> fullWalk pacing ports counters mount

{- The default shape: every bucket, carrying only the names the advisory database covers or an
identity deny pins. The listing is consumed a page at a time and never held whole. -}
candidateCycle :: SweepPacing -> SweepPorts -> SweepState -> SweepMount -> IO (Maybe CycleHalt)
candidateCycle pacing ports counters mount =
    stepUntilHalt candidateBucket (walkBuckets (alphabetOf mount))
  where
    candidateBucket prefix =
        withCandidates ports mount $ \candidates ctx ->
            streamCandidates pacing ports counters mount ctx (inCandidates candidates) prefix

{- One bucket's candidates under a pinned lookup. Later rule evaluations acquire their own lookups.
A generation swapped mid-bucket defers a name it newly covers by one cycle. -}
withCandidates :: SweepPorts -> SweepMount -> (CandidateSet -> EvalContext -> IO a) -> IO a
withCandidates ports mount act =
    rdWithCveLookup (smRuleDeps mount) $ \mLookup -> do
        candidates <- candidateSet (smProjectName mount) (smConfigured mount) (snd <$> mLookup)
        ctx <- mkEvalContext (sweepNow ports) (pure (fst <$> mLookup))
        act candidates ctx

{- Say once per mount when no generation is loaded. Only the identity half then sweeps, so a rule
set that reads advisories decided on less than it names, which is the gap recorded here. -}
reportAdvisoryHalf :: SweepPorts -> SweepState -> SweepMount -> IO ()
reportAdvisoryHalf ports counters mount =
    rdWithCveLookup (smRuleDeps mount) $ \mLookup ->
        whenNothing_ mLookup $ do
            when (any readsAdvisories (smConfigured mount)) (recordGap counters unloadedGeneration)
            auditError
                (sweepAudit ports)
                ( "no advisory database generation is loaded for the "
                    <> ecosystemName (smEcosystem mount)
                    <> " mount, so this cycle sweeps only the names an identity deny pins"
                )

-- The opt-in shape: every name in the store, bucket by bucket, resuming where the last run
-- stopped, and clearing the record when a walk completes so the next cycle starts a fresh one.
fullWalk :: SweepPacing -> SweepPorts -> SweepState -> SweepMount -> IO (Maybe CycleHalt)
fullWalk pacing ports counters mount =
    readWalkCursor pacing ports mount >>= \case
        Left halt -> pure (Just halt)
        Right resume ->
            walkFrom pacing ports counters mount resume (resumeAfter resume (walkBuckets (alphabetOf mount))) >>= \case
                Just halt -> pure (Just halt)
                Nothing -> onCursor pacing ports mount clearCursor

{- Walk the buckets in turn. A split replaces a bucket in place with the narrower ones covering it,
and the record is applied to those too, since the walk may have stopped inside that very split. -}
walkFrom :: SweepPacing -> SweepPorts -> SweepState -> SweepMount -> Maybe NamePrefix -> [NamePrefix] -> IO (Maybe CycleHalt)
walkFrom pacing ports counters mount resume = go
  where
    go [] = pure Nothing
    go (prefix : rest) =
        collectBucket (alphabetOf mount) prefix (obListPackagesIn (observed mount) prefix) >>= \case
            BucketFaulted fault -> pure (Just (storeHalt mount fault))
            BucketUnsplittable -> pure (Just (unsplittableHalt mount prefix))
            BucketOverflowed narrower -> go (resumeAfter resume (toList narrower) <> rest)
            BucketRead names -> sweepBucket prefix names >>= maybe (go rest) (pure . Just)

    -- A completed bucket is recorded before the next one starts, so a restart re-does one bucket.
    sweepBucket prefix names = do
        etag <- sweepAdvisoryEtag ports (smEcosystem mount)
        ctx <- mkEvalContext (sweepNow ports) (pure etag)
        sweepChunks pacing ports counters mount ctx names
            >>= maybe (onCursor pacing ports mount (`writeCursor` prefix)) (pure . Just)

{- One bucket's listing, consumed page by page so nothing holds it whole. This cycle's own halt
abandons the stream, and a listing that stopped on a fault halts too. -}
streamCandidates ::
    SweepPacing ->
    SweepPorts ->
    SweepState ->
    SweepMount ->
    EvalContext ->
    (PackageName -> Bool) ->
    NamePrefix ->
    IO (Maybe CycleHalt)
streamCandidates pacing ports counters mount ctx keep prefix =
    outcome <$> runConduit (fuseBothMaybe (obListPackagesIn (observed mount) prefix) foldPages)
  where
    -- The sweep's own halt is read first: it is the arm that abandoned the stream.
    outcome = \case
        (_, Just halt) -> Just halt
        (Just (Just fault), _) -> Just (storeHalt mount fault)
        _ -> Nothing

    foldPages :: ConduitT [PackageName] o IO (Maybe CycleHalt)
    foldPages =
        await >>= \case
            Nothing -> pure Nothing
            Just page ->
                lift (sweepChunks pacing ports counters mount ctx (filter keep page))
                    >>= maybe foldPages (pure . Just)

-- Pause only when another name needs examination, including after a page or bucket ends.
sweepChunks ::
    SweepPacing ->
    SweepPorts ->
    SweepState ->
    SweepMount ->
    EvalContext ->
    [PackageName] ->
    IO (Maybe CycleHalt)
sweepChunks pacing ports counters mount ctx = stepUntilHalt paced
  where
    paced name = do
        progress <- readIORef (stChunkProgress counters)
        when (progress >= max 1 (swpChunkSize pacing)) $ do
            sweepDelay ports (swpChunkPause pacing)
            writeIORef (stChunkProgress counters) 0
        modifyIORef' (stChunkProgress counters) (+ 1)
        sweepOne pacing ports counters mount ctx name

-- One package: what the store serves for it, then the shared decision step over those versions.
sweepOne ::
    SweepPacing ->
    SweepPorts ->
    SweepState ->
    SweepMount ->
    EvalContext ->
    PackageName ->
    IO (Maybe CycleHalt)
sweepOne pacing ports counters mount ctx name =
    withStoreRetry pacing ports mount (obEnumerateVersions (observed mount) name) >>= \case
        Left halt -> pure (Just halt)
        Right stored -> sweepPackage pacing ports counters mount ctx name stored

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

unsplittableHalt :: SweepMount -> NamePrefix -> CycleHalt
unsplittableHalt mount prefix =
    HaltBucketUnsplittable (smEcosystem mount) (backendOf mount) (renderNamePrefix prefix)

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
            ( "retrying a call against the "
                <> ecosystemName (smEcosystem mount)
                <> " mirror store on "
                <> backendOf mount
                <> " after "
                <> renderStoreFault fault
            )
        sweepDelay ports delay
        first (storeHalt mount) <$> call
