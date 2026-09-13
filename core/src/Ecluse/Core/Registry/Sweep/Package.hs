-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Decide stored versions from the store's evidence and hand named denials to its execution.
module Ecluse.Core.Registry.Sweep.Package (
    sweepPackage,
    previewPackageGroup,
) where

import Data.List (partition)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Ecluse.Core.Cve (DbEtag)
import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Registry.Maintenance (
    StoreDeletion (dlDeleteVersions),
    StoreFacts (factBackend),
    StoreFault,
    StoreObservation (obFacts, obReadManifest),
    StoredVersion (storedPresence, storedVersion),
    VersionOutcome (VersionRefused, VersionRemoved, VersionRemoving, VersionUnreached),
    VersionPresence (VersionServed),
    refusalCode,
    refusalDetail,
 )
import Ecluse.Core.Registry.Metadata (Manifest (manifestInfo))
import Ecluse.Core.Registry.Sweep.Types (
    CycleHalt (HaltDeletionCap),
    SweepAudit (auditError, auditInfo),
    SweepExecution (SweepCounts, SweepRemoves),
    SweepMount (smConfigured, smFirstParty, smRuleDeps, smRules, smStore),
    SweepPacing (swpDeletionCap),
    SweepPorts (sweepAudit, sweepReport),
    SweepReport (reportCapHalts, reportOpening, reportRemoval),
    SweepState (stIssued),
    SweepStore (ssExecute, ssObserve),
    previewStore,
    record,
    recordGap,
    renderGeneration,
    renderStoreFault,
    unreadManifest,
 )
import Ecluse.Core.Rules (RuleDeps (rdAdvisoryFreshness), evalRules, renderIneligible)
import Ecluse.Core.Rules.Types (Decision (Blocked), EvalContext, Reason, RuleEvidence, completeEvidence, identityEvidence, readsAdvisories, ruleName)
import Ecluse.Core.Server.Metadata (selectVersion)
import Ecluse.Core.Telemetry.Metrics (SweepResult (SweepExamined, SweepGuardSkipped, SweepKept))
import Ecluse.Core.Version (Version, renderVersion)

{- | Decide one package's stored versions and hand the condemned ones over, yielding the halt the
deletion cap raised. A faulted read decides on identity alone, so it too can reach the cap.
-}
sweepPackage ::
    SweepPacing ->
    SweepPorts ->
    SweepState ->
    SweepMount ->
    EvalContext ->
    PackageName ->
    [StoredVersion] ->
    IO (Maybe CycleHalt)
sweepPackage pacing ports counters mount ctx name stored =
    selectPackage ports counters mount ctx name stored >>= deleteSelected pacing ports counters mount name

selectPackage :: SweepPorts -> SweepState -> SweepMount -> EvalContext -> PackageName -> [StoredVersion] -> IO [Condemned]
selectPackage ports counters mount ctx name stored
    | smFirstParty mount name = [] <$ traverse_ (const (record ports counters SweepGuardSkipped)) served
    | otherwise =
        obReadManifest (ssObserve (smStore mount)) name >>= \case
            Left fault -> do
                recordGap counters unreadManifest
                announceUnread ports name served fault
                decideAll (identityEvidence name)
            Right manifest -> decideAll (evidenceIn name manifest)
  where
    served = [storedVersion s | s <- stored, storedPresence s == VersionServed]
    decideAll evidence = catMaybes <$> traverse (decideVersion ports counters mount ctx evidence) served

-- | Evaluate each copy with its own evidence and count each selected version once for the mount.
previewPackageGroup :: SweepPacing -> SweepPorts -> SweepState -> SweepMount -> EvalContext -> PackageName -> [(StoreObservation, [StoredVersion])] -> IO (Maybe CycleHalt)
previewPackageGroup pacing ports counters mount ctx name locations = do
    selections <- forM locations $ \(store, versions) -> do
        let locatedMount = mount{smStore = previewStore store}
            locatedPorts = ports{sweepAudit = labelAudit (factBackend (obFacts store)) (sweepAudit ports)}
        selected <-
            selectPackage locatedPorts counters locatedMount ctx name versions
                >>= stillEligible locatedPorts counters locatedMount name
        traverse_ (announce locatedPorts name) selected
        let selectedKeys = Set.fromList (map (renderVersion . cdVersion) selected)
            kept =
                [ storedVersion version
                | version <- versions
                , storedPresence version == VersionServed
                , Set.notMember (renderVersion (storedVersion version)) selectedKeys
                ]
        traverse_
            ( \version ->
                auditInfo
                    (sweepAudit locatedPorts)
                    ("dry run, keeping " <> renderPackageName name <> "@" <> renderVersion version)
            )
            kept
        pure selected
    let logical = Map.elems (Map.fromList [(renderVersion (cdVersion selected), selected) | selected <- concat selections])
    issued <- readIORef (stIssued counters)
    let reached = issued + length logical
        cap = swpDeletionCap pacing
        etag = cdAdvisoryEtag =<< listToMaybe (drop (cap - issued - 1) logical)
    writeIORef (stIssued counters) reached
    when (issued < cap && reached >= cap) (announceCap ports cap reached etag)
    traverse_ (const (record ports counters (reportRemoval (sweepReport ports)))) logical
    pure Nothing

labelAudit :: Text -> SweepAudit -> SweepAudit
labelAudit target audit =
    audit
        { auditInfo = auditInfo audit . ((target <> ": ") <>)
        , auditError = auditError audit . ((target <> ": ") <>)
        }

{- The store served no metadata, so each version is decided on the identity the listing carries. The
shared fetch discards the response status, so a package the store no longer serves arrives here too. -}
announceUnread :: SweepPorts -> PackageName -> [Version] -> StoreFault -> IO ()
announceUnread ports name served fault =
    auditError
        (sweepAudit ports)
        ( renderPackageName name
            <> ": the store served no metadata this cycle, so its "
            <> show (length served)
            <> " versions are decided on identity alone: "
            <> renderStoreFault fault
        )

{- The manifest's own entry for a version, or identity alone where it projects none. A listing can
name a version the manifest omits, and identity is established either way. -}
evidenceIn :: PackageName -> Manifest -> Version -> RuleEvidence
evidenceIn name manifest version =
    maybe (identityEvidence name version) completeEvidence (selectVersion version (manifestInfo manifest))

{- | One version a named decisive deny condemned, with the rule that named it. Its audit line
and its deletion both read this, so neither can credit a rule the other did not.
-}
data Condemned = Condemned
    { cdVersion :: Version
    , cdRule :: Text
    , cdAdvisoryEtag :: Maybe DbEtag
    , cdReason :: Reason
    }

{- Decide one version from whatever evidence it has and count it. Only a named decisive deny
condemns, so this runs 'evalRules' rather than the wrapper that folds in deny-by-default. -}
decideVersion ::
    SweepPorts ->
    SweepState ->
    SweepMount ->
    EvalContext ->
    (Version -> RuleEvidence) ->
    Version ->
    IO (Maybe Condemned)
decideVersion ports counters mount ctx evidence version = do
    record ports counters SweepExamined
    evalRules ctx (smRules mount) (evidence version) >>= \case
        Blocked rule etag reason -> pure (Just Condemned{cdVersion = version, cdRule = rule, cdAdvisoryEtag = etag, cdReason = reason})
        _ -> record ports counters SweepKept $> Nothing

{- Hand the condemned versions over, up to what the cycle's cap still allows. The cap counts
what was handed over rather than what came back, because the cap bounds destructive calls. -}
deleteSelected ::
    SweepPacing ->
    SweepPorts ->
    SweepState ->
    SweepMount ->
    PackageName ->
    [Condemned] ->
    IO (Maybe CycleHalt)
deleteSelected pacing ports counters mount name decided
    | null decided = pure Nothing
    | otherwise = do
        condemned <- stillEligible ports counters mount name decided
        issued <- readIORef (stIssued counters)
        let allowance = max 0 (cap - issued)
            (taken, held) = splitAt (if capHalts then allowance else length condemned) condemned
            reached = issued + length taken
            thresholdEtag = cdAdvisoryEtag =<< listToMaybe (drop (cap - issued - 1) taken)
        traverse_ (const (record ports counters SweepGuardSkipped)) held
        unless (null taken) $ do
            traverse_ (announce ports name) taken
            writeIORef (stIssued counters) reached
            when (crossedCap issued reached) (announceCap ports cap reached thresholdEtag)
            outcomes <- sendDeletes (smStore mount) name (map cdVersion taken)
            traverse_ (recordOutcome ports counters name) outcomes
        pure (cappedHalt pacing reached thresholdEtag <$ guard (halts reached))
  where
    cap = swpDeletionCap pacing
    capHalts = reportCapHalts (sweepReport ports)

    -- Reaching the cap latches, whether or not this package had more to hand over.
    halts reached = capHalts && reached >= cap

    -- A run that counts past the cap says once where a run that halts on it would have stopped.
    crossedCap issued reached = not capHalts && issued < cap && reached >= cap

{- The push can stop being eligible evidence between a version's decision and this hand-over,
across a long manifest read or a batch, and a delete is permanent. So it is read again here. -}
stillEligible :: SweepPorts -> SweepState -> SweepMount -> PackageName -> [Condemned] -> IO [Condemned]
stillEligible ports counters mount name condemned =
    rdAdvisoryFreshness (smRuleDeps mount) >>= \freshness -> case renderIneligible freshness of
        Nothing -> pure condemned
        Just why -> do
            let (withheld, keeping) = partition (advisoryNamed mount . cdRule) condemned
            unless (null withheld) $ do
                traverse_ (const (record ports counters SweepGuardSkipped)) withheld
                announceIneligible ports name why withheld
            pure keeping

-- Whether a rule name credited to a condemnation is one of this mount's advisory-reading rules.
advisoryNamed :: SweepMount -> Text -> Bool
advisoryNamed mount credited = credited `elem` [ruleName r | r <- smConfigured mount, readsAdvisories r]

-- The versions the unusable evidence spared, so an operator sees what a recovered Pilot would act on.
announceIneligible :: SweepPorts -> PackageName -> Text -> [Condemned] -> IO ()
announceIneligible ports name why withheld =
    auditError
        (sweepAudit ports)
        ( renderPackageName name
            <> ": "
            <> show (length withheld)
            <> " versions an advisory rule denied stay in the store, because "
            <> why
        )

-- The halt the cap raises, carrying what an operator needs to judge the generation that filled it.
cappedHalt :: SweepPacing -> Int -> Maybe DbEtag -> CycleHalt
cappedHalt pacing = HaltDeletionCap (swpDeletionCap pacing)

{- The cap as a run that does not halt on it reports it: where a halting run would have stopped,
and that this one carries on, so the closing tally names the full reach. -}
announceCap :: SweepPorts -> Int -> Int -> Maybe DbEtag -> IO ()
announceCap ports cap reached etag =
    auditInfo (sweepAudit ports) $
        "the cycle has handed over "
            <> show reached
            <> " versions and reached the deletion cap of "
            <> show cap
            <> " under advisory generation "
            <> renderGeneration etag
            <> ". This run counts past the cap rather than halting, so its closing tally reports the full reach"

{- Every deletion's audit line: the package, the version, the rule that denied it, and the
advisory generation pinned when it was decided. -}
announce :: SweepPorts -> PackageName -> Condemned -> IO ()
announce ports name condemned =
    auditInfo (sweepAudit ports) $
        reportOpening (sweepReport ports)
            <> renderPackageName name
            <> "@"
            <> renderVersion (cdVersion condemned)
            <> ": blocked by "
            <> cdRule condemned
            <> " ("
            <> cdReason condemned
            <> "); advisory generation "
            <> renderGeneration (cdAdvisoryEtag condemned)

-- The backend owns chunk limits and stops later requests after a fault.
sendDeletes :: SweepStore -> PackageName -> [Version] -> IO [(Version, VersionOutcome)]
sendDeletes store name versions = case ssExecute store of
    SweepRemoves deletion -> dlDeleteVersions deletion name versions
    -- The audit line above has already put the reach on record, so the count reads from it.
    SweepCounts -> pure [(version, VersionRemoved) | version <- versions]

{- What the backend reported for one version. A refusal or an unreached call leaves the version
in the store, so it counts as kept and reports for an operator to follow up. -}
recordOutcome :: SweepPorts -> SweepState -> PackageName -> (Version, VersionOutcome) -> IO ()
recordOutcome ports counters name (version, outcome) = case outcome of
    VersionRemoved -> record ports counters removal
    VersionRemoving reference -> do
        -- No backend completes later today. The next cycle's listing shows whether it finished,
        -- and a version still served is decided and deleted again, which is idempotent.
        auditInfo (sweepAudit ports) (subject <> ": the backend is removing it under " <> reference)
        record ports counters removal
    VersionRefused refusal -> do
        auditError
            (sweepAudit ports)
            (subject <> ": the backend refused the delete, " <> refusalCode refusal <> ": " <> refusalDetail refusal)
        record ports counters SweepKept
    VersionUnreached fault -> do
        auditError (sweepAudit ports) (subject <> ": the delete did not reach the backend: " <> renderStoreFault fault)
        record ports counters SweepKept
  where
    subject = renderPackageName name <> "@" <> renderVersion version
    removal = reportRemoval (sweepReport ports)
