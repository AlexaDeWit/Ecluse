-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Decide stored versions from the store's evidence and hand named denials to its execution.
module Ecluse.Core.Registry.Sweep.Package (
    previewPackageGroup,
    sweepPackageGroup,
) where

import Data.Containers.ListUtils (nubOrdOn)
import Data.List (partition)
import Data.Set qualified as Set

import Ecluse.Core.Cve.Types (DbEtag)
import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Registry.Maintenance (
    StoreFault,
    StoreObservation (obReadManifest),
    StoredVersion (storedPresence, storedVersion),
    VersionOutcome (VersionRefused, VersionRemoved, VersionRemoving, VersionUncertain, VersionUnreached),
    VersionPresence (VersionServed),
    refusalCode,
    refusalDetail,
 )
import Ecluse.Core.Registry.Metadata (Manifest (manifestInfo))
import Ecluse.Core.Registry.Sweep.Deletion (Selection (Selection), deleteGroup)
import Ecluse.Core.Registry.Sweep.Types (
    CycleHalt,
    SweepAudit (auditError, auditInfo),
    SweepMount (smConfigured, smEcosystem, smFirstParty, smRuleDeps, smRules, smStore),
    SweepPacing (swpDeletionCap),
    SweepPorts (sweepAdvisoryEtag, sweepAudit, sweepNow, sweepReport),
    SweepReport (reportOpening, reportRemoval),
    SweepState (stIssued),
    SweepStore (ssObserve),
    countingAt,
    locatedPorts,
    record,
    recordGap,
    recordMetric,
    recordTally,
    renderGeneration,
    renderStoreFault,
    unreadManifest,
 )
import Ecluse.Core.Rules (RuleDeps (rdAdvisoryFreshness), evalRules, renderIneligible)
import Ecluse.Core.Rules.Types (Decision (Blocked), EvalContext, Reason, RuleEvidence, completeEvidence, identityEvidence, mkEvalContext, readsAdvisories, ruleName)
import Ecluse.Core.Server.Metadata (selectVersion)
import Ecluse.Core.Telemetry.Metrics (SweepResult (SweepExamined, SweepGuardSkipped, SweepKept))
import Ecluse.Core.Version (Version, renderVersion)

{- | One version a named decisive deny condemned, with the rule that named it. Its audit line
and its deletion both read this, so neither can credit a rule the other did not.
-}
data Condemned = Condemned
    { cdVersion :: Version
    , cdRule :: Text
    , cdAdvisoryEtag :: Maybe DbEtag
    , cdReason :: Reason
    }

-- | Evaluate each copy with its own evidence and count each selected version once for the mount.
previewPackageGroup :: SweepPacing -> SweepPorts -> SweepState -> SweepMount -> EvalContext -> PackageName -> [(StoreObservation, [StoredVersion])] -> IO (Maybe CycleHalt)
previewPackageGroup pacing ports counters mount ctx name locations = do
    selections <- traverse (previewLocation ports counters mount ctx name) locations
    chargePreview pacing ports counters (nubOrdOn (renderVersion . cdVersion) (concat selections))
    pure Nothing

previewLocation :: SweepPorts -> SweepState -> SweepMount -> EvalContext -> PackageName -> (StoreObservation, [StoredVersion]) -> IO [Condemned]
previewLocation ports counters mount ctx name (store, versions) = do
    selected <-
        selectPackage True located counters locatedMount ctx name versions
            >>= stillEligible located counters locatedMount name
    traverse_ (announce located name) selected
    traverse_ (const (recordMetric located (reportRemoval (sweepReport ports)))) selected
    announceKept located name (stillServed versions selected)
    pure selected
  where
    locatedMount = mount{smStore = countingAt (smStore mount) store}
    located = locatedPorts mount store ports

-- The served versions a preview leaves behind, which it reports one line each.
stillServed :: [StoredVersion] -> [Condemned] -> [Version]
stillServed stored selected =
    [ storedVersion version
    | version <- stored
    , storedPresence version == VersionServed
    , Set.notMember (renderVersion (storedVersion version)) condemnedKeys
    ]
  where
    condemnedKeys = Set.fromList (map (renderVersion . cdVersion) selected)

{- A preview charges the cap once per distinct version, however many of the mount's stores hold it,
and counts past the cap rather than halting, so the closing tally names the full reach. -}
chargePreview :: SweepPacing -> SweepPorts -> SweepState -> [Condemned] -> IO ()
chargePreview pacing ports counters logical = do
    issued <- readIORef (stIssued counters)
    let reached = issued + length logical
        cap = swpDeletionCap pacing
        etag = cdAdvisoryEtag =<< listToMaybe (drop (cap - issued - 1) logical)
    writeIORef (stIssued counters) reached
    when (issued < cap && reached >= cap) (announceCap ports cap reached etag)
    traverse_ (const (recordTally counters (reportRemoval (sweepReport ports)))) logical

-- | The grouped executor reuses the complete evaluator with fresh context for every backend batch.
sweepPackageGroup :: SweepPacing -> SweepPorts -> SweepState -> SweepMount -> PackageName -> [(SweepStore, [StoredVersion])] -> IO (Maybe CycleHalt)
sweepPackageGroup pacing ports counters mount name =
    deleteGroup pacing ports counters mount name select (\located -> recordOutcome located counters name)
  where
    select counting located stored = do
        ctx <- mkEvalContext (sweepNow ports) (sweepAdvisoryEtag ports (smEcosystem mount))
        let store = ssObserve (smStore located)
            targetPorts = locatedPorts mount store ports
        selected <- selectPackage counting targetPorts counters located ctx name stored >>= stillEligible targetPorts counters located name
        pure [Selection (cdVersion item) (condemnationMessage ports name item) (cdAdvisoryEtag item) | item <- selected]

selectPackage :: Bool -> SweepPorts -> SweepState -> SweepMount -> EvalContext -> PackageName -> [StoredVersion] -> IO [Condemned]
selectPackage counting ports counters mount ctx name stored
    | smFirstParty mount name = [] <$ when counting (traverse_ (const (record ports counters SweepGuardSkipped)) served)
    | null served = pure []
    | otherwise =
        obReadManifest (ssObserve (smStore mount)) name >>= \case
            Left fault -> do
                recordGap counters unreadManifest
                announceUnread ports name served fault
                decideAll (identityEvidence name)
            Right manifest -> decideAll (evidenceIn name manifest)
  where
    served = [storedVersion s | s <- stored, storedPresence s == VersionServed]
    decideAll evidence = catMaybes <$> traverse (decideVersion counting ports counters mount ctx evidence) served

{- The manifest's own entry for a version, or identity alone where it projects none. A listing can
name a version the manifest omits, and identity is established either way. -}
evidenceIn :: PackageName -> Manifest -> Version -> RuleEvidence
evidenceIn name manifest version =
    maybe (identityEvidence name version) completeEvidence (selectVersion version (manifestInfo manifest))

{- Decide one version from whatever evidence it has and count it. Only a named decisive deny
condemns, so this runs 'evalRules' rather than the wrapper that folds in deny-by-default. -}
decideVersion ::
    Bool ->
    SweepPorts ->
    SweepState ->
    SweepMount ->
    EvalContext ->
    (Version -> RuleEvidence) ->
    Version ->
    IO (Maybe Condemned)
decideVersion counting ports counters mount ctx evidence version = do
    when counting (record ports counters SweepExamined)
    evalRules ctx (smRules mount) (evidence version) >>= \case
        Blocked rule etag reason -> pure (Just Condemned{cdVersion = version, cdRule = rule, cdAdvisoryEtag = etag, cdReason = reason})
        _ -> when counting (record ports counters SweepKept) $> Nothing

{- The push can stop being eligible evidence between a version's decision and this hand-over,
across a long manifest read or a batch, and a delete is permanent. So it is read again here. -}
stillEligible :: SweepPorts -> SweepState -> SweepMount -> PackageName -> [Condemned] -> IO [Condemned]
stillEligible ports counters mount name condemned =
    rdAdvisoryFreshness (smRuleDeps mount) >>= \freshness -> case renderIneligible freshness of
        Nothing -> pure condemned
        Just why -> do
            let advisoryRules = [ruleName r | r <- smConfigured mount, readsAdvisories r]
                (withheld, keeping) = partition ((`elem` advisoryRules) . cdRule) condemned
            unless (null withheld) $ do
                traverse_ (const (record ports counters SweepGuardSkipped)) withheld
                announceIneligible ports name why withheld
            pure keeping

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

announceKept :: SweepPorts -> PackageName -> [Version] -> IO ()
announceKept ports name =
    traverse_
        ( \version ->
            auditInfo
                (sweepAudit ports)
                ("dry run, keeping " <> renderPackageName name <> "@" <> renderVersion version)
        )

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

-- Where a halting run would have stopped, for a run that carries on past the cap instead.
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

announce :: SweepPorts -> PackageName -> Condemned -> IO ()
announce ports name condemned = auditInfo (sweepAudit ports) (condemnationMessage ports name condemned)

{- Every deletion's audit line: the package, the version, the rule that denied it, and the
advisory generation pinned when it was decided. -}
condemnationMessage :: SweepPorts -> PackageName -> Condemned -> Text
condemnationMessage ports name condemned =
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

{- What the backend reported for one version. A refusal or an unreached call leaves the version
in the store, so it counts as kept and reports for an operator to follow up. -}
recordOutcome :: SweepPorts -> SweepState -> PackageName -> (Version, VersionOutcome) -> IO ()
recordOutcome ports counters name (version, outcome) = case outcome of
    VersionRemoved -> record ports counters removal
    VersionRemoving reference -> do
        -- No backend completes a removal later, so the next cycle's listing settles it: a version
        -- still served is decided and deleted again, which is idempotent.
        auditInfo (sweepAudit ports) (subject <> ": the backend is removing it under " <> reference)
        record ports counters removal
    VersionRefused refusal -> do
        auditError
            (sweepAudit ports)
            (subject <> ": the backend refused the delete, " <> refusalCode refusal <> ": " <> refusalDetail refusal)
        record ports counters SweepKept
    VersionUncertain fault -> do
        auditError (sweepAudit ports) (subject <> ": deletion outcome is uncertain: " <> renderStoreFault fault)
        record ports counters SweepKept
    VersionUnreached fault -> do
        auditError (sweepAudit ports) (subject <> ": the delete did not reach the backend: " <> renderStoreFault fault)
        record ports counters SweepKept
  where
    subject = renderPackageName name <> "@" <> renderVersion version
    removal = reportRemoval (sweepReport ports)
