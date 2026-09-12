-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | One package, the sweep's work unit. Both cycle shapes decide a version the same way here.

The metadata comes from the store being dredged, never the public upstream: one manifest read per
package. A version the manifest omits, or a package whose read faulted, is decided on the identity
the listing establishes. A version is deleted only on a named decisive deny, because deletion is
permanent and the store may hold the only surviving copy. The mount's own execution decides what
becomes of a condemned version, so a preview reaches no delete because it holds none.
-}
module Ecluse.Core.Registry.Sweep.Package (
    sweepPackage,
) where

import Ecluse.Core.Cve (DbEtag)
import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Registry.Maintenance (
    StoreDeletion (dlDeleteVersions),
    StoreFacts (factDeleteCeiling),
    StoreFault,
    StoreObservation (obFacts, obReadManifest),
    StoredVersion (storedPresence, storedVersion),
    VersionOutcome (VersionRefused, VersionRemoved, VersionRemoving, VersionUnreached),
    VersionPresence (VersionServed),
    chunksOfCeiling,
    deleteAll,
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
    record,
    recordGap,
    renderGeneration,
    renderStoreFault,
    unreadManifest,
 )
import Ecluse.Core.Rules (RuleDeps (rdAdvisoryFreshness), evalRules, renderExpiredPush)
import Ecluse.Core.Rules.Freshness (AdvisoryAge, AdvisoryFreshness (AdvisoryAging, AdvisoryFresh, AdvisoryStale))
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
    Maybe DbEtag ->
    PackageName ->
    [StoredVersion] ->
    IO (Maybe CycleHalt)
sweepPackage pacing ports counters mount ctx etag name stored
    | smFirstParty mount name = Nothing <$ traverse_ (const (record ports counters SweepGuardSkipped)) served
    | otherwise =
        obReadManifest (ssObserve (smStore mount)) name >>= \case
            Left fault -> unreadable fault *> decideAll (identityEvidence name)
            Right manifest -> decideAll (evidenceIn name manifest)
  where
    served = [storedVersion s | s <- stored, storedPresence s == VersionServed]

    -- The versions below are decided on less than the whole rule set, so the cycle records the gap.
    unreadable fault = recordGap counters unreadManifest *> announceUnread ports name served fault

    decideAll evidence = do
        condemned <- catMaybes <$> traverse (decideVersion ports counters mount ctx evidence) served
        disposeOf pacing ports counters mount etag name condemned

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
        Blocked rule reason -> pure (Just Condemned{cdVersion = version, cdRule = rule, cdReason = reason})
        _ -> record ports counters SweepKept $> Nothing

{- Hand the condemned versions over, up to what the cycle's cap still allows. The cap counts
what was handed over rather than what came back, because the cap bounds destructive calls. -}
disposeOf ::
    SweepPacing ->
    SweepPorts ->
    SweepState ->
    SweepMount ->
    Maybe DbEtag ->
    PackageName ->
    [Condemned] ->
    IO (Maybe CycleHalt)
disposeOf pacing ports counters mount etag name decided
    | null decided = pure Nothing
    | otherwise = do
        condemned <- stillEligible ports counters mount name decided
        issued <- readIORef (stIssued counters)
        let allowance = max 0 (cap - issued)
            (taken, held) = splitAt (if capHalts then allowance else length condemned) condemned
            reached = issued + length taken
        traverse_ (const (record ports counters SweepGuardSkipped)) held
        unless (null taken) $ do
            traverse_ (announce ports etag name) taken
            writeIORef (stIssued counters) reached
            when (crossedCap issued reached) (announceCap ports cap reached etag)
            outcomes <- sendDeletes (smStore mount) name (map cdVersion taken)
            traverse_ (recordOutcome ports counters name) outcomes
        pure (cappedHalt pacing reached etag <$ guard (halts reached))
  where
    cap = swpDeletionCap pacing
    capHalts = reportCapHalts (sweepReport ports)

    -- Reaching the cap latches, whether or not this package had more to hand over.
    halts reached = capHalts && reached >= cap

    -- A run that counts past the cap says once where a run that halts on it would have stopped.
    crossedCap issued reached = not capHalts && issued < cap && reached >= cap

{- The push age can expire between a version's decision and this hand-over, across a long manifest
read or a batch, and a delete is permanent. So the advisory-named condemnations are read again. -}
stillEligible :: SweepPorts -> SweepState -> SweepMount -> PackageName -> [Condemned] -> IO [Condemned]
stillEligible ports counters mount name condemned =
    rdAdvisoryFreshness (smRuleDeps mount) >>= \case
        AdvisoryFresh -> pure condemned
        AdvisoryAging{} -> pure condemned
        AdvisoryStale observed -> do
            let (withheld, keeping) = partition (advisoryNamed mount . cdRule) condemned
            unless (null withheld) $ do
                traverse_ (const (record ports counters SweepGuardSkipped)) withheld
                announceExpired ports name observed withheld
            pure keeping

-- Whether a rule name credited to a condemnation is one of this mount's advisory-reading rules.
advisoryNamed :: SweepMount -> Text -> Bool
advisoryNamed mount credited = credited `elem` [ruleName r | r <- smConfigured mount, readsAdvisories r]

-- The versions the expired push spared, named so an operator sees what a recovered Pilot would act on.
announceExpired :: SweepPorts -> PackageName -> AdvisoryAge -> [Condemned] -> IO ()
announceExpired ports name observed withheld =
    auditError
        (sweepAudit ports)
        ( renderPackageName name
            <> ": "
            <> show (length withheld)
            <> " versions an advisory rule denied stay in the store, because "
            <> renderExpiredPush observed
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
announce :: SweepPorts -> Maybe DbEtag -> PackageName -> Condemned -> IO ()
announce ports etag name condemned =
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
            <> renderGeneration etag

{- Dispose of the batch the way this run's own execution does: through the store's delete, split
to the backend's ceiling, or counted where the run holds no delete to reach. -}
sendDeletes :: SweepStore -> PackageName -> [Version] -> IO [(Version, VersionOutcome)]
sendDeletes store name versions = case ssExecute store of
    SweepRemoves deletion ->
        deleteAll (fmap Right . dlDeleteVersions deletion name) (chunksOfCeiling ceiling' versions)
    -- The audit line above has already put the reach on record, so the count reads from it.
    SweepCounts -> pure [(version, VersionRemoved) | version <- versions]
  where
    ceiling' = factDeleteCeiling (obFacts (ssObserve store))

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
