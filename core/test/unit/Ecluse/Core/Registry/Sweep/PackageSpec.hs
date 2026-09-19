-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Unit checks for package sweep verdicts and deletion limits.
A mutable store exposes removals and first-party protection.
-}
module Ecluse.Core.Registry.Sweep.PackageSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian, nominalDay)
import Test.Hspec

import Ecluse.Core.Cve (AdvisoryRange (AdvisoryRange))
import Ecluse.Core.Cve.Types (DbEtag (DbEtag))
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Osv.Types (UpperBound (FixedBefore))
import Ecluse.Core.Registry.Maintenance (
    StoreFacts (factDeleteCeiling),
    StoreMaintenance (deleteVersions, readStoreManifest, storeFacts),
    StoredVersion (StoredVersion),
    VersionOutcome (VersionRefused, VersionRemoved, VersionUnreached),
    VersionPresence (VersionWithdrawn),
    chunksOfCeiling,
    deleteAll,
    protocolFault,
    storeRefusal,
 )
import Ecluse.Core.Registry.Metadata (Manifest)
import Ecluse.Core.Registry.Sweep.Outcome (
    CycleHalt (HaltDeletionCap),
    EvidenceGaps (gapManifests),
    evidenceComplete,
 )
import Ecluse.Core.Registry.Sweep.Package (previewPackageGroup, sweepPackageGroup)
import Ecluse.Core.Registry.Sweep.Types (
    SweepExecution (SweepCounts, SweepRemoves),
    SweepMount (smConfigured, smFirstParty, smRuleDeps, smStore),
    SweepPacing (swpDeletionCap),
    SweepPorts (sweepAdvisoryEtag, sweepNow),
    SweepState (stEvidence, stIssued),
    SweepStore (ssExecute, ssObserve),
    newSweepState,
 )
import Ecluse.Core.Rules (PreparedRule (prepEval), RuleDeps (rdAdvisoryFreshness, rdWithCveLookup), prepare)
import Ecluse.Core.Rules.Freshness (
    AdvisoryFreshness (AdvisoryFresh, AdvisoryUndated),
    AdvisoryPublication (PublishedAt),
    assessAdvisoryAge,
    maxAdvisoryAgeFor,
 )
import Ecluse.Core.Rules.Types (
    DenyIfCveParams (DenyIfCveParams),
    FailureAlignment (FailDeny),
    PrecededRule (PrecededRule),
    Rule (AllowByIdentity, AllowIfOlderThan, DenyByIdentity, DenyIfCve),
    RuleVerdict (Deny),
    mkEvalContext,
 )
import Ecluse.Core.Telemetry.Metrics (SweepResult (..))
import Ecluse.Core.Version (Version)
import Ecluse.Test.Cve (fakeCveLookup)
import Ecluse.Test.Maintenance (FakeStore (fakeMaintenance, fakeObservation), FakeStoreConfig (..), heldVersions, newFakeStore, seededStoreConfig, servedVersions)
import Ecluse.Test.Package (leftPadName, npmVersion, sampleManifest)
import Ecluse.Test.Rules (admitRule, atDefaultPrecedence, cannotVetRule, denyRule, inertRuleDeps)
import Ecluse.Test.Sweep (RecordedSweep (..), previewMount, previewingReport, recordingPorts, recordingPortsUnder, testMount, testPacing)

epoch :: UTCTime
epoch = UTCTime (fromGregorian 2026 1 1) 0

-- | Verify each sweep outcome against the store's remaining versions.
spec :: Spec
spec = do
    verdictSpec
    identityOnlySpec
    beltSpec
    outcomeSpec
    capSpec
    dryRunSpec
    expirySpec
    generationCapSpec

{- Only a named decisive deny deletes. Deny by default and a rule that could not vet both keep,
because the store may hold the only surviving copy. -}
verdictSpec :: Spec
verdictSpec = describe "the delete verdict" $ do
    it "deletes a version a named decisive deny condemns" $ do
        (rec', store) <- sweepOne [denyRule] ["1.0.0"] ["1.0.0"]
        recResults rec' `shouldReturn` [SweepExamined, SweepDeleted]
        held store `shouldReturn` []

    -- The serve path refuses an undecided version. Here each of these keeps it instead.
    for_
        [ ("keeps a version deny-by-default left undecided", [])
        , ("keeps a version a rule admitted", [admitRule])
        , ("keeps a version no rule could vet", [cannotVetRule])
        ]
        $ \(caseName, rules) ->
            it caseName $ do
                (rec', store) <- sweepOne rules ["1.0.0"] ["1.0.0"]
                recResults rec' `shouldReturn` [SweepExamined, SweepKept]
                held store `shouldReturn` [npmVersion "1.0.0"]

    it "deletes a version the manifest omits but a deny names by identity, so the next request 404s" $ do
        -- The store lists it and its own metadata does not. The listing establishes identity anyway.
        rules <- identityDeny
        store <- storeWith [npmVersion "1.0.0"] (Just (sampleManifest leftPadName []))
        rec' <- recordingPorts generation
        void (runStep rec' testPacing (mount store rules) (served ["1.0.0"]))
        recResults rec' `shouldReturn` [SweepExamined, SweepDeleted]
        held store `shouldReturn` []

    it "keeps a version the manifest omits when no rule is decisive, so the next request serves it" $ do
        (rec', store) <- sweepOne [] ["1.0.0"] []
        recResults rec' `shouldReturn` [SweepExamined, SweepKept]
        held store `shouldReturn` [npmVersion "1.0.0"]

    it "never decides a version the store lists but no longer serves" $ do
        -- A backend keeps listing a deleted version, so a sweep blind to this would re-issue a
        -- destructive call for it every cycle.
        store <- storeWith [] (Just (sampleManifest leftPadName [npmVersion "1.0.0"]))
        rec' <- recordingPorts generation
        halt <- runStep rec' testPacing (mount store [denyRule]) [StoredVersion (npmVersion "1.0.0") VersionWithdrawn Nothing]
        halt `shouldBe` Nothing
        recResults rec' `shouldReturn` []

{- A read that produced no manifest decides on the identity the listing carries. The shared bounded
fetch discards the response status, so a 404 and a 5xx arrive here alike. -}
identityOnlySpec :: Spec
identityOnlySpec = describe "a manifest the store did not serve" $ do
    it "deletes a version the operator revoked by identity, so the next request 404s" $ do
        rules <- identityDeny
        store <- storeWith [npmVersion "1.0.0"] Nothing
        rec' <- recordingPorts generation
        halt <- runStep rec' testPacing (mount store rules) (served ["1.0.0"])
        halt `shouldBe` Nothing
        recResults rec' `shouldReturn` [SweepExamined, SweepDeleted]
        held store `shouldReturn` []

    it "keeps a version a higher-precedence allow admits, so the next request still serves it" $ do
        rules <-
            prepare
                inertRuleDeps
                [PrecededRule 500 (AllowByIdentity "left-pad@1.0.0"), atDefaultPrecedence (DenyByIdentity "left-pad@1.0.0")]
        (rec', store) <- unreadStep rules ["1.0.0"]
        recResults rec' `shouldReturn` [SweepExamined, SweepKept]
        held store `shouldReturn` [npmVersion "1.0.0"]

    it "keeps a version an earlier rule could not decide, so the next request still serves it" $ do
        -- The age rule reads a publish time no listing carries, so the fold stops above the deny
        -- rather than letting it delete past an unresolved rule.
        rules <-
            prepare
                inertRuleDeps
                [ PrecededRule 500 (AllowIfOlderThan (7 * nominalDay))
                , atDefaultPrecedence (DenyByIdentity "left-pad@1.0.0")
                ]
        (rec', store) <- unreadStep rules ["1.0.0"]
        recResults rec' `shouldReturn` [SweepExamined, SweepKept]
        held store `shouldReturn` [npmVersion "1.0.0"]

    it "keeps every version when no rule is decisive, so the next request still serves them" $ do
        (rec', store) <- unreadStep [] ["1.0.0", "2.0.0"]
        recResults rec' `shouldReturn` [SweepExamined, SweepKept, SweepExamined, SweepKept]
        held store `shouldReturn` map npmVersion ["1.0.0", "2.0.0"]

    it "names the package and the fault on the line an operator acts on, having kept the versions" $ do
        (rec', _) <- unreadStep [] ["1.0.0"]
        errors <- recErrors rec'
        errors `shouldSatisfy` any (T.isInfixOf "decided on identity alone")

    it "records the gap it decided across, so a count taken here reads as partial" $ do
        store <- storeWith [npmVersion "1.0.0"] Nothing
        rec' <- recordingPorts generation
        gaps <- stepEvidence rec' (mount store []) (served ["1.0.0"])
        gapManifests gaps `shouldBe` 1
        evidenceComplete gaps `shouldBe` False

    it "keeps a version the backend refused to delete, so the next request still serves it" $ do
        rules <- identityDeny
        store <- refusingStore' Nothing (VersionRefused (storeRefusal "ACCESS_DENIED" "the identity may not delete"))
        rec' <- recordingPorts generation
        halt <- runStep rec' testPacing (mount store rules) (served ["1.0.0"])
        halt `shouldBe` Nothing
        recResults rec' `shouldReturn` [SweepExamined, SweepKept]
        errors <- recErrors rec'
        errors `shouldSatisfy` any (T.isInfixOf "ACCESS_DENIED")

    it "reaches the deletion cap from a read that produced no manifest, so the held-back version serves" $ do
        -- Identity alone can now condemn, so this branch counts against the cycle's cap like any
        -- other and latches the halt when it fills it.
        rules <- prepare inertRuleDeps (map atDefaultPrecedence [DenyByIdentity "left-pad@1.0.0", DenyByIdentity "left-pad@2.0.0"])
        store <- storeWith (map npmVersion ["1.0.0", "2.0.0"]) Nothing
        rec' <- recordingPorts generation
        halt <- runStep rec' testPacing{swpDeletionCap = 1} (mount store rules) (served ["1.0.0", "2.0.0"])
        case halt of
            Just (HaltDeletionCap cap issued _) -> (cap, issued) `shouldBe` (1, 1)
            other -> expectationFailure ("expected the cap halt, got: " <> show other)
        held store `shouldReturn` [npmVersion "2.0.0"]

beltSpec :: Spec
beltSpec = describe "the first-party belt" $
    it "serves an identity-denied first-party version until the guard is removed, then 404s" $ do
        store <- storeWith [npmVersion "1.0.0"] (Just (sampleManifest leftPadName [npmVersion "1.0.0"]))
        manifestReads <- newIORef (0 :: Int)
        rules <- identityDeny
        rec' <- recordingPorts generation
        let handle = fakeMaintenance store
            tracked =
                store
                    { fakeMaintenance =
                        handle{readStoreManifest = \name -> modifyIORef' manifestReads (+ 1) >> readStoreManifest handle name}
                    }
            shielded = (mount tracked rules){smFirstParty = (== leftPadName)}
        halt <- runStep rec' testPacing shielded (served ["1.0.0"])
        halt `shouldBe` Nothing
        recResults rec' `shouldReturn` [SweepGuardSkipped]
        readIORef manifestReads `shouldReturn` 0
        held store `shouldReturn` [npmVersion "1.0.0"]
        unshielded <- recordingPorts generation
        runStep unshielded testPacing (mount tracked rules) (served ["1.0.0"]) `shouldReturn` Nothing
        recResults unshielded `shouldReturn` [SweepExamined, SweepDeleted]
        {- The belt reads nothing at all. Without it the package's metadata is read to decide the
        version, and again to reassess it against current evidence before the delete leaves. -}
        readIORef manifestReads `shouldReturn` 2
        held store `shouldReturn` []

{- A refused or unreached delete leaves the version in the store, so it counts as kept and reports
the backend's own code for an operator to follow up. -}
outcomeSpec :: Spec
outcomeSpec = describe "what the backend reported" $ do
    it "counts a refused delete as kept, with the backend's code on an error line" $ do
        rec' <- recordingPorts generation
        store <- refusingStore (VersionRefused (storeRefusal "ACCESS_DENIED" "the identity may not delete"))
        halt <- runStep rec' testPacing (mount store [denyRule]) (served ["1.0.0"])
        halt `shouldBe` Nothing
        recResults rec' `shouldReturn` [SweepExamined, SweepKept]
        errors <- recErrors rec'
        errors `shouldSatisfy` any (T.isInfixOf "ACCESS_DENIED")

    it "counts a delete that never reached the backend as kept, with the fault" $ do
        rec' <- recordingPorts generation
        store <- refusingStore (VersionUnreached (protocolFault "the store never answered"))
        void (runStep rec' testPacing (mount store [denyRule]) (served ["1.0.0"]))
        recResults rec' `shouldReturn` [SweepExamined, SweepKept]
        errors <- recErrors rec'
        errors `shouldSatisfy` any (T.isInfixOf "did not reach the backend")

    it "counts a delete the backend is still doing as deleted, and never awaits it" $ do
        -- The fake reports a late-finishing delete, which is the arm no backend takes today.
        -- The next cycle's listing shows whether it finished, and a repeat delete is idempotent.
        (rec', _) <- sweepOne [denyRule] ["1.0.0"] ["1.0.0"]
        recResults rec' `shouldReturn` [SweepExamined, SweepDeleted]
        info <- recInfo rec'
        info `shouldSatisfy` any (T.isInfixOf "the backend is removing it under")

capSpec :: Spec
capSpec = describe "the per-cycle deletion cap" $ do
    for_ [0, 1] $ \successful ->
        it ("hands the selected batch over once and charges unreached versions after " <> show successful <> " successes") $ do
            let versions = map npmVersion ["1.0.0", "2.0.0", "3.0.0"]
                fault = protocolFault "the store never answered"
            store <- storeWith versions (Just (sampleManifest leftPadName versions))
            calls <- newIORef []
            rec' <- recordingPorts generation
            counters <- newSweepState
            let original = fakeMaintenance store
                handle =
                    original
                        { deleteVersions = \checks _ selected ->
                            deleteAll
                                checks
                                ( \batch -> do
                                    modifyIORef' calls (<> [batch])
                                    pure (Right (zip batch (replicate successful VersionRemoved <> repeat (VersionUnreached fault))))
                                )
                                (chunksOfCeiling (factDeleteCeiling (storeFacts original)) selected)
                        }
            halt <-
                stepUnder
                    rec'
                    testPacing{swpDeletionCap = 2}
                    counters
                    (testMount handle [denyRule] [])
                    (served ["1.0.0", "2.0.0", "3.0.0"])
            readIORef calls `shouldReturn` [take 2 versions]
            readIORef (stIssued counters) `shouldReturn` 2
            halt `shouldBe` Just (HaltDeletionCap 2 2 Nothing)
            recResults rec'
                `shouldReturn` (replicate 3 SweepExamined <> [SweepGuardSkipped] <> replicate successful SweepDeleted <> replicate (2 - successful) SweepKept)

    it "hands over what the cap allows, holds the rest back, and halts" $ do
        store <- storeWith (map npmVersion ["1.0.0", "2.0.0"]) (Just (sampleManifest leftPadName (map npmVersion ["1.0.0", "2.0.0"])))
        rec' <- recordingPorts generation
        halt <- runStep rec' testPacing{swpDeletionCap = 1} (mount store [denyRule]) (served ["1.0.0", "2.0.0"])
        case halt of
            Just (HaltDeletionCap cap issued etag) -> (cap, issued, etag) `shouldBe` (1, 1, Nothing)
            other -> expectationFailure ("expected the cap halt, got: " <> show other)
        recResults rec'
            `shouldReturn` [SweepExamined, SweepExamined, SweepGuardSkipped, SweepDeleted]
        held store `shouldReturn` [npmVersion "2.0.0"]

    it "latches on reaching the cap even when nothing was held back" $ do
        -- The breaker is the count handed over, not whether this package had more to give, so a
        -- cycle that fills the cap exactly still stops.
        store <- storeWith [npmVersion "1.0.0"] (Just (sampleManifest leftPadName [npmVersion "1.0.0"]))
        rec' <- recordingPorts generation
        halt <- runStep rec' testPacing{swpDeletionCap = 1} (mount store [denyRule]) (served ["1.0.0"])
        case halt of
            Just (HaltDeletionCap cap issued _) -> (cap, issued) `shouldBe` (1, 1)
            other -> expectationFailure ("expected the cap halt, got: " <> show other)

    it "does not halt a package that left the cap unreached" $ do
        (rec', _) <- sweepOne [denyRule] ["1.0.0"] ["1.0.0"]
        recErrors rec' `shouldReturn` []

{- A dry run holds the store's observing calls and an execution that counts, so this module cannot
delete because nothing it is given can. The cap only logs. -}
dryRunSpec :: Spec
dryRunSpec = describe "a dry run" $ do
    it "counts what it would delete under its own arm and deletes nothing" $ do
        (rec', store) <- previewOne testPacing ["1.0.0"]
        recResults rec' `shouldReturn` [SweepExamined, SweepWouldDelete]
        held store `shouldReturn` [npmVersion "1.0.0"]

    it "says it would delete rather than that it is deleting" $ do
        (rec', _) <- previewOne testPacing ["1.0.0"]
        info <- recInfo rec'
        info `shouldSatisfy` any (T.isInfixOf "dry run, would delete")

    it "counts the full reach past the cap and never halts on it" $ do
        -- The cap is the breaker on real deletions, so under a preview it only logs: an operator
        -- reads the whole count a real run would reach rather than a count that stopped at one.
        (rec', store) <- previewOne testPacing{swpDeletionCap = 1} ["1.0.0", "2.0.0"]
        recResults rec'
            `shouldReturn` [SweepExamined, SweepExamined, SweepWouldDelete, SweepWouldDelete]
        held store `shouldReturn` map npmVersion ["1.0.0", "2.0.0"]

    it "reports once where a run that halts on the cap would have stopped" $ do
        -- The line is what an operator sizes the cap from ahead of the first real sweep, so it
        -- names the cap and the count, and it is written at the crossing rather than per version.
        (rec', _) <- previewOne testPacing{swpDeletionCap = 1} ["1.0.0", "2.0.0"]
        info <- recInfo rec'
        filter (T.isInfixOf "deletion cap") info
            `shouldSatisfy` \lines' -> length lines' == 1 && all (T.isInfixOf "handed over 2 versions") lines'

{- One package's step under a preview's report, over a mount holding the store's observing calls
alone. Nothing here asks which run it is in: the execution it was handed is what differs. -}
previewOne :: SweepPacing -> [Text] -> IO (RecordedSweep, FakeStore)
previewOne pacing stored = do
    store <- storeWith (map npmVersion stored) (Just (sampleManifest leftPadName (map npmVersion stored)))
    rec' <- recordingPortsUnder previewingReport generation
    void (runStep rec' pacing (previewMount (fakeObservation store) [denyRule] []) (served stored))
    pure (rec', store)

-- One package's step over a store that serves no metadata at all for it.
unreadStep :: [PreparedRule] -> [Text] -> IO (RecordedSweep, FakeStore)
unreadStep rules stored = do
    store <- storeWith (map npmVersion stored) Nothing
    rec' <- recordingPorts generation
    halt <- runStep rec' testPacing (mount store rules) (served stored)
    halt `shouldBe` Nothing
    pure (rec', store)

-- The operator's own revocation of one exact version, which identity alone decides.
identityDeny :: IO [PreparedRule]
identityDeny = prepare inertRuleDeps [atDefaultPrecedence (DenyByIdentity "left-pad@1.0.0")]

-- One package's step over a store seeded with those versions and a manifest carrying those.
sweepOne :: [PreparedRule] -> [Text] -> [Text] -> IO (RecordedSweep, FakeStore)
sweepOne rules stored inManifest = do
    store <- storeWith (map npmVersion stored) (Just (sampleManifest leftPadName (map npmVersion inManifest)))
    rec' <- recordingPorts generation
    void (runStep rec' testPacing (mount store rules) (served stored))
    pure (rec', store)

-- One package's step, keeping what the cycle could not read rather than the halt it did not raise.
stepEvidence :: RecordedSweep -> SweepMount -> [StoredVersion] -> IO EvidenceGaps
stepEvidence rec' mount' stored = do
    counters <- newSweepState
    void (stepUnder rec' testPacing counters mount' stored)
    readIORef (stEvidence counters)

runStep :: RecordedSweep -> SweepPacing -> SweepMount -> [StoredVersion] -> IO (Maybe CycleHalt)
runStep rec' pacing mount' stored = newSweepState >>= \counters -> stepUnder rec' pacing counters mount' stored

{- One package's step at the mount's own store, under the execution the boot handed it. The
dispatch is the one a cycle makes: a preview counts its selections and a real run hands them over.
-}
stepUnder :: RecordedSweep -> SweepPacing -> SweepState -> SweepMount -> [StoredVersion] -> IO (Maybe CycleHalt)
stepUnder rec' pacing counters mount' stored = case ssExecute (smStore mount') of
    SweepRemoves _ -> sweepPackageGroup pacing ports counters mount' leftPadName [(smStore mount', stored)]
    SweepCounts -> do
        ctx <- mkEvalContext (sweepNow ports) (sweepAdvisoryEtag ports Npm)
        previewPackageGroup pacing ports counters mount' ctx leftPadName [(ssObserve (smStore mount'), stored)]
  where
    ports = recPorts rec'

-- A store holding those versions, serving that manifest, or serving none at all.
storeWith :: [Version] -> Maybe Manifest -> IO FakeStore
storeWith stored manifest =
    newFakeStore
        (seededStoreConfig [(leftPadName, stored)])
            { fakeManifests = maybe Map.empty (Map.singleton leftPadName) manifest
            }

{- A store whose delete reports the given outcome and changes nothing, so the refusal and the
unreached arms are both drivable without a fault that would stop the whole cycle. -}
refusingStore :: VersionOutcome -> IO FakeStore
refusingStore = refusingStore' (Just (sampleManifest leftPadName [npmVersion "1.0.0"]))

-- | As 'refusingStore', over the given manifest, so the identity-only path drives the same arms.
refusingStore' :: Maybe Manifest -> VersionOutcome -> IO FakeStore
refusingStore' manifest outcome = do
    store <- storeWith [npmVersion "1.0.0"] manifest
    let handle = fakeMaintenance store
    pure store{fakeMaintenance = handle{deleteVersions = \_ _ versions -> pure [(v, outcome) | v <- versions]}}

mount :: FakeStore -> [PreparedRule] -> SweepMount
mount store rules = testMount (fakeMaintenance store) rules []

held :: FakeStore -> IO [Version]
held = heldVersions leftPadName

served :: [Text] -> [StoredVersion]
served = servedVersions . map npmVersion

generation :: Maybe DbEtag
generation = Just (DbEtag "etag-1")

{- An expired advisory push is not authority to delete. The rule refuses rather than denying, and
a push that expires after the verdict is read again before the batch leaves. -}
expirySpec :: Spec
expirySpec = describe "an expired advisory push" $ do
    it "keeps a version an affecting advisory would have condemned" $ do
        (rec', store) <- advisorySweep (pure expiredReading)
        recResults rec' `shouldReturn` [SweepExamined, SweepKept]
        held store `shouldReturn` [npmVersion "1.0.0"]

    it "spares a version when the push expires between the verdict and the hand-over" $ do
        crossing <- newIORef [AdvisoryFresh]
        (rec', store) <- advisorySweep (nextReading expiredReading crossing)
        recResults rec' `shouldReturn` [SweepExamined, SweepGuardSkipped]
        held store `shouldReturn` [npmVersion "1.0.0"]

    it "withholds an advisory-named condemnation on a generation with no publication time" $ do
        crossing <- newIORef [AdvisoryFresh]
        (rec', store) <- advisorySweep (nextReading AdvisoryUndated crossing)
        recResults rec' `shouldReturn` [SweepExamined, SweepGuardSkipped]
        held store `shouldReturn` [npmVersion "1.0.0"]

    it "withholds rather than counting it, on a run whose execution only counts" $ do
        -- A preview reaches no delete, so this is the arm where a stale condemnation would
        -- otherwise be reported as a would-delete an operator sizes a real run from.
        store <- storeWith [npmVersion "1.0.0"] (Just (sampleManifest leftPadName [npmVersion "1.0.0"]))
        crossing <- newIORef [AdvisoryFresh]
        let deps = advisoryDeps (nextReading expiredReading crossing)
        rules <- prepare deps [atDefaultPrecedence denyCveRule]
        rec' <- recordingPortsUnder previewingReport generation
        let swept = (previewMount (fakeObservation store) rules []){smRuleDeps = deps, smConfigured = [denyCveRule]}
        void (runStep rec' testPacing swept (served ["1.0.0"]))
        recResults rec' `shouldReturn` [SweepExamined, SweepGuardSkipped]
        errors <- recErrors rec'
        errors `shouldSatisfy` any (T.isInfixOf "stay in the store, because the advisory push is")
        held store `shouldReturn` [npmVersion "1.0.0"]

    it "lets an identity deny act, because it reads no advisory database" $ do
        store <- storeWith [npmVersion "1.0.0"] (Just (sampleManifest leftPadName [npmVersion "1.0.0"]))
        let deps = advisoryDeps (pure expiredReading)
        rules <- prepare deps [atDefaultPrecedence (DenyByIdentity "left-pad@1.0.0")]
        rec' <- recordingPorts generation
        let swept = (mount store rules){smRuleDeps = deps, smConfigured = [DenyByIdentity "left-pad@1.0.0", denyCveRule]}
        void (runStep rec' testPacing swept (served ["1.0.0"]))
        recResults rec' `shouldReturn` [SweepExamined, SweepDeleted]
        held store `shouldReturn` []

-- One package swept by the real advisory deny over a database that affects its only version.
advisorySweep :: IO AdvisoryFreshness -> IO (RecordedSweep, FakeStore)
advisorySweep freshness = do
    store <- storeWith [npmVersion "1.0.0"] (Just (sampleManifest leftPadName [npmVersion "1.0.0"]))
    let deps = advisoryDeps freshness
    rules <- prepare deps [atDefaultPrecedence denyCveRule]
    rec' <- recordingPorts generation
    let swept = (mount store rules){smRuleDeps = deps, smConfigured = [denyCveRule]}
    void (runStep rec' testPacing swept (served ["1.0.0"]))
    pure (rec', store)

-- Capabilities whose database affects the fixture version, under the given push-age reading.
advisoryDeps :: IO AdvisoryFreshness -> RuleDeps
advisoryDeps freshness =
    inertRuleDeps
        { rdWithCveLookup = \use -> use (Just (DbEtag "etag-1", fakeCveLookup [("left-pad", affectingRange)]))
        , rdAdvisoryFreshness = freshness
        }

affectingRange :: AdvisoryRange
affectingRange = AdvisoryRange "GHSA-affect-0001" (Just 9.8) (Just "0") (FixedBefore "2.0.0") Nothing

denyCveRule :: Rule
denyCveRule = DenyIfCve (DenyIfCveParams 8.0 FailDeny)

-- Take the next queued reading, then hold the last, so a case drives one crossing and no more.
nextReading :: AdvisoryFreshness -> IORef [AdvisoryFreshness] -> IO AdvisoryFreshness
nextReading afterwards queued = atomicModifyIORef' queued $ \case
    [] -> ([], afterwards)
    (next : rest) -> (rest, next)

-- A push three days past the six-day maximum a seven-day quarantine derives.
expiredReading :: AdvisoryFreshness
expiredReading =
    assessAdvisoryAge
        (maxAdvisoryAgeFor Nothing [AllowIfOlderThan (7 * nominalDay)])
        epoch
        (PublishedAt (addUTCTime (negate (9 * nominalDay)) epoch))

generationCapSpec :: Spec
generationCapSpec = describe "the generation that reaches the cap" $ do
    for_ [False, True] $ \preview ->
        it ("credits the middle selected denial with an existing charge, preview=" <> show preview) $ do
            let versions = ["1.0.0", "2.0.0", "3.0.0"]
                generations = map (Just . DbEtag) ["first", "threshold", "last"]
            store <- storeWith (map npmVersion versions) (Just (sampleManifest leftPadName (map npmVersion versions)))
            queuedGenerations <- newIORef generations
            -- The queue cycles, because the grouped executor reassesses every version before it
            -- hands the batch over, and each pass acquires the same evidence in the same order.
            let deciding =
                    denyRule
                        { prepEval = \_ _ -> do
                            etag <- atomicModifyIORef' queuedGenerations (\case [] -> ([], Nothing); item : rest -> (rest <> [item], item))
                            pure (Deny etag "acquired advisory evidence")
                        }
                swept =
                    if preview
                        then previewMount (fakeObservation store) [deciding] []
                        else mount store [deciding]
            rec' <- if preview then recordingPortsUnder previewingReport generation else recordingPorts generation
            counters <- newSweepState
            writeIORef (stIssued counters) 1
            halt <- stepUnder rec' testPacing{swpDeletionCap = 3} counters swept (served versions)
            halt `shouldBe` if preview then Nothing else Just (HaltDeletionCap 3 3 (Just (DbEtag "threshold")))
            readIORef (stIssued counters) `shouldReturn` if preview then 4 else 3
            info <- recInfo rec'
            let deletions = filter (T.isInfixOf "blocked by") info
            length deletions `shouldBe` if preview then 3 else 2
            zipWith T.isInfixOf ["first", "threshold", "last"] deletions
                `shouldSatisfy` and
            when preview $
                filter (T.isInfixOf "deletion cap") info
                    `shouldSatisfy` (\lines' -> length lines' == 1 && all (T.isInfixOf "threshold") lines')

    it "credits no advisory when an identity denial reaches the cap" $ do
        store <- storeWith [npmVersion "1.0.0"] (Just (sampleManifest leftPadName [npmVersion "1.0.0"]))
        rules <- identityDeny
        rec' <- recordingPorts generation
        runStep rec' testPacing{swpDeletionCap = 1} (mount store rules) (served ["1.0.0"])
            `shouldReturn` Just (HaltDeletionCap 1 1 Nothing)
        info <- recInfo rec'
        filter (T.isInfixOf "blocked by") info `shouldSatisfy` all (T.isInfixOf "advisory generation none")

    it "selects the threshold after withholding stale advisory evidence" $ do
        let versions = ["1.0.0", "2.0.0"]
            configured = [DenyByIdentity "left-pad@2.0.0", denyCveRule]
        store <- storeWith (map npmVersion versions) (Just (sampleManifest leftPadName (map npmVersion versions)))
        freshness <- newIORef [AdvisoryFresh]
        let deps = advisoryDeps (nextReading expiredReading freshness)
        rules <- prepare deps (map atDefaultPrecedence configured)
        rec' <- recordingPorts generation
        let swept = (mount store rules){smRuleDeps = deps, smConfigured = configured}
        -- The cap admits both, so the recheck rather than the cap is what withholds the stale one.
        runStep rec' testPacing{swpDeletionCap = 2} swept (served versions)
            `shouldReturn` Nothing
        held store `shouldReturn` [npmVersion "1.0.0"]
        info <- recInfo rec'
        let deletions = filter (T.isInfixOf "blocked by") info
        length deletions `shouldBe` 1
        deletions `shouldSatisfy` all (\line -> T.isInfixOf "2.0.0" line && T.isInfixOf "advisory generation none" line)
