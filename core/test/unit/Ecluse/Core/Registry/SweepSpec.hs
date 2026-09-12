-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cycle permissions, retries, traversal, and pacing through recorded store effects.
module Ecluse.Core.Registry.SweepSpec (spec) where

import Data.Conduit (yield)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Cve (AdvisoryRange (..), DbEtag (DbEtag))
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Fault (TransportCause (TransportTimeout), transportFault)
import Ecluse.Core.Osv.Types (UpperBound (Unbounded))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Registry.Maintenance (
    ConsentVerdict (ConsentGranted, ConsentWithheld),
    NamePrefix,
    RetryAdvice (RetryWorthwhile),
    StoreClass (StoreDestroyable, StorePreserved),
    StoreCursor (writeCursor),
    StoreFacts (factNameAlphabet),
    StoreFault (StoreFault, faultRetry, faultTransport),
    StoreMaintenance (classifyStore, enumerateVersions, listPackagesIn, storeCursor, verifyConsent),
    StoreObservation (obVerifyConsent),
    StoredVersion (StoredVersion),
    VersionPresence (VersionServed),
    inBucket,
    mkNameAlphabet,
    protocolFault,
    renderNamePrefix,
    storedVersion,
 )
import Ecluse.Core.Registry.Sweep (sweepCycle, withStoreRetry)
import Ecluse.Core.Registry.Sweep.Types (
    CycleHalt (HaltConsentWithheld, HaltDeletionCap, HaltStoreFault, HaltStorePreserved),
    CycleOutcome (outcomeEvidence, outcomeHalt, outcomePrerequisites, outcomeTally),
    EvidenceGaps (gapAdvisoryGeneration),
    PrerequisiteStatus (PrerequisiteMet, PrerequisiteUnmet, PrerequisiteUnread),
    SweepMount (smConfigured, smFirstParty, smRuleDeps),
    SweepPacing (swpChunkPause, swpChunkSize, swpDeletionCap, swpShape),
    SweepPorts (sweepDelay),
    SweepShape (SweepCandidates, SweepEverything),
    SweepTally (tallyDeleted, tallyExamined, tallyGuardSkipped, tallyKept),
    TargetPrerequisites (tpClassification, tpConsent),
    outcomeComplete,
    prerequisitesMet,
 )
import Ecluse.Core.Rules (RuleDeps (rdWithCveLookup), prepare)
import Ecluse.Core.Rules.Types (DenyIfCveParams (..), DenyIfEpssParams (..), FailureAlignment (FailDeny), Rule (DenyByIdentity, DenyIfCve, DenyIfEpss))
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Cve (fakeCveLookup, unscoredEpssCases)
import Ecluse.Test.Maintenance (
    FakeStore (fakeMaintenance, fakeObservation, readFakeContents, readFakeCursor),
    FakeStoreConfig (..),
    defaultFakeStoreConfig,
    newFakeStore,
    withBucket,
 )
import Ecluse.Test.Package (sampleManifest)
import Ecluse.Test.Rules (atDefaultPrecedence, denyRule, inertRuleDeps)
import Ecluse.Test.Sweep (
    RecordedSweep (..),
    previewMount,
    previewingReport,
    recordingPorts,
    recordingPortsUnder,
    testMount,
    testPacing,
 )

spec :: Spec
spec = do
    permissionSpec
    previewSpec
    retrySpec
    candidateCycleSpec
    fullWalkSpec
    epssSpec
    pacingSpec

permissionSpec :: Spec
permissionSpec = describe "consent and classification" $ do
    it "halts the cycle when the store carries no consent marker, naming the backend" $ do
        store <- seededStore
        let withheld = withStore store (\h -> h{verifyConsent = pure (Right (ConsentWithheld "set permitDeletion to true"))})
        (rec', outcome) <- runCycle testPacing withheld
        outcomeHalt outcome `shouldBe` Just (HaltConsentWithheld Npm "fake" "set permitDeletion to true")
        errors <- recErrors rec'
        errors `shouldSatisfy` any (T.isInfixOf "carries no deletion consent marker")

    it "halts the cycle when the store refills itself, so a delete would change nothing" $ do
        store <- seededStore
        let preserved = withStore store (\h -> h{classifyStore = pure (Right (StorePreserved "it has an upstream"))})
        (_, outcome) <- runCycle testPacing preserved
        outcomeHalt outcome `shouldBe` Just (HaltStorePreserved Npm "fake" "it has an upstream")

    it "deletes nothing without consent, so the next request still serves the revoked version" $ do
        (outcome, held) <- unreadableCycle (\h -> h{verifyConsent = pure (Right (ConsentWithheld "attach it"))})
        outcomeHalt outcome `shouldBe` Just (HaltConsentWithheld Npm "fake" "attach it")
        tallyDeleted (outcomeTally outcome) `shouldBe` 0
        held `shouldBe` [version "1.0.0"]

    it "deletes nothing from a store that refills itself, so the next request still serves it" $ do
        (outcome, held) <- unreadableCycle (\h -> h{classifyStore = pure (Right (StorePreserved "it has an upstream"))})
        outcomeHalt outcome `shouldBe` Just (HaltStorePreserved Npm "fake" "it has an upstream")
        tallyDeleted (outcomeTally outcome) `shouldBe` 0
        held `shouldBe` [version "1.0.0"]

    it "deletes on identity alone once both guards pass, so the next request 404s" $ do
        (outcome, held) <- unreadableCycle id
        outcomeHalt outcome `shouldBe` Nothing
        tallyDeleted (outcomeTally outcome) `shouldBe` 1
        held `shouldBe` []

    it "reads both at every cycle start, so nothing stale decides a delete" $ do
        store <- seededStore
        reads' <- newIORef (0 :: Int)
        let counting h =
                h
                    { verifyConsent = modifyIORef' reads' (+ 1) >> pure (Right ConsentGranted)
                    , classifyStore = modifyIORef' reads' (+ 1) >> pure (Right StoreDestroyable)
                    }
        void (runCycle testPacing (withStore store counting))
        void (runCycle testPacing (withStore store counting))
        readIORef reads' `shouldReturn` 4

{- A preview holds the store's observing calls and an execution that counts, so it reaches no
delete and no marker. The two standing permissions become findings rather than halts. -}
previewSpec :: Spec
previewSpec = describe "a preview cycle" $ do
    it "enumerates and counts without the consent marker, reporting it unmet" $ do
        store <- newFakeStore seededConfig{fakeConsent = ConsentWithheld "attach it"}
        (rec', outcome) <- previewCycle testPacing store
        outcomeHalt outcome `shouldBe` Nothing
        tallyDeleted (outcomeTally outcome) `shouldBe` 1
        map tpConsent (outcomePrerequisites outcome) `shouldBe` [PrerequisiteUnmet "attach it"]
        warnings <- recWarnings rec'
        warnings `shouldSatisfy` any (T.isInfixOf "deletion consent is not met: attach it")

    it "reports a store that refills itself rather than refusing to sweep it" $ do
        store <- newFakeStore seededConfig{fakeClass = StorePreserved "it has an upstream"}
        (_, outcome) <- previewCycle testPacing store
        outcomeHalt outcome `shouldBe` Nothing
        map tpClassification (outcomePrerequisites outcome)
            `shouldBe` [PrerequisiteUnmet "it has an upstream"]

    it "leaves every version in the store, because it holds nothing that deletes" $ do
        store <- newFakeStore seededConfig{fakeConsent = ConsentWithheld "attach it"}
        seeded <- readFakeContents store
        (_, outcome) <- previewCycle testPacing store
        tallyDeleted (outcomeTally outcome) `shouldBe` 1
        readFakeContents store `shouldReturn` seeded

    it "counts the names the first-party belt shields apart from what it would delete" $ do
        store <- seededStore
        rec' <- recordingPortsUnder previewingReport generation
        outcome <-
            sweepCycle
                testPacing
                (recPorts rec')
                [(previewMountFor store){smFirstParty = (== packageName "left-pad")}]
        tallyGuardSkipped (outcomeTally outcome) `shouldBe` 1
        tallyDeleted (outcomeTally outcome) `shouldBe` 0

    it "walks from the first bucket and leaves the store's own marker where it was" $ do
        store <- newFakeStore bucketedConfig
        withBucket "l" $ \completed -> do
            traverse_ (\cursor -> void (writeCursor cursor completed)) (storeCursor (fakeMaintenance store))
            (_, outcome) <- previewCycle walkPacing store
            tallyExamined (outcomeTally outcome) `shouldBe` 2
            readFakeCursor store `shouldReturn` Just completed

    it "reads as complete though a prerequisite it reported is unmet" $ do
        store <- newFakeStore seededConfig{fakeConsent = ConsentWithheld "attach it"}
        (_, outcome) <- previewCycle testPacing store
        outcomeComplete outcome `shouldBe` True
        all prerequisitesMet (outcomePrerequisites outcome) `shouldBe` False

    it "reads as partial when a rule that reads advisories decided without a generation" $ do
        store <- seededStore
        rec' <- recordingPortsUnder previewingReport generation
        let unloaded =
                (previewMountFor store)
                    { smRuleDeps = inertRuleDeps
                    , smConfigured = [DenyByIdentity "left-pad", DenyIfCve (DenyIfCveParams 8.0 FailDeny)]
                    }
        outcome <- sweepCycle testPacing (recPorts rec') [unloaded]
        gapAdvisoryGeneration (outcomeEvidence outcome) `shouldBe` 1
        outcomeComplete outcome `shouldBe` False
        info <- recInfo rec'
        info `shouldSatisfy` any (T.isInfixOf "counted from partial evidence")

    it "reads as complete without a generation where no rule reads one" $ do
        store <- seededStore
        rec' <- recordingPortsUnder previewingReport generation
        outcome <-
            sweepCycle testPacing (recPorts rec') [(previewMountFor store){smRuleDeps = inertRuleDeps}]
        outcomeComplete outcome `shouldBe` True

    it "reads both standing permissions as met where the store carries them" $ do
        store <- seededStore
        (_, outcome) <- previewCycle testPacing store
        map tpConsent (outcomePrerequisites outcome) `shouldBe` [PrerequisiteMet]
        map tpClassification (outcomePrerequisites outcome) `shouldBe` [PrerequisiteMet]

    it "carries on when a standing permission could not be read, and reports that as well" $ do
        -- Nothing a preview reads settles the permission, which is a finding rather than an end to
        -- the enumeration: the counts still stand and the status still follows completeness.
        store <- seededStore
        rec' <- recordingPortsUnder previewingReport generation
        outcome <- sweepCycle testPacing (recPorts rec') [unreadableConsent store]
        outcomeHalt outcome `shouldBe` Nothing
        tallyDeleted (outcomeTally outcome) `shouldBe` 1
        outcomeComplete outcome `shouldBe` True
        map tpConsent (outcomePrerequisites outcome) `shouldSatisfy` all unread
        warnings <- recWarnings rec'
        warnings `shouldSatisfy` any (T.isInfixOf "deletion consent could not be read")
  where
    walkPacing = testPacing{swpShape = SweepEverything}

    -- Both seeded names lead with l, so one bucket holds them and the other is walked empty.
    bucketedConfig = seededConfig{fakeFacts = (fakeFacts seededConfig){factNameAlphabet = mkNameAlphabet "lx"}}

    unread = \case
        PrerequisiteUnread _ -> True
        _ -> False

-- A preview whose consent read alone faults, so the listing it walks still answers.
unreadableConsent :: FakeStore -> SweepMount
unreadableConsent store =
    (previewMount observing [denyRule] [DenyByIdentity "left-pad"]){smRuleDeps = loadedDeps}
  where
    observing =
        (fakeObservation store)
            { obVerifyConsent = pure (Left (protocolFault "the store did not answer the consent read"))
            }

{- A cycle over the store's observing calls alone, under a generation the rules can read, so a
complete preview is one whose only open question is the permission it reported. -}
previewCycle :: SweepPacing -> FakeStore -> IO (RecordedSweep, CycleOutcome)
previewCycle pacing store = do
    rec' <- recordingPortsUnder previewingReport generation
    outcome <- sweepCycle pacing (recPorts rec') [previewMountFor store]
    pure (rec', outcome)

previewMountFor :: FakeStore -> SweepMount
previewMountFor store =
    (previewMount (fakeObservation store) [denyRule] [DenyByIdentity "left-pad"])
        { smRuleDeps = loadedDeps
        }

-- A generation the sweep can read, which leaves the identity half to pin the candidate names.
loadedDeps :: RuleDeps
loadedDeps = inertRuleDeps{rdWithCveLookup = \use -> use (Just (fakeCveLookup []))}

{- One retry after the wait the fault itself advises. A fault that survives it halts the cycle,
and the next cycle re-attempts, so an outage reports once per interval and clears on its own. -}
retrySpec :: Spec
retrySpec = describe "withStoreRetry" $ do
    it "answers straight through when the call succeeds" $
        retrying (pure (Right ('a' :: Char))) $ \rec' outcome -> do
            outcome `shouldBe` Right 'a'
            recDelays rec' `shouldReturn` 0

    it "retries once after a fault worth another attempt, then answers" $ do
        attempts <- newIORef (0 :: Int)
        let flaky = do
                n <- atomicModifyIORef' attempts (\k -> (k + 1, k))
                pure (if n == 0 then Left retryable else Right 'a')
        retrying flaky $ \rec' outcome -> do
            outcome `shouldBe` Right 'a'
            recDelays rec' `shouldReturn` 1

    it "halts the cycle on a fault that survives the retry" $
        retrying (pure (Left retryable :: Either StoreFault Char)) $ \rec' outcome -> do
            outcome `shouldSatisfy` isLeft
            recDelays rec' `shouldReturn` 1

    it "does not retry a fault whose own advice says another attempt is futile" $ do
        attempts <- newIORef (0 :: Int)
        let futile = modifyIORef' attempts (+ 1) >> pure (Left (protocolFault "it never decodes") :: Either StoreFault Char)
        retrying futile $ \rec' _ -> do
            readIORef attempts `shouldReturn` 1
            recDelays rec' `shouldReturn` 0

    it "warns while it retries, and reserves the error line for the halt after one" $
        -- A retry that clears leaves the cycle running, so it is not something an operator must
        -- act on. Only a fault that survived the retry is.
        retrying (pure (Left retryable :: Either StoreFault Char)) $ \rec' _ -> do
            warnings <- recWarnings rec'
            warnings `shouldSatisfy` any (T.isInfixOf "retrying a call against the")
            recErrors rec' `shouldReturn` []

    it "names the backend on the halt a surviving fault raises, not the ecosystem alone" $
        retrying (pure (Left retryable :: Either StoreFault Char)) $ \_ outcome ->
            case outcome of
                Left (HaltStoreFault Npm backend detail) -> do
                    backend `shouldBe` "fake"
                    -- The cause reads as what happened, never as a constructor name.
                    detail `shouldSatisfy` T.isInfixOf "the peer did not answer in time"
                _ -> expectationFailure "expected a store-fault halt naming the backend"
  where
    retryable = StoreFault{faultTransport = transportFault TransportTimeout "no answer", faultRetry = RetryWorthwhile}

    retrying call assert' = do
        rec' <- recordingPorts generation
        store <- seededStore
        outcome <- withStoreRetry testPacing (recPorts rec') (testMount (fakeMaintenance store) [] []) call
        assert' rec' outcome

{- The default shape carries only the names an advisory or an identity deny can have changed,
so the store's listing bounds the cycle and the candidate set bounds the metadata reads. -}
candidateCycleSpec :: Spec
candidateCycleSpec = describe "the candidate cycle" $ do
    it "decides only the names the candidate set carries" $ do
        store <- seededStore
        (_, outcome) <- runCycleWith store [DenyByIdentity "left-pad"]
        tallyExamined (outcomeTally outcome) `shouldBe` 1
        tallyDeleted (outcomeTally outcome) `shouldBe` 1

    it "examines nothing when no name in the store is a candidate" $ do
        store <- seededStore
        (_, outcome) <- runCycleWith store [DenyByIdentity "not-in-this-store"]
        outcomeTally outcome `shouldSatisfy` \t -> tallyExamined t == 0 && tallyKept t == 0
        outcomeHalt outcome `shouldBe` Nothing

    it "reports once that no advisory generation is loaded, and still sweeps the identity half" $ do
        store <- seededStore
        rec' <- recordingPorts Nothing
        outcome <- sweepCycle testPacing (recPorts rec') [testMount (fakeMaintenance store) [denyRule] [DenyByIdentity "left-pad"]]
        tallyDeleted (outcomeTally outcome) `shouldBe` 1
        errors <- recErrors rec'
        length (filter (T.isInfixOf "no advisory database generation is loaded") errors) `shouldBe` 1

    it "closes a completed cycle with its counts on a routine line" $ do
        store <- seededStore
        (rec', _) <- runCycleWith store [DenyByIdentity "left-pad"]
        info <- recInfo rec'
        info `shouldSatisfy` any (T.isInfixOf "mirror sweep cycle complete: examined 1, deleted 1")

    it "reports a halted cycle on a line an operator must act on" $ do
        store <- seededStore
        let withheld = withStore store (\h -> h{verifyConsent = pure (Right (ConsentWithheld "attach it"))})
        (rec', _) <- runCycle testPacing withheld
        errors <- recErrors rec'
        errors `shouldSatisfy` any (T.isInfixOf "mirror sweep cycle halted")

{- The opt-in shape carries every name instead, which is what covers a rule-configuration change,
and it records each completed bucket so a restart re-does at most one. -}
fullWalkSpec :: Spec
fullWalkSpec = describe "the full walk" $ do
    it "decides every name in the store, candidate or not" $ do
        store <- seededStore
        (_, outcome) <- runCycle walkPacing (fakeMaintenance store)
        tallyExamined (outcomeTally outcome) `shouldBe` 2

    it "clears the marker when the walk completes, so the next cycle starts fresh" $ do
        store <- seededStore
        void (runCycle walkPacing (fakeMaintenance store))
        readFakeCursor store `shouldReturn` Nothing

    it "records each completed bucket, so a restart re-does at most the one in flight" $ do
        store <- newFakeStore (bucketedBy "lx")
        (writes, handle) <- recordingCursor (fakeMaintenance store)
        void (runCycle walkPacing handle)
        map renderNamePrefix <$> writes `shouldReturn` ["l", "x"]

    it "walks every bucket the alphabet gives, so no name falls outside the walk" $ do
        store <- newFakeStore (bucketedBy "lx")
        (_, outcome) <- runCycle walkPacing (fakeMaintenance store)
        tallyExamined (outcomeTally outcome) `shouldBe` 2

    it "walks a store that keeps no marker whole, every cycle" $ do
        store <- newFakeStore seededConfig{fakeKeepsCursor = False}
        (_, outcome) <- runCycle walkPacing (fakeMaintenance store)
        outcomeHalt outcome `shouldBe` Nothing
        tallyExamined (outcomeTally outcome) `shouldBe` 2

    it "halts the cycle when the store's listing stops on a fault" $ do
        store <- newFakeStore seededConfig{fakeFault = Just (protocolFault "the store stopped answering")}
        (_, outcome) <- runCycle walkPacing (fakeMaintenance store)
        outcomeHalt outcome `shouldSatisfy` isStoreFault
  where
    walkPacing = testPacing{swpShape = SweepEverything}

    isStoreFault = \case
        Just HaltStoreFault{} -> True
        _ -> False

    -- Both seeded names lead with l, so one bucket holds them and the other is walked empty.
    bucketedBy chars = seededConfig{fakeFacts = (fakeFacts seededConfig){factNameAlphabet = mkNameAlphabet chars}}

{- The handle over a cursor that records what the walk wrote to it, so a case reads the buckets in
the order they completed rather than only the one left behind. -}
recordingCursor :: StoreMaintenance -> IO (IO [NamePrefix], StoreMaintenance)
recordingCursor handle = do
    written <- newIORef []
    let recorded cursor =
            cursor{writeCursor = \prefix -> modifyIORef' written (prefix :) >> writeCursor cursor prefix}
    pure (reverse <$> readIORef written, handle{storeCursor = recorded <$> storeCursor handle})

pacingSpec :: Spec
pacingSpec = describe "cycle chunk pacing" $ do
    it "pauses before the second one-name page" $
        assertPacing SweepCandidates 1 "" [["a"], ["b"]] ["a", "b"] [("a", 0), ("b", 1)]

    it "carries a partial chunk across short pages without a trailing pause" $
        assertPacing
            SweepCandidates
            2
            ""
            [["a"], ["b"], ["c", "d"]]
            ["a", "b", "c", "d"]
            [("a", 0), ("b", 0), ("c", 1), ("d", 1)]

    it "keeps progress across empty pages and filters before counting" $
        assertPacing
            SweepCandidates
            2
            ""
            [[], ["a", "x"], [], ["y"], ["b"], [], ["c"], []]
            ["a", "b", "c"]
            [("a", 0), ("b", 0), ("c", 1)]

    it "does not pause for an empty listing" $
        assertPacing SweepCandidates 1 "" [[], []] [] []

    it "does not pause for pages containing no candidates" $
        assertPacing SweepCandidates 1 "" [["x"], [], ["y"]] [] []

    it "carries a completed chunk across empty candidate buckets" $
        assertPacing SweepCandidates 1 "abc" [["a"], ["c"]] ["a", "c"] [("a", 0), ("c", 1)]

    it "carries a partial chunk across full-walk buckets" $
        assertPacing
            SweepEverything
            2
            "abcd"
            [["a"], [], ["b"], ["d"]]
            []
            [("a", 0), ("b", 0), ("d", 1)]

    it "paces a full walk within a bucket" $
        assertPacing
            SweepEverything
            1
            ""
            [["a", "b"], [], ["c"]]
            []
            [("a", 0), ("b", 1), ("c", 2)]

    it "retains the single-name fallback for a non-positive chunk size" $
        assertPacing SweepCandidates 0 "" [["a", "b"]] ["a", "b"] [("a", 0), ("b", 1)]

    it "halts at the cap without pausing or requesting another page" $ do
        store <- seededStore
        rec' <- recordingPorts generation
        let handle =
                (fakeMaintenance store)
                    { listPackagesIn = \_ -> do
                        yield [packageName "left-pad"]
                        lift (expectationFailure "the cap must abandon the listing before its next page")
                        pure Nothing
                    }
            pacing = testPacing{swpChunkSize = 1, swpDeletionCap = 1}
        outcome <- sweepCycle pacing (recPorts rec') [testMount handle [denyRule] [DenyByIdentity "left-pad"]]
        outcomeHalt outcome `shouldBe` Just (HaltDeletionCap 1 1 generation)
        tallyDeleted (outcomeTally outcome) `shouldBe` 1
        recDelays rec' `shouldReturn` 0

    it "shares progress across mounts and resets it for a new cycle" $ do
        stores <- replicateM 2 seededStore
        rec' <- recordingPorts generation
        observed <- newIORef []
        let observe store =
                let handle = fakeMaintenance store
                 in handle
                        { enumerateVersions = \name -> do
                            pauses <- recDelays rec'
                            modifyIORef' observed (pauses :)
                            enumerateVersions handle name
                        }
            mounts = [testMount (observe store) [] [] | store <- stores]
            pacing = testPacing{swpShape = SweepEverything, swpChunkSize = 3}
        replicateM_ 2 $ do
            outcome <- sweepCycle pacing (recPorts rec') mounts
            outcomeHalt outcome `shouldBe` Nothing
            tallyKept (outcomeTally outcome) `shouldBe` 4
        reverse <$> readIORef observed `shouldReturn` [0, 0, 0, 1, 1, 1, 1, 2]
        recDelays rec' `shouldReturn` 2

assertPacing :: SweepShape -> Int -> String -> [[Text]] -> [Text] -> [(Text, Int)] -> Expectation
assertPacing shape chunkSize alphabet pages candidates expected = do
    let names = map packageName (concat pages)
        config =
            (storeConfigFor names)
                { fakeFacts = (fakeFacts seededConfig){factNameAlphabet = mkNameAlphabet alphabet}
                }
    store <- newFakeStore config
    rec' <- recordingPorts generation
    observed <- newIORef []
    let base = fakeMaintenance store
        selected name = shape == SweepEverything || name `elem` map packageName candidates
        handle =
            base
                { listPackagesIn = \prefix -> do
                    forM_ pages $ \page -> do
                        let namesInPage = filter (inBucket prefix) (map packageName page)
                        priorCount <- lift (length <$> readIORef observed)
                        yield namesInPage
                        when (shape == SweepCandidates) $
                            lift ((length <$> readIORef observed) `shouldReturn` (priorCount + length (filter selected namesInPage)))
                    pure Nothing
                , enumerateVersions = \name -> do
                    pauses <- recDelays rec'
                    modifyIORef' observed ((name, pauses) :)
                    enumerateVersions base name
                }
        ports =
            (recPorts rec')
                { sweepDelay = \duration -> do
                    duration `shouldBe` 2
                    sweepDelay (recPorts rec') duration
                }
        pacing = testPacing{swpShape = shape, swpChunkSize = chunkSize, swpChunkPause = 2}
    outcome <- sweepCycle pacing ports [testMount handle [denyRule] (map DenyByIdentity candidates)]
    outcomeHalt outcome `shouldBe` Nothing
    tallyDeleted (outcomeTally outcome) `shouldBe` length expected
    reverse <$> readIORef observed `shouldReturn` map (first packageName) expected
    recDelays rec' `shouldReturn` foldl' max 0 (map snd expected)

epssSpec :: Spec
epssSpec = describe "individual missing EPSS scores under the production evaluator" $ do
    forM_ unscoredEpssCases $ \(label, score) ->
        it ("keeps the stored version for " <> label) $ do
            score `shouldBe` Nothing
            (outcome, contents) <- sweepAdvisories [advisory "MAL-2026-1" score] [epssRule]
            tallyExamined (outcomeTally outcome) `shouldBe` 1
            tallyKept (outcomeTally outcome) `shouldBe` 1
            tallyDeleted (outcomeTally outcome) `shouldBe` 0
            Map.lookup (packageName "left-pad") contents `shouldBe` Map.lookup (packageName "left-pad") (fakeContents seededConfig)

    it "keeps below-threshold scores and deletes when a known high score returns" $
        forM_ [(Just 0.01, 0), (Nothing, 0), (Just 0.75, 1)] $ \(score, deleted) -> do
            (outcome, _) <- sweepAdvisories [advisory "CVE-2026-10001" score] [epssRule]
            tallyDeleted (outcomeTally outcome) `shouldBe` deleted

    it "deletes on another affecting advisory above the EPSS threshold" $ do
        (outcome, contents) <-
            sweepAdvisories
                [advisory "MAL-2026-1" Nothing, advisory "CVE-2026-10002" (Just 0.75)]
                [epssRule]
        tallyDeleted (outcomeTally outcome) `shouldBe` 1
        Map.findWithDefault [] (packageName "left-pad") contents `shouldBe` []

    it "preserves CVSS denial of an unscored malware advisory" $ do
        (outcome, _) <-
            sweepAdvisories
                [advisory "MAL-2026-1" Nothing]
                [epssRule, DenyIfCve (DenyIfCveParams 8.0 FailDeny)]
        tallyDeleted (outcomeTally outcome) `shouldBe` 1

    it "deletes on another decisive rule when EPSS abstains" $ do
        (outcome, _) <-
            sweepAdvisories
                [advisory "MAL-2026-1" Nothing]
                [epssRule, DenyByIdentity "left-pad"]
        tallyDeleted (outcomeTally outcome) `shouldBe` 1
  where
    epssRule = DenyIfEpss (DenyIfEpssParams 0.5 FailDeny)
    advisory identifier = AdvisoryRange identifier Nothing (Just "0") Unbounded

sweepAdvisories :: [AdvisoryRange] -> [Rule] -> IO (CycleOutcome, Map PackageName [StoredVersion])
sweepAdvisories ranges configured = do
    store <- seededStore
    rec' <- recordingPorts generation
    let deps = inertRuleDeps{rdWithCveLookup = \use -> use (Just (fakeCveLookup [("left-pad", ar) | ar <- ranges]))}
    prepared <- prepare deps (map atDefaultPrecedence configured)
    let mount = (testMount (fakeMaintenance store) prepared configured){smRuleDeps = deps}
    outcome <- sweepCycle testPacing (recPorts rec') [mount]
    contents <- readFakeContents store
    pure (outcome, contents)

runCycle :: SweepPacing -> StoreMaintenance -> IO (RecordedSweep, CycleOutcome)
runCycle pacing handle = do
    rec' <- recordingPorts generation
    outcome <- sweepCycle pacing (recPorts rec') [testMount handle [denyRule] []]
    pure (rec', outcome)

runCycleWith :: FakeStore -> [Rule] -> IO (RecordedSweep, CycleOutcome)
runCycleWith store configured = do
    rec' <- recordingPorts generation
    outcome <- sweepCycle testPacing (recPorts rec') [testMount (fakeMaintenance store) [denyRule] configured]
    pure (rec', outcome)

withStore :: FakeStore -> (StoreMaintenance -> StoreMaintenance) -> StoreMaintenance
withStore store f = f (fakeMaintenance store)

{- A cycle over a store that serves no metadata, carrying the identity deny that identity alone
condemns. The transform is where one guard is withdrawn, so each guard is read on its own. -}
unreadableCycle :: (StoreMaintenance -> StoreMaintenance) -> IO (CycleOutcome, [Version])
unreadableCycle f = do
    store <- newFakeStore seededConfig{fakeManifests = Map.empty}
    rec' <- recordingPorts generation
    prepared <- prepare inertRuleDeps [atDefaultPrecedence revoked]
    outcome <- sweepCycle testPacing (recPorts rec') [testMount (f (fakeMaintenance store)) prepared [revoked]]
    contents <- readFakeContents store
    pure (outcome, maybe [] (map storedVersion) (Map.lookup (packageName "left-pad") contents))
  where
    revoked = DenyByIdentity "left-pad@1.0.0"

seededStore :: IO FakeStore
seededStore = newFakeStore seededConfig

seededConfig :: FakeStoreConfig
seededConfig = storeConfigFor [packageName "left-pad", packageName "lodash"]

storeConfigFor :: [PackageName] -> FakeStoreConfig
storeConfigFor names =
    defaultFakeStoreConfig
        { fakeContents = Map.fromList [(name, [StoredVersion (version "1.0.0") VersionServed]) | name <- names]
        , fakeManifests = Map.fromList [(name, sampleManifest name [version "1.0.0"]) | name <- names]
        }

generation :: Maybe DbEtag
generation = Just (DbEtag "etag-1")

packageName :: Text -> PackageName
packageName = mkPackageName Npm Nothing

version :: Text -> Version
version = mkVersion Npm
