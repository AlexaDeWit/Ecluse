-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Queued mirror admission, publishing, and retry outcomes.
module Ecluse.Core.Worker.JobSpec (spec) where

import Data.Aeson (Value (String), object, (.=))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian, nominalDay)
import Katip (Severity (InfoS))
import Test.Hspec
import UnliftIO.Exception (impureThrow)

import Ecluse.Core.Cve (AdvisoryRange (AdvisoryRange))
import Ecluse.Core.Cve.Types (DbEtag (DbEtag))
import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Fault (TransportCause (TransportUnreachable), transportFault)
import Ecluse.Core.Mirror.State
import Ecluse.Core.Osv.Types (UpperBound (LastAffected))
import Ecluse.Core.Package (
    Artifact (artCoreMetadata, artFilename, artHashes),
    Availability (Deprecated, Yanked),
    CoreMetadata (MetadataDigests),
    Hash,
    HashAlg (Blake2b, SHA1, SHA256, SHA512, SRI),
    PackageDetails (pkgArtifacts, pkgAvailability, pkgPublishedAt),
    PackageInfo (..),
    PackageName,
 )
import Ecluse.Core.Package.Admission (ArtifactAdmission (AdmissionUndecidable))
import Ecluse.Core.Queue (MirrorJob (jobTriggerFilename))
import Ecluse.Core.Registry (
    FetchFault (FetchBoundExceeded, FetchCooldown, FetchTransport, FetchUrlUnformable),
    MirrorArtifact (MirrorArtifact, maFilename, maHashes, maSize),
    PublishError (PublishError),
    PublishFault (PublishFetch, PublishRejected),
    UrlFormationError (EmptyBaseUrl),
 )
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (artifactByUrl))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached)
import Ecluse.Core.Registry.Metadata (
    VersionDoc (VersionDoc, vdDetails, vdRaw),
    VersionEvaluation (VersionMetadataUnavailable, VersionMissing, VersionPresent),
 )
import Ecluse.Core.Registry.MirrorUpload (MirrorInventory, inventoryDetails)
import Ecluse.Core.Registry.MirrorUpload.Internal (restoreMirrorInventory)
import Ecluse.Core.Registry.Npm.Publish (npmPublishCodec)
import Ecluse.Core.Registry.Publish (
    MirrorPublish (mpProbeInventory),
    PublishPlan (ppLatest),
 )
import Ecluse.Core.Registry.PyPI.Mirror (pypiStateFormat)
import Ecluse.Core.Registry.Request (nativePresentation)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Rules.Types (Decision (Undecidable), DenyIfCveParams (DenyIfCveParams), FailureAlignment (FailDeny, FailNoDecision), Inability (NoDatabaseLoaded, RuleThrew), Reason (RuleUnable), Rule (AllowIfOlderThan, DenyIfCve, DenyIfDeprecated, DenyIfKnownMalicious, DenyIfYanked), Transience (WillResolve, WontResolve), ruleName)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit, MirrorArtifactBodyLimit), LimitError (BodyTooLarge), defaultLimits, hostPortAddress)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Version (Version, renderVersion)
import Ecluse.Core.Worker (
    JobOutcome (DeadLettered, Dropped, Retried, Succeeded),
    RetryLeg (AfterPublish, BeforePublish),
    WorkerPolicies,
    WorkerPolicy (wpArtifact, wpArtifactHostHonoured, wpFirstParty, wpMirrorState, wpNow, wpPublish, wpRules),
    processJob,
 )
import Ecluse.Core.Worker.Job (mirrorLatest, outcomeOfAdmission, outcomeOfFetchFault)
import Ecluse.Core.Worker.Types (WorkerMirrorState (..))
import Ecluse.Test.Cve (fakeCveLookup)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Json (fieldAt)
import Ecluse.Test.Log (captureJsonLog, captureJsonLogAt)
import Ecluse.Test.Mirror.State
import Ecluse.Test.Package (hexSha256Of, npmVersion, unsafeFilename, unsafeHash)
import Ecluse.Test.Port (noopWorkerMetricsPort)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Registry.Npm (sourceVersionDoc)
import Ecluse.Test.Registry.Publish (anonymousPublishAt)
import Ecluse.Test.Rules (admissionOnly, admitRule, atDefaultPrecedence, cannotVetRule, constRule, denyRule, inertRuleDeps, servingRuleDeps)
import Ecluse.Test.Snapshot (versionDocOf)
import Ecluse.Test.Stub (allCaptured, capMethod, capPath, stubBaseUrl, withRoutedStub, withStub)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape), decodeJsonOrFail, expectRight)
import Ecluse.Worker.Support
import Ecluse.Worker.Support.Managed
import Network.HTTP.Types.Status (status200, status404)

spec :: Spec
spec = do
    describe "processJob: complete managed releases" $ do
        it "copies every file with exactly two native inventory probes" $
            withUpstream $ \url -> do
                fixture <- managedFixture url False
                runManaged fixture `shouldReturn` Succeeded
                readIORef (mfUploads fixture) `shouldReturn` map mirrorFilename [1, 2, 3]
                readIORef (mfProbes fixture) `shouldReturn` 2

        it "does not treat unavailable state as a smaller expected inventory" $ do
            fixture <- managedFixture unreachableUrl False
            writeIORef (mfStateFault fixture) (Just StateRevisionMismatch)
            runManaged fixture >>= (`shouldSatisfy` isRetried)
            readIORef (mfUploads fixture) `shouldReturn` []
            readIORef (mfRecord fixture) `shouldReturn` Nothing

        it "does not upload before the expected record persists" $
            withUpstream $ \url -> do
                fixture <- managedFixture url False
                let binding = mfBinding fixture
                    store = (wmsStore binding){msWriteProject = \_ _ _ _ -> pure (Left StateRetryExhausted)}
                    policy = (mfPolicy fixture){wpMirrorState = Just binding{wmsStore = store}}
                runManaged fixture{mfPolicy = policy} >>= (`shouldSatisfy` isRetried)
                readIORef (mfUploads fixture) `shouldReturn` []

        for_
            [ ("dead-letters a bounded state read", StateBound (BodyTooLarge (MetadataBodyLimit 1024)), DeadLettered)
            , ("dead-letters a state identity mismatch", StateIdentityMismatch, DeadLettered)
            , ("retries a state transport failure before publication", StateTransport (transportFault TransportUnreachable "connection reset"), Retried BeforePublish)
            ]
            $ \(label, fault, expected) ->
                it label $
                    withStub status200 (toLazy tarballBytes) $ \upstream -> do
                        fixture <- managedFixture (stubBaseUrl upstream) False
                        writeIORef (mfStateFault fixture) (Just fault)
                        runManaged fixture `shouldReturn` expected ("mirror state unavailable: " <> show fault)
                        readIORef (mfProbes fixture) `shouldReturn` 1
                        readIORef (mfResolves fixture) `shouldReturn` 0
                        readIORef (mfUploads fixture) `shouldReturn` []
                        readIORef (mfRecord fixture) `shouldReturn` Nothing
                        allCaptured upstream `shouldReturn` []

        it "drops a companion retention conflict without publishing the remaining files" $
            withStub status200 (toLazy tarballBytes) $ \upstream -> do
                fixture <- managedFixture (stubBaseUrl upstream) True
                let fault = StateCompanionConflict (mirrorFilename 1)
                    binding = mfBinding fixture
                    store = (wmsStore binding){msRetainCompanion = \_ _ -> pure (Left fault)}
                    policy = (mfPolicy fixture){wpMirrorState = Just binding{wmsStore = store}}
                runManaged fixture{mfPolicy = policy} `shouldReturn` Dropped ("mirror state unavailable: " <> show fault)
                readIORef (mfProbes fixture) `shouldReturn` 1
                readIORef (mfUploads fixture) `shouldReturn` [mirrorFilename 1]
                readIORef (mfCompanions fixture) `shouldReturn` mempty
                recorded <- readIORef (mfRecord fixture)
                fmap versionCompanions (recordVersion mirrorVersion =<< recorded)
                    `shouldBe` Just (Map.fromList [(mirrorFilename number, CompanionPending) | number <- [1, 2, 3]])
                requests <- allCaptured upstream
                map capPath requests `shouldBe` map encodeUtf8 ["/" <> mirrorFilename 1, "/" <> mirrorFilename 1 <> ".metadata"]

        it "restarts after a partial failure and fetches only missing distributions" $
            withStub status200 (toLazy tarballBytes) $ \upstream -> do
                fixture <- managedFixture (stubBaseUrl upstream) False
                writeIORef (mfRejectFile fixture) (Just (mirrorFilename 2))
                runManaged fixture >>= (`shouldSatisfy` isRetried)
                writeIORef (mfRejectFile fixture) Nothing
                runManaged fixture `shouldReturn` Succeeded
                readIORef (mfUploads fixture) `shouldReturn` map mirrorFilename [1, 2, 2, 3]
                length <$> allCaptured upstream `shouldReturn` 4

        it "recovers a lost publish response through current target identity" $
            withUpstream $ \url -> do
                fixture <- managedFixture url False
                writeIORef (mfLoseReply fixture) True
                runManaged fixture >>= (`shouldSatisfy` isRetried)
                writeIORef (mfLoseReply fixture) False
                runManaged fixture `shouldReturn` Succeeded
                readIORef (mfUploads fixture) `shouldReturn` map mirrorFilename [1, 2, 3]

        it "acknowledges a complete restart without public reads or current admission" $
            withUpstream $ \url -> do
                fixture <- managedFixture url False
                runManaged fixture `shouldReturn` Succeeded
                resolved <- readIORef (mfResolves fixture)
                writeIORef (mfSource fixture) VersionMetadataUnavailable
                let policy = (mfPolicy fixture){wpRules = [denyRule]}
                (outcome, logs) <- captureJsonLogAt InfoS (\logEnv -> runManagedWithLog logEnv managedJob fixture{mfPolicy = policy})
                outcome `shouldBe` Succeeded
                logs `shouldBe` ""
                readIORef (mfResolves fixture) `shouldReturn` resolved

        it "extends an already complete record for an explicitly requested later file" $
            withUpstream $ \url -> do
                fixture <- managedFixture url False
                initial <- managedInventory url False [1, 2]
                writeIORef (mfSource fixture) (VersionPresent (sourceDocument initial) Nothing)
                runManaged fixture `shouldReturn` Succeeded
                added <- managedInventory url False [1, 2, 3]
                writeIORef (mfSource fixture) (VersionPresent (sourceDocument added) Nothing)
                let requested = managedJob{jobTriggerFilename = Just (unsafeFilename (mirrorFilename 3))}
                runManagedWith requested fixture `shouldReturn` Succeeded
                readIORef (mfUploads fixture) `shouldReturn` map mirrorFilename [1, 2, 3]
                readIORef (mfResolves fixture) `shouldReturn` 2
                writeIORef (mfSource fixture) VersionMetadataUnavailable
                runManagedWith requested fixture `shouldReturn` Succeeded
                readIORef (mfResolves fixture) `shouldReturn` 2

        it "does not call a denied later filename complete or alter the earlier mirrored files" $
            withUpstream $ \url -> do
                fixture <- managedFixture url False
                initial <- managedInventory url False [1, 2]
                writeIORef (mfSource fixture) (VersionPresent (sourceDocument initial) Nothing)
                runManaged fixture `shouldReturn` Succeeded
                priorRecord <- readIORef (mfRecord fixture)
                added <- managedInventory url False [1, 2, 3]
                writeIORef (mfSource fixture) (VersionPresent (sourceDocument added) Nothing)
                let requested = managedJob{jobTriggerFilename = Just (unsafeFilename (mirrorFilename 3))}
                    policy = (mfPolicy fixture){wpRules = [denyRule]}
                runManagedWith requested fixture{mfPolicy = policy} >>= (`shouldSatisfy` isDropped)
                readIORef (mfRecord fixture) `shouldReturn` priorRecord
                readIORef (mfUploads fixture) `shouldReturn` map mirrorFilename [1, 2]
                readIORef (mfResolves fixture) `shouldReturn` 2

        it "keeps a partial release incomplete during a public outage" $
            withUpstream $ \url -> do
                fixture <- managedFixture url False
                writeIORef (mfRejectFile fixture) (Just (mirrorFilename 2))
                runManaged fixture >>= (`shouldSatisfy` isRetried)
                writeIORef (mfSource fixture) VersionMetadataUnavailable
                runManaged fixture >>= (`shouldSatisfy` isRetried)
                readIORef (mfUploads fixture) `shouldReturn` map mirrorFilename [1, 2]

        it "shares a verified SHA-256 bridge with completion for a SHA-512-only source" $
            withUpstream $ \url -> do
                fixture <- managedFixture url False
                initial <- managedInventory url False [1, 2, 3]
                changed <- sha512Source initial
                writeIORef (mfSource fixture) (VersionPresent (sourceDocument changed) Nothing)
                writeIORef (mfTargetTransform fixture) (\file -> file{artHashes = [unsafeHash SHA256 trueSha256]})
                runManaged fixture `shouldReturn` Succeeded
                writeIORef (mfSource fixture) VersionMetadataUnavailable
                runManaged fixture `shouldReturn` Succeeded

        it "verifies a pre-existing SHA-256 target without republishing SHA-512 source files" $
            withUpstream $ \url -> do
                fixture <- managedFixture url False
                initial <- managedInventory url False [1, 2, 3]
                changed <- sha512Source initial
                writeIORef (mfSource fixture) (VersionPresent (sourceDocument changed) Nothing)
                writeIORef (mfTarget fixture) (toList (pkgArtifacts (inventoryDetails initial)))
                runManaged fixture `shouldReturn` Succeeded
                readIORef (mfUploads fixture) `shouldReturn` []
                writeIORef (mfSource fixture) VersionMetadataUnavailable
                runManaged fixture `shouldReturn` Succeeded

        for_
            [ ("drops a conflicting SHA-256 target after verifying SHA-512 source bytes", unsafeHash SHA256 someSha256, Dropped ("mirror target identity conflicts: " <> mirrorFilename 1))
            , ("retries an incompatible Blake2b target after verifying SHA-512 source bytes", unsafeHash Blake2b trueBlake2b, Retried BeforePublish ("mirror file lacks compatible current identity: " <> mirrorFilename 1))
            ]
            $ \(label, targetHash, expected) ->
                it label $
                    withStub status200 (toLazy tarballBytes) $ \upstream -> do
                        fixture <- managedFixture (stubBaseUrl upstream) False
                        initial <- managedInventory (stubBaseUrl upstream) False [1, 2, 3]
                        changed <- sha512Source initial
                        let target = map (\file -> file{artHashes = [targetHash]}) (toList (pkgArtifacts (inventoryDetails initial)))
                        writeIORef (mfSource fixture) (VersionPresent (sourceDocument changed) Nothing)
                        writeIORef (mfTarget fixture) target
                        runManaged fixture `shouldReturn` expected
                        readIORef (mfProbes fixture) `shouldReturn` 1
                        readIORef (mfResolves fixture) `shouldReturn` 1
                        readIORef (mfUploads fixture) `shouldReturn` []
                        readIORef (mfTarget fixture) `shouldReturn` target
                        requests <- allCaptured upstream
                        map capPath requests `shouldBe` [encodeUtf8 ("/" <> mirrorFilename 1)]
                        recorded <- readIORef (mfRecord fixture)
                        fmap versionVerifiedIdentities (recordVersion mirrorVersion =<< recorded)
                            `shouldBe` Just (Map.singleton (mirrorFilename 1) (unsafeHash SHA256 trueSha256))

        it "refuses conflicting target bytes without repeated publication" $
            withUpstream $ \url -> do
                fixture <- managedFixture url False
                initial <- managedInventory url False [1, 2, 3]
                let changed file = file{artHashes = [unsafeHash SHA256 "0000000000000000000000000000000000000000000000000000000000000000"]}
                writeIORef (mfTarget fixture) (map changed (toList (pkgArtifacts (inventoryDetails initial))))
                runManaged fixture >>= (`shouldSatisfy` isDropped)
                runManaged fixture >>= (`shouldSatisfy` isDropped)
                readIORef (mfUploads fixture) `shouldReturn` []

        it "rereads and unions a conflicting state update before any upload" $
            withUpstream $ \url -> do
                fixture <- managedFixture url False
                competing <- managedInventory url False [4]
                collided <- newIORef False
                let binding = mfBinding fixture
                    original = wmsStore binding
                    store =
                        original
                            { msWriteProject = \charge name revision record -> do
                                already <- atomicModifyIORef' collided (True,)
                                if already
                                    then msWriteProject original charge name revision record
                                    else do
                                        _ <- recordExpectedInventory pypiStateFormat defaultLimits (const (pure ())) (wmsTarget binding) original competing >>= expectRight
                                        pure (Right StateConflict)
                            }
                    policy = (mfPolicy fixture){wpMirrorState = Just binding{wmsStore = store}}
                runManaged fixture{mfPolicy = policy} `shouldReturn` Succeeded
                sort <$> readIORef (mfUploads fixture) `shouldReturn` map mirrorFilename [1, 2, 3, 4]

        it "refuses digestless target listings despite successful physical writes" $
            withUpstream $ \url -> do
                fixture <- managedFixture url False
                writeIORef (mfTargetTransform fixture) (\file -> file{artHashes = []})
                runManaged fixture >>= (`shouldSatisfy` isRetried)
                readIORef (mfProbes fixture) `shouldReturn` 2

        it "refuses a contradictory final target digest" $
            withUpstream $ \url -> do
                fixture <- managedFixture url False
                writeIORef (mfTargetTransform fixture) (\file -> file{artHashes = [unsafeHash SHA256 someSha256]})
                runManaged fixture >>= (`shouldSatisfy` isDropped)
                readIORef (mfProbes fixture) `shouldReturn` 2

        it "refuses a missing final file without repeated upload passes" $
            withUpstream $ \url -> do
                fixture <- managedFixture url False
                writeIORef (mfOnProbe fixture) $ \number ->
                    when (number == 2) $
                        modifyIORef' (mfTarget fixture) (filter ((/= mirrorFilename 3) . artFilename))
                runManaged fixture >>= (`shouldSatisfy` isRetried)
                readIORef (mfProbes fixture) `shouldReturn` 2

        it "retains and validates declared companions before whole-version success" $
            withStub status200 (toLazy tarballBytes) $ \upstream -> do
                fixture <- managedFixture (stubBaseUrl upstream) True
                runManaged fixture `shouldReturn` Succeeded
                length <$> allCaptured upstream `shouldReturn` 6
                writeIORef (mfSource fixture) VersionMetadataUnavailable
                runManaged fixture `shouldReturn` Succeeded
                length <$> allCaptured upstream `shouldReturn` 6

        for_ ["missing", "corrupt"] $ \failure ->
            it ("refuses a " <> failure <> " retained companion after restart") $
                withUpstream $ \url -> do
                    fixture <- managedFixture url True
                    runManaged fixture `shouldReturn` Succeeded
                    modifyIORef' (mfCompanions fixture) (if failure == "missing" then const mempty else fmap (const "corrupt"))
                    runManaged fixture >>= (`shouldSatisfy` (\outcome -> isRetried outcome || isDeadLettered outcome))

        it "recovers pending companions without fetching an already copied archive" $
            withStub status200 (toLazy tarballBytes) $ \upstream -> do
                fixture <- managedFixture (stubBaseUrl upstream) True
                let binding = mfBinding fixture
                    store = (wmsStore binding){msRetainCompanion = \_ _ -> pure (Left StateRetryExhausted)}
                    policy = (mfPolicy fixture){wpMirrorState = Just binding{wmsStore = store}}
                runManaged fixture{mfPolicy = policy} >>= (`shouldSatisfy` isRetried)
                runManaged fixture `shouldReturn` Succeeded
                readIORef (mfUploads fixture) `shouldReturn` map mirrorFilename [1, 2, 3]
                requests <- allCaptured upstream
                length (filter ((== encodeUtf8 ("/" <> mirrorFilename 1)) . capPath) requests) `shouldBe` 1

        it "adds a bounded pass for a concurrent expected file missing from the final target" $
            withUpstream $ \url -> do
                fixture <- managedFixture url False
                added <- managedInventory url False [1, 2, 3, 4]
                let binding = mfBinding fixture
                writeIORef (mfOnProbe fixture) $ \number -> when (number == 2) $ do
                    _ <- recordExpectedInventory pypiStateFormat defaultLimits (const (pure ())) (wmsTarget binding) (wmsStore binding) added >>= expectRight
                    writeIORef (mfSource fixture) (VersionPresent (sourceDocument added) Nothing)
                runManaged fixture `shouldReturn` Succeeded
                readIORef (mfUploads fixture) `shouldReturn` map mirrorFilename [1, 2, 3, 4]
                readIORef (mfProbes fixture) `shouldReturn` 3

        it "does not add a pass when concurrent expected files already have target proof" $
            withUpstream $ \url -> do
                fixture <- managedFixture url False
                added <- managedInventory url False [1, 2, 3, 4]
                let binding = mfBinding fixture
                writeIORef (mfOnProbe fixture) $ \number -> when (number == 2) $ do
                    _ <- recordExpectedInventory pypiStateFormat defaultLimits (const (pure ())) (wmsTarget binding) (wmsStore binding) added >>= expectRight
                    writeIORef (mfTarget fixture) (toList (pkgArtifacts (inventoryDetails added)))
                runManaged fixture `shouldReturn` Succeeded
                readIORef (mfUploads fixture) `shouldReturn` map mirrorFilename [1, 2, 3]
                readIORef (mfProbes fixture) `shouldReturn` 2

        it "uses original admitted locations when a later public snapshot moves them" $
            withStub status200 (toLazy tarballBytes) $ \original ->
                withStub status404 "unavailable" $ \moved -> do
                    fixture <- managedFixture (stubBaseUrl original) False
                    writeIORef (mfRejectFile fixture) (Just (mirrorFilename 2))
                    runManaged fixture >>= (`shouldSatisfy` isRetried)
                    changed <- managedInventory (stubBaseUrl moved) False [1, 2, 3]
                    writeIORef (mfSource fixture) (VersionPresent (sourceDocument changed) Nothing)
                    writeIORef (mfRejectFile fixture) Nothing
                    runManaged fixture `shouldReturn` Succeeded
                    allCaptured moved `shouldReturn` []

        it "recovers missing files from newly admitted locations when the original source disappears" $
            withStub status404 "unavailable" $ \original ->
                withStub status200 (toLazy tarballBytes) $ \moved -> do
                    fixture <- managedFixture (stubBaseUrl original) False
                    runManaged fixture >>= (`shouldSatisfy` isRetried)
                    changed <- managedInventory (stubBaseUrl moved) False [1, 2, 3]
                    writeIORef (mfSource fixture) (VersionPresent (sourceDocument changed) Nothing)
                    runManaged fixture `shouldReturn` Succeeded
                    readIORef (mfUploads fixture) `shouldReturn` map mirrorFilename [1, 2, 3]
                    length <$> allCaptured moved `shouldReturn` 3

        it "checks bytes received from a replacement URL against the original admitted digest" $
            withStub status404 "unavailable" $ \original ->
                withStub status200 "changed bytes" $ \moved -> do
                    fixture <- managedFixture (stubBaseUrl original) False
                    runManaged fixture >>= (`shouldSatisfy` isRetried)
                    changed <- managedInventory (stubBaseUrl moved) False [1, 2, 3]
                    writeIORef (mfSource fixture) (VersionPresent (sourceDocument changed) Nothing)
                    runManaged fixture >>= (`shouldSatisfy` isDropped)
                    length <$> allCaptured moved `shouldReturn` 1
                    readIORef (mfUploads fixture) `shouldReturn` []

        for_ [False, True] $ \corrupt ->
            it (if corrupt then "refuses changed companion bytes at a newly admitted location" else "retains pending companions from a newly admitted location before completion") $
                withRoutedStub (\request -> if ".metadata" `T.isSuffixOf` decodeUtf8 (capPath request) then (status404, [], "missing") else (status200, [], toLazy tarballBytes)) $ \original ->
                    withRoutedStub (\request -> (status200, [], if ".metadata" `T.isSuffixOf` decodeUtf8 (capPath request) then if corrupt then "changed" else "metadata" else toLazy tarballBytes)) $ \moved -> do
                        fixture <- managedFixture (stubBaseUrl original) True
                        let declared location = do
                                inventory <- managedInventory location True [1, 2, 3]
                                let details = inventoryDetails inventory
                                    files = fmap (\file -> file{artCoreMetadata = Just (MetadataDigests (Map.singleton "sha256" (hexSha256Of "metadata")))}) (pkgArtifacts details)
                                expectRight (restoreMirrorInventory details{pkgArtifacts = files} (mirrorSource files "declared companion"))
                        initial <- declared (stubBaseUrl original)
                        writeIORef (mfSource fixture) (VersionPresent (sourceDocument initial) Nothing)
                        runManaged fixture >>= (`shouldSatisfy` isRetried)
                        readIORef (mfCompanions fixture) `shouldReturn` Map.empty
                        changed <- declared (stubBaseUrl moved)
                        writeIORef (mfSource fixture) (VersionPresent (sourceDocument changed) Nothing)
                        outcome <- runManaged fixture
                        if corrupt
                            then do
                                outcome `shouldSatisfy` isDeadLettered
                                readIORef (mfCompanions fixture) `shouldReturn` Map.empty
                                readIORef (mfUploads fixture) `shouldReturn` [mirrorFilename 1]
                            else do
                                outcome `shouldBe` Succeeded
                                readIORef (mfUploads fixture) `shouldReturn` map mirrorFilename [1, 2, 3]
                                record <- readIORef (mfRecord fixture)
                                let companions = versionCompanions <$> (record >>= recordVersion mirrorVersion)
                                fmap (notElem CompanionPending . Map.elems) companions `shouldBe` Just True
                        captured <- allCaptured moved
                        captured `shouldSatisfy` any ((".metadata" `T.isSuffixOf`) . decodeUtf8 . capPath)

        it "refuses a moved source whose current identity conflicts with the retained files" $
            withStub status404 "unavailable" $ \original ->
                withStub status200 (toLazy tarballBytes) $ \moved -> do
                    fixture <- managedFixture (stubBaseUrl original) False
                    runManaged fixture >>= (`shouldSatisfy` isRetried)
                    incoming <- managedInventory (stubBaseUrl moved) False [1, 2, 3]
                    let details = inventoryDetails incoming
                        files = fmap (\file -> file{artHashes = [unsafeHash SHA256 (hexSha256Of "changed bytes")]}) (pkgArtifacts details)
                    changed <- expectRight (restoreMirrorInventory details{pkgArtifacts = files} (mirrorSource files "changed identity"))
                    writeIORef (mfSource fixture) (VersionPresent (sourceDocument changed) Nothing)
                    runManaged fixture >>= (`shouldSatisfy` isDropped)
                    allCaptured moved `shouldReturn` []

        it "does not revive an old source when current admission refuses every moved location" $
            withStub status404 "unavailable" $ \original ->
                withStub status200 (toLazy tarballBytes) $ \moved -> do
                    fixture <- managedFixture (stubBaseUrl original) False
                    runManaged fixture >>= (`shouldSatisfy` isRetried)
                    changed <- managedInventory (stubBaseUrl moved) False [1, 2, 3]
                    writeIORef (mfSource fixture) (VersionPresent (sourceDocument changed) Nothing)
                    priorRequests <- allCaptured original
                    let policy = (mfPolicy fixture){wpArtifactHostHonoured = (== hostPortAddress (stubBaseUrl original))}
                    runManaged fixture{mfPolicy = policy} >>= (`shouldSatisfy` isDropped)
                    allCaptured original `shouldReturn` priorRequests
                    allCaptured moved `shouldReturn` []

        it "preserves missing historic files when the new public snapshot omits them" $
            withUpstream $ \url -> do
                fixture <- managedFixture url False
                writeIORef (mfRejectFile fixture) (Just (mirrorFilename 2))
                runManaged fixture >>= (`shouldSatisfy` isRetried)
                reduced <- managedInventory url False [1]
                writeIORef (mfSource fixture) (VersionPresent (sourceDocument reduced) Nothing)
                writeIORef (mfRejectFile fixture) Nothing
                runManaged fixture `shouldReturn` Succeeded
                readIORef (mfUploads fixture) `shouldReturn` map mirrorFilename [1, 2, 2, 3]

        it "refuses first-party work before the target or state probe" $ do
            fixture <- managedFixture unreachableUrl False
            let policy = (mfPolicy fixture){wpFirstParty = const True}
            runManaged fixture{mfPolicy = policy} >>= (`shouldSatisfy` isDropped)
            readIORef (mfProbes fixture) `shouldReturn` 0

        it "reserves managed memory before reading native or retained inventories" $ do
            fixture <- managedFixture unreachableUrl False
            let binding = (mfBinding fixture){wmsWithinBudget = const (pure Nothing)}
                policy = (mfPolicy fixture){wpMirrorState = Just binding}
            runManaged fixture{mfPolicy = policy} >>= (`shouldSatisfy` isRetried)
            readIORef (mfProbes fixture) `shouldReturn` 0

    describe "outcomeOfFetchFault (the worker's one retry-versus-drop table)" $ do
        -- One table serves the artifact fetch and the mirror write, so no exchange fault can
        -- drop on one leg and retry on the other.
        it "drops an unformable URL (a redelivery re-forms the same URL from the same inputs)" $
            outcomeOfFetchFault BeforePublish renderFault (FetchUrlUnformable EmptyBaseUrl)
                `shouldBe` Dropped "unformable"

        -- An over-bound response is terminal, so the backend dead-letters it. Treating every
        -- fetch Left as a retry would redeliver a deterministically over-cap tarball forever.
        it "dead-letters an over-bound response (it can never succeed, so it rides the terminus)" $
            outcomeOfFetchFault BeforePublish renderFault (FetchBoundExceeded (BodyTooLarge (MirrorArtifactBodyLimit 1024)))
                `shouldBe` DeadLettered "over the bound"

        it "retries a transport fault (a redelivery may succeed)" $
            outcomeOfFetchFault BeforePublish renderFault (FetchTransport (transportFault TransportUnreachable "connection reset"))
                `shouldBe` Retried BeforePublish "transport failed"

        it "carries the caller's leg, so only a failed publish releases the message's visibility" $
            -- The cause is shared between the fetch and the publish; the disposition is not.
            outcomeOfFetchFault AfterPublish renderFault (FetchTransport (transportFault TransportUnreachable "connection reset"))
                `shouldBe` Retried AfterPublish "transport failed"

    describe "outcomeOfAdmission (the shared admission verdict, split on the shared transience)" $ do
        -- 'Ecluse.Core.Package.Admission.admissionTransience' is the one input to the split, and
        -- the serve gate reads it to choose a 503 over a 500. The two cannot disagree.
        it "retries an inability the evaluator expects to clear (an advisory source briefly down)" $
            case outcomeOfAdmission sampleMirrorJob (undecided (WillResolve Nothing) NoDatabaseLoaded) of
                Left (Retried _ reason) -> reason `shouldSatisfy` T.isInfixOf "DenyIfCve: no advisory database loaded"
                other -> expectationFailure ("expected a retry for a clearing inability, got " <> show other)

        it "drops an inability no retry can clear, rather than redelivering until the budget retires it" $
            -- WontResolve is the rule engine's own statement that no redelivery changes the
            -- verdict. A repaired advisory source rides the next request's enqueue instead.
            case outcomeOfAdmission sampleMirrorJob (undecided WontResolve (RuleThrew "the advisory index is corrupt")) of
                Left (Dropped reason) -> reason `shouldSatisfy` T.isInfixOf "the advisory index is corrupt"
                other -> expectationFailure ("expected a drop for an unclearable inability, got " <> show other)

    describe "processJob -- the integrity gate" $ do
        it "names the mirror-artifact bound when the buffered download is too large" $
            withUpstream $ \url ->
                withRuntimePolicies (withArtifactCap 8 admitPolicies) noopWorkerMetricsPort (Right ()) $ \runtime queue _ -> do
                    job <- enqueueAndReceive queue sampleMirrorJob
                    outcome <- runWM (sourceAt url runtime) (processJob job)
                    outcome `shouldBe` DeadLettered "artifact exceeded the response bound: BodyTooLarge (MirrorArtifactBodyLimit 8)"

        -- The worker recomputes whichever digest current metadata carries, so an artifact the
        -- floor admitted is never admit-but-uncomputable.
        for_ soleAdmittedDigests $ \(label, hash) ->
            it ("publishes a version whose current metadata carries only " <> label) $
                withUpstream $ \url ->
                    withRuntimePolicies (admitPoliciesWithDigests [hash]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                        job <- enqueueAndReceive queue sampleMirrorJob
                        outcome <- runWM (sourceAt url runtime) (processJob job)
                        outcome `shouldBe` Succeeded
                        published <- plDocuments <$> readIORef logRef
                        length published `shouldBe` 1

        it "refuses to publish (no publish) when the bytes do not match the re-admitted digest" $
            withUpstream $ \url ->
                -- The admitted digest names different bytes. The queue payload carries
                -- no digest that could weaken the publication gate.
                withRuntimePolicies (admitPoliciesWithDigests [unsafeHash SRI falseSri]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                    job <- enqueueAndReceive queue sampleMirrorJob
                    outcome <- runWM (sourceAt url runtime) (processJob job)
                    outcome `shouldSatisfy` isDropped
                    published <- plDocuments <$> readIORef logRef
                    published `shouldBe` []

        it "hands the publish step the re-admitted descriptor exactly" $
            -- The queue payload names the artifact by filename only. Every field of the trusted-
            -- tier publish document must come from current metadata.
            withUpstream $ \url ->
                withRuntime (Right ()) $ \runtime queue logRef -> do
                    job <- enqueueAndReceive queue sampleMirrorJob
                    outcome <- runWM (sourceAt url runtime) (processJob job)
                    outcome `shouldBe` Succeeded
                    descriptors <- plArtifacts <$> readIORef logRef
                    descriptors
                        `shouldBe` [ MirrorArtifact
                                        { maFilename = unsafeFilename "thing-1.0.0.tgz"
                                        , maHashes = unsafeHash SRI trueSri :| []
                                        , maSize = Nothing
                                        }
                                   ]

        it "leaves the job for redelivery on a transient fetch failure (no publish)" $
            -- An unreachable upstream (connection refused) is a transient fault, not a terminal
            -- one.
            withRuntime (Right ()) $ \runtime queue logRef -> do
                job <- enqueueAndReceive queue sampleMirrorJob
                outcome <- runWM (sourceAt unreachableUrl runtime) (processJob job)
                outcome `shouldSatisfy` isRetried
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "renders a failed artifact fetch without the URL's userinfo, path, or query" $
            -- The Retried reason reaches logs and spans. It may name the authority
            -- and bounded transport cause, never the credential-bearing location.
            withRuntime (Right ()) $ \runtime queue _logRef -> do
                job <- enqueueAndReceive queue sampleMirrorJob
                outcome <- runWM (sourceAt credentialBearingUnreachableUrl runtime) (processJob job)
                case outcome of
                    Retried _ reason -> do
                        reason `shouldSatisfy` T.isInfixOf "127.0.0.1:1"
                        reason `shouldSatisfy` (not . T.isInfixOf "hunter2")
                        reason `shouldSatisfy` (not . T.isInfixOf "sig=abc")
                        reason `shouldSatisfy` (not . T.isInfixOf "/x")
                    other -> expectationFailure ("expected a Retried outcome from the refused connect, got " <> show other)

        it "treats a registry rejection as retryable (job left for redelivery)" $
            withUpstream $ \url ->
                withRuntime (Left (PublishRejected (PublishError "503"))) $ \runtime queue _logRef -> do
                    job <- enqueueAndReceive queue sampleMirrorJob
                    outcome <- runWM (sourceAt url runtime) (processJob job)
                    outcome `shouldSatisfy` isRetried

        it "renders a transport fault's retry reason with the prefix exactly once" $
            -- The consumer renders the reason prefix, once. A fault carrying the prefix as raw text
            -- would make the consumer add it again, doubling it in the log line.
            withUpstream $ \url ->
                withRuntime (Left (PublishFetch (FetchTransport (transportFault TransportUnreachable "connection refused")))) $ \runtime queue _logRef -> do
                    job <- enqueueAndReceive queue sampleMirrorJob
                    outcome <- runWM (sourceAt url runtime) (processJob job)
                    case outcome of
                        Retried _ reason -> do
                            reason `shouldSatisfy` T.isInfixOf "connection refused"
                            T.count "publish transport failure:" reason `shouldBe` 1
                        other -> expectationFailure ("expected a Retried transport outcome, got " <> show other)

        it "drops a job (non-retryable) when the artifact URL is unformable (no publish)" $
            -- A redelivery re-forms the same unformable URL from the same job payload, so the
            -- worker retires the job, the same verdict the publish leg reaches below.
            withRuntime (Right ()) $ \runtime queue logRef -> do
                job <- enqueueAndReceive queue sampleMirrorJob
                outcome <- runWM (sourceAt unformableUrl runtime) (processJob job)
                outcome `shouldSatisfy` isDropped
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "drops a job (non-retryable) when the publish URL is unformable (a config fault)" $
            -- Redelivery cannot repair an unformable publish URL. Treat it as
            -- a configuration fault rather than a retryable rejection.
            withUpstream $ \url ->
                withRuntime (Left (PublishFetch (FetchUrlUnformable EmptyBaseUrl))) $ \runtime queue _logRef -> do
                    job <- enqueueAndReceive queue sampleMirrorJob
                    outcome <- runWM (sourceAt url runtime) (processJob job)
                    outcome `shouldSatisfy` isDropped
    describe "processJob: ingest-time policy re-evaluation" $ do
        for_ [("an npm", Npm, sampleMirrorJob), ("a PyPI", PyPI, pypiMirrorJob)] $ \(label, eco, jobAt) ->
            it ("drops " <> label <> " job newly MAL-denied before fetching or publishing") $ do
                let deps = servingRuleDeps (DbEtag "mal-generation") (fakeCveLookup [("thing", AdvisoryRange "MAL-worker" Nothing (Just "1.0.0") (LastAffected "1.0.0") Nothing)])
                rules <- prepare deps [atDefaultPrecedence (DenyIfKnownMalicious FailDeny), atDefaultPrecedence (AllowIfOlderThan 0)]
                withRuntimePolicies (Map.singleton eco (npmPolicy presentResolver rules)) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                    job <- enqueueAndReceive queue jobAt
                    runWM (sourceAt unreachableUrl runtime) (processJob job) >>= \case
                        Dropped reason -> reason `shouldSatisfy` T.isInfixOf "MAL-worker (known malicious)"
                        other -> expectationFailure ("expected MAL denial, got " <> show other)
                    plDocuments <$> readIORef logRef `shouldReturn` []

        for_ [False, True] $ \poisonEvidence ->
            it ("names a pure rule exception without fetching or publishing, evidence=" <> show poisonEvidence) $ do
                let thrown = TestContractEscape "pure worker verdict"
                    resolver name version =
                        pure (VersionPresent (versionDocOf (sampleDetails name version){pkgPublishedAt = Just (impureThrow thrown)}) Nothing)
                rules <-
                    if poisonEvidence
                        then prepare inertRuleDeps [atDefaultPrecedence (AllowIfOlderThan 0)]
                        else pure [constRule "PureBomb" (impureThrow thrown)]
                let policies = npmPolicies (if poisonEvidence then resolver else presentResolver) rules
                    name = if poisonEvidence then "AllowIfOlderThan" else "PureBomb"
                withRuntimePolicies policies noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                    job <- enqueueAndReceive queue sampleMirrorJob
                    runWM (sourceAt unreachableUrl runtime) (processJob job) >>= \case
                        Retried BeforePublish reason -> reason `shouldSatisfy` T.isInfixOf (name <> ": the rule threw: " <> show thrown)
                        other -> expectationFailure ("expected a named rule retry, got " <> show other)
                    plDocuments <$> readIORef logRef `shouldReturn` []

        -- Every row drives 'unreachableUrl': skipping the re-evaluation would surface a Retried
        -- from the artifact fetch instead of the verdict the row names.
        for_ reEvaluationVerdicts $ \(label, policies, expected) ->
            it label $
                withRuntimePolicies policies noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                    job <- enqueueAndReceive queue sampleMirrorJob
                    outcome <- runWM (sourceAt unreachableUrl runtime) (processJob job)
                    outcome `shouldSatisfy` expected
                    published <- plDocuments <$> readIORef logRef
                    published `shouldBe` []

        -- Ingest counts as admission, so a deny the Dredger does not read still keeps the version
        -- out of the store. The rules that apply at revocation would admit it and reach the fetch.
        for_ [("an npm", Npm, sampleMirrorJob), ("a PyPI", PyPI, pypiMirrorJob)] $ \(label, eco, jobAt) ->
            it ("drops " <> label <> " job whose version a deny limited to admission refuses, without publishing") $ do
                rules <- refusedAtAdmissionOnly
                withRuntimePolicies (Map.singleton eco (npmPolicy presentResolver rules)) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                    job <- enqueueAndReceive queue jobAt
                    runWM runtime (processJob job) >>= \case
                        Dropped reason -> reason `shouldSatisfy` T.isInfixOf "blocked by DenyByIdentity"
                        other -> expectationFailure ("expected the deny to drop the job, got " <> show other)
                    published <- plDocuments <$> readIORef logRef
                    published `shouldBe` []

        for_ [(Npm, Deprecated, DenyIfDeprecated, sampleMirrorJob), (PyPI, Yanked, DenyIfYanked, pypiMirrorJob)] $ \(eco, flag, rule, jobAt) ->
            it ("drops a queued version refused by admission-only " <> toString (ruleName rule)) $ do
                rules <- prepare inertRuleDeps [admissionOnly (atDefaultPrecedence rule), atDefaultPrecedence (AllowIfOlderThan 0)]
                let resolver name version = pure (VersionPresent (versionDocOf (sampleDetails name version){pkgAvailability = flag}) Nothing)
                withRuntimePolicies (Map.singleton eco (npmPolicy resolver rules)) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                    job <- enqueueAndReceive queue jobAt
                    runWM runtime (processJob job) >>= \case
                        Dropped reason -> reason `shouldSatisfy` T.isInfixOf ("blocked by " <> ruleName rule)
                        other -> expectationFailure ("expected availability refusal, got " <> show other)
                    plDocuments <$> readIORef logRef `shouldReturn` []

        it "hands the publish step the version object current metadata carried at admission, never the enqueue-time one" $
            withUpstream $ \url ->
                withRuntimePolicies (npmPolicies (resolverCarrying admissionObject) [admitRule]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                    job <- enqueueAndReceive queue sampleMirrorJob
                    runWM (sourceAt url runtime) (processJob job) `shouldReturn` Succeeded
                    publications <- plPublications <$> readIORef logRef
                    decoded <- traverse decodeJsonOrFail publications
                    map (\document -> fieldAt "versions" document >>= fieldAt "1.0.0" >>= fieldAt "marker") decoded `shouldBe` [Just (String "admission-time")]

        it "refuses, apart from a policy deny, a job whose re-admission carries no source version object, without publishing" $
            withUpstream $ \url ->
                withRuntimePolicies (npmPolicies (\name version -> pure (VersionPresent (versionDocOf (sampleDetails name version)) Nothing)) [admitRule]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                    job <- enqueueAndReceive queue sampleMirrorJob
                    outcome <- runWM (sourceAt url runtime) (processJob job)
                    outcome `shouldSatisfy` isSourceUnavailable
                    outcome `shouldSatisfy` (not . isDropped)
                    plans <- plPlans <$> readIORef logRef
                    plans `shouldBe` []

        it "reports invalid source metadata as source-unavailable before publication" $
            withUpstream $ \url ->
                withStub status404 "" $ \mirror -> withSweeper $ \deadlines -> do
                    publish <- codecPublishAt deadlines (stubBaseUrl mirror)
                    withRuntimeRegistry (const publish) (npmPolicies (resolverCarrying (fst npmCached (String "not an object"))) [admitRule]) noopWorkerMetricsPort $ \runtime queue _logRef -> do
                        job <- enqueueAndReceive queue sampleMirrorJob
                        outcome <- runWM (sourceAt url runtime) (processJob job)
                        outcome `shouldSatisfy` isSourceUnavailable
                        outcome `shouldSatisfy` (not . isDropped)
                        -- The inventory probe reached the target; the write never did.
                        written <- filter (== "PUT") . map capMethod <$> allCaptured mirror
                        written `shouldBe` []

        it "refuses malformed source metadata before downloading the artifact" $
            withStub status200 (toLazy tarballBytes) $ \upstream ->
                withStub status404 "" $ \mirror -> withSweeper $ \deadlines -> do
                    publish <- codecPublishAt deadlines (stubBaseUrl mirror)
                    let policies = npmPolicies (resolverCarrying (fst npmCached (String "not an object"))) [admitRule]
                    withRuntimeRegistry (const publish) policies noopWorkerMetricsPort $ \runtime queue _ -> do
                        queued <- enqueueAndReceive queue sampleMirrorJob
                        runWM (sourceAt (stubBaseUrl upstream) runtime) (processJob queued) >>= (`shouldSatisfy` isSourceUnavailable)
                        allCaptured upstream `shouldReturn` []
                        written <- filter (== "PUT") . map capMethod <$> allCaptured mirror
                        written `shouldBe` []

        it "lets no carried version object reach the publish step once current policy denies the version" $
            withRuntimePolicies (npmPolicies (resolverCarrying admissionObject) [denyRule]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                job <- enqueueAndReceive queue sampleMirrorJob
                outcome <- runWM (sourceAt unreachableUrl runtime) (processJob job)
                outcome `shouldSatisfy` isDropped
                plans <- plPlans <$> readIORef logRef
                plans `shouldBe` []

        it "lets no carried version object reach the publish step once the bytes fail the integrity gate" $
            withUpstream $ \url ->
                withRuntimePolicies (npmPolicies (resolverCarryingWith sampleArtifact{artHashes = [unsafeHash SRI falseSri]} admissionObject) [admitRule]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                    job <- enqueueAndReceive queue sampleMirrorJob
                    outcome <- runWM (sourceAt url runtime) (processJob job)
                    outcome `shouldSatisfy` isDropped
                    plans <- plPlans <$> readIORef logRef
                    plans `shouldBe` []

        it "mirrors an admitted version when the v2 trigger omits a filename" $
            withUpstream $ \url ->
                withRuntime (Right ()) $ \runtime queue logRef -> do
                    job <- enqueueAndReceive queue sampleMirrorJob{jobTriggerFilename = Nothing}
                    runWM (sourceAt url runtime) (processJob job) `shouldReturn` Succeeded
                    length . plDocuments <$> readIORef logRef `shouldReturn` 1

        for_ [False, True] $ \changedBytes ->
            it (if changedBytes then "keeps integrity verification when an advisory check is skipped" else "mirrors an age-admitted version and logs its skipped advisory check") $ do
                rules <- prepare inertRuleDeps (map atDefaultPrecedence [DenyIfCve (DenyIfCveParams 8 FailNoDecision), AllowIfOlderThan (7 * nominalDay)])
                let artifact = if changedBytes then sampleArtifact{artHashes = [unsafeHash SRI falseSri]} else sampleArtifact
                    resolve name version = pure (VersionPresent (sourceVersionDoc ((sampleDetails name version){pkgArtifacts = artifact :| [], pkgPublishedAt = Just (UTCTime (fromGregorian 2000 1 1) 0)})) Nothing)
                    policies = Map.map (\policy -> policy{wpNow = pure (UTCTime (fromGregorian 2026 1 1) 0)}) (npmPolicies resolve rules)
                withUpstream $ \url ->
                    withRuntimePolicies policies noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                        job <- enqueueAndReceive queue sampleMirrorJob
                        (outcome, logs) <- captureJsonLog (\logEnv -> runWMWith logEnv (sourceAt url runtime) (processJob job))
                        if changedBytes then outcome `shouldSatisfy` isDropped else outcome `shouldBe` Succeeded
                        length . plDocuments <$> readIORef logRef `shouldReturn` (if changedBytes then 0 else 1)
                        logs `shouldSatisfy` T.isInfixOf "admitted with a check skipped for unavailability"
                        logs `shouldSatisfy` T.isInfixOf "DenyIfCve"
                        logs `shouldSatisfy` T.isInfixOf "no advisory database loaded"

        it "does not mirror when a skipped check has no winning allow" $ do
            rules <- prepare inertRuleDeps [atDefaultPrecedence (DenyIfCve (DenyIfCveParams 8 FailNoDecision))]
            withRuntimePolicies (npmPolicies presentResolver rules) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                job <- enqueueAndReceive queue sampleMirrorJob
                (outcome, logs) <- captureJsonLog (\logEnv -> runWMWith logEnv runtime (processJob job))
                outcome `shouldSatisfy` isDropped
                plDocuments <$> readIORef logRef `shouldReturn` []
                logs `shouldSatisfy` (not . T.isInfixOf "admitted with a check skipped for unavailability")

        it "publishes a job whose version current policy admits (happy path unregressed)" $
            withUpstream $ \url ->
                withRuntimePolicies (npmPolicies presentResolver [admitRule]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                    job <- enqueueAndReceive queue sampleMirrorJob
                    (outcome, logs) <- captureJsonLogAt InfoS (\logEnv -> runWMWith logEnv (sourceAt url runtime) (processJob job))
                    outcome `shouldBe` Succeeded
                    published <- plDocuments <$> readIORef logRef
                    length published `shouldBe` 1
                    case filter (T.isInfixOf "mirrored artifact published") (lines logs) of
                        [entry] -> do
                            entry `shouldSatisfy` T.isInfixOf "\"event\":\"mirror_publication\""
                            entry `shouldSatisfy` T.isInfixOf "\"target\":\"mirrorTarget\""
                            entry `shouldSatisfy` T.isInfixOf "\"digest\":{"
                            entry `shouldSatisfy` T.isInfixOf (hexSha256Of tarballBytes)
                            entry `shouldSatisfy` T.isInfixOf "was approved by test-admit"
                            entry `shouldSatisfy` T.isInfixOf "\"rule\":\"test-admit\""
                        other -> expectationFailure ("expected one publication audit, got " <> show other)

        it "fetches through the request formation keyed by the job's own ecosystem" $
            -- The PyPI builder refuses every request. The npm job must use its
            -- own ecosystem's builder, without consulting that decoy.
            withUpstream $ \url -> do
                let base = npmPolicy presentResolver [admitRule]
                    refusing = base{wpArtifact = (wpArtifact base){artifactByUrl = \_ _ -> Left EmptyBaseUrl}}
                    policies = Map.insert PyPI refusing (npmPolicies presentResolver [admitRule])
                withRuntimePolicies policies noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                    job <- enqueueAndReceive queue sampleMirrorJob
                    outcome <- runWM (sourceAt url runtime) (processJob job)
                    outcome `shouldBe` Succeeded
                    published <- plDocuments <$> readIORef logRef
                    length published `shouldBe` 1

        it "publishes through the publish capability keyed by the job's own ecosystem" $
            -- The decoy PyPI bundle carries its own recording publish capability. The npm job's
            -- probe and publish must both ride npm's, so the decoy records nothing.
            withUpstream $ \url -> do
                npmLog <- newIORef emptyPublishLog
                decoyLog <- newIORef emptyPublishLog
                let npmBundle = (npmPolicy presentResolver [admitRule]){wpPublish = recordingPublish npmLog (Right ())}
                    decoyBundle = (npmPolicy presentResolver [admitRule]){wpPublish = recordingPublish decoyLog (Right ())}
                    policies = Map.fromList [(Npm, npmBundle), (PyPI, decoyBundle)]
                queue <- newTestMemoryQueue
                withWiredRuntime queue policies noopWorkerMetricsPort $ \runtime -> do
                    job <- enqueueAndReceive queue sampleMirrorJob
                    outcome <- runWM (sourceAt url runtime) (processJob job)
                    outcome `shouldBe` Succeeded
                    published <- plDocuments <$> readIORef npmLog
                    length published `shouldBe` 1
                    decoyPublished <- plDocuments <$> readIORef decoyLog
                    decoyPublished `shouldBe` []

        it "drops the job when the job ecosystem's own request formation refuses the URL, without publishing" $
            -- The npm bundle's own builder refuses, with no other builder to fall back to. A
            -- redelivery would refuse identically, so the worker retires the job.
            withRuntimePolicies (withArtifactRequest (\_ _ -> Left EmptyBaseUrl) (npmPolicies presentResolver [admitRule])) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                job <- enqueueAndReceive queue sampleMirrorJob
                outcome <- runWM (sourceAt unreachableUrl runtime) (processJob job)
                case outcome of
                    Dropped reason -> reason `shouldSatisfy` T.isInfixOf "unformable artifact URL"
                    other -> expectationFailure ("expected a Dropped outcome from the refused request formation, got " <> show other)
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "drops a job whose current source artifact host the policy refuses" $
            withRuntimePolicies (withHostGate (const False) (npmPolicies presentResolver [admitRule])) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                job <- enqueueAndReceive queue sampleMirrorJob
                outcome <- runWM (sourceAt unreachableUrl runtime) (processJob job)
                outcome `shouldSatisfy` isDropped
                -- The reason names the authority the fetch would dial, never the URL:
                -- a source location can carry userinfo or a signed query.
                case outcome of
                    Dropped reason -> do
                        reason `shouldSatisfy` T.isInfixOf "127.0.0.1:1"
                        reason `shouldSatisfy` (not . T.isInfixOf "/thing/-/thing-1.0.0.tgz")
                    other -> expectationFailure ("expected a Dropped outcome, got " <> show other)
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

    describe "processJob: the first-party privilege" $ do
        -- A namespace declared after the enqueue, so the queue still holds a job for a name the
        -- deployment now owns. 'refusingResolver' throws if the metadata re-fetch is reached.
        let ownedPolicies = withFirstParty (const True) (npmPolicies refusingResolver [admitRule])

        it "drops a job whose name the deployment owns, making no public request" $
            -- 'unreachableUrl' would surface a Retried had the artifact bytes been fetched.
            withRuntimePolicies ownedPolicies noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                job <- enqueueAndReceive queue sampleMirrorJob
                outcome <- runWM (sourceAt unreachableUrl runtime) (processJob job)
                -- The reason is the audit line the terminal path logs, so it names the package,
                -- the version, and why the job was retired.
                case outcome of
                    Dropped reason -> do
                        reason `shouldSatisfy` T.isInfixOf "thing@1.0.0"
                        reason `shouldSatisfy` T.isInfixOf "first-party"
                    other -> expectationFailure ("expected a Dropped outcome for a first-party name, got " <> show other)
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "drops a first-party job the mirror target already lists, rather than acking it as present" $
            -- The privilege is read ahead of the dedup probe, so a name the deployment owns can
            -- never take the already-mirrored short circuit and report success.
            withRuntimeRegistry (\logRef -> mirrorListingPublish logRef (Right ()) [ver]) ownedPolicies noopWorkerMetricsPort $ \runtime queue logRef -> do
                job <- enqueueAndReceive queue sampleMirrorJob
                outcome <- runWM (sourceAt unreachableUrl runtime) (processJob job)
                outcome `shouldSatisfy` isDropped
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "mirrors a job whose name the deployment does not own (deny by default, unchanged)" $
            -- The control case: the same wiring under a predicate that owns nothing still
            -- re-evaluates, fetches, and publishes.
            withUpstream $ \url ->
                withRuntimePolicies (withFirstParty (const False) admitPolicies) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                    job <- enqueueAndReceive queue sampleMirrorJob
                    outcome <- runWM (sourceAt url runtime) (processJob job)
                    outcome `shouldBe` Succeeded
                    published <- plDocuments <$> readIORef logRef
                    length published `shouldBe` 1

    describe "processJob: the mirror inventory probe" $ do
        -- The default 'recordingPublish' answers 404, the known-empty store every other test
        -- in this file rides. These cover the present skip and the unreadable arms.
        it "acks a complete native npm version during public outage without new admission" $
            -- 'unreachableUrl' doubles as the no-fetch guard: were the probe's skip not
            -- taken, the artifact fetch would surface a Retried, not this Succeeded.
            withRuntimeRegistry (\logRef -> mirrorListingPublish logRef (Right ()) [ver]) (npmPolicies (\_ _ -> pure VersionMetadataUnavailable) [denyRule]) noopWorkerMetricsPort $ \runtime queue logRef -> do
                job <- enqueueAndReceive queue sampleMirrorJob
                (outcome, logs) <- captureJsonLogAt InfoS (\logEnv -> runWMWith logEnv (sourceAt unreachableUrl runtime) (processJob job))
                outcome `shouldBe` Succeeded
                logs `shouldBe` ""
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        for_ [("digestless", \details -> details{pkgArtifacts = fmap (\file -> file{artHashes = []}) (pkgArtifacts details)}), ("multiple files", \details -> details{pkgArtifacts = pkgArtifacts details <> fmap (\file -> file{artFilename = "another.tgz"}) (pkgArtifacts details)})] $ \(label, change) ->
            it ("does not acknowledge " <> label <> " native npm inventory during public outage")
                $ withRuntimeRegistry
                    (\logRef -> (recordingPublish logRef (Right ())){mpProbeInventory = \_ name -> pure (Right (PackageInfo name (Map.singleton (renderVersion ver) (change (sampleDetails name ver))) mempty []))})
                    (npmPolicies (\_ _ -> pure VersionMetadataUnavailable) [admitRule])
                    noopWorkerMetricsPort
                $ \runtime queue logRef -> do
                    job <- enqueueAndReceive queue sampleMirrorJob
                    runWM runtime (processJob job) >>= (`shouldSatisfy` isRetried)
                    plDocuments <$> readIORef logRef `shouldReturn` []

        it "retries without publishing when the probe cannot reach the mirror" $
            -- The release tag is chosen over the inventory, so an unreachable mirror is a
            -- transient fault. Publishing anyway would declare a tag decided without it.
            withUpstream $ \url ->
                withRuntimeRegistry (`probeUnreachablePublish` Right ()) admitPolicies noopWorkerMetricsPort $ \runtime queue logRef -> do
                    job <- enqueueAndReceive queue sampleMirrorJob
                    outcome <- runWM (sourceAt url runtime) (processJob job)
                    outcome `shouldSatisfy` isRetried
                    published <- plDocuments <$> readIORef logRef
                    published `shouldBe` []

        it "retries without publishing when the mirror's answer does not project" $
            withUpstream $ \url ->
                withRuntimeRegistry (`probeUnreadablePublish` Right ()) admitPolicies noopWorkerMetricsPort $ \runtime queue logRef -> do
                    job <- enqueueAndReceive queue sampleMirrorJob
                    outcome <- runWM (sourceAt url runtime) (processJob job)
                    outcome `shouldSatisfy` isRetried
                    published <- plDocuments <$> readIORef logRef
                    published `shouldBe` []

        it "retries without publishing when the mirror refuses the inventory probe" $
            -- A readable status that is neither success nor an absence leaves the inventory
            -- unknown, so the tag cannot be chosen and the job waits for a redelivery.
            withUpstream $ \url ->
                withRuntimeRegistry (`probeRefusingPublish` Right ()) admitPolicies noopWorkerMetricsPort $ \runtime queue logRef -> do
                    job <- enqueueAndReceive queue sampleMirrorJob
                    outcome <- runWM (sourceAt url runtime) (processJob job)
                    outcome `shouldSatisfy` isRetried
                    recorded <- readIORef logRef
                    plDocuments recorded `shouldBe` []
                    plPlans recorded `shouldBe` []

        it "dead-letters without publishing when the inventory probe overruns the bound" $
            -- The probe reads the shared terminal-versus-transient table, so an over-bound
            -- answer rides the dead-letter terminus rather than redelivering for ever.
            withUpstream $ \url ->
                withRuntimeRegistry (`probeOverboundPublish` Right ()) admitPolicies noopWorkerMetricsPort $ \runtime queue logRef -> do
                    job <- enqueueAndReceive queue sampleMirrorJob
                    outcome <- runWM (sourceAt url runtime) (processJob job)
                    outcome `shouldSatisfy` isDeadLettered
                    recorded <- readIORef logRef
                    plDocuments recorded `shouldBe` []
                    plPlans recorded `shouldBe` []

        it "falls through when the mirror lists other versions but not this one" $
            -- The worker judges presence per version: a package already partially
            -- mirrored must still mirror its missing versions.
            withUpstream $ \url ->
                withRuntimeRegistry (\logRef -> mirrorListingPublish logRef (Right ()) [otherVer]) admitPolicies noopWorkerMetricsPort $ \runtime queue logRef -> do
                    job <- enqueueAndReceive queue sampleMirrorJob
                    outcome <- runWM (sourceAt url runtime) (processJob job)
                    outcome `shouldBe` Succeeded
                    published <- plDocuments <$> readIORef logRef
                    length published `shouldBe` 1

    describe "mirrorLatest: the release tag one mirror write declares" $ do
        -- The shared selector decides it, so these pin the mirror's own arguments: the
        -- upstream tag as chosen, and the store's post-write inventory as survivors.
        it "keeps a present upstream latest, whatever this job publishes" $
            -- 2.0.0 mirrored first, then an older 1.0.0 job: completion order must not retag.
            mirrorLatest (Just (npmVersion "2.0.0")) [npmVersion "2.0.0"] (npmVersion "1.0.0")
                `shouldBe` npmVersion "2.0.0"

        it "converges on the same tag when the two jobs complete in the reverse order" $ do
            -- 1.0.0 lands first with its target absent, so it is the only version to name.
            mirrorLatest (Just (npmVersion "2.0.0")) [] (npmVersion "1.0.0") `shouldBe` npmVersion "1.0.0"
            mirrorLatest (Just (npmVersion "2.0.0")) [npmVersion "1.0.0"] (npmVersion "2.0.0") `shouldBe` npmVersion "2.0.0"

        it "falls back to the highest stable version when the upstream target is not mirrored" $
            mirrorLatest (Just (npmVersion "9.9.9")) [npmVersion "2.0.0"] (npmVersion "1.0.0")
                `shouldBe` npmVersion "2.0.0"

        it "falls back the same way when no upstream latest is known" $
            mirrorLatest Nothing [npmVersion "2.0.0"] (npmVersion "1.0.0") `shouldBe` npmVersion "2.0.0"

        it "prefers a stable version over a higher prerelease" $
            mirrorLatest Nothing [npmVersion "2.0.0"] (npmVersion "3.0.0-beta.1") `shouldBe` npmVersion "2.0.0"

        it "names a prerelease only when no stable version is present" $
            mirrorLatest Nothing [npmVersion "3.0.0-beta.1"] (npmVersion "3.0.0-beta.2")
                `shouldBe` npmVersion "3.0.0-beta.2"

        it "keeps an explicit upstream prerelease latest that is present" $
            mirrorLatest (Just (npmVersion "3.0.0-beta.1")) [npmVersion "2.0.0"] (npmVersion "3.0.0-beta.1")
                `shouldBe` npmVersion "3.0.0-beta.1"

        it "names the published version when the store held nothing" $
            mirrorLatest Nothing [] (npmVersion "1.0.0") `shouldBe` npmVersion "1.0.0"

    describe "processJob: the release tag the write declares" $ do
        it "declares the upstream's latest, not the version this job publishes" $
            withUpstream $ \url ->
                withRuntimeRegistry
                    (\logRef -> mirrorListingPublish logRef (Right ()) [otherVer])
                    (npmPolicies (taggedResolver (Just otherVer)) [admitRule])
                    noopWorkerMetricsPort
                    $ \runtime queue logRef -> do
                        job <- enqueueAndReceive queue sampleMirrorJob
                        runWM (sourceAt url runtime) (processJob job) `shouldReturn` Succeeded
                        plans <- plPlans <$> readIORef logRef
                        map ppLatest plans `shouldBe` [otherVer]

        it "declares its own version when the store holds nothing else" $
            withUpstream $ \url ->
                withRuntime (Right ()) $ \runtime queue logRef -> do
                    job <- enqueueAndReceive queue sampleMirrorJob
                    runWM (sourceAt url runtime) (processJob job) `shouldReturn` Succeeded
                    plans <- plPlans <$> readIORef logRef
                    map ppLatest plans `shouldBe` [ver]

sha512Source :: MirrorInventory -> IO MirrorInventory
sha512Source inventory = do
    let details = inventoryDetails inventory
        files = fmap (\file -> file{artHashes = [unsafeHash SHA512 trueSha512Hex]}) (pkgArtifacts details)
    expectRight (restoreMirrorInventory details{pkgArtifacts = files} (mirrorSource files "sha512 source"))

-- Each admitted digest must remain verifiable without a second algorithm in the source.
soleAdmittedDigests :: [(String, Hash)]
soleAdmittedDigests =
    [ ("a sha512 SRI", unsafeHash SRI trueSri)
    , ("a sha384 SRI", unsafeHash SRI trueSha384Sri)
    , ("a sha256 digest", unsafeHash SHA256 trueSha256)
    , ("a blake2b-512 digest", unsafeHash Blake2b trueBlake2b)
    ]

-- | What ingest re-evaluation decides, per reason the job is no longer mirrorable as enqueued.
reEvaluationVerdicts :: [(String, WorkerPolicies, JobOutcome -> Bool)]
reEvaluationVerdicts =
    [ ("drops a job whose version current policy denies, without publishing", npmPolicies presentResolver [denyRule], isDropped)
    , -- A version the upstream withdrew must not be mirrored, so the drop is non-retryable.
      ("drops a job whose version the upstream no longer offers (withdrawn), without publishing", npmPolicies (\_ _ -> pure VersionMissing) [admitRule], isDropped)
    , -- A transient metadata outage maps to the serve path's transient degrade: leave the job
      -- for redelivery rather than dropping it or publishing it unvetted.
      ("retries a job when the re-evaluation metadata cannot be re-fetched, without publishing", npmPolicies (\_ _ -> pure VersionMetadataUnavailable) [admitRule], isRetried)
    , -- A job for an ecosystem with no bundle is fail-closed: never mirrored unvetted.
      ("drops a job whose ecosystem has no configured policy (fail-closed), without publishing", mempty, isDropped)
    , -- The upstream now serves only a legacy SHA-1. The shared oracle refuses it at ingest
      -- exactly as the serve gate would.
      ("drops a job whose artifact's current digests fall below the integrity floor", npmPolicies (resolverWithArtifact sampleArtifact{artHashes = [unsafeHash SHA1 trueSha1]}) [admitRule], isDropped)
    , -- The stripped-digest degrade: current metadata offers nothing to tie the bytes to.
      ("drops a job whose version no longer carries any integrity digest", npmPolicies (resolverWithArtifact sampleArtifact{artHashes = []}) [admitRule], isDropped)
    , -- The version survives upstream but its file set no longer names the admitted artifact,
      -- which no redelivery can restore.
      ("drops a job whose admitted artifact file the current metadata no longer carries", npmPolicies (resolverWithArtifact sampleArtifact{artFilename = "renamed-9.9.9.tgz"}) [admitRule], isDropped)
    , -- The advisory-outage degrade: the serve path renders the same cause as a transient 503.
      ("retries a job when a fail-closed rule cannot be computed (undecidable), without publishing", npmPolicies presentResolver [cannotVetRule], isRetried)
    ]

-- The version object current metadata carries, marked so a case can tell it from any other.
admissionObject :: CachedDoc
admissionObject = fst npmCached (object ["marker" .= ("admission-time" :: Text)])

-- The real npm codec over the shared transport against a stub mirror target, so the codec's own
-- refusal is the one under test rather than a recording double's.
codecPublishAt :: Deadlines -> Text -> IO MirrorPublish
codecPublishAt deadlines = anonymousPublishAt deadlines (npmPublishCodec nativePresentation) . loopbackRegistryUrl

-- A resolver whose present verdict carries the given raw object beside the sample details.
resolverCarrying :: CachedDoc -> PackageName -> Version -> IO VersionEvaluation
resolverCarrying = resolverCarryingWith sampleArtifact

resolverCarryingWith :: Artifact -> CachedDoc -> PackageName -> Version -> IO VersionEvaluation
resolverCarryingWith artifact raw name version =
    pure (VersionPresent VersionDoc{vdDetails = (sampleDetails name version){pkgArtifacts = artifact :| []}, vdRaw = Just raw} Nothing)

-- An admission verdict the advisory deny could not decide, with the given transience and cause.
undecided :: Transience -> Inability -> ArtifactAdmission
undecided transience why = AdmissionUndecidable (Undecidable transience (RuleUnable "DenyIfCve" why))

-- A stand-in reason renderer. The unit pins the verdict and that the reason is rendered from
-- the very fault being judged, never the wording each worker leg chooses.
renderFault :: FetchFault -> Text
renderFault = \case
    FetchUrlUnformable _ -> "unformable"
    FetchBoundExceeded _ -> "over the bound"
    FetchTransport _ -> "transport failed"
    FetchCooldown _ -> "upstream cooling down"
