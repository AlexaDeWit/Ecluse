-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Advisory verification, publication, and cancellation contracts.
module Ecluse.Runtime.Cve.Sync.StepSpec (spec) where

import Data.List (lookup)
import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian)
import Database.SQLite.Simple (execute_, withConnection)
import System.Directory (copyFile, doesFileExist)
import System.FilePath ((</>))
import Test.Hspec (Spec, anyException, around, describe, expectationFailure, it, shouldBe, shouldReturn, shouldSatisfy, shouldThrow)
import UnliftIO.Async (AsyncCancelled (AsyncCancelled), async, cancel, waitCatch, withAsync)
import UnliftIO.Exception (mask, throwIO)
import UnliftIO.Timeout (timeout)

import Ecluse.Core.Cve (CveDb (..), CveDbRejected (CveDbEpssNotEstablished, CveDbIntegrityFailed, CveDbWrongEpoch), CveLookup (..))
import Ecluse.Core.Cve.Slot (AdvisorySource (..), currentAdvisoryEtag, currentAdvisorySource, generationInstalledAt, swapIn, withSlotGeneration)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Osv.Provenance (AdvisoryProvenance (apOsvNewestModified, apOsvSource), noProvenance)
import Ecluse.Core.Osv.Schema (EpssRequirement (..), osvSchemaEpoch)
import Ecluse.Runtime.Cve.Sync.Step (CveFetch (..), DbEtag (..), FetchedObject (..), SyncEnv (..), SyncOutcome (..), publishVerified, syncStep)
import Ecluse.Runtime.Test.Advisory (advisoryDriver)
import Ecluse.Runtime.Test.Cve (fetchServing, fetchServingAt, headOnlyFetch, installedSource, pollBudget, probesFor, publishedAt, transportDown, waitFor, withSyncEnv)
import Ecluse.Test.Contract.Advisory (AdvisoryFixture (..), advisoryContract)
import Ecluse.Test.Cve (fakeCveLookup, namesFix)
import Ecluse.Test.Osv (CorpusVersion (CorpusV1), mkDbWithMalformedProvenance, mkDbWithWrongEpoch, mkMinimalValidDb, mkMinimalValidDbWithMeta, osvCorpusZip)
import Ecluse.Test.OsvDb (withOsvZipDb)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

spec :: Spec
spec = do
    describe "syncStep" $ do
        it "discards a completed download when cancellation interrupts its return" $
            withSyncEnv $ \_ slot envWith -> do
                downloaded <- newEmptyMVar
                blocked <- newEmptyMVar
                let fetch = fetchServing (Just "e1") $ \dest -> do
                        mkMinimalValidDb dest "pkg-a"
                        putMVar downloaded ()
                        takeMVar blocked
                    env = envWith fetch
                withAsync (syncStep env Nothing) $ \worker -> do
                    takeMVar downloaded
                    cancel worker
                doesFileExist (syncDbPath env <> ".tmp") `shouldReturn` False
                currentAdvisoryEtag slot `shouldReturn` Nothing

        it "closes once and discards the file when publication fails before slot ownership" $
            withSyncEnv $ \_ slot envWith -> do
                closes <- newIORef (0 :: Int)
                let env = envWith (headOnlyFetch (Right Nothing))
                    temp = syncDbPath env <> ".tmp"
                    db = CveDb (fakeCveLookup []) (modifyIORef' closes (+ 1)) [] noProvenance
                writeFileBS temp "downloaded"
                mask (\restore -> publishVerified env (restore (throwIO (TestContractEscape "rename"))) temp (FetchedObject (DbEtag "e1") Nothing) db)
                    `shouldThrow` (\(TestContractEscape _) -> True)
                readIORef closes `shouldReturn` 1
                doesFileExist temp `shouldReturn` False
                currentAdvisoryEtag slot `shouldReturn` Nothing

        it "closes once when cancellation interrupts publication before slot ownership" $
            withSyncEnv $ \_ slot envWith -> do
                closes <- newIORef (0 :: Int)
                renaming <- newEmptyMVar
                blocked <- newEmptyMVar
                let env = envWith (headOnlyFetch (Right Nothing))
                    temp = syncDbPath env <> ".tmp"
                    db = CveDb (fakeCveLookup []) (modifyIORef' closes (+ 1)) [] noProvenance
                    rename = putMVar renaming () >> takeMVar blocked
                writeFileBS temp "downloaded"
                withAsync (mask (\restore -> publishVerified env (restore rename) temp (FetchedObject (DbEtag "e1") Nothing) db)) $ \worker -> do
                    takeMVar renaming
                    cancel worker
                readIORef closes `shouldReturn` 1
                doesFileExist temp `shouldReturn` False
                currentAdvisoryEtag slot `shouldReturn` Nothing

        it "reports the object absent without attempting a download" $
            withSyncEnv $ \_ _ envWith -> do
                let fetch = headOnlyFetch (Right Nothing)
                syncStep (envWith fetch) Nothing >>= \case
                    SyncAbsent -> pass
                    other -> expectationFailure ("expected SyncAbsent, got " <> show other)

        it "does nothing when the remote ETag matches the last seen one" $
            withSyncEnv $ \_ _ envWith -> do
                let fetch = headOnlyFetch (Right (Just (FetchedObject (DbEtag "e1") Nothing)))
                syncStep (envWith fetch) (Just (DbEtag "e1")) >>= \case
                    SyncUnchanged -> pass
                    other -> expectationFailure ("expected SyncUnchanged, got " <> show other)

        it "downloads, verifies, renames onto the canonical name, and swaps in" $
            withSyncEnv $ \_ slot envWith -> do
                let env = envWith (fetchServing (Just "e1") (`mkMinimalValidDb` "pkg-a"))
                syncStep env Nothing >>= \case
                    SyncSwapped etag meta -> do
                        etag `shouldBe` DbEtag "e1"
                        meta `shouldSatisfy` elem ("ecosystem", "npm")
                    other -> expectationFailure ("expected SyncSwapped, got " <> show other)
                probesFor slot "pkg-a" `shouldReturn` Just True
                doesFileExist (syncDbPath env) `shouldReturn` True
                doesFileExist (syncDbPath env <> ".tmp") `shouldReturn` False

        it "carries the artifact's provenance and the object's publication time onto the slot" $
            withSyncEnv $ \_ slot envWith -> do
                let write dest =
                        mkMinimalValidDbWithMeta
                            dest
                            "pkg-a"
                            [ ("osv_source", "https://osv.example.test/npm/all.zip")
                            , ("osv_newest_modified", "2026-08-30T00:00:00Z")
                            ]
                void (syncStep (envWith (fetchServingAt (Just publishedAt) (Just "e1") write)) Nothing)
                source <- installedSource slot
                asPushedAt source `shouldBe` Just publishedAt
                apOsvSource (asProvenance source) `shouldBe` Just "https://osv.example.test/npm/all.zip"
                apOsvNewestModified (asProvenance source) `shouldBe` Just (UTCTime (fromGregorian 2026 8 30) 0)

        it "reads an artifact carrying none of the provenance keys as absence, never a refusal" $
            withSyncEnv $ \_ slot envWith -> do
                syncStep (envWith (fetchServing (Just "e1") (`mkMinimalValidDb` "pkg-a"))) Nothing >>= \case
                    SyncSwapped _ _ -> pass
                    other -> expectationFailure ("expected SyncSwapped on an older artifact, got " <> show other)
                source <- installedSource slot
                asProvenance source `shouldBe` noProvenance
                asPushedAt source `shouldBe` Nothing

        it "keeps the last decoded provenance and publication time across a failed poll" $
            withSyncEnv $ \_ slot envWith -> do
                let write dest = mkMinimalValidDbWithMeta dest "pkg-a" [("osv_source", "https://osv.example.test/npm/all.zip")]
                void (syncStep (envWith (fetchServingAt (Just publishedAt) (Just "e1") write)) Nothing)
                syncStep (envWith (headOnlyFetch (Left transportDown))) (Just (DbEtag "e1")) >>= \case
                    SyncFetchFaulted _ -> pass
                    other -> expectationFailure ("expected SyncFetchFaulted, got " <> show other)
                source <- installedSource slot
                asPushedAt source `shouldBe` Just publishedAt
                apOsvSource (asProvenance source) `shouldBe` Just "https://osv.example.test/npm/all.zip"

        it "refreshes an accepted republication without downloading or resetting installation age" $
            withSyncEnv $ \_ slot envWith -> do
                void (syncStep (envWith (fetchServingAt (Just publishedAt) (Just "e1") (`mkMinimalValidDb` "pkg-a"))) Nothing)
                installed <- generationInstalledAt slot
                original <- installedSource slot
                let newer = addUTCTime 60 publishedAt
                    fetch = headOnlyFetch (Right (Just (FetchedObject (DbEtag "e1") (Just newer))))
                syncStep (envWith fetch) (Just (DbEtag "e1")) >>= \case
                    SyncUnchanged -> pass
                    other -> expectationFailure ("expected metadata-only SyncUnchanged, got " <> show other)
                installedSource slot `shouldReturn` original{asPushedAt = Just newer}
                generationInstalledAt slot `shouldReturn` installed
                probesFor slot "pkg-a" `shouldReturn` Just True

        it "keeps accepted publication time on unchanged, older, or undated HEAD responses" $
            withSyncEnv $ \_ slot envWith -> do
                void (syncStep (envWith (fetchServingAt (Just publishedAt) (Just "e1") (`mkMinimalValidDb` "pkg-a"))) Nothing)
                original <- installedSource slot
                for_ [Just publishedAt, Just (addUTCTime (-60) publishedAt), Nothing] $ \stamp -> do
                    let fetch = headOnlyFetch (Right (Just (FetchedObject (DbEtag "e1") stamp)))
                    void (syncStep (envWith fetch) (Just (DbEtag "e1")))
                    installedSource slot `shouldReturn` original

        it "never refreshes last-good publication time from a remembered rejected ETag" $
            withSyncEnv $ \_ slot envWith -> do
                void (syncStep (envWith (fetchServingAt (Just publishedAt) (Just "good") (`mkMinimalValidDb` "pkg-a"))) Nothing)
                original <- installedSource slot
                void (syncStep (envWith (fetchServing (Just "bad") mkDbWithWrongEpoch)) (Just (DbEtag "good")))
                let newer = addUTCTime 60 publishedAt
                    fetch = headOnlyFetch (Right (Just (FetchedObject (DbEtag "bad") (Just newer))))
                syncStep (envWith fetch) (Just (DbEtag "bad")) >>= \case
                    SyncUnchanged -> pass
                    other -> expectationFailure ("expected rejected redownload suppression, got " <> show other)
                installedSource slot `shouldReturn` original
                probesFor slot "pkg-a" `shouldReturn` Just True

        it "installs the GET identity and time when publication races HEAD" $
            withSyncEnv $ \_ slot envWith -> do
                let newer = addUTCTime 60 publishedAt
                    fetch =
                        (fetchServingAt (Just newer) (Just "get") (`mkMinimalValidDb` "pkg-a"))
                            { fetchHead = pure (Right (Just (FetchedObject (DbEtag "head") (Just publishedAt))))
                            }
                syncStep (envWith fetch) Nothing >>= \case
                    SyncSwapped etag _ -> etag `shouldBe` DbEtag "get"
                    other -> expectationFailure ("expected GET generation swap, got " <> show other)
                currentAdvisoryEtag slot `shouldReturn` Just (DbEtag "get")
                asPushedAt <$> installedSource slot `shouldReturn` Just newer

        it "a second artifact displaces the first" $
            withSyncEnv $ \_ slot envWith -> do
                void (syncStep (envWith (fetchServing (Just "e1") (`mkMinimalValidDb` "pkg-a"))) Nothing)
                void (syncStep (envWith (fetchServing (Just "e2") (`mkMinimalValidDb` "pkg-b"))) (Just (DbEtag "e1")))
                probesFor slot "pkg-b" `shouldReturn` Just True
                probesFor slot "pkg-a" `shouldReturn` Just False

        for_ [False, True] $ \warm ->
            it ("rejects a downloaded epoch 4 artifact " <> if warm then "without changing last-good evidence or publication age" else "before any generation exists") $ do
                archive <- osvCorpusZip CorpusV1
                withOsvZipDb Npm archive $ \compiled ->
                    withSyncEnv $ \_ slot envWith -> do
                        let accepted = envWith (fetchServingAt (Just publishedAt) (Just "good") (copyFile compiled))
                            downloadOld dest = do
                                copyFile compiled dest
                                withConnection dest (`execute_` "PRAGMA user_version = 4")
                            rejected = envWith (fetchServingAt (Just (addUTCTime 60 publishedAt)) (Just "old") downloadOld)
                        when warm (void (syncStep accepted Nothing))
                        etag <- currentAdvisoryEtag slot
                        installed <- generationInstalledAt slot
                        source <- currentAdvisorySource slot
                        priorBytes <- if warm then Just <$> readFileBS (syncDbPath accepted) else pure Nothing
                        syncStep rejected etag >>= \case
                            SyncRejected rejectedEtag reason -> do
                                rejectedEtag `shouldBe` DbEtag "old"
                                reason `shouldBe` CveDbWrongEpoch 4
                            other -> expectationFailure ("expected epoch 4 SyncRejected, got " <> show other)
                        currentAdvisoryEtag slot `shouldReturn` etag
                        generationInstalledAt slot `shouldReturn` installed
                        currentAdvisorySource slot `shouldReturn` source
                        doesFileExist (syncDbPath rejected <> ".tmp") `shouldReturn` False
                        case priorBytes of
                            Just bytes -> do
                                readFileBS (syncDbPath accepted) `shouldReturn` bytes
                                probesFor slot "corpus-vuln" `shouldReturn` Just False
                                withSlotGeneration slot (traverse (\(_, cve, _) -> namesFix cve "corpus-vuln" "1.2.0")) `shouldReturn` Just True
                            Nothing -> doesFileExist (syncDbPath rejected) `shouldReturn` False

        around
            ( \use -> withSyncEnv $ \dir _ envWith -> do
                let goodPath = dir </> "accepted.db"
                    badPath = dir </> "refused.db"
                mkMinimalValidDb goodPath "pkg-a"
                mkDbWithWrongEpoch badPath
                bytes <- readFileBS goodPath
                selected <- newIORef (fetchServing (Just "e1") (copyFile goodPath))
                let publish fixture = do
                        let (etag, path) = case fixture of
                                AcceptedArtifact -> ("e1", goodPath)
                                RefusedArtifact -> ("e2", badPath)
                        writeIORef selected (fetchServing (Just etag) (copyFile path))
                        pure (DbEtag etag)
                advisoryDriver bytes publish (envWith <$> readIORef selected) >>= use
            )
            $ advisoryContract "advisory/local" (CveDbWrongEpoch (osvSchemaEpoch + 1))

        it "a download that faults mid-stream is a SyncFetchFaulted outcome and the partial temp file is discarded" $
            withSyncEnv $ \_ _ envWith -> do
                let fetch =
                        CveFetch
                            { fetchHead = pure (Right (Just (FetchedObject (DbEtag "e1") Nothing)))
                            , fetchDownload = \dest -> do
                                writeFileBS dest "partial bytes"
                                pure (Left transportDown)
                            }
                    env = envWith fetch
                syncStep env Nothing >>= \case
                    SyncFetchFaulted fault -> fault `shouldBe` transportDown
                    other -> expectationFailure ("expected SyncFetchFaulted, got " <> show other)
                doesFileExist (syncDbPath env <> ".tmp") `shouldReturn` False

        it "a head fault is a SyncFetchFaulted outcome; nothing is downloaded" $
            withSyncEnv $ \_ _ envWith -> do
                let fetch = headOnlyFetch (Left transportDown)
                syncStep (envWith fetch) Nothing >>= \case
                    SyncFetchFaulted fault -> fault `shouldBe` transportDown
                    other -> expectationFailure ("expected SyncFetchFaulted, got " <> show other)

        it "residue: a download that throws past its typed contract still discards the partial temp file" $
            withSyncEnv $ \_ _ envWith -> do
                -- The fetch contract reports every failure as a value, so a throw here is an
                -- invariant break. The onException guard must still discard the partial download.
                let fetch =
                        CveFetch
                            { fetchHead = pure (Right (Just (FetchedObject (DbEtag "e1") Nothing)))
                            , fetchDownload = \dest -> do
                                writeFileBS dest "partial bytes"
                                throwIO (TestContractEscape "connection reset mid-stream")
                            }
                    env = envWith fetch
                syncStep env Nothing `shouldThrow` anyException
                doesFileExist (syncDbPath env <> ".tmp") `shouldReturn` False

        it "an artifact whose meta values violate the strict declaration is refused and its ETag remembered" $
            withSyncEnv $ \_ slot envWith -> do
                void (syncStep (envWith (fetchServing (Just "e1") (`mkMinimalValidDb` "pkg-a"))) Nothing)
                downloads <- newIORef (0 :: Int)
                let fetch =
                        CveFetch
                            { fetchHead = pure (Right (Just (FetchedObject (DbEtag "e2") Nothing)))
                            , fetchDownload = \dest -> do
                                modifyIORef' downloads (+ 1)
                                mkDbWithMalformedProvenance dest
                                pure (Right (FetchedObject (DbEtag "e2") Nothing))
                            }
                    env = envWith fetch
                syncStep env (Just (DbEtag "e1")) >>= \case
                    SyncRejected etag (CveDbIntegrityFailed _) -> etag `shouldBe` DbEtag "e2"
                    other -> expectationFailure ("expected SyncRejected on the forged artifact, got " <> show other)
                doesFileExist (syncDbPath env <> ".tmp") `shouldReturn` False
                probesFor slot "pkg-a" `shouldReturn` Just True
                -- The remembered ETag turns the next poll into a no-op: the same
                -- bad object is never re-downloaded.
                syncStep env (Just (DbEtag "e2")) >>= \case
                    SyncUnchanged -> pass
                    other -> expectationFailure ("expected SyncUnchanged on the remembered ETag, got " <> show other)
                readIORef downloads `shouldReturn` 1

        it "a swapper cancelled while draining retires the old generation and preserves the new one" $
            withSyncEnv $ \_ slot envWith -> do
                closes <- newIORef (0 :: Int)
                let oldDb = CveDb (fakeCveLookup []) (modifyIORef' closes (+ 1)) [] noProvenance
                swapIn slot (DbEtag "e1") Nothing oldDb
                insideReader <- newEmptyMVar
                releaseReader <- newEmptyMVar
                pinned <- async $ withSlotGeneration slot $ \_ -> do
                    putMVar insideReader ()
                    takeMVar releaseReader
                takeMVar insideReader
                swapper <- async (syncStep (envWith (fetchServing (Just "e2") (`mkMinimalValidDb` "pkg-b"))) (Just (DbEtag "e1")))
                waitFor "generation e2 publication" ((== Just (DbEtag "e2")) <$> currentAdvisoryEtag slot)
                timeout pollBudget (cancel swapper) `shouldReturn` Just ()
                readIORef closes `shouldReturn` 0
                putMVar releaseReader ()
                timeout pollBudget (void (waitCatch pinned)) `shouldReturn` Just ()
                readIORef closes `shouldReturn` 1
                waitCatch swapper >>= \case
                    Left err | Just AsyncCancelled <- fromException err -> pass
                    finished -> expectationFailure ("expected the swapper to be cancelled inside its drain, got " <> show finished)
                -- The cancellation interrupted the drain wait, never the
                -- published generation: the slot must still answer.
                probesFor slot "pkg-b" `shouldReturn` Just True

    describe "EPSS artifact qualification" $ do
        it "retains the qualified generation across rejection and rejected republication" $
            withSyncEnv $ \_ slot envWith -> do
                let required fetch = (envWith fetch){syncEpssRequirement = EpssRequired}
                    writeGood dest = mkMinimalValidDbWithMeta dest "pkg-a" [("epss_status", "available"), ("osv_source", "https://osv.example.test/npm/all.zip")]
                    good = required (fetchServingAt (Just publishedAt) (Just "good") writeGood)
                syncStep good Nothing >>= \case
                    SyncSwapped etag _ -> etag `shouldBe` DbEtag "good"
                    other -> expectationFailure ("expected qualified swap, got " <> show other)
                bytes <- readFileBS (syncDbPath good)
                source <- installedSource slot
                installed <- generationInstalledAt slot
                let bad = required (fetchServingAt (Just (addUTCTime 60 publishedAt)) (Just "bad") (`mkMinimalValidDb` "pkg-b"))
                syncStep bad (Just (DbEtag "good")) >>= \case
                    SyncRejected etag rejection -> do
                        etag `shouldBe` DbEtag "bad"
                        rejection `shouldBe` CveDbEpssNotEstablished
                    other -> expectationFailure ("expected EPSS rejection, got " <> show other)
                let repeated = required (headOnlyFetch (Right (Just (FetchedObject (DbEtag "bad") (Just (addUTCTime 120 publishedAt))))))
                replicateM_ 2 $
                    syncStep repeated (Just (DbEtag "bad")) >>= \case
                        SyncUnchanged -> pass
                        other -> expectationFailure ("expected rejected download suppression, got " <> show other)
                readFileBS (syncDbPath good) `shouldReturn` bytes
                installedSource slot `shouldReturn` source
                generationInstalledAt slot `shouldReturn` installed
                currentAdvisoryEtag slot `shouldReturn` Just (DbEtag "good")
                probesFor slot "pkg-a" `shouldReturn` Just True
                probesFor slot "pkg-b" `shouldReturn` Just False
                doesFileExist (syncDbPath bad <> ".tmp") `shouldReturn` False

        it "keeps advisory and remediation evidence in optional marker-free artifacts" $
            withSyncEnv $ \_ slot envWith -> do
                syncStep (envWith (fetchServing (Just "legacy") (`mkMinimalValidDb` "pkg-a"))) Nothing >>= \case
                    SyncSwapped _ meta -> lookup "epss_status" meta `shouldBe` Nothing
                    other -> expectationFailure ("expected optional swap, got " <> show other)
                withSlotGeneration slot $ \case
                    Nothing -> expectationFailure "optional artifact was not installed"
                    Just (_, lookup', _) -> do
                        namesFix lookup' "pkg-a" "1.0.0" `shouldReturn` True
                        cveAdvisoriesFor lookup' "pkg-a" >>= (`shouldSatisfy` not . null)
