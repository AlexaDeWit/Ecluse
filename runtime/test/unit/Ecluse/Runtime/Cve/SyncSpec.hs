-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Sync acceptance, scheduling, and logging regressions
over local artifact fixtures.
-}
module Ecluse.Runtime.Cve.SyncSpec (spec) where

import Conduit (runConduit, yieldMany, (.|))
import Control.Concurrent.STM (check)
import Data.Aeson (Value (String))
import Data.ByteString.Lazy qualified as LBS
import Data.Conduit.Combinators qualified as C
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian)
import Katip (KatipContextT, closeScribes, runKatipContextT)
import System.Directory (copyFile, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Expectation, Spec, anyException, describe, expectationFailure, it, shouldBe, shouldReturn, shouldSatisfy, shouldThrow)
import UnliftIO.Async (AsyncCancelled (AsyncCancelled), async, cancel, waitCatch, withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO)
import UnliftIO.Timeout (timeout)

import Ecluse.Core.Cve (AdvisoryRange (arCveId), CveDb (..), CveDbRejected (CveDbIntegrityFailed, CveDbWrongEpoch), CveLookup (..))
import Ecluse.Core.Cve.Slot (AdvisorySource (..), CveSlot, currentAdvisoryEtag, currentAdvisorySource, generationInstalledAt, newCveSlot, swapIn, withSlotGeneration, withSlotLookup)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Fault (TransportCause (TransportUnreachable), transportFault)
import Ecluse.Core.Osv.Provenance (AdvisoryProvenance (apOsvNewestModified, apOsvSource), noProvenance)
import Ecluse.Core.Osv.Schema (osvDbFileName, osvSchemaEpoch)
import Ecluse.Core.Osv.Stream (IngestStats (IngestStats), PilotIngestAborted (PilotIngestAborted))
import Ecluse.Core.Registry.Maintenance (StoredVersion (StoredVersion), VersionPresence (VersionServed))
import Ecluse.Core.Registry.Sweep.Package (sweepPackage)
import Ecluse.Core.Registry.Sweep.Types (SweepMount (smFirstParty), newSweepState)
import Ecluse.Core.Rules (RuleDeps (..), evalRule, prepare)
import Ecluse.Core.Rules.Types (DenyIfCveParams (DenyIfCveParams), DenyIfEpssParams (DenyIfEpssParams), EvalContext, FailureAlignment (FailDeny), Rule (..), RuleVerdict (..), completeEvidence, mkEvalContext)
import Ecluse.Core.Telemetry.Metrics (
    AdvisorySyncResult (AdvisoryFetchFailed, AdvisoryNonePublished, AdvisoryRefused, AdvisorySwapped, AdvisoryUnchanged),
 )
import Ecluse.Core.Version (mkVersion)
import Ecluse.Runtime.Cve.Sync (
    CveFetch (..),
    DbEtag (..),
    FetchedObject (..),
    OsvDbCapExceeded (OsvDbCapExceeded),
    OsvDbFetchFault (OsvDbTransport),
    SyncEnv (..),
    SyncHooks (SyncHooks, hookFirstSync, hookPushAge),
    SyncOutcome (..),
    SyncSchedule (..),
    cappedAt,
    runCveSync,
    syncStep,
 )
import Ecluse.Runtime.Test.Cve (headOnlyFetch)
import Ecluse.Test.Cve (fakeCveLookup)
import Ecluse.Test.Log (captureStdout, jsonLogEnv, runQuietKatip)
import Ecluse.Test.Maintenance (FakeStore (..), FakeStoreConfig (..), defaultFakeStoreConfig, newFakeStore)
import Ecluse.Test.Osv (mkDbWithMalformedProvenance, mkDbWithWrongEpoch, mkMinimalValidDb, mkMinimalValidDbWithMeta, osvZipOf)
import Ecluse.Test.Osv.Withdrawal (withdrawalBytes, withdrawalZip)
import Ecluse.Test.OsvDb (compileOsvZipDbTo, withOsvZipDb)
import Ecluse.Test.Package (sampleDetails, sampleManifest, unscopedNpm)
import Ecluse.Test.Port (
    noopAdvisorySyncMetricsPort,
    passthroughAdvisorySyncTracingPort,
    recordingAdvisorySyncMetricsPort,
    recordingAdvisorySyncTracingPort,
 )
import Ecluse.Test.Rules (atDefaultPrecedence, inertRuleDeps)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))
import Ecluse.Test.Sweep (RecordedSweep (recPorts), recordingPorts, testMount, testPacing)

withSyncEnv :: (FilePath -> CveSlot -> (CveFetch -> SyncEnv) -> IO a) -> IO a
withSyncEnv use =
    withSystemTempDirectory "ecluse-cve-sync" $ \dir -> do
        slot <- newCveSlot
        let envWith fetch =
                SyncEnv
                    { syncFetch = fetch
                    , syncEcosystem = Npm
                    , syncDbPath = dir </> osvDbFileName "npm"
                    , syncSlot = slot
                    }
        use dir slot envWith

fetchServing :: Maybe Text -> (FilePath -> IO ()) -> CveFetch
fetchServing = fetchServingAt Nothing

-- 'fetchServing' with the publication time the store reports for the object.
fetchServingAt :: Maybe UTCTime -> Maybe Text -> (FilePath -> IO ()) -> CveFetch
fetchServingAt pushedAt mEtag write =
    CveFetch
        { fetchHead = pure (Right ((\etag -> FetchedObject (DbEtag etag) pushedAt) <$> mEtag))
        , fetchDownload = \dest -> case mEtag of
            Nothing -> throwIO (TestContractEscape "download called with no object present")
            Just etag -> write dest $> Right (FetchedObject (DbEtag etag) pushedAt)
        }

transportDown :: OsvDbFetchFault
transportDown = OsvDbTransport (transportFault TransportUnreachable "transport down")

-- The publication time the store reports for a served object.
publishedAt :: UTCTime
publishedAt = UTCTime (fromGregorian 2026 9 1) 0

-- Run one boot attempt against a capturing scribe and hand back everything it logged.
captureSwapLog :: SyncEnv -> IO Text
captureSwapLog env = do
    (swaps, notify) <- newSwapCounter
    captureStdout $ do
        logEnv <- jsonLogEnv
        withAsync (runKatipContextT logEnv () mempty (runUnobserved env oneAttempt notify)) $ \_ ->
            awaitCount "provenance swap" swaps 1
        void (closeScribes logEnv)

installedSource :: CveSlot -> IO AdvisorySource
installedSource slot =
    currentAdvisorySource slot >>= maybe (throwIO (TestContractEscape "no generation installed")) pure

probesFor :: CveSlot -> Text -> IO (Maybe Bool)
probesFor slot pkg = withSlotLookup slot (traverse (\l -> cveRemediationProbe l pkg "1.0.0"))

pollInterval :: Int
pollInterval = 25_000

-- One artifact build under coverage took five seconds. This microsecond budget permits slower CI.
pollBudget :: Int
pollBudget = 60_000_000

waitFor :: Text -> IO Bool -> IO ()
waitFor what ready = go (pollBudget `div` pollInterval)
  where
    go 0 = expectationFailure ("timed out waiting for " <> toString what)
    go n =
        ready >>= \case
            True -> pass
            False -> threadDelay pollInterval >> go (n - 1)

awaitCount :: Text -> TVar Int -> Int -> IO ()
awaitCount what counter wanted =
    timeout pollBudget (atomically (readTVar counter >>= check . (>= wanted)))
        >>= maybe (expectationFailure ("timed out waiting for " <> toString what)) (const pass)

newSwapCounter :: IO (TVar Int, IO ())
newSwapCounter = do
    swaps <- newTVarIO (0 :: Int)
    pure (swaps, atomically (modifyTVar' swaps (+ 1)))

runUnobserved :: SyncEnv -> SyncSchedule -> IO () -> KatipContextT IO ()
runUnobserved env schedule notify =
    runCveSync noopAdvisorySyncMetricsPort passthroughAdvisorySyncTracingPort env schedule (notifyOnly notify)

-- Hooks that only notify. These specs assert on sync outcomes, and the push-age alarm is the
-- shell's ("Ecluse.Cve.Sync"), so it has nothing to observe here.
notifyOnly :: IO () -> SyncHooks
notifyOnly notify = SyncHooks{hookFirstSync = notify, hookPushAge = pass}

-- The first poll interval outlasts every test, leaving only the immediate boot attempt.
oneAttempt :: SyncSchedule
oneAttempt = SyncSchedule{schedBootBackoff = [], schedPollDelay = 600_000_000}

data Observed = Observed
    { obsSpans :: [(Ecosystem, AdvisorySyncResult)]
    , obsAttempts :: [(Ecosystem, AdvisorySyncResult)]
    , obsDurations :: [(Ecosystem, AdvisorySyncResult, Double)]
    }

-- The span recorder runs last, so its count establishes that the metrics settled too.
observeAttempts :: Int -> SyncSchedule -> SyncEnv -> IO Observed
observeAttempts wanted schedule env = do
    (metricsPort, readAttempts, readDurations) <- recordingAdvisorySyncMetricsPort
    (tracingPort, readSpans) <- recordingAdvisorySyncTracingPort
    withAsync (runQuietKatip (runCveSync metricsPort tracingPort env schedule (notifyOnly pass))) $ \_ -> do
        waitFor (show wanted <> " bracketed sync attempt(s)") ((>= wanted) . length <$> readSpans)
        Observed <$> readSpans <*> readAttempts <*> readDurations

shouldObserve :: Observed -> [(Ecosystem, AdvisorySyncResult)] -> Expectation
shouldObserve observed expected = do
    obsSpans observed `shouldBe` expected
    obsAttempts observed `shouldBe` expected
    map (\(eco, result, _) -> (eco, result)) (obsDurations observed) `shouldBe` expected
    map (\(_, _, seconds) -> seconds >= 0) (obsDurations observed) `shouldBe` map (const True) expected

truncateObserved :: Int -> Observed -> Observed
truncateObserved n (Observed spans attempts durations) =
    Observed (take n spans) (take n attempts) (take n durations)

withdrawalSpec :: Spec
withdrawalSpec = describe "compiled withdrawal through sync and shared policy" $ do
    it "refuses a withdrawal-only replacement and keeps the last accepted evidence" $ do
        active <- withdrawalBytes Nothing >>= \bytes -> osvZipOf [("active.json", LBS.fromStrict bytes)]
        withdrawn <- withdrawalBytes (Just (String "2024-05-14T20:15:44Z")) >>= \bytes -> osvZipOf [("active.json", LBS.fromStrict bytes)]
        withOsvZipDb Npm active $ \path ->
            withSyncEnv $ \_ slot envWith -> do
                let env = envWith (fetchServing (Just "active") (copyFile path))
                syncStep env Nothing >>= \case
                    SyncSwapped actual meta -> do
                        actual `shouldBe` DbEtag "active"
                        lookup "row_count" meta `shouldBe` Just "4"
                    other -> expectationFailure ("expected active generation swap, got " <> show other)
                accepted <- readFileBS path
                compileOsvZipDbTo Npm withdrawn (takeDirectory path)
                    `shouldThrow` (\(PilotIngestAborted stats) -> stats == IngestStats 1 0 0 0 0)
                readFileBS path `shouldReturn` accepted
                syncStep env (Just (DbEtag "active")) >>= \case
                    SyncUnchanged -> pass
                    other -> expectationFailure ("expected unchanged generation, got " <> show other)
                withSlotLookup slot $ \case
                    Nothing -> expectationFailure "withdrawal refusal lost the synced database"
                    Just lookup' -> do
                        cveRemediationProbe lookup' "withdrawal-only" "2.0.0" `shouldReturn` True
                        rows <- cveAdvisoriesFor lookup' "withdrawal-only"
                        map arCveId rows `shouldBe` replicate 2 "GHSA-withdrawal"

    it "removes withdrawn evidence while retaining independent denies, fixes, and deletion guards" $
        withSyncEnv $ \_ slot envWith -> do
            let deps = inertRuleDeps{rdWithCveLookup = withSlotGeneration slot, rdCurrentAdvisoryEtag = currentAdvisoryEtag slot}
                cvss = DenyIfCve (DenyIfCveParams 5 FailDeny)
                epss = DenyIfEpss (DenyIfEpssParams 0.25 FailDeny)
            ctx <- mkEvalContext (pure (UTCTime (fromGregorian 2026 1 1) 0)) (currentAdvisoryEtag slot)
            for_ [(Nothing, "active", Nothing, True), (Just (String "2024-05-14T20:15:44Z"), "withdrawn", Just (DbEtag "active"), False)] $ \(withdrawn, etag, previous, active) -> do
                archive <- withdrawalZip withdrawn
                withOsvZipDb Npm archive $ \path -> do
                    let env = envWith (fetchServing (Just etag) (copyFile path))
                    syncStep env previous >>= \case
                        SyncSwapped actual meta -> do
                            actual `shouldBe` DbEtag etag
                            lookup "row_count" meta `shouldBe` Just (if active then "6" else "2")
                        other -> expectationFailure ("expected withdrawal generation swap, got " <> show other)
                withSlotLookup slot $ \case
                    Nothing -> expectationFailure "withdrawal test has no synced database"
                    Just lookup' -> do
                        cveRemediationProbe lookup' "withdrawal-only" "2.0.0" `shouldReturn` active
                        cveRemediationProbe lookup' "withdrawal-overlap" "2.0.0" `shouldReturn` active
                        cveRemediationProbe lookup' "withdrawal-overlap" "3.0.0" `shouldReturn` True
                        cveRemediationProbe lookup' "corpus-vuln" "1.2.0" `shouldReturn` True
                        rows <- cveAdvisoriesFor lookup' "withdrawal-only"
                        map arCveId rows `shouldBe` replicate (if active then 2 else 0) "GHSA-withdrawal"
                        overlapping <- cveAdvisoriesFor lookup' "withdrawal-overlap"
                        sort (ordNub (map arCveId overlapping))
                            `shouldBe` if active then ["GHSA-independent", "GHSA-withdrawal"] else ["GHSA-independent"]
                        names <- cveCoveredNames lookup'
                        ("withdrawal-only" `elem` names) `shouldBe` active
                for_ [cvss, epss] $ \rule -> do
                    for_ ["1.0.0", "4.0.0"] $ \version -> do
                        verdict <- evalRule deps ctx rule (completeEvidence (sampleDetails (unscopedNpm "withdrawal-only") (mkVersion Npm version)))
                        case verdict of
                            Deny _ _ -> active `shouldBe` True
                            NoDecision _ -> active `shouldBe` False
                            other -> expectationFailure ("unexpected withdrawal denial verdict: " <> show other)
                    for_ ["withdrawal-overlap", "corpus-vuln"] $ \name -> do
                        verdict <- evalRule deps ctx rule (completeEvidence (sampleDetails (unscopedNpm name) (mkVersion Npm "1.0.0")))
                        verdict `shouldSatisfy` \case
                            Deny _ _ -> True
                            _ -> False
                    sweepWithdrawal deps ctx rule False "withdrawal-only" `shouldReturn` not active
                    sweepWithdrawal deps ctx rule False "withdrawal-overlap" `shouldReturn` False
                    sweepWithdrawal deps ctx rule True "withdrawal-overlap" `shouldReturn` True
                fixVerdict <- evalRule deps ctx AllowIfRemediatesCve (completeEvidence (sampleDetails (unscopedNpm "withdrawal-only") (mkVersion Npm "2.0.0")))
                case fixVerdict of
                    Allow _ -> active `shouldBe` True
                    NoDecision _ -> active `shouldBe` False
                    other -> expectationFailure ("unexpected withdrawal remediation verdict: " <> show other)
            sweepWithdrawal deps ctx (DenyByIdentity "withdrawal-only") False "withdrawal-only" `shouldReturn` False

sweepWithdrawal :: RuleDeps -> EvalContext -> Rule -> Bool -> Text -> IO Bool
sweepWithdrawal deps ctx rule firstParty rawName = do
    let name = unscopedNpm rawName
        version = mkVersion Npm "1.0.0"
        stored = [StoredVersion version VersionServed]
    store <-
        newFakeStore
            defaultFakeStoreConfig
                { fakeContents = Map.singleton name stored
                , fakeManifests = Map.singleton name (sampleManifest name [version])
                }
    rules <- prepare deps [atDefaultPrecedence rule]
    generation <- rdCurrentAdvisoryEtag deps
    recorded <- recordingPorts generation
    counters <- newSweepState
    let handle = fakeMaintenance store
        mount = (testMount handle rules [rule]){smFirstParty = const firstParty}
    sweepPackage testPacing (recPorts recorded) counters mount ctx name stored `shouldReturn` Nothing
    contents <- readFakeContents store
    pure (maybe False (not . null) (Map.lookup name contents))

spec :: Spec
spec = do
    withdrawalSpec
    describe "sync provenance logging" $ do
        for_ [("2026-09-08T12:34:56Z", "42", "(Just 2026-09-08 12:34:56 UTC,Just 42)"), ("SECRET-time", "SECRET-count", "(Nothing,Nothing)"), (T.replicate 65 "9", T.replicate 21 "9", "(Nothing,Nothing)"), ("invalid", "18446744073709551616", "(Nothing,Nothing)"), ("invalid", "-1", "(Nothing,Nothing)"), ("invalid", "", "(Nothing,Nothing)")] $ \(builtAt, rowCount, summary) ->
            it ("logs only parsed values for built_at=" <> toString builtAt <> " and row_count=" <> toString rowCount) $
                withSyncEnv $ \_ slot envWith -> do
                    let meta =
                            [ ("source_url", "https://SECRET-user:SECRET-password@osv.example/feed?unknown=SECRET-osv#SECRET-fragment")
                            , ("epss_source_url", "https://epss.example/feed?signature=SECRET-epss")
                            , ("pilot_version", "SECRET-version")
                            , ("SECRET-key", T.replicate 4096 "SECRET-value")
                            , ("built_at", builtAt)
                            , ("row_count", rowCount)
                            ]
                        env = envWith (fetchServing (Just "e1") (\path -> mkMinimalValidDbWithMeta path "pkg-a" meta))
                    (swaps, notify) <- newSwapCounter
                    logged <- captureStdout $ do
                        logEnv <- jsonLogEnv
                        withAsync (runKatipContextT logEnv () mempty (runUnobserved env oneAttempt notify)) $ \_ ->
                            awaitCount "legacy artifact swap" swaps 1
                        void (closeScribes logEnv)
                    logged `shouldSatisfy` T.isInfixOf "advisory database swapped in"
                    logged `shouldSatisfy` T.isInfixOf summary
                    logged `shouldSatisfy` (not . T.isInfixOf "SECRET")
                    T.length logged `shouldSatisfy` (< 2048)
                    probesFor slot "pkg-a" `shouldReturn` Just True

        it "names where the serving artifact came from on every swap" $
            withSyncEnv $ \_ _ envWith -> do
                let meta =
                        [ ("osv_source", "https://osv.example.test/npm/all.zip")
                        , ("osv_newest_modified", "2026-08-30T00:00:00Z")
                        , ("epss_score_date", "2026-08-29T00:00:00Z")
                        ]
                    env = envWith (fetchServingAt (Just publishedAt) (Just "e1") (\path -> mkMinimalValidDbWithMeta path "pkg-a" meta))
                logged <- captureSwapLog env
                -- The recorded URL renders as its authority, so artifact text never reaches the log.
                logged `shouldSatisfy` T.isInfixOf "serving artifact source: pushed_at=2026-09-01T00:00:00Z"
                logged `shouldSatisfy` T.isInfixOf "osv_source=osv.example.test:443"
                logged `shouldSatisfy` T.isInfixOf "osv_newest_modified=2026-08-30T00:00:00Z"
                logged `shouldSatisfy` T.isInfixOf "epss_score_date=2026-08-29T00:00:00Z"

        it "reads a value an older artifact never recorded as absent, not as a zero" $
            withSyncEnv $ \_ _ envWith -> do
                logged <- captureSwapLog (envWith (fetchServing (Just "e1") (`mkMinimalValidDb` "pkg-a")))
                logged
                    `shouldSatisfy` T.isInfixOf
                        "serving artifact source: pushed_at=<unrecorded> osv_source=<unrecorded> osv_newest_modified=<unrecorded> epss_score_date=<unrecorded>"

        it "reports an artifact the store gave no publication time for at Error, once for that swap" $
            withSyncEnv $ \_ _ envWith -> do
                logged <- captureSwapLog (envWith (fetchServing (Just "e1") (`mkMinimalValidDb` "pkg-a")))
                T.count "reported no publication time" logged `shouldBe` 1
                logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Error\""
                logged `shouldSatisfy` T.isInfixOf "CVE-based denial refuses until a push carries one"

        it "says nothing of the kind for an artifact the store dated" $
            withSyncEnv $ \_ _ envWith -> do
                logged <- captureSwapLog (envWith (fetchServingAt (Just publishedAt) (Just "e1") (`mkMinimalValidDb` "pkg-a")))
                logged `shouldSatisfy` (not . T.isInfixOf "reported no publication time")

    describe "syncStep" $ do
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

        it "a refused artifact is discarded and the last-good generation keeps serving" $
            withSyncEnv $ \_ slot envWith -> do
                let goodEnv = envWith (fetchServing (Just "e1") (`mkMinimalValidDb` "pkg-a"))
                void (syncStep goodEnv Nothing)
                let badEnv = envWith (fetchServing (Just "e2") mkDbWithWrongEpoch)
                syncStep badEnv (Just (DbEtag "e1")) >>= \case
                    SyncRejected etag rejection -> do
                        etag `shouldBe` DbEtag "e2"
                        rejection `shouldBe` CveDbWrongEpoch (osvSchemaEpoch + 1)
                    other -> expectationFailure ("expected SyncRejected, got " <> show other)
                probesFor slot "pkg-a" `shouldReturn` Just True
                doesFileExist (syncDbPath badEnv <> ".tmp") `shouldReturn` False

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
                pinned <- async $ withSlotLookup slot $ \_ -> do
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

    describe "runCveSync" $ do
        it "the boot burst retries through typed fetch faults until the artifact lands" $
            withSyncEnv $ \_ slot envWith -> do
                calls <- newIORef (0 :: Int)
                (swaps, onSwap) <- newSwapCounter
                let flaky =
                        CveFetch
                            { fetchHead = do
                                n <- atomicModifyIORef' calls (\n -> (n + 1, n + 1))
                                pure $
                                    if n <= 2
                                        then Left transportDown
                                        else Right (Just (FetchedObject (DbEtag "e1") Nothing))
                            , fetchDownload = \dest -> mkMinimalValidDb dest "pkg-a" $> Right (FetchedObject (DbEtag "e1") Nothing)
                            }
                    schedule = SyncSchedule{schedBootBackoff = replicate 5 10_000, schedPollDelay = 5_000_000}
                withAsync (runQuietKatip (runUnobserved (envWith flaky) schedule onSwap)) $ \_ -> do
                    awaitCount "the first swap to publish" swaps 1
                    probesFor slot "pkg-a" `shouldReturn` Just True
                    readTVarIO swaps `shouldReturn` 1

        it "the boot burst is allowed to fail; the poll recovers when the artifact appears" $
            withSyncEnv $ \_ slot envWith -> do
                published <- newTVarIO False
                attempted <- newTVarIO (0 :: Int)
                (swaps, onSwap) <- newSwapCounter
                let lateFetch =
                        CveFetch
                            { fetchHead = atomically $ do
                                modifyTVar' attempted (+ 1)
                                readTVar published <&> \case
                                    False -> Right Nothing
                                    True -> Right (Just (FetchedObject (DbEtag "e1") Nothing))
                            , fetchDownload = \dest -> mkMinimalValidDb dest "pkg-a" $> Right (FetchedObject (DbEtag "e1") Nothing)
                            }
                    schedule = SyncSchedule{schedBootBackoff = [5_000, 5_000], schedPollDelay = 25_000}
                    burstAttempts = length (schedBootBackoff schedule) + 1
                withAsync (runQuietKatip (runUnobserved (envWith lateFetch) schedule onSwap)) $ \_ -> do
                    -- Each attempt reads the flag in one transaction with the counter, so
                    -- the burst spends its whole budget before the publication below.
                    awaitCount "the boot burst to spend every attempt" attempted burstAttempts
                    probesFor slot "pkg-a" `shouldReturn` Nothing
                    atomically (writeTVar published True)
                    awaitCount "the poll to swap the artifact in" swaps 1
                    probesFor slot "pkg-a" `shouldReturn` Just True

        it "the boot burst concedes on a rejected artifact and its remembered ETag stops re-downloads" $
            withSyncEnv $ \_ slot envWith -> do
                downloads <- newIORef (0 :: Int)
                let fetch =
                        CveFetch
                            { fetchHead = pure (Right (Just (FetchedObject (DbEtag "bad") Nothing)))
                            , fetchDownload = \dest -> do
                                modifyIORef' downloads (+ 1)
                                mkDbWithWrongEpoch dest
                                pure (Right (FetchedObject (DbEtag "bad") Nothing))
                            }
                    schedule = SyncSchedule{schedBootBackoff = replicate 5 10_000, schedPollDelay = 20_000}
                withAsync (runQuietKatip (runUnobserved (envWith fetch) schedule pass)) $ \_ -> do
                    threadDelay 200_000
                    -- Identical bytes cannot verify differently. The remembered ETag prevents
                    -- another download until a re-publish.
                    readIORef downloads `shouldReturn` 1
                    probesFor slot "pkg" `shouldReturn` Nothing

    describe "advisory sync observation" $ do
        it "observes a swapped-in artifact as one attempt" $
            withSyncEnv $ \_ _ envWith -> do
                observed <- observeAttempts 1 oneAttempt (envWith (fetchServing (Just "e1") (`mkMinimalValidDb` "pkg-a")))
                observed `shouldObserve` [(Npm, AdvisorySwapped)]

        it "observes an unpublished artifact as one attempt" $
            withSyncEnv $ \_ _ envWith -> do
                let fetch = headOnlyFetch (Right Nothing)
                observed <- observeAttempts 1 oneAttempt (envWith fetch)
                observed `shouldObserve` [(Npm, AdvisoryNonePublished)]

        it "observes a failed fetch as one attempt, so a broken bucket still meters" $
            withSyncEnv $ \_ _ envWith -> do
                let fetch = headOnlyFetch (Left transportDown)
                observed <- observeAttempts 1 oneAttempt (envWith fetch)
                observed `shouldObserve` [(Npm, AdvisoryFetchFailed)]

        it "observes a refused artifact as one attempt" $
            withSyncEnv $ \_ _ envWith -> do
                observed <- observeAttempts 1 oneAttempt (envWith (fetchServing (Just "bad") mkDbWithWrongEpoch))
                observed `shouldObserve` [(Npm, AdvisoryRefused)]

        it "observes the poll that finds the artifact unchanged" $
            withSyncEnv $ \_ _ envWith -> do
                -- The burst has no last-seen ETag. The first poll can report unchanged,
                -- so only the first two attempts matter here.
                let polling = SyncSchedule{schedBootBackoff = [], schedPollDelay = 20_000}
                observed <- observeAttempts 2 polling (envWith (fetchServing (Just "e1") (`mkMinimalValidDb` "pkg-a")))
                truncateObserved 2 observed `shouldObserve` [(Npm, AdvisorySwapped), (Npm, AdvisoryUnchanged)]

        it "reports republication as unchanged and runs no replacement hook" $
            withSyncEnv $ \_ slot envWith -> do
                heads <- newIORef (0 :: Int)
                (swaps, onSwap) <- newSwapCounter
                let newer = addUTCTime 60 publishedAt
                    fetch =
                        (fetchServingAt (Just publishedAt) (Just "e1") (`mkMinimalValidDb` "pkg-a"))
                            { fetchHead = do
                                count <- atomicModifyIORef' heads (\n -> (n + 1, n))
                                pure (Right (Just (FetchedObject (DbEtag "e1") (Just (if count == 0 then publishedAt else newer)))))
                            }
                    schedule = SyncSchedule{schedBootBackoff = [], schedPollDelay = 20_000}
                (metrics, readAttempts, _) <- recordingAdvisorySyncMetricsPort
                withAsync (runQuietKatip (runCveSync metrics passthroughAdvisorySyncTracingPort (envWith fetch) schedule (notifyOnly onSwap))) $ \_ -> do
                    waitFor "republication observation" ((>= 2) . length <$> readAttempts)
                    take 2 <$> readAttempts `shouldReturn` [(Npm, AdvisorySwapped), (Npm, AdvisoryUnchanged)]
                    readTVarIO swaps `shouldReturn` 1
                    asPushedAt <$> installedSource slot `shouldReturn` Just newer

        it "syncs identically over inert ports, so observation is never load-bearing" $
            withSyncEnv $ \_ slot envWith -> do
                (swaps, onSwap) <- newSwapCounter
                let env = envWith (fetchServing (Just "e1") (`mkMinimalValidDb` "pkg-a"))
                withAsync (runQuietKatip (runUnobserved env oneAttempt onSwap)) $ \_ -> do
                    awaitCount "the first swap to publish" swaps 1
                    probesFor slot "pkg-a" `shouldReturn` Just True
                    readTVarIO swaps `shouldReturn` 1

    describe "cappedAt" $ do
        it "passes a stream that ends exactly at the cap through unchanged" $ do
            out <- runConduit (yieldMany (["ab", "cd"] :: [ByteString]) .| cappedAt 4 .| C.sinkList)
            mconcat out `shouldBe` ("abcd" :: ByteString)

        it "throws the confined cap exception the moment the stream oversteps the cap" $
            -- The conduit's mid-stream escape. The adapter boundary ('s3Download')
            -- folds it into the 'OsvDbTooLarge' value on the 'CveFetch' channel.
            runConduit (yieldMany (["ab", "cde"] :: [ByteString]) .| cappedAt 4 .| C.sinkList)
                `shouldThrow` (== OsvDbCapExceeded 4)
