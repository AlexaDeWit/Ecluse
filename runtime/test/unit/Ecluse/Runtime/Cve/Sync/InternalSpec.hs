-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Advisory retry schedules, observation, and operator reports.
module Ecluse.Runtime.Cve.Sync.InternalSpec (spec) where

import Control.Concurrent.STM (check)
import Data.Text qualified as T
import Data.Time (addUTCTime)
import Katip (KatipContextT)
import System.Directory (doesFileExist)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe, shouldReturn, shouldSatisfy)
import UnliftIO.Async (withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Timeout (timeout)

import Ecluse.Core.Cve.Slot (AdvisorySource (..), currentAdvisoryEtag)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Osv.Schema (EpssRequirement (..))
import Ecluse.Core.Telemetry.Metrics (AdvisorySyncResult (AdvisoryFetchFailed, AdvisoryNonePublished, AdvisoryRefused, AdvisorySwapped, AdvisoryUnchanged))
import Ecluse.Runtime.Cve.Sync.Internal (CveFetch (..), DbEtag (..), FetchedObject (..), SyncEnv (..), SyncHooks (SyncHooks, hookFirstSync, hookPushAge), SyncSchedule (..), runCveSync)
import Ecluse.Runtime.Test.Cve (fetchServing, fetchServingAt, headOnlyFetch, installedSource, notifyOnly, oneAttempt, pollBudget, probesFor, publishedAt, scheduleOf, testStoreRef, transportDown, waitFor, withSyncEnv)
import Ecluse.Test.Log (runJsonLog, runQuietKatip)
import Ecluse.Test.Osv (mkDbWithWrongEpoch, mkMinimalValidDb, mkMinimalValidDbWithMeta)
import Ecluse.Test.Port (noopAdvisorySyncMetricsPort, passthroughAdvisorySyncTracingPort, recordingAdvisorySyncMetricsPort, recordingAdvisorySyncTracingPort)

captureSyncLog :: SyncEnv -> SyncSchedule -> SyncHooks -> IO () -> IO Text
captureSyncLog env schedule hooks settle =
    runJsonLog $
        withAsync (runCveSync noopAdvisorySyncMetricsPort passthroughAdvisorySyncTracingPort env schedule hooks) (const (liftIO settle))

captureSwapLog :: SyncEnv -> IO Text
captureSwapLog env = do
    (swaps, notify) <- newSwapCounter
    captureSyncLog env oneAttempt (notifyOnly notify) (awaitCount "provenance swap" swaps 1)

unloadedReports :: Text -> Int
unloadedReports = T.count "no advisory artifact has ever been published"

fetchFailureReports :: Text -> Int
fetchFailureReports logged = length (filter (\l -> T.isInfixOf "sync fetch failed" l && T.isInfixOf "\"sev\":\"Error\"" l) (lines logged))

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

-- A schedule whose unloaded-database report is due at every poll, so a short case sees repeats.
reportingEveryPoll :: SyncSchedule
reportingEveryPoll = SyncSchedule{schedBootBackoff = [], schedPollDelay = 10_000, schedAbsentReport = 10_000}

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

spec :: Spec
spec = do
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
                    logged <- runJsonLog $
                        withAsync (runUnobserved env oneAttempt notify) $ \_ ->
                            liftIO (awaitCount "legacy artifact swap" swaps 1)
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

        it "names the port a portless http source dials" $
            withSyncEnv $ \_ _ envWith -> do
                let write dest = mkMinimalValidDbWithMeta dest "pkg-a" [("osv_source", "http://osv.example.test/npm/all.zip")]
                logged <- captureSwapLog (envWith (fetchServingAt (Just publishedAt) (Just "e1") write))
                logged `shouldSatisfy` T.isInfixOf "osv_source=osv.example.test:80"

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

    describe "EPSS artifact qualification" $ do
        it "rejects an unqualified cold artifact before installation and records refusal" $
            withSyncEnv $ \_ slot envWith -> do
                let env = (envWith (fetchServing (Just "missing") (`mkMinimalValidDb` "pkg-a"))){syncEpssRequirement = EpssRequired}
                observed <- observeAttempts 1 oneAttempt env
                shouldObserve observed [(Npm, AdvisoryRefused)]
                probesFor slot "pkg-a" `shouldReturn` Nothing
                currentAdvisoryEtag slot `shouldReturn` Nothing
                doesFileExist (syncDbPath env) `shouldReturn` False
                doesFileExist (syncDbPath env <> ".tmp") `shouldReturn` False

        it "notifies first sync only after a later qualified generation arrives" $
            withSyncEnv $ \_ slot envWith -> do
                (swaps, onSwap) <- newSwapCounter
                (metrics, readAttempts, _) <- recordingAdvisorySyncMetricsPort
                (tracing, readSpans) <- recordingAdvisorySyncTracingPort
                let missing = fetchServing (Just "missing") (`mkMinimalValidDb` "pkg-a")
                    good = fetchServing (Just "good") (\dest -> mkMinimalValidDbWithMeta dest "pkg-a" [("epss_status", "available")])
                current <- newIORef missing
                let fetch =
                        CveFetch
                            { fetchHead = readIORef current >>= fetchHead
                            , fetchDownload = \dest -> readIORef current >>= (`fetchDownload` dest)
                            }
                    env = (envWith fetch){syncEpssRequirement = EpssRequired}
                    schedule = scheduleOf [] 20_000
                withAsync (runQuietKatip (runCveSync metrics tracing env schedule (notifyOnly onSwap))) $ \_ -> do
                    waitFor "the rejected attempt to finish" (not . null <$> readSpans)
                    take 1 <$> readAttempts `shouldReturn` [(Npm, AdvisoryRefused)]
                    readTVarIO swaps `shouldReturn` 0
                    writeIORef current good
                    awaitCount "qualified first sync" swaps 1
                    probesFor slot "pkg-a" `shouldReturn` Just True

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
                    schedule = scheduleOf (replicate 5 10_000) 5_000_000
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
                    schedule = scheduleOf [5_000, 5_000] 25_000
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
                    schedule = scheduleOf (replicate 5 10_000) 20_000
                withAsync (runQuietKatip (runUnobserved (envWith fetch) schedule pass)) $ \_ -> do
                    threadDelay 200_000
                    -- Identical bytes cannot verify differently. The remembered ETag prevents
                    -- another download until a re-publish.
                    readIORef downloads `shouldReturn` 1
                    probesFor slot "pkg" `shouldReturn` Nothing

        it "names Pilot and the store once the boot budget ends with nothing ever published" $
            withSyncEnv $ \_ _ envWith -> do
                (steps, onStep) <- newSwapCounter
                logged <-
                    captureSyncLog
                        (envWith (headOnlyFetch (Right Nothing)))
                        (scheduleOf [] 20_000)
                        SyncHooks{hookFirstSync = pass, hookPushAge = onStep}
                        (awaitCount "the poll that follows the boot burst" steps 2)
                unloadedReports logged `shouldSatisfy` (>= 1)
                logged `shouldSatisfy` T.isInfixOf testStoreRef
                logged `shouldSatisfy` T.isInfixOf "ecluse pilot"

        it "names the store's access, not Pilot, when the fetch keeps failing" $
            withSyncEnv $ \_ _ envWith -> do
                (steps, onStep) <- newSwapCounter
                logged <-
                    captureSyncLog
                        (envWith (headOnlyFetch (Left transportDown)))
                        (scheduleOf [] 20_000)
                        SyncHooks{hookFirstSync = pass, hookPushAge = onStep}
                        (awaitCount "the poll that follows the boot burst" steps 2)
                unloadedReports logged `shouldBe` 0
                logged `shouldSatisfy` T.isInfixOf "investigate the bucket, object, or IAM"

        it "repeats that report at its own interval while nothing is published" $
            withSyncEnv $ \_ _ envWith -> do
                (steps, onStep) <- newSwapCounter
                logged <-
                    captureSyncLog
                        (envWith (headOnlyFetch (Right Nothing)))
                        reportingEveryPoll
                        SyncHooks{hookFirstSync = pass, hookPushAge = onStep}
                        (awaitCount "three polls past the boot burst" steps 4)
                unloadedReports logged `shouldSatisfy` (>= 2)

        it "reports a failing fetch at Error once, not on every poll inside the interval" $
            withSyncEnv $ \_ _ envWith -> do
                (steps, onStep) <- newSwapCounter
                logged <-
                    captureSyncLog
                        (envWith (headOnlyFetch (Left transportDown)))
                        (scheduleOf [] 10_000)
                        SyncHooks{hookFirstSync = pass, hookPushAge = onStep}
                        (awaitCount "five polls past the boot attempt" steps 6)
                fetchFailureReports logged `shouldBe` 1
                logged `shouldSatisfy` T.isInfixOf "transport down"
                T.count "sync fetch still failing" logged `shouldBe` 0

        it "reports the failure again once the interval has passed, with the latest fault" $
            withSyncEnv $ \_ _ envWith -> do
                (steps, onStep) <- newSwapCounter
                logged <-
                    captureSyncLog
                        (envWith (headOnlyFetch (Left transportDown)))
                        reportingEveryPoll
                        SyncHooks{hookFirstSync = pass, hookPushAge = onStep}
                        (awaitCount "three polls past the boot attempt" steps 4)
                fetchFailureReports logged `shouldBe` 1
                length (filter (\l -> T.isInfixOf "sync fetch still failing" l && T.isInfixOf "\"sev\":\"Error\"" l) (lines logged)) `shouldSatisfy` (>= 2)

        it "reports the recovery at Info on the first poll that fetches again" $
            withSyncEnv $ \_ _ envWith -> do
                (steps, onStep) <- newSwapCounter
                failing <- newIORef True
                let flaky =
                        CveFetch
                            { fetchHead =
                                readIORef failing <&> \case
                                    True -> Left transportDown
                                    False -> Right Nothing
                            , fetchDownload = \_ -> pure (Left transportDown)
                            }
                logged <-
                    captureSyncLog
                        (envWith flaky)
                        (scheduleOf [] 10_000)
                        SyncHooks{hookFirstSync = pass, hookPushAge = onStep}
                        (awaitCount "two failing polls" steps 3 >> writeIORef failing False >> awaitCount "two polls after recovery" steps 6)
                fetchFailureReports logged `shouldBe` 1
                length (filter (\l -> T.isInfixOf "sync fetch recovered" l && T.isInfixOf "\"sev\":\"Info\"" l) (lines logged)) `shouldBe` 1

        it "writes no such report once an artifact has loaded" $
            withSyncEnv $ \_ _ envWith -> do
                (swaps, onSwap) <- newSwapCounter
                logged <-
                    captureSyncLog
                        (envWith (fetchServing (Just "e1") (`mkMinimalValidDb` "pkg-a")))
                        reportingEveryPoll
                        (notifyOnly onSwap)
                        (awaitCount "the first swap to publish" swaps 1 >> threadDelay 100_000)
                unloadedReports logged `shouldBe` 0

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
                let polling = scheduleOf [] 20_000
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
                    schedule = scheduleOf [] 20_000
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
