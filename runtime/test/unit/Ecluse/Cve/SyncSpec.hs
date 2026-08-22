-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Cve.SyncSpec (spec) where

import Conduit (runConduit, yieldMany, (.|))
import Control.Concurrent.STM (check)
import Data.Conduit.Combinators qualified as C
import Katip (Environment (Environment), KatipContextT, Namespace (Namespace), SimpleLogPayload, initLogEnv, runKatipContextT)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Expectation, Spec, anyException, describe, expectationFailure, it, shouldBe, shouldReturn, shouldSatisfy, shouldThrow)
import UnliftIO.Async (AsyncCancelled (AsyncCancelled), async, cancel, waitCatch, withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (mask_, throwIO)
import UnliftIO.Timeout (timeout)

import Ecluse.Core.Cve (CveDbRejected (CveDbIntegrityFailed, CveDbWrongEpoch), CveLookup (cveRemediationProbe))
import Ecluse.Core.Cve.Slot (CveSlot, newCveSlot, withSlotLookup)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Fault (TransportCause (TransportUnreachable), transportFault)
import Ecluse.Core.Osv.Schema (osvDbFileName, osvSchemaEpoch)
import Ecluse.Core.Telemetry.Metrics (
    AdvisorySyncResult (AdvisoryFetchFailed, AdvisoryNonePublished, AdvisoryRefused, AdvisorySwapped, AdvisoryUnchanged),
 )
import Ecluse.Runtime.Cve.Sync (
    CveFetch (..),
    DbEtag (..),
    OsvDbCapExceeded (OsvDbCapExceeded),
    OsvDbFetchFault (OsvDbTransport),
    SyncEnv (..),
    SyncOutcome (..),
    SyncSchedule (..),
    cappedAt,
    runCveSync,
    syncStep,
 )
import Ecluse.Test.Osv (mkDbWithMalformedProvenance, mkDbWithWrongEpoch, mkMinimalValidDb)
import Ecluse.Test.Port (
    noopAdvisorySyncMetricsPort,
    passthroughAdvisorySyncTracingPort,
    recordingAdvisorySyncMetricsPort,
    recordingAdvisorySyncTracingPort,
 )

-- | An env over a fresh slot and a temp data dir, handed to each case.
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

{- | A typed stand-in for an exception that escapes the fetch's typed contract.
No test double in this spec throws a stringly exception.
-}
newtype SyncSpecEscape = SyncSpecEscape Text
    deriving stock (Eq, Show)

instance Exception SyncSpecEscape

{- | A fetch whose HEAD answers the given ETag and whose download builds an
artifact via the given writer, returning the same ETag.
-}
fetchServing :: Maybe Text -> (FilePath -> IO ()) -> CveFetch
fetchServing mEtag write =
    CveFetch
        { fetchHeadEtag = pure (Right (DbEtag <$> mEtag))
        , fetchDownload = \dest -> case mEtag of
            Nothing -> throwIO (SyncSpecEscape "download called with no object present")
            Just etag -> write dest $> Right (DbEtag etag)
        }

-- | The typed transport fault the failing-fetch doubles report.
transportDown :: OsvDbFetchFault
transportDown = OsvDbTransport (transportFault TransportUnreachable "transport down")

-- | Does the slot's current generation answer the probe for this package?
probesFor :: CveSlot -> Text -> IO (Maybe Bool)
probesFor slot pkg = withSlotLookup slot (traverse (\l -> cveRemediationProbe l pkg "1.0.0"))

-- | The gap between two reads of a polled condition, in microseconds.
pollInterval :: Int
pollInterval = 25_000

{- | The ceiling on every wait in this spec, in microseconds. One artifact build under
coverage has taken five seconds, so only a condition that never arrives reaches it.
-}
pollBudget :: Int
pollBudget = 60_000_000

{- | Poll a condition until the budget runs out, then fail the case naming what never
happened. A recorder that another thread appends to is readable only this way.
-}
waitFor :: Text -> IO Bool -> IO ()
waitFor what ready = go (pollBudget `div` pollInterval)
  where
    go 0 = expectationFailure ("timed out waiting for " <> toString what)
    go n =
        ready >>= \case
            True -> pass
            False -> threadDelay pollInterval >> go (n - 1)

{- | Block until a counter that another thread advances reaches @wanted@, or fail the case
naming what never happened once the budget runs out, so a dead task cannot hang the suite.
-}
awaitCount :: Text -> TVar Int -> Int -> IO ()
awaitCount what counter wanted =
    timeout pollBudget (atomically (readTVar counter >>= check . (>= wanted)))
        >>= maybe (expectationFailure ("timed out waiting for " <> toString what)) (const pass)

{- | A counter over the sync task's swap notifications, with the action to hand
'runCveSync'. The task notifies after the swap publishes, so a counted swap already serves.
-}
newSwapCounter :: IO (TVar Int, IO ())
newSwapCounter = do
    swaps <- newTVarIO (0 :: Int)
    pure (swaps, atomically (modifyTVar' swaps (+ 1)))

-- | Run a Katip-constrained action against a scribe-less environment.
runQuiet :: KatipContextT IO a -> IO a
runQuiet action = do
    logEnv <- initLogEnv (Namespace ["ecluse"]) (Environment "test")
    runKatipContextT logEnv (mempty :: SimpleLogPayload) mempty action

-- | The sync task over inert observation ports, for the scheduling cases.
runUnobserved :: SyncEnv -> SyncSchedule -> IO () -> KatipContextT IO ()
runUnobserved = runCveSync noopAdvisorySyncMetricsPort passthroughAdvisorySyncTracingPort

{- | A schedule that yields exactly one attempt. An empty boot backoff spends the burst
budget on the first attempt, and the steady poll's first interval outlasts any test.
-}
oneAttempt :: SyncSchedule
oneAttempt = SyncSchedule{schedBootBackoff = [], schedPollDelay = 600_000_000}

-- | What one run of the sync loop recorded, each list in record order.
data Observed = Observed
    { obsSpans :: [(Ecosystem, AdvisorySyncResult)]
    , obsAttempts :: [(Ecosystem, AdvisorySyncResult)]
    , obsDurations :: [(Ecosystem, AdvisorySyncResult, Double)]
    }

{- | Drive the sync loop over recording ports until it brackets @wanted@ attempts, then
read back the three recorders. The bracket records the span last, so the metrics settle first.
-}
observeAttempts :: Int -> SyncSchedule -> SyncEnv -> IO Observed
observeAttempts wanted schedule env = do
    (metricsPort, readAttempts, readDurations) <- recordingAdvisorySyncMetricsPort
    (tracingPort, readSpans) <- recordingAdvisorySyncTracingPort
    withAsync (runQuiet (runCveSync metricsPort tracingPort env schedule pass)) $ \_ -> do
        waitFor (show wanted <> " bracketed sync attempt(s)") ((>= wanted) . length <$> readSpans)
        Observed <$> readSpans <*> readAttempts <*> readDurations

{- | Assert that the run bracketed exactly these attempts: one span, one counter
increment, and one non-negative latency sample each, all under the same labels.
-}
shouldObserve :: Observed -> [(Ecosystem, AdvisorySyncResult)] -> Expectation
shouldObserve observed expected = do
    obsSpans observed `shouldBe` expected
    obsAttempts observed `shouldBe` expected
    map (\(eco, result, _) -> (eco, result)) (obsDurations observed) `shouldBe` expected
    map (\(_, _, seconds) -> seconds >= 0) (obsDurations observed) `shouldBe` map (const True) expected

-- | Keep only the first @n@ of each recording, for a loop that keeps attempting.
truncateObserved :: Int -> Observed -> Observed
truncateObserved n (Observed spans attempts durations) =
    Observed (take n spans) (take n attempts) (take n durations)

spec :: Spec
spec = do
    describe "syncStep" $ do
        it "reports the object absent without attempting a download" $
            withSyncEnv $ \_ _ envWith -> do
                let fetch =
                        CveFetch
                            { fetchHeadEtag = pure (Right Nothing)
                            , fetchDownload = \_ -> throwIO (SyncSpecEscape "must not download")
                            }
                syncStep (envWith fetch) Nothing >>= \case
                    SyncAbsent -> pass
                    other -> expectationFailure ("expected SyncAbsent, got " <> show other)

        it "does nothing when the remote ETag matches the last seen one" $
            withSyncEnv $ \_ _ envWith -> do
                let fetch =
                        CveFetch
                            { fetchHeadEtag = pure (Right (Just (DbEtag "e1")))
                            , fetchDownload = \_ -> throwIO (SyncSpecEscape "must not download")
                            }
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
                            { fetchHeadEtag = pure (Right (Just (DbEtag "e1")))
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
                let fetch =
                        CveFetch
                            { fetchHeadEtag = pure (Left transportDown)
                            , fetchDownload = \_ -> throwIO (SyncSpecEscape "must not download")
                            }
                syncStep (envWith fetch) Nothing >>= \case
                    SyncFetchFaulted fault -> fault `shouldBe` transportDown
                    other -> expectationFailure ("expected SyncFetchFaulted, got " <> show other)

        it "residue: a download that throws past its typed contract still discards the partial temp file" $
            withSyncEnv $ \_ _ envWith -> do
                -- The fetch contract reports every failure as a value, so a throw here is an
                -- invariant break. The onException guard must still discard the partial download.
                let fetch =
                        CveFetch
                            { fetchHeadEtag = pure (Right (Just (DbEtag "e1")))
                            , fetchDownload = \dest -> do
                                writeFileBS dest "partial bytes"
                                throwIO (SyncSpecEscape "connection reset mid-stream")
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
                            { fetchHeadEtag = pure (Right (Just (DbEtag "e2")))
                            , fetchDownload = \dest -> do
                                modifyIORef' downloads (+ 1)
                                mkDbWithMalformedProvenance dest
                                pure (Right (DbEtag "e2"))
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

        it "a swapper cancelled while draining never closes the newly published generation" $
            withSyncEnv $ \_ slot envWith -> do
                void (syncStep (envWith (fetchServing (Just "e1") (`mkMinimalValidDb` "pkg-a"))) Nothing)
                insideReader <- newEmptyMVar
                releaseReader <- newEmptyMVar
                insideSwapper <- newEmptyMVar
                pinned <- async $ withSlotLookup slot $ \_ -> do
                    putMVar insideReader ()
                    takeMVar releaseReader
                takeMVar insideReader
                let buildPkgB dest = do
                        putMVar insideSwapper ()
                        mkMinimalValidDb dest "pkg-b"
                -- The swap publishes the new generation, then parks draining the displaced
                -- one, whose reader is still pinned inside. The mask defers the cancellation
                -- to the swapper's only blocking point, that drain, so no wait rides a clock.
                swapper <- async (mask_ (syncStep (envWith (fetchServing (Just "e2") buildPkgB)) (Just (DbEtag "e1"))))
                takeMVar insideSwapper
                cancel swapper
                putMVar releaseReader ()
                void (waitCatch pinned)
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
                            { fetchHeadEtag = do
                                n <- atomicModifyIORef' calls (\n -> (n + 1, n + 1))
                                pure $
                                    if n <= 2
                                        then Left transportDown
                                        else Right (Just (DbEtag "e1"))
                            , fetchDownload = \dest -> mkMinimalValidDb dest "pkg-a" $> Right (DbEtag "e1")
                            }
                    schedule = SyncSchedule{schedBootBackoff = replicate 5 10_000, schedPollDelay = 5_000_000}
                withAsync (runQuiet (runUnobserved (envWith flaky) schedule onSwap)) $ \_ -> do
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
                            { fetchHeadEtag = atomically $ do
                                modifyTVar' attempted (+ 1)
                                readTVar published <&> \case
                                    False -> Right Nothing
                                    True -> Right (Just (DbEtag "e1"))
                            , fetchDownload = \dest -> mkMinimalValidDb dest "pkg-a" $> Right (DbEtag "e1")
                            }
                    -- A short burst that finds nothing published, then a fast poll
                    -- that finds the artifact once it exists.
                    schedule = SyncSchedule{schedBootBackoff = [5_000, 5_000], schedPollDelay = 25_000}
                    -- Mirrors bootBurstPolicy in Ecluse.Runtime.Cve.Sync: one attempt per delay, plus the first.
                    burstAttempts = length (schedBootBackoff schedule) + 1
                withAsync (runQuiet (runUnobserved (envWith lateFetch) schedule onSwap)) $ \_ -> do
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
                            { fetchHeadEtag = pure (Right (Just (DbEtag "bad")))
                            , fetchDownload = \dest -> do
                                modifyIORef' downloads (+ 1)
                                mkDbWithWrongEpoch dest
                                pure (Right (DbEtag "bad"))
                            }
                    schedule = SyncSchedule{schedBootBackoff = replicate 5 10_000, schedPollDelay = 20_000}
                withAsync (runQuiet (runUnobserved (envWith fetch) schedule pass)) $ \_ -> do
                    threadDelay 200_000
                    -- One download despite the burst budget and several polls:
                    -- identical bytes cannot verify differently, so the
                    -- remembered ETag reads as unchanged until a re-publish.
                    readIORef downloads `shouldReturn` 1
                    probesFor slot "pkg" `shouldReturn` Nothing

    describe "advisory sync observation" $ do
        -- One span, one counter increment, and one latency sample per attempt, each labelled
        -- by the ecosystem and the attempt's result. The five cases below are the five
        -- outcomes 'observedStep' folds.
        it "observes a swapped-in artifact as one attempt" $
            withSyncEnv $ \_ _ envWith -> do
                observed <- observeAttempts 1 oneAttempt (envWith (fetchServing (Just "e1") (`mkMinimalValidDb` "pkg-a")))
                observed `shouldObserve` [(Npm, AdvisorySwapped)]

        it "observes an unpublished artifact as one attempt" $
            withSyncEnv $ \_ _ envWith -> do
                let fetch =
                        CveFetch
                            { fetchHeadEtag = pure (Right Nothing)
                            , fetchDownload = \_ -> throwIO (SyncSpecEscape "must not download")
                            }
                observed <- observeAttempts 1 oneAttempt (envWith fetch)
                observed `shouldObserve` [(Npm, AdvisoryNonePublished)]

        it "observes a failed fetch as one attempt, so a broken bucket still meters" $
            withSyncEnv $ \_ _ envWith -> do
                let fetch =
                        CveFetch
                            { fetchHeadEtag = pure (Left transportDown)
                            , fetchDownload = \_ -> throwIO (SyncSpecEscape "must not download")
                            }
                observed <- observeAttempts 1 oneAttempt (envWith fetch)
                observed `shouldObserve` [(Npm, AdvisoryFetchFailed)]

        it "observes a refused artifact as one attempt" $
            withSyncEnv $ \_ _ envWith -> do
                observed <- observeAttempts 1 oneAttempt (envWith (fetchServing (Just "bad") mkDbWithWrongEpoch))
                observed `shouldObserve` [(Npm, AdvisoryRefused)]

        it "observes the poll that finds the artifact unchanged" $
            withSyncEnv $ \_ _ envWith -> do
                -- The burst can only ever start from no last-seen ETag, so an unchanged
                -- attempt is the poll that follows a settled one. The loop keeps polling,
                -- so this case pins the first two attempts rather than the whole run.
                let polling = SyncSchedule{schedBootBackoff = [], schedPollDelay = 20_000}
                observed <- observeAttempts 2 polling (envWith (fetchServing (Just "e1") (`mkMinimalValidDb` "pkg-a")))
                truncateObserved 2 observed `shouldObserve` [(Npm, AdvisorySwapped), (Npm, AdvisoryUnchanged)]

        it "syncs identically over inert ports, so observation is never load-bearing" $
            withSyncEnv $ \_ slot envWith -> do
                (swaps, onSwap) <- newSwapCounter
                let env = envWith (fetchServing (Just "e1") (`mkMinimalValidDb` "pkg-a"))
                withAsync (runQuiet (runUnobserved env oneAttempt onSwap)) $ \_ -> do
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
