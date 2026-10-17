-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Advisory transport doubles, temporary slots, and bounded observation helpers.
module Ecluse.Runtime.Test.Cve (
    headOnlyFetch,
    withSyncEnv,
    withSyncEnvFor,
    testStoreRef,
    transportDown,
    publishedAt,
    installedSource,
    probesFor,
    pollBudget,
    waitFor,
    notifyOnly,
    scheduleOf,
    oneAttempt,
    fetchServing,
    fetchServingAt,
    refusingFetch,
) where

import Data.Time (UTCTime (UTCTime), fromGregorian)
import Ecluse.Core.Cve.Slot (AdvisorySource, CveSlot, currentAdvisorySource, newCveSlot, withSlotGeneration)
import Ecluse.Core.Ecosystem (Ecosystem (Npm), ecosystemName)
import Ecluse.Core.Fault (TransportCause (TransportUnreachable), transportFault)
import Ecluse.Core.Osv.Schema (EpssRequirement (EpssOptional), osvDbFileName)
import Ecluse.Test.Cve (namesFix)
import Ecluse.Test.Poll (pollUntil)
import System.FilePath ((</>))
import Test.Hspec (expectationFailure)
import UnliftIO.Exception (throwIO)
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.Runtime.Cve.Sync.Internal (SyncHooks (..), SyncSchedule (..))
import Ecluse.Runtime.Cve.Sync.Step (CveFetch (..), DbEtag (DbEtag), FetchedObject (FetchedObject), OsvDbFetchFault (OsvDbTransport), SyncEnv (..))
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

-- | A HEAD response whose download arm throws if the test unexpectedly reaches it.
headOnlyFetch :: Either OsvDbFetchFault (Maybe FetchedObject) -> CveFetch
headOnlyFetch headResult =
    CveFetch
        { fetchHead = pure headResult
        , fetchDownload = \_ -> throwIO (TestContractEscape "must not download")
        }

-- | A fetch that refuses both arms, for a case that threads a handle but never syncs.
refusingFetch :: CveFetch
refusingFetch =
    CveFetch
        { fetchHead = throwIO (TestContractEscape "must not fetch")
        , fetchDownload = \_ -> throwIO (TestContractEscape "must not fetch")
        }

-- | An object whose download writes the supplied artifact fixture.
fetchServing :: Maybe Text -> (FilePath -> IO ()) -> CveFetch
fetchServing = fetchServingAt Nothing

-- | 'fetchServing' with the publication time the store reports for the object.
fetchServingAt :: Maybe UTCTime -> Maybe Text -> (FilePath -> IO ()) -> CveFetch
fetchServingAt pushedAt mEtag write =
    CveFetch
        { fetchHead = pure (Right ((\etag -> FetchedObject (DbEtag etag) pushedAt) <$> mEtag))
        , fetchDownload = \dest -> case mEtag of
            Nothing -> throwIO (TestContractEscape "download called with no object present")
            Just etag -> write dest $> Right (FetchedObject (DbEtag etag) pushedAt)
        }

-- | A temporary npm artifact path and an empty slot.
withSyncEnv :: (FilePath -> CveSlot -> (CveFetch -> SyncEnv) -> IO a) -> IO a
withSyncEnv = withSyncEnvFor Npm

-- | A temporary artifact path and an empty slot for the selected ecosystem.
withSyncEnvFor :: Ecosystem -> (FilePath -> CveSlot -> (CveFetch -> SyncEnv) -> IO a) -> IO a
withSyncEnvFor eco use =
    withSystemTempDirectory "ecluse-cve-sync" $ \dir -> do
        slot <- newCveSlot
        let envWith fetch =
                SyncEnv
                    { syncFetch = fetch
                    , syncEcosystem = eco
                    , syncEpssRequirement = EpssOptional
                    , syncDbPath = dir </> osvDbFileName (ecosystemName eco)
                    , syncSlot = slot
                    , syncStoreRef = testStoreRef
                    }
        use dir slot envWith

-- | The store every case here reads from, which the unloaded-database report names back.
testStoreRef :: Text
testStoreRef = "s3://test-advisories"

-- | A typed transport outage for sync tests.
transportDown :: OsvDbFetchFault
transportDown = OsvDbTransport (transportFault TransportUnreachable "transport down")

-- | The publication time for served fixture artifacts.
publishedAt :: UTCTime
publishedAt = UTCTime (fromGregorian 2026 9 1) 0

-- | Read the installed source, failing if the fixture never installed one.
installedSource :: CveSlot -> IO AdvisorySource
installedSource slot =
    currentAdvisorySource slot >>= maybe (throwIO (TestContractEscape "no generation installed")) pure

-- | Whether the installed fixture names the exact fix for a package.
probesFor :: CveSlot -> Text -> IO (Maybe Bool)
probesFor slot pkg = withSlotGeneration slot (traverse (\(_, l, _) -> namesFix l pkg "1.0.0"))

pollInterval :: Int
pollInterval = 25_000

-- | Maximum wait in microseconds, allowing fixture compilation under coverage.
pollBudget :: Int
pollBudget = 60_000_000

-- | Wait for a sync observation within the shared test budget.
waitFor :: Text -> IO Bool -> IO ()
waitFor what ready =
    pollUntil (pollBudget `div` pollInterval) pollInterval id ready
        >>= (`unless` expectationFailure ("timed out waiting for " <> toString what))

-- | Hooks that notify on publication without a push-age callback.
notifyOnly :: IO () -> SyncHooks
notifyOnly notify = SyncHooks{hookFirstSync = notify, hookPushAge = pass}

-- | A schedule whose unloaded report outlasts the test.
scheduleOf :: [Int] -> Int -> SyncSchedule
scheduleOf backoff pollDelay =
    SyncSchedule{schedBootBackoff = backoff, schedPollDelay = pollDelay, schedAbsentReport = 600_000_000}

-- | A schedule whose first poll outlasts the test.
oneAttempt :: SyncSchedule
oneAttempt = scheduleOf [] 600_000_000
