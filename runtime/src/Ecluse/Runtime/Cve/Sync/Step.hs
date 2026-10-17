-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Advisory download, verification, and ownership transfer into the live slot.
module Ecluse.Runtime.Cve.Sync.Step (
    CveFetch (..),
    FetchedObject (..),
    DbEtag (..),
    OsvDbFetchFault (..),
    SyncEnv (..),
    SyncOutcome (..),
    syncStep,
    publishVerified,
) where

import Data.Time (UTCTime)
import System.Directory (removeFile, renameFile)
import UnliftIO.Exception (catchAny, mask, onException)

import Ecluse.Core.Cve (CveDb (cveDbClose, cveDbMeta), CveDbRejected)
import Ecluse.Core.Cve.Internal (transferCveDb)
import Ecluse.Core.Cve.Slot (CveSlot, observeAdvisoryPublication, swapIn)
import Ecluse.Core.Cve.Types (DbEtag (..), OsvDbFetchFault (..))
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Osv.Schema (EpssRequirement)

-- | The advisory transport supplied to one sync cycle.
data CveFetch = CveFetch
    { fetchHead :: IO (Either OsvDbFetchFault (Maybe FetchedObject))
    -- ^ The object's ETag and publication time. @Right Nothing@ means it does not exist, and a fetch failure is a 'Left'.
    , fetchDownload :: FilePath -> IO (Either OsvDbFetchFault FetchedObject)
    -- ^ Download the artifact, byte-bounded, with the download's own ETag, so a racing publish reads true. A 'Left' may leave a partial file.
    }

-- | Metadata from one HEAD or GET response. A download carries its own, so a publication racing HEAD cannot mislabel the bytes.
data FetchedObject = FetchedObject
    { foEtag :: DbEtag
    , foPushedAt :: Maybe UTCTime
    -- ^ The object's own timestamp, 'Nothing' when the store reported none.
    }
    deriving stock (Eq, Show)

-- | Everything one ecosystem's sync task operates on.
data SyncEnv = SyncEnv
    { syncFetch :: CveFetch
    -- ^ The transport for this ecosystem's object key.
    , syncEcosystem :: Ecosystem
    -- ^ The ecosystem the artifact must verify as.
    , syncEpssRequirement :: EpssRequirement
    -- ^ Whether this ecosystem requires successful EPSS enrichment.
    , syncDbPath :: FilePath
    -- ^ The canonical on-disk artifact path (the stable per-ecosystem name).
    , syncSlot :: CveSlot
    -- ^ The slot this task's swaps publish to.
    , syncStoreRef :: Text
    -- ^ How the configured store reads back, for the reports that name where an artifact belongs.
    }

-- | The result of one sync cycle, for scheduling and observation.
data SyncOutcome
    = -- | Verification accepted a new artifact and it is now live (its ETag and provenance carried).
      SyncSwapped DbEtag [(Text, Text)]
    | -- | No database replacement, though an accepted republication can advance publication time.
      SyncUnchanged
    | -- | The object does not exist in the bucket (not yet published).
      SyncAbsent
    | -- | The artifact downloaded, and verification __refused__ it. The last-good generation serves on, and the sync remembers the ETag.
      SyncRejected DbEtag CveDbRejected
    | -- | The fetch itself failed (carried). The step learned nothing, so the last seen ETag stands and the schedule retries.
      SyncFetchFaulted OsvDbFetchFault
    deriving stock (Show)

-- | One detect-download-verify-swap cycle against the last seen ETag. A failed fetch and a refused artifact are outcomes, not exceptions.
syncStep :: SyncEnv -> Maybe DbEtag -> IO SyncOutcome
syncStep env lastSeen =
    fetchHead (syncFetch env) >>= \case
        Left fault -> pure (SyncFetchFaulted fault)
        Right Nothing -> pure SyncAbsent
        Right (Just remote)
            | Just (foEtag remote) == lastSeen -> do
                observeAdvisoryPublication (syncSlot env) (foEtag remote) (foPushedAt remote)
                pure SyncUnchanged
            | otherwise -> syncNewArtifact env

syncNewArtifact :: SyncEnv -> IO SyncOutcome
syncNewArtifact env = mask $ \restore -> do
    let temp = syncDbPath env <> ".tmp"
    downloaded <- restore (fetchDownload (syncFetch env) temp) `onException` discardTemp temp
    case downloaded of
        Left fault -> do
            discardTemp temp
            pure (SyncFetchFaulted fault)
        Right fetched -> do
            opened <-
                restore (transferCveDb (syncEcosystem env) (syncEpssRequirement env) temp (publishVerified env (restore (renameFile temp (syncDbPath env))) temp fetched))
                    `onException` discardTemp temp
            case opened of
                Left rejection -> do
                    discardTemp temp
                    pure (SyncRejected (foEtag fetched) rejection)
                Right outcome -> pure outcome

-- | Transfer under a mask, with a restored rename action. The slot owns the database from swap entry.
publishVerified :: SyncEnv -> IO () -> FilePath -> FetchedObject -> CveDb -> IO SyncOutcome
publishVerified env rename temp fetched db = do
    -- The transfer continuation owns the connection under a mask until 'swapIn' takes it.
    rename `onException` (cveDbClose db >> discardTemp temp)
    -- 'swapIn' owns the connection from entry, including cancellation during old-reader draining.
    swapIn (syncSlot env) (foEtag fetched) (foPushedAt fetched) db
    pure (SyncSwapped (foEtag fetched) (cveDbMeta db))

-- Best-effort: the temp may already be renamed away or never created.
discardTemp :: FilePath -> IO ()
discardTemp temp = removeFile temp `catchAny` const pass
