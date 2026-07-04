{- | The advisory database's sync mechanics: detect a new @osv.db@ artifact in
object storage, download it bounded, verify it, and shadow-swap it into the
read path -- one ecosystem per task, driven by the configured mounts.

The write side of "Ecluse.Core.Cve.Slot": 'syncStep' performs exactly one
detect-download-verify-swap cycle over an injected 'CveFetch' (so unit tests
drive it without a network), and 'runCveSync' schedules those steps: an eager
__boot burst__ (an immediate attempt, retried with incremental backoff, that is
eventually allowed to fail so a broken bucket never wedges startup) followed by
the steady ETag poll. The proxy is thus rules-engine complete as early as the
artifact can be had, and serves deny-by-default-safely before then.

The swap's file discipline: the download lands in a temp file beside the
canonical per-ecosystem path; 'Ecluse.Core.Cve.openCveDb' verifies the temp
file (epoch stamp, table shape, ecosystem) -- this is the artifact contract's
verify-before-swap -- and __the connection that verified is the connection that
serves__: the accepted temp file is renamed atomically onto the canonical name
(the open connection follows the inode; there is no reopen and so no
verify-to-serve gap) and that same 'CveDb' is swapped in. The displaced
generation drains and closes inside 'Ecluse.Core.Cve.Slot.swapIn', releasing
the old inode's last reference; reclamation is the kernel's, never a delete
this code could mistime. A rejected artifact is deleted, its ETag remembered
(re-downloading a known-bad object every poll buys nothing), and the last-good
generation keeps serving.
-}
module Ecluse.Core.Cve.Sync (
    -- * The injected transport
    CveFetch (..),
    DbEtag (..),
    OsvDbFetchFault (..),
    s3CveFetch,

    -- * One sync cycle
    SyncEnv (..),
    SyncOutcome (..),
    syncStep,

    -- * The scheduled task
    SyncSchedule (..),
    runCveSync,
    bootBackoffDelays,
) where

import Conduit (ConduitT, await, runResourceT, yield, (.|))
import Data.ByteString qualified as BS
import Data.Conduit.Combinators qualified as C
import Katip (KatipContext, Severity (DebugS, ErrorS, InfoS, WarningS), logFM, ls)
import Network.HTTP.Types.Status (statusCode)
import System.Directory (removeFile, renameFile)
import UnliftIO (MonadUnliftIO, tryAny)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (catchAny, onException, throwIO)

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3
import Amazonka.S3.Lens qualified as S3L
import Lens.Micro ((^.))

import Ecluse.Core.Cve (CveDb (cveDbClose, cveDbMeta), CveDbRejected, openCveDb)
import Ecluse.Core.Cve.Slot (CveSlot, swapIn)
import Ecluse.Core.Ecosystem (Ecosystem)

{- | An artifact version marker: S3's ETag, opaque text compared for equality
only. Two objects with equal ETags carry equal bytes, so an unchanged ETag is
"nothing to do" and a rejected artifact's remembered ETag is "still the same
bad artifact".
-}
newtype DbEtag = DbEtag Text
    deriving stock (Eq, Show)

{- | The sync transport, as data: how to learn the remote artifact's current
version and how to fetch its bytes. Injected so 'syncStep' is unit-testable
without a network; the composition root supplies 's3CveFetch'.
-}
data CveFetch = CveFetch
    { fetchHeadEtag :: IO (Maybe DbEtag)
    {- ^ The remote artifact's current ETag; 'Nothing' when the object does not
    exist (not yet published for this ecosystem). A transport fault throws.
    -}
    , fetchDownload :: FilePath -> IO DbEtag
    {- ^ Download the artifact to the given path (byte-bounded) and return the
    ETag of the bytes actually fetched -- the download's own, so a publish
    racing the poll is recorded truthfully. Throws on transport faults and on
    'OsvDbFetchFault'.
    -}
    }

{- | A download refused by this side: the object oversteps the configured byte
cap, or the response carried no ETag to record.
-}
data OsvDbFetchFault
    = -- | The object exceeds the configured byte cap (carried, in bytes).
      OsvDbTooLarge Int
    | -- | The response carried no ETag; nothing truthful to record.
      OsvDbNoEtag
    deriving stock (Eq, Show)

instance Exception OsvDbFetchFault

-- | Everything one ecosystem's sync task operates on.
data SyncEnv = SyncEnv
    { syncFetch :: CveFetch
    -- ^ The transport for this ecosystem's object key.
    , syncEcosystem :: Ecosystem
    -- ^ The ecosystem the artifact must verify as.
    , syncDbPath :: FilePath
    -- ^ The canonical on-disk artifact path (the stable per-ecosystem name).
    , syncSlot :: CveSlot
    -- ^ The slot this task's swaps publish to.
    }

{- | What one 'syncStep' concluded; the caller ('runCveSync') logs it and
decides scheduling. Failures of the transport itself surface as exceptions.
-}
data SyncOutcome
    = -- | A new artifact was verified and is now live (its ETag and provenance carried).
      SyncSwapped DbEtag [(Text, Text)]
    | -- | The remote ETag matches the last seen one; nothing to do.
      SyncUnchanged
    | -- | The object does not exist in the bucket (not yet published).
      SyncAbsent
    | {- | The artifact was downloaded and __refused__ by verification; the
      last-good generation keeps serving and the ETag is remembered.
      -}
      SyncRejected DbEtag CveDbRejected
    deriving stock (Show)

{- | One detect-download-verify-swap cycle against the last seen ETag. Total
over verification (a refused artifact is an outcome, not an exception);
transport faults propagate for the caller to log and retry. See the module
header for the file discipline.
-}
syncStep :: SyncEnv -> Maybe DbEtag -> IO SyncOutcome
syncStep env lastSeen =
    fetchHeadEtag (syncFetch env) >>= \case
        Nothing -> pure SyncAbsent
        Just remote
            | Just remote == lastSeen -> pure SyncUnchanged
            | otherwise -> do
                let temp = syncDbPath env <> ".tmp"
                fetched <- fetchDownload (syncFetch env) temp `onException` discardTemp temp
                opened <- openCveDb (syncEcosystem env) temp `onException` discardTemp temp
                case opened of
                    Left rejection -> do
                        discardTemp temp
                        pure (SyncRejected fetched rejection)
                    Right db ->
                        ( do
                            -- The verified connection follows the inode through the
                            -- rename; the canonical name now holds the newest
                            -- accepted artifact and the temp name is gone.
                            renameFile temp (syncDbPath env)
                            swapIn (syncSlot env) db
                            pure (SyncSwapped fetched (cveDbMeta db))
                        )
                            `onException` (cveDbClose db >> discardTemp temp)
  where
    -- Best-effort: the temp may already be renamed away or never created.
    discardTemp temp = removeFile temp `catchAny` const pass

{- | The task's timing: the boot burst's backoff delays and the steady poll
interval, both in microseconds. The composition root ships 'bootBackoffDelays'
and the configured poll interval; tests inject tiny values.
-}
data SyncSchedule = SyncSchedule
    { schedBootBackoff :: [Int]
    -- ^ Delays before each boot-burst retry; the list's length is the budget.
    , schedPollDelay :: Int
    -- ^ The steady ETag-poll interval.
    }

{- | The shipped boot-burst backoff: an immediate first attempt, then retries
after each of these, then the burst concedes and the steady poll takes over.
Constants by design; the poll interval is the operator-facing knob.
-}
bootBackoffDelays :: [Int]
bootBackoffDelays = [1_000_000, 2_000_000, 4_000_000, 8_000_000, 16_000_000]

{- | One ecosystem's sync task: the boot burst, then the steady poll, forever.

The __boot burst__ attempts a sync immediately and retries per the schedule's
backoff until an artifact is live, so a healthy deployment is
rules-engine complete within seconds of boot. It concedes early on a
__rejected__ artifact (retrying the same bytes cannot end differently) and
gives up after the schedule with a warning -- the proxy serves regardless (an
empty slot only ever abstains into deny-by-default), and the poll keeps trying.

Every iteration is supervised: a transport fault is caught and logged, never
fatal to the task. @notifyFirstSync@ runs after each successful swap (its
consumer, the readiness signal, is an idempotent one-way flip).
-}
runCveSync :: (MonadUnliftIO m, KatipContext m) => SyncEnv -> SyncSchedule -> IO () -> m ()
runCveSync env schedule notifyFirstSync = do
    seen <- burst Nothing (schedBootBackoff schedule)
    poll seen
  where
    eco = show (syncEcosystem env) :: Text

    burst lastSeen delays = do
        (settled, seen') <- attempt lastSeen
        case (settled, delays) of
            (True, _) -> pure seen'
            (False, []) -> do
                logFM WarningS (ls ("cve-sync[" <> eco <> "]: boot fetch did not produce an advisory database; continuing without one, polling"))
                pure seen'
            (False, d : rest) -> do
                threadDelay d
                burst seen' rest

    poll lastSeen = do
        threadDelay (schedPollDelay schedule)
        (_, seen') <- attempt lastSeen
        poll seen'

    -- One supervised step: (the burst may stop, the ETag now last seen).
    attempt lastSeen =
        tryAny (liftIO (syncStep env lastSeen)) >>= \case
            Left err -> do
                logFM ErrorS (ls ("cve-sync[" <> eco <> "]: sync attempt failed: " <> show err))
                pure (False, lastSeen)
            Right (SyncSwapped etag meta) -> do
                logFM InfoS (ls ("cve-sync[" <> eco <> "]: advisory database swapped in: etag=" <> show etag <> " meta=" <> show meta))
                liftIO notifyFirstSync
                pure (True, Just etag)
            Right SyncUnchanged -> do
                logFM DebugS (ls ("cve-sync[" <> eco <> "]: advisory database unchanged"))
                pure (True, lastSeen)
            Right SyncAbsent -> do
                logFM DebugS (ls ("cve-sync[" <> eco <> "]: no advisory database published yet"))
                pure (False, lastSeen)
            Right (SyncRejected etag rejection) -> do
                logFM ErrorS (ls ("cve-sync[" <> eco <> "]: downloaded artifact refused (keeping last good): " <> show rejection))
                -- Remembered so the same bad artifact is not re-downloaded
                -- every poll; a fixed re-publish carries a new ETag. The burst
                -- stops: retrying identical bytes cannot end differently.
                pure (True, Just etag)

{- | The real transport: S3 @HEAD@ for the ETag, bounded streaming @GET@ for
the bytes, against one bucket and key. A @404@ on @HEAD@ is the honest
'Nothing' (not yet published); every other service or transport fault throws
for the sync task's supervision to log.
-}
s3CveFetch :: AWS.Env -> Text -> Text -> Int -> CveFetch
s3CveFetch awsEnv bucket key maxBytes =
    CveFetch
        { fetchHeadEtag =
            runResourceT (AWS.sendEither awsEnv (S3.newHeadObject (S3.BucketName bucket) (S3.ObjectKey key))) >>= \case
                Right resp -> pure (dbEtag <$> resp ^. S3L.headObjectResponse_eTag)
                Left err
                    | isNotFound err -> pure Nothing
                    | otherwise -> throwIO err
        , fetchDownload = \dest -> runResourceT $ do
            resp <- AWS.send awsEnv (S3.newGetObject (S3.BucketName bucket) (S3.ObjectKey key))
            -- The declared length fails fast; the streaming cap is the
            -- enforcement (a declared length is not a guarantee).
            for_ (resp ^. S3L.getObjectResponse_contentLength) $ \len ->
                when (len > fromIntegral maxBytes) (throwIO (OsvDbTooLarge maxBytes))
            AWS.sinkBody (resp ^. S3L.getObjectResponse_body) (cappedAt maxBytes .| C.sinkFile dest)
            maybe (throwIO OsvDbNoEtag) (pure . dbEtag) (resp ^. S3L.getObjectResponse_eTag)
        }
  where
    dbEtag :: S3.ETag -> DbEtag
    dbEtag (S3.ETag bytes) = DbEtag (decodeUtf8 bytes)

    isNotFound = \case
        AWS.ServiceError se -> statusCode (se ^. AWS.serviceError_status) == 404
        _ -> False

-- A pass-through conduit that refuses to stream past the byte cap.
cappedAt :: (MonadIO m) => Int -> ConduitT ByteString ByteString m ()
cappedAt maxBytes = go 0
  where
    go seen =
        await >>= \case
            Nothing -> pass
            Just chunk -> do
                let seen' = seen + BS.length chunk
                when (seen' > maxBytes) (throwIO (OsvDbTooLarge maxBytes))
                yield chunk
                go seen'
