-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The advisory database's sync mechanics: detect a new @osv.db@ artifact in
object storage, download it bounded, verify it, and shadow-swap it into the
read path, one ecosystem per task, driven by the configured mounts.

The write side of "Ecluse.Core.Cve.Slot": 'syncStep' performs exactly one
detect-download-verify-swap cycle over an injected 'CveFetch' (so unit tests
drive it without a network), and 'runCveSync' schedules those steps: an eager
__boot burst__ (an immediate attempt, retried with incremental backoff, that is
eventually allowed to fail so a broken bucket never wedges startup) followed by
the steady ETag poll. The proxy is rules-engine complete as early as the
artifact can be had; before then it serves deny-by-default.

The swap's file discipline: the download lands in a temp file beside the
canonical per-ecosystem path, and 'Ecluse.Core.Cve.openCveDb' verifies the
temp file (epoch stamp, table shape, ecosystem), the artifact contract's
verify-before-swap. __The connection that verified is the connection that
serves__: the accepted temp file is renamed atomically onto the canonical
name, the open connection follows the inode through the rename, and that same
'CveDb' is swapped in; there is no reopen and so no verify-to-serve gap. The
displaced generation drains and closes inside 'Ecluse.Core.Cve.Slot.swapIn', releasing
the old inode's last reference; reclamation is the kernel's, never a delete
this code could mistime. A rejected artifact is deleted, its ETag remembered
(re-downloading a known-bad object every poll buys nothing), and the last-good
generation keeps serving.
-}
module Ecluse.Runtime.Cve.Sync (
    -- * The injected transport
    CveFetch (..),
    DbEtag (..),
    OsvDbFetchFault (..),
    OsvDbCapExceeded (..),
    s3CveFetch,
    cappedAt,

    -- * One sync cycle
    SyncEnv (..),
    SyncOutcome (..),
    syncStep,

    -- * The scheduled task
    SyncSchedule (..),
    runCveSync,
    bootBackoffDelays,
    bootBurstPolicy,
) where

import Conduit (ConduitT, await, runResourceT, yield, (.|))
import Control.Retry (RetryPolicyM, RetryStatus (rsIterNumber), retryPolicy, retrying)
import Data.ByteString qualified as BS
import Data.Conduit.Combinators qualified as C
import Katip (KatipContext, Severity (DebugS, ErrorS, InfoS), logFM, ls)
import Network.HTTP.Types.Status (statusCode)
import System.Directory (removeFile, renameFile)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (catch, catchAny, mask, onException, throwIO)

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3
import Amazonka.S3.Lens qualified as S3L
import Lens.Micro ((^.))

import Ecluse.Core.Cve (CveDb (cveDbClose, cveDbMeta), CveDbRejected, DbEtag (..), openCveDb)
import Ecluse.Core.Cve.Slot (CveSlot, swapIn)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Fault (TransportFault)
import Ecluse.Runtime.Aws.Fault (classifyAwsTransport)

{- | The sync transport, as data: how to learn the remote artifact's current
version and how to fetch its bytes. Injected so 'syncStep' is unit-testable
without a network; the composition root supplies 's3CveFetch'.
-}
data CveFetch = CveFetch
    { fetchHeadEtag :: IO (Either OsvDbFetchFault (Maybe DbEtag))
    {- ^ The remote artifact's current ETag; @Right Nothing@ when the object does
    not exist (not yet published for this ecosystem). Every fetch failure --
    a transport fault included -- is the 'Left' value.
    -}
    , fetchDownload :: FilePath -> IO (Either OsvDbFetchFault DbEtag)
    {- ^ Download the artifact to the given path (byte-bounded) and return the
    ETag of the bytes actually fetched, the download's own rather than an
    earlier @HEAD@'s, so a publish racing the poll is recorded truthfully.
    Every fetch failure -- an overstepped byte cap, a missing ETag, a transport
    fault -- is the 'Left' value; a 'Left' may leave a partial file at the
    given path for the caller to discard.
    -}
    }

{- | Why an artifact fetch did not yield usable bytes: refused by this side (the
object oversteps the configured byte cap, or the response carried no ETag to
record), or not delivered at all (a transport fault, classified into the core
vocabulary at the adapter edge). A value on the 'CveFetch' channel, never an
exception: the sync task's step folds it into its outcome and the schedule
retries.
-}
data OsvDbFetchFault
    = -- | The object exceeds the configured byte cap (carried, in bytes).
      OsvDbTooLarge Int
    | -- | The response carried no ETag; nothing truthful to record.
      OsvDbNoEtag
    | -- | The transport could not deliver the object (carried, classified).
      OsvDbTransport TransportFault
    deriving stock (Eq, Show)

{- | The byte cap's mid-stream escape hatch: 'cappedAt' sits inside a conduit
pipeline (no value channel of its own), so it reports an overstepped cap by
throwing this -- __confined__ typed exception, caught at the adapter boundary
('s3Download') and folded into 'OsvDbTooLarge'. It never crosses the 'CveFetch'
interface.
-}
newtype OsvDbCapExceeded = OsvDbCapExceeded Int
    deriving stock (Eq, Show)

instance Exception OsvDbCapExceeded

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
decides scheduling.
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
    | {- | The fetch itself failed (carried); nothing was learned about the
      remote artifact, so the last seen ETag stands and the schedule retries.
      -}
      SyncFetchFaulted OsvDbFetchFault
    deriving stock (Show)

{- | One detect-download-verify-swap cycle against the last seen ETag. Total
over the fetch and over verification -- a failed fetch and a refused artifact
are both outcomes, not exceptions -- so the caller's scheduling is a plain fold
over 'SyncOutcome'. See the module header for the file discipline.
-}
syncStep :: SyncEnv -> Maybe DbEtag -> IO SyncOutcome
syncStep env lastSeen =
    fetchHeadEtag (syncFetch env) >>= \case
        Left fault -> pure (SyncFetchFaulted fault)
        Right Nothing -> pure SyncAbsent
        Right (Just remote)
            | Just remote == lastSeen -> pure SyncUnchanged
            | otherwise -> syncNewArtifact env

-- The 'onException' guards absorb nothing: they discard the temp file when a
-- fault __below__ the typed channels escapes (a filesystem error writing or
-- opening the temp path), then re-propagate it as the residue it is.
syncNewArtifact :: SyncEnv -> IO SyncOutcome
syncNewArtifact env = do
    let temp = syncDbPath env <> ".tmp"
    downloaded <- fetchDownload (syncFetch env) temp `onException` discardTemp temp
    case downloaded of
        Left fault -> do
            -- A failed download may have written partial bytes to the temp path
            -- (the byte cap trips mid-stream); discard them.
            discardTemp temp
            pure (SyncFetchFaulted fault)
        Right fetched -> do
            opened <- openCveDb (syncEcosystem env) temp `onException` discardTemp temp
            case opened of
                Left rejection -> do
                    discardTemp temp
                    pure (SyncRejected fetched rejection)
                Right db -> publishVerified env temp fetched db

publishVerified :: SyncEnv -> FilePath -> DbEtag -> CveDb -> IO SyncOutcome
publishVerified env temp fetched db = mask $ \restore -> do
    -- The verified connection follows the inode through the rename; the
    -- canonical name now holds the newest accepted artifact and the temp name
    -- is gone. Up to here this side still owns the connection, so a failure
    -- closes it and discards the download.
    restore (renameFile temp (syncDbPath env))
        `onException` (cveDbClose db >> discardTemp temp)
    -- 'swapIn' publishes atomically before it retires the displaced
    -- generation, and owns the connection from entry: a failure or
    -- cancellation while the displaced generation drains must never close the
    -- newly live database, so no cleanup wraps it. The mask pins the
    -- ownership handoff; the drain wait inside stays interruptible.
    swapIn (syncSlot env) fetched db
    pure (SyncSwapped fetched (cveDbMeta db))

-- Best-effort: the temp may already be renamed away or never created.
discardTemp :: FilePath -> IO ()
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

{- | The boot-burst backoff schedule compiled to a "Control.Retry" policy: the
n-th retry waits the n-th delay (microseconds) before it, and the policy stops
(yields 'Nothing') once the list is spent, so the list's length is the retry
budget. Inspect the schedule without sleeping with 'Control.Retry.simulatePolicy'.
-}
bootBurstPolicy :: (Monad m) => [Int] -> RetryPolicyM m
bootBurstPolicy delays = retryPolicy (\rs -> delays !!? rsIterNumber rs)

{- | One ecosystem's sync task: the boot burst, then the steady poll, forever.

The __boot burst__ attempts a sync immediately and retries per the schedule's
backoff until an artifact is live, so a healthy deployment is
rules-engine complete within seconds of boot. It concedes early on a
__rejected__ artifact (retrying the same bytes cannot end differently) and
gives up after the schedule with a warning. The proxy serves regardless, since
an empty slot only ever abstains into deny-by-default, and the poll keeps
trying.

A fetch fault arrives as a value in the step's outcome and is logged here;
residue (a filesystem fault on the temp path, a contract escape) propagates to
the supervision the composition root wraps this task in
('Ecluse.Core.Supervision.superviseLoop'), which restarts the task -- it simply
resumes from the remote artifact. @notifyFirstSync@ runs after each successful
swap (its consumer, the readiness signal, is an idempotent one-way flip).
-}
runCveSync :: (MonadUnliftIO m, KatipContext m) => SyncEnv -> SyncSchedule -> IO () -> m ()
runCveSync env schedule notifyFirstSync = do
    seen <- burst
    poll seen
  where
    eco = show (syncEcosystem env) :: Text

    -- The boot burst under 'Control.Retry': an immediate first attempt, then a
    -- retry on each not-settled outcome per 'bootBurstPolicy' until an artifact
    -- settles the step or the schedule is spent. 'lastSeen' is fixed at 'Nothing'
    -- because the only not-settled outcomes ('SyncAbsent', 'SyncFetchFaulted')
    -- return it untouched, so it never changes across the burst; the settled ETag
    -- is what 'poll' resumes from.
    burst = do
        (settled, seen') <-
            retrying
                (bootBurstPolicy (schedBootBackoff schedule))
                (\_ (done, _) -> pure (not done))
                (\_ -> loggedStep env eco notifyFirstSync Nothing)
        unless settled $
            -- The boot budget is spent without an artifact. This ecosystem stays
            -- not-ready (the readiness gate reads 'csReady'), so its rules deny by
            -- default and no traffic is served against a missing advisory database;
            -- the poll continues in case the artifact appears later. Logged at
            -- 'ErrorS' because a persistent failure here is a real misconfiguration
            -- (bucket, object key, or IAM), not a condition a healthy deploy hits.
            logFM ErrorS (ls ("cve-sync[" <> eco <> "]: boot fetch did not acquire an advisory database within the boot budget; this ecosystem stays not-ready and denies by default until one is acquired. Continuing to poll; investigate the bucket, object, or IAM if this persists."))
        pure seen'

    poll lastSeen = do
        threadDelay (schedPollDelay schedule)
        (_, seen') <- loggedStep env eco notifyFirstSync lastSeen
        poll seen'

-- One logged step: (the burst may stop, the ETag now last seen). 'syncStep' is
-- total over the fetch and over verification, so this is a plain fold over
-- 'SyncOutcome' -- nothing is caught, and residue propagates to the task's
-- supervision at the composition root.
loggedStep :: (MonadUnliftIO m, KatipContext m) => SyncEnv -> Text -> IO () -> Maybe DbEtag -> m (Bool, Maybe DbEtag)
loggedStep env eco notifyFirstSync lastSeen =
    liftIO (syncStep env lastSeen) >>= \case
        SyncFetchFaulted fault -> do
            -- Nothing was learned about the remote artifact: keep the last seen
            -- ETag, let the burst retry (or the poll try again next interval).
            logFM ErrorS (ls ("cve-sync[" <> eco <> "]: sync fetch failed: " <> show fault))
            pure (False, lastSeen)
        SyncSwapped etag meta -> do
            logFM InfoS (ls ("cve-sync[" <> eco <> "]: advisory database swapped in: etag=" <> show etag <> " meta=" <> show meta))
            liftIO notifyFirstSync
            pure (True, Just etag)
        SyncUnchanged -> do
            logFM DebugS (ls ("cve-sync[" <> eco <> "]: advisory database unchanged"))
            pure (True, lastSeen)
        SyncAbsent -> do
            logFM DebugS (ls ("cve-sync[" <> eco <> "]: no advisory database published yet"))
            pure (False, lastSeen)
        SyncRejected etag rejection -> do
            logFM ErrorS (ls ("cve-sync[" <> eco <> "]: downloaded artifact refused (keeping last good): " <> show rejection))
            -- Remembered so the same bad artifact is not re-downloaded
            -- every poll; a fixed re-publish carries a new ETag. The burst
            -- stops: retrying identical bytes cannot end differently.
            pure (True, Just etag)

{- | The real transport: S3 @HEAD@ for the ETag, bounded streaming @GET@ for
the bytes, against one bucket and key. A @404@ on @HEAD@ is the honest
@Right Nothing@ (not yet published); every other service or transport fault is
the 'Left' value, classified into the core vocabulary at this edge.
-}
s3CveFetch :: AWS.Env -> Text -> Text -> Int -> CveFetch
s3CveFetch awsEnv bucket key maxBytes =
    CveFetch
        { fetchHeadEtag = s3HeadEtag awsEnv bucket key
        , fetchDownload = s3Download awsEnv bucket key maxBytes
        }

s3HeadEtag :: AWS.Env -> Text -> Text -> IO (Either OsvDbFetchFault (Maybe DbEtag))
s3HeadEtag awsEnv bucket key =
    runResourceT (AWS.sendEither awsEnv (S3.newHeadObject (S3.BucketName bucket) (S3.ObjectKey key))) <&> \case
        Right resp -> Right (dbEtag <$> resp ^. S3L.headObjectResponse_eTag)
        Left err
            | isNotFound err -> Right Nothing
            | otherwise -> Left (OsvDbTransport (classifyAwsTransport err))

s3Download :: AWS.Env -> Text -> Text -> Int -> FilePath -> IO (Either OsvDbFetchFault DbEtag)
s3Download awsEnv bucket key maxBytes dest = classified . runResourceT $ do
    resp <- AWS.send awsEnv (S3.newGetObject (S3.BucketName bucket) (S3.ObjectKey key))
    -- The declared length fails fast; the streaming cap is the
    -- enforcement (a declared length is not a guarantee).
    for_ (resp ^. S3L.getObjectResponse_contentLength) $ \len ->
        when (len > fromIntegral maxBytes) (throwIO (OsvDbCapExceeded maxBytes))
    AWS.sinkBody (resp ^. S3L.getObjectResponse_body) (cappedAt maxBytes .| C.sinkFile dest)
    pure (maybe (Left OsvDbNoEtag) (Right . dbEtag) (resp ^. S3L.getObjectResponse_eTag))
  where
    -- The adapter boundary: fold the two typed escapes into the value channel --
    -- amazonka's error sum ('AWS.send' throws it) into 'OsvDbTransport', the
    -- streaming cap's confined 'OsvDbCapExceeded' into 'OsvDbTooLarge'. Nothing
    -- else is caught: a filesystem fault writing the destination propagates as
    -- residue for the sync task's supervision to log.
    classified :: IO (Either OsvDbFetchFault DbEtag) -> IO (Either OsvDbFetchFault DbEtag)
    classified act =
        act
            `catch` (\(err :: AWS.Error) -> pure (Left (OsvDbTransport (classifyAwsTransport err))))
            `catch` (\(OsvDbCapExceeded n) -> pure (Left (OsvDbTooLarge n)))

dbEtag :: S3.ETag -> DbEtag
dbEtag (S3.ETag bytes) = DbEtag (decodeUtf8 bytes)

isNotFound :: AWS.Error -> Bool
isNotFound = \case
    AWS.ServiceError se -> statusCode (se ^. AWS.serviceError_status) == 404
    _ -> False

{- | A pass-through conduit that refuses to stream past the byte cap: the
enforcement behind 's3CveFetch''s bounded download, where the declared content
length is only the fast-fail. A breach throws the confined 'OsvDbCapExceeded'
(a conduit has no value channel of its own); the adapter boundary folds it into
'OsvDbTooLarge'.
-}
cappedAt :: (MonadIO m) => Int -> ConduitT ByteString ByteString m ()
cappedAt maxBytes = go 0
  where
    go seen =
        await >>= \case
            Nothing -> pass
            Just chunk -> do
                let seen' = seen + BS.length chunk
                when (seen' > maxBytes) (throwIO (OsvDbCapExceeded maxBytes))
                yield chunk
                go seen'
