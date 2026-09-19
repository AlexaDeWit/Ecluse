-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Advisory artifact sync and the write side of "Ecluse.Core.Cve.Slot".
Each mount retries at boot, then polls for new artifacts. An empty slot denies by default.
-}
module Ecluse.Runtime.Cve.Sync (
    -- * The injected transport
    CveFetch (..),
    FetchedObject (..),
    DbEtag (..),
    OsvDbFetchFault (..),
    OsvDbCapExceeded (..),
    S3CveSource,
    newS3CveSource,
    s3CveFetchFor,
    cappedAt,

    -- * One sync cycle
    SyncEnv (..),
    SyncOutcome (..),
    syncStep,

    -- * The scheduled task
    SyncSchedule (..),
    SyncHooks (..),
    runCveSync,
    bootBackoffDelays,
    absentReportInterval,
) where

import Conduit (ConduitT, runResourceT, (.|))
import Data.Conduit.Combinators qualified as C
import Data.List (lookup)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Katip (KatipContext, Severity (DebugS, ErrorS, InfoS), logFM, ls)
import Network.HTTP.Types.Status (statusCode)
import System.Directory (removeFile, renameFile)
import UnliftIO (MonadUnliftIO, withRunInIO)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (catch, catchAny, mask, onException, throwIO)

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3
import Amazonka.S3.Lens qualified as S3L
import Lens.Micro ((^.))

import Ecluse.Core.Cve (CveDb (cveDbClose, cveDbMeta), CveDbRejected, DbEtag (..), openCveDb)
import Ecluse.Core.Cve.Slot (AdvisorySource (..), CveSlot, currentAdvisoryEtag, currentAdvisorySource, observeAdvisoryPublication, swapIn)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Fault (TransportFault)
import Ecluse.Core.Osv.Provenance (AdvisoryProvenance (apEpssScoreDate, apOsvNewestModified, apOsvSource))
import Ecluse.Core.Osv.Schema (EpssRequirement, MetaKey (MetaBuiltAt, MetaRowCount), renderMetaKey)
import Ecluse.Core.Security.Authority (authorityLabel)
import Ecluse.Core.Stream (boundBytes)
import Ecluse.Core.Telemetry.Metrics (
    AdvisorySyncResult (AdvisoryFetchFailed, AdvisoryNonePublished, AdvisoryRefused, AdvisorySwapped, AdvisoryUnchanged),
 )
import Ecluse.Core.Telemetry.Record (AdvisorySyncMetricsPort (asmpSyncAttempt, asmpSyncDuration), timedSeconds)
import Ecluse.Core.Telemetry.Span (AdvisorySyncTracingPort (astpSyncAttemptSpan))
import Ecluse.Core.Text (readDecimalText, renderIso8601Utc)
import Ecluse.Runtime.Aws.Env (AwsEndpoint)
import Ecluse.Runtime.Aws.Fault (classifyAwsTransport)
import Ecluse.Runtime.Aws.S3 (buildS3Env)

-- | The advisory transport supplied to 'syncStep' by 'newS3CveSource'.
data CveFetch = CveFetch
    { fetchHead :: IO (Either OsvDbFetchFault (Maybe FetchedObject))
    {- ^ The object's ETag and publication time. @Right Nothing@ means it does not exist.
    Fetch failures use 'Left'.
    -}
    , fetchDownload :: FilePath -> IO (Either OsvDbFetchFault FetchedObject)
    {- ^ Download the artifact to the given path, byte-bounded. The ETag is the download's own, so a
    publish racing the poll is recorded truthfully. A 'Left' may leave a partial file at that path.
    -}
    }

{- | Metadata from one HEAD or GET response. A download carries its own metadata,
so a publication racing HEAD cannot mislabel the downloaded bytes.
-}
data FetchedObject = FetchedObject
    { foEtag :: DbEtag
    , foPushedAt :: Maybe UTCTime
    -- ^ The object's own timestamp, 'Nothing' when the store reported none.
    }
    deriving stock (Eq, Show)

{- | Why an artifact fetch did not yield usable bytes. Every one is a value on the 'CveFetch'
channel, never an exception, and 'syncStep' folds it into its outcome.
-}
data OsvDbFetchFault
    = -- | The object exceeds the configured byte cap (carried, in bytes).
      OsvDbTooLarge Int
    | -- | The response carried no ETag, so there is nothing truthful to record.
      OsvDbNoEtag
    | -- | The transport could not deliver the object (carried, classified).
      OsvDbTransport TransportFault
    deriving stock (Eq, Show)

{- | 'cappedAt' sits in a conduit and has no value channel, so it reports an overstepped byte cap
by throwing this __confined__ exception. 's3Download' catches it and folds it into 'OsvDbTooLarge'.
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
    , syncEpssRequirement :: EpssRequirement
    -- ^ Whether this ecosystem requires successful EPSS enrichment.
    , syncDbPath :: FilePath
    -- ^ The canonical on-disk artifact path (the stable per-ecosystem name).
    , syncSlot :: CveSlot
    -- ^ The slot this task's swaps publish to.
    , syncStoreRef :: Text
    -- ^ How the configured store reads back, for the reports that name where an artifact belongs.
    }

{- | What one 'syncStep' concluded. The caller ('runCveSync') logs it and decides
scheduling.
-}
data SyncOutcome
    = -- | Verification accepted a new artifact and it is now live (its ETag and provenance carried).
      SyncSwapped DbEtag [(Text, Text)]
    | -- | No database replacement, though an accepted republication can advance publication time.
      SyncUnchanged
    | -- | The object does not exist in the bucket (not yet published).
      SyncAbsent
    | {- | The artifact downloaded, and verification __refused__ it. The last-good
      generation keeps serving and the sync remembers the ETag.
      -}
      SyncRejected DbEtag CveDbRejected
    | {- | The fetch itself failed (carried). The step learned nothing about the
      remote artifact, so the last seen ETag stands and the schedule retries.
      -}
      SyncFetchFaulted OsvDbFetchFault
    deriving stock (Show)

{- | One detect-download-verify-swap cycle against the last seen ETag. Total over the fetch and
over verification: a failed fetch and a refused artifact are outcomes, not exceptions.
-}
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

-- Nothing unverified is renamed onto the name the read path opens. The 'onException' guards
-- absorb nothing: they discard the temp file when a filesystem fault escapes, then re-propagate.
syncNewArtifact :: SyncEnv -> IO SyncOutcome
syncNewArtifact env = do
    let temp = syncDbPath env <> ".tmp"
    downloaded <- fetchDownload (syncFetch env) temp `onException` discardTemp temp
    case downloaded of
        Left fault -> do
            -- A byte-cap failure can leave a partial file.
            discardTemp temp
            pure (SyncFetchFaulted fault)
        Right fetched -> do
            opened <- openCveDb (syncEcosystem env) (syncEpssRequirement env) temp `onException` discardTemp temp
            case opened of
                Left rejection -> do
                    discardTemp temp
                    pure (SyncRejected (foEtag fetched) rejection)
                Right db -> publishVerified env temp fetched db

publishVerified :: SyncEnv -> FilePath -> FetchedObject -> CveDb -> IO SyncOutcome
publishVerified env temp fetched db = mask $ \restore -> do
    -- The verified connection follows the inode through the rename. This side still owns it,
    -- so a failure closes the connection and discards the download.
    restore (renameFile temp (syncDbPath env))
        `onException` (cveDbClose db >> discardTemp temp)
    -- 'swapIn' owns the connection from entry, so nothing wraps it: a failure while the displaced
    -- generation drains must never close the newly live database. The mask pins the handoff.
    swapIn (syncSlot env) (foEtag fetched) (foPushedAt fetched) db
    pure (SyncSwapped (foEtag fetched) (cveDbMeta db))

-- Best-effort: the temp may already be renamed away or never created.
discardTemp :: FilePath -> IO ()
discardTemp temp = removeFile temp `catchAny` const pass

{- | The task's timing: the boot burst's backoff delays and the steady poll interval, both in
microseconds. The composition root ships 'bootBackoffDelays' and the configured poll interval.
-}
data SyncSchedule = SyncSchedule
    { schedBootBackoff :: [Int]
    -- ^ Delays before each boot-burst retry. The list's length is the budget.
    , schedPollDelay :: Int
    -- ^ The steady ETag-poll interval.
    , schedAbsentReport :: Int
    -- ^ How long between repeats of the unloaded-database and fetch-failure reports.
    }

{- | The shipped boot-burst backoff: an immediate first attempt, then a retry after each delay,
then the burst concedes to the steady poll. The poll interval, not this, is the operator's knob.
-}
bootBackoffDelays :: [Int]
bootBackoffDelays = [1_000_000, 2_000_000, 4_000_000, 8_000_000, 16_000_000]

{- | The shipped gap, in microseconds, between repeats of the unloaded-database and
fetch-failure reports, so a stuck rollout keeps saying so without filling the log.
-}
absentReportInterval :: Int
absentReportInterval = 900_000_000

{- | What the shell hangs off one sync task. Both run inside the task, so neither may block it,
and both must tolerate being called again.
-}
data SyncHooks = SyncHooks
    { hookFirstSync :: IO ()
    -- ^ Runs after every swap, so it must be idempotent.
    , hookPushAge :: IO ()
    {- ^ Runs after every step, settled or not, so the push age is read on a poll that
    changed nothing.
    -}
    }

-- | Retry at boot, then poll forever. A refused artifact ends the boot burst.
runCveSync ::
    (MonadUnliftIO m, KatipContext m) =>
    AdvisorySyncMetricsPort ->
    AdvisorySyncTracingPort ->
    SyncEnv ->
    SyncSchedule ->
    SyncHooks ->
    m ()
runCveSync metrics tracing env schedule hooks =
    burstCycle loop initialPacing 0 (schedBootBackoff schedule) >>= uncurry (pollCycle loop)
  where
    loop =
        SyncLoop
            { slMetrics = metrics
            , slTracing = tracing
            , slEnv = env
            , slSchedule = schedule
            , slHooks = hooks
            , slEcosystem = show (syncEcosystem env)
            }

-- Everything the loop's arms read. The ecosystem label is rendered once, at the top of the task.
data SyncLoop = SyncLoop
    { slMetrics :: AdvisorySyncMetricsPort
    , slTracing :: AdvisorySyncTracingPort
    , slEnv :: SyncEnv
    , slSchedule :: SyncSchedule
    , slHooks :: SyncHooks
    , slEcosystem :: Text
    }

{- The loop's pacing of its two repeating reports, both on 'schedAbsentReport': the time since the
unloaded-database report, and the time since the fetch-failure report while fetches keep failing. -}
data Pacing = Pacing
    { pacUnloaded :: Int
    , pacFetchFailure :: Maybe Int
    }

initialPacing :: Pacing
initialPacing = Pacing{pacUnloaded = 0, pacFetchFailure = Nothing}

loopStep :: (MonadUnliftIO m, KatipContext m) => SyncLoop -> Maybe DbEtag -> m Stepped
loopStep loop lastSeen = do
    stepped <-
        observedStep
            (slMetrics loop)
            (slTracing loop)
            (slEnv loop)
            (slEcosystem loop)
            (hookFirstSync (slHooks loop))
            lastSeen
    liftIO (hookPushAge (slHooks loop))
    pure stepped

-- Each attempt reads 'Nothing' as last seen, because a not-settled outcome never advances it.
-- The burst concedes to the steady poll once its delays are spent.
burstCycle :: (MonadUnliftIO m, KatipContext m) => SyncLoop -> Pacing -> Int -> [Int] -> m (Pacing, Maybe DbEtag)
burstCycle loop pacing delta delays = do
    stepped <- loopStep loop Nothing
    pacing' <- reportFetch loop delta stepped pacing
    case delays of
        _ | stSettled stepped -> pure (pacing', stSeen stepped)
        [] -> (pacing', stSeen stepped) <$ reportLoopUnloaded loop (stResult stepped)
        delay : rest -> threadDelay delay >> burstCycle loop pacing' delay rest

pollCycle :: (MonadUnliftIO m, KatipContext m) => SyncLoop -> Pacing -> Maybe DbEtag -> m ()
pollCycle loop pacing lastSeen = do
    threadDelay pollDelay
    stepped <- loopStep loop lastSeen
    pacing' <- reportFetch loop pollDelay stepped pacing
    unloaded <- repeatUnloaded loop (stResult stepped) (pacUnloaded pacing' + pollDelay)
    pollCycle loop pacing'{pacUnloaded = unloaded} (stSeen stepped)
  where
    pollDelay = schedPollDelay (slSchedule loop)

reportFetch :: (KatipContext m) => SyncLoop -> Int -> Stepped -> Pacing -> m Pacing
reportFetch loop delta stepped pacing = do
    let (fetching, report) =
            paceFetchFailure (schedAbsentReport (slSchedule loop)) delta (stFault stepped) (pacFetchFailure pacing)
    traverse_ (reportFetchHealth (slEcosystem loop)) report
    pure pacing{pacFetchFailure = fetching}

-- The report repeats only while the slot has never been filled, so the first swap ends it
-- and a later outage starts the interval again.
repeatUnloaded :: (KatipContext m) => SyncLoop -> AdvisorySyncResult -> Int -> m Int
repeatUnloaded loop result elapsed =
    liftIO (currentAdvisoryEtag (syncSlot (slEnv loop))) >>= \case
        Just _ -> pure 0
        Nothing
            | elapsed < schedAbsentReport (slSchedule loop) -> pure elapsed
            | otherwise -> 0 <$ reportLoopUnloaded loop result

reportLoopUnloaded :: (KatipContext m) => SyncLoop -> AdvisorySyncResult -> m ()
reportLoopUnloaded loop = reportUnloaded (slEcosystem loop) (syncStoreRef (slEnv loop))

-- What one paced fetch outcome reports, if anything.
data FetchHealth
    = FetchFailing OsvDbFetchFault
    | FetchStillFailing OsvDbFetchFault
    | FetchRecovered

{- Pace the fetch-failure report: the first failure reports at once, a later one only after
@interval@, and the first fetch that succeeds after a failure reports the recovery. -}
paceFetchFailure :: Int -> Int -> Maybe OsvDbFetchFault -> Maybe Int -> (Maybe Int, Maybe FetchHealth)
paceFetchFailure interval delta fault failing = case (fault, failing) of
    (Nothing, Nothing) -> (Nothing, Nothing)
    (Nothing, Just _) -> (Nothing, Just FetchRecovered)
    (Just f, Nothing) -> (Just 0, Just (FetchFailing f))
    (Just f, Just elapsed)
        | elapsed + delta >= interval -> (Just 0, Just (FetchStillFailing f))
        | otherwise -> (Just (elapsed + delta), Nothing)

{- The store that keeps failing ages the serving artifact towards its maximum, so the failure logs at
the level an operator pages on. The fault names the transport cause, never a credential. -}
reportFetchHealth :: (KatipContext m) => Text -> FetchHealth -> m ()
reportFetchHealth eco = \case
    FetchFailing fault -> logFM ErrorS (ls ("cve-sync[" <> eco <> "]: sync fetch failed: " <> show fault))
    FetchStillFailing fault -> logFM ErrorS (ls ("cve-sync[" <> eco <> "]: sync fetch still failing: " <> show fault))
    FetchRecovered -> logFM InfoS (ls ("cve-sync[" <> eco <> "]: sync fetch recovered"))

{- The line an operator alerts on while nothing is loaded. Its cause separates an artifact never
published from one verification refused and from an access that keeps failing. -}
reportUnloaded :: (KatipContext m) => Text -> Text -> AdvisorySyncResult -> m ()
reportUnloaded eco store result =
    logFM ErrorS (ls ("cve-sync[" <> eco <> "]: " <> unloadedCause store result))

-- Why nothing is loaded, worded to name the role or the access that has to change.
unloadedCause :: Text -> AdvisorySyncResult -> Text
unloadedCause store = \case
    AdvisoryNonePublished ->
        "no advisory artifact has ever been published to " <> store <> ", and ecluse pilot is what compiles and publishes one. This ecosystem stays not-ready and its advisory denies refuse until an artifact lands."
    AdvisoryRefused -> refusedArtifact
    -- A poll that finds nothing changed while nothing is loaded is the refused artifact standing.
    AdvisoryUnchanged -> refusedArtifact
    AdvisoryFetchFailed ->
        "no advisory database could be fetched from " <> store <> ": this ecosystem stays not-ready and denies by default until one loads. Continuing to poll; investigate the bucket, object, or IAM if this persists."
    AdvisorySwapped ->
        "no advisory database is loaded from " <> store <> ", so this ecosystem stays not-ready and denies by default until one is."
  where
    refusedArtifact =
        "the advisory artifact at " <> store <> " was refused by verification, so nothing is loaded and this ecosystem stays not-ready. The refusal line names what failed, and ecluse pilot must publish an artifact that verifies."

-- One observed step as the loop reads it. A fetch fault rides along for the loop's paced report.
data Stepped = Stepped
    { stResult :: AdvisorySyncResult
    , stSettled :: Bool
    -- ^ Whether the boot burst may stop.
    , stSeen :: Maybe DbEtag
    -- ^ The ETag now last seen.
    , stFault :: Maybe OsvDbFetchFault
    }

steppedOf :: AdvisorySyncResult -> Bool -> Maybe DbEtag -> Stepped
steppedOf result settled seen =
    Stepped{stResult = result, stSettled = settled, stSeen = seen, stFault = Nothing}

-- One observed step: the attempt, timed and labelled, inside this ecosystem's attempt span.
observedStep ::
    (MonadUnliftIO m, KatipContext m) =>
    AdvisorySyncMetricsPort ->
    AdvisorySyncTracingPort ->
    SyncEnv ->
    Text ->
    IO () ->
    Maybe DbEtag ->
    m Stepped
observedStep metrics tracing env eco notifyFirstSync lastSeen =
    withRunInIO $ \runInIO ->
        astpSyncAttemptSpan
            tracing
            ecosystem
            stResult
            (meteredStep metrics ecosystem (runInIO (attemptStep env eco notifyFirstSync lastSeen)))
  where
    ecosystem = syncEcosystem env

-- Residue escaping the attempt bypasses these records. An attempt that never concluded has no
-- result to label, and the supervision above reports it.
meteredStep :: AdvisorySyncMetricsPort -> Ecosystem -> IO Stepped -> IO Stepped
meteredStep metrics ecosystem act = do
    (attempted, seconds) <- timedSeconds act
    asmpSyncAttempt metrics ecosystem (stResult attempted)
    asmpSyncDuration metrics ecosystem (stResult attempted) seconds
    pure attempted

attemptStep :: (KatipContext m) => SyncEnv -> Text -> IO () -> Maybe DbEtag -> m Stepped
attemptStep env eco notifyFirstSync lastSeen =
    liftIO (syncStep env lastSeen) >>= \case
        SyncFetchFaulted fault ->
            -- The step learned nothing about the remote artifact, so the last seen ETag and
            -- the last good database both stand and the next poll retries.
            pure (steppedOf AdvisoryFetchFailed False lastSeen){stFault = Just fault}
        SyncSwapped etag meta -> do
            logFM InfoS (ls ("cve-sync[" <> eco <> "]: advisory database swapped in: etag=" <> show etag <> " meta=" <> show (metadataSummary meta)))
            source <- liftIO (currentAdvisorySource (syncSlot env))
            logFM InfoS (ls ("cve-sync[" <> eco <> "]: serving artifact source: " <> maybe unrecordedValue renderAdvisorySource source))
            whenNothing_ (asPushedAt =<< source) (undatedArtifact eco etag)
            liftIO notifyFirstSync
            pure (steppedOf AdvisorySwapped True (Just etag))
        SyncUnchanged -> do
            logFM DebugS (ls ("cve-sync[" <> eco <> "]: advisory database unchanged"))
            pure (steppedOf AdvisoryUnchanged True lastSeen)
        SyncAbsent -> do
            logFM DebugS (ls ("cve-sync[" <> eco <> "]: no advisory database published yet"))
            pure (steppedOf AdvisoryNonePublished False lastSeen)
        SyncRejected etag rejection -> do
            logFM ErrorS (ls ("cve-sync[" <> eco <> "]: downloaded artifact refused (keeping last good): " <> show rejection))
            -- Remember the ETag so the same refused artifact is not re-downloaded.
            -- A fixed re-publish carries a new one. Identical bytes cannot end differently.
            pure (steppedOf AdvisoryRefused True (Just etag))

{- An artifact the object store gave no publication time for: its age cannot be established, so
CVE-based denial refuses on it. One line per swap, because only a swap can install one. -}
undatedArtifact :: (KatipContext m) => Text -> DbEtag -> m ()
undatedArtifact eco etag =
    logFM
        ErrorS
        ( ls
            ( "cve-sync["
                <> eco
                <> "]: the object store reported no publication time for the artifact it served (etag="
                <> show etag
                <> "), so its age cannot be established and CVE-based denial refuses until a push carries one"
            )
        )

{- Where the serving artifact came from, for the swap line. The source renders as its authority
alone, on the same rule as 'metadataSummary' below: artifact text never reaches a log verbatim. -}
renderAdvisorySource :: AdvisorySource -> Text
renderAdvisorySource source =
    "pushed_at="
        <> renderStamp (asPushedAt source)
        <> " osv_source="
        <> maybe unrecordedValue authorityLabel (apOsvSource prov)
        <> " osv_newest_modified="
        <> renderStamp (apOsvNewestModified prov)
        <> " epss_score_date="
        <> renderStamp (apEpssScoreDate prov)
  where
    prov = asProvenance source

renderStamp :: Maybe UTCTime -> Text
renderStamp = maybe unrecordedValue renderIso8601Utc

-- What a value the artifact never recorded reads as, so absence is not read as a zero.
unrecordedValue :: Text
unrecordedValue = "<unrecorded>"

-- Legacy artifacts contain arbitrary text. Only parsed, bounded values reach the log.
metadataSummary :: [(Text, Text)] -> (Maybe UTCTime, Maybe Word64)
metadataSummary meta =
    ( boundedValue MetaBuiltAt 64 >>= iso8601ParseM . toString
    , boundedValue MetaRowCount 20 >>= parseMetadataCount
    )
  where
    boundedValue key limit = do
        value <- lookup (renderMetaKey key) meta
        guard (T.compareLength value limit /= GT)
        pure value

parseMetadataCount :: Text -> Maybe Word64
parseMetadataCount value = do
    count <- readDecimalText value :: Maybe Integer
    guard (count <= toInteger (maxBound :: Word64))
    pure (fromInteger count)

{- | An S3-backed advisory-fetch source. 'newS3CveSource' captures one @amazonka@ 'AWS.Env', so
every mount's 'CveFetch' shares one credential discovery. The composition shell never sees it.
-}
newtype S3CveSource = S3CveSource
    { s3CveFetchFor :: Text -> Text -> Int -> CveFetch
    -- ^ A 'CveFetch' against one bucket, object key, and byte cap, over the captured env.
    }

-- | Build an 'S3CveSource' over one S3 @amazonka@ env, honouring the resolved endpoint override.
newS3CveSource :: Maybe AwsEndpoint -> IO S3CveSource
newS3CveSource mEndpoint = do
    awsEnv <- buildS3Env mEndpoint
    pure (S3CveSource (s3CveFetch awsEnv))

s3CveFetch :: AWS.Env -> Text -> Text -> Int -> CveFetch
s3CveFetch awsEnv bucket key maxBytes =
    CveFetch
        { fetchHead = s3Head awsEnv bucket key
        , fetchDownload = s3Download awsEnv bucket key maxBytes
        }

s3Head :: AWS.Env -> Text -> Text -> IO (Either OsvDbFetchFault (Maybe FetchedObject))
s3Head awsEnv bucket key =
    runResourceT (AWS.sendEither awsEnv (S3.newHeadObject (S3.BucketName bucket) (S3.ObjectKey key))) <&> \case
        Right resp ->
            let observed etag = FetchedObject (dbEtag etag) (resp ^. S3L.headObjectResponse_lastModified)
             in maybe (Left OsvDbNoEtag) (Right . Just . observed) (resp ^. S3L.headObjectResponse_eTag)
        Left err
            | isNotFound err -> Right Nothing
            | otherwise -> Left (OsvDbTransport (classifyAwsTransport err))

s3Download :: AWS.Env -> Text -> Text -> Int -> FilePath -> IO (Either OsvDbFetchFault FetchedObject)
s3Download awsEnv bucket key maxBytes dest = foldFetchEscapes . runResourceT $ do
    resp <- AWS.send awsEnv (S3.newGetObject (S3.BucketName bucket) (S3.ObjectKey key))
    -- The declared length fails fast. The streaming cap is the enforcement: a
    -- declared length is not a guarantee.
    for_ (resp ^. S3L.getObjectResponse_contentLength) $ \len ->
        when (len > fromIntegral maxBytes) (throwIO (OsvDbCapExceeded maxBytes))
    AWS.sinkBody (resp ^. S3L.getObjectResponse_body) (cappedAt maxBytes .| C.sinkFile dest)
    let fetched etag = FetchedObject{foEtag = dbEtag etag, foPushedAt = resp ^. S3L.getObjectResponse_lastModified}
    pure (maybe (Left OsvDbNoEtag) (Right . fetched) (resp ^. S3L.getObjectResponse_eTag))

-- The adapter boundary: fold the two typed escapes into the value channel. Nothing else is
-- caught, so a filesystem fault writing the destination propagates as residue.
foldFetchEscapes :: IO (Either OsvDbFetchFault FetchedObject) -> IO (Either OsvDbFetchFault FetchedObject)
foldFetchEscapes act =
    act
        `catch` (\(err :: AWS.Error) -> pure (Left (OsvDbTransport (classifyAwsTransport err))))
        `catch` (\(OsvDbCapExceeded n) -> pure (Left (OsvDbTooLarge n)))

dbEtag :: S3.ETag -> DbEtag
dbEtag (S3.ETag bytes) = DbEtag (decodeUtf8 bytes)

isNotFound :: AWS.Error -> Bool
isNotFound = \case
    AWS.ServiceError se -> statusCode (se ^. AWS.serviceError_status) == 404
    _ -> False

{- | A breach throws 'OsvDbCapExceeded' before yielding the excess chunk.
The S3 adapter folds it into 'OsvDbTooLarge'.
-}
cappedAt :: (MonadIO m) => Int -> ConduitT ByteString ByteString m ()
cappedAt maxBytes = boundBytes maxBytes (const (throwIO (OsvDbCapExceeded maxBytes)))
