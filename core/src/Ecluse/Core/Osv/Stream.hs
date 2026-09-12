-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Streaming ingest of the osv.dev export archive Pilot compiles @osv.db@ from. The feed
aggregates many upstream databases, so one poisoned record can ride in with every transport
header honest, and the bounds here are per entry: a drop is counted in 'IngestStats' and the
rest of the archive keeps flowing. 'ilMaxAdvisoryBytes' applies before the bytes are retained
and before the JSON decodes, so an inflation bomb never reaches the decoder whole, and the
offending entry drains to its boundary so the entries after it stay aligned. An advisory over
the feed's 'osvMaxAdvisoryFanOut' is anomalous, logged, and kept. The aggregate verdict is the
separate pure decision 'systemicDrop', which the compiler reads once the stream completes.
-}
module Ecluse.Core.Osv.Stream (
    streamOsvUrl,
    parseOsvStream,

    -- * Ingest bounds and drop accounting
    IngestLimits (..),
    defaultIngestLimits,
    IngestStats (..),
    OsvIngest,
    newOsvIngest,
    readIngestStats,
    resetIngestStats,
    systemicDrop,
    PilotIngestAborted (..),

    -- * What one attempt learned about its source
    OsvAttempt (..),
    readOsvAttempt,
    resetOsvAttempt,
) where

import Codec.Archive.Zip.Conduit.Types (ZipEntry (..))
import Codec.Archive.Zip.Conduit.UnZip (unZipStream)
import Conduit
import Data.Aeson (decodeStrict)
import Data.ByteString qualified as BS
import Data.Time (UTCTime)
import Katip (KatipContext, Severity (..), logFM, ls)
import Network.HTTP.Simple (getResponseBody, getResponseHeader, httpSource, parseRequest, setRequestCheckStatus)
import Network.HTTP.Types.Header (hLastModified)
import OpenTelemetry.Trace.Core (SpanKind (Internal), TracerProvider, addAttribute)

import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Osv.Advisory (ExtractedOsv, OsvAdvisory, extPackage, extractFromAdvisory, orderableBounds, osvId, osvModified, unorderableBounds)
import Ecluse.Core.Osv.Ecosystem (OsvEcosystem (osvEcosystemTag, osvMaxAdvisoryFanOut))
import Ecluse.Core.Osv.Epss (EpssScores)
import Ecluse.Core.Osv.Provenance (parseHttpDate)
import Ecluse.Core.Security.Authority (authorityLabel)
import Ecluse.Core.Telemetry.Span (closeOptionalSpan, openOptionalSpan)

-- | The per-advisory byte bound one ingest pass holds every zip entry to.
newtype IngestLimits = IngestLimits
    { ilMaxAdvisoryBytes :: Int
    {- ^ Largest decompressed advisory JSON, in bytes, the ingest accepts from one
    zip entry. It drops a larger one. Bounds memory and, transitively, decode cost.
    -}
    }
    deriving stock (Eq, Show)

-- | An 8 MiB per-advisory ceiling.
defaultIngestLimits :: IngestLimits
defaultIngestLimits = IngestLimits{ilMaxAdvisoryBytes = 8 * 1024 * 1024}

{- | The running tally of one ingest pass. Pilot reads it once the stream completes to
decide whether the artifact is trustworthy enough to publish ('systemicDrop').
-}
data IngestStats = IngestStats
    { statAccepted :: !Int
    -- ^ Advisory entries that decoded successfully.
    , statDroppedOversize :: !Int
    -- ^ Entries dropped for breaching 'ilMaxAdvisoryBytes'.
    , statDroppedMalformed :: !Int
    -- ^ Entries dropped because their JSON did not decode.
    , statUnorderable :: !Int
    {- ^ Rows kept with a bound the grammar cannot parse ('orderableBounds'). Counted in
    rows, so it stays out of 'systemicDrop'.
    -}
    , statFutureModified :: !Int
    {- ^ Records whose @modified@ is dated after the run's clock. Their rows are kept and
    only their date is ignored, so this counts records and stays out of 'systemicDrop'.
    -}
    }
    deriving stock (Eq, Show)

emptyIngestStats :: IngestStats
emptyIngestStats = IngestStats 0 0 0 0 0

{- | What one ingest attempt learned about its source beside the rows. A retry replaces it
whole, so it always describes the attempt that produced the tally beside it.
-}
data OsvAttempt = OsvAttempt
    { oaLastModified :: Maybe UTCTime
    -- ^ The @Last-Modified@ the export answered this attempt with.
    , oaNewestModified :: Maybe UTCTime
    -- ^ The newest @modified@ across the records this attempt read.
    }
    deriving stock (Eq, Show)

emptyOsvAttempt :: OsvAttempt
emptyOsvAttempt = OsvAttempt Nothing Nothing

-- The mutable drop tally for one ingest pass. Opaque: read it with 'readIngestStats'.
newtype IngestCounter = IngestCounter {counterRef :: IORef IngestStats}

-- | The context one ingest pass threads through the stream.
data OsvIngest = OsvIngest
    { ingestLimits :: IngestLimits
    , ingestCounter :: IngestCounter
    , ingestEcosystem :: OsvEcosystem
    {- ^ The feed this pass compiles: it carries the grammar that orders the pass's bounds and
    the fan-out an ordinary advisory of the feed stays under.
    -}
    , ingestEpss :: EpssScores
    -- ^ The pass's EPSS table, joined onto each advisory as it is extracted.
    , ingestNow :: UTCTime
    -- ^ The run's clock, which every record's @modified@ is judged against.
    , ingestAttempt :: IORef OsvAttempt
    }

-- | A fresh ingest context with the given bounds, feed, EPSS table and clock, and a zeroed tally.
newOsvIngest :: (MonadIO m) => IngestLimits -> OsvEcosystem -> EpssScores -> UTCTime -> m OsvIngest
newOsvIngest limits eco scores now = do
    counter <- IngestCounter <$> newIORef emptyIngestStats
    attempt <- newIORef emptyOsvAttempt
    pure (OsvIngest limits counter eco scores now attempt)

-- | Read the current drop tally.
readIngestStats :: (MonadIO m) => OsvIngest -> m IngestStats
readIngestStats ingest = readIORef (counterRef (ingestCounter ingest))

{- | Zero the tally. The compiler re-streams from a clean slate on each retry attempt
and zeroes the tally alongside it, so the tally reflects only the final attempt.
-}
resetIngestStats :: (MonadIO m) => OsvIngest -> m ()
resetIngestStats ingest = writeIORef (counterRef (ingestCounter ingest)) emptyIngestStats

-- | Read what the current attempt learned about its source.
readOsvAttempt :: (MonadIO m) => OsvIngest -> m OsvAttempt
readOsvAttempt ingest = readIORef (ingestAttempt ingest)

{- | Forget the source metadata, alongside 'resetIngestStats'. A retry re-reads the export, so
last attempt's response header and record dates must not survive into this one's artifact.
-}
resetOsvAttempt :: (MonadIO m) => OsvIngest -> m ()
resetOsvAttempt ingest = writeIORef (ingestAttempt ingest) emptyOsvAttempt

-- | Whether the drop tally requires Pilot to refuse publication.
systemicDrop :: IngestStats -> Bool
systemicDrop s =
    dropped >= systemicDropFloor && dropped * 100 >= total * systemicDropPercent
  where
    dropped = statDroppedOversize s + statDroppedMalformed s
    total = dropped + statAccepted s

systemicDropFloor :: Int
systemicDropFloor = 16

systemicDropPercent :: Int
systemicDropPercent = 10

{- | Raised when systemic drops or zero relevant output prevent publication.
The tally records the rejected pass, without replacing a consumer's last-good artifact.
-}
newtype PilotIngestAborted = PilotIngestAborted IngestStats
    deriving stock (Show)

instance Exception PilotIngestAborted

-- | Fetch the OSV zip and stream its contents, bounded by @ingest@.
streamOsvUrl :: (MonadResource m, MonadThrow m, KatipContext m) => Maybe TracerProvider -> OsvIngest -> String -> ConduitT i ExtractedOsv m ()
streamOsvUrl mTracerProvider ingest urlStr = do
    lift $ logFM InfoS (ls ("Initializing OSV stream from " <> authorityLabel (toText urlStr)))
    bracketP
        (openOptionalSpan mTracerProvider Internal "ecluse.pilot.osv.stream")
        closeOptionalSpan
        ( \mSpan -> do
            forM_ mSpan $ \sp -> addAttribute sp "ecluse.osv.source_host" (authorityLabel (toText urlStr))
            -- Reject non-2xx responses before unzip so the retry policy sees HTTP failures.
            req <- liftIO $ setRequestCheckStatus <$> parseRequest urlStr
            httpSource req $ \res -> do
                recordResponseDate ingest (getResponseHeader hLastModified res)
                getResponseBody res .| parseOsvStream mTracerProvider ingest
        )

-- | Parse the zip stream and emit ExtractedOsv, bounded by @ingest@.
parseOsvStream :: (MonadResource m, MonadThrow m, KatipContext m) => Maybe TracerProvider -> OsvIngest -> ConduitT ByteString ExtractedOsv m ()
parseOsvStream mTracerProvider ingest = do
    lift $ logFM InfoS (ls ("Starting OSV zip extraction and parsing pipeline" :: String))
    bracketP
        (openOptionalSpan mTracerProvider Internal "ecluse.pilot.osv.parse")
        closeOptionalSpan
        (\_ -> void (transPipe liftIO unZipStream) .| processZipEntries ingest)

processZipEntries :: (MonadThrow m, KatipContext m) => OsvIngest -> ConduitT (Either ZipEntry ByteString) ExtractedOsv m ()
processZipEntries ingest =
    await >>= \case
        Nothing -> lift $ logFM InfoS (ls ("OSV stream fully processed" :: String))
        Just (Left entry) -> do
            outcome <- collectFile (ilMaxAdvisoryBytes (ingestLimits ingest))
            handleEntry ingest entry outcome
            processZipEntries ingest
        Just (Right _) -> processZipEntries ingest

-- The outcome of accumulating one zip entry: its bytes, or a signal that it breached
-- the byte cap. The signal carries the entry's full decompressed size, for the log.
data EntryOutcome = EntryBytes !ByteString | EntryOversize !Int

-- Decide what one collected entry yields: a counted drop for an over-large or malformed
-- entry, or the decoded advisory's rows.
handleEntry :: (KatipContext m) => OsvIngest -> ZipEntry -> EntryOutcome -> ConduitT (Either ZipEntry ByteString) ExtractedOsv m ()
handleEntry ingest entry = \case
    EntryOversize seen -> lift $ do
        bumpOversize (ingestCounter ingest)
        logFM WarningS (ls ("Dropping oversized OSV entry " <> zipEntryNameText entry <> ": " <> show seen <> " bytes exceeds the " <> show cap <> "-byte per-advisory cap"))
    EntryBytes fileBytes -> case decodeStrict fileBytes :: Maybe OsvAdvisory of
        Nothing -> lift $ do
            bumpMalformed (ingestCounter ingest)
            logFM WarningS (ls ("Failed to parse OSV advisory JSON from entry: " <> zipEntryNameText entry))
        Just adv -> admitAdvisory ingest adv
  where
    cap = ilMaxAdvisoryBytes (ingestLimits ingest)

-- Emit every row one decoded advisory yields. Nothing here filters: a row whose bound the
-- grammar cannot order is counted and logged, and still reaches the artifact.
admitAdvisory :: (KatipContext m) => OsvIngest -> OsvAdvisory -> ConduitT (Either ZipEntry ByteString) ExtractedOsv m ()
admitAdvisory ingest adv = do
    lift $ bumpAccepted (ingestCounter ingest)
    lift $ recordModified ingest adv
    lift $ warnOnFanOut ingest adv extracted
    lift $ forM_ (nonEmpty unorderable) (warnOnUnorderable ingest adv)
    yieldMany extracted
  where
    extracted = extractFromAdvisory (ingestEpss ingest) adv
    -- A name this build does not serve has no grammar to judge its bounds by, so nothing
    -- about it is anomalous.
    unorderable = maybe [] (\eco -> mapMaybe (unorderableExample eco) extracted) (osvEcosystemTag (ingestEcosystem ingest))

-- The package and the first unorderable bound of one row, for the log line below.
unorderableExample :: Ecosystem -> ExtractedOsv -> Maybe (Text, Text)
unorderableExample eco row
    | orderableBounds eco row = Nothing
    | otherwise = (,) (extPackage row) <$> listToMaybe (unorderableBounds eco row)

-- One line per advisory carrying one example, so a feed naming thousands of packages cannot
-- flood the log. The rows are kept, so this is an alarm and never a refusal.
warnOnUnorderable :: (KatipContext m) => OsvIngest -> OsvAdvisory -> NonEmpty (Text, Text) -> m ()
warnOnUnorderable ingest adv unorderable@((pkg, bound) :| _) = do
    bumpUnorderable (ingestCounter ingest) (length unorderable)
    logFM WarningS (ls ("OSV advisory " <> osvId adv <> " carries " <> show (length unorderable) <> " range(s) the version grammar cannot order, for example " <> pkg <> " " <> bound <> "; keeping them"))

warnOnFanOut :: (KatipContext m) => OsvIngest -> OsvAdvisory -> [ExtractedOsv] -> m ()
warnOnFanOut ingest adv extracted =
    when (n > limit) $
        logFM WarningS (ls ("OSV advisory " <> osvId adv <> " expanded into " <> show n <> " ranges, exceeding the sanity threshold of " <> show limit <> "; ingesting it regardless"))
  where
    n = length extracted
    limit = osvMaxAdvisoryFanOut (ingestEcosystem ingest)

-- The export's own @Last-Modified@, from the response that carried the rows. An absent or
-- unreadable header records nothing.
recordResponseDate :: (MonadIO m) => OsvIngest -> [ByteString] -> m ()
recordResponseDate ingest headers =
    modifyIORef' (ingestAttempt ingest) $ \attempt ->
        attempt{oaLastModified = parseHttpDate . decodeUtf8 =<< listToMaybe headers}

-- A source cannot know a change that has not happened, so a record dated ahead of the run's
-- clock is counted and its date dropped, never clamped. Its rows are kept either way.
recordModified :: (MonadIO m) => OsvIngest -> OsvAdvisory -> m ()
recordModified ingest adv = for_ (osvModified adv) $ \stamp ->
    if stamp > ingestNow ingest
        then bumpFutureModified (ingestCounter ingest)
        else modifyIORef' (ingestAttempt ingest) $ \attempt ->
            attempt{oaNewestModified = max (Just stamp) (oaNewestModified attempt)}

bumpAccepted :: (MonadIO m) => IngestCounter -> m ()
bumpAccepted (IngestCounter ref) = modifyIORef' ref (\s -> s{statAccepted = statAccepted s + 1})

bumpOversize :: (MonadIO m) => IngestCounter -> m ()
bumpOversize (IngestCounter ref) = modifyIORef' ref (\s -> s{statDroppedOversize = statDroppedOversize s + 1})

bumpMalformed :: (MonadIO m) => IngestCounter -> m ()
bumpMalformed (IngestCounter ref) = modifyIORef' ref (\s -> s{statDroppedMalformed = statDroppedMalformed s + 1})

bumpUnorderable :: (MonadIO m) => IngestCounter -> Int -> m ()
bumpUnorderable (IngestCounter ref) n = modifyIORef' ref (\s -> s{statUnorderable = statUnorderable s + n})

bumpFutureModified :: (MonadIO m) => IngestCounter -> m ()
bumpFutureModified (IngestCounter ref) = modifyIORef' ref (\s -> s{statFutureModified = statFutureModified s + 1})

zipEntryNameText :: ZipEntry -> Text
zipEntryNameText entry = case zipEntryName entry of
    Left txt -> txt
    Right bs -> decodeUtf8With lenientDecode bs

-- Checks @cap@ before each chunk, so memory never exceeds the cap plus one chunk. It is also the
-- only depth guard: 'decodeStrict' materialises the whole value before any post-decode check.
collectFile :: (Monad m) => Int -> ConduitT (Either ZipEntry ByteString) o m EntryOutcome
collectFile cap = go 0 []
  where
    go !seen acc =
        await >>= \case
            Nothing -> pure (EntryBytes (BS.concat (reverse acc)))
            Just (Left entry) -> do
                leftover (Left entry)
                pure (EntryBytes (BS.concat (reverse acc)))
            Just (Right bs) ->
                let seen' = seen + BS.length bs
                 in if seen' > cap
                        then drainOversize seen'
                        else go seen' (bs : acc)
    -- Not carrying acc forward frees the accumulated prefix, so the drain to the next
    -- entry boundary retains only the running size.
    drainOversize !seen =
        await >>= \case
            Nothing -> pure (EntryOversize seen)
            Just (Left entry) -> do
                leftover (Left entry)
                pure (EntryOversize seen)
            Just (Right bs) -> drainOversize (seen + BS.length bs)
