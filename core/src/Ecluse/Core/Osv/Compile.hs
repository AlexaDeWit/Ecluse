-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Compile OSV advisories and EPSS scores into the artifact consumed by CVE sync. Every pass
attempts the EPSS feed, and the ecosystem's 'EpssRequirement' decides whether a failed feed stops
publication or leaves the artifact recording unavailable enrichment.

A pass compiles into an exclusive temporary file, the candidate, which replaces the canonical file
only once its caller promotes it, so a refused publication leaves the previous artifact in place.
-}
module Ecluse.Core.Osv.Compile (
    CompileSources,
    mkCompileSources,
    csOsvExportUrl,
    csEpssFeedUrl,
    csOsvRecorded,
    csEpssRecorded,

    -- * The candidate a publication decides on
    OsvCandidate,
    candidatePath,
    candidateDestination,
    candidateFacts,
    withOsvCandidate,
    stampCandidateNonce,
    promoteCandidate,

    -- * Feed downloads
    newFeedManager,

    -- * Refusals and rows
    PilotEpssRequired (..),
    PilotOsvFetchFailed (..),
    osvToRow,
) where

import Conduit
import Control.Monad.Catch (MonadMask)
import Data.Conduit.List qualified as CL
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Database.SQLite.Simple
import Katip (KatipContext, Severity (..), SimpleLogPayload, katipAddContext, logFM, ls, sl)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import System.Directory (createDirectoryIfMissing, removeFile, renameFile)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.IO.Error (catchIOError)
import UnliftIO.Exception (bracket, throwIO, try)

import Ecluse.Core.BuildIdentity (productVersion)
import Ecluse.Core.Fault (HttpFault, renderHttpFault)
import Ecluse.Core.Fault.Http (httpFault, typedBodyReads)
import Ecluse.Core.Osv.Advisory (ExtractedOsv (..))
import Ecluse.Core.Osv.Ecosystem (OsvEcosystem (osvExportDirectory, osvWireName))
import Ecluse.Core.Osv.Epss (
    EpssEnrichment (EpssEnriched, EpssUnavailable),
    EpssFeed (efDigest, efLastModified, efModelVersion, efScoreDate, efScores),
    EpssFeedFailure,
    acquireEpssFeed,
    enrichedFeed,
    enrichmentStatus,
    maxEpssFeedBytes,
    mkEpssScores,
    renderEpssFeedFailure,
    resolveEnrichment,
 )
import Ecluse.Core.Osv.Provenance (
    AdvisoryProvenance (..),
    QuietTime,
    RecordedSource,
    SourceAge,
    SourcePathRecording,
    provenanceRows,
    recordSource,
    renderRecordedSource,
    renderSourceAge,
    sourceAges,
    sourceQuiet,
 )
import Ecluse.Core.Osv.Publication (
    CandidateFacts (..),
    PublicationNonce,
    SourceMarker (..),
    newPublicationNonce,
    renderPublicationNonce,
    sourceDigestHex,
    sourceDigestOf,
    sourceIdentityOf,
    usableLastModified,
 )
import Ecluse.Core.Osv.Retry (defaultOsvRetryPolicy, withOsvRetry)
import Ecluse.Core.Osv.Schema (
    EpssRequirement (EpssOptional, EpssRequired),
    EpssStatus (EnrichmentAvailable, EnrichmentUnavailable),
    MetaKey (..),
    metaTableDdl,
    osvDbFileName,
    osvSchemaEpoch,
    rangesTableDdl,
    renderEpssStatus,
    renderMetaKey,
 )
import Ecluse.Core.Osv.Stream (
    IngestStats (..),
    OsvAttempt (..),
    OsvIngest,
    PilotIngestAborted (..),
    defaultIngestLimits,
    newOsvIngest,
    readIngestStats,
    readOsvAttempt,
    resetIngestStats,
    resetOsvAttempt,
    streamOsvUrl,
    systemicDrop,
 )
import Ecluse.Core.Osv.Types (UpperBound (FixedBefore, LastAffected, Unbounded))
import Ecluse.Core.Security.Authority (credentialFreeUrl, dialledAuthorityLabel)
import Ecluse.Core.Snapshot (ContentDigest)
import Ecluse.Core.Telemetry.Metrics (
    AdvisoryCompileResult (CompileAborted, CompileCompleted),
    AdvisoryDropCause (DropMalformed, DropOversize),
 )
import Ecluse.Core.Telemetry.Record (AdvisoryCompileMetricsPort (acmpCompileAccepted, acmpCompileDropped, acmpCompileRun))
import Ecluse.Core.Telemetry.Span (withOptionalSpan)
import OpenTelemetry.Trace.Core (Span, SpanKind (Internal), SpanStatus (Error), TracerProvider, addAttribute, setStatus)

-- | The two upstreams one compile pass reads: the ecosystem's advisories, and the scores it joins onto them.
data CompileSources = CompileSources String String RecordedSource RecordedSource
    deriving stock (Eq, Show)

-- | The sources one pass reads. Each recorded form derives from its fetched URL, under the one choice given.
mkCompileSources :: SourcePathRecording -> String -> String -> CompileSources
mkCompileSources recording osvUrl epssUrl =
    CompileSources osvUrl epssUrl (recordSource recording (toText osvUrl)) (recordSource recording (toText epssUrl))

-- | The ecosystem's OSV export archive ('Ecluse.Core.Osv.Advisory.osvExportUrl').
csOsvExportUrl :: CompileSources -> String
csOsvExportUrl (CompileSources url _ _ _) = url

-- | The EPSS daily feed, from the configured @advisories.epssFeedUrl@.
csEpssFeedUrl :: CompileSources -> String
csEpssFeedUrl (CompileSources _ url _ _) = url

-- | The export as its artifact records it.
csOsvRecorded :: CompileSources -> RecordedSource
csOsvRecorded (CompileSources _ _ recorded _) = recorded

-- | The feed as its artifact records it.
csEpssRecorded :: CompileSources -> RecordedSource
csEpssRecorded (CompileSources _ _ _ recorded) = recorded

-- | A compiled artifact in its exclusive temporary file, with the facts its publication compares.
data OsvCandidate = OsvCandidate
    { candidatePath :: FilePath
    , candidateDestination :: FilePath
    , candidateFacts :: CandidateFacts
    }

-- | Compile one ecosystem into a candidate for @use@, which may promote it. A refused compile throws before @use@ runs.
withOsvCandidate :: (MonadResource m, MonadMask m, MonadUnliftIO m, KatipContext m) => AdvisoryCompileMetricsPort -> Maybe TracerProvider -> Manager -> FilePath -> OsvEcosystem -> EpssRequirement -> CompileSources -> QuietTime -> (OsvCandidate -> m a) -> m a
withOsvCandidate metrics mTracerProvider manager outDir eco requirement sources quietTime use = do
    let dbFile = outDir </> osvDbFileName (osvWireName eco)
    logFM InfoS (ls ("Compiling OSV data for " <> osvWireName eco <> " to " <> toText dbFile <> ", EPSS enrichment " <> renderRequirement requirement))

    liftIO $ createDirectoryIfMissing True outDir

    bracket (liftIO $ newCandidate outDir) (liftIO . removeCandidate) $ \candidate -> do
        facts <- compileCandidate run candidate
        use OsvCandidate{candidatePath = candidate, candidateDestination = dbFile, candidateFacts = facts}
  where
    run =
        CompileRun
            { crMetrics = metrics
            , crTracerProvider = mTracerProvider
            , crManager = manager
            , crEcosystem = eco
            , crEpss = requirement
            , crSources = sources
            , crQuietTime = quietTime
            }

-- | The manager a pass downloads its feeds through, under 'typedBodyReads' so a reset mid-body reaches the retry. It honours the HTTP proxy variables.
newFeedManager :: IO Manager
newFeedManager = newTlsManagerWith (typedBodyReads tlsManagerSettings)

-- | Write this attempt's nonce into the candidate. The nonce is fixed width, so the file keeps its size.
stampCandidateNonce :: (MonadIO m) => OsvCandidate -> PublicationNonce -> m ()
stampCandidateNonce candidate nonce =
    liftIO $ withConnection (candidatePath candidate) $ \conn ->
        execute conn "UPDATE meta SET value = ? WHERE key = ?" (renderPublicationNonce nonce, renderMetaKey MetaPublicationNonce)

-- | Replace the canonical artifact with the candidate, and return its path.
promoteCandidate :: (MonadIO m) => OsvCandidate -> m FilePath
promoteCandidate candidate = do
    liftIO (renameFile (candidatePath candidate) (candidateDestination candidate))
    pure (candidateDestination candidate)

-- What stays fixed across one pass, so each step below takes one parameter rather than six.
data CompileRun = CompileRun
    { crMetrics :: AdvisoryCompileMetricsPort
    , crTracerProvider :: Maybe TracerProvider
    , crManager :: Manager
    , crEcosystem :: OsvEcosystem
    , crEpss :: EpssRequirement
    , crSources :: CompileSources
    , crQuietTime :: QuietTime
    }

-- | A required EPSS feed failed, so the compile published nothing. It names the feed by @host:port@, as the URL can carry a credential.
data PilotEpssRequired = PilotEpssRequired
    { perEcosystem :: Text
    , perFeed :: Text
    -- ^ The feed's @host:port@.
    , perFailure :: EpssFeedFailure
    }
    deriving stock (Eq, Show)

instance Exception PilotEpssRequired where
    displayException = toString . renderEpssRequired

renderEpssRequired :: PilotEpssRequired -> Text
renderEpssRequired refusal =
    perEcosystem refusal
        <> " requires EPSS enrichment, and the feed failed: "
        <> renderEpssFeedFailure (perFeed refusal) (perFailure refusal)

-- | The OSV export fetch failed, after any retries. It names the feed by @host:port@, as a redirect can reach a URL that carries a credential.
data PilotOsvFetchFailed = PilotOsvFetchFailed
    { pofFeed :: Text
    -- ^ The export's @host:port@.
    , pofFault :: HttpFault
    }
    deriving stock (Eq, Show)

instance Exception PilotOsvFetchFailed where
    displayException failed = toString ("OSV export fetch failed at " <> renderHttpFault (pofFeed failed) (pofFault failed))

-- Fill one candidate file and return what its publication compares.
compileCandidate :: (MonadResource m, MonadMask m, MonadUnliftIO m, KatipContext m) => CompileRun -> FilePath -> m CandidateFacts
compileCandidate run dbFile =
    withOptionalSpan (crTracerProvider run) Internal "ecluse.pilot.osv.compile" $ \mSpan -> do
        forM_ mSpan (describeCompile run)

        -- Every record's date is judged against this one instant, so a long pass
        -- cannot let a later record pass a check an earlier one failed.
        now <- liftIO getCurrentTime

        -- The join needs the whole score table before the first advisory row lands.
        enrichment <- enrichOrRefuse run mSpan
        epssMarker <- traverse (\feed -> answeredMarker (csEpssFeedUrl sources) (efLastModified feed) (efDigest feed)) (enrichedFeed enrichment)
        ingest <- newOsvIngest defaultIngestLimits (crEcosystem run) (maybe (mkEpssScores []) efScores (enrichedFeed enrichment)) now

        bracket (liftIO $ open dbFile) (liftIO . close) $ \conn -> do
            liftIO $ initSchema conn
            osvDigest <- ingestAdvisories run ingest conn
            stats <- readIngestStats ingest
            attempt <- readOsvAttempt ingest
            osvMarker <- answeredMarker (csOsvExportUrl sources) (oaLastModified attempt) osvDigest
            concludeCompile (crMetrics run) mSpan conn (conclusionOf run now enrichment attempt stats osvMarker epssMarker)
  where
    sources = crSources run

-- A retried fetch can answer minutes after the pass began, so a source's date is bounded by a clock read once it answered.
answeredMarker :: (MonadIO m) => String -> Maybe UTCTime -> ContentDigest -> m SourceMarker
answeredMarker url declared digest = do
    answeredAt <- liftIO getCurrentTime
    pure (SourceMarker (sourceIdentityOf (credentialFreeUrl (toText url))) (usableLastModified answeredAt declared) (sourceDigestOf digest))

describeCompile :: (MonadIO m) => CompileRun -> Span -> m ()
describeCompile run sp = do
    addAttribute sp "ecluse.osv.ecosystem" (osvWireName (crEcosystem run))
    addAttribute sp "ecluse.osv.source_host" (osvFeedLabel run)

-- The attempt runs whatever the requirement, so an ecosystem that could publish without scores
-- still carries them whenever the feed is up.
enrichOrRefuse :: (MonadResource m, MonadMask m, MonadUnliftIO m, KatipContext m) => CompileRun -> Maybe Span -> m EpssEnrichment
enrichOrRefuse run mSpan = do
    acquired <- acquireEpssFeed (crManager run) maxEpssFeedBytes (csEpssFeedUrl (crSources run))
    forM_ mSpan $ \sp -> addAttribute sp "ecluse.osv.epss_status" (renderEpssStatus (either (const EnrichmentUnavailable) (const EnrichmentAvailable) acquired))
    enrichment <- either (refuseRequiredEpss run mSpan) pure (resolveEnrichment (crEpss run) acquired)
    case enrichment of
        EpssUnavailable failure ->
            logFM WarningS (ls ("EPSS enrichment unavailable for " <> ecosystem <> ": " <> renderEpssFeedFailure (epssFeedLabel run) failure <> ". No " <> ecosystem <> " rule depends on EPSS, so the artifact publishes without scores"))
        EpssEnriched _ -> pass
    pure enrichment
  where
    ecosystem = osvWireName (crEcosystem run)

-- The throw leaves the candidate unrenamed, so nothing publishes and any previous artifact stays.
refuseRequiredEpss :: (KatipContext m) => CompileRun -> Maybe Span -> EpssFeedFailure -> m a
refuseRequiredEpss run mSpan failure = do
    forM_ mSpan $ \sp -> setStatus sp (Error "required EPSS enrichment unavailable, compile abandoned")
    logFM ErrorS (ls ("Aborting OSV compile: " <> renderEpssRequired refusal))
    -- A fault for the caller: the scheduled loop retries on its cadence and a one-shot run exits non-zero.
    throwIO refusal
  where
    refusal =
        PilotEpssRequired
            { perEcosystem = osvWireName (crEcosystem run)
            , perFeed = epssFeedLabel run
            , perFailure = failure
            }

epssFeedLabel :: CompileRun -> Text
epssFeedLabel run = dialledAuthorityLabel (toText (csEpssFeedUrl (crSources run)))

osvFeedLabel :: CompileRun -> Text
osvFeedLabel run = dialledAuthorityLabel (toText (csOsvExportUrl (crSources run)))

renderRequirement :: EpssRequirement -> Text
renderRequirement = \case
    EpssRequired -> "required"
    EpssOptional -> "optional"

-- The client's exception holds the request a redirect reached. The typed fault is thrown outside any
-- handler, so nothing can annotate it with that exception.
ingestAdvisories :: (MonadResource m, MonadMask m, MonadUnliftIO m, KatipContext m) => CompileRun -> OsvIngest -> Connection -> m ContentDigest
ingestAdvisories run ingest conn =
    try (withOsvRetry feed defaultOsvRetryPolicy (restreamAdvisories run ingest conn))
        >>= either (throwIO . PilotOsvFetchFailed feed . httpFault) pure
  where
    feed = osvFeedLabel run

-- A failed attempt leaves committed batches. NULL bounds defeat deduplication, so each attempt
-- clears the table, the tally, and the source metadata before it re-streams.
restreamAdvisories :: (MonadResource m, MonadThrow m, KatipContext m) => CompileRun -> OsvIngest -> Connection -> m ContentDigest
restreamAdvisories run ingest conn = do
    resetIngestStats ingest
    resetOsvAttempt ingest
    liftIO $ execute_ conn "DELETE FROM package_vulnerability_ranges"
    runConduit $
        fuseUpstream
            (streamOsvUrl (crTracerProvider run) (crManager run) ingest (csOsvExportUrl (crSources run)))
            ( CL.filter ((== osvExportDirectory (crEcosystem run)) . extEcosystem)
                .| CL.chunksOf 2000
                .| sinkSqlite conn
            )

data CompileConclusion = CompileConclusion
    { ccEcosystem :: Text
    , ccSources :: CompileSources
    , ccStats :: IngestStats
    , ccEpssStatus :: EpssStatus
    , ccProvenance :: AdvisoryProvenance
    , ccOsvMarker :: SourceMarker
    , ccEpssMarker :: Maybe SourceMarker
    , ccQuietTime :: QuietTime
    , ccNow :: UTCTime
    }

conclusionOf :: CompileRun -> UTCTime -> EpssEnrichment -> OsvAttempt -> IngestStats -> SourceMarker -> Maybe SourceMarker -> CompileConclusion
conclusionOf run now enrichment attempt stats osvMarker epssMarker =
    CompileConclusion
        { ccEcosystem = osvWireName (crEcosystem run)
        , ccSources = sources
        , ccStats = stats
        , ccEpssStatus = enrichmentStatus enrichment
        , ccProvenance = passProvenance sources enrichment attempt
        , ccOsvMarker = osvMarker
        , ccEpssMarker = epssMarker
        , ccQuietTime = crQuietTime run
        , ccNow = now
        }
  where
    sources = crSources run

newCandidate :: FilePath -> IO FilePath
newCandidate outDir = do
    (path, created) <- openTempFile outDir ".osv-candidate.db"
    hClose created
    pure path

removeCandidate :: FilePath -> IO ()
removeCandidate path = catchIOError (removeFile path) (const $ pure ())

-- Each source in the form 'CompileSources' recorded, because the artifact reaches every consumer.
-- A feed that never arrived described nothing, so it records no EPSS source or date.
passProvenance :: CompileSources -> EpssEnrichment -> OsvAttempt -> AdvisoryProvenance
passProvenance sources enrichment attempt =
    AdvisoryProvenance
        { apOsvSource = Just (renderRecordedSource (csOsvRecorded sources))
        , apOsvLastModified = oaLastModified attempt
        , apOsvNewestModified = oaNewestModified attempt
        , apEpssSource = renderRecordedSource (csEpssRecorded sources) <$ feed
        , apEpssLastModified = efLastModified =<< feed
        , apEpssScoreDate = efScoreDate =<< feed
        , apEpssModelVersion = efModelVersion =<< feed
        }
  where
    feed = enrichedFeed enrichment

concludeCompile :: (KatipContext m) => AdvisoryCompileMetricsPort -> Maybe Span -> Connection -> CompileConclusion -> m CandidateFacts
concludeCompile metrics mSpan conn conclusion = do
    recordCompileSpan mSpan stats
    liftIO (recordTallies metrics stats)
    rowCount <- liftIO (countRows conn "SELECT COUNT(*) FROM package_vulnerability_ranges")
    forM_ (compileRefusal stats rowCount) (refuseCompile metrics mSpan ecosystem stats)

    scoredCount <- liftIO (countRows conn "SELECT COUNT(*) FROM package_vulnerability_ranges WHERE epss_score IS NOT NULL")
    liftIO $ writeMeta conn conclusion rowCount scoredCount
    liftIO (acmpCompileRun metrics CompileCompleted)
    forM_ mSpan $ \sp -> addAttribute sp "ecluse.osv.row_count" (show rowCount :: Text)
    katipAddContext (sl "row_count" rowCount <> sl "epss_status" epssStatus <> dropFields ecosystem stats) $
        logFM InfoS (ls ("Compiled " <> show rowCount <> " advisory ranges for " <> ecosystem <> " (" <> renderDrops stats <> "), epss_status=" <> epssStatus))
    warnOnUnusableDates ecosystem stats
    logSourceAges ecosystem (sourceAges (ccNow conclusion) (ccQuietTime conclusion) (ccProvenance conclusion))
    pure
        CandidateFacts
            { cfEcosystem = ecosystem
            , cfOsv = ccOsvMarker conclusion
            , cfEpss = ccEpssMarker conclusion
            , cfRelevantRows = toInteger rowCount
            , cfScoredRows = toInteger scoredCount
            }
  where
    ecosystem = ccEcosystem conclusion
    stats = ccStats conclusion
    epssStatus = renderEpssStatus (ccEpssStatus conclusion)

countRows :: Connection -> Query -> IO Int
countRows conn statement = maybe 0 fromOnly . listToMaybe <$> (query_ conn statement :: IO [Only Int])

recordCompileSpan :: (MonadIO m) => Maybe Span -> IngestStats -> m ()
recordCompileSpan mSpan stats = forM_ mSpan $ \sp -> do
    addAttribute sp "ecluse.osv.accepted" (show (statAccepted stats) :: Text)
    addAttribute sp "ecluse.osv.dropped_oversize" (show (statDroppedOversize stats) :: Text)
    addAttribute sp "ecluse.osv.dropped_malformed" (show (statDroppedMalformed stats) :: Text)
    addAttribute sp "ecluse.osv.unorderable" (show (statUnorderable stats) :: Text)

-- The refusal throws, so nothing after it in 'concludeCompile' runs: no metadata is written and
-- the candidate file is discarded unrenamed.
refuseCompile :: (KatipContext m) => AdvisoryCompileMetricsPort -> Maybe Span -> Text -> IngestStats -> Text -> m ()
refuseCompile metrics mSpan ecosystem stats reason = do
    forM_ mSpan $ \sp -> setStatus sp (Error (reason <> ", compile abandoned"))
    liftIO (acmpCompileRun metrics CompileAborted)
    katipAddContext (dropFields ecosystem stats) $
        logFM ErrorS (ls ("Aborting OSV compile for " <> ecosystem <> ": " <> reason <> " (" <> renderDrops stats <> ")"))
    throwIO (PilotIngestAborted stats)

compileRefusal :: IngestStats -> Int -> Maybe Text
compileRefusal stats rowCount
    | systemicDrop stats = Just "systemic advisory drop rate"
    | rowCount == 0 = Just "zero relevant advisory rows"
    | otherwise = Nothing

-- One line per pass, not per record: a source whose dates have gone wrong writes many, and
-- the rows are kept regardless.
warnOnUnusableDates :: (KatipContext m) => Text -> IngestStats -> m ()
warnOnUnusableDates ecosystem stats =
    when (unusable > 0) $
        logFM WarningS (ls ("Ignoring the modified date of " <> show unusable <> " " <> ecosystem <> " advisory record(s), unreadable or dated after this run's clock; their ranges are kept"))
  where
    unusable = statUnusableModified stats

-- The ages the sources declared, on every pass that published. A source past its threshold is
-- an operator alarm: raise the threshold for a slow ecosystem, or change the source.
logSourceAges :: (KatipContext m) => Text -> [SourceAge] -> m ()
logSourceAges ecosystem ages = for_ ages $ \reading -> do
    logFM InfoS (ls (ecosystem <> ": " <> renderSourceAge reading))
    when (sourceQuiet reading) $
        logFM ErrorS (ls (ecosystem <> ": " <> renderSourceAge reading <> ", so the source has gone quiet"))

-- An abandoned pass records its tallies too, and a pass with no drops records a zero, so
-- the drop series exists before the first drop.
recordTallies :: AdvisoryCompileMetricsPort -> IngestStats -> IO ()
recordTallies metrics stats = do
    acmpCompileAccepted metrics (statAccepted stats)
    acmpCompileDropped metrics DropOversize (statDroppedOversize stats)
    acmpCompileDropped metrics DropMalformed (statDroppedMalformed stats)

renderDrops :: IngestStats -> Text
renderDrops s =
    "accepted "
        <> show (statAccepted s)
        <> ", dropped "
        <> show (statDroppedOversize s)
        <> " oversize / "
        <> show (statDroppedMalformed s)
        <> " malformed, kept "
        <> show (statUnorderable s)
        <> " unorderable"

dropFields :: Text -> IngestStats -> SimpleLogPayload
dropFields ecosystem s =
    sl "ecosystem" ecosystem
        <> sl "accepted" (statAccepted s)
        <> sl "dropped_oversize" (statDroppedOversize s)
        <> sl "dropped_malformed" (statDroppedMalformed s)
        <> sl "unorderable" (statUnorderable s)

initSchema :: Connection -> IO ()
initSchema conn = do
    execute_ conn (Query rangesTableDdl)
    -- A unique index rather than a composite PRIMARY KEY: @STRICT@ makes primary-key
    -- columns implicitly NOT NULL, and the three bound columns are legitimately NULL.
    execute_ conn "CREATE UNIQUE INDEX uq_ranges_segment ON package_vulnerability_ranges(package_name, cve_id, introduced_version, fixed_version, last_affected_version)"
    execute_ conn "CREATE INDEX idx_package_name ON package_vulnerability_ranges(package_name)"
    execute_ conn "CREATE INDEX idx_package_fixed ON package_vulnerability_ranges(package_name, fixed_version)"
    execute_ conn (Query metaTableDdl)
    execute_ conn (fromString ("PRAGMA user_version = " <> show osvSchemaEpoch))

-- Written once, after the stream completes and the refusals pass: the row count and the
-- source provenance are only meaningful for a complete artifact.
writeMeta :: Connection -> CompileConclusion -> Int -> Int -> IO ()
writeMeta conn conclusion rowCount scoredCount = do
    builtAt <- getCurrentTime
    nonce <- newPublicationNonce
    executeMany
        conn
        "INSERT INTO meta (key, value) VALUES (?, ?)"
        ( [ (renderMetaKey MetaPilotVersion, productVersion)
          , (renderMetaKey MetaEcosystem, ccEcosystem conclusion)
          , (renderMetaKey MetaBuiltAt, toText (iso8601Show builtAt))
          , (renderMetaKey MetaSourceUrl, dialledAuthorityLabel (toText (csOsvExportUrl sources)))
          , (renderMetaKey MetaEpssStatus, renderEpssStatus status)
          , (renderMetaKey MetaRowCount, show rowCount)
          , (renderMetaKey MetaScoredRowCount, show scoredCount)
          , (renderMetaKey MetaOsvDigest, sourceDigestHex (smDigest (ccOsvMarker conclusion)))
          , (renderMetaKey MetaPublicationNonce, renderPublicationNonce nonce)
          ]
            <> [(renderMetaKey MetaEpssSourceUrl, dialledAuthorityLabel (toText (csEpssFeedUrl sources))) | status == EnrichmentAvailable]
            <> [(renderMetaKey MetaEpssDigest, sourceDigestHex (smDigest marker)) | marker <- maybeToList (ccEpssMarker conclusion)]
            <> provenanceRows (ccProvenance conclusion)
        )
  where
    sources = ccSources conclusion
    status = ccEpssStatus conclusion

sinkSqlite :: (MonadIO m) => Connection -> ConduitT [ExtractedOsv] o m ()
sinkSqlite conn = awaitForever $ \batch ->
    liftIO $
        withTransaction conn $
            executeMany
                conn
                "INSERT OR IGNORE INTO package_vulnerability_ranges (package_name, cve_id, introduced_version, fixed_version, last_affected_version, severity, epss_score) VALUES (?, ?, ?, ?, ?, ?, ?)"
                (map osvToRow batch)

-- | One extracted segment as its artifact row. The upper bound fills at most one of @fixed_version@ and @last_affected_version@.
osvToRow :: ExtractedOsv -> (Text, Text, Maybe Text, Maybe Text, Maybe Text, Maybe Double, Maybe Double)
osvToRow osv = (extPackage osv, extCveId osv, extIntroduced osv, fixed, lastAffected, extSeverity osv, extEpss osv)
  where
    (fixed, lastAffected) = case extUpperBound osv of
        FixedBefore f -> (Just f, Nothing)
        LastAffected la -> (Nothing, Just la)
        Unbounded -> (Nothing, Nothing)
