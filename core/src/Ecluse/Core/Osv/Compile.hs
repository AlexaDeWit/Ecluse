-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Compile OSV advisories and EPSS scores into the artifact consumed by CVE sync. Every pass
attempts the EPSS feed, and the ecosystem's 'EpssRequirement' decides whether a failed feed stops
publication or leaves the artifact recording unavailable enrichment.
-}
module Ecluse.Core.Osv.Compile (
    CompileSources (..),
    compileOsvToSqlite,
    PilotEpssRequired (..),
    osvToRow,
) where

import Conduit
import Control.Monad.Catch (MonadMask)
import Data.Conduit.List qualified as CL
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Database.SQLite.Simple
import Katip (KatipContext, Severity (..), SimpleLogPayload, katipAddContext, logFM, ls, sl)
import System.Directory (createDirectoryIfMissing, removeFile, renameFile)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.IO.Error (catchIOError)
import UnliftIO.Exception (bracket, throwIO)

import Ecluse.Core.BuildIdentity (productVersion)
import Ecluse.Core.Osv.Advisory (ExtractedOsv (..))
import Ecluse.Core.Osv.Ecosystem (OsvEcosystem (osvExportDirectory, osvWireName))
import Ecluse.Core.Osv.Epss (
    EpssEnrichment (EpssEnriched, EpssUnavailable),
    EpssFeed (efLastModified, efModelVersion, efScoreDate, efScores),
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
    SourceAge,
    provenanceRows,
    renderSourceAge,
    sourceAges,
    sourceQuiet,
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
import Ecluse.Core.Telemetry.Metrics (
    AdvisoryCompileResult (CompileAborted, CompileCompleted),
    AdvisoryDropCause (DropMalformed, DropOversize),
 )
import Ecluse.Core.Telemetry.Record (AdvisoryCompileMetricsPort (acmpCompileAccepted, acmpCompileDropped, acmpCompileRun))
import Ecluse.Core.Telemetry.Span (withOptionalSpan)
import OpenTelemetry.Trace.Core (Span, SpanKind (Internal), SpanStatus (Error), TracerProvider, addAttribute, setStatus)

{- | The two upstreams one compile pass reads: the ecosystem's advisories, and the
exploitability scores it joins onto them.
-}
data CompileSources = CompileSources
    { csOsvExportUrl :: String
    -- ^ The ecosystem's OSV export archive ('Ecluse.Core.Osv.Advisory.osvExportUrl').
    , csEpssFeedUrl :: String
    -- ^ The EPSS daily feed, from the configured @advisories.epssFeedUrl@.
    }
    deriving stock (Eq, Show)

{- | Compile one ecosystem into @outDir@, refusing systemic drops, zero relevant rows, or a failed
EPSS feed the requirement makes fatal. A refusal leaves any previous artifact unchanged.
-}
compileOsvToSqlite :: (MonadResource m, MonadMask m, MonadUnliftIO m, KatipContext m) => AdvisoryCompileMetricsPort -> Maybe TracerProvider -> FilePath -> OsvEcosystem -> EpssRequirement -> CompileSources -> QuietTime -> m FilePath
compileOsvToSqlite metrics mTracerProvider outDir eco requirement sources quietTime = do
    let dbFile = outDir </> osvDbFileName (osvWireName eco)
    logFM InfoS (ls ("Compiling OSV data for " <> osvWireName eco <> " to " <> toText dbFile <> ", EPSS enrichment " <> renderRequirement requirement))

    liftIO $ createDirectoryIfMissing True outDir

    bracket (liftIO $ newCandidate outDir) (liftIO . removeCandidate) $ \candidate -> do
        compileCandidate run candidate
        liftIO $ renameFile candidate dbFile
    pure dbFile
  where
    run =
        CompileRun
            { crMetrics = metrics
            , crTracerProvider = mTracerProvider
            , crEcosystem = eco
            , crEpss = requirement
            , crSources = sources
            , crQuietTime = quietTime
            }

-- What stays fixed across one pass, so each step below takes one parameter rather than six.
data CompileRun = CompileRun
    { crMetrics :: AdvisoryCompileMetricsPort
    , crTracerProvider :: Maybe TracerProvider
    , crEcosystem :: OsvEcosystem
    , crEpss :: EpssRequirement
    , crSources :: CompileSources
    , crQuietTime :: QuietTime
    }

{- | A compile whose ecosystem requires EPSS enrichment met a failed feed, so it published nothing.
It names the feed by host and port alone, because the configured URL can carry a credential.
-}
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
        <> " requires EPSS enrichment, and the feed at "
        <> perFeed refusal
        <> " failed: "
        <> renderEpssFeedFailure (perFailure refusal)

-- Fill one candidate file, which the caller renames into place only once this returns.
compileCandidate :: (MonadResource m, MonadMask m, MonadUnliftIO m, KatipContext m) => CompileRun -> FilePath -> m ()
compileCandidate run dbFile =
    withOptionalSpan (crTracerProvider run) Internal "ecluse.pilot.osv.compile" $ \mSpan -> do
        forM_ mSpan (describeCompile run)

        -- Every record's date is judged against this one instant, so a long pass
        -- cannot let a later record pass a check an earlier one failed.
        now <- liftIO getCurrentTime

        -- The join needs the whole score table before the first advisory row lands.
        enrichment <- enrichOrRefuse run mSpan
        ingest <- newOsvIngest defaultIngestLimits (crEcosystem run) (maybe (mkEpssScores []) efScores (enrichedFeed enrichment)) now

        bracket (liftIO $ open dbFile) (liftIO . close) $ \conn -> do
            liftIO $ initSchema conn
            ingestAdvisories run ingest conn
            stats <- readIngestStats ingest
            attempt <- readOsvAttempt ingest
            concludeCompile (crMetrics run) mSpan conn (conclusionOf run now enrichment attempt stats)

describeCompile :: (MonadIO m) => CompileRun -> Span -> m ()
describeCompile run sp = do
    addAttribute sp "ecluse.osv.ecosystem" (osvWireName (crEcosystem run))
    addAttribute sp "ecluse.osv.source_host" (dialledAuthorityLabel (toText (csOsvExportUrl (crSources run))))

-- The attempt runs whatever the requirement, so an ecosystem that could publish without scores
-- still carries them whenever the feed is up.
enrichOrRefuse :: (MonadResource m, MonadMask m, MonadUnliftIO m, KatipContext m) => CompileRun -> Maybe Span -> m EpssEnrichment
enrichOrRefuse run mSpan = do
    acquired <- acquireEpssFeed maxEpssFeedBytes (csEpssFeedUrl (crSources run))
    forM_ mSpan $ \sp -> addAttribute sp "ecluse.osv.epss_status" (renderEpssStatus (either (const EnrichmentUnavailable) (const EnrichmentAvailable) acquired))
    enrichment <- either (refuseRequiredEpss run mSpan) pure (resolveEnrichment (crEpss run) acquired)
    case enrichment of
        EpssUnavailable failure ->
            logFM WarningS (ls ("EPSS enrichment unavailable for " <> ecosystem <> " from " <> epssFeedLabel run <> ": " <> renderEpssFeedFailure failure <> ". No " <> ecosystem <> " rule depends on EPSS, so the artifact publishes without scores"))
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

renderRequirement :: EpssRequirement -> Text
renderRequirement = \case
    EpssRequired -> "required"
    EpssOptional -> "optional"

-- A failed attempt leaves committed batches. NULL bounds defeat deduplication, so each retry
-- clears the table, the tally, and the source metadata before it re-streams.
ingestAdvisories :: (MonadResource m, MonadMask m, KatipContext m) => CompileRun -> OsvIngest -> Connection -> m ()
ingestAdvisories run ingest conn =
    withOsvRetry defaultOsvRetryPolicy $ do
        resetIngestStats ingest
        resetOsvAttempt ingest
        liftIO $ execute_ conn "DELETE FROM package_vulnerability_ranges"
        runConduit $
            streamOsvUrl (crTracerProvider run) ingest (csOsvExportUrl (crSources run))
                .| CL.filter ((== osvExportDirectory (crEcosystem run)) . extEcosystem)
                .| CL.chunksOf 2000
                .| sinkSqlite conn

data CompileConclusion = CompileConclusion
    { ccEcosystem :: Text
    , ccSources :: CompileSources
    , ccStats :: IngestStats
    , ccEpssStatus :: EpssStatus
    , ccProvenance :: AdvisoryProvenance
    , ccQuietTime :: QuietTime
    , ccNow :: UTCTime
    }

conclusionOf :: CompileRun -> UTCTime -> EpssEnrichment -> OsvAttempt -> IngestStats -> CompileConclusion
conclusionOf run now enrichment attempt stats =
    CompileConclusion
        { ccEcosystem = osvWireName (crEcosystem run)
        , ccSources = crSources run
        , ccStats = stats
        , ccEpssStatus = enrichmentStatus enrichment
        , ccProvenance = passProvenance (crSources run) enrichment attempt
        , ccQuietTime = crQuietTime run
        , ccNow = now
        }

newCandidate :: FilePath -> IO FilePath
newCandidate outDir = do
    (path, handle) <- openTempFile outDir ".osv-candidate.db"
    hClose handle
    pure path

removeCandidate :: FilePath -> IO ()
removeCandidate path = catchIOError (removeFile path) (const $ pure ())

-- The sources as they described themselves, credential-free because the artifact reaches every
-- consumer. A feed that never arrived described nothing, so it records no EPSS source or date.
passProvenance :: CompileSources -> EpssEnrichment -> OsvAttempt -> AdvisoryProvenance
passProvenance sources enrichment attempt =
    AdvisoryProvenance
        { apOsvSource = Just (credentialFreeUrl (toText (csOsvExportUrl sources)))
        , apOsvLastModified = oaLastModified attempt
        , apOsvNewestModified = oaNewestModified attempt
        , apEpssSource = credentialFreeUrl (toText (csEpssFeedUrl sources)) <$ feed
        , apEpssLastModified = efLastModified =<< feed
        , apEpssScoreDate = efScoreDate =<< feed
        , apEpssModelVersion = efModelVersion =<< feed
        }
  where
    feed = enrichedFeed enrichment

concludeCompile :: (KatipContext m) => AdvisoryCompileMetricsPort -> Maybe Span -> Connection -> CompileConclusion -> m ()
concludeCompile metrics mSpan conn conclusion = do
    recordCompileSpan mSpan stats
    liftIO (recordTallies metrics stats)
    counted <- liftIO (query_ conn "SELECT COUNT(*) FROM package_vulnerability_ranges" :: IO [Only Int])
    let rowCount = maybe 0 fromOnly (listToMaybe counted)
    forM_ (compileRefusal stats rowCount) (refuseCompile metrics mSpan ecosystem stats)

    liftIO $ writeMeta conn conclusion rowCount
    liftIO (acmpCompileRun metrics CompileCompleted)
    forM_ mSpan $ \sp -> addAttribute sp "ecluse.osv.row_count" (show rowCount :: Text)
    katipAddContext (sl "row_count" rowCount <> sl "epss_status" epssStatus <> dropFields ecosystem stats) $
        logFM InfoS (ls ("Compiled " <> show rowCount <> " advisory ranges for " <> ecosystem <> " (" <> renderDrops stats <> "), epss_status=" <> epssStatus))
    warnOnUnusableDates ecosystem stats
    logSourceAges ecosystem (sourceAges (ccNow conclusion) (ccQuietTime conclusion) (ccProvenance conclusion))
  where
    ecosystem = ccEcosystem conclusion
    stats = ccStats conclusion
    epssStatus = renderEpssStatus (ccEpssStatus conclusion)

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
writeMeta :: Connection -> CompileConclusion -> Int -> IO ()
writeMeta conn conclusion rowCount = do
    builtAt <- getCurrentTime
    executeMany
        conn
        "INSERT INTO meta (key, value) VALUES (?, ?)"
        ( [ (renderMetaKey MetaPilotVersion, productVersion)
          , (renderMetaKey MetaEcosystem, ccEcosystem conclusion)
          , (renderMetaKey MetaBuiltAt, toText (iso8601Show builtAt))
          , (renderMetaKey MetaSourceUrl, dialledAuthorityLabel (toText (csOsvExportUrl sources)))
          , (renderMetaKey MetaEpssStatus, renderEpssStatus status)
          , (renderMetaKey MetaRowCount, show rowCount)
          ]
            <> [(renderMetaKey MetaEpssSourceUrl, dialledAuthorityLabel (toText (csEpssFeedUrl sources))) | status == EnrichmentAvailable]
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

{- | One extracted segment as its artifact row. The upper bound spreads over the
@fixed_version@ and @last_affected_version@ columns, and fills at most one of them.
-}
osvToRow :: ExtractedOsv -> (Text, Text, Maybe Text, Maybe Text, Maybe Text, Maybe Double, Maybe Double)
osvToRow osv = (extPackage osv, extCveId osv, extIntroduced osv, fixed, lastAffected, extSeverity osv, extEpss osv)
  where
    (fixed, lastAffected) = case extUpperBound osv of
        FixedBefore f -> (Just f, Nothing)
        LastAffected la -> (Nothing, Just la)
        Unbounded -> (Nothing, Nothing)
