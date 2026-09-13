-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

{- | Compile OSV advisories and EPSS scores into the artifact
consumed by CVE sync.
-}
module Ecluse.Core.Osv.Compile (
    CompileSources (..),
    compileOsvToSqlite,
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
import Ecluse.Core.Osv.Epss (EpssFeed (efLastModified, efModelVersion, efScoreDate, efScores), fetchEpssScores, maxEpssFeedBytes)
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
import Ecluse.Core.Osv.Schema (MetaKey (..), metaTableDdl, osvDbFileName, osvSchemaEpoch, rangesTableDdl, renderMetaKey)
import Ecluse.Core.Osv.Stream (
    IngestStats (..),
    OsvAttempt (..),
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
import Ecluse.Core.Security.Authority (authorityLabel, credentialFreeUrl)
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

{- | Compile one ecosystem into @outDir@, refusing systemic drops or zero relevant rows. A
refused candidate leaves any previous artifact, its metadata, and its recorded ages unchanged.
-}
compileOsvToSqlite :: (MonadResource m, MonadMask m, MonadUnliftIO m, KatipContext m) => AdvisoryCompileMetricsPort -> Maybe TracerProvider -> FilePath -> OsvEcosystem -> CompileSources -> QuietTime -> m FilePath
compileOsvToSqlite metrics mTracerProvider outDir eco sources quietTime = do
    let ecosystem = osvWireName eco
        dbFile = outDir </> osvDbFileName ecosystem
    logFM InfoS (ls ("Compiling OSV data for " <> ecosystem <> " to " <> toText dbFile))

    liftIO $ createDirectoryIfMissing True outDir

    bracket (liftIO $ newCandidate outDir) (liftIO . removeCandidate) $ \candidate -> do
        compileCandidate candidate ecosystem
        liftIO $ renameFile candidate dbFile
    pure dbFile
  where
    compileCandidate dbFile ecosystem =
        withOptionalSpan mTracerProvider Internal "ecluse.pilot.osv.compile" $
            \mSpan -> do
                forM_ mSpan $ \sp -> do
                    addAttribute sp "ecluse.osv.ecosystem" ecosystem
                    addAttribute sp "ecluse.osv.source_host" (authorityLabel (toText (csOsvExportUrl sources)))

                -- Every record's date is judged against this one instant, so a long pass
                -- cannot let a later record pass a check an earlier one failed.
                now <- liftIO getCurrentTime

                -- The join needs the whole score table before the first advisory row lands, and a
                -- feed the retry budget cannot fetch fails the pass rather than shipping without.
                feed <- withOsvRetry defaultOsvRetryPolicy (fetchEpssScores maxEpssFeedBytes (csEpssFeedUrl sources))
                ingest <- newOsvIngest defaultIngestLimits eco (efScores feed) now

                bracket (liftIO $ open dbFile) (liftIO . close) $ \conn -> do
                    liftIO $ initSchema conn

                    -- A failed attempt leaves committed batches. NULL bounds defeat deduplication,
                    -- so each retry clears the table, the tally, and the source metadata.
                    withOsvRetry defaultOsvRetryPolicy $ do
                        resetIngestStats ingest
                        resetOsvAttempt ingest
                        liftIO $ execute_ conn "DELETE FROM package_vulnerability_ranges"
                        runConduit $
                            streamOsvUrl mTracerProvider ingest (csOsvExportUrl sources)
                                .| CL.filter ((== osvExportDirectory eco) . extEcosystem)
                                .| CL.chunksOf 2000
                                .| sinkSqlite conn

                    stats <- readIngestStats ingest
                    attempt <- readOsvAttempt ingest
                    concludeCompile metrics mSpan conn (conclusionOf ecosystem now feed attempt stats)

    conclusionOf ecosystem now feed attempt stats =
        CompileConclusion
            { ccEcosystem = ecosystem
            , ccSources = sources
            , ccStats = stats
            , ccProvenance = passProvenance sources feed attempt
            , ccQuietTime = quietTime
            , ccNow = now
            }

newCandidate :: FilePath -> IO FilePath
newCandidate outDir = do
    (path, handle) <- openTempFile outDir ".osv-candidate.db"
    hClose handle
    pure path

removeCandidate :: FilePath -> IO ()
removeCandidate path = catchIOError (removeFile path) (const $ pure ())

data CompileConclusion = CompileConclusion
    { ccEcosystem :: Text
    , ccSources :: CompileSources
    , ccStats :: IngestStats
    , ccProvenance :: AdvisoryProvenance
    , ccQuietTime :: QuietTime
    , ccNow :: UTCTime
    }

-- The sources one finished pass read, as they described themselves. The identities are
-- credential-free, because the artifact travels to every consumer.
passProvenance :: CompileSources -> EpssFeed -> OsvAttempt -> AdvisoryProvenance
passProvenance sources feed attempt =
    AdvisoryProvenance
        { apOsvSource = Just (credentialFreeUrl (toText (csOsvExportUrl sources)))
        , apOsvLastModified = oaLastModified attempt
        , apOsvNewestModified = oaNewestModified attempt
        , apEpssSource = Just (credentialFreeUrl (toText (csEpssFeedUrl sources)))
        , apEpssLastModified = efLastModified feed
        , apEpssScoreDate = efScoreDate feed
        , apEpssModelVersion = efModelVersion feed
        }

concludeCompile :: (KatipContext m) => AdvisoryCompileMetricsPort -> Maybe Span -> Connection -> CompileConclusion -> m ()
concludeCompile metrics mSpan conn conclusion = do
    forM_ mSpan $ \sp -> do
        addAttribute sp "ecluse.osv.accepted" (show (statAccepted stats) :: Text)
        addAttribute sp "ecluse.osv.dropped_oversize" (show (statDroppedOversize stats) :: Text)
        addAttribute sp "ecluse.osv.dropped_malformed" (show (statDroppedMalformed stats) :: Text)
        addAttribute sp "ecluse.osv.unorderable" (show (statUnorderable stats) :: Text)
    liftIO (recordTallies metrics stats)
    counted <- liftIO (query_ conn "SELECT COUNT(*) FROM package_vulnerability_ranges" :: IO [Only Int])
    let rowCount = maybe 0 fromOnly (listToMaybe counted)
    forM_ (compileRefusal stats rowCount) $ \reason -> do
        forM_ mSpan $ \sp -> setStatus sp (Error (reason <> ", compile abandoned"))
        liftIO (acmpCompileRun metrics CompileAborted)
        katipAddContext (dropFields ecosystem stats) $
            logFM ErrorS (ls ("Aborting OSV compile for " <> ecosystem <> ": " <> reason <> " (" <> renderDrops stats <> ")"))
        throwIO (PilotIngestAborted stats)

    liftIO $ writeMeta conn conclusion rowCount
    liftIO (acmpCompileRun metrics CompileCompleted)
    forM_ mSpan $ \sp -> addAttribute sp "ecluse.osv.row_count" (show rowCount :: Text)
    katipAddContext (sl "row_count" rowCount <> dropFields ecosystem stats) $
        logFM InfoS (ls ("Compiled " <> show rowCount <> " advisory ranges for " <> ecosystem <> " (" <> renderDrops stats <> ")"))
    warnOnUnusableDates ecosystem stats
    logSourceAges ecosystem (sourceAges (ccNow conclusion) (ccQuietTime conclusion) (ccProvenance conclusion))
  where
    ecosystem = ccEcosystem conclusion
    stats = ccStats conclusion

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
          , (renderMetaKey MetaSourceUrl, authorityLabel (toText (csOsvExportUrl sources)))
          , (renderMetaKey MetaEpssSourceUrl, authorityLabel (toText (csEpssFeedUrl sources)))
          , (renderMetaKey MetaEpssStatus, "available")
          , (renderMetaKey MetaRowCount, show rowCount)
          ]
            <> provenanceRows (ccProvenance conclusion)
        )
  where
    sources = ccSources conclusion

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
