-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Core.Osv.Compile (
    compileOsvToSqlite,
) where

import Conduit
import Control.Monad.Catch (MonadMask)
import Data.Conduit.List qualified as CL
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Version (showVersion)
import Database.SQLite.Simple
import Katip (KatipContext, Severity (..), SimpleLogPayload, katipAddContext, logFM, ls, sl)
import Paths_ecluse (version)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath ((</>))
import System.IO.Error (catchIOError)
import UnliftIO.Exception (bracket, throwIO)

import Ecluse.Core.Osv.Advisory (ExtractedOsv (..))
import Ecluse.Core.Osv.Retry (defaultOsvRetryPolicy, withOsvRetry)
import Ecluse.Core.Osv.Schema (MetaKey (..), metaTableDdl, osvDbFileName, osvSchemaEpoch, rangesTableDdl, renderMetaKey)
import Ecluse.Core.Osv.Stream (
    IngestStats (..),
    PilotIngestAborted (..),
    defaultIngestLimits,
    newOsvIngest,
    readIngestStats,
    resetIngestStats,
    streamOsvUrl,
    systemicDrop,
 )
import Ecluse.Core.Security.Authority (authorityLabel)
import Ecluse.Core.Telemetry.Metrics (
    AdvisoryCompileResult (CompileAborted, CompileCompleted),
    AdvisoryDropCause (DropMalformed, DropOversize),
 )
import Ecluse.Core.Telemetry.Record (AdvisoryCompileMetricsPort (acmpCompileAccepted, acmpCompileDropped, acmpCompileRun))
import OpenTelemetry.Context qualified as Ctx
import OpenTelemetry.Trace.Core (Span, SpanKind (Internal), SpanStatus (Error), TracerProvider, addAttribute, createSpan, defaultSpanArguments, endSpan, kind, makeTracer, setStatus, tracerOptions)

{- | Compile an ecosystem's OSV advisory export into the SQLite artifact at @outDir@.
The artifact's name, epoch stamp, and @meta@ table follow "Ecluse.Core.Osv.Schema".

The pass records its tallies and its verdict through @metrics@, which the caller binds to the
ecosystem it compiles. A fault that escapes the stream records neither, so the supervision above
reports an abandoned pass instead.
-}
compileOsvToSqlite :: (MonadResource m, MonadMask m, MonadUnliftIO m, KatipContext m) => AdvisoryCompileMetricsPort -> Maybe TracerProvider -> FilePath -> Text -> String -> m FilePath
compileOsvToSqlite metrics mTracerProvider outDir ecosystem urlStr = do
    let dbFile = outDir </> osvDbFileName ecosystem
        mTracer = (\tp -> makeTracer tp "ecluse" tracerOptions) <$> mTracerProvider
    logFM InfoS (ls ("Compiling OSV data for " <> ecosystem <> " to " <> toText dbFile))

    liftIO $ createDirectoryIfMissing True outDir
    liftIO $ catchIOError (removeFile dbFile) (const $ pure ())

    -- One span covers the whole compile pass. A systemic-drop abort marks it errored, so
    -- an abandoned run is legible from the trace alone.
    bracket
        (traverse (\t -> createSpan t Ctx.empty "ecluse.pilot.osv.compile" defaultSpanArguments{kind = Internal}) mTracer)
        (mapM_ (`endSpan` Nothing))
        $ \mSpan -> do
            forM_ mSpan $ \sp -> do
                addAttribute sp "ecluse.osv.ecosystem" ecosystem
                addAttribute sp "ecluse.osv.source_host" (authorityLabel (toText urlStr))

            bracket (liftIO $ open dbFile) (liftIO . close) $ \conn -> do
                liftIO $ initSchema conn
                ingest <- newOsvIngest defaultIngestLimits

                -- Batches commit incrementally, so a failed attempt leaves a partial table
                -- that INSERT OR IGNORE cannot dedup, because the unique index treats a
                -- NULL bound as distinct. Each retry therefore wipes the table and the tally.
                withOsvRetry defaultOsvRetryPolicy $ do
                    resetIngestStats ingest
                    liftIO $ execute_ conn "DELETE FROM package_vulnerability_ranges"
                    runConduit $
                        streamOsvUrl mTracerProvider ingest urlStr
                            .| CL.filter ((== ecosystem) . extEcosystem)
                            .| CL.chunksOf 2000
                            .| sinkSqlite conn

                stats <- readIngestStats ingest
                concludeCompile metrics mSpan conn ecosystem urlStr stats

    pure dbFile

-- A systemic drop rate must not ship as a fresh-looking artifact that silently omits
-- advisories, so this abandons the run before 'writeMeta' finalises it.
concludeCompile :: (KatipContext m) => AdvisoryCompileMetricsPort -> Maybe Span -> Connection -> Text -> String -> IngestStats -> m ()
concludeCompile metrics mSpan conn ecosystem urlStr stats = do
    forM_ mSpan $ \sp -> do
        addAttribute sp "ecluse.osv.accepted" (show (statAccepted stats) :: Text)
        addAttribute sp "ecluse.osv.dropped_oversize" (show (statDroppedOversize stats) :: Text)
        addAttribute sp "ecluse.osv.dropped_malformed" (show (statDroppedMalformed stats) :: Text)
    liftIO (recordTallies metrics stats)
    when (systemicDrop stats) $ do
        forM_ mSpan $ \sp -> setStatus sp (Error "systemic advisory drop rate; compile abandoned")
        liftIO (acmpCompileRun metrics CompileAborted)
        katipAddContext (dropFields ecosystem stats) $
            logFM ErrorS (ls ("Aborting OSV compile for " <> ecosystem <> ": " <> renderDrops stats))
        throwIO (PilotIngestAborted stats)

    rowCount <- liftIO $ writeMeta conn ecosystem urlStr
    liftIO (acmpCompileRun metrics CompileCompleted)
    forM_ mSpan $ \sp -> addAttribute sp "ecluse.osv.row_count" (show rowCount :: Text)
    katipAddContext (sl "row_count" rowCount <> dropFields ecosystem stats) $
        logFM InfoS (ls ("Compiled " <> show rowCount <> " advisory ranges for " <> ecosystem <> " (" <> renderDrops stats <> ")"))

-- An abandoned pass records its tallies too, and a pass with no drops records a zero, so
-- the drop series exists before the first drop.
recordTallies :: AdvisoryCompileMetricsPort -> IngestStats -> IO ()
recordTallies metrics stats = do
    acmpCompileAccepted metrics (statAccepted stats)
    acmpCompileDropped metrics DropOversize (statDroppedOversize stats)
    acmpCompileDropped metrics DropMalformed (statDroppedMalformed stats)

-- A one-line summary of an ingest pass's drop tally for the boot log.
renderDrops :: IngestStats -> Text
renderDrops s =
    "accepted "
        <> show (statAccepted s)
        <> ", dropped "
        <> show (statDroppedOversize s)
        <> " oversize / "
        <> show (statDroppedMalformed s)
        <> " malformed"

-- The drop tally as structured log fields. The completion line and the abort line share
-- it, so an operator can filter both on one shape.
dropFields :: Text -> IngestStats -> SimpleLogPayload
dropFields ecosystem s =
    sl "ecosystem" ecosystem
        <> sl "accepted" (statAccepted s)
        <> sl "dropped_oversize" (statDroppedOversize s)
        <> sl "dropped_malformed" (statDroppedMalformed s)

initSchema :: Connection -> IO ()
initSchema conn = do
    execute_ conn (Query rangesTableDdl)
    -- A unique index rather than a composite PRIMARY KEY: @STRICT@ makes primary-key
    -- columns implicitly NOT NULL, and the three bound columns are legitimately NULL.
    execute_ conn "CREATE UNIQUE INDEX uq_ranges_segment ON package_vulnerability_ranges(package_name, cve_id, introduced_version, fixed_version, last_affected_version)"
    execute_ conn "CREATE INDEX idx_package_name ON package_vulnerability_ranges(package_name)"
    -- The reader's remediation probe is an exact (name, fixed) equality, and this
    -- index makes it one B-tree traversal. Additive, so epoch-neutral.
    execute_ conn "CREATE INDEX idx_package_fixed ON package_vulnerability_ranges(package_name, fixed_version)"
    execute_ conn (Query metaTableDdl)
    execute_ conn (fromString ("PRAGMA user_version = " <> show osvSchemaEpoch))

-- Written once, after the stream completes: the row count is only meaningful for a
-- complete artifact.
writeMeta :: Connection -> Text -> String -> IO Int
writeMeta conn ecosystem urlStr = do
    now <- getCurrentTime
    counted <- query_ conn "SELECT COUNT(*) FROM package_vulnerability_ranges" :: IO [Only Int]
    let rowCount = maybe 0 fromOnly (listToMaybe counted)
    executeMany
        conn
        "INSERT INTO meta (key, value) VALUES (?, ?)"
        [ (renderMetaKey MetaPilotVersion, toText (showVersion version))
        , (renderMetaKey MetaEcosystem, ecosystem)
        , (renderMetaKey MetaBuiltAt, toText (iso8601Show now))
        , (renderMetaKey MetaSourceUrl, toText urlStr)
        , (renderMetaKey MetaRowCount, show rowCount)
        ]
    pure rowCount

sinkSqlite :: (MonadIO m) => Connection -> ConduitT [ExtractedOsv] o m ()
sinkSqlite conn = awaitForever $ \batch ->
    liftIO $
        withTransaction conn $
            executeMany
                conn
                "INSERT OR IGNORE INTO package_vulnerability_ranges (package_name, cve_id, introduced_version, fixed_version, last_affected_version, severity) VALUES (?, ?, ?, ?, ?, ?)"
                (map osvToRow batch)
  where
    osvToRow osv = (extPackage osv, extCveId osv, extIntroduced osv, extFixed osv, extLastAffected osv, extSeverity osv)
