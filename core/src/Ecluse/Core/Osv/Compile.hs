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
import Katip (KatipContext, Severity (..), logFM, ls)
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
import OpenTelemetry.Trace.Core (TracerProvider)

{- | Compile an ecosystem's OSV advisory export into the SQLite artifact and
return its path. The artifact's name, epoch stamp, and @meta@ table follow the
contract in "Ecluse.Core.Osv.Schema".
-}
compileOsvToSqlite :: (MonadResource m, MonadMask m, MonadUnliftIO m, KatipContext m) => Maybe TracerProvider -> FilePath -> Text -> String -> m FilePath
compileOsvToSqlite mTracerProvider outDir ecosystem urlStr = do
    let dbFile = outDir </> osvDbFileName ecosystem
    logFM InfoS (ls ("Compiling OSV data for " <> ecosystem <> " to " <> toText dbFile))

    -- Ensure clean state
    liftIO $ createDirectoryIfMissing True outDir
    liftIO $ catchIOError (removeFile dbFile) (const $ pure ())

    bracket (liftIO $ open dbFile) (liftIO . close) $ \conn -> do
        liftIO $ initSchema conn
        ingest <- newOsvIngest defaultIngestLimits

        -- The fetch runs under a truncated exponential backoff (see
        -- 'Ecluse.Core.Osv.Retry'): a transient osv.dev failure is retried with
        -- jittered, capped, and count-bounded backoff rather than tight-looping, so
        -- an outage cannot get our egress IP rate-limited or banned. Batches commit
        -- incrementally, so a mid-stream drop can leave a partial table behind; each
        -- attempt therefore wipes it first and re-streams from a clean slate. (INSERT
        -- OR IGNORE alone would not suffice: a NULL introduced/fixed bound is distinct
        -- under the dedup index's uniqueness, so a re-run would duplicate those ranges.)
        -- The ingest tally is reset alongside the table so it reflects only the final
        -- attempt.
        withOsvRetry defaultOsvRetryPolicy $ do
            resetIngestStats ingest
            liftIO $ execute_ conn "DELETE FROM package_vulnerability_ranges"
            runConduit $
                streamOsvUrl mTracerProvider ingest urlStr
                    .| CL.filter ((== ecosystem) . extEcosystem)
                    .| CL.chunksOf 2000
                    .| sinkSqlite conn

        -- The stream drops an over-large or malformed advisory rather than halting, so a
        -- few poisoned records never freeze the feed. But a systemically corrupt payload
        -- must not become a fresh-looking artifact that silently omits advisories: on a
        -- systemic drop rate, abandon the run before 'writeMeta' finalises it, so a
        -- consumer keeps its last-good db instead.
        stats <- readIngestStats ingest
        when (systemicDrop stats) $ do
            logFM ErrorS (ls ("Aborting OSV compile for " <> ecosystem <> ": " <> renderDrops stats))
            throwIO (PilotIngestAborted stats)

        rowCount <- liftIO $ writeMeta conn ecosystem urlStr
        logFM InfoS (ls ("Compiled " <> show rowCount <> " advisory ranges for " <> ecosystem <> " (" <> renderDrops stats <> ")"))

    pure dbFile

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

initSchema :: Connection -> IO ()
initSchema conn = do
    execute_ conn (Query rangesTableDdl)
    -- The dedup guard over a segment's five identity columns. A unique index
    -- rather than a composite PRIMARY KEY because @STRICT@ makes primary-key
    -- columns implicitly NOT NULL and the three bound columns are legitimately
    -- NULL; uniqueness behaviour is identical (INSERT OR IGNORE honours it, and
    -- NULL bounds are distinct under both forms).
    execute_ conn "CREATE UNIQUE INDEX uq_ranges_segment ON package_vulnerability_ranges(package_name, cve_id, introduced_version, fixed_version, last_affected_version)"
    execute_ conn "CREATE INDEX idx_package_name ON package_vulnerability_ranges(package_name)"
    -- The reader's remediation probe is an exact (name, fixed) equality; this
    -- index makes it one B-tree traversal. Additive, so epoch-neutral.
    execute_ conn "CREATE INDEX idx_package_fixed ON package_vulnerability_ranges(package_name, fixed_version)"
    execute_ conn (Query metaTableDdl)
    execute_ conn (fromString ("PRAGMA user_version = " <> show osvSchemaEpoch))

-- Written once, after the stream has completed: the row count is only
-- meaningful for a complete artifact.
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
