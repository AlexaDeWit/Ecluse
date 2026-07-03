{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Pilot.Osv.Compile (
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
import UnliftIO.Exception (bracket)

import Ecluse.Osv.Schema (MetaKey (..), osvDbFileName, osvSchemaEpoch, renderMetaKey)
import Ecluse.Pilot.Osv (ExtractedOsv (..))
import Ecluse.Pilot.Osv.Retry (defaultOsvRetryPolicy, withOsvRetry)
import Ecluse.Pilot.Osv.Stream (streamOsvUrl)
import Ecluse.Telemetry (Telemetry)

{- | Compile an ecosystem's OSV advisory export into the SQLite artifact and
return its path. The artifact's name, epoch stamp, and @meta@ table follow the
contract in "Ecluse.Osv.Schema".
-}
compileOsvToSqlite :: (MonadResource m, MonadMask m, MonadUnliftIO m, KatipContext m) => Telemetry -> FilePath -> Text -> String -> m FilePath
compileOsvToSqlite telemetry outDir ecosystem urlStr = do
    let dbFile = outDir </> osvDbFileName ecosystem
    logFM InfoS (ls ("Compiling OSV data for " <> ecosystem <> " to " <> toText dbFile))

    -- Ensure clean state
    liftIO $ createDirectoryIfMissing True outDir
    liftIO $ catchIOError (removeFile dbFile) (const $ pure ())

    bracket (liftIO $ open dbFile) (liftIO . close) $ \conn -> do
        liftIO $ initSchema conn

        -- The fetch runs under a truncated exponential backoff (see
        -- 'Ecluse.Pilot.Osv.Retry'): a transient osv.dev failure is retried with
        -- jittered, capped, and count-bounded backoff rather than tight-looping, so
        -- an outage cannot get our egress IP rate-limited or banned. Batches commit
        -- incrementally, so a mid-stream drop can leave a partial table behind; each
        -- attempt therefore wipes it first and re-streams from a clean slate. (INSERT
        -- OR IGNORE alone would not suffice: a NULL introduced/fixed bound is distinct
        -- under the composite primary key, so a re-run would duplicate those ranges.)
        withOsvRetry defaultOsvRetryPolicy $ do
            liftIO $ execute_ conn "DELETE FROM package_vulnerability_ranges"
            runConduit $
                streamOsvUrl telemetry urlStr
                    .| CL.chunksOf 2000
                    .| sinkSqlite conn

        rowCount <- liftIO $ writeMeta conn ecosystem urlStr
        logFM InfoS (ls ("Compiled " <> show rowCount <> " advisory ranges for " <> ecosystem))

    pure dbFile

initSchema :: Connection -> IO ()
initSchema conn = do
    execute_
        conn
        "CREATE TABLE package_vulnerability_ranges (\
        \  package_name TEXT NOT NULL,\
        \  cve_id TEXT NOT NULL,\
        \  introduced_version TEXT,\
        \  fixed_version TEXT,\
        \  severity TEXT,\
        \  PRIMARY KEY (package_name, cve_id, introduced_version, fixed_version)\
        \)"
    execute_ conn "CREATE INDEX idx_package_name ON package_vulnerability_ranges(package_name)"
    execute_
        conn
        "CREATE TABLE meta (\
        \  key TEXT NOT NULL PRIMARY KEY,\
        \  value TEXT NOT NULL\
        \)"
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
                "INSERT OR IGNORE INTO package_vulnerability_ranges (package_name, cve_id, introduced_version, fixed_version, severity) VALUES (?, ?, ?, ?, ?)"
                (map osvToRow batch)
  where
    osvToRow osv = (extPackage osv, extCveId osv, extIntroduced osv, extFixed osv, extSeverity osv)
