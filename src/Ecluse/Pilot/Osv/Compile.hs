{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Pilot.Osv.Compile (
    compileOsvToSqlite,
) where

import Conduit
import Data.Conduit.List qualified as CL
import Data.Version (showVersion)
import Database.SQLite.Simple
import Katip (KatipContext, Severity (..), logFM, ls)
import Paths_ecluse (version)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath ((</>))
import System.IO.Error (catchIOError)
import UnliftIO.Exception (bracket)

import Ecluse.Pilot.Osv (ExtractedOsv (..))
import Ecluse.Pilot.Osv.Stream (streamOsvUrl)
import Ecluse.Telemetry (Telemetry)

compileOsvToSqlite :: (MonadResource m, MonadThrow m, MonadUnliftIO m, PrimMonad m, KatipContext m) => Telemetry -> FilePath -> Text -> String -> m FilePath
compileOsvToSqlite telemetry outDir ecosystem urlStr = do
    let dbFile = outDir </> (toString ecosystem <> "-v" <> showVersion version <> "-osv.db")
    logFM InfoS (ls ("Compiling OSV data for " <> ecosystem <> " to " <> toText dbFile))

    -- Ensure clean state
    liftIO $ createDirectoryIfMissing True outDir
    liftIO $ catchIOError (removeFile dbFile) (const $ pure ())

    bracket (liftIO $ open dbFile) (liftIO . close) $ \conn -> do
        liftIO $ initSchema conn

        runConduit $
            streamOsvUrl telemetry urlStr
                .| CL.chunksOf 2000
                .| sinkSqlite conn

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
        \  epss_score REAL,\
        \  PRIMARY KEY (package_name, cve_id, introduced_version, fixed_version)\
        \)"
    execute_ conn "CREATE INDEX idx_package_name ON package_vulnerability_ranges(package_name)"

sinkSqlite :: (MonadIO m) => Connection -> ConduitT [ExtractedOsv] o m ()
sinkSqlite conn = awaitForever $ \batch ->
    liftIO $
        withTransaction conn $
            executeMany
                conn
                "INSERT INTO package_vulnerability_ranges (package_name, cve_id, introduced_version, fixed_version, severity, epss_score) VALUES (?, ?, ?, ?, NULL, NULL)"
                (map osvToRow batch)
  where
    osvToRow osv = (extPackage osv, extCveId osv, extIntroduced osv, extFixed osv)
