{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Osv.Database (
    compileToSqlite,
    withAdvisoryDb,
    lookupAdvisories,
) where

import Control.Exception (bracket)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit
import Data.Conduit.List qualified as CL
import Data.Text (Text)
import Database.SQLite.Simple qualified as SQLite
import Katip (KatipContext, Severity (..), logFM, ls)

import Ecluse.Osv (ExtractedOsv (..))

{- | A sink that compiles 'ExtractedOsv' into a SQLite database.
The resulting database is optimized for fast lookups by package name and ecosystem.
It uses bulk inserts and transactions for performance, and finishes with
VACUUM and ANALYZE to ensure the file is compact and query plans are optimized.
-}
compileToSqlite :: (MonadResource m, MonadThrow m, KatipContext m) => FilePath -> ConduitT ExtractedOsv o m ()
compileToSqlite dbPath = do
    lift $ logFM InfoS (ls ("Initializing SQLite database for OSV compilation at: " <> dbPath))

    bracketP
        (SQLite.open dbPath)
        SQLite.close
        $ \conn -> do
            liftIO $ setupSchema conn

            -- Performance tuning for bulk ingestion
            liftIO $ SQLite.execute_ conn "PRAGMA synchronous = OFF"
            liftIO $ SQLite.execute_ conn "PRAGMA journal_mode = MEMORY"
            liftIO $ SQLite.execute_ conn "PRAGMA trusted_schema = OFF"

            liftIO $ SQLite.execute_ conn "BEGIN TRANSACTION"

            let flatten osv = [(extPackage osv, extEcosystem osv, v) | v <- extFixedVersions osv]

            CL.map flatten
                .| CL.concat
                .| CL.chunksOf 1000
                .| CL.mapM_
                    ( \batch -> liftIO $ do
                        SQLite.executeMany conn "INSERT INTO advisories (package, ecosystem, fixed_version) VALUES (?, ?, ?)" batch
                    )

            liftIO $ SQLite.execute_ conn "COMMIT"

            lift $ logFM InfoS (ls ("Compacting and indexing OSV database" :: String))
            liftIO $ SQLite.execute_ conn "CREATE INDEX IF NOT EXISTS idx_advisories_pkg_eco_ver ON advisories (package, ecosystem, fixed_version)"
            liftIO $ SQLite.execute_ conn "VACUUM"
            liftIO $ SQLite.execute_ conn "ANALYZE"

{- | Open an advisory database in read-only mode and execute an action.
It applies defense-in-depth pragmas.
-}
withAdvisoryDb :: FilePath -> (SQLite.Connection -> IO a) -> IO a
withAdvisoryDb dbPath action =
    bracket
        (SQLite.open dbPath)
        SQLite.close
        ( \conn -> do
            SQLite.execute_ conn "PRAGMA query_only = ON"
            SQLite.execute_ conn "PRAGMA trusted_schema = OFF"
            action conn
        )
-- | Fast lookup for a package's remediation boundaries (fixed versions).
lookupAdvisories :: SQLite.Connection -> Text -> Text -> IO [Text]
lookupAdvisories conn pkg eco =
    fmap SQLite.fromOnly
        <$> SQLite.query conn "SELECT fixed_version FROM advisories WHERE package = ? AND ecosystem = ?" (pkg, eco)

setupSchema :: SQLite.Connection -> IO ()
setupSchema conn = do
    SQLite.execute_ conn "DROP TABLE IF EXISTS advisories"
    SQLite.execute_ conn "CREATE TABLE advisories (package TEXT NOT NULL, ecosystem TEXT NOT NULL, fixed_version TEXT NOT NULL)"
