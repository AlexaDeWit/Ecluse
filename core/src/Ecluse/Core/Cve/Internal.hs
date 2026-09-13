-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The advisory lookup's internals: the hardened SQLite open and the raw queries
"Ecluse.Core.Cve" curates into the public handle.

Importing this module opts out of the public surface's stability promises. It exists
so a test can pin the hardening properties directly against the connection the handle
actually uses. That connection refuses writes, and it distrusts schema-borne SQL.
-}
module Ecluse.Core.Cve.Internal (
    AdvisoryRange (..),
    CveDbRejected (..),
    openHardenedConnection,
    probeQuery,
    advisoriesQuery,
    coveredNamesQuery,
    toRange,
    provenanceQuery,
) where

import Database.SQLite.Simple (Connection, Only (..), SQLError, close, execute_, open, query, query_)
import UnliftIO.Exception (onException, try)

import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Osv.Schema (ColumnSpec (..), EpssEvidence (EpssAvailable, EpssNotEstablished), EpssRequirement (EpssOptional, EpssRequired), MetaKey (MetaEcosystem, MetaEpssStatus), TableSpec (..), decodeEpssEvidence, osvSchemaEpoch, osvTableSpecs, renderMetaKey)
import Ecluse.Core.Osv.Types (UpperBound (FixedBefore, LastAffected, Unbounded))

{- | An advisory segment with nullable CVSS and EPSS scores and verbatim version bounds.
The introduced bound is inclusive. Absence means the segment starts at the beginning.
-}
data AdvisoryRange = AdvisoryRange
    { arCveId :: Text
    , arSeverity :: Maybe Double
    , arIntroduced :: Maybe Text
    , arUpperBound :: UpperBound
    , arEpss :: Maybe Double
    }
    deriving stock (Eq, Show)

{- | Why the hardened open refused an artifact before building a handle over it. A
rejection is a value, not a fault, so the caller can keep the last known-good database.
-}
data CveDbRejected
    = -- | The artifact's @user_version@ differs from 'osvSchemaEpoch'.
      CveDbWrongEpoch Int
    | -- | SQLite refused the file or reported integrity faults, carrying its error or report.
      CveDbIntegrityFailed [Text]
    | -- | A required relation is absent, non-strict, or lacks a column with its required type.
      CveDbSchemaNonConformant Text
    | -- | The ecosystem marker differs from the requested ecosystem or is absent.
      CveDbEcosystemMismatch (Maybe Text)
    | -- | Required feed enrichment lacks the exact success marker.
      CveDbEpssNotEstablished
    deriving stock (Eq, Show)

{- | Harden the connection before acceptance. SQLite's query-only pragma refuses writes.
Rejection and opening faults close the connection before returning.
-}
openHardenedConnection :: Ecosystem -> EpssRequirement -> FilePath -> IO (Either CveDbRejected Connection)
openHardenedConnection eco epssRequirement dbFile = do
    conn <- open dbFile
    -- The 'onException' guard closes the connection when a statement throws instead, for
    -- example a non-SQLite file whose first file-touching pragma raises.
    let hardenAndAccept = do
            execute_ conn "PRAGMA trusted_schema = OFF"
            execute_ conn "PRAGMA query_only = ON"
            execute_ conn "PRAGMA cell_size_check = ON"
            execute_ conn "PRAGMA mmap_size = 0"
            acceptArtifact eco epssRequirement conn
    accepted <- hardenAndAccept `onException` close conn
    case accepted of
        Left rejection -> do
            close conn
            pure (Left rejection)
        Right () -> pure (Right conn)

acceptArtifact :: Ecosystem -> EpssRequirement -> Connection -> IO (Either CveDbRejected ())
acceptArtifact eco epssRequirement conn = runExceptT $ do
    ExceptT (checkEpochStamp conn)
    ExceptT (checkIntegrity conn)
    traverse_ (ExceptT . checkTableConformance conn) osvTableSpecs
    ExceptT (checkMetaEcosystem eco conn)
    ExceptT (checkEpssRequirement epssRequirement conn)

checkEpochStamp :: Connection -> IO (Either CveDbRejected ())
checkEpochStamp conn = do
    -- The first header read can throw SQLITE_NOTADB. A typed rejection suppresses repeated downloads.
    stamped <- try (query_ conn "PRAGMA user_version") :: IO (Either SQLError [Only Int])
    pure $ case stamped of
        Left err -> Left (CveDbIntegrityFailed ["not a valid SQLite database: " <> show err])
        Right rows -> case map fromOnly rows of
            [epoch]
                | epoch == osvSchemaEpoch -> Right ()
                | otherwise -> Left (CveDbWrongEpoch epoch)
            _ -> Left (CveDbWrongEpoch 0)

-- SQLite can report corrupt pages as rows or throw during the integrity walk.
checkIntegrity :: Connection -> IO (Either CveDbRejected ())
checkIntegrity conn = do
    result <- try (query_ conn "PRAGMA quick_check") :: IO (Either SQLError [Only Text])
    pure $ case result of
        Left err -> Left (CveDbIntegrityFailed [show err])
        Right report -> case map fromOnly report of
            ["ok"] -> Right ()
            problems -> Left (CveDbIntegrityFailed problems)

-- Require real strict tables and their decoded columns. Compatible extra columns remain acceptable.
checkTableConformance :: Connection -> TableSpec -> IO (Either CveDbRejected ())
checkTableConformance conn spec = do
    listed <- try (query conn "SELECT type, strict FROM pragma_table_list WHERE name = ?" (Only (tableName spec))) :: IO (Either SQLError [(Maybe Text, Maybe Int)])
    columns <- try (query conn "SELECT name, type, \"notnull\" FROM pragma_table_xinfo(?)" (Only (tableName spec))) :: IO (Either SQLError [(Maybe Text, Maybe Text, Maybe Int)])
    pure $ case (listed, columns) of
        (Right [(Just "table", Just 1)], Right cols)
            | all (hasConformingColumn cols) (tableColumns spec) -> Right ()
        _ -> Left (CveDbSchemaNonConformant (tableName spec))

-- Is the required column among the table's actual columns, under its declared
-- type and (where the decode relies on it) NOT NULL?
hasConformingColumn :: [(Maybe Text, Maybe Text, Maybe Int)] -> ColumnSpec -> Bool
hasConformingColumn cols spec = any conforms cols
  where
    conforms (name, declaredType, notnull) =
        name == Just (colName spec)
            && declaredType == Just (colDeclaredType spec)
            && (not (colNotNull spec) || notnull == Just 1)

checkMetaEcosystem :: Ecosystem -> Connection -> IO (Either CveDbRejected ())
checkMetaEcosystem eco conn = do
    found <- readMetaValue conn MetaEcosystem
    pure $
        if found == Just (ecosystemName eco)
            then Right ()
            else Left (CveDbEcosystemMismatch found)

checkEpssRequirement :: EpssRequirement -> Connection -> IO (Either CveDbRejected ())
checkEpssRequirement EpssOptional _ = pure (Right ())
checkEpssRequirement EpssRequired conn = do
    evidence <- decodeEpssEvidence <$> readMetaValue conn MetaEpssStatus
    pure $ case evidence of
        EpssAvailable -> Right ()
        EpssNotEstablished -> Left CveDbEpssNotEstablished

-- Table conformance and integrity precede this decode. Query faults establish no metadata evidence.
readMetaValue :: Connection -> MetaKey -> IO (Maybe Text)
readMetaValue conn key = do
    result <- try (query conn "SELECT value FROM meta WHERE key = ?" (Only (renderMetaKey key))) :: IO (Either SQLError [Only Text])
    pure (either (const Nothing) (fmap fromOnly . listToMaybe) result)

{- | Does any advisory for this package name carry this exact version string as a fixed
bound? Deliberately string equality, under the artifact contract's canonical-semver expectation.
-}
probeQuery :: Connection -> Text -> Text -> IO Bool
probeQuery conn name version = do
    hits <- query conn "SELECT 1 FROM package_vulnerability_ranges WHERE package_name = ? AND fixed_version = ? LIMIT 1" (name, version) :: IO [Only Int]
    pure (not (null hits))

{- | Every package name this artifact records an advisory against, each once. The name index
covers the scan, and the result is what a store sweep intersects its listing with.
-}
coveredNamesQuery :: Connection -> IO [Text]
coveredNamesQuery conn =
    map fromOnly <$> query_ conn "SELECT DISTINCT package_name FROM package_vulnerability_ranges"

-- | Every advisory segment recorded against a package name.
advisoriesQuery :: Connection -> Text -> IO [AdvisoryRange]
advisoriesQuery conn name = do
    rows <- query conn "SELECT cve_id, introduced_version, fixed_version, last_affected_version, severity, epss_score FROM package_vulnerability_ranges WHERE package_name = ?" (Only name)
    pure (map toRange rows)

{- | One artifact row as an advisory segment, decoding the two nullable bound columns
into the segment's single upper bound.
-}
toRange :: (Text, Maybe Text, Maybe Text, Maybe Text, Maybe Double, Maybe Double) -> AdvisoryRange
toRange (cveId, intro, fixed, lastAffected, severity, epss) =
    AdvisoryRange
        { arCveId = cveId
        , arSeverity = severity
        , arIntroduced = intro
        , arUpperBound = upper
        , arEpss = epss
        }
  where
    -- The writer fills at most one bound column. A row carrying both resolves as the fix.
    upper = case (fixed, lastAffected) of
        (Just f, _) -> FixedBefore f
        (Nothing, Just la) -> LastAffected la
        (Nothing, Nothing) -> Unbounded

{- | The artifact's @meta@ provenance rows, key-sorted for a deterministic snapshot.
It runs only on an accepted connection, so the @(Text, Text)@ decode cannot throw.
-}
provenanceQuery :: Connection -> IO [(Text, Text)]
provenanceQuery conn = query_ conn "SELECT key, value FROM meta ORDER BY key"
