-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Advisory handles, SQLite verification, and masked ownership transfer.
The public "Ecluse.Core.Cve" module curates these operations.
-}
module Ecluse.Core.Cve.Internal (
    AdvisoryRange (..),
    CveDbRejected (..),
    openHardenedConnection,
    acceptConnection,
    openCveDb,
    transferCveDb,
    CveDb (..),
    CveLookup (..),
    CveQueryFault (..),
    advisoriesQuery,
    coveredNamesQuery,
    toRange,
    provenanceQuery,
) where

import Database.SQLite.Simple (Connection, Only (..), SQLError, close, execute_, open, query, query_)
import UnliftIO.Exception (catch, catchAny, mask, onException, throwIO, try)

import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Osv.Provenance (AdvisoryProvenance, decodeProvenance)
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
openHardenedConnection eco epssRequirement dbFile =
    acceptConnection (open dbFile) close (verifyConnection eco epssRequirement) (\conn () -> pure conn)

-- | Acquire masked, verify under the caller's mask state, then transfer ownership to the masked continuation.
acceptConnection :: IO Connection -> (Connection -> IO ()) -> (Connection -> IO (Either CveDbRejected a)) -> (Connection -> a -> IO b) -> IO (Either CveDbRejected b)
acceptConnection acquire release verify use = mask $ \restore -> do
    conn <- acquire
    accepted <- restore (verify conn) `onException` release conn
    case accepted of
        Left rejection -> release conn $> Left rejection
        Right value -> Right <$> use conn value

verifyConnection :: Ecosystem -> EpssRequirement -> Connection -> IO (Either CveDbRejected ())
verifyConnection eco epssRequirement conn = hardenConnection conn >> acceptArtifact eco epssRequirement conn

-- | Reject incompatible artifacts as values. The caller must protect the returned database's ownership.
openCveDb :: Ecosystem -> EpssRequirement -> FilePath -> IO (Either CveDbRejected CveDb)
openCveDb eco epssRequirement dbFile = transferCveDb eco epssRequirement dbFile pure

-- | Transfer an accepted database to a masked owner. Verification retains the caller's masking state.
transferCveDb :: Ecosystem -> EpssRequirement -> FilePath -> (CveDb -> IO a) -> IO (Either CveDbRejected a)
transferCveDb eco epssRequirement dbFile use =
    acceptConnection (open dbFile) close verify (\conn meta -> use (mkCveDb conn meta))
  where
    verify conn =
        verifyConnection eco epssRequirement conn >>= \case
            Left rejection -> pure (Left rejection)
            Right () -> Right <$> provenanceQuery conn

hardenConnection :: Connection -> IO ()
hardenConnection conn = do
    execute_ conn "PRAGMA trusted_schema = OFF"
    execute_ conn "PRAGMA query_only = ON"
    execute_ conn "PRAGMA cell_size_check = ON"
    execute_ conn "PRAGMA mmap_size = 0"

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

-- | Query by canonical package key, with an npm scope inline. A display name is never a query key.
data CveLookup = CveLookup
    { cveAdvisoriesFor :: Text -> IO [AdvisoryRange]
    -- ^ Every advisory range recorded against a package name. Every field throws 'CveQueryFault' on a query fault.
    , cveCoveredNames :: IO [Text]
    -- ^ Every package name this generation records an advisory against, for a store sweep.
    }

-- | A database query fault for the rule's resilience policy to classify.
data CveQueryFault = CveQueryFault
    { cqfQuery :: Text
    -- ^ Which handle field was asked (@advisories-for@ or @covered-names@).
    , cqfDetail :: Text
    -- ^ The rendered 'SQLError', for the operator's outage report. Never parsed.
    }
    deriving stock (Eq, Show)

instance Exception CveQueryFault

-- | One opened artifact. Its holder owns the connection's lifetime, and hands a consumer 'cveDbLookup' only.
data CveDb = CveDb
    { cveDbLookup :: CveLookup
    -- ^ The view consumers query through.
    , cveDbClose :: IO ()
    -- ^ Release the connection. Owner-only, and __never throws__, since the connection is going away either way.
    , cveDbMeta :: [(Text, Text)]
    -- ^ The artifact's @meta@ provenance rows, snapshotted at open and key-sorted for the audit trail.
    , cveDbProvenance :: AdvisoryProvenance
    -- ^ What the artifact records about its sources. An artifact written before those keys decodes as absence.
    }

mkCveDb :: Connection -> [(Text, Text)] -> CveDb
mkCveDb conn meta =
    CveDb
        { cveDbLookup =
            CveLookup
                { cveAdvisoriesFor = taggedQuery "advisories-for" . advisoriesQuery conn
                , cveCoveredNames = taggedQuery "covered-names" (coveredNamesQuery conn)
                }
        , cveDbClose = close conn `catchAny` const pass
        , cveDbMeta = meta
        , cveDbProvenance = decodeProvenance meta
        }

-- The SQLite edge: the driver's 'SQLError' never escapes the handle, only this module's
-- confined 'CveQueryFault'.
taggedQuery :: Text -> IO a -> IO a
taggedQuery tag act = act `catch` \(err :: SQLError) -> throwIO (CveQueryFault tag (show err))
