{- | The advisory lookup's internals: the hardened SQLite open and the raw
queries "Ecluse.Core.Cve" curates into the public handle.

Importing this module opts out of the public surface's stability promises; it
exists so tests can pin the hardening properties (the connection refuses
writes, schema-borne SQL is distrusted) directly against the connection the
handle actually uses.
-}
module Ecluse.Core.Cve.Internal (
    AdvisoryRange (..),
    CveDbRejected (..),
    openHardenedConnection,
    probeQuery,
    advisoriesQuery,
    provenanceQuery,
) where

import Database.SQLite.Simple (Connection, Only (..), SQLError, close, execute_, open, query, query_)
import UnliftIO.Exception (onException, try)

import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Osv.Schema (ColumnSpec (..), MetaKey (MetaEcosystem), TableSpec (..), osvSchemaEpoch, osvTableSpecs, renderMetaKey)

{- | One advisory segment recorded against a package: the advisory's identifier,
its CVSS base score (0 to 10, 'Nothing' when unscored), and the affected
interval's bounds as the artifact stores them (verbatim version text). The lower
bound 'arIntroduced' is inclusive ('Nothing' == from the beginning); the upper
bound is @'arFixed'@ (exclusive) or @'arLastAffected'@ (inclusive) or neither
(open-ended). An exactly-enumerated affected version is a point segment
(@introduced == last_affected@).
-}
data AdvisoryRange = AdvisoryRange
    { arCveId :: Text
    , arSeverity :: Maybe Double
    , arIntroduced :: Maybe Text
    , arFixed :: Maybe Text
    , arLastAffected :: Maybe Text
    }
    deriving stock (Eq, Show)

{- | Why a downloaded artifact was refused before a handle was built over it.

A rejection is a value, not an exception: the caller (the sync task, once it
exists) has a real decision to make, keep the last known-good database and
alarm, rather than a fault to unwind from.
-}
data CveDbRejected
    = {- | The artifact's @user_version@ stamp (carried) does not match this
      binary's 'osvSchemaEpoch'.
      -}
      CveDbWrongEpoch Int
    | {- | The artifact is not a usable SQLite database: either it is not a
      database at all (absent or wrong header magic, which SQLite reports as
      @SQLITE_NOTADB@ on the first header read), or @PRAGMA quick_check@ found it
      structurally corrupt (a malformed, truncated, or crafted b-tree). The
      carried lines are the thrown error or the integrity report (which SQLite
      caps at 100 problems).
      -}
      CveDbIntegrityFailed [Text]
    | {- | A required relation (carried) does not conform to the epoch's schema
      contract: absent, not a real @STRICT@ table, or missing a required column
      with its declared type. A view here is attacker-authored SQL wearing the
      table's name; a lax (non-@STRICT@) table would leave the reader's decodes
      exposed to type-confused values.
      -}
      CveDbSchemaNonConformant Text
    | {- | The artifact's @meta@ table names a different ecosystem (carried) than
      the one this handle was asked to serve, or carries no ecosystem row at all
      so the ecosystem cannot be confirmed ('Nothing'). An absent @meta@ table
      is caught earlier, as 'CveDbSchemaNonConformant'.
      -}
      CveDbEcosystemMismatch (Maybe Text)
    deriving stock (Eq, Show)

{- | Open an artifact read-only-in-effect and accept or reject it.

Hardening order matters, and every pragma is applied before the first query.
@trusted_schema = OFF@ distrusts schema-defined functions, views feeding
triggers, and virtual tables in the file; @query_only = ON@ refuses every write,
so no trigger can ever fire through the connection; @cell_size_check = ON@
validates each b-tree cell against its page as pages are read, so a crafted
oversized cell becomes a clean error rather than an out-of-bounds access; and
@mmap_size = 0@ keeps reads on the bounds-checked pager instead of mapping
hostile file pages straight into the address space.

Acceptance then checks, cheapest and least trusting first: the 'osvSchemaEpoch'
stamp (a header field, so a stale, substituted, or non-SQLite artifact is refused
before the file's interior is walked at all), a @PRAGMA quick_check@ integrity
walk (a malformed or truncated b-tree is rejected before any lookup dereferences
it, and stored values are verified against each @STRICT@ table's declared column
types), the required tables conforming to the epoch's schema contract
('osvTableSpecs': real @STRICT@ tables carrying the required columns with their
declared types, which is what makes every later row decode total), and the
@meta@ ecosystem matching the one asked for. A rejected artifact's connection is closed
before returning, and so is a connection whose hardening or acceptance /throws/
before it can return a rejection value: the whole phase runs under a
close-on-exception guard, so the just-opened connection is never leaked (the
"an exception never leaks it" contract 'Ecluse.Core.Cve.openCveDb' promises).

Read-only is enforced at the connection level: sqlite-simple's public API has
no way to pass @SQLITE_OPEN_READONLY@ at open time, and @query_only@ yields
the same guarantee for every statement this connection will run.
-}
openHardenedConnection :: Ecosystem -> FilePath -> IO (Either CveDbRejected Connection)
openHardenedConnection eco dbFile = do
    conn <- open dbFile
    -- Apply the hardening pragmas and accept-or-reject the artifact. Acceptance
    -- folds a hostile artifact into a 'CveDbRejected' value; the 'onException'
    -- guard closes the connection should a statement instead throw (e.g. a
    -- non-SQLite file whose first file-touching pragma raises), so the
    -- just-opened connection is never leaked on that path.
    let hardenAndAccept = do
            execute_ conn "PRAGMA trusted_schema = OFF"
            execute_ conn "PRAGMA query_only = ON"
            execute_ conn "PRAGMA cell_size_check = ON"
            execute_ conn "PRAGMA mmap_size = 0"
            acceptArtifact eco conn
    accepted <- hardenAndAccept `onException` close conn
    case accepted of
        Left rejection -> do
            close conn
            pure (Left rejection)
        Right () -> pure (Right conn)

acceptArtifact :: Ecosystem -> Connection -> IO (Either CveDbRejected ())
acceptArtifact eco conn = runExceptT $ do
    ExceptT (checkEpochStamp conn)
    ExceptT (checkIntegrity conn)
    traverse_ (ExceptT . checkTableConformance conn) osvTableSpecs
    ExceptT (checkMetaEcosystem eco conn)

checkEpochStamp :: Connection -> IO (Either CveDbRejected ())
checkEpochStamp conn = do
    -- @PRAGMA user_version@ is the first statement to read the file's header, so
    -- a non-SQLite artifact (absent or wrong header magic) raises @SQLITE_NOTADB@
    -- here rather than returning a stamp. Fold that throw into a rejection value,
    -- exactly as 'checkIntegrity' folds a b-tree walk that aborts: a hostile
    -- artifact is refused as a value the sync task can remember (so it is not
    -- re-downloaded every poll), never a fault that unwinds and leaks the
    -- connection.
    stamped <- try (query_ conn "PRAGMA user_version") :: IO (Either SQLError [Only Int])
    pure $ case stamped of
        Left err -> Left (CveDbIntegrityFailed ["not a valid SQLite database: " <> show err])
        Right rows -> case map fromOnly rows of
            [epoch]
                | epoch == osvSchemaEpoch -> Right ()
                | otherwise -> Left (CveDbWrongEpoch epoch)
            _ -> Left (CveDbWrongEpoch 0)

{- | Walk the whole database structure and refuse an artifact SQLite reports as
corrupt. @quick_check@ (unlike full @integrity_check@) skips the index-vs-table
content cross-validation we do not rely on, keeping the scan to the structural
soundness a hostile file could weaponise; it returns a single @ok@ on success.

A well-formed database reports its problems as result rows, but a badly enough
mangled b-tree can abort the walk with @SQLITE_CORRUPT@ instead. Both are the
same verdict here, so the thrown error is caught and folded into the rejection
rather than propagated: a hostile artifact is refused, never a fault to unwind.
-}
checkIntegrity :: Connection -> IO (Either CveDbRejected ())
checkIntegrity conn = do
    result <- try (query_ conn "PRAGMA quick_check") :: IO (Either SQLError [Only Text])
    pure $ case result of
        Left err -> Left (CveDbIntegrityFailed [show err])
        Right report -> case map fromOnly report of
            ["ok"] -> Right ()
            problems -> Left (CveDbIntegrityFailed problems)

{- | Does the artifact carry this relation as the schema contract demands: a
real @STRICT@ table with every required column under its declared type (and
@NOT NULL@ where the reader's decode relies on it)? Columns beyond the spec are
tolerated, keeping additive schema changes epoch-neutral. This declaration
check is one half of the totality guarantee; 'checkIntegrity' is the other,
verifying the stored values actually conform to the @STRICT@ declaration.

Any SQLite throw folds into the rejection so acceptance stays total at the
type: a read fault here is a refusal value the sync task remembers, never an
exception that unwinds and re-fetches the artifact every poll. The pragma rows
decode through 'Maybe' for the same reason -- nothing an artifact carries may
make this check throw.
-}
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
    -- By this point conformance has confirmed @meta@ is a real STRICT table of
    -- NOT NULL TEXT, and the integrity walk has verified the stored values, so
    -- the row decode here is total. The try-fold stays as the siblings' shape:
    -- should SQLite still throw, that is a refusal value the sync task
    -- remembers, never an exception that re-fetches the artifact every poll.
    named <- try (query conn "SELECT value FROM meta WHERE key = ?" (Only (renderMetaKey MetaEcosystem))) :: IO (Either SQLError [Only Text])
    pure $ case named of
        Left _ -> Left (CveDbEcosystemMismatch Nothing)
        Right rows ->
            let found = fromOnly <$> listToMaybe rows
             in if found == Just (ecosystemName eco)
                    then Right ()
                    else Left (CveDbEcosystemMismatch found)

{- | Does any advisory for this package name this exact version string as a
fixed bound? One indexed probe (@package_name, fixed_version@); deliberately
string equality, per the artifact contract's canonical-semver expectation.
-}
probeQuery :: Connection -> Text -> Text -> IO Bool
probeQuery conn name version = do
    hits <- query conn "SELECT 1 FROM package_vulnerability_ranges WHERE package_name = ? AND fixed_version = ? LIMIT 1" (name, version) :: IO [Only Int]
    pure (not (null hits))

-- | Every advisory segment recorded against a package name.
advisoriesQuery :: Connection -> Text -> IO [AdvisoryRange]
advisoriesQuery conn name = do
    rows <- query conn "SELECT cve_id, introduced_version, fixed_version, last_affected_version, severity FROM package_vulnerability_ranges WHERE package_name = ?" (Only name)
    pure (map toRange rows)
  where
    toRange (cveId, intro, fixed, lastAffected, severity) =
        AdvisoryRange
            { arCveId = cveId
            , arIntroduced = intro
            , arFixed = fixed
            , arLastAffected = lastAffected
            , arSeverity = severity
            }

{- | The artifact's @meta@ provenance rows, key-sorted for a deterministic
snapshot. This only ever runs on an accepted connection, and acceptance has
confirmed @meta@ is a @STRICT@ table of @NOT NULL TEXT@ whose stored values the
integrity walk verified, so the @(Text, Text)@ decode is total here: no
artifact content can make it throw.
-}
provenanceQuery :: Connection -> IO [(Text, Text)]
provenanceQuery conn = query_ conn "SELECT key, value FROM meta ORDER BY key"
