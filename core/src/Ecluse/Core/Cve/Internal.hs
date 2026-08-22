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
    provenanceQuery,
) where

import Database.SQLite.Simple (Connection, Only (..), SQLError, close, execute_, open, query, query_)
import UnliftIO.Exception (onException, try)

import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Osv.Schema (ColumnSpec (..), MetaKey (MetaEcosystem), TableSpec (..), osvSchemaEpoch, osvTableSpecs, renderMetaKey)

{- | One advisory segment recorded against a package: the advisory's identifier, its
CVSS base score (0 to 10, 'Nothing' when unscored), and the affected interval's bounds.
The artifact stores the bounds as verbatim version text. The lower bound 'arIntroduced' is
inclusive ('Nothing' == from the beginning). The upper bound is @'arFixed'@
(exclusive), or @'arLastAffected'@ (inclusive), or neither (open-ended). An
exactly-enumerated affected version is a point segment (@introduced == last_affected@).
-}
data AdvisoryRange = AdvisoryRange
    { arCveId :: Text
    , arSeverity :: Maybe Double
    , arIntroduced :: Maybe Text
    , arFixed :: Maybe Text
    , arLastAffected :: Maybe Text
    }
    deriving stock (Eq, Show)

{- | Why the hardened open refused an artifact before building a handle over it.

A rejection is a value, not an exception. The caller (the sync task, once it exists)
has a real decision to make: keep the last known-good database and alarm. It does not
have a fault to unwind from.
-}
data CveDbRejected
    = {- | The artifact's @user_version@ stamp (carried) does not match this
      binary's 'osvSchemaEpoch'.
      -}
      CveDbWrongEpoch Int
    | {- | The artifact is not a usable SQLite database. Either it is not a
      database at all (absent or wrong header magic, which SQLite reports as
      @SQLITE_NOTADB@ on the first header read), or @PRAGMA quick_check@ found it
      structurally corrupt (a malformed, truncated, or crafted b-tree). The
      carried lines are the thrown error or the integrity report, which SQLite
      caps at 100 problems.
      -}
      CveDbIntegrityFailed [Text]
    | {- | A required relation (carried) does not conform to the epoch's schema
      contract: absent, not a real @STRICT@ table, or missing a required column
      with its declared type. A view here is attacker-authored SQL wearing the
      table's name. A lax (non-@STRICT@) table would leave the reader's decodes
      exposed to type-confused values.
      -}
      CveDbSchemaNonConformant Text
    | {- | The artifact's @meta@ table names a different ecosystem (carried) than
      the one this handle was asked to serve, or carries no ecosystem row at all
      so nothing can confirm the ecosystem ('Nothing'). Conformance catches an
      absent @meta@ table earlier, as 'CveDbSchemaNonConformant'.
      -}
      CveDbEcosystemMismatch (Maybe Text)
    deriving stock (Eq, Show)

{- | Open an artifact read-only-in-effect and accept or reject it.

Hardening order matters, and every pragma runs before the first query.

* @trusted_schema = OFF@ distrusts schema-defined functions, views feeding triggers,
  and virtual tables in the file.
* @query_only = ON@ refuses every write, so no trigger can ever fire through the
  connection.
* @cell_size_check = ON@ validates each b-tree cell against its page as the pager
  reads it. A crafted oversized cell becomes a clean error, not an out-of-bounds
  access.
* @mmap_size = 0@ keeps reads on the bounds-checked pager instead of mapping hostile
  file pages straight into the address space.

Acceptance then checks, cheapest and least trusting first: the 'osvSchemaEpoch' stamp,
a @PRAGMA quick_check@ integrity walk, and the required tables against the epoch's
schema contract. Last, the @meta@ ecosystem must match the one asked for. The stamp is
a header field, so acceptance refuses a stale, substituted, or non-SQLite artifact
before anything walks the file's interior at all. The integrity walk rejects a
malformed or truncated b-tree before any lookup dereferences it, and verifies stored
values against each @STRICT@ table's declared column types. The conformance check
('osvTableSpecs') demands real @STRICT@ tables carrying the required columns with their
declared types, which is what makes every later row decode total.

A rejected artifact's connection closes before this returns, and so does a connection
whose hardening or acceptance /throws/ before it can return a rejection value. The
whole phase runs under a close-on-exception guard, so it never leaks the just-opened
connection. That is the "an exception never leaks it" contract
'Ecluse.Core.Cve.openCveDb' promises.

Read-only holds at the connection level. The sqlite-simple public API has no way to
pass @SQLITE_OPEN_READONLY@ at open time, and @query_only@ gives the same guarantee for
every statement this connection runs.
-}
openHardenedConnection :: Ecosystem -> FilePath -> IO (Either CveDbRejected Connection)
openHardenedConnection eco dbFile = do
    conn <- open dbFile
    -- Apply the hardening pragmas and accept-or-reject the artifact. Acceptance
    -- folds a hostile artifact into a 'CveDbRejected' value. The 'onException'
    -- guard closes the connection should a statement instead throw, for example a
    -- non-SQLite file whose first file-touching pragma raises. That path never leaks
    -- the just-opened connection.
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
    -- @PRAGMA user_version@ is the first statement to read the file's header. A
    -- non-SQLite artifact (absent or wrong header magic) therefore raises
    -- @SQLITE_NOTADB@ here rather than returning a stamp. Fold that throw into a
    -- rejection value, exactly as 'checkIntegrity' folds a b-tree walk that aborts.
    -- The check refuses a hostile artifact as a value the sync task can remember, so
    -- no poll re-downloads it. It never refuses one as a fault that unwinds and
    -- leaks the connection.
    stamped <- try (query_ conn "PRAGMA user_version") :: IO (Either SQLError [Only Int])
    pure $ case stamped of
        Left err -> Left (CveDbIntegrityFailed ["not a valid SQLite database: " <> show err])
        Right rows -> case map fromOnly rows of
            [epoch]
                | epoch == osvSchemaEpoch -> Right ()
                | otherwise -> Left (CveDbWrongEpoch epoch)
            _ -> Left (CveDbWrongEpoch 0)

{- | Walk the whole database structure and refuse an artifact SQLite reports as
corrupt. Unlike full @integrity_check@, @quick_check@ skips the index-vs-table
content cross-validation this code does not rely on. It keeps the scan to the
structural soundness a hostile file could weaponise, and returns a single @ok@ on
success.

A well-formed database reports its problems as result rows, but a badly enough
mangled b-tree can abort the walk with @SQLITE_CORRUPT@ instead. Both are the same
verdict here, so this catches the thrown error and folds it into the rejection rather
than propagating it. The check refuses a hostile artifact, never raising a fault to
unwind.
-}
checkIntegrity :: Connection -> IO (Either CveDbRejected ())
checkIntegrity conn = do
    result <- try (query_ conn "PRAGMA quick_check") :: IO (Either SQLError [Only Text])
    pure $ case result of
        Left err -> Left (CveDbIntegrityFailed [show err])
        Right report -> case map fromOnly report of
            ["ok"] -> Right ()
            problems -> Left (CveDbIntegrityFailed problems)

{- | Does the artifact carry this relation as the schema contract demands? The contract
wants a real @STRICT@ table with every required column under its declared type, and
@NOT NULL@ where the reader's decode relies on it. The check tolerates a column beyond
the spec, which keeps an additive schema change epoch-neutral. It is one half of
the totality guarantee. 'checkIntegrity' is the other, verifying that the stored
values conform to the @STRICT@ declaration.

Any SQLite throw folds into the rejection, so acceptance stays total at the type. A
read fault here is a refusal value the sync task remembers, never an exception that
unwinds and re-fetches the artifact every poll. The pragma rows decode through 'Maybe'
for the same reason: nothing an artifact carries may make this check throw.
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
    -- By this point conformance confirmed @meta@ is a real STRICT table of
    -- NOT NULL TEXT, and the integrity walk verified the stored values. The row
    -- decode here is therefore total. The try-fold keeps the siblings' shape: a
    -- throw still becomes a refusal value, never an exception.
    named <- try (query conn "SELECT value FROM meta WHERE key = ?" (Only (renderMetaKey MetaEcosystem))) :: IO (Either SQLError [Only Text])
    pure $ case named of
        Left _ -> Left (CveDbEcosystemMismatch Nothing)
        Right rows ->
            let found = fromOnly <$> listToMaybe rows
             in if found == Just (ecosystemName eco)
                    then Right ()
                    else Left (CveDbEcosystemMismatch found)

{- | Does any advisory for this package name carry this exact version string as a
fixed bound? One indexed probe (@package_name, fixed_version@), and deliberately
string equality, under the artifact contract's canonical-semver expectation.
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

{- | The artifact's @meta@ provenance rows, key-sorted for a deterministic snapshot.
This only ever runs on an accepted connection. Acceptance confirmed @meta@ is a
@STRICT@ table of @NOT NULL TEXT@ whose stored values the integrity walk verified. The
@(Text, Text)@ decode is therefore total here: no artifact content can make it throw.
-}
provenanceQuery :: Connection -> IO [(Text, Text)]
provenanceQuery conn = query_ conn "SELECT key, value FROM meta ORDER BY key"
