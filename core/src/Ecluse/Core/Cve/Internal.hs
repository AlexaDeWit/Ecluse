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
import Ecluse.Core.Osv.Schema (MetaKey (MetaEcosystem), osvSchemaEpoch, renderMetaKey)

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
    | {- | The ranges relation is not a plain table -- a view here is
      attacker-authored SQL wearing the table's name.
      -}
      CveDbRangesNotATable
    | {- | The artifact's @meta@ table names a different ecosystem (carried) than
      the one this handle was asked to serve, or the @meta@ table is absent or
      unreadable so the ecosystem cannot be confirmed ('Nothing').
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
before the file's interior is walked at all), a @PRAGMA quick_check@
structural-integrity walk (a malformed or truncated b-tree is rejected before any
lookup dereferences it), the ranges relation being a real table, and the @meta@
ecosystem matching the one asked for. A rejected artifact's connection is closed
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
    ExceptT (checkRangesTable conn)
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

checkRangesTable :: Connection -> IO (Either CveDbRejected ())
checkRangesTable conn = do
    -- 'sqlite_master' is present in any structurally-sound database (quick_check has
    -- already passed), but fold any SQLite throw into the rejection so acceptance
    -- stays total at the type: a read fault here is a refusal value the sync task
    -- remembers, never an exception that unwinds and re-fetches the artifact every poll.
    kinds <- try (query_ conn "SELECT type FROM sqlite_master WHERE name = 'package_vulnerability_ranges'") :: IO (Either SQLError [Only Text])
    pure $ case kinds of
        Left err -> Left (CveDbIntegrityFailed ["ranges relation unreadable: " <> show err])
        Right rows
            | map fromOnly rows == ["table"] -> Right ()
            | otherwise -> Left CveDbRangesNotATable

checkMetaEcosystem :: Ecosystem -> Connection -> IO (Either CveDbRejected ())
checkMetaEcosystem eco conn = do
    -- A structurally-sound artifact can still lack the @meta@ table entirely, in
    -- which case this query raises "no such table". Fold that throw into the same
    -- ecosystem-cannot-be-confirmed refusal a present-but-wrong value yields
    -- ('CveDbEcosystemMismatch' 'Nothing'), so a missing @meta@ table is a remembered
    -- rejection value rather than an exception that re-fetches the artifact every poll.
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
snapshot. An artifact with no @meta@ table would have failed acceptance, so
this only ever runs on an accepted connection.
-}
provenanceQuery :: Connection -> IO [(Text, Text)]
provenanceQuery conn = query_ conn "SELECT key, value FROM meta ORDER BY key"
