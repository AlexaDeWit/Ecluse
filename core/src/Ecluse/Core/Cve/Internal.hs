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
) where

import Database.SQLite.Simple (Connection, Only (..), close, execute_, open, query, query_)

import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Osv.Schema (MetaKey (MetaEcosystem), osvSchemaEpoch, renderMetaKey)

{- | One advisory range recorded against a package: the advisory's identifier,
its optional qualitative severity label, and the affected interval's bounds as
the artifact stores them (verbatim version text; 'Nothing' introduced means
"from the beginning", 'Nothing' fixed means "no fix known").
-}
data AdvisoryRange = AdvisoryRange
    { arCveId :: Text
    , arSeverity :: Maybe Text
    , arIntroduced :: Maybe Text
    , arFixed :: Maybe Text
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
    | {- | The ranges relation is not a plain table -- a view here is
      attacker-authored SQL wearing the table's name.
      -}
      CveDbRangesNotATable
    | {- | The artifact's @meta@ table names a different ecosystem (carried)
      than the one this handle was asked to serve.
      -}
      CveDbEcosystemMismatch (Maybe Text)
    deriving stock (Eq, Show)

{- | Open an artifact read-only-in-effect and accept or reject it.

Hardening order matters: @trusted_schema = OFF@ (schema-defined functions,
views feeding triggers, and virtual tables in the file are distrusted) and
@query_only = ON@ (the connection refuses every write, so no trigger can ever
fire through it) are applied before the first query. Acceptance then checks,
cheapest and least trusting first: the 'osvSchemaEpoch' stamp, the ranges
relation being a real table, and the @meta@ ecosystem matching the one asked
for. A rejected artifact's connection is closed before returning.

Read-only is enforced at the connection level: sqlite-simple's public API has
no way to pass @SQLITE_OPEN_READONLY@ at open time, and @query_only@ yields
the same guarantee for every statement this connection will run.
-}
openHardenedConnection :: Ecosystem -> FilePath -> IO (Either CveDbRejected Connection)
openHardenedConnection eco dbFile = do
    conn <- open dbFile
    execute_ conn "PRAGMA trusted_schema = OFF"
    execute_ conn "PRAGMA query_only = ON"
    accepted <- acceptArtifact eco conn
    case accepted of
        Left rejection -> do
            close conn
            pure (Left rejection)
        Right () -> pure (Right conn)

acceptArtifact :: Ecosystem -> Connection -> IO (Either CveDbRejected ())
acceptArtifact eco conn = do
    stamped <- query_ conn "PRAGMA user_version" :: IO [Only Int]
    case map fromOnly stamped of
        [epoch] | epoch == osvSchemaEpoch -> do
            kinds <- query_ conn "SELECT type FROM sqlite_master WHERE name = 'package_vulnerability_ranges'" :: IO [Only Text]
            if map fromOnly kinds /= ["table"]
                then pure (Left CveDbRangesNotATable)
                else do
                    named <- query conn "SELECT value FROM meta WHERE key = ?" (Only (renderMetaKey MetaEcosystem)) :: IO [Only Text]
                    let found = fromOnly <$> listToMaybe named
                    if found == Just (ecosystemName eco)
                        then pure (Right ())
                        else pure (Left (CveDbEcosystemMismatch found))
        [epoch] -> pure (Left (CveDbWrongEpoch epoch))
        _ -> pure (Left (CveDbWrongEpoch 0))

{- | Does any advisory for this package name this exact version string as a
fixed bound? One indexed probe (@package_name, fixed_version@); deliberately
string equality, per the artifact contract's canonical-semver expectation.
-}
probeQuery :: Connection -> Text -> Text -> IO Bool
probeQuery conn name version = do
    hits <- query conn "SELECT 1 FROM package_vulnerability_ranges WHERE package_name = ? AND fixed_version = ? LIMIT 1" (name, version) :: IO [Only Int]
    pure (not (null hits))

-- | Every advisory range recorded against a package name.
advisoriesQuery :: Connection -> Text -> IO [AdvisoryRange]
advisoriesQuery conn name = do
    rows <- query conn "SELECT cve_id, introduced_version, fixed_version, severity FROM package_vulnerability_ranges WHERE package_name = ?" (Only name)
    pure (map toRange rows)
  where
    toRange (cveId, intro, fixed, severity) =
        AdvisoryRange
            { arCveId = cveId
            , arIntroduced = intro
            , arFixed = fixed
            , arSeverity = severity
            }
