{- | The shared OSV advisory fixture corpus and the artifacts derived from it.

The committed JSON advisories under @test\/fixtures\/osv\/@ are the single
source of truth for advisory-shaped test data. Everything a suite consumes is
derived from them at test time: 'osvCorpusZip' assembles the osv.dev-shaped
export archive in memory, and the app-tier helper (@Ecluse.Test.OsvDb@ in
@ecluse-test-support-app@) compiles that archive into a real @osv.db@ through
the same pipeline Pilot runs. Deriving instead of committing binaries means a
fixture can never drift from the artifact contract ("Ecluse.Core.Osv.Schema").

The corpus is versioned: 'CorpusV2' is 'CorpusV1' plus an advisory for a
package V1 leaves clean, so a V1-to-V2 shadow-swap flips an observable rule
outcome as well as the artifact's ETag.

The hostile builders are the deliberate exception to "the real compiler is
the only writer": they model tampered artifacts the compiler must never be
able to produce, for the reader's rejection tests.
-}
module Ecluse.Test.Osv (
    -- * The corpus
    CorpusVersion (..),
    osvCorpusFiles,
    osvCorpusZip,
    osvZipOf,

    -- * Hostile artifacts
    mkDbWithWrongEpoch,
    mkDbWithViewShadowingRanges,
    mkDbWithMaliciousTrigger,
    mkDbWithMalformedProvenance,
    mkDbWithMalformedEcosystem,
    mkDbWithCorruptPage,
    mkMinimalValidDb,
) where

import Codec.Archive.Zip.Conduit.Zip (ZipData (..), ZipEntry (..), defaultZipOptions, zipStream)
import Conduit (runConduit, sinkLazy, yieldMany, (.|))
import Data.ByteString qualified as BS
import Data.Time (LocalTime (..), fromGregorian, midnight)
import Database.SQLite.Simple (Connection, Only (Only), execute, execute_, withConnection)
import System.FilePath (takeFileName, (</>))
import System.IO (SeekMode (AbsoluteSeek), hSeek, withBinaryFile)

import Ecluse.Core.Osv.Schema (osvSchemaEpoch)

data CorpusVersion = CorpusV1 | CorpusV2
    deriving stock (Bounded, Enum, Eq, Show)

corpusRoot :: FilePath
corpusRoot = "test/fixtures/osv"

-- Explicit lists, not a directory listing: the corpus is pinned by name, so a
-- stray file cannot silently join the fixture set.
corpusV1Files :: [FilePath]
corpusV1Files =
    [ "v1/GHSA-corpus-0001.json"
    , "v1/GHSA-corpus-0002.json"
    , "v1/GHSA-corpus-0003.json"
    , "v1/GHSA-corpus-0004.json"
    , "v1/GHSA-corpus-0005.json"
    , "v1/GHSA-corpus-0006.json"
    , "v1/malformed-deliberate.json"
    ]

corpusV2ExtraFiles :: [FilePath]
corpusV2ExtraFiles = ["v2/GHSA-corpus-1001.json"]

-- | A corpus version's advisory files, as (zip-entry name, bytes).
osvCorpusFiles :: CorpusVersion -> IO [(FilePath, LByteString)]
osvCorpusFiles v = traverse readEntry (files v)
  where
    files CorpusV1 = corpusV1Files
    files CorpusV2 = corpusV1Files <> corpusV2ExtraFiles
    readEntry rel = do
        bytes <- readFileLBS (corpusRoot </> rel)
        pure (takeFileName rel, bytes)

{- | Assemble the osv.dev-shaped export (a flat zip of advisory JSON files)
for a corpus version, in memory. The entry timestamp is fixed so the archive
is deterministic.
-}
osvCorpusZip :: CorpusVersion -> IO LByteString
osvCorpusZip v = do
    entries <- osvCorpusFiles v
    osvZipOf (map (first toText) entries)

{- | Assemble an osv.dev-shaped export (a flat zip of advisory JSON files) from
arbitrary (entry name, bytes) pairs, in memory. The entry timestamp is fixed so the
archive is deterministic. Suites use this to build tampered or pathological archives
the corpus does not carry.
-}
osvZipOf :: [(Text, LByteString)] -> IO LByteString
osvZipOf entries =
    runConduit $
        yieldMany (map toZipEntry entries)
            .| void (zipStream defaultZipOptions)
            .| sinkLazy
  where
    toZipEntry (name, bytes) =
        ( ZipEntry
            { zipEntryName = Left name
            , zipEntryTime = corpusTimestamp
            , zipEntrySize = Nothing
            , zipEntryExternalAttributes = Nothing
            }
        , ZipDataByteString bytes
        )

corpusTimestamp :: LocalTime
corpusTimestamp = LocalTime (fromGregorian 2026 1 1) midnight

{- | A structurally plausible artifact stamped with a different table-schema
epoch. A reader must reject it on the 'osvSchemaEpoch' check alone, before
trusting anything else about the file, so the interior shape is deliberately
minimal.
-}
mkDbWithWrongEpoch :: FilePath -> IO ()
mkDbWithWrongEpoch path = withConnection path $ \conn -> do
    execute_
        conn
        "CREATE TABLE package_vulnerability_ranges (\
        \  package_name TEXT NOT NULL,\
        \  cve_id TEXT NOT NULL,\
        \  introduced_version TEXT,\
        \  fixed_version TEXT,\
        \  last_affected_version TEXT,\
        \  severity REAL\
        \)"
    setEpoch conn (osvSchemaEpoch + 1)

{- | An artifact with the right epoch whose ranges relation is a __view__:
schema-borne SQL that a hardened reader (read-only,
@PRAGMA trusted_schema = OFF@) must refuse to evaluate on the file's terms.
-}
mkDbWithViewShadowingRanges :: FilePath -> IO ()
mkDbWithViewShadowingRanges path = withConnection path $ \conn -> do
    execute_
        conn
        "CREATE TABLE raw_rows (\
        \  package_name TEXT,\
        \  cve_id TEXT,\
        \  introduced_version TEXT,\
        \  fixed_version TEXT,\
        \  last_affected_version TEXT,\
        \  severity REAL\
        \)"
    execute_
        conn
        "CREATE VIEW package_vulnerability_ranges AS \
        \SELECT package_name, cve_id, introduced_version, fixed_version, last_affected_version, severity FROM raw_rows"
    setEpoch conn osvSchemaEpoch

{- | An artifact that passes acceptance (right epoch, real ranges table, npm
@meta@ row) but carries a malicious trigger poised on the ranges table. A
read-only consumer must behave exactly as it would on a clean artifact: a
trigger can only ever fire on a write, and the hardened connection refuses
writes outright.
-}
mkDbWithMaliciousTrigger :: FilePath -> IO ()
mkDbWithMaliciousTrigger path = withConnection path $ \conn -> do
    execute_
        conn
        "CREATE TABLE package_vulnerability_ranges (\
        \  package_name TEXT NOT NULL,\
        \  cve_id TEXT NOT NULL,\
        \  introduced_version TEXT,\
        \  fixed_version TEXT,\
        \  last_affected_version TEXT,\
        \  severity REAL\
        \)"
    execute_ conn "CREATE TABLE meta (key TEXT NOT NULL PRIMARY KEY, value TEXT NOT NULL)"
    execute_ conn "INSERT INTO meta (key, value) VALUES ('ecosystem', 'npm')"
    execute_ conn "INSERT INTO package_vulnerability_ranges VALUES ('trigger-pkg', 'GHSA-trigger', '0', '1.0.0', NULL, 7.5)"
    execute_
        conn
        "CREATE TRIGGER malicious AFTER INSERT ON package_vulnerability_ranges \
        \BEGIN DELETE FROM package_vulnerability_ranges; END"
    setEpoch conn osvSchemaEpoch

{- | An artifact acceptance passes (right epoch, real ranges table, npm @meta@
row) whose @meta@ holds one extra row with a BLOB value. SQLite's TEXT
affinity stores a blob verbatim, so the row survives the schema's @NOT NULL
TEXT@ declaration yet defeats a @(Text, Text)@ decode of the provenance rows,
after acceptance (which decodes only the ecosystem row) has already
succeeded. Handle construction must fail without leaking the accepted
connection.
-}
mkDbWithMalformedProvenance :: FilePath -> IO ()
mkDbWithMalformedProvenance path = withConnection path $ \conn -> do
    execute_
        conn
        "CREATE TABLE package_vulnerability_ranges (\
        \  package_name TEXT NOT NULL,\
        \  cve_id TEXT NOT NULL,\
        \  introduced_version TEXT,\
        \  fixed_version TEXT,\
        \  last_affected_version TEXT,\
        \  severity REAL\
        \)"
    execute_ conn "CREATE TABLE meta (key TEXT NOT NULL PRIMARY KEY, value TEXT NOT NULL)"
    execute_ conn "INSERT INTO meta (key, value) VALUES ('ecosystem', 'npm')"
    execute_ conn "INSERT INTO meta (key, value) VALUES ('zz-opaque', X'DEADBEEF')"
    setEpoch conn osvSchemaEpoch

{- | An artifact with the right epoch whose @meta@ ecosystem row holds a BLOB
value. Acceptance (specifically 'checkMetaEcosystem') must fold the decode
throw into a rejection value (CveDbEcosystemMismatch Nothing) rather than an
uncaught exception.
-}
mkDbWithMalformedEcosystem :: FilePath -> IO ()
mkDbWithMalformedEcosystem path = withConnection path $ \conn -> do
    execute_
        conn
        "CREATE TABLE package_vulnerability_ranges (\
        \  package_name TEXT NOT NULL,\
        \  cve_id TEXT NOT NULL,\
        \  introduced_version TEXT,\
        \  fixed_version TEXT,\
        \  last_affected_version TEXT,\
        \  severity REAL\
        \)"
    execute_ conn "CREATE TABLE meta (key TEXT NOT NULL PRIMARY KEY, value TEXT NOT NULL)"
    execute_ conn "INSERT INTO meta (key, value) VALUES ('ecosystem', X'DEADBEEF')"
    setEpoch conn osvSchemaEpoch

{- | A structurally corrupt artifact: a valid, right-epoch database whose
interior b-tree pages have been overwritten with garbage on disk. Page 1 (the
file header and the schema) is left intact, so the file still opens, reports the
current 'osvSchemaEpoch', and presents a real @package_vulnerability_ranges@
table; only the @PRAGMA quick_check@ integrity walk, reading the wrecked table
b-tree, catches it. This models a tampered or truncated download that parses as
SQLite but is not a sound database. The ranges table is created first, so its
b-tree root is page 2; a handful of rows keep that page a populated leaf.
-}
mkDbWithCorruptPage :: FilePath -> IO ()
mkDbWithCorruptPage path = do
    withConnection path $ \conn -> do
        execute_
            conn
            "CREATE TABLE package_vulnerability_ranges (\
            \  package_name TEXT NOT NULL,\
            \  cve_id TEXT NOT NULL,\
            \  introduced_version TEXT,\
            \  fixed_version TEXT,\
            \  last_affected_version TEXT,\
            \  severity REAL\
            \)"
        execute_ conn "CREATE TABLE meta (key TEXT NOT NULL PRIMARY KEY, value TEXT NOT NULL)"
        execute_ conn "INSERT INTO meta (key, value) VALUES ('ecosystem', 'npm')"
        for_ [1 .. 32 :: Int] $ \i ->
            execute
                conn
                "INSERT INTO package_vulnerability_ranges VALUES (?, 'GHSA-corpus-bulk', '0', '1.0.0', NULL, NULL)"
                (Only (show i :: Text))
        setEpoch conn osvSchemaEpoch
    -- Overwrite page 2 (the ranges b-tree root, at the default 4096-byte page
    -- size) with 0xFF; page 1's header and schema stay readable.
    withBinaryFile path ReadWriteMode $ \h -> do
        hSeek h AbsoluteSeek 4096
        BS.hPut h (BS.replicate 4096 255)

{- | A minimal artifact 'Ecluse.Core.Cve.openCveDb' accepts: the ranges table,
an npm @meta@ row, the current epoch stamp, and one advisory row whose package
name is the given tag with @1.0.0@ as its exact fixed bound, so sync and slot
tests can tell generations apart by which package answers the remediation
probe. The corpus-compiled fixtures stay the schema's conformance authority;
this builder exists for mechanics tests below the app tier.
-}
mkMinimalValidDb :: FilePath -> Text -> IO ()
mkMinimalValidDb path pkg = withConnection path $ \conn -> do
    execute_
        conn
        "CREATE TABLE package_vulnerability_ranges (\
        \  package_name TEXT NOT NULL,\
        \  cve_id TEXT NOT NULL,\
        \  introduced_version TEXT,\
        \  fixed_version TEXT,\
        \  last_affected_version TEXT,\
        \  severity REAL\
        \)"
    execute_ conn "CREATE TABLE meta (key TEXT NOT NULL PRIMARY KEY, value TEXT NOT NULL)"
    execute_ conn "INSERT INTO meta (key, value) VALUES ('ecosystem', 'npm')"
    execute conn "INSERT INTO meta (key, value) VALUES ('source_url', ?)" (Only pkg)
    execute conn "INSERT INTO package_vulnerability_ranges VALUES (?, 'GHSA-minimal', '0', '1.0.0', NULL, NULL)" (Only pkg)
    setEpoch conn osvSchemaEpoch

setEpoch :: Connection -> Int -> IO ()
setEpoch conn epoch = execute_ conn (fromString ("PRAGMA user_version = " <> show epoch))
