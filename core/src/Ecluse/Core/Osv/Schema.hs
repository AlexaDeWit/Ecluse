{- | The compiled advisory artifact's schema contract.

Écluse Pilot compiles OSV advisory data into a read-only SQLite artifact
(@osv.db@) and publishes it to object storage; the proxy downloads it and
queries it locally on the request path. This module is the one place the
writer and the reader agree on what that artifact looks like: the table-schema
epoch that names and stamps it, the tables' canonical DDL and the column
requirements the reader verifies at acceptance, and the keys of its @meta@
table.

The artifact is immutable and rebuilt from scratch on every compilation, so
there are no migrations, only a read-compatibility contract between whoever
wrote a file and whoever reads it. The epoch expresses exactly that contract:
it moves only when the shape of the data breaks, so the key stays findable
and the stamp stays checkable across releases of either side.
-}
module Ecluse.Core.Osv.Schema (
    -- * The table-schema epoch
    osvSchemaEpoch,
    osvDbFileName,

    -- * The tables
    rangesTableDdl,
    metaTableDdl,
    ColumnSpec (..),
    TableSpec (..),
    osvTableSpecs,

    -- * The @meta@ table
    MetaKey (..),
    renderMetaKey,
) where

{- | The table-schema epoch: the version of the artifact's shape, shared by
the Pilot writer and the proxy reader.

Bump it only for a breaking change to the existing shape (a column rename, a
semantic change, a key change). Additive changes (a new column, a new table)
must not bump it: readers select explicit columns, so additions are invisible
to them. A column exists exactly when the build populates it, so a reader
learns what data an artifact offers from the schema itself.

The epoch names the published artifact ('osvDbFileName') and is stamped into
it as SQLite's @user_version@; a reader must reject an artifact whose stamp
does not match its own compiled-in epoch and keep its last known-good
database.

Epoch 2 widened the affected-set model: the ranges table gained a
@last_affected_version@ column (an inclusive upper bound, distinct from the
exclusive @fixed_version@), exact enumerated versions are stored as points, and
@severity@ became a numeric @REAL@ CVSS base score. A reader compiled for epoch 2
requires those columns, so an epoch-1 artifact is rejected rather than read.

Epoch 3 made the stored value types part of the contract: both tables are
declared @STRICT@, so SQLite enforces each column's declared type at write time
and @PRAGMA quick_check@ verifies the stored values against it, and the reader
accepts an artifact only after confirming that declaration ('osvTableSpecs').
Every value the reader decodes is therefore type-sound by construction. The
ranges table's composite primary key became an equivalent unique index, since
@STRICT@ makes primary-key columns implicitly @NOT NULL@ and the bound columns
are legitimately absent.
-}
osvSchemaEpoch :: Int
osvSchemaEpoch = 3

{- | The artifact's file name, and object-storage key, for an ecosystem.

The key is stable per ecosystem, so a reader can poll one known key by ETag,
and embeds only the epoch, so the key changes exactly when a reader could no
longer use the file.

>>> osvDbFileName "npm"
"npm-osv-schema2.db"
-}
osvDbFileName :: Text -> FilePath
osvDbFileName ecosystem =
    toString ecosystem <> "-osv-schema" <> show osvSchemaEpoch <> ".db"

{- | The ranges table's canonical DDL. @STRICT@ turns the declared column types
from affinity hints into enforced storage types, which is what lets the reader
decode rows without defending against type-confused values. The dedup guard
(the unique index over all five identity columns) is the writer's concern, not
part of the read contract, so it lives with the writer.
-}
rangesTableDdl :: Text
rangesTableDdl =
    "CREATE TABLE package_vulnerability_ranges (\
    \  package_name TEXT NOT NULL,\
    \  cve_id TEXT NOT NULL,\
    \  introduced_version TEXT,\
    \  fixed_version TEXT,\
    \  last_affected_version TEXT,\
    \  severity REAL\
    \) STRICT"

-- | The @meta@ provenance table's canonical DDL; @STRICT@ as 'rangesTableDdl'.
metaTableDdl :: Text
metaTableDdl =
    "CREATE TABLE meta (\
    \  key TEXT NOT NULL PRIMARY KEY,\
    \  value TEXT NOT NULL\
    \) STRICT"

{- | One column the reader requires of an artifact table: its name, its declared
type (which @STRICT@ makes the enforced storage type), and whether the reader's
decode relies on the column being @NOT NULL@.
-}
data ColumnSpec = ColumnSpec
    { colName :: Text
    , colDeclaredType :: Text
    , colNotNull :: Bool
    }
    deriving stock (Eq, Show)

-- | A table the reader requires, with the columns its queries decode.
data TableSpec = TableSpec
    { tableName :: Text
    , tableColumns :: [ColumnSpec]
    }
    deriving stock (Eq, Show)

{- | What the reader verifies before trusting an artifact: each listed table
must be a real @STRICT@ table carrying at least these columns with these
declared types. Columns beyond these are tolerated, which is what keeps
additive schema changes epoch-neutral; the specs mirror 'rangesTableDdl' and
'metaTableDdl' column for column.
-}
osvTableSpecs :: [TableSpec]
osvTableSpecs =
    [ TableSpec
        { tableName = "package_vulnerability_ranges"
        , tableColumns =
            [ ColumnSpec{colName = "package_name", colDeclaredType = "TEXT", colNotNull = True}
            , ColumnSpec{colName = "cve_id", colDeclaredType = "TEXT", colNotNull = True}
            , ColumnSpec{colName = "introduced_version", colDeclaredType = "TEXT", colNotNull = False}
            , ColumnSpec{colName = "fixed_version", colDeclaredType = "TEXT", colNotNull = False}
            , ColumnSpec{colName = "last_affected_version", colDeclaredType = "TEXT", colNotNull = False}
            , ColumnSpec{colName = "severity", colDeclaredType = "REAL", colNotNull = False}
            ]
        }
    , TableSpec
        { tableName = "meta"
        , tableColumns =
            [ ColumnSpec{colName = "key", colDeclaredType = "TEXT", colNotNull = True}
            , ColumnSpec{colName = "value", colDeclaredType = "TEXT", colNotNull = True}
            ]
        }
    ]

{- | A key of the artifact's @meta@ table (one @TEXT@ key\/value row per key).

The table carries the artifact's provenance: which build produced it, from
what source, and when.
-}
data MetaKey
    = -- | The Pilot application version that produced the artifact.
      MetaPilotVersion
    | -- | The ecosystem the artifact was compiled for (e.g. @npm@).
      MetaEcosystem
    | -- | When the compilation finished, as an ISO-8601 UTC timestamp.
      MetaBuiltAt
    | -- | The advisory-dump URL the artifact was compiled from.
      MetaSourceUrl
    | -- | The number of advisory ranges the artifact holds.
      MetaRowCount
    deriving stock (Bounded, Enum, Eq, Show)

-- | The key's stored form in the @meta@ table.
renderMetaKey :: MetaKey -> Text
renderMetaKey = \case
    MetaPilotVersion -> "pilot_version"
    MetaEcosystem -> "ecosystem"
    MetaBuiltAt -> "built_at"
    MetaSourceUrl -> "source_url"
    MetaRowCount -> "row_count"
