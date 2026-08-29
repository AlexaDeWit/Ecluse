-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The compiled advisory artifact's schema contract.

Écluse Pilot compiles OSV advisory data into a read-only SQLite artifact (@osv.db@)
and publishes it to object storage. The proxy downloads it and queries it locally on
the request path. This module is the one place the writer and the reader agree on what
that artifact looks like. It holds the table-schema epoch that names and stamps the
artifact. It also holds the tables' canonical DDL, the column requirements the reader
verifies at acceptance, and the keys of the @meta@ table.

The artifact is immutable, and every compilation rebuilds it from scratch, so there
are no migrations. There is only a read-compatibility contract between whoever wrote a
file and whoever reads it. The epoch expresses exactly that contract. It moves only
when the shape of the data breaks. The key therefore stays findable, and the stamp
stays checkable across releases of either side.
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

import Data.Universe.Class (Universe (..))
import Data.Universe.Generic (universeGeneric)

{- | The version of the artifact's shape, shared by the Pilot writer and the proxy
reader. Bump it only for a breaking change to the existing shape, never for an additive
one: a reader selects explicit columns.

The epoch names the published artifact ('osvDbFileName'), and SQLite's @user_version@
carries it as the artifact's stamp. A reader rejects an artifact whose stamp does not
match its own compiled-in epoch and keeps its last known-good database.
-}
osvSchemaEpoch :: Int
osvSchemaEpoch = 3

{- | The artifact's file name, and object-storage key, for an ecosystem.

The key is stable per ecosystem, so a reader can poll one known key by ETag. It
embeds only the epoch, so the key changes exactly when a reader could no longer use
the file.

>>> osvDbFileName "npm"
"npm-osv-schema3.db"
-}
osvDbFileName :: Text -> FilePath
osvDbFileName ecosystem =
    toString ecosystem <> "-osv-schema" <> show osvSchemaEpoch <> ".db"

{- | The ranges table's canonical DDL. Declaring the table @STRICT@ turns the column
types from affinity hints into enforced storage types. The reader can then decode rows
without defending against type-confused values. The dedup guard, the unique index over
all five identity columns, is the writer's concern. It is not part of the read
contract, so it lives with the writer.
-}
rangesTableDdl :: Text
rangesTableDdl =
    "CREATE TABLE package_vulnerability_ranges (\
    \  package_name TEXT NOT NULL,\
    \  cve_id TEXT NOT NULL,\
    \  introduced_version TEXT,\
    \  fixed_version TEXT,\
    \  last_affected_version TEXT,\
    \  severity REAL,\
    \  epss_score REAL\
    \) STRICT"

-- | The @meta@ provenance table's canonical DDL, @STRICT@ like 'rangesTableDdl'.
metaTableDdl :: Text
metaTableDdl =
    "CREATE TABLE meta (\
    \  key TEXT NOT NULL PRIMARY KEY,\
    \  value TEXT NOT NULL\
    \) STRICT"

{- | One column the reader requires of an artifact table: its name, its declared type,
and whether the reader's decode relies on @NOT NULL@. Under @STRICT@ the declared type
is the enforced storage type.
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

{- | What the reader verifies before trusting an artifact. Each listed table must be a
real @STRICT@ table carrying at least these columns with these declared types. The
reader tolerates a column beyond these, which keeps an additive schema change
epoch-neutral. The specs mirror 'rangesTableDdl' and 'metaTableDdl' column for column.
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
            , ColumnSpec{colName = "epss_score", colDeclaredType = "REAL", colNotNull = False}
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

{- | A key of the artifact's @meta@ table, which holds one @TEXT@ key\/value row per key
and carries the artifact's provenance.
-}
data MetaKey
    = -- | The Pilot application version that produced the artifact.
      MetaPilotVersion
    | -- | The ecosystem Pilot compiled the artifact for (e.g. @npm@).
      MetaEcosystem
    | -- | When the compilation finished, as an ISO-8601 UTC timestamp.
      MetaBuiltAt
    | -- | The advisory-dump URL Pilot compiled the artifact from.
      MetaSourceUrl
    | -- | The EPSS feed URL Pilot joined the artifact's @epss_score@ column from.
      MetaEpssSourceUrl
    | -- | The number of advisory ranges the artifact holds.
      MetaRowCount
    deriving stock (Eq, Generic, Show)

-- Enumerate every MetaKey from the type itself, so a new key needs no hand-maintained
-- list. Derived from Generic, not a partial Enum/Bounded pair.
instance Universe MetaKey where universe = universeGeneric

-- | The key's stored form in the @meta@ table.
renderMetaKey :: MetaKey -> Text
renderMetaKey = \case
    MetaPilotVersion -> "pilot_version"
    MetaEcosystem -> "ecosystem"
    MetaBuiltAt -> "built_at"
    MetaSourceUrl -> "source_url"
    MetaEpssSourceUrl -> "epss_source_url"
    MetaRowCount -> "row_count"
