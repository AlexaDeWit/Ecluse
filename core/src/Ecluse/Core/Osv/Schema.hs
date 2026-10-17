-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The advisory artifact contract shared by Pilot and its consumers.
The epoch covers incompatible table shapes and incompatible meanings of stored values.
Artifacts are rebuilt rather than migrated. Compatible additions preserve the epoch.
-}
module Ecluse.Core.Osv.Schema (
    -- * The artifact epoch
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
    EpssRequirement (..),
    EpssStatus (..),
    renderEpssStatus,
    EpssEvidence (..),
    decodeEpssEvidence,
) where

import Data.Universe.Class (Universe (..))
import Data.Universe.Generic (universeGeneric)

-- | Advance for incompatible shape or stored-value semantics, not compatible additions.
osvSchemaEpoch :: Int
osvSchemaEpoch = 5

-- | Keep a stable object key until the artifact's read contract changes.
osvDbFileName :: Text -> FilePath
osvDbFileName ecosystem =
    toString ecosystem <> "-osv-schema" <> show osvSchemaEpoch <> ".db"

-- | The ranges table's canonical strict DDL. The writer owns indexes outside this read contract.
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

-- | A required column's enforced storage type and nullability.
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

-- | Required strict tables and columns. Additional columns preserve reader compatibility.
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

-- | A key of the artifact's @meta@ table, one @TEXT@ row per key, which carries the artifact's provenance.
data MetaKey
    = -- | The Pilot application version that produced the artifact.
      MetaPilotVersion
    | -- | The ecosystem Pilot compiled the artifact for (e.g. @npm@).
      MetaEcosystem
    | -- | When the compilation finished, as an ISO-8601 UTC timestamp.
      MetaBuiltAt
    | -- | The advisory source's host:port identity. Older artifacts can carry a complete URL.
      MetaSourceUrl
    | -- | The EPSS source's host:port identity. Older artifacts can carry a complete URL.
      MetaEpssSourceUrl
    | -- | The advisory source's origin, then its path or the path's SHA-256 ('Ecluse.Core.Osv.Provenance.renderRecordedSource').
      MetaOsvSource
    | -- | The @Last-Modified@ the advisory export answered the fetch with.
      MetaOsvLastModified
    | -- | The newest @modified@ of the advisory records the artifact was compiled from.
      MetaOsvNewestModified
    | -- | The EPSS feed in the same form as 'MetaOsvSource'.
      MetaEpssSource
    | -- | The @Last-Modified@ the EPSS feed answered the fetch with.
      MetaEpssLastModified
    | -- | The @score_date@ the EPSS feed declares for its scores.
      MetaEpssScoreDate
    | -- | The scoring model the EPSS feed declares.
      MetaEpssModelVersion
    | -- | The whole-feed enrichment outcome, an 'EpssStatus'.
      MetaEpssStatus
    | -- | The number of advisory ranges the artifact holds.
      MetaRowCount
    | -- | The SHA-256 of the advisory export's whole response body.
      MetaOsvDigest
    | -- | The SHA-256 of the EPSS feed's whole response body, when the feed arrived.
      MetaEpssDigest
    | -- | The number of advisory ranges that carry an EPSS score.
      MetaScoredRowCount
    | -- | The fixed-width nonce that gives each publication attempt its own bytes.
      MetaPublicationNonce
    deriving stock (Eq, Generic, Show)

instance Universe MetaKey where universe = universeGeneric

-- | The key's stored form in the @meta@ table.
renderMetaKey :: MetaKey -> Text
renderMetaKey = \case
    MetaPilotVersion -> "pilot_version"
    MetaEcosystem -> "ecosystem"
    MetaBuiltAt -> "built_at"
    MetaSourceUrl -> "source_url"
    MetaEpssSourceUrl -> "epss_source_url"
    MetaOsvSource -> "osv_source"
    MetaOsvLastModified -> "osv_last_modified"
    MetaOsvNewestModified -> "osv_newest_modified"
    MetaEpssSource -> "epss_source"
    MetaEpssLastModified -> "epss_last_modified"
    MetaEpssScoreDate -> "epss_score_date"
    MetaEpssModelVersion -> "epss_model_version"
    MetaEpssStatus -> "epss_status"
    MetaRowCount -> "row_count"
    MetaOsvDigest -> "osv_digest"
    MetaEpssDigest -> "epss_digest"
    MetaScoredRowCount -> "scored_row_count"
    MetaPublicationNonce -> "publication_nonce"

-- | Whether an ecosystem's policy depends on EPSS. Where it does, Pilot publishes nothing without enrichment and a reader refuses its absence.
data EpssRequirement = EpssOptional | EpssRequired
    deriving stock (Eq, Show)

-- | The whole-feed enrichment outcome Pilot records under 'MetaEpssStatus'.
data EpssStatus
    = -- | The feed arrived and its scores joined, whether or not any advisory matched one.
      EnrichmentAvailable
    | -- | The feed failed where the ecosystem does not require it, so no score joined.
      EnrichmentUnavailable
    deriving stock (Eq, Show)

-- | The status's stored form.
renderEpssStatus :: EpssStatus -> Text
renderEpssStatus = \case
    EnrichmentAvailable -> "available"
    EnrichmentUnavailable -> "unavailable"

-- | Whether metadata establishes successful feed enrichment, independent of individual scores.
data EpssEvidence = EpssAvailable | EpssNotEstablished
    deriving stock (Eq, Show)

-- | Only the exact stored form of 'EnrichmentAvailable' establishes enrichment.
decodeEpssEvidence :: Maybe Text -> EpssEvidence
decodeEpssEvidence stored
    | stored == Just (renderEpssStatus EnrichmentAvailable) = EpssAvailable
    | otherwise = EpssNotEstablished
