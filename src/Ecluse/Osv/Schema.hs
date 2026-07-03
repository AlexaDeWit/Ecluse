{- | The compiled advisory artifact's schema contract.

Écluse Pilot compiles OSV advisory data into a read-only SQLite artifact
(@osv.db@) and publishes it to object storage; the proxy downloads it and
queries it locally on the request path. This module is the one place the
writer and the reader agree on what that artifact looks like: the table-schema
epoch that names and stamps it, and the keys of its @meta@ table.

The artifact is immutable and rebuilt from scratch on every compilation, so
there are no migrations, only a read-compatibility contract between whoever
wrote a file and whoever reads it. The epoch expresses exactly that contract:
it moves only when the shape of the data breaks, so the key stays findable
and the stamp stays checkable across releases of either side.
-}
module Ecluse.Osv.Schema (
    -- * The table-schema epoch
    osvSchemaEpoch,
    osvDbFileName,

    -- * The @meta@ table
    MetaKey (..),
    renderMetaKey,
) where

{- | The table-schema epoch: the version of the artifact's shape, shared by
the Pilot writer and the proxy reader.

Bump it only for a breaking change to the existing shape (a column rename, a
semantic change, a key change). Additive changes (a new column, a new table)
must not bump it: readers select explicit columns, so additions are invisible
to them, and whether an optional column actually carries data is advertised
through the @meta@ table instead ('MetaSeverityPopulated',
'MetaEpssPopulated').

The epoch names the published artifact ('osvDbFileName') and is stamped into
it as SQLite's @user_version@; a reader must reject an artifact whose stamp
does not match its own compiled-in epoch and keep its last known-good
database.
-}
osvSchemaEpoch :: Int
osvSchemaEpoch = 1

{- | The artifact's file name, and object-storage key, for an ecosystem.

The key is stable per ecosystem, so a reader can poll one known key by ETag,
and embeds only the epoch, so the key changes exactly when a reader could no
longer use the file.

>>> osvDbFileName "npm"
"npm-osv-schema1.db"
-}
osvDbFileName :: Text -> FilePath
osvDbFileName ecosystem =
    toString ecosystem <> "-osv-schema" <> show osvSchemaEpoch <> ".db"

{- | A key of the artifact's @meta@ table (one @TEXT@ key\/value row per key).

The table carries the artifact's provenance (which build produced it, from
what source, and when) and its capabilities (which optional columns this build
populates), so a reader can tell "this build does not emit the column" apart
from "no data known for this package".
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
    | -- | @1@ when this build populates @severity@; @0@ until then.
      MetaSeverityPopulated
    | -- | @1@ when this build populates @epss_score@; @0@ until then.
      MetaEpssPopulated
    deriving stock (Bounded, Enum, Eq, Show)

-- | The key's stored form in the @meta@ table.
renderMetaKey :: MetaKey -> Text
renderMetaKey = \case
    MetaPilotVersion -> "pilot_version"
    MetaEcosystem -> "ecosystem"
    MetaBuiltAt -> "built_at"
    MetaSourceUrl -> "source_url"
    MetaRowCount -> "row_count"
    MetaSeverityPopulated -> "severity_populated"
    MetaEpssPopulated -> "epss_populated"
