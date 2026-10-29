-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Artifact coordinates inside one source snapshot.
Adapters assign keys before admission. Assembly combines them with the snapshot and winning source.
-}
module Ecluse.Core.Package.Entry (
    EntryKey (..),
    validEntryKey,
    validRecordKey,
    distinctKeys,
    AdmittedEntry (..),
) where

import Data.Set qualified as Set

import Ecluse.Core.Snapshot (ContentDigest)

-- | A raw entry's coordinate, independent of its declared artifact filename.
data EntryKey
    = -- | A zero-based position in the original array, before any entry drops.
      ArrayEntry Int
    | -- | A zero-based physical record ordinal, including skipped blank records.
      RecordEntry Int
    | -- | The exact key in the original object, before normalisation.
      ObjectEntry Text
    | -- | A snapshot that contains only one artifact entry.
      SingletonEntry
    deriving stock (Eq, Ord, Show)

-- | Array and record coordinates cannot precede the first source entry.
validEntryKey :: EntryKey -> Bool
validEntryKey = \case
    ArrayEntry position -> position >= 0
    RecordEntry position -> position >= 0
    ObjectEntry _ -> True
    SingletonEntry -> True

-- | A physical row of a record stream, the only coordinate a native line-oriented source assigns.
validRecordKey :: EntryKey -> Bool
validRecordKey = \case
    RecordEntry position -> position >= 0
    _ -> False

-- | No key repeats, so no stored coordinate or identity can select two entries.
distinctKeys :: (Ord key) => [key] -> Bool
distinctKeys keys = Set.size (Set.fromList keys) == length keys

-- | The admitted coordinate and filename, without retaining the artifact's other metadata.
data AdmittedEntry = AdmittedEntry
    { admittedSnapshot :: ContentDigest
    , admittedKey :: EntryKey
    , admittedFilename :: Text
    }
    deriving stock (Eq, Show)
