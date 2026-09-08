-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Artifact coordinates inside one source snapshot.
Adapters assign keys before admission. Assembly combines them with the snapshot and winning source.
-}
module Ecluse.Core.Package.Entry (
    EntryKey (..),
    AdmittedEntry (..),
) where

import Ecluse.Core.Snapshot (ContentDigest)

-- | A raw entry's coordinate, independent of its declared artifact filename.
data EntryKey
    = -- | A zero-based position in the original array, before any entry drops.
      ArrayEntry Int
    | -- | The exact key in the original object, before normalisation.
      ObjectEntry Text
    | -- | A snapshot that contains only one artifact entry.
      SingletonEntry
    deriving stock (Eq, Ord, Show)

-- | The admitted coordinate and filename, without retaining the artifact's other metadata.
data AdmittedEntry = AdmittedEntry
    { admittedSnapshot :: ContentDigest
    , admittedKey :: EntryKey
    , admittedFilename :: Text
    }
    deriving stock (Eq, Show)
