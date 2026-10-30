-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The record "Ecluse.Core.Registry.Cargo.Archive" builds after a successful inspection, with its
fields. Importing this module opts out of the public surface's stability promises. It exists so a
spec can read the inventory.
-}
module Ecluse.Core.Registry.Cargo.Archive.Internal (
    CargoArchiveMetadata (..),
) where

import Text.Show (showString, shows, showsPrec)

import Ecluse.Core.Registry.Cargo.Manifest (CargoManifest)

-- | The inspected source bytes, checked against the admitted SHA-256 when one exists.
data CargoArchiveMetadata = CargoArchiveMetadata
    { caManifest :: CargoManifest
    , caInventory :: Set Text
    -- ^ Regular-file paths relative to the admitted single archive root.
    , caSource :: ByteString
    , caRoot :: Text
    , caRetainedBytes :: Int
    , caManifestWorkspace :: Int
    }

instance Show CargoArchiveMetadata where
    showsPrec _ metadata = showString "CargoArchiveMetadata " . shows (caRoot metadata)
