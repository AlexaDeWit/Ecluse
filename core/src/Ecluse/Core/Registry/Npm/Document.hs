-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A full read's packed packument: its small top-level members as aeson's tree, and every retained
release packed against the document's table. Each release already holds the source author pointer.
-}
module Ecluse.Core.Registry.Npm.Document (
    PackedPackument (..),
    packumentValue,
    packumentBytes,
    tarballHole,
) where

import Data.Aeson (Value (Object))
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap

import Ecluse.Core.Registry.Json.Packed (DocTable, Packed, packedBytes, packedValue, tableBytes)

-- | The top-level members other than @versions@, the table, and each release by version key.
data PackedPackument = PackedPackument
    { packumentTop :: KeyMap Value
    , packumentTable :: DocTable
    , packumentVersions :: KeyMap Packed
    }
    deriving stock (Eq, Show)

-- | The packument as aeson's tree, as the read would have built it.
packumentValue :: PackedPackument -> Value
packumentValue packument =
    Object (KeyMap.insert "versions" (Object (KeyMap.map (\packed -> packedValue (packumentTable packument) packed Nothing) (packumentVersions packument))) (packumentTop packument))

-- | The bytes the packed releases and the table hold.
packumentBytes :: PackedPackument -> Int
packumentBytes packument = tableBytes (packumentTable packument) + sum (map packedBytes (KeyMap.elems (packumentVersions packument)))

-- | The member path of the string a served release rebases.
tarballHole :: [Text]
tarballHole = ["dist", "tarball"]
