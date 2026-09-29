-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A full read's packed packument: its small top-level members as aeson's tree, and every retained
release packed against the document's table. Each release already holds the source author pointer.
-}
module Ecluse.Core.Registry.Npm.Document (
    PackedPackument (..),
    packumentValue,
    packumentResident,
    tarballHole,
    tarballUrl,
    withTarball,
) where

import Data.Aeson (Value (Object, String))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap

import Ecluse.Core.Registry.Json.Packed (DocTable, Packed, packedResident, packedValue, tableResident)
import Ecluse.Core.Text (textStorageBytes)

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
    Object (KeyMap.insert "versions" (Object (KeyMap.map (packedValue (packumentTable packument)) (packumentVersions packument))) (packumentTop packument))

{- | The heap bytes the table and the releases hold: each release with its map node, its version key's
text and array, and its packed value.
-}
packumentResident :: PackedPackument -> Int
packumentResident packument =
    tableResident (packumentTable packument)
        + sum [104 + textStorageBytes (Key.toText version) + packedResident release | (version, release) <- KeyMap.toList (packumentVersions packument)]

-- | The member path of the string a served release rebases.
tarballHole :: [Text]
tarballHole = ["dist", "tarball"]

-- | The string at 'tarballHole' in a release, when it holds one there.
tarballUrl :: Value -> Maybe Text
tarballUrl = go tarballHole
  where
    go path value = case (path, value) of
        ([], String url) -> Just url
        (name : rest, Object fields) -> KeyMap.lookup (Key.fromText name) fields >>= go rest
        _ -> Nothing

-- | The release with the string at 'tarballHole' replaced, when it holds one there.
withTarball :: Text -> Value -> Value
withTarball url = go tarballHole
  where
    go path value = case (path, value) of
        ([], String _) -> String url
        (name : rest, Object fields)
            | Just member <- KeyMap.lookup (Key.fromText name) fields -> Object (KeyMap.insert (Key.fromText name) (go rest member) fields)
        _ -> value
