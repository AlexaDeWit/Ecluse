-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Packed Compact Index discovery does not promise admission.
module Ecluse.Core.Registry.RubyGems.Directory (
    GemDirectory (..),
    DirectoryRead (..),
    directoryWeight,
    directorySteps,
    directoryMergeWorkspace,
    mergeDirectories,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8

import Ecluse.Core.Registry.JsonStream (Steps)
import Ecluse.Core.Registry.RubyGems.Directory.Internal (DirectoryRead (..), GemDirectory (..), applyChanges, directoryNameCeiling, directoryStepsWithin)
import Ecluse.Core.Server.MemoryModel (decodedDocumentCharge)

-- | Packed bytes and fixed bookkeeping share the metadata retention pool.
directoryWeight :: GemDirectory -> Int
directoryWeight directory = BS.length (directoryBytes directory) + 256

-- | Repeated rows apply additions and removals before packing the final snapshot. A malformed row is dropped on its own.
directorySteps :: (Monad m) => (Int -> m ()) -> Steps m DirectoryRead
directorySteps = directoryStepsWithin directoryNameCeiling

-- | Expand version tokens only for names present in both source directories.
mergeDirectories :: GemDirectory -> GemDirectory -> GemDirectory
mergeDirectories publicDirectory@(GemDirectory public) privateDirectory@(GemDirectory private)
    | BS.null private = publicDirectory
    | BS.null public = privateDirectory
    | otherwise = GemDirectory (BS.concat (merge (B8.lines public) (B8.lines private)))
  where
    merge [] right = map (<> "\n") right
    merge left [] = map (<> "\n") left
    merge left@(a : as) right@(b : bs) = case compare (nameOf a) (nameOf b) of
        LT -> (a <> "\n") : merge as right
        GT -> (b <> "\n") : merge left bs
        EQ -> (nameOf a <> " " <> applyChanges (versionsOf a) (versionsOf b) <> " \n") : merge as bs
    nameOf = BS.takeWhile (/= 32)
    versionsOf = BS.takeWhile (/= 32) . BS.drop 1 . BS.dropWhile (/= 32)

-- | Reserve only the largest colliding history before expanding either token set.
directoryMergeWorkspace :: GemDirectory -> GemDirectory -> Int
directoryMergeWorkspace (GemDirectory public) (GemDirectory private) = go 0 (B8.lines public) (B8.lines private)
  where
    go peak [] _ = peak
    go peak _ [] = peak
    go peak left@(a : as) right@(b : bs) = case compare (BS.takeWhile (/= 32) a) (BS.takeWhile (/= 32) b) of
        LT -> go peak as right
        GT -> go peak left bs
        EQ -> go (max peak (decodedDocumentCharge a + decodedDocumentCharge b)) as bs
