-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Packed Compact Index discovery does not promise admission.
module Ecluse.Core.Registry.RubyGems.Directory (
    GemDirectory (..),
    directoryWeight,
    directoryNameCeiling,
    directorySteps,
    directoryStepsWithin,
    directoryMergeWorkspace,
    mergeDirectories,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Encoding qualified as TE

import Ecluse.Core.Registry.JsonStream (Steps (..))
import Ecluse.Core.Registry.Metadata.RecordStream (recordFragmentCharge, recordSteps)
import Ecluse.Core.Registry.RubyGems.Project (projectName)
import Ecluse.Core.Security (LimitError (TooManyVersions), withinCeiling)
import Ecluse.Core.Server.MemoryModel (decodedDocumentCharge, saturatingCharge)

-- | Sorted complete rows carry an explicit blank info-file hint.
newtype GemDirectory = GemDirectory {directoryBytes :: ByteString}
    deriving stock (Eq, Show)

-- | Packed bytes and fixed bookkeeping share the metadata retention pool.
directoryWeight :: GemDirectory -> Int
directoryWeight directory = BS.length (directoryBytes directory) + 256

-- | Distinct gem names one directory may hold. config/default.yaml and the RubyGems protocol reference document it.
directoryNameCeiling :: Int
directoryNameCeiling = 2_000_000

data Scan = Scan Bool Bool Int Int (Map ByteString ByteString)

-- | Repeated rows apply additions and removals before packing the final snapshot.
directorySteps :: (Monad m) => (Int -> m ()) -> Steps m GemDirectory
directorySteps = directoryStepsWithin directoryNameCeiling

-- | 'directorySteps' under another gem-name ceiling. The metadata byte cap bounds the rows.
directoryStepsWithin :: (Monad m) => Int -> (Int -> m ()) -> Steps m GemDirectory
directoryStepsWithin nameCeiling pay = recordSteps reserve consume complete (Scan False False 0 0 Map.empty)
  where
    reserve (Scan _ _ retained workspace _) previous fragment = do
        let cost = recordFragmentCharge previous fragment
        pay (saturatingCharge (2 * toInteger retained + toInteger workspace + toInteger cost))
        pure cost
    consume (Scan header bad retained workspace rows) _ bytes
        | not header = Right (Scan (bytes == "---") bad retained workspace rows)
        | BS.null bytes = Right (Scan header bad retained workspace rows)
        | otherwise = case decodeRow bytes of
            Nothing -> Right (Scan header True retained workspace rows)
            Just (name, changes) -> do
                let previous = Map.lookup name rows
                    versions = maybe (initialVersions changes) (`applyChanges` changes) previous
                    next = Map.insert (BS.copy name) versions rows
                    weight = retained + BS.length name + BS.length versions + 256 - maybe 0 (\old -> BS.length name + BS.length old + 256) previous
                withinCeiling TooManyVersions nameCeiling (Map.size next)
                Right (Scan header bad weight (max workspace (decodedDocumentCharge versions)) next)
    complete (Scan header bad _ _ rows)
        | not header || bad = Failed "RubyGems directory is malformed or has no header delimiter"
        | otherwise = Finished (GemDirectory (BS.concat [name <> " " <> versions <> " \n" | (name, versions) <- Map.toAscList rows, not (BS.null versions)]))

decodeRow :: ByteString -> Maybe (ByteString, ByteString)
decodeRow bytes = do
    firstSpace <- BS.elemIndex 32 bytes
    let name = BS.take firstSpace bytes
        rest = BS.drop (firstSpace + 1) bytes
    lastSpace <- BS.elemIndex 32 rest
    let versions = BS.take lastSpace rest
        hint = BS.drop (lastSpace + 1) rest
    text <- rightToMaybe (TE.decodeUtf8' name)
    _ <- rightToMaybe (projectName text)
    guard (BS.null hint || (BS.length hint == 32 && BS.all hexadecimal hint))
    guard (not (BS.null versions) && all validToken (B8.split ',' versions))
    pure (name, versions)
  where
    hexadecimal c = (c >= 48 && c <= 57) || (c >= 97 && c <= 102) || (c >= 65 && c <= 70)
    validToken token = not (BS.null token) && BS.all (\c -> (c >= 48 && c <= 57) || (c >= 65 && c <= 90) || (c >= 97 && c <= 122) || c == 45 || c == 46 || c == 95) token && token /= "-"

initialVersions :: ByteString -> ByteString
initialVersions changes
    | BS.take 1 changes == "-" || not (BS.null (snd (BS.breakSubstring ",-" changes))) = applyChanges "" changes
    | otherwise = BS.copy changes

applyChanges :: ByteString -> ByteString -> ByteString
applyChanges previous changes = B8.intercalate "," (Set.toAscList (foldl' apply initial (B8.split ',' changes)))
  where
    initial = if BS.null previous then Set.empty else Set.fromList (B8.split ',' previous)
    apply seen token = case BS.uncons token of
        Just (45, removed) -> Set.delete removed seen
        _ -> Set.insert (BS.copy token) seen

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
