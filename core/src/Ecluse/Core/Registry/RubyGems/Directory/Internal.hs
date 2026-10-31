-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

{- | The Compact Index directory parser under any gem-name ceiling, and its fixed production ceiling.
Importing this module opts out of the public surface's stability promises. It exists so a spec can
parse under a small ceiling.
-}
module Ecluse.Core.Registry.RubyGems.Directory.Internal (
    GemDirectory (..),
    DirectoryRead (..),
    directoryNameCeiling,
    directoryStepsWithin,
    applyChanges,
) where

import Data.Aeson (Value (Null))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Encoding qualified as TE

import Ecluse.Core.Package (InvalidEntry, InvalidEntryKind (InvalidVersionListing), mkInvalidEntry)
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.JsonStream (Steps (..))
import Ecluse.Core.Registry.Metadata.RecordStream (recordFragmentCharge, recordSteps)
import Ecluse.Core.Registry.RubyGems.Project (projectName)
import Ecluse.Core.Security (LimitError (TooManyVersions), withinCeiling)
import Ecluse.Core.Server.MemoryModel (decodedDocumentCharge, saturatingCharge)
import Ecluse.Core.Strict (strictElements)

-- | Sorted complete rows carry an explicit blank info-file hint.
newtype GemDirectory = GemDirectory {directoryBytes :: ByteString}
    deriving stock (Eq, Show)

-- | A parsed directory and the malformed rows it dropped, in source order.
data DirectoryRead = DirectoryRead {readDirectory :: GemDirectory, readDropped :: [InvalidEntry]}
    deriving stock (Eq, Show)

-- | Distinct gem names one directory may hold. config/default.yaml and the RubyGems protocol reference document it.
directoryNameCeiling :: Int
directoryNameCeiling = 2_000_000

-- The packed rows' weight is charged twice, once more for packing. A dropped row holds 256 bytes until it is logged.
data Scan = Scan
    { scanHeaderDone :: Bool
    , scanOrdinal :: Int
    , scanRetained :: Int
    , scanWorkspace :: Int
    , scanDropped :: [InvalidEntry]
    , scanDroppedCharge :: Int
    , scanRows :: Map ByteString ByteString
    }

-- | Repeated rows apply additions and removals under a gem-name ceiling. The metadata byte cap bounds the rows.
directoryStepsWithin :: (Monad m) => Int -> (Int -> m ()) -> Steps m DirectoryRead
directoryStepsWithin nameCeiling pay = recordSteps reserve consume complete (Scan False 0 0 0 [] 0 Map.empty)
  where
    reserve scan previous fragment = do
        let cost = recordFragmentCharge previous fragment
        pay (saturatingCharge (2 * toInteger (scanRetained scan) + toInteger (scanWorkspace scan) + toInteger (scanDroppedCharge scan) + toInteger cost))
        pure cost
    consume scan _ bytes
        | not (scanHeaderDone scan) = Right next{scanHeaderDone = bytes == "---"}
        | BS.null bytes = Right next
        | otherwise = case decodeRow bytes of
            Nothing -> Right next{scanDropped = droppedRow (scanOrdinal scan) : scanDropped scan, scanDroppedCharge = scanDroppedCharge scan + 256}
            Just (name, changes) -> do
                let previous = Map.lookup name (scanRows scan)
                    versions = maybe (initialVersions changes) (`applyChanges` changes) previous
                    rows = Map.insert (BS.copy name) versions (scanRows scan)
                    weight = scanRetained scan + BS.length name + BS.length versions + 256 - maybe 0 (\old -> BS.length name + BS.length old + 256) previous
                withinCeiling TooManyVersions nameCeiling (Map.size rows)
                Right next{scanRetained = weight, scanWorkspace = max (scanWorkspace scan) (decodedDocumentCharge versions), scanRows = rows}
      where
        next = scan{scanOrdinal = scanOrdinal scan + 1}
    complete scan
        | not (scanHeaderDone scan) = Failed "RubyGems directory has no header delimiter"
        | otherwise = Finished (DirectoryRead (packRows (scanRows scan)) (strictElements (reverse (scanDropped scan))))

packRows :: Map ByteString ByteString -> GemDirectory
packRows rows = GemDirectory (BS.concat [name <> " " <> versions <> " \n" | (name, versions) <- Map.toAscList rows, not (BS.null versions)])

droppedRow :: Int -> InvalidEntry
droppedRow ordinal = mkInvalidEntry InvalidVersionListing (show (RecordEntry ordinal)) Null "malformed RubyGems directory row"

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

-- | Apply comma-separated additions and @-@ removals to a version list, in sorted token order.
applyChanges :: ByteString -> ByteString -> ByteString
applyChanges previous changes = B8.intercalate "," (Set.toAscList (foldl' apply initial (B8.split ',' changes)))
  where
    initial = if BS.null previous then Set.empty else Set.fromList (B8.split ',' previous)
    apply seen token = case BS.uncons token of
        Just (45, removed) -> Set.delete removed seen
        _ -> Set.insert (BS.copy token) seen
