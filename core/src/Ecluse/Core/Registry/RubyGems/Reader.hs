-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Complete compact-index scans quarantine conflicting coordinates before version grouping.
module Ecluse.Core.Registry.RubyGems.Reader (
    GemRead (..),
    GemReadResult (..),
    readGemInfoCharged,
    gemInfoSteps,
) where

import Data.Aeson (Value (Null))
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Ecluse.Core.Package (InvalidEntry, InvalidEntryKind (InvalidVersionManifest), PackageInfo (..), PackageName, mkInvalidEntry)
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.JsonStream (Steps (..), readStepsWithEnd)
import Ecluse.Core.Registry.Metadata.Fetch.Types (DocumentWalk)
import Ecluse.Core.Registry.Metadata.RecordStream (finishRecordSteps, recordFragmentCharge, recordSteps)
import Ecluse.Core.Registry.RubyGems.Document (GemDocument (..), GemRow (..))
import Ecluse.Core.Registry.RubyGems.Project (coordinatePlatform, coordinateVersion, unGemPlatform)
import Ecluse.Core.Registry.RubyGems.Projection (projectGemRows)
import Ecluse.Core.Registry.RubyGems.Wire (GemRowError (..), decodeInfoRow, infoRowIdentity, infoRowVersion)
import Ecluse.Core.Security (Limits, checkArtifactCount, checkVersionCountOf)
import Ecluse.Core.Server.MemoryModel (saturatingCharge)
import Ecluse.Core.Strict (strictElements)
import Ecluse.Core.Version (Version, renderVersion)

-- | Selected reads retain all platforms and scan later rows for identity conflicts.
data GemRead
    = -- | Retain every supported artifact in the named package.
      FullRead
    | -- | Retain one exact raw version, without stopping the source scan early.
      SelectedRead Version
    deriving stock (Eq, Show)

-- | Native source rows and policy facts come from the same complete response.
data GemReadResult = GemReadResult
    { gemReadInfo :: PackageInfo
    , gemReadDocument :: GemDocument
    }
    deriving stock (Eq, Show)

data Scan = Scan
    { scanOrdinal :: Int
    , scanHeaderDone :: Bool
    , scanHeader :: [ByteString]
    , scanCount :: Int
    , scanVersions :: Set Text
    , scanSeen :: Map (Text, Text) Int
    , scanBad :: Set Text
    , scanRows :: [GemRow]
    , scanInvalid :: [InvalidEntry]
    , scanKeptCharge :: Int
    }

-- | Reserve row workspace before buffering and apply the complete upstream body ceiling.
readGemInfoCharged :: (Int -> IO ()) -> Limits -> PackageName -> Text -> GemRead -> DocumentWalk GemReadResult
readGemInfoCharged pay limits name base mode bound _ = readStepsWithEnd id finishRecordSteps bound (gemInfoSteps pay limits name base mode)

-- | Selected scans count source positions, while full scans count distinct versions before grouping.
gemInfoSteps :: (Monad m) => (Int -> m ()) -> Limits -> PackageName -> Text -> GemRead -> Steps m GemReadResult
gemInfoSteps pay limits name base mode = recordSteps reserve consume complete initial
  where
    initial = Scan 0 False [] 0 Set.empty Map.empty Set.empty [] [] 0
    selected raw = case mode of FullRead -> True; SelectedRead version -> raw == renderVersion version
    reserve scan previous fragment = do
        let cost = recordFragmentCharge previous fragment
            bookkeeping = 128 * toInteger (Set.size (scanVersions scan) + Map.size (scanSeen scan) + Set.size (scanBad scan))
        pay (saturatingCharge (toInteger (scanKeptCharge scan) + toInteger cost + bookkeeping))
        pure cost
    consume scan charge bytes
        | not (scanHeaderDone scan) = Right scan{scanOrdinal = scanOrdinal scan + 1, scanHeaderDone = bytes == "---", scanHeader = BS.copy bytes : scanHeader scan, scanKeptCharge = addCharge scan charge}
        | BS.null bytes = Right scan{scanOrdinal = scanOrdinal scan + 1}
        | otherwise = do
            let count = scanCount scan + 1
                key = RecordEntry (scanOrdinal scan)
                claimed = infoRowIdentity name bytes
                claimedVersion = renderVersion <$> infoRowVersion bytes
                nativeIdentity = fmap (\coordinate -> (renderVersion (coordinateVersion coordinate), unGemPlatform (coordinatePlatform coordinate))) claimed
                versions = case mode of
                    FullRead -> maybe (scanVersions scan) (`Set.insert` scanVersions scan) claimedVersion
                    SelectedRead _ -> Set.empty
                seen = case nativeIdentity of
                    Just native@(raw, _) | selected raw -> Map.insertWith (+) native 1 (scanSeen scan)
                    _ -> scanSeen scan
                next = scan{scanOrdinal = scanOrdinal scan + 1, scanCount = count, scanVersions = versions, scanSeen = seen}
            checkArtifactCount limits count
            checkVersionCountOf limits (case mode of FullRead -> Set.size versions; SelectedRead _ -> count)
            case decodeInfoRow name key bytes of
                Left fault ->
                    let reason = case fault of GemRowMalformed detail -> detail; GemRowUnsupported detail -> detail
                        bad = case claimedVersion of Just raw | selected raw -> Set.insert raw (scanBad next); _ -> scanBad next
                        retain = case mode of FullRead -> True; SelectedRead _ -> null (scanInvalid next)
                        invalid = if retain then mkInvalidEntry InvalidVersionManifest (show key) Null reason : scanInvalid next else scanInvalid next
                     in Right next{scanBad = bad, scanInvalid = invalid, scanKeptCharge = if retain then addCharge next 256 else scanKeptCharge next}
                Right row
                    | selected (renderVersion (coordinateVersion (grCoordinate row))) -> Right next{scanRows = row : scanRows next, scanKeptCharge = addCharge next charge}
                    | otherwise -> Right next
    addCharge :: Scan -> Int -> Int
    addCharge scan charge = saturatingCharge (toInteger (scanKeptCharge scan) + toInteger charge)
    complete scan
        | not (scanHeaderDone scan) = Failed "RubyGems info response has no header delimiter"
        | otherwise =
            let duplicateVersions = Set.fromList [raw | ((raw, _), count) <- Map.toList (scanSeen scan), count /= 1]
                bad = Set.union (scanBad scan) duplicateVersions
                rows = reverse (filter ((`Set.notMember` bad) . renderVersion . coordinateVersion . grCoordinate) (scanRows scan))
                conflicts = [mkInvalidEntry InvalidVersionManifest raw Null "conflicting or malformed RubyGems version artifacts" | raw <- Set.toList bad]
             in case projectGemRows limits name base rows of
                    Left _ -> Failed "RubyGems info projection failed"
                    Right info -> Finished (GemReadResult info{infoInvalidEntries = strictElements (reverse (scanInvalid scan) <> conflicts)} (GemDocument name (BS.intercalate "\n" (reverse (scanHeader scan)) <> "\n") rows))
