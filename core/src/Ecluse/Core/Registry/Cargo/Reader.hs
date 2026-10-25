-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Bounded LF framing scans complete sparse files and quarantines duplicate native release identities.
module Ecluse.Core.Registry.Cargo.Reader (
    CargoRead (..),
    CargoIndexResult (..),
    readCargoIndex,
) where

import Data.Aeson (Value (Null, Object))
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Primitive.SmallArray (smallArrayFromList)

import Ecluse.Core.Package (InvalidEntry, InvalidEntryKind (InvalidVersionManifest), PackageDetails, PackageInfo (..), PackageName, mkInvalidEntry)
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.Cargo.Discovery (CargoDiscovery)
import Ecluse.Core.Registry.Cargo.Document (PackedIndex (..))
import Ecluse.Core.Registry.Cargo.Projection (projectCargoRecord)
import Ecluse.Core.Registry.Cargo.Wire (CargoRecordError (..), decodeCargoRecord, recordIdentity)
import Ecluse.Core.Registry.Json.Packed (Packed, docTable)
import Ecluse.Core.Registry.Json.Writer (packTree)
import Ecluse.Core.Registry.JsonStream (Steps (..), isJsonSpace, readStepsWithEnd)
import Ecluse.Core.Registry.Metadata.Fetch.Types (DocumentWalk)
import Ecluse.Core.Registry.ServedDocument (stringField)
import Ecluse.Core.Security (Limits (maxNestingDepth), checkArtifactCount, checkVersionCountOf)
import Ecluse.Core.Strict (strictElements)
import Ecluse.Core.Version (Version, renderVersion)

-- | Selected reads retain one raw version but still inspect every physical record through EOF.
data CargoRead = FullRead | SelectedRead Version
    deriving stock (Eq, Show)

-- | Typed facts and retained rows share the same original source coordinates.
data CargoIndexResult = CargoIndexResult
    { cargoIndexInfo :: PackageInfo
    , cargoIndexDocument :: PackedIndex
    }
    deriving stock (Eq, Show)

data NativeRow = NativeRow !Text !Text !EntryKey !PackageDetails !Packed

data Scan = Scan
    { scanOrdinal :: !Int
    , scanCount :: !Int
    , scanArtifacts :: !Int
    , scanSeen :: !(Map Text Int)
    , scanRows :: ![NativeRow]
    , scanInvalid :: ![InvalidEntry]
    }

-- | Hold at most one undecoded record and preserve body-limit failures as whole-read errors.
readCargoIndex :: CargoDiscovery -> Limits -> PackageName -> CargoRead -> DocumentWalk CargoIndexResult
readCargoIndex discovery limits name mode bound _ = readStepsWithEnd (pure . runIdentity) end bound (await initial [])
  where
    initial = Scan 0 0 0 Map.empty [] []
    end = \case
        NeedData next -> pure (runIdentity (next BS.empty))
        step -> pure step
    await !scan pieces = NeedData (Identity . receive scan pieces)
    receive scan pieces chunk
        | BS.null chunk = case pieces of
            [] -> Finished (complete scan)
            _ -> either Refused (Finished . complete) (consume scan (BS.concat (reverse pieces)))
        | otherwise = splitLines scan pieces chunk
    splitLines !scan pieces chunk = case BS.elemIndex 10 chunk of
        Nothing -> await scan (chunk : pieces)
        Just at ->
            let (line, rest) = BS.splitAt at chunk
                record = case pieces of [] -> line; _ -> BS.concat (reverse (line : pieces))
             in case consume scan record of
                    Left fault -> Refused fault
                    Right next
                        | BS.length rest == 1 -> await next []
                        | otherwise -> splitLines next [] (BS.drop 1 rest)
    consume !scan bytes
        | BS.all isJsonSpace bytes = Right scan{scanOrdinal = scanOrdinal scan + 1}
        | otherwise = do
            let count = scanCount scan + 1
                key = RecordEntry (scanOrdinal scan)
                next = scan{scanOrdinal = scanOrdinal scan + 1, scanCount = count}
            checkVersionCountOf limits count
            case decodeCargoRecord (maxNestingDepth limits) bytes of
                Left (CargoRecordBound fault) -> Left fault
                Left (CargoRecordMalformed reason) -> Right (dropRecord key reason next)
                Right value -> do
                    let identity = recordIdentity value
                        seen = maybe (scanSeen next) (\unique -> Map.insertWith (+) unique 1 (scanSeen next)) identity
                        observed = next{scanSeen = seen}
                    case projectCargoRecord name key discovery value of
                        Left invalid -> Right observed{scanInvalid = invalid : scanInvalid observed}
                        Right details -> do
                            let artifacts = scanArtifacts observed + 1
                            checkArtifactCount limits artifacts
                            let raw = case value of
                                    Object fields -> fromMaybe "" (stringField "vers" fields)
                                    _ -> ""
                                selected = case mode of FullRead -> True; SelectedRead version -> renderVersion version == raw
                                rows = case identity of
                                    Just unique
                                        | selected ->
                                            let !row = NativeRow unique raw key details (packTree [] value)
                                             in row : scanRows observed
                                    _ -> scanRows observed
                            pure observed{scanArtifacts = artifacts, scanRows = rows}
    dropRecord key reason scan = scan{scanInvalid = mkInvalidEntry InvalidVersionManifest (show key) Null reason : scanInvalid scan}
    complete scan =
        let (rows, duplicates) = foldl' (\acc row -> finishRow (scanSeen scan) row acc) ([], []) (scanRows scan)
         in CargoIndexResult
                { cargoIndexInfo = PackageInfo name (Map.fromList [(raw, details) | NativeRow _ raw _ details _ <- rows]) Map.empty (strictElements (reverse (scanInvalid scan) <> duplicates))
                , cargoIndexDocument = PackedIndex (docTable (smallArrayFromList [])) (strictElements [(key, packed) | NativeRow _ _ key _ packed <- rows])
                }
    finishRow seen row@(NativeRow unique _ key _ _) (rows, invalid)
        | Map.lookup unique seen == Just 1 = (row : rows, invalid)
        | otherwise = (rows, mkInvalidEntry InvalidVersionManifest (show key) Null "duplicate Cargo release identity" : invalid)
