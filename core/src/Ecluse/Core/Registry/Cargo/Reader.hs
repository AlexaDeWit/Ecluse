-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Bounded LF framing scans complete sparse files and quarantines duplicate native release identities.
module Ecluse.Core.Registry.Cargo.Reader (
    CargoRead (..),
    CargoIndexResult (..),
    readCargoIndexCharged,
    cargoIndexSteps,
    finishCargoSteps,
) where

import Data.Aeson (Value (Null, Object))
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Primitive.SmallArray (smallArrayFromList)

import Ecluse.Core.Package (InvalidEntry, InvalidEntryKind (InvalidVersionManifest), PackageDetails, PackageInfo (..), PackageName, mkInvalidEntry)
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, cargoCached, cargoPacked)
import Ecluse.Core.Registry.Cargo.Discovery (CargoDiscovery)
import Ecluse.Core.Registry.Cargo.Document (PackedIndex (..), cargoDocument)
import Ecluse.Core.Registry.Cargo.Projection (projectCargoRecord)
import Ecluse.Core.Registry.Cargo.Wire (CargoRecordError (..), decodeCargoRecord, recordIdentity)
import Ecluse.Core.Registry.Json.Packed (Packed, docTable)
import Ecluse.Core.Registry.Json.Writer (packTree)
import Ecluse.Core.Registry.JsonStream (Steps (..), isJsonSpace, readStepsWithEnd)
import Ecluse.Core.Registry.Metadata.Fetch.Types (DocumentWalk)
import Ecluse.Core.Registry.Metadata.RecordStream (finishRecordSteps, recordSteps)
import Ecluse.Core.Registry.ServedDocument (stringField)
import Ecluse.Core.Security (Limits (maxNestingDepth), checkArtifactCount, checkVersionCountOf)
import Ecluse.Core.Server.MemoryModel (bufferedChunkCharge, decodedDocumentCharge)
import Ecluse.Core.Strict (strictElements)
import Ecluse.Core.Version (Version, renderVersion)
import Ecluse.Core.Version.Cargo (cargoUniquenessKey, parseCargo)

-- | Selected reads retain one raw version but still inspect every physical record through EOF.
data CargoRead = FullRead | SelectedRead Version
    deriving stock (Eq, Show)

-- | Typed facts and retained rows share the same original source coordinates.
data CargoIndexResult = CargoIndexResult
    { cargoIndexInfo :: PackageInfo
    , cargoIndexDocument :: CachedDoc
    }
    deriving stock (Eq, Show)

data NativeRow = NativeRow !Text !Text !EntryKey !PackageDetails !NativeRecord

data NativeRecord = PackedRecord !Packed | SelectedRecord !Value

data Scan = Scan
    { scanOrdinal :: !Int
    , scanCount :: !Int
    , scanArtifacts :: !Int
    , scanSeen :: !(Map Text Int)
    , scanRows :: ![NativeRow]
    , scanInvalid :: ![InvalidEntry]
    , scanKeptCharge :: !Int
    }

-- | Reserve current-row workspace and any retained selected row before buffering or decoding.
readCargoIndexCharged :: (Int -> IO ()) -> CargoDiscovery -> Limits -> PackageName -> CargoRead -> DocumentWalk CargoIndexResult
readCargoIndexCharged pay discovery limits name mode bound _ =
    readStepsWithEnd id finishCargoSteps bound (cargoIndexSteps pay discovery limits name mode)

-- | Close an incomplete final record with the same empty-chunk signal as the streaming reader.
finishCargoSteps :: (Monad m) => Steps m a -> m (Steps m a)
finishCargoSteps = finishRecordSteps

-- | Frame and project native records while reserving current and retained selected-row workspace.
cargoIndexSteps :: (Monad m) => (Int -> m ()) -> CargoDiscovery -> Limits -> PackageName -> CargoRead -> Steps m CargoIndexResult
cargoIndexSteps pay discovery limits name mode = recordSteps reserve consume (Finished . complete) initial
  where
    initial = Scan 0 0 0 Map.empty [] [] 0
    requestedIdentity = case mode of
        FullRead -> Nothing
        SelectedRead version -> cargoUniquenessKey <$> parseCargo (renderVersion version)
    trackIdentity unique = case mode of
        FullRead -> True
        SelectedRead _ -> Just unique == requestedIdentity
    retainAnother scan = case mode of
        FullRead -> True
        SelectedRead _ -> null (scanRows scan)
    rememberInvalid invalid scan = case mode of
        FullRead -> scan{scanInvalid = invalid : scanInvalid scan}
        SelectedRead _ -> scan
    reserve scan previous fragment = do
        let cost = saturated (toInteger previous + toInteger (decodedDocumentCharge fragment) + toInteger (bufferedChunkCharge (BS.length fragment)))
        pay (saturated (toInteger (scanKeptCharge scan) + toInteger cost))
        pure cost
    saturated = fromInteger . min (toInteger (maxBound :: Int))
    consume !scan rowCharge bytes
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
                    let nativeIdentity = recordIdentity value
                        seen = case nativeIdentity of
                            Just unique | trackIdentity unique -> Map.insertWith (+) unique 1 (scanSeen next)
                            _ -> scanSeen next
                        observed = next{scanSeen = seen}
                    case projectCargoRecord name key discovery value of
                        Left invalid -> Right (rememberInvalid invalid observed)
                        Right details -> do
                            let artifacts = scanArtifacts observed + 1
                            checkArtifactCount limits artifacts
                            let raw = case value of
                                    Object fields -> fromMaybe "" (stringField "vers" fields)
                                    _ -> ""
                                selected = case mode of FullRead -> True; SelectedRead version -> renderVersion version == raw
                                rows = case nativeIdentity of
                                    Just unique
                                        | selected && retainAnother observed ->
                                            let retained = case mode of FullRead -> PackedRecord (packTree [] value); SelectedRead _ -> SelectedRecord value
                                                !row = NativeRow unique raw key details retained
                                             in row : scanRows observed
                                    _ -> scanRows observed
                            pure observed{scanArtifacts = artifacts, scanRows = rows, scanKeptCharge = case mode of SelectedRead _ | null (scanRows observed) && not (null rows) -> rowCharge; _ -> scanKeptCharge observed}
    dropRecord key reason = rememberInvalid (mkInvalidEntry InvalidVersionManifest (show key) Null reason)
    complete scan =
        let (rows, duplicates) = foldl' (finishRow (scanSeen scan)) ([], []) (scanRows scan)
         in CargoIndexResult
                { cargoIndexInfo = PackageInfo name (Map.fromList [(raw, details) | NativeRow _ raw _ details _ <- rows]) Map.empty (strictElements (reverse (scanInvalid scan) <> duplicates))
                , cargoIndexDocument = case mode of
                    FullRead -> fst cargoPacked (PackedIndex (docTable (smallArrayFromList [])) (strictElements [(key, packed) | NativeRow _ _ key _ (PackedRecord packed) <- rows]))
                    SelectedRead _ -> fst cargoCached (cargoDocument [(key, value) | NativeRow _ _ key _ (SelectedRecord value) <- rows])
                }
    finishRow seen (rows, invalid) row@(NativeRow unique _ key _ _)
        | Map.lookup unique seen == Just 1 = (row : rows, invalid)
        | otherwise = (rows, mkInvalidEntry InvalidVersionManifest (show key) Null "duplicate Cargo release identity" : invalid)
