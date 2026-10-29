-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Opaque serving documents with ecosystem-specific inject/project pairs.
The pipeline carries source snapshot scope separately and delegates wire access to adapters.
-}
module Ecluse.Core.Registry.CachedDocument (
    CachedDoc,
    weighCachedDoc,
    estimateValueBytes,
    npmCached,
    pypiSimpleCached,

    -- * npm's packed full reads and their renders
    npmPacked,
    npmRendered,

    -- * PyPI's packed full reads and their renders
    pypiPacked,
    pypiRendered,
    cargoCached,
    cargoPacked,
    cargoRendered,
    rubyGemsCached,
) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Scientific (coefficient)
import Data.Text.Internal qualified as Text
import Math.NumberTheory.Logarithms (integerLog2)

import Ecluse.Core.Package (hashValue, renderPackageName)
import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry, RecordEntry))
import Ecluse.Core.Registry.Cargo.Document (CargoDocument, PackedIndex, cargoDocument, cargoRecords, packedIndexDocument, packedIndexResident)
import Ecluse.Core.Registry.Json.Packed (LinePlan, RenderPlan (planMembers), linePlanResident, linePlanValues, planResident, planValue)
import Ecluse.Core.Registry.Npm.Document (PackedPackument (packumentTop), packumentResident, packumentValue)
import Ecluse.Core.Registry.PyPI.Document (PackedSimple, SimpleDocument, packedEnvelope, packedSimpleDocument, packedSimpleResident, simpleDocument, simpleEnvelope, simpleFiles)
import Ecluse.Core.Registry.RubyGems.Document (GemDependency (..), GemDocument (..), GemRow (..))
import Ecluse.Core.Registry.RubyGems.Project (coordinateFilename, coordinateName, coordinatePlatform, coordinateVersion, unGemPlatform)
import Ecluse.Core.Server.MemoryModel (chargeForResident)
import Ecluse.Core.Server.Path (unFilename)
import Ecluse.Core.Text (textStorageBytes)
import Ecluse.Core.Version (renderVersion)

-- | Serving documents held by the pipeline and permitted caches. Derived 'Show' and 'Eq' support debugging and tests, not wire projection.
data CachedDoc
    = CachedNpm Value ~Int64
    | CachedPyPISimple SimpleDocument ~Int64
    | PackedNpm PackedPackument
    | RenderedNpm RenderPlan
    | PackedPyPI PackedSimple
    | RenderedPyPI RenderPlan
    | CachedCargo CargoDocument ~Int64
    | PackedCargo PackedIndex
    | RenderedCargo LinePlan
    | CachedRubyGems GemDocument ~Int64
    deriving stock (Eq, Show)

-- | Estimate compact cache units without measuring retained heap. Packed storage estimates use the consumer's expansion factor.
weighCachedDoc :: CachedDoc -> Int64
weighCachedDoc = \case
    CachedNpm _ charge -> charge
    CachedPyPISimple _ charge -> charge
    PackedNpm packed -> estimateValueBytes (Object (packumentTop packed)) + fromIntegral (chargeForResident (packumentResident packed))
    RenderedNpm plan -> estimateValueBytes (Object (planMembers plan)) + fromIntegral (chargeForResident (planResident plan))
    PackedPyPI document -> estimateValueBytes (Object (packedEnvelope document)) + fromIntegral (chargeForResident (packedSimpleResident document))
    RenderedPyPI plan -> estimateValueBytes (Object (planMembers plan)) + fromIntegral (chargeForResident (planResident plan))
    CachedCargo _ charge -> charge
    PackedCargo document -> fromIntegral (chargeForResident (packedIndexResident document))
    RenderedCargo plan -> fromIntegral (chargeForResident (linePlanResident plan))
    CachedRubyGems _ charge -> charge

-- | Explicit cases require updates for new ecosystems. npm projects packed or rendered forms to trees, refusing missing strings or tables.
npmCached :: (Value -> CachedDoc, CachedDoc -> Maybe Value)
npmCached = (\v -> CachedNpm v (estimateValueBytes v), project)
  where
    project = \case
        CachedNpm v _ -> Just v
        PackedNpm packed -> packumentValue packed
        RenderedNpm plan -> planValue plan
        CachedPyPISimple _ _ -> Nothing
        PackedPyPI _ -> Nothing
        RenderedPyPI _ -> Nothing
        CachedCargo _ _ -> Nothing
        PackedCargo _ -> Nothing
        RenderedCargo _ -> Nothing
        CachedRubyGems _ _ -> Nothing

-- | PyPI's boundary pair, spelled out arm by arm for the same reason as 'npmCached'.
pypiSimpleCached :: (SimpleDocument -> CachedDoc, CachedDoc -> Maybe SimpleDocument)
pypiSimpleCached = (\v -> CachedPyPISimple v (simpleBytes v), project)
  where
    project = \case
        CachedPyPISimple v _ -> Just v
        CachedNpm _ _ -> Nothing
        PackedNpm _ -> Nothing
        RenderedNpm _ -> Nothing
        PackedPyPI document -> packedSimpleDocument document
        RenderedPyPI plan -> fromValue =<< planValue plan
        CachedCargo _ _ -> Nothing
        PackedCargo _ -> Nothing
        RenderedCargo _ -> Nothing
        CachedRubyGems _ _ -> Nothing
    fromValue = \case
        Object fields -> case KeyMap.lookup "files" fields of
            Just (Array files) -> Just (simpleDocument fields (zipWith (\position value -> (ArrayEntry position, value)) [0 ..] (toList files)))
            _ -> Nothing
        _ -> Nothing

-- | npm's packed full read, and its packed form when the document is one.
npmPacked :: (PackedPackument -> CachedDoc, CachedDoc -> Maybe PackedPackument)
npmPacked = (PackedNpm, \case PackedNpm packed -> Just packed; _ -> Nothing)

-- | An assembled npm listing that renders from packed releases, and its plan when the document is one.
npmRendered :: (RenderPlan -> CachedDoc, CachedDoc -> Maybe RenderPlan)
npmRendered = (RenderedNpm, \case RenderedNpm plan -> Just plan; _ -> Nothing)

-- | PyPI's packed full-read document, available without materialising its files.
pypiPacked :: (PackedSimple -> CachedDoc, CachedDoc -> Maybe PackedSimple)
pypiPacked = (PackedPyPI, \case PackedPyPI document -> Just document; _ -> Nothing)

-- | An assembled PyPI listing that renders directly from packed files and source tables.
pypiRendered :: (RenderPlan -> CachedDoc, CachedDoc -> Maybe RenderPlan)
pypiRendered = (RenderedPyPI, \case RenderedPyPI plan -> Just plan; _ -> Nothing)

-- | Cargo's native source records, preserving physical source coordinates on tree projection.
cargoCached :: (CargoDocument -> CachedDoc, CachedDoc -> Maybe CargoDocument)
cargoCached = (\document -> CachedCargo document (cargoBytes document), project)
  where
    project = \case
        CachedCargo document _ -> Just document
        PackedCargo document -> packedIndexDocument document
        RenderedCargo plan -> cargoDocument . zipWith (\position value -> (RecordEntry position, value)) [0 ..] <$> linePlanValues plan
        CachedNpm _ _ -> Nothing
        CachedPyPISimple _ _ -> Nothing
        PackedNpm _ -> Nothing
        RenderedNpm _ -> Nothing
        PackedPyPI _ -> Nothing
        RenderedPyPI _ -> Nothing
        CachedRubyGems _ _ -> Nothing

-- | Cargo's packed full source remains available without materialising native records.
cargoPacked :: (PackedIndex -> CachedDoc, CachedDoc -> Maybe PackedIndex)
cargoPacked = (PackedCargo, \case PackedCargo document -> Just document; _ -> Nothing)

-- | A native output plan keeps ordered records over their separate source tables.
cargoRendered :: (LinePlan -> CachedDoc, CachedDoc -> Maybe LinePlan)
cargoRendered = (RenderedCargo, \case RenderedCargo plan -> Just plan; _ -> Nothing)

cargoBytes :: CargoDocument -> Int64
cargoBytes document = sum [64 + estimateValueBytes value | (_, value) <- cargoRecords document]

simpleBytes :: SimpleDocument -> Int64
simpleBytes document = estimateValueBytes (Object (simpleEnvelope document)) + 12 + sum [32 + estimateValueBytes value | (_, value) <- simpleFiles document]

-- | Estimate accounting bytes without encoding. This is neither measured heap nor exact JSON length.
estimateValueBytes :: Value -> Int64
estimateValueBytes = \case
    Object fields -> 2 + sum [4 + textBytes (Key.toText key) + estimateValueBytes value | (key, value) <- KeyMap.toList fields]
    Array items -> 2 + sum [1 + estimateValueBytes value | value <- toList items]
    String value -> 2 + textBytes value
    Number number -> 24 + integerBytes (coefficient number)
    Bool _ -> 5
    Null -> 4

textBytes :: Text -> Int64
textBytes (Text.Text _ _ len) = fromIntegral len

integerBytes :: Integer -> Int64
integerBytes value
    | value == 0 = 8
    | otherwise = 8 * (1 + fromIntegral (integerLog2 (abs value) `div` 64))

-- | RubyGems retains native artifact rows without reconstructing installation metadata from policy facts.
rubyGemsCached :: (GemDocument -> CachedDoc, CachedDoc -> Maybe GemDocument)
rubyGemsCached = (\document -> CachedRubyGems document (gemBytes document), \case CachedRubyGems document _ -> Just document; _ -> Nothing)

gemBytes :: GemDocument -> Int64
gemBytes document = 64 + nativeTextBytes (renderPackageName (gemName document)) + fromIntegral (BS.length (gemHeader document)) + sum (map rowBytes (gemRows document))
  where
    rowBytes row =
        let coordinate = grCoordinate row
            strings = [renderPackageName (coordinateName coordinate), renderVersion (coordinateVersion coordinate), unGemPlatform (coordinatePlatform coordinate), unFilename (coordinateFilename coordinate), hashValue (grChecksum row)]
            requirementBytes values = sum [32 + nativeTextBytes clause | clause <- toList values]
         in 256
                + sum (map nativeTextBytes strings)
                + fromIntegral (BS.length (grSource row))
                + requirementBytes (grRequiredRuby row)
                + requirementBytes (grRequiredRubyGems row)
                + sum [64 + nativeTextBytes (renderPackageName (gdName dependency)) + requirementBytes (gdRequirement dependency) | dependency <- grDependencies row]
                + sum [64 + nativeTextBytes key + nativeTextBytes value | (key, value) <- Map.toList (grExtras row)]

nativeTextBytes :: Text -> Int64
nativeTextBytes = fromIntegral . textStorageBytes
