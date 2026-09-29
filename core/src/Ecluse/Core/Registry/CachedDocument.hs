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

    -- * Packed full reads and their renders
    npmPacked,
    npmRendered,
    pypiPacked,
    pypiRendered,
) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Scientific (coefficient)
import Data.Text.Internal qualified as Text
import Math.NumberTheory.Logarithms (integerLog2)

import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry))
import Ecluse.Core.Registry.Json.Packed (RenderPlan (planMembers), planResident, planValue)
import Ecluse.Core.Registry.Npm.Document (PackedPackument (packumentTop), packumentResident, packumentValue)
import Ecluse.Core.Registry.PyPI.Document (PackedSimple (packedEnvelope), SimpleDocument, packedSimpleDocument, packedSimpleResident, simpleDocument, simpleEnvelope, simpleFiles)
import Ecluse.Core.Server.MemoryModel (chargeForResident)

{- | A serving document the pipeline threads and permitted caches hold. The derived 'Show' and 'Eq' are a
debug and test affordance, not a projection.
-}
data CachedDoc
    = CachedNpm Value ~Int64
    | CachedPyPISimple SimpleDocument ~Int64
    | PackedNpm PackedPackument
    | RenderedNpm RenderPlan
    | PackedPyPI PackedSimple
    | RenderedPyPI RenderPlan
    deriving stock (Eq, Show)

{- | Cached estimate of compact bytes. This is an accounting input, not measured resident memory. A
packed form charges the heap bytes it holds, in the compact units a consumer expands.
-}
weighCachedDoc :: CachedDoc -> Int64
weighCachedDoc = \case
    CachedNpm _ charge -> charge
    CachedPyPISimple _ charge -> charge
    PackedNpm packed -> estimateValueBytes (Object (packumentTop packed)) + fromIntegral (chargeForResident (packumentResident packed))
    RenderedNpm plan -> planCharge plan
    PackedPyPI packed -> estimateValueBytes (Object (packedEnvelope packed)) + fromIntegral (chargeForResident (packedSimpleResident packed))
    RenderedPyPI plan -> planCharge plan
  where
    planCharge plan = estimateValueBytes (Object (planMembers plan)) + fromIntegral (chargeForResident (planResident plan))

{- | npm's boundary pair. Every arm is spelled out, so a third ecosystem fails to compile here
rather than silently projecting as 'Nothing'. A packed or rendered document projects as its tree.
-}
npmCached :: (Value -> CachedDoc, CachedDoc -> Maybe Value)
npmCached = (\v -> CachedNpm v (estimateValueBytes v), project)
  where
    project = \case
        CachedNpm v _ -> Just v
        PackedNpm packed -> Just (packumentValue packed)
        RenderedNpm plan -> Just (planValue plan)
        CachedPyPISimple _ _ -> Nothing
        PackedPyPI _ -> Nothing
        RenderedPyPI _ -> Nothing

-- | PyPI's boundary pair, spelled out arm by arm for the same reason as 'npmCached'.
pypiSimpleCached :: (SimpleDocument -> CachedDoc, CachedDoc -> Maybe SimpleDocument)
pypiSimpleCached = (\v -> CachedPyPISimple v (simpleBytes v), project)
  where
    project = \case
        CachedPyPISimple v _ -> Just v
        PackedPyPI packed -> Just (packedSimpleDocument packed)
        RenderedPyPI plan -> Just (planDocument plan)
        CachedNpm _ _ -> Nothing
        PackedNpm _ -> Nothing
        RenderedNpm _ -> Nothing
    -- The assembled index as the tree assembly would have built it, its files at their served positions.
    planDocument plan = case planValue plan of
        Object fields -> simpleDocument (KeyMap.delete "files" fields) (zip (map ArrayEntry [0 ..]) (servedFiles fields))
        _ -> simpleDocument mempty []
    servedFiles fields = case KeyMap.lookup "files" fields of
        Just (Array files) -> toList files
        _ -> []

-- | npm's packed full read, and its packed form when the document is one.
npmPacked :: (PackedPackument -> CachedDoc, CachedDoc -> Maybe PackedPackument)
npmPacked = (PackedNpm, \case PackedNpm packed -> Just packed; _ -> Nothing)

-- | An assembled npm listing that renders from packed releases, and its plan when the document is one.
npmRendered :: (RenderPlan -> CachedDoc, CachedDoc -> Maybe RenderPlan)
npmRendered = (RenderedNpm, \case RenderedNpm plan -> Just plan; _ -> Nothing)

-- | PyPI's packed full read, and its packed form when the document is one.
pypiPacked :: (PackedSimple -> CachedDoc, CachedDoc -> Maybe PackedSimple)
pypiPacked = (PackedPyPI, \case PackedPyPI packed -> Just packed; _ -> Nothing)

-- | An assembled Simple index that renders from packed files, and its plan when the document is one.
pypiRendered :: (RenderPlan -> CachedDoc, CachedDoc -> Maybe RenderPlan)
pypiRendered = (RenderedPyPI, \case RenderedPyPI plan -> Just plan; _ -> Nothing)

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
