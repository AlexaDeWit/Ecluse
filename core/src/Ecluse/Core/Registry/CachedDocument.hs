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
    pypiPacked,
    npmRendered,
    pypiRendered,
    rendered,
) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Scientific (coefficient)
import Data.Text.Internal qualified as Text
import Math.NumberTheory.Logarithms (integerLog2)

import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry))
import Ecluse.Core.Registry.Json.Packed (Pieces (..), RenderPlan (..), planLength, planValue)
import Ecluse.Core.Registry.Npm.Document (PackedPackument, packumentBytes, packumentValue)
import Ecluse.Core.Registry.PyPI.Document (PackedSimple, SimpleDocument, packedSimpleBytes, packedSimpleDocument, simpleDocument, simpleEnvelope, simpleFiles)

{- | A serving document the pipeline threads and permitted caches hold. The derived 'Show' and 'Eq' are a
debug and test affordance, not a projection.
-}
data CachedDoc
    = CachedNpm Value ~Int64
    | CachedPyPISimple SimpleDocument ~Int64
    | PackedNpm PackedPackument
    | PackedPyPI PackedSimple
    | RenderedNpm RenderPlan
    | RenderedPyPI RenderPlan
    deriving stock (Eq, Show)

{- | Cached estimate of compact bytes for aeson's trees, and the bytes a packed form or render holds.
This is an accounting input, not measured resident memory.
-}
weighCachedDoc :: CachedDoc -> Int64
weighCachedDoc = \case
    CachedNpm _ charge -> charge
    CachedPyPISimple _ charge -> charge
    PackedNpm packed -> fromIntegral (packumentBytes packed)
    PackedPyPI packed -> fromIntegral (packedSimpleBytes packed)
    RenderedNpm plan -> fromIntegral (planLength plan)
    RenderedPyPI plan -> fromIntegral (planLength plan)

{- | npm's boundary pair. Every arm is spelled out, so a third ecosystem fails to compile here
rather than silently projecting as 'Nothing'. A packed or rendered document projects as the tree
the read or the assembly would have built.
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
    planDocument plan@(RenderPlan _ _ pieces) = case planValue plan of
        Object fields -> simpleDocument fields (zip (map ArrayEntry [0 ..]) (filesOf fields pieces))
        _ -> simpleDocument mempty []
    filesOf fields = \case
        ArrayPieces _ | Just (Array files) <- KeyMap.lookup "files" fields -> toList files
        _ -> []

-- | npm's packed full read, and its packed form when the document is one.
npmPacked :: (PackedPackument -> CachedDoc, CachedDoc -> Maybe PackedPackument)
npmPacked = (PackedNpm, \case PackedNpm packed -> Just packed; _ -> Nothing)

-- | PyPI's packed full read, and its packed form when the document is one.
pypiPacked :: (PackedSimple -> CachedDoc, CachedDoc -> Maybe PackedSimple)
pypiPacked = (PackedPyPI, \case PackedPyPI packed -> Just packed; _ -> Nothing)

-- | An assembled npm listing that renders from packed releases.
npmRendered :: RenderPlan -> CachedDoc
npmRendered = RenderedNpm

-- | An assembled Simple index that renders from packed files.
pypiRendered :: RenderPlan -> CachedDoc
pypiRendered = RenderedPyPI

-- | The render of an assembled document, when it is one.
rendered :: CachedDoc -> Maybe RenderPlan
rendered = \case
    RenderedNpm plan -> Just plan
    RenderedPyPI plan -> Just plan
    _ -> Nothing

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
