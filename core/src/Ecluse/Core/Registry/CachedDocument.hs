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
) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Scientific (coefficient)
import Data.Text.Internal qualified as Text
import Math.NumberTheory.Logarithms (integerLog2)

import Ecluse.Core.Registry.Json.Packed (RenderPlan (planMembers), planResident, planValue)
import Ecluse.Core.Registry.Npm.Document (PackedPackument (packumentTop), packumentResident, packumentValue)
import Ecluse.Core.Registry.PyPI.Document (SimpleDocument, simpleEnvelope, simpleFiles)
import Ecluse.Core.Server.MemoryModel (chargeForResident)

{- | A serving document the pipeline threads and permitted caches hold. The derived 'Show' and 'Eq' are a
debug and test affordance, not a projection.
-}
data CachedDoc
    = CachedNpm Value ~Int64
    | CachedPyPISimple SimpleDocument ~Int64
    | PackedNpm PackedPackument
    | RenderedNpm RenderPlan
    deriving stock (Eq, Show)

{- | Cached estimate of compact bytes. This is an accounting input, not measured resident memory. A
packed form charges the heap bytes it holds, in the compact units a consumer expands.
-}
weighCachedDoc :: CachedDoc -> Int64
weighCachedDoc = \case
    CachedNpm _ charge -> charge
    CachedPyPISimple _ charge -> charge
    PackedNpm packed -> estimateValueBytes (Object (packumentTop packed)) + fromIntegral (chargeForResident (packumentResident packed))
    RenderedNpm plan -> estimateValueBytes (Object (planMembers plan)) + fromIntegral (chargeForResident (planResident plan))

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

-- | PyPI's boundary pair, spelled out arm by arm for the same reason as 'npmCached'.
pypiSimpleCached :: (SimpleDocument -> CachedDoc, CachedDoc -> Maybe SimpleDocument)
pypiSimpleCached = (\v -> CachedPyPISimple v (simpleBytes v), project)
  where
    project = \case
        CachedPyPISimple v _ -> Just v
        CachedNpm _ _ -> Nothing
        PackedNpm _ -> Nothing
        RenderedNpm _ -> Nothing

-- | npm's packed full read, and its packed form when the document is one.
npmPacked :: (PackedPackument -> CachedDoc, CachedDoc -> Maybe PackedPackument)
npmPacked = (PackedNpm, \case PackedNpm packed -> Just packed; _ -> Nothing)

-- | An assembled npm listing that renders from packed releases, and its plan when the document is one.
npmRendered :: (RenderPlan -> CachedDoc, CachedDoc -> Maybe RenderPlan)
npmRendered = (RenderedNpm, \case RenderedNpm plan -> Just plan; _ -> Nothing)

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
