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
) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Scientific (coefficient)
import Data.Text.Internal qualified as Text
import Math.NumberTheory.Logarithms (integerLog2)

import Ecluse.Core.Registry.PyPI.Document (SimpleDocument, simpleEnvelope, simpleFiles)

{- | A serving document the pipeline threads and permitted caches hold. The derived 'Show' and 'Eq' are a
debug and test affordance, not a projection.
-}
data CachedDoc
    = CachedNpm Value ~Int64
    | CachedPyPISimple SimpleDocument ~Int64
    deriving stock (Eq, Show)

-- | Cached estimate of compact bytes. This is an accounting input, not measured resident memory.
weighCachedDoc :: CachedDoc -> Int64
weighCachedDoc = \case
    CachedNpm _ charge -> charge
    CachedPyPISimple _ charge -> charge

{- | npm's boundary pair. Every arm is spelled out, so a third ecosystem fails to compile here
rather than silently projecting as 'Nothing'.
-}
npmCached :: (Value -> CachedDoc, CachedDoc -> Maybe Value)
npmCached = (\v -> CachedNpm v (estimateValueBytes v), \case CachedNpm v _ -> Just v; CachedPyPISimple _ _ -> Nothing)

-- | PyPI's boundary pair, spelled out arm by arm for the same reason as 'npmCached'.
pypiSimpleCached :: (SimpleDocument -> CachedDoc, CachedDoc -> Maybe SimpleDocument)
pypiSimpleCached = (\v -> CachedPyPISimple v (simpleBytes v), \case CachedPyPISimple v _ -> Just v; CachedNpm _ _ -> Nothing)

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
