-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Opaque serving documents with ecosystem-specific inject/project pairs.
The pipeline carries source snapshot scope separately and delegates wire access to adapters.
-}
module Ecluse.Core.Registry.CachedDocument (
    CachedDoc,
    weighCachedDoc,
    foldCachedDoc,
    npmCached,
    pypiSimpleCached,
) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Scientific (coefficient)
import Data.Text.Internal qualified as Text
import Math.NumberTheory.Logarithms (integerLog2)

{- | A serving document the pipeline threads and permitted caches hold. The derived 'Show' and 'Eq' are a
debug and test affordance, not a projection.
-}
data CachedDoc
    = CachedNpm Value ~Int64
    | CachedPyPISimple Value ~Int64
    deriving stock (Eq, Show)

-- | Cached estimate of compact bytes. This is an accounting input, not measured resident memory.
weighCachedDoc :: CachedDoc -> Int64
weighCachedDoc = foldCachedDoc (\_ charge -> charge)

{- | Read a held document blind to its ecosystem, for accounting only. Projection goes through
the ecosystem's own pair below, so no adapter reads another's document through this.
-}
foldCachedDoc :: (Value -> Int64 -> a) -> CachedDoc -> a
foldCachedDoc f = \case
    CachedNpm v charge -> f v charge
    CachedPyPISimple v charge -> f v charge

{- | npm's boundary pair. Every arm is spelled out, so a third ecosystem fails to compile here
rather than silently projecting as 'Nothing'.
-}
npmCached :: (Value -> CachedDoc, CachedDoc -> Maybe Value)
npmCached = (\v -> CachedNpm v (wireBytes v), \case CachedNpm v _ -> Just v; CachedPyPISimple _ _ -> Nothing)

-- | PyPI's boundary pair, spelled out arm by arm for the same reason as 'npmCached'.
pypiSimpleCached :: (Value -> CachedDoc, CachedDoc -> Maybe Value)
pypiSimpleCached = (\v -> CachedPyPISimple v (wireBytes v), \case CachedPyPISimple v _ -> Just v; CachedNpm _ _ -> Nothing)

wireBytes :: Value -> Int64
wireBytes = \case
    Object fields -> 2 + sum [4 + textBytes (Key.toText key) + wireBytes value | (key, value) <- KeyMap.toList fields]
    Array items -> 2 + sum [1 + wireBytes value | value <- toList items]
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
