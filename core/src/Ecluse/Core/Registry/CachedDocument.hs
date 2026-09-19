-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Opaque raw documents with ecosystem-specific inject/project pairs.
The pipeline carries source snapshot scope separately and delegates wire access to adapters.
-}
module Ecluse.Core.Registry.CachedDocument (
    CachedDoc,
    weighCachedDoc,
    foldCachedDoc,
    npmCached,
    pypiSimpleCached,
) where

import Data.Aeson (Value, encode)
import Data.ByteString.Lazy qualified as BSL

{- | A raw document the cache holds and the pipeline threads. The derived 'Show' and 'Eq' are a
debug and test affordance, not a projection.
-}
data CachedDoc
    = CachedNpm Value
    | CachedPyPISimple Value
    deriving stock (Eq, Show)

{- | A held document's resident-size estimate: the byte length of its compact encoding, the
figure the metadata cache weighs an entry by.
-}
weighCachedDoc :: CachedDoc -> Int64
weighCachedDoc = foldCachedDoc (BSL.length . encode)

{- | Read a held document blind to its ecosystem, for accounting only. Projection goes through
the ecosystem's own pair below, so no adapter reads another's document through this.
-}
foldCachedDoc :: (Value -> a) -> CachedDoc -> a
foldCachedDoc f = \case
    CachedNpm v -> f v
    CachedPyPISimple v -> f v

{- | npm's boundary pair. Every arm is spelled out, so a third ecosystem fails to compile here
rather than silently projecting as 'Nothing'.
-}
npmCached :: (Value -> CachedDoc, CachedDoc -> Maybe Value)
npmCached = (CachedNpm, \case CachedNpm v -> Just v; CachedPyPISimple _ -> Nothing)

-- | PyPI's boundary pair, spelled out arm by arm for the same reason as 'npmCached'.
pypiSimpleCached :: (Value -> CachedDoc, CachedDoc -> Maybe Value)
pypiSimpleCached = (CachedPyPISimple, \case CachedPyPISimple v -> Just v; CachedNpm _ -> Nothing)
