-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Opaque raw documents with ecosystem-specific inject/project pairs.
The pipeline carries source snapshot scope separately and delegates wire access to adapters.
-}
module Ecluse.Core.Registry.CachedDocument (
    CachedDoc,
    weighCachedDoc,
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
weighCachedDoc = \case
    CachedNpm v -> BSL.length (encode v)
    CachedPyPISimple v -> BSL.length (encode v)

-- | npm's boundary pair. A document another ecosystem injected projects as 'Nothing'.
npmCached :: (Value -> CachedDoc, CachedDoc -> Maybe Value)
npmCached = (CachedNpm, \case CachedNpm v -> Just v; _ -> Nothing)

-- | PyPI's boundary pair. A document another ecosystem injected projects as 'Nothing'.
pypiSimpleCached :: (Value -> CachedDoc, CachedDoc -> Maybe Value)
pypiSimpleCached = (CachedPyPISimple, \case CachedPyPISimple v -> Just v; _ -> Nothing)
