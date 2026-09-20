-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A projection scoped to the upstream bytes it came from.
Adapters retain the fetch digest while transforming either the typed or raw view.
-}
module Ecluse.Core.Snapshot (
    Snapshot (..),
    ContentDigest,
    digestBytes,
    digestFromContext,
) where

import Crypto.Hash (Context, SHA256, hashFinalize)
import Data.ByteArray qualified as BA

{- | A view paired with the digest computed before projection. Producers must use the same fetch.
Mapping preserves the snapshot scope.
-}
data Snapshot a = Snapshot
    { snapshotDigest :: ContentDigest
    , snapshotValue :: a
    }
    deriving stock (Eq, Show, Functor, Foldable, Traversable)

-- | Fingerprint the exact upstream bytes used to build a manifest.
newtype ContentDigest = ContentDigest ByteString
    deriving stock (Eq, Ord, Show)

-- | The digest's raw 32 bytes, for feeding into a wider fingerprint.
digestBytes :: ContentDigest -> ByteString
digestBytes (ContentDigest bytes) = bytes

-- | Finish the SHA-256 state accumulated from the exact consumed source chunks.
digestFromContext :: Context SHA256 -> ContentDigest
digestFromContext = ContentDigest . BA.convert . hashFinalize
