-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A projection scoped to the upstream bytes it came from.
Adapters retain the fetch digest while transforming either the typed or raw view.
-}
module Ecluse.Core.Snapshot (
    Snapshot (..),
    ContentDigest,
    digestOf,
    digestBytes,
) where

import Crypto.Hash (Digest, SHA256, hash)
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

-- | Digest a strict body: one @O(body)@ pass, paid at fetch time, never per serve.
digestOf :: ByteString -> ContentDigest
digestOf body = ContentDigest (BA.convert (hash body :: Digest SHA256))

-- | The digest's raw 32 bytes, for feeding into a wider fingerprint.
digestBytes :: ContentDigest -> ByteString
digestBytes (ContentDigest bytes) = bytes
