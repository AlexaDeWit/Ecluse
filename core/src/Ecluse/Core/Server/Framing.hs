-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Length framing for byte strings that stand for a tuple of components: the input of a digest, or
the identity of a cache key. Each component carries its own length, so no content can pass for a boundary.
-}
module Ecluse.Core.Server.Framing (
    frameBytes,
    frameComponents,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString, intDec)
import Data.ByteString.Builder.Extra (smallChunkSize, toLazyByteStringWith, untrimmedStrategy)
import Data.ByteString.Lazy qualified as LBS

-- | One component: its decimal byte length, a colon, then its bytes.
frameBytes :: ByteString -> Builder
frameBytes bytes = intDec (BS.length bytes) <> ":" <> byteString bytes

{- | A tuple's components in order, so that no two tuples frame to the same bytes. An absent
component is a lone @-@, which no length starts with, so it never frames as the shorter tuple.
-}
frameComponents :: [Maybe ByteString] -> ByteString
frameComponents components =
    LBS.toStrict (toLazyByteStringWith (untrimmedStrategy room smallChunkSize) LBS.empty frames)
  where
    frames = foldMap (maybe "-" frameBytes) components
    -- Each present component gets its bytes, a colon, and the 20 free bytes the writer wants before a length.
    -- An absent one gets the 4 it wants before a marker. A component over the builder's copy limit is its own chunk.
    room = sum (map (maybe 4 ((+ 21) . BS.length)) components)
