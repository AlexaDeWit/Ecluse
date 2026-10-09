-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Length framing for the byte strings a digest covers. Each component carries its own length,
so no content can pass for a boundary.
-}
module Ecluse.Core.Server.Framing (
    frameBytes,
    frameComponents,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString, intDec)

-- | One component: its decimal byte length, a colon, then its bytes.
frameBytes :: ByteString -> Builder
frameBytes bytes = intDec (BS.length bytes) <> ":" <> byteString bytes

{- | A tuple's components in order, so that no two tuples frame to the same bytes. An absent
component is a lone @-@, which no length starts with, so it never frames as the shorter tuple.
-}
frameComponents :: [Maybe ByteString] -> Builder
frameComponents = foldMap (maybe "-" frameBytes)
