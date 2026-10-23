-- SPDX-FileCopyrightText: 2015 Ondrej Palkovsky
-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: BSD-3-Clause

{- | Incremental scanning over immutable input. Tokens and cursors retain their input slices.
Feed only after 'More' or 'Unclosed'. Empty input is a chunk boundary, not an end-of-document marker.
Callers own body limits and completion checks. Scanning preserves json-stream's lenient rules.
-}
module Data.JsonStream.Lexer (
    Cursor,
    Element (..),
    Scanned (..),
    Skipped (..),
    start,
    next,
    skipLevels,
    feed,
    remaining,
    PlainString,
    mkPlainString,
    plainBytes,
    plainText,
    decodeString,
) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)

import Data.JsonStream.Lexer.Internal
import Data.JsonStream.Unescape (unescapeText, unsafeDecodeASCII)

-- | Copy validated plain bytes into text without retaining the source allocation.
plainText :: PlainString -> Text
plainText = unsafeDecodeASCII . plainBytes

-- | Decode a string payload, rejecting invalid escapes and UTF-8.
decodeString :: BS.ByteString -> Either UnicodeException Text
decodeString = unescapeText
{-# INLINE decodeString #-}
