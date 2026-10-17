-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Lexing for HTTP request field values (RFC 9110, section 5): repeated fields, comma lists, and
@;@ parameters. Negotiation and conditional requests read their headers through it.
-}
module Ecluse.Core.Server.FieldValue (
    fieldValues,
    listMembers,
    memberParameters,
    trimWhitespace,
    lowerAscii,
) where

import Data.ByteString.Char8 qualified as BS8
import Data.Char (isAsciiUpper, toLower)

-- | Every value of a field, in request order. A field may legally repeat.
fieldValues :: (Eq name) => name -> [(name, value)] -> [value]
fieldValues name = map snd . filter ((== name) . fst)

-- | The members of one comma-separated value, trimmed, with empty members dropped.
listMembers :: ByteString -> [ByteString]
listMembers = filter (not . BS8.null) . map trimWhitespace . BS8.split ','

-- | Split a list member into its token and parameters, names lowered. A parameter with no @=@ has an empty value.
memberParameters :: ByteString -> (ByteString, [(ByteString, ByteString)])
memberParameters member = case map trimWhitespace (BS8.split ';' member) of
    [] -> ("", [])
    token : parameters -> (token, map parameter parameters)
  where
    parameter raw = case BS8.break (== '=') raw of
        (name, value) -> (lowerAscii (trimWhitespace name), trimWhitespace (BS8.drop 1 value))

-- | Drop the optional whitespace (spaces and tabs) around a value.
trimWhitespace :: ByteString -> ByteString
trimWhitespace = BS8.dropWhile isWhitespace . BS8.dropWhileEnd isWhitespace
  where
    isWhitespace c = c == ' ' || c == '\t'

-- | Lower ASCII letters, for the tokens the grammar compares case-insensitively.
lowerAscii :: ByteString -> ByteString
lowerAscii = BS8.map (\c -> if isAsciiUpper c then toLower c else c)
