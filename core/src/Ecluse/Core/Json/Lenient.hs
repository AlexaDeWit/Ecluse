-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The __lenient-decode__ primitives every ecosystem's aeson wire decoder shares. They are
pure aeson support with no registry or package concept, so they sit beside the bounded
selective-decode engine in "Ecluse.Core.Json.Selective", not in any one ecosystem's wire module.

* 'lenientOptional' reads an optional field. It degrades a present-but-undecodable
  value to 'Nothing' instead of failing the whole decode, so one poisoned advisory
  value cannot deny a whole document.
* 'typeMismatchOneOf' fails a permissive string-or-object decoder with a message that
  names the accepted shapes and the JSON kind it found, over 'valueKind'.
* 'valueKind' names a JSON value's kind, the one vocabulary a shape refusal phrases it in.
-}
module Ecluse.Core.Json.Lenient (
    lenientOptional,
    typeMismatchOneOf,
    valueKind,
) where

import Data.Aeson (
    FromJSON (parseJSON),
    Object,
    Value (Array, Bool, Null, Number, Object, String),
    (.:?),
 )
import Data.Aeson.Key (Key)
import Data.Aeson.Types (Parser, parseMaybe)

{- | Decode an optional field __leniently__: an absent, @null@, or present-but-undecodable
value all yield 'Nothing'. Use it for __advisory__ fields only, so one poisoned value cannot
deny the whole document. A load-bearing field keeps @(.:?)@\/@(.:)@.
-}
lenientOptional :: (FromJSON a) => Object -> Key -> Parser (Maybe a)
lenientOptional o k = do
    mv <- o .:? k -- Parser (Maybe Value): a present junk value still arrives here
    pure (mv >>= parseMaybe parseJSON) -- a Value that will not decode becomes Nothing

{- | Fail a lenient decoder with a message that names the accepted shapes and the JSON kind
it found.
-}
typeMismatchOneOf :: String -> Value -> Parser a
typeMismatchOneOf expected actual =
    fail ("expected " <> expected <> ", but encountered " <> valueKind actual)

-- | A short description of a JSON value's kind, for parse-error messages.
valueKind :: Value -> String
valueKind = \case
    Object{} -> "an object"
    String{} -> "a string"
    Array{} -> "an array"
    Number{} -> "a number"
    Bool{} -> "a boolean"
    Null -> "null"
