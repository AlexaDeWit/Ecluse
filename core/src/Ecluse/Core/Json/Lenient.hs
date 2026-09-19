-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The __lenient-decode__ primitives every ecosystem's aeson wire decoder shares. They are
pure aeson support with no registry or package concept, so they sit beside the bounded
selective-decode engine in "Ecluse.Core.Json.Selective", not in any one ecosystem's wire module.
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

{- | Decode an optional field __leniently__: absent, @null@, and undecodable yield 'Nothing', so
one poisoned value cannot deny the document. For __advisory__ fields only, never a load-bearing one.
-}
lenientOptional :: (FromJSON a) => Object -> Key -> Parser (Maybe a)
lenientOptional o k = do
    mv <- o .:? k -- Parser (Maybe Value): a present junk value still arrives here
    pure (mv >>= parseMaybe parseJSON)

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
