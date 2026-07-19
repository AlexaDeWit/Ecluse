-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Small __lenient-decode__ primitives shared by every ecosystem's aeson wire
decoders. Pure aeson support with no registry or package concept, so it sits beside
the bounded selective-decode engine in "Ecluse.Core.Json.Selective" rather than in
any one ecosystem's wire module:

* 'lenientOptional' reads an optional field, degrading a present-but-undecodable
  value to 'Nothing' rather than failing the whole decode, so one poisoned advisory
  value cannot deny a whole document.
* 'typeMismatchOneOf' fails a permissive string-or-object decoder with a descriptive
  message that names the accepted shapes and the JSON kind actually found.
-}
module Ecluse.Core.Json.Lenient (
    lenientOptional,
    typeMismatchOneOf,
) where

import Data.Aeson (
    FromJSON (parseJSON),
    Object,
    Value (Array, Bool, Null, Number, Object, String),
    (.:?),
 )
import Data.Aeson.Key (Key)
import Data.Aeson.Types (Parser, parseMaybe)

{- | Decode an optional field __leniently__: an absent, @null@, or
present-but-undecodable value all yield 'Nothing'. Where @(.:?)@ fails the whole
decode on a present-but-wrong value, this degrades a hostile value (wrong-typed,
fractional, or outside the target's range) to 'Nothing' instead. Reserved for
__advisory__ fields, so one poisoned value cannot deny the whole document; a
load-bearing field keeps @(.:?)@\/@(.:)@.
-}
lenientOptional :: (FromJSON a) => Object -> Key -> Parser (Maybe a)
lenientOptional o k = do
    mv <- o .:? k -- Parser (Maybe Value): a present junk value still arrives here
    pure (mv >>= parseMaybe parseJSON) -- but a Value that will not decode becomes Nothing

{- | Fail a lenient string-or-object decoder with a descriptive message, naming the
accepted shapes and reporting what was actually found. Keeps the @other ->@ branch of
each tolerant instance to one readable line.
-}
typeMismatchOneOf :: String -> Value -> Parser a
typeMismatchOneOf expected actual =
    fail ("expected " <> expected <> ", but encountered " <> valueKind actual)

-- A short, human description of a JSON value's kind, for parse-error messages.
valueKind :: Value -> String
valueKind = \case
    Object{} -> "an object"
    String{} -> "a string"
    Array{} -> "an array"
    Number{} -> "a number"
    Bool{} -> "a boolean"
    Null -> "null"
