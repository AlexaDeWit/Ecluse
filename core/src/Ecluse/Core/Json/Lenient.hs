-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Optional advisory fields share lenient Aeson decoding across ecosystems.
module Ecluse.Core.Json.Lenient (
    lenientOptional,
    lenientOptionalWith,
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
lenientOptional :: (FromJSON a, NFData a) => Object -> Key -> Parser (Maybe a)
lenientOptional = lenientOptionalWith parseJSON

-- | 'lenientOptional' through the given decoder, in place of the type's own.
lenientOptionalWith :: (NFData a) => (Value -> Parser a) -> Object -> Key -> Parser (Maybe a)
lenientOptionalWith decode o k = do
    mv <- o .:? k -- Parser (Maybe Value): a present junk value still arrives here
    -- Evaluated in full, so a retained field holds none of the decoder's state.
    pure $ case mv >>= parseMaybe decode of
        Just value -> Just $!! value
        Nothing -> Nothing

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
