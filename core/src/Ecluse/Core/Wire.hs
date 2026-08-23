-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | A table-driven codec for the small named-enum vocabularies the system speaks: the
ecosystem key, the log format and level, the telemetry switch, the divergence policy.
A 'WireVocab' instance carries one @(value, name)@ table plus the human noun for the
set. 'lookupWire', 'parseWire' and 'renderWire' all read that table, so a parse, a
render, and the accepted-set message cannot drift apart.

The vocabulary keys on the type, so each type speaks exactly one vocabulary.
-}
module Ecluse.Core.Wire (
    WireVocab (..),
    lookupWire,
    parseWire,
    renderWire,
) where

import Data.Text qualified as T

{- | The wire vocabulary of a named-enum type. The @(value, name)@ table is the single source of
truth for the codec.
-}
class WireVocab a where
    {- | The human noun for the vocabulary, e.g. @"log format"@. Names the
    accepted set in 'parseWire's failure message.
    -}
    wireKind :: Text

    {- | Every value paired with its canonical wire name, in the order the accepted-set
    message names them. It must list every inhabitant, because 'renderWire' reads from it.
    -}
    wireTable :: NonEmpty (a, Text)

    {- | Further spellings 'lookupWire' accepts. An alias never reaches the accepted-set
    message, and 'renderWire' never emits one. Empty by default.
    -}
    wireAliases :: [(a, Text)]
    wireAliases = []

{- | The value a wire name denotes, through 'wireTable' and then 'wireAliases'.
'Nothing' for a name in neither.
-}
lookupWire :: forall a. (WireVocab a) => Text -> Maybe a
lookupWire raw = fst <$> find ((raw ==) . snd) (toList (wireTable @a) <> wireAliases @a)

{- | Parse a wire name, or report the accepted set on an unrecognised input. The message is
@unknown \\<kind\\> "\\<raw\\>" (expected one of: \\<names\\>)@, with the names in table order.
-}
parseWire :: forall a. (WireVocab a) => Text -> Either Text a
parseWire raw = maybeToRight unknown (lookupWire raw)
  where
    unknown =
        "unknown "
            <> wireKind @a
            <> " \""
            <> raw
            <> "\" (expected one of: "
            <> T.intercalate ", " (toList (fmap snd (wireTable @a)))
            <> ")"

{- | The canonical wire name of a value. A value the 'wireTable' omits breaches the class
contract, and renders as the table's first name rather than crashing a caller.
-}
renderWire :: forall a. (WireVocab a, Eq a) => a -> Text
renderWire value = case wireTable @a of
    first'@(_, firstName) :| rest -> fromMaybe firstName (lookup value (first' : rest))
