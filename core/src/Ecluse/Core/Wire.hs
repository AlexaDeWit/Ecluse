-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | A table-driven codec for the small "named-enum" wire vocabularies the
configuration boundary speaks: a credential provider, the log format, the
telemetry switch. Each is a fixed, finite set of wire names, historically
hand-rolled as a parse @\\case@ and a separately maintained
"(expected one of: …)" string -- kept in step by hand.

A 'WireVocab' instance carries the vocabulary as one @(value, name)@ table plus the
human noun for the set. 'parseWire' is derived from it once and dispatched by type,
so the parse and the accepted-set message can no longer drift apart: there is a
single list of names per type.

The vocabulary is keyed by type, so each type speaks exactly one vocabulary: the one
'wireTable' names its values in.
-}
module Ecluse.Core.Wire (
    WireVocab (..),
    parseWire,
) where

import Data.Text qualified as T

{- | The wire vocabulary of a named-enum type: the @(value, name)@ table that is the
single source of truth for 'parseWire', and the human noun the rejected-input
message names the set with.
-}
class WireVocab a where
    {- | The human noun for the vocabulary, e.g. @"log format"@. Names the
    accepted set in 'parseWire's failure message.
    -}
    wireKind :: Text

    {- | Every value paired with its wire name, listed in the order the accepted-set
    message names them. The table is expected to be complete (to list every
    inhabitant of @a@).
    -}
    wireTable :: NonEmpty (a, Text)

{- | Parse a wire name to its value through the 'wireTable', or report the
accepted set on an unrecognised input. The failure message is
@unknown \<kind\> "\<raw\>" (expected one of: \<names\>)@, the names joined with a
comma and a space in table order.
-}
parseWire :: forall a. (WireVocab a) => Text -> Either Text a
parseWire raw =
    case find ((raw ==) . snd) table of
        Just (value, _name) -> Right value
        Nothing ->
            Left
                ( "unknown "
                    <> wireKind @a
                    <> " \""
                    <> raw
                    <> "\" (expected one of: "
                    <> T.intercalate ", " (toList (fmap snd table))
                    <> ")"
                )
  where
    table = wireTable @a
