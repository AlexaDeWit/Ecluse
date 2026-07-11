-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | A table-driven codec for the small "named-enum" wire vocabularies the
configuration boundary speaks: a queue or credential provider, the log format, the
telemetry switch. Each is a fixed, finite set of wire names, historically
hand-rolled as a parse @\\case@, a render @\\case@, and a separately maintained
"(expected one of: …)" string -- three places kept in step by hand.

A 'WireVocab' instance carries the vocabulary as one @(value, name)@ table plus the
human noun for the set. 'renderWire' and 'parseWire' are derived from it once and
dispatched by type, so the rendered names, the parse, and the accepted-set message
can no longer drift apart: there is a single list of names per type.

The vocabulary is keyed by type, which means one type speaks exactly one vocabulary.
Where a type must be spoken two ways -- a credential provider is named differently as
a mirror-target selector than as a per-mount field -- the second vocabulary is a
@newtype@ over the first with its own instance (see
'Ecluse.Config.parseMirrorCredentialProvider').

The only contract is the round trip: for every value an instance's table names,
@'parseWire' ('renderWire' x) == Right x@. It holds by construction, since both
directions read the one 'wireTable'. No algebraic laws are at stake, so this is
deliberately not built on 'Ord', 'Semigroup', or 'Monoid'.
-}
module Ecluse.Core.Wire (
    WireVocab (..),
    renderWire,
    parseWire,
) where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T

{- | The wire vocabulary of a named-enum type: the @(value, name)@ table that is the
single source of truth for both 'renderWire' and 'parseWire', and the human noun the
rejected-input message names the set with.
-}
class WireVocab a where
    {- | The human noun for the vocabulary, e.g. @"queue provider"@. Names the
    accepted set in 'parseWire's failure message.
    -}
    wireKind :: Text

    {- | Every value paired with its wire name, listed in the order the accepted-set
    message names them. The table is expected to be complete (to list every
    inhabitant of @a@); 'renderWire' relies on that.
    -}
    wireTable :: NonEmpty (a, Text)

{- | The wire name of a value, looked up in its instance's 'wireTable' -- the inverse
of 'parseWire'.

A complete table (the contract every instance keeps) makes this total. A value the
table omits -- an instance that has fallen behind its type -- renders as the first
entry's name, which the instance's round-trip test surfaces rather than letting it
pass silently.
-}
renderWire :: forall a. (Eq a, WireVocab a) => a -> Text
renderWire value =
    maybe (snd (NE.head table)) snd (find ((value ==) . fst) table)
  where
    table = wireTable @a

{- | Parse a wire name back to its value through the same 'wireTable', or report the
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
