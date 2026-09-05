-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Generators for a route table's properties, shared across ecosystems.

The module name follows this support library's @Ecluse.X -> Ecluse.Test.X@ convention, and
mirrors "Ecluse.Core.Server.Route".

A router property explores paths, and the traversal, separator, and control fragments it must
explore are the same whatever protocol the table speaks. 'genPathSegmentFrom' builds those in
and takes the ecosystem's own names and route words as literals, so a second table reuses the
hostile half rather than restating it.
-}
module Ecluse.Test.Server.Route (
    genPathSegmentsFrom,
    genPathSegmentFrom,
    genSegmentName,
) where

import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- | A URL path of arbitrary segments, at the length a router's property explores.
genPathSegmentsFrom :: [Text] -> Gen [Text]
genPathSegmentsFrom = Gen.list (Range.linear 0 4) . genPathSegmentFrom

{- | One path segment: a plain name, one of the built-in hostile fragments or the caller's own
@literals@, or free text over the punctuation a path carries.
-}
genPathSegmentFrom :: [Text] -> Gen Text
genPathSegmentFrom literals =
    Gen.frequency
        [ (5, genSegmentName)
        , (4, Gen.element (hostileFragments <> literals))
        , (4, Gen.text (Range.linear 0 8) (Gen.element segmentChars))
        ]

-- | A plain segment name: alphanumerics, with the punctuation a name component may carry.
genSegmentName :: Gen Text
genSegmentName =
    Gen.text (Range.linear 1 8) (Gen.frequency [(8, Gen.alphaNum), (2, Gen.element ['.', '-', '_'])])

-- Traversal, separator, and control fragments no router may trust a segment with.
hostileFragments :: [Text]
hostileFragments =
    [ ""
    , "."
    , ".."
    , "-"
    , "-foo"
    , "a/b"
    , "a\\b"
    , "a\tb"
    , "a\0b"
    , "foo/bar"
    ]

segmentChars :: String
segmentChars = ['a', 'b', 'c', 'n', 'p', 'm', '@', '-', '/', '.', '%', ' ', '1', '2', '3', '4']
