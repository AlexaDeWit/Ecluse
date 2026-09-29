-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Generated retained-value shapes, described once for every reader a spec compares.
module Ecluse.Test.Registry.Shape (
    TestShape (..),
    TestMembers (..),
    shapeNames,
    genShape,
    toShape,
) where

import Data.Aeson (Value (Null, Number, String))
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Ecluse.Core.Registry.Json.Shape (Shape (..))
import Ecluse.Core.Registry.Json.Shape qualified as Shape

-- | A shape with the members it names.
data TestShape
    = TestScalar Int
    | TestGeneric Int
    | TestObjectWith Int TestMembers TestShape
    | TestArrayWith Int TestShape TestShape
    | TestStringOr Int TestShape
    | TestObjectOr Value TestMembers
    | TestChecked Int TestShape
    deriving stock (Show)

-- | The members a generated object shape names.
data TestMembers = TestNamed [(Text, TestShape)] | TestEvery TestShape | TestKnown [Text] TestShape
    deriving stock (Show)

-- | The member names generated shapes and bodies draw from.
shapeNames :: [ByteString]
shapeNames = ["a", "b", "url", "tarball"]

-- | A shape nested at most the given depth, with budgets that run out as often as not.
genShape :: Int -> Gen TestShape
genShape depth =
    Gen.frequency
        ( [(3, TestScalar <$> budget), (2, TestGeneric <$> budget)]
            <> [ (w, shape)
               | depth > 0
               , (w, shape) <-
                    [ (2, TestObjectWith <$> budget <*> genMembers <*> genShape (depth - 1))
                    , (2, TestArrayWith <$> budget <*> genShape (depth - 1) <*> genShape (depth - 1))
                    , (1, TestStringOr <$> budget <*> genShape (depth - 1))
                    , (2, TestObjectOr <$> Gen.element [Null, Number 0, String "fallback"] <*> genMembers)
                    , (1, TestChecked <$> budget <*> genShape (depth - 1))
                    ]
               ]
        )
  where
    budget = Gen.frequency [(4, Gen.int (Range.linear 1 6)), (1, Gen.int (Range.linear (-1) 0))]
    genMembers =
        Gen.choice
            [ TestNamed <$> Gen.list (Range.linear 0 4) ((,) <$> Gen.element (map decodeUtf8 shapeNames) <*> genShape (depth - 1))
            , TestEvery <$> genShape (depth - 1)
            , TestKnown <$> Gen.subsequence (map decodeUtf8 shapeNames) <*> genShape (depth - 1)
            ]

-- | The reader's shape for a generated one.
toShape :: TestShape -> Shape
toShape = \case
    TestScalar budget -> Scalar budget
    TestGeneric budget -> Generic budget
    TestObjectWith budget members fallback -> ObjectWith budget (toMembers members) (toShape fallback)
    TestArrayWith budget item fallback -> ArrayWith budget (toShape item) (toShape fallback)
    TestStringOr budget other -> StringOr budget (toShape other)
    TestObjectOr fallback members -> ObjectOr fallback (toMembers members)
    TestChecked budget inner -> Checked budget (toShape inner)
  where
    toMembers = \case
        TestNamed entries -> Shape.namedMembers [(name, toShape shape) | (name, shape) <- entries]
        TestEvery shape -> Shape.everyMember (toShape shape)
        TestKnown known shape -> Shape.knownMembers known (toShape shape)
