-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Each shape against the json-stream combinator of the same name, on generated shapes and bodies,
and the packed tree against aeson's tree for the same read.
-}
module Ecluse.Core.Registry.Json.ShapeSpec (spec) where

import Control.Monad.ST (ST)
import Data.Aeson (Value (Array, Null, Number, String), encode, object, (.=))
import Data.ByteString qualified as BS
import Data.JsonStream.Parser qualified as J
import Data.JsonStream.TokenParser (TokenResult)
import Hedgehog (Gen, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Registry.Json.Intern (tableTexts)
import Ecluse.Core.Registry.Json.Packed (docTable, packedValue)
import Ecluse.Core.Registry.Json.Shape (Mode (..), Shape (..), Trees (..), readShape)
import Ecluse.Core.Registry.Json.Shape qualified as Shape
import Ecluse.Core.Registry.Json.Walk (Steps (Finished), withElement)
import Ecluse.Core.Registry.Json.Writer (decodeWhole, newWriter, sealValue)
import Ecluse.Core.Registry.JsonStream qualified as JsonStream
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit))
import Ecluse.Test.Registry.JsonBytes (damaged, genChunks, genJsonBytes)
import Ecluse.Test.Registry.JsonStream (parseJsonChunks, readOutcome, testTable, walkJsonChunks, walkWritingChunks)
import Ecluse.Test.Registry.Packed (renderAlone)

-- | Every generated shape reads every generated body to the same value, or fails the same way.
spec :: Spec
spec = describe "readShape" $
    modifyMaxSuccess (const 3000) $ do
        it "writes a packed value that decodes and renders as the tree aeson would hold, for generated shapes and bodies" $
            hedgehog $ do
                shape <- forAll (genShape 3)
                body <- forAll (genJsonBytes names >>= damaged)
                chunks <- forAll (genChunks body)
                share <- forAll Gen.bool
                let bound = MetadataBodyLimit (BS.length body)
                    mode = if share then Share else Keep
                    tree tokens = withElement tokens $ \element rest ->
                        readShape Trees (toShape shape) mode (testTable ["url"]) element rest $ \value _ _ ->
                            Finished (Just (value, value, toStrict (encode (object ["k" .= [value]]))))
                    packed :: ST st (TokenResult -> ST st (Steps (ST st) (Maybe (Value, Value, ByteString))))
                    packed =
                        newWriter Nothing <&> \writer tokens -> withElement tokens $ \element rest ->
                            readShape writer (toShape shape) mode (testTable ["url"]) element rest $ \() table _ -> do
                                form <- sealValue writer []
                                shared <- decodeWhole writer form
                                let held = docTable (tableTexts table)
                                pure (Finished (Just (shared, packedValue held form Nothing, renderAlone held form Nothing)))
                readOutcome (walkJsonChunks bound tree chunks) === readOutcome (walkWritingChunks bound packed chunks)

        it "reads what the json-stream combinators read, for generated shapes and bodies" $
            hedgehog $ do
                shape <- forAll (genShape 3)
                body <- forAll (genJsonBytes names >>= damaged)
                chunks <- forAll (genChunks body)
                share <- forAll Gen.bool
                let bound = MetadataBodyLimit (BS.length body)
                    walk tokens = withElement tokens $ \element rest ->
                        readShape Trees (toShape shape) (if share then Share else Keep) (testTable ["url"]) element rest (\value _ _ -> Finished (Just value))
                readOutcome (parseJsonChunks bound (toParser shape) (\_ value -> Right (Just value)) Nothing chunks)
                    === readOutcome (walkJsonChunks bound walk chunks)

-- A shape with the members it names, described once for both readers.
data TestShape
    = TestScalar Int
    | TestGeneric Int
    | TestObjectWith Int TestMembers TestShape
    | TestArrayWith Int TestShape TestShape
    | TestStringOr Int TestShape
    | TestObjectOr Value TestMembers
    | TestChecked Int TestShape
    deriving stock (Show)

data TestMembers = TestNamed [(Text, TestShape)] | TestEvery TestShape | TestKnown [Text] TestShape
    deriving stock (Show)

names :: [ByteString]
names = ["a", "b", "url", "tarball"]

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
            [ TestNamed <$> Gen.list (Range.linear 0 4) ((,) <$> Gen.element (map decodeUtf8 names) <*> genShape (depth - 1))
            , TestEvery <$> genShape (depth - 1)
            , TestKnown <$> Gen.subsequence (map decodeUtf8 names) <*> genShape (depth - 1)
            ]

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

-- The json-stream combinators the npm and PyPI field parsers compose for each shape. Known names
-- only share keys, so every member reads alike.
toParser :: TestShape -> J.Parser Value
toParser = \case
    TestScalar budget -> JsonStream.withinRetainedDepth budget (JsonStream.retainedScalar <|> pure (Array mempty))
    TestGeneric budget -> JsonStream.retainedValue budget
    TestObjectWith budget members fallback -> JsonStream.withinRetainedDepth budget (JsonStream.retainedObjectWith (toParser fallback) (toMembers members))
    TestArrayWith budget item fallback -> JsonStream.withinRetainedDepth budget (JsonStream.retainedArrayWith (toParser fallback) (toParser item))
    TestStringOr budget other -> JsonStream.withinRetainedDepth budget ((String <$> J.string) <|> toParser other)
    TestObjectOr fallback members -> JsonStream.retainedObjectOr fallback (toMembers members)
    TestChecked budget inner -> JsonStream.withinRetainedDepth budget (toParser inner)
  where
    toMembers = \case
        TestNamed entries -> JsonStream.namedMembers [(name, toParser shape) | (name, shape) <- entries]
        TestEvery shape -> JsonStream.everyMember (toParser shape)
        TestKnown _ shape -> JsonStream.everyMember (toParser shape)
