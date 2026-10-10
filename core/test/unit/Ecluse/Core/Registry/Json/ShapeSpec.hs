-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Each shape against the json-stream combinator of the same name, on generated shapes and bodies.
module Ecluse.Core.Registry.Json.ShapeSpec (spec) where

import Data.Aeson (Value (Array, Null, String), object, (.=))
import Data.ByteString qualified as BS
import Data.JsonStream.Parser qualified as J
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Registry.Json.Intern (tableTexts)
import Ecluse.Core.Registry.Json.Shape (Mode (..), Shape (ObjectOr, Scalar), Trees (..), namedMembers, prepareMembers, readShape)
import Ecluse.Core.Registry.Json.Walk (Walk (finish), withElement)
import Ecluse.Core.Registry.JsonStream qualified as JsonStream
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit))
import Ecluse.Test.Registry.JsonBytes (damaged, genChunks, genJsonBytes)
import Ecluse.Test.Registry.JsonStream (parseJsonChunks, readOutcome, testTable, walkJsonChunks)
import Ecluse.Test.Registry.Shape (TestMembers (..), TestShape (..), genShape, shapeNames, toShape, toShapeWith)

-- | Every generated shape reads every generated body to the same value, or fails the same way.
spec :: Spec
spec = describe "readShape" $ do
    modifyMaxSuccess (const 3000) $
        it "reads what the json-stream combinators read, for generated shapes and bodies" $
            hedgehog $ do
                shape <- forAll (genShape 3)
                body <- forAll (genJsonBytes shapeNames >>= damaged)
                chunks <- forAll (genChunks body)
                share <- forAll Gen.bool
                prepared <- forAll Gen.bool
                let bound = MetadataBodyLimit (BS.length body)
                    initial = testTable ["url"]
                    retained = if prepared then toShapeWith (prepareMembers initial) shape else toShape shape
                    walk tokens = withElement tokens $ \element rest ->
                        readShape Trees retained (if share then Share else Keep) initial element rest (\value _ _ -> finish (Just value))
                readOutcome (parseJsonChunks bound (toParser shape) (\_ value -> Right (Just value)) Nothing chunks)
                    === readOutcome (walkJsonChunks bound walk chunks)

    it "keeps first occurrences and table indices across escaped prepared keys at every split" $ do
        let initial = testTable ["url"]
            members = prepareMembers initial (namedMembers [(name, Scalar preparedDepth) | name <- ["a", "b", "url"]])
            shape = ObjectOr Null members
            walk tokens = withElement tokens $ \element rest ->
                readShape Trees shape Share initial element rest (\value held _ -> finish (value, toList (tableTexts held)))
            expected = object ["a" .= String "first", "url" .= String "private", "b" .= String "a"]
        forM_ [0 .. BS.length preparedBody] $ \cut ->
            readOutcome (walkJsonChunks (MetadataBodyLimit (BS.length preparedBody)) walk (filter (not . BS.null) [BS.take cut preparedBody, BS.drop cut preparedBody]))
                `shouldBe` Right (BS.length preparedBody, Right (expected, ["url", "a", "first", "b"]))

preparedDepth :: Int
preparedDepth = 8

preparedBody :: ByteString
preparedBody = "{\"a\":\"first\",\"\\u0061\":\"ignored\",\"url\":\"private\",\"b\":\"a\"}"

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
