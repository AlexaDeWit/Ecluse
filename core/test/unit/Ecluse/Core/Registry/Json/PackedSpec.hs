-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The packed form against aeson: its string encoding, its render, its decoding, and its hole.
module Ecluse.Core.Registry.Json.PackedSpec (spec) where

import Data.Aeson (Value (Object, String), encode)
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Registry.Json.Packed (Piece (..), Pieces (..), RenderPlan (..), encodeString, encodedLength, holeText, packedValue, planLength, planValue, renderPlan, replacement)
import Ecluse.Test.Json (genJsonText, genValue)
import Ecluse.Test.Registry.Packed (packValue)

spec :: Spec
spec = modifyMaxSuccess (const 2000) $ do
    describe "encodeString" $
        it "writes every string as aeson writes it, escapes, controls and astral characters included" $
            hedgehog $ do
                text <- forAll (Gen.text (Range.linear 0 24) (Gen.frequency [(4, Gen.unicode), (1, Gen.element ("\"\\\n\r\t\b\f\0\x1f\x7f\x2028\x1F600" :: String))]))
                encodeString text === toStrict (encodingToLazyByteString (Encoding.text text))
                encodedLength text === BS.length (encodeString text)

    describe "renderPlan" $
        it "renders a plan as aeson encodes its tree, with each hole's replacement in place" $
            hedgehog $ do
                members <- forAll (Gen.list (Range.linear 0 3) ((,) <$> genJsonText <*> genValue ["url", "a"]))
                values <- forAll (Gen.list (Range.linear 0 4) (genValue ["url", "a"]))
                substitutes <- forAll (Gen.list (Range.singleton (length values)) (Gen.maybe genJsonText))
                asObject <- forAll Gen.bool
                slot <- forAll genJsonText
                let packed = [(value, packValue ["url"] value) | value <- values]
                    pieces = [(value, Piece table form (replacement <$> substitute)) | ((value, Just (table, form)), substitute) <- zip packed substitutes]
                    top = KeyMap.fromList [(Key.fromText key, value) | (key, value) <- members]
                    plan
                        | asObject = RenderPlan top (Key.fromText slot) (ObjectPieces [(show index, piece) | (index, (_, piece)) <- zip [0 :: Int ..] pieces])
                        | otherwise = RenderPlan top (Key.fromText slot) (ArrayPieces (map snd pieces))
                length pieces === length values
                [packedValue table form Nothing | (_, Piece table form _) <- pieces] === map fst pieces
                [holeText table form | (_, Piece table form _) <- pieces] === map (urlOf . fst) pieces
                renderPlan plan === toStrict (encode (planValue plan))
                planLength plan === BS.length (renderPlan plan)
                [packedValue table form substitute | ((_, Piece table form _), substitute) <- zip pieces substitutes] === [replaced value substitute | ((value, _), substitute) <- zip pieces substitutes]
  where
    urlOf = \case
        Object fields | Just (String url) <- KeyMap.lookup "url" fields -> Just url
        _ -> Nothing
    replaced value substitute = case (value, substitute) of
        (Object fields, Just text) | Just (String _) <- KeyMap.lookup "url" fields -> Object (KeyMap.insert "url" (String text) fields)
        _ -> value
