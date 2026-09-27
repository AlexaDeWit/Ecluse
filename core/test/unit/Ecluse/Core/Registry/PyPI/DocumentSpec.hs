-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Direct encoding writes the bytes of the rendered JSON object, files in source order.
module Ecluse.Core.Registry.PyPI.DocumentSpec (spec) where

import Data.Aeson (Value (Array, Null, Object, String), encode, object, (.=))
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.KeyMap qualified as KeyMap
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry))
import Ecluse.Core.Registry.PyPI.Document (SimpleDocument, simpleDocument, simpleEncoding)
import Ecluse.Test.Json (genKey, genValue)
import Ecluse.Test.Registry.PyPI.Metadata (documentFromValue, simpleValue)

spec :: Spec
spec = describe "simpleEncoding" $ do
    it "encodes an empty document as an object with an empty file array" $
        encoded (simpleDocument mempty []) `shouldBe` "{\"files\":[]}"

    it "replaces an envelope file field and keeps duplicate files in source order" $ do
        let file = object ["filename" .= ("quoted\"\\\n\x00e9.whl" :: Text), "size" .= (12 :: Int)]
            envelope = KeyMap.fromList [("files", Null), ("name", String "requests"), ("\x00e9\"", String "\x1f600\t")]
            document = simpleDocument envelope [(ArrayEntry 9, file), (ArrayEntry 2, Null), (ArrayEntry 9, file), (ArrayEntry 4, String "last")]
        encoded document `shouldBe` encode (simpleValue document)

    describe "properties" $
        it "writes the bytes of the rendered JSON object for any envelope and files" $
            hedgehog $ do
                envelope <- forAll (KeyMap.fromList <$> Gen.list (Range.linear 0 12) ((,) <$> genKey keyPool <*> genValue keyPool))
                files <- forAll (Gen.list (Range.linear 0 40) (genValue keyPool))
                let document = documentFromValue (Object (KeyMap.insert "files" (Array (fromList files)) envelope))
                encoded document === encode (simpleValue document)

encoded :: SimpleDocument -> LByteString
encoded = encodingToLazyByteString . simpleEncoding

keyPool :: [Text]
keyPool = ["files", "name", "meta", "versions", "alternate-locations", "project-status", "quoted\"\\\n"]
