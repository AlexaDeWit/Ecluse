-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Direct encoding writes the bytes of the rendered JSON object, files in source order.
module Ecluse.Core.Registry.PyPI.DocumentSpec (spec) where

import Data.Aeson (Value (Array, Bool, Null, Number, Object, String), encode, object, toEncoding, (.=))
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Scientific (scientific)
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry))
import Ecluse.Core.Registry.PyPI.Document (SimpleDocument, simpleDocument, simpleEncoding, simpleEnvelope, simpleFiles)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpPath), pypiCorpusPackages)
import Ecluse.Test.Json (genKey, genValue)
import Ecluse.Test.Registry.PyPI (simpleFile)
import Ecluse.Test.Registry.PyPI.Metadata (documentFromValue, projectPyPIIndex, simpleValue)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "simpleEncoding" $ do
    it "encodes an empty document as an object with an empty file array" $
        encoded (simpleDocument mempty []) `shouldBe` "{\"files\":[]}"

    it "replaces an envelope file field and keeps duplicate files in source order" $ do
        let file = object ["filename" .= ("quoted\"\\\n\x00e9.whl" :: Text), "size" .= (12 :: Int)]
            envelope = KeyMap.fromList [("files", Null), ("name", String "requests"), ("\x00e9\"", String "\x1f600\t")]
            document = simpleDocument envelope [(ArrayEntry 9, file), (ArrayEntry 2, Null), (ArrayEntry 9, file), (ArrayEntry 4, String "last")]
        encoded document `shouldBe` encode (simpleValue document)

    it "keeps files between its neighbouring keys in the previous byte order" $
        for_ [[], ["file"], ["files "], ["files\NUL"], ["file", "files\NUL", "files ", "filet"], ["", "FileS", "fil", "z", "\x1f600"]] $ \keys -> do
            let envelope = KeyMap.fromList [(key, String (Key.toText key)) | key <- keys]
                document = simpleDocument envelope []
            encoded document `shouldBe` previousBytes document

    it "writes the previous literal bytes with fields on both sides of files" $
        encoded (simpleDocument (KeyMap.fromList [("z", Null), ("file", Null)]) []) `shouldBe` "{\"file\":null,\"files\":[],\"z\":null}"

    it "matches the previous bytes across envelope and file-count boundaries" $
        for_ [0, 1, 2, 8, 64, 512 :: Int] $ \count ->
            for_ [[], take 1 boundaryValues, boundaryValues, concat (replicate 16 boundaryValues)] $ \values -> do
                let envelope = KeyMap.fromList [(Key.fromText (prefix <> show position), value) | prefix <- ["a-", "z-"], (position, value) <- zip [0 .. count - 1] (concat (replicate count boundaryValues))]
                    files = zipWith (\position value -> (ArrayEntry (position * 3), value)) [0 ..] values
                    document = simpleDocument envelope files
                encoded document `shouldBe` previousBytes document

    it "matches the previous bytes for malformed envelopes and file fields" $
        for_ boundaryValues $ \value -> do
            let documents =
                    [ documentFromValue value
                    , documentFromValue (object ["files" .= value, "meta" .= value, "name" .= value])
                    , simpleDocument (KeyMap.fromList [("files", value), ("meta", value)]) [(ArrayEntry 9, value), (ArrayEntry 2, Null), (ArrayEntry 9, value)]
                    ]
            for_ documents $ \document -> encoded document `shouldBe` previousBytes document

    describe "the corpus captures" $
        for_ pypiCorpusPackages $ \package ->
            it ("keeps the previous serialisation bytes for " <> cpPath package) $ do
                bytes <- readFileBS (cpPath package)
                (_, document) <- expectRight (projectPyPIIndex defaultLimits (cpPackage package) bytes)
                encoded document `shouldBe` previousBytes document

    describe "properties" $
        it "writes the bytes of the rendered JSON object for any envelope and files" $
            hedgehog $ do
                envelope <- forAll (KeyMap.fromList <$> Gen.list (Range.linear 0 12) ((,) <$> genKey keyPool <*> genValue keyPool))
                files <- forAll (Gen.list (Range.linear 0 40) (genValue keyPool))
                let document = documentFromValue (Object (KeyMap.insert "files" (Array (fromList files)) envelope))
                encoded document === encode (simpleValue document)
                encoded document === previousBytes document

encoded :: SimpleDocument -> LByteString
encoded = encodingToLazyByteString . simpleEncoding

-- Freeze the previous production encoder as the byte reference, including its temporary map.
previousBytes :: SimpleDocument -> LByteString
previousBytes document =
    encodingToLazyByteString $
        Encoding.pairs (KeyMap.foldMapWithKey Encoding.pair (KeyMap.insert "files" files (toEncoding <$> simpleEnvelope document)))
  where
    files = Encoding.list (toEncoding . snd) (simpleFiles document)

boundaryValues :: [Value]
boundaryValues =
    [ Null
    , Bool False
    , Bool True
    , Number 0
    , Number (-1)
    , Number (scientific 123456789012345678901234567890 (-20))
    , Number (scientific 1 (-400))
    , Number (scientific 1 400)
    , String "\NUL\b\f\n\r\t\"\\/\x00e9\x1f600"
    , Array mempty
    , object []
    , Array (fromList [Null, object ["quoted\"\\\n" .= ("\x00e9" :: Text)]])
    , foldr (\_ value -> object ["nested" .= value]) Null [1 .. 32 :: Int]
    , simpleFile "requests-1.0.tar.gz"
    ]

keyPool :: [Text]
keyPool = ["files", "name", "meta", "versions", "alternate-locations", "project-status", "quoted\"\\\n"]
