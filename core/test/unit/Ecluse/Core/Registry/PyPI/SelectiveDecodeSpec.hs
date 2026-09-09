-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Selective PyPI decoding preserves protocol fields and file positions.
Skipped values still obey JSON syntax and depth limits.
-}
module Ecluse.Core.Registry.PyPI.SelectiveDecodeSpec (spec) where

import Data.Aeson (Value (Array, Object, String), encode, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Test.Registry.PyPI (simpleFile)

import Ecluse.Core.Registry.PyPI.SelectiveDecode (
    SelectedFiles (sfFileCount, sfFiles, sfMeta, sfName),
    SelectiveError (SelectiveTooDeeplyNested, SelectiveUndecodable),
    selectFilesFromIndex,
 )

spec :: Spec
spec = do
    selectionSpec
    volumeSpec
    faithfulnessSpec

selectionSpec :: Spec
selectionSpec = describe "selectFilesFromIndex" $ do
    it "retains only the API declaration from a large metadata object" $ do
        selected <- shouldSelect (const True) (BL.toStrict (encode (object ["meta" .= object ["api-version" .= ("1.4" :: Text), "unrelated" .= replicate 400 (rawIndexValue [])]])))
        sfMeta selected `shouldBe` Just (object ["api-version" .= ("1.4" :: Text)])

    it "skips the contents of a malformed metadata array" $ do
        selected <- shouldSelect (const True) "{\"meta\":[{\"ignored\":1}]}"
        sfMeta selected `shouldBe` Just (Array mempty)

    it "materialises the files of the requested release and no others" $ do
        selected <- shouldSelect (belongsTo "2.34.2") (indexOf ["requests-2.34.2.tar.gz", "requests-2.34.2-py3-none-any.whl", "requests-2.34.1.tar.gz"])
        selectedNames selected `shouldBe` ["requests-2.34.2.tar.gz", "requests-2.34.2-py3-none-any.whl"]

    it "reads the index's self-reported name, the anti-shadowing authority" $ do
        selected <- shouldSelect (belongsTo "2.34.2") (indexOf ["requests-2.34.2.tar.gz"])
        sfName selected `shouldBe` Just (String "requests")

    it "counts every entry of the array, not only the ones it kept" $ do
        selected <- shouldSelect (belongsTo "2.34.2") (indexOf ["requests-2.34.2.tar.gz", "requests-2.34.1.tar.gz", "requests-2.34.0.tar.gz"])
        sfFileCount selected `shouldBe` 3

    it "keeps a selected entry whole, unmodelled keys and all" $ do
        selected <- shouldSelect (belongsTo "2.34.2") (indexOf ["requests-2.34.2.tar.gz"])
        (entryKey "provenance" . snd =<< listToMaybe (sfFiles selected)) `shouldBe` Just (String "https://pypi.org/integrity/x/provenance")

    it "selects nothing for a release the index does not carry" $ do
        selected <- shouldSelect (belongsTo "9.9.9") (indexOf ["requests-2.34.2.tar.gz"])
        sfFiles selected `shouldBe` []

    it "skips an entry that declares no readable name, rather than guessing at one" $ do
        selected <- shouldSelect (const True) (rawIndex [object ["url" .= ("https://files.test/x" :: Text)]])
        sfFiles selected `shouldBe` []
        sfFileCount selected `shouldBe` 1

    it "reads an index that lists no files at all" $ do
        selected <- shouldSelect (const True) (rawIndex [])
        sfFiles selected `shouldBe` []
        sfFileCount selected `shouldBe` 0

    it "keeps the first files array when a hostile document repeats the key" $ do
        selected <- shouldSelect (const True) duplicateFilesIndex
        selectedNames selected `shouldBe` ["first.tar.gz"]

volumeSpec :: Spec
volumeSpec = describe "decode volume" $
    it "materialises one entry per matching file, whatever the size of the array" $ do
        selected <- shouldSelect (belongsTo "1.0.0") manyFileIndex
        length (sfFiles selected) `shouldBe` 2
        sfFileCount selected `shouldBe` 400

faithfulnessSpec :: Spec
faithfulnessSpec = describe "faithful to a whole-document decode" $ do
    for_ ["{\"meta\":{\"unrelated\":[1,]}}", "{\"meta\":[1,]}", "{\"meta\":null,\"meta\":{\"ignored\":[1,]}}"] $ \body ->
        it ("checks syntax in skipped metadata " <> show body) $
            selectFilesFromIndex 64 (const True) body `shouldBe` Left SelectiveUndecodable

    for_ ["{\"meta\":{\"unrelated\":[1]}}", "{\"meta\":[[1]]}", "{\"meta\":null,\"meta\":{\"ignored\":[1]}}", "{\"meta\":{\"api-version\":null,\"api-version\":[1]}}"] $ \body ->
        it ("checks depth in skipped metadata " <> show body) $
            selectFilesFromIndex 3 (const True) body `shouldBe` Left SelectiveTooDeeplyNested

    it "refuses malformed JSON inside an entry it would have skipped" $
        selectFilesFromIndex 64 (belongsTo "2.34.2") "{\"name\":\"requests\",\"files\":[{\"filename\":\"other-1.0.tar.gz\",}]}"
            `shouldBe` Left SelectiveUndecodable

    it "refuses trailing non-whitespace after the top-level object" $
        selectFilesFromIndex 64 (const True) (BL.toStrict (encode (rawIndexValue [])) <> "junk")
            `shouldBe` Left SelectiveUndecodable

    it "refuses a body that is not a JSON object" $
        selectFilesFromIndex 64 (const True) "[]" `shouldBe` Left SelectiveUndecodable

    it "refuses a value nested past the budget, wherever it sits" $
        selectFilesFromIndex 3 (const True) (indexOf ["requests-2.34.2.tar.gz"])
            `shouldBe` Left SelectiveTooDeeplyNested

    it "refuses the document outright when the budget cannot hold the object itself" $
        selectFilesFromIndex 0 (const True) (indexOf ["requests-2.34.2.tar.gz"])
            `shouldBe` Left SelectiveTooDeeplyNested

shouldSelect :: (Text -> Bool) -> ByteString -> IO SelectedFiles
shouldSelect belongs = either (fail . show) pure . selectFilesFromIndex 64 belongs

belongsTo :: Text -> Text -> Bool
belongsTo version filename = T.isInfixOf ("-" <> version <> ".") filename || T.isInfixOf ("-" <> version <> "-") filename

indexOf :: [Text] -> ByteString
indexOf = rawIndex . map simpleFile

rawIndex :: [Value] -> ByteString
rawIndex = BL.toStrict . encode . rawIndexValue

rawIndexValue :: [Value] -> Value
rawIndexValue files = object ["name" .= ("requests" :: Text), "meta" .= object ["api-version" .= ("1.4" :: Text)], "files" .= files]

duplicateFilesIndex :: ByteString
duplicateFilesIndex =
    "{\"name\":\"requests\",\"files\":[" <> entry "first.tar.gz" <> "],\"files\":[" <> entry "second.tar.gz" <> "]}"
  where
    entry :: Text -> ByteString
    entry name = "{\"filename\":\"" <> encodeUtf8 name <> "\",\"url\":\"https://files.test/" <> encodeUtf8 name <> "\"}"

manyFileIndex :: ByteString
manyFileIndex = indexOf (["requests-1.0.0.tar.gz", "requests-1.0.0-py3-none-any.whl"] <> [T.pack ("requests-2." <> show n <> ".0.tar.gz") | n <- [1 .. 398 :: Int]])

selectedNames :: SelectedFiles -> [Text]
selectedNames selected = mapMaybe stringOf (mapMaybe (entryKey "filename" . snd) (sfFiles selected))
  where
    stringOf = \case
        String s -> Just s
        _ -> Nothing

entryKey :: Text -> Value -> Maybe Value
entryKey key = \case
    Object entry -> KeyMap.lookup (fromString (toString key)) entry
    _ -> Nothing
