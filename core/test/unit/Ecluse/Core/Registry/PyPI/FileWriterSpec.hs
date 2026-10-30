-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Typed member reduction agrees with the tree decoder, including optional and duplicate fields.
module Ecluse.Core.Registry.PyPI.FileWriterSpec (spec) where

import Data.ByteString qualified as BS
import Hedgehog (Gen, cover, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Package (PackageDetails (pkgArtifacts), PackageInfo (infoVersions), PackageName)
import Ecluse.Core.Registry.Metadata (MetadataError)
import Ecluse.Core.Registry.PyPI.Document (SimpleDocument, packedSimpleDocument)
import Ecluse.Core.Registry.PyPI.Metadata (projectPyPIPacked)
import Ecluse.Core.Security (Limits (maxVersionCount), defaultLimits)
import Ecluse.Test.Package (unscopedPyPI)
import Ecluse.Test.Registry.JsonBytes (genChunks)
import Ecluse.Test.Registry.JsonStream (byteChunks)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIIndex, projectPyPIPackedChunks)

spec :: Spec
spec = describe "fileWriter" $ do
    for_ fields $ \field -> for_ values $ \value ->
        it ("keeps the tree decoder's facts and diagnostics for " <> decodeUtf8 field <> "=" <> decodeUtf8 value) $
            agrees ["\"" <> field <> "\":" <> value]

    for_
        [ encodeUtf8 (show (minBound :: Int) :: Text)
        , encodeUtf8 (show (maxBound :: Int) :: Text)
        , encodeUtf8 (show (toInteger (minBound :: Int) - 1) :: Text)
        , encodeUtf8 (show (toInteger (maxBound :: Int) + 1) :: Text)
        , "0"
        , "-0"
        , "12.0"
        , "12e0"
        , "120e-1"
        , "12e-1"
        , "1e309"
        , "-1e309"
        ]
        $ \size ->
            it ("keeps bounded integer and scientific size semantics for " <> decodeUtf8 size) $
                agrees ["\"size\":" <> size]

    for_
        [ "2026-01-01T00:00:00.123456789012Z"
        , "2026-01-01T00:00:00.1234567890123Z"
        , "2026-01-01T01:00:00+01:00"
        , "2016-12-31T23:59:60Z"
        , "2026-02-30T00:00:00Z"
        ]
        $ \stamp ->
            it ("keeps the timestamp fast path and library fallback for " <> decodeUtf8 stamp) $
                agrees ["\"upload-time\":\"" <> stamp <> "\""]

    for_ ["requires-python", "provenance"] $ \field ->
        for_ [("7", "\"text\""), ("null", "7"), ("\"text\"", "false")] $ \(firstValue, repeated) ->
            it ("keeps the first optional text value for " <> decodeUtf8 field <> "=" <> decodeUtf8 firstValue) $
                agrees ["\"" <> field <> "\":" <> firstValue, "\"" <> field <> "\":" <> repeated]

    for_ ["null", "true", "false", "{}", "{\"unknown\":\"raw\"}", "{\"unknown\":null}"] $ \firstValue ->
        it ("keeps the first companion declaration " <> decodeUtf8 firstValue) $
            agrees ["\"core-metadata\":" <> firstValue, "\"core-metadata\":true"]

    it "keeps scalar facts and served values across single-byte chunks" $ do
        let body = "{\"name\":\"thing\",\"files\":[{\"filename\":\"thing-1.0.tar.gz\",\"url\":\"https://files.example/a.tar.gz\",\"hashes\":{\"custom\":\"\\u00e9\"},\"requires-python\":\">=3\",\"provenance\":\"https://files.example/\\u00e9\",\"size\":12345,\"upload-time\":\"2026-01-01T00:00:00Z\",\"yanked\":\"\"}]}"
        packedRead defaultLimits (byteChunks body) `shouldBe` treeRead defaultLimits body

    modifyMaxSuccess (const 1000) $
        it "keeps each file's own facts for generated files with repeated, reordered and mistyped members at any chunk cut" $
            hedgehog $ do
                files <- forAll (Gen.list (Range.constant 1 6) genFile)
                bounded <- forAll (Gen.frequency [(2, pure False), (1, pure True)])
                let body = "{\"name\":\"thing\",\"files\":[" <> BS.intercalate "," (map renderFile files) <> "]}"
                    limits = if bounded then defaultLimits{maxVersionCount = 1} else defaultLimits
                chunks <- forAll (genChunks body)
                let packed = packedRead limits chunks
                cover 15 "two or more files project" (projectedFiles packed >= 2)
                cover 30 "a later file lacks a member an earlier file has" (lacksEarlierMember files)
                cover 20 "a file repeats a member" (any repeatsMember files)
                cover 5 "the version bound trips" (isLeft packed)
                packed === treeRead limits body

    it "keeps the first repeated scalar, hash object and digest" $
        agrees ["\"yanked\":true", "\"yanked\":false", "\"hashes\":{\"custom\":\"first\",\"custom\":null}", "\"hashes\":false"]

    it "does not let a later valid value repair a failed first digest" $
        agrees ["\"hashes\":{\"sha256\":null,\"sha256\":\"00\"}"]

    it "keeps empty digest strings distinct from invalid digest values" $
        agrees ["\"hashes\":{\"sha256\":\"\",\"custom\":\"\",\"md5\":\"11\",\"custom\":null}"]

    for_ ["null", "false", "12", "[]", "{}"] $ \invalid ->
        for_ [0 .. 2] $ \position ->
            it ("keeps a failed digest at position " <> show position <> " invalid after later strings: " <> decodeUtf8 invalid) $
                let digests = ["\"md5\":\"11\"", "\"sha256\":\"00\""]
                    (leadingDigests, trailingDigests) = splitAt position digests
                 in agrees ["\"hashes\":{" <> BS.intercalate "," (leadingDigests <> ["\"custom\":" <> invalid] <> trailingDigests <> ["\"custom\":\"repaired\""]) <> "}"]

    it "resets digest validity between files and keeps invalid-entry order" $
        compareRead "{\"files\":[{\"filename\":\"thing-1.0.tar.gz\",\"url\":\"https://files.example/a.tar.gz\",\"hashes\":{\"custom\":null,\"sha256\":\"00\"}},{\"filename\":\"thing-2.0.tar.gz\",\"url\":\"https://files.example/b.tar.gz\",\"hashes\":{\"sha256\":\"\"}},{\"filename\":\"thing-3.0.tar.gz\",\"url\":\"https://files.example/c.tar.gz\",\"hashes\":{\"md5\":\"11\",\"custom\":false}}],\"name\":\"thing\"}"

    it "keeps a missing required field's diagnostic and its original position" $
        compareRead "{\"files\":[null,{}, {\"filename\":\"thing-1.0.tar.gz\"}],\"name\":\"thing\"}"

    it "keeps canonical collisions, source order and a late reported name" $
        compareRead "{\"files\":[{\"filename\":\"thing-01.0.tar.gz\",\"url\":\"https://files.example/a.tar.gz\",\"upload-time\":\"2026-01-01T00:00:00Z\",\"yanked\":true},{\"filename\":\"Thing-1.0-py3-none-any.whl\",\"url\":\"https://files.example/b.whl\",\"yanked\":false}],\"name\":\"Thing\"}"

fields :: [ByteString]
fields = ["core-metadata", "hashes", "requires-python", "size", "upload-time", "yanked", "provenance"]

values :: [ByteString]
values = ["null", "true", "false", "12", "12.5", "-1", "1e20", "\"\"", "\"text\"", "\"2026-01-01T00:00:00Z\"", "[]", "{}", "{\"sha256\":\"00\",\"custom\":\"11\"}", "{\"custom\":null}"]

agrees :: [ByteString] -> Expectation
agrees members = compareRead ("{\"name\":\"thing\",\"files\":[{" <> BS.intercalate "," (members <> ["\"filename\":\"thing-1.0.tar.gz\"", "\"url\":\"https://files.example/thing-1.0.tar.gz\""]) <> "}]}")

compareRead :: ByteString -> Expectation
compareRead body = packedRead defaultLimits [body] `shouldBe` treeRead defaultLimits body

packedRead :: Limits -> [ByteString] -> Either MetadataError (PackageInfo, Maybe SimpleDocument)
packedRead limits chunks = second packedSimpleDocument <$> (projectPyPIPackedChunks limits thing chunks >>= projectPyPIPacked limits thing)

treeRead :: Limits -> ByteString -> Either MetadataError (PackageInfo, Maybe SimpleDocument)
treeRead limits body = second Just <$> projectPyPIIndex limits thing body

thing :: PackageName
thing = unscopedPyPI "thing"

-- A file's members in source order, or a value that is not an object.
type GeneratedFile = Either ByteString [(ByteString, ByteString)]

-- Mostly accepted values, so most files project and a fact left from an earlier file shows.
genFile :: Gen GeneratedFile
genFile = Gen.frequency [(9, Right <$> members), (1, Left <$> Gen.element ["null", "7", "[]"])]
  where
    members = do
        required <- Gen.frequency [(4, pure ["filename", "url"]), (1, Gen.subsequence ["filename", "url"])]
        others <- Gen.list (Range.constant 0 8) (Gen.element ("filename" : "url" : "data-dist-info-metadata" : fields))
        keys <- Gen.shuffle (required <> others)
        traverse (\key -> (key,) <$> Gen.frequency [(3, Gen.element (accepted key)), (1, Gen.element values)]) keys

accepted :: ByteString -> [ByteString]
accepted = \case
    "filename" -> ["\"thing-1.0.tar.gz\"", "\"thing-1.0-py3-none-any.whl\"", "\"thing-2.0.tar.gz\"", "\"thing-2.0-py3-none-any.whl\""]
    "url" -> ["\"https://files.example/a\"", "\"https://files.example/b\""]
    "hashes" -> ["{\"sha256\":\"00\"}", "{\"md5\":\"11\",\"custom\":\"22\"}", "{}", "null"]
    "core-metadata" -> ["true", "false", "{\"sha256\":\"33\"}"]
    "requires-python" -> ["\">=3\"", "null"]
    "size" -> ["12", "345"]
    "upload-time" -> ["\"2026-01-01T00:00:00Z\"", "\"2025-06-01T12:00:00Z\""]
    "yanked" -> ["true", "false", "\"reason\""]
    "provenance" -> ["\"https://files.example/p\"", "null"]
    _ -> ["null"]

renderFile :: GeneratedFile -> ByteString
renderFile = either id (\members -> "{" <> BS.intercalate "," ["\"" <> key <> "\":" <> value | (key, value) <- members] <> "}")

projectedFiles :: Either MetadataError (PackageInfo, Maybe SimpleDocument) -> Int
projectedFiles = either (const 0) (sum . fmap (length . pkgArtifacts) . infoVersions . fst)

lacksEarlierMember :: [GeneratedFile] -> Bool
lacksEarlierMember files = or [any (`notElem` later) earlier | (earlier : rest) <- tails keys, later <- rest]
  where
    keys = map (map fst) (rights files)

repeatsMember :: GeneratedFile -> Bool
repeatsMember = either (const False) (\members -> length (ordNub (map fst members)) < length members)
