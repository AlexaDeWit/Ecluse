-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Typed member reduction agrees with the tree decoder, including optional and duplicate fields.
module Ecluse.Core.Registry.PyPI.FileWriterSpec (spec) where

import Data.ByteString qualified as BS
import Test.Hspec

import Ecluse.Core.Registry.Metadata (MetadataError (MetadataBoundExceeded))
import Ecluse.Core.Registry.PyPI.Document (packedSimpleDocument)
import Ecluse.Core.Registry.PyPI.Metadata (projectPyPIPacked)
import Ecluse.Core.Security (LimitError (TooManyArtifacts, TooManyVersions), Limits (maxArtifactCount, maxVersionCount), defaultLimits)
import Ecluse.Test.Package (unscopedPyPI)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIIndex, projectPyPIPackedChunks)

spec :: Spec
spec = describe "fileWriter" $ do
    for_ fields $ \field -> for_ values $ \value ->
        it ("keeps the tree decoder's facts and diagnostics for " <> decodeUtf8 field <> "=" <> decodeUtf8 value) $
            agrees ["\"" <> field <> "\":" <> value]

    for_
        [ encodeUtf8 (show (minBound :: Int))
        , encodeUtf8 (show (maxBound :: Int))
        , encodeUtf8 (show (toInteger (minBound :: Int) - 1))
        , encodeUtf8 (show (toInteger (maxBound :: Int) + 1))
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

    it "keeps scalar facts and served values across single-byte chunks" $ do
        let body = "{\"name\":\"thing\",\"files\":[{\"filename\":\"thing-1.0.tar.gz\",\"url\":\"https://files.example/a.tar.gz\",\"hashes\":{\"custom\":\"\\u00e9\"},\"requires-python\":\">=3\",\"provenance\":\"https://files.example/\\u00e9\",\"size\":12345,\"upload-time\":\"2026-01-01T00:00:00Z\",\"yanked\":\"\"}]}"
            name = unscopedPyPI "thing"
        (second packedSimpleDocument <$> (projectPyPIPackedChunks defaultLimits name (map BS.singleton (BS.unpack body)) >>= projectPyPIPacked defaultLimits name))
            `shouldBe` (second Just <$> projectPyPIIndex defaultLimits name body)

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

    describe "first members in Keep mode after an artifact fault"
        $ for_
            [ ("the first filename stays in the existing release", "\"filename\":\"thing-1.0.tar.gz\",\"filename\":\"thing-2.0.tar.gz\"", TooManyArtifacts 2 1)
            , ("the first filename names a new release", "\"filename\":\"thing-2.0.tar.gz\",\"filename\":\"thing-1.0.tar.gz\"", TooManyVersions 2 1)
            , ("a later URL cannot repair the first one", "\"filename\":\"thing-2.0.tar.gz\",\"url\":null", TooManyArtifacts 2 1)
            , ("a later hashes field cannot repair the first one", "\"filename\":\"thing-2.0.tar.gz\",\"hashes\":false,\"hashes\":null", TooManyArtifacts 2 1)
            , ("a later hashes field cannot invalidate the first one", "\"filename\":\"thing-2.0.tar.gz\",\"hashes\":null,\"hashes\":false", TooManyVersions 2 1)
            , ("a later digest cannot repair the first one", "\"filename\":\"thing-2.0.tar.gz\",\"hashes\":{\"custom\":null,\"custom\":\"digest\"}", TooManyArtifacts 2 1)
            , ("a later interpreter field cannot invalidate the first one", "\"filename\":\"thing-2.0.tar.gz\",\"requires-python\":null,\"requires-python\":false", TooManyVersions 2 1)
            , ("a later provenance field cannot invalidate the first one", "\"filename\":\"thing-2.0.tar.gz\",\"provenance\":null,\"provenance\":7", TooManyVersions 2 1)
            ]
        $ \(label, members, fault) -> it label $ do
            let bounds = defaultLimits{maxVersionCount = 1, maxArtifactCount = 1}
                name = unscopedPyPI "thing"
                body = "{\"name\":\"thing\",\"files\":[{\"filename\":\"thing-1.0.tar.gz\",\"url\":\"https://files.example/thing-1.0.tar.gz\"},{\"filename\":\"thing-1.0-py3-none-any.whl\",\"url\":\"https://files.example/thing-1.0-py3-none-any.whl\"},{" <> members <> ",\"url\":\"https://files.example/thing-2.0.tar.gz\"}]}"
            void (projectPyPIPackedChunks bounds name [body] >>= projectPyPIPacked bounds name)
                `shouldBe` Left (MetadataBoundExceeded fault)

fields :: [ByteString]
fields = ["hashes", "requires-python", "size", "upload-time", "yanked", "provenance"]

values :: [ByteString]
values = ["null", "true", "false", "12", "12.5", "-1", "1e20", "\"\"", "\"text\"", "\"2026-01-01T00:00:00Z\"", "[]", "{}", "{\"sha256\":\"00\",\"custom\":\"11\"}", "{\"custom\":null}"]

agrees :: [ByteString] -> Expectation
agrees members = compareRead ("{\"name\":\"thing\",\"files\":[{" <> BS.intercalate "," (members <> ["\"filename\":\"thing-1.0.tar.gz\"", "\"url\":\"https://files.example/thing-1.0.tar.gz\""]) <> "}]}")

compareRead :: ByteString -> Expectation
compareRead body =
    (second packedSimpleDocument <$> (projectPyPIPackedChunks defaultLimits name [body] >>= projectPyPIPacked defaultLimits name))
        `shouldBe` (second Just <$> projectPyPIIndex defaultLimits name body)
  where
    name = unscopedPyPI "thing"
