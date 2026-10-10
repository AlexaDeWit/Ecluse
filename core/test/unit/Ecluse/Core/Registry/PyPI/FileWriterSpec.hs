-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Typed member reduction agrees with the tree decoder, including optional and duplicate fields.
module Ecluse.Core.Registry.PyPI.FileWriterSpec (spec) where

import Data.ByteString qualified as BS
import Test.Hspec

import Ecluse.Core.Registry.PyPI.Document (packedSimpleDocument)
import Ecluse.Core.Registry.PyPI.Metadata (projectPyPIPacked)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Test.Package (unscopedPyPI)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIIndex, projectPyPIPackedChunks)

spec :: Spec
spec = describe "fileWriter" $ do
    for_ fields $ \field -> for_ values $ \value ->
        it ("keeps the tree decoder's facts and diagnostics for " <> decodeUtf8 field <> "=" <> decodeUtf8 value) $
            agrees ["\"" <> field <> "\":" <> value]

    it "keeps the first repeated scalar, hash object and digest" $
        agrees ["\"yanked\":true", "\"yanked\":false", "\"hashes\":{\"custom\":\"first\",\"custom\":null}", "\"hashes\":false"]

    it "does not let a later valid value repair a failed first digest" $
        agrees ["\"hashes\":{\"sha256\":null,\"sha256\":\"00\"}"]

    it "keeps a missing required field's diagnostic and its original position" $
        compareRead "{\"files\":[null,{}, {\"filename\":\"thing-1.0.tar.gz\"}],\"name\":\"thing\"}"

    it "keeps canonical collisions, source order and a late reported name" $
        compareRead "{\"files\":[{\"filename\":\"thing-01.0.tar.gz\",\"url\":\"https://files.example/a.tar.gz\",\"upload-time\":\"2026-01-01T00:00:00Z\",\"yanked\":true},{\"filename\":\"Thing-1.0-py3-none-any.whl\",\"url\":\"https://files.example/b.whl\",\"yanked\":false}],\"name\":\"Thing\"}"

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
