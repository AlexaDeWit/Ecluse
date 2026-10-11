-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Full reads preserve refusal priority when a structural fault stops payload retention.
module Ecluse.Core.Registry.PyPI.StreamingProjectionSpec (spec) where

import Data.ByteString qualified as BS
import Test.Hspec

import Ecluse.Core.Registry.Json.Intern (tableTexts)
import Ecluse.Core.Registry.Json.Walk (Walked (..))
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Registry.Metadata (MetadataError (..))
import Ecluse.Core.Registry.PyPI.Metadata (projectPyPIPacked)
import Ecluse.Core.Security (LimitError (TooDeeplyNested, TooManyArtifacts, TooManyVersions), Limits (..), defaultLimits)
import Ecluse.Test.Package (unscopedPyPI)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIPackedChunks)
import Ecluse.Test.Support (expectRight)

-- | Pin full-read refusal order with literal results independent of the current reducer.
spec :: Spec
spec = describe "packed full-read refusal priority" $ do
    let firstTwo = "{\"files\":[" <> BS.intercalate "," [firstSdist, firstWheel] <> "]"
        allThree = "{\"files\":[" <> BS.intercalate "," [firstSdist, firstWheel, secondSdist] <> "]"
        named prefix = prefix <> ",\"name\":\"thing\"}"

    it "replaces an earlier artifact fault with the later version fault" $
        packed limits (named allThree) `shouldBe` Left (MetadataBoundExceeded (TooManyVersions 2 1))

    it "reports the artifact fault when no later version crosses the limit" $
        packed limits (named firstTwo) `shouldBe` Left (MetadataBoundExceeded (TooManyArtifacts 2 1))

    it "checks versions before artifacts when both limits first trip on one file" $
        packed limits (named ("{\"files\":[" <> BS.intercalate "," [firstSdist, secondSdist] <> "]"))
            `shouldBe` Left (MetadataBoundExceeded (TooManyVersions 2 1))

    it "keeps the first artifact fault when more files of that release follow" $
        packed limits (named ("{\"files\":[" <> BS.intercalate "," [firstSdist, firstWheel, firstSdist] <> "]"))
            `shouldBe` Left (MetadataBoundExceeded (TooManyArtifacts 2 1))

    it "does not count invalid files after an artifact fault" $
        packed limits (named ("{\"files\":[" <> BS.intercalate "," [firstSdist, firstWheel, "{\"filename\":\"thing-2.0.tar.gz\",\"url\":7}"] <> "]"))
            `shouldBe` Left (MetadataBoundExceeded (TooManyArtifacts 2 1))

    it "does not let a duplicate files array replace an earlier artifact fault" $
        packed limits (named (firstTwo <> ",\"files\":[" <> secondSdist <> "]"))
            `shouldBe` Left (MetadataBoundExceeded (TooManyArtifacts 2 1))

    it "does not count duplicate files arrays before a fault either" $
        packed limits (named ("{\"files\":[" <> firstSdist <> "],\"files\":[" <> BS.intercalate "," [firstWheel, secondSdist] <> "]"))
            `shouldBe` Right ()

    it "validates a late name before reporting the final structural fault" $
        packed limits (allThree <> ",\"name\":\"other\"}") `shouldBe` Left (MetadataNameMismatch "other")

    it "rejects a late invalid name before reporting the final structural fault" $
        packed limits (allThree <> ",\"name\":null}") `shouldBe` Left MetadataUndecodable

    it "validates a late name before a late shape fault" $
        packed limits (allThree <> ",\"name\":\"other\",\"versions\":true}") `shouldBe` Left (MetadataNameMismatch "other")

    it "validates a late protocol before name, shape and structural faults" $
        packed limits (allThree <> ",\"name\":\"other\",\"versions\":true,\"meta\":{\"api-version\":\"2.0\"}}")
            `shouldBe` Left MetadataUndecodable

    it "validates a late shape before reporting the final structural fault" $
        packed limits (allThree <> ",\"versions\":true,\"name\":\"thing\"}") `shouldBe` Left MetadataUndecodable

    it "consumes a malformed body tail despite the structural faults" $
        packed limits (allThree <> ",\"name\":\"thing\",\"unknown\":tru}") `shouldBe` Left MetadataUndecodable

    it "still decodes supported strings after payload retention stops" $
        packed limits (named ("{\"files\":[" <> BS.intercalate "," [firstSdist, firstWheel, "{\"filename\":\"thing-2.0.tar.gz\",\"url\":\"\\x\"}"] <> "]"))
            `shouldBe` Left MetadataUndecodable

    it "preserves the nesting check after payload retention stops" $
        packed limits{maxNestingDepth = 4} (firstTwo <> ",\"name\":\"thing\",\"files\":[{\"hashes\":{\"sha256\":\"00\"}}]}")
            `shouldBe` Left (MetadataBoundExceeded (TooDeeplyNested 4))

    it "does not grow the retained intern table for files read after the artifact fault" $ do
        let later = "{\"filename\":\"thing-2.0.tar.gz\",\"url\":\"https://files.pythonhosted.org/thing-2.0.tar.gz\",\"requires-python\":\"" <> BS.replicate 4096 120 <> "\"}"
            withLater = named ("{\"files\":[" <> BS.intercalate "," [firstSdist, firstWheel, later] <> "]")
        beforeTexts <- expectRight (retainedTexts (named firstTwo))
        afterTexts <- expectRight (retainedTexts withLater)
        afterTexts `shouldBe` beforeTexts
        packed limits withLater `shouldBe` Left (MetadataBoundExceeded (TooManyVersions 2 1))

limits :: Limits
limits = defaultLimits{maxVersionCount = 1, maxArtifactCount = 1}

packed :: Limits -> ByteString -> Either MetadataError ()
packed bounds body = void (projectPyPIPackedChunks bounds (unscopedPyPI "thing") [body] >>= projectPyPIPacked bounds (unscopedPyPI "thing"))

retainedTexts :: ByteString -> Either MetadataError [Text]
retainedTexts body = do
    streamed <- projectPyPIPackedChunks limits (unscopedPyPI "thing") [body]
    Walked table _ <- first (const MetadataUndecodable) (streamValue streamed)
    pure (toList (tableTexts table))

firstSdist, firstWheel, secondSdist :: ByteString
firstSdist = "{\"filename\":\"thing-1.0.tar.gz\",\"url\":\"https://files.pythonhosted.org/thing-1.0.tar.gz\"}"
firstWheel = "{\"filename\":\"thing-1.0-py3-none-any.whl\",\"url\":\"https://files.pythonhosted.org/thing-1.0-py3-none-any.whl\"}"
secondSdist = "{\"filename\":\"thing-2.0.tar.gz\",\"url\":\"https://files.pythonhosted.org/thing-2.0.tar.gz\"}"
