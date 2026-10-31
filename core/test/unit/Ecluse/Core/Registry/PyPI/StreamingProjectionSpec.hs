-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Full reads refuse at the first file position past the version ceiling, before any later fault.
module Ecluse.Core.Registry.PyPI.StreamingProjectionSpec (spec) where

import Data.ByteString qualified as BS
import Test.Hspec

import Ecluse.Core.Registry.Metadata (MetadataError (..))
import Ecluse.Core.Registry.PyPI.Metadata (projectPyPIPacked)
import Ecluse.Core.Security (LimitError (TooManyVersions), Limits (..), defaultLimits)
import Ecluse.Test.Package (unscopedPyPI)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIPackedChunks)

-- | Pin full-read refusal order with literal results independent of the current reducer.
spec :: Spec
spec = describe "packed full-read refusal priority" $ do
    let firstTwo = "{\"files\":[" <> BS.intercalate "," [firstSdist, firstWheel] <> "]"
        allThree = "{\"files\":[" <> BS.intercalate "," [firstSdist, firstWheel, secondSdist] <> "]"
        named prefix = prefix <> ",\"name\":\"thing\"}"
        refused = Left (MetadataBoundExceeded (TooManyVersions 2 1))

    it "refuses at the first file position past the version ceiling" $ do
        packed limits (named firstTwo) `shouldBe` refused
        packed limits (named allThree) `shouldBe` refused

    it "accepts every file up to the version ceiling" $ do
        packed limits (named ("{\"files\":[" <> firstSdist <> "]")) `shouldBe` Right ()
        packed limits{maxVersionCount = 3} (named allThree) `shouldBe` Right ()

    it "counts files that name no release and entries that are not file objects" $
        for_ ["{\"filename\":\"not-a-distribution\",\"url\":\"https://files.pythonhosted.org/x\"}", "7", "null", "{\"filename\":\"thing-2.0.tar.gz\",\"url\":7}"] $ \other ->
            packed limits (named ("{\"files\":[" <> firstSdist <> "," <> other <> "]")) `shouldBe` refused

    it "does not count duplicate files arrays before a fault either" $
        packed limits (named ("{\"files\":[" <> firstSdist <> "],\"files\":[" <> BS.intercalate "," [firstWheel, secondSdist] <> "]"))
            `shouldBe` Right ()

    it "refuses before a late name, protocol or shape fault" $
        for_ [",\"name\":\"other\"}", ",\"name\":null}", ",\"name\":\"thing\",\"meta\":{\"api-version\":\"2.0\"}}", ",\"versions\":true,\"name\":\"thing\"}"] $ \tail' ->
            packed limits (firstTwo <> tail') `shouldBe` refused

    it "stops reading at the refusal, before a malformed body tail or a later nesting fault" $ do
        packed limits (firstTwo <> ",\"name\":\"thing\",\"unknown\":tru}") `shouldBe` refused
        packed limits (named ("{\"files\":[" <> BS.intercalate "," [firstSdist, firstWheel, "{\"filename\":\"thing-2.0.tar.gz\",\"url\":\"\\x\"}"] <> "]")) `shouldBe` refused
        packed limits{maxNestingDepth = 4} (firstTwo <> ",\"name\":\"thing\",\"files\":[{\"hashes\":{\"sha256\":\"00\"}}]}") `shouldBe` refused

    it "validates a late name before a late shape fault" $
        packed limits (oneFile <> ",\"name\":\"other\",\"versions\":true}") `shouldBe` Left (MetadataNameMismatch "other")

    it "validates a late protocol before name and shape faults" $
        packed limits (oneFile <> ",\"name\":\"other\",\"versions\":true,\"meta\":{\"api-version\":\"2.0\"}}")
            `shouldBe` Left MetadataUndecodable

limits :: Limits
limits = defaultLimits{maxVersionCount = 1}

packed :: Limits -> ByteString -> Either MetadataError ()
packed bounds body = void (projectPyPIPackedChunks bounds (unscopedPyPI "thing") [body] >>= projectPyPIPacked bounds (unscopedPyPI "thing"))

oneFile :: ByteString
oneFile = "{\"files\":[" <> firstSdist <> "]"

firstSdist, firstWheel, secondSdist :: ByteString
firstSdist = "{\"filename\":\"thing-1.0.tar.gz\",\"url\":\"https://files.pythonhosted.org/thing-1.0.tar.gz\"}"
firstWheel = "{\"filename\":\"thing-1.0-py3-none-any.whl\",\"url\":\"https://files.pythonhosted.org/thing-1.0-py3-none-any.whl\"}"
secondSdist = "{\"filename\":\"thing-2.0.tar.gz\",\"url\":\"https://files.pythonhosted.org/thing-2.0.tar.gz\"}"
