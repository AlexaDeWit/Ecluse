-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Full reads preserve refusal priority when a structural fault stops payload retention.
module Ecluse.Core.Registry.PyPI.StreamingProjectionSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Data.ByteString qualified as BS
import Data.List (findIndex)
import Hedgehog (Gen, assert, cover, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Package (PackageInfo, PackageName)
import Ecluse.Core.Registry.Json.Intern (tableTexts)
import Ecluse.Core.Registry.Json.Walk (Walked (..))
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Registry.Metadata (MetadataError (..))
import Ecluse.Core.Registry.PyPI.Document (SimpleDocument, packedSimpleDocument)
import Ecluse.Core.Registry.PyPI.Metadata (projectPyPIPacked)
import Ecluse.Core.Security (LimitError (TooDeeplyNested, TooManyArtifacts, TooManyVersions), Limits (..), defaultLimits)
import Ecluse.Test.Json (encodeStrict)
import Ecluse.Test.Package (unscopedPyPI)
import Ecluse.Test.Registry.PyPI (simpleFile, simpleIndex)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIIndex, projectPyPIPackedChunks)
import Ecluse.Test.Support (expectRight)

-- | Pin full-read refusal order against literal results and a count of release keys, both independent of the current reducer.
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

    describe "properties" $
        modifyMaxSuccess (const 1000) $
            it "agrees with the tree read and with a count of release keys on adjacent and split releases" $
                hedgehog $ do
                    versions <- forAll (Gen.int (Range.constant 1 3))
                    artifacts <- forAll (Gen.int (Range.constant 1 6))
                    entries <- forAll genEntries
                    let bounds = defaultLimits{maxVersionCount = versions, maxArtifactCount = artifacts}
                        body = encodeStrict (simpleIndex "thing" (map fst entries))
                        releases = mapMaybe snd entries
                        unbounded = treeRead defaultLimits body
                        versionFaultAt = findIndex (> versions) (map (length . ordNub) (drop 1 (inits releases)))
                        keptAfter at = length (filter (`elem` take versions (ordNub releases)) (drop (at + 1) releases))
                    cover 5 "a version fault after the artifact fault" (any (> artifacts) versionFaultAt)
                    cover 5 "the artifact ceiling trips after a version fault" (any (\at -> keptAfter at > 0 && at + keptAfter at > artifacts) versionFaultAt)
                    cover 20 "an artifact fault alone" (isNothing versionFaultAt && length releases > artifacts)
                    cover 20 "no fault" (isNothing (expectedBound bounds releases))
                    cover 15 "a release split across runs" (length (group releases) > length (ordNub releases))
                    assert (isRight unbounded)
                    treeRead bounds body === maybe unbounded (Left . MetadataBoundExceeded) (expectedBound bounds releases)
                    packedRead bounds body === treeRead bounds body

limits :: Limits
limits = defaultLimits{maxVersionCount = 1, maxArtifactCount = 1}

packed :: Limits -> ByteString -> Either MetadataError ()
packed bounds = void . packedRead bounds

packedRead, treeRead :: Limits -> ByteString -> Either MetadataError (PackageInfo, Maybe SimpleDocument)
packedRead bounds body = second packedSimpleDocument <$> (projectPyPIPackedChunks bounds thing [body] >>= projectPyPIPacked bounds thing)
treeRead bounds body = second Just <$> projectPyPIIndex bounds thing body

thing :: PackageName
thing = unscopedPyPI "thing"

-- The bound a full read reports from its release keys alone: any version fault outranks the artifact fault.
expectedBound :: Limits -> [Text] -> Maybe LimitError
expectedBound bounds releases
    | length (ordNub releases) > maxVersionCount bounds = Just (TooManyVersions (maxVersionCount bounds + 1) (maxVersionCount bounds))
    | length releases > maxArtifactCount bounds = Just (TooManyArtifacts (maxArtifactCount bounds + 1) (maxArtifactCount bounds))
    | otherwise = Nothing

-- Runs of one release's files, each with the release key it counts toward. "01.0" spells release
-- "1.0" a second way, and a foreign or undecodable entry counts toward none.
genEntries :: Gen [(Value, Maybe Text)]
genEntries = concat <$> Gen.list (Range.constant 0 6) genRun
  where
    genRun = do
        (release, spelling) <- Gen.element [("1.0", "1.0"), ("1.0", "01.0"), ("2.0", "2.0"), ("3.0rc1", "3.0rc1")]
        Gen.list (Range.constant 1 4) $
            Gen.frequency
                [ (6, (\suffix -> (simpleFile ("thing-" <> spelling <> suffix), Just release)) <$> Gen.element [".tar.gz", "-py3-none-any.whl"])
                , (1, pure (simpleFile "other-1.0.tar.gz", Nothing))
                , (1, pure (object ["filename" .= ("thing-4.0.tar.gz" :: Text)], Nothing))
                ]

retainedTexts :: ByteString -> Either MetadataError [Text]
retainedTexts body = do
    streamed <- projectPyPIPackedChunks limits thing [body]
    Walked table _ <- first (const MetadataUndecodable) (streamValue streamed)
    pure (toList (tableTexts table))

firstSdist, firstWheel, secondSdist :: ByteString
firstSdist = "{\"filename\":\"thing-1.0.tar.gz\",\"url\":\"https://files.pythonhosted.org/thing-1.0.tar.gz\"}"
firstWheel = "{\"filename\":\"thing-1.0-py3-none-any.whl\",\"url\":\"https://files.pythonhosted.org/thing-1.0-py3-none-any.whl\"}"
secondSdist = "{\"filename\":\"thing-2.0.tar.gz\",\"url\":\"https://files.pythonhosted.org/thing-2.0.tar.gz\"}"
