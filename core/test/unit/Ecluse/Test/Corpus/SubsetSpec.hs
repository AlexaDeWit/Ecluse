-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Test.Corpus.SubsetSpec (spec) where

import Data.Aeson (Value (Array, Object, String), encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.Ratio ((%))
import Test.Hspec

import Ecluse.Core.Security (defaultLimits)
import Ecluse.Test.Corpus.Subset (newestNpmShare, newestPyPIShare, oldestVersions)
import Ecluse.Test.Package (unscopedNpm, unscopedPyPI, validSha1, validSha512Sri)
import Ecluse.Test.Registry.Npm (VersionSpec (vsIntegrity, vsShasum), packumentValue, versionSpec, versionValue)
import Ecluse.Test.Registry.Npm.Metadata (projectNpmManifest)
import Ecluse.Test.Registry.PyPI (simpleFile, simpleIndexWith, withFileKeys)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIIndex)

spec :: Spec
spec = do
    describe "newestNpmShare" $ do
        it "keeps the newest share by publish time, with those versions' times and tags" $ do
            let cut = newestNpmShare (1 % 2) (unscopedNpm "thing") npmCapture
            keysOf "versions" <$> cut `shouldBe` Right ["2.0.0", "4.0.0"]
            keysOf "time" <$> cut `shouldBe` Right ["2.0.0", "4.0.0", "created", "modified"]
            keysOf "dist-tags" <$> cut `shouldBe` Right ["beta", "latest"]

        it "keeps at least the newest version" $
            keysOf "versions" <$> newestNpmShare 0 (unscopedNpm "thing") npmCapture `shouldBe` Right ["4.0.0"]

    describe "oldestVersions" $ do
        it "keeps the oldest share of an npm packument by publish time" $
            (sort . toList . oldestVersions (1 % 2) . fst <$> projectNpmManifest defaultLimits (unscopedNpm "thing") npmCapture)
                `shouldBe` Right ["1.0.0", "3.0.0"]

        it "keeps the oldest share of a Simple index by upload time" $
            (sort . toList . oldestVersions (1 % 2) . fst <$> projectPyPIIndex defaultLimits (unscopedPyPI "thing") pypiCapture)
                `shouldBe` Right ["1", "3"]

    describe "newestPyPIShare" $ do
        it "keeps the files and listed versions of the newest share by upload time" $ do
            let cut = newestPyPIShare (1 % 2) (unscopedPyPI "thing") pypiCapture
            filenamesOf <$> cut `shouldBe` Right ["thing-2.0.tar.gz", "thing-2.0-py3-none-any.whl", "thing-3.0.tar.gz"]
            stringsAt "versions" <$> cut `shouldBe` Right ["2.0", "3.0"]

        it "keeps at least the newest version" $
            filenamesOf <$> newestPyPIShare 0 (unscopedPyPI "thing") pypiCapture `shouldBe` Right ["thing-2.0.tar.gz", "thing-2.0-py3-none-any.whl"]

-- Four releases whose key order differs from their publish order: 3.0.0 is a later backport of 2.0.0.
npmCapture :: ByteString
npmCapture =
    LBS.toStrict . encode $
        withDistTags
            ( packumentValue
                "thing"
                "4.0.0"
                [(version, release version) | version <- ["1.0.0", "2.0.0", "3.0.0", "4.0.0"]]
                [ "created" .= ("2020-01-01T00:00:00.000Z" :: Text)
                , "modified" .= ("2020-01-04T00:00:00.000Z" :: Text)
                , "1.0.0" .= ("2020-01-01T00:00:00.000Z" :: Text)
                , "2.0.0" .= ("2020-01-03T00:00:00.000Z" :: Text)
                , "3.0.0" .= ("2020-01-02T00:00:00.000Z" :: Text)
                , "4.0.0" .= ("2020-01-04T00:00:00.000Z" :: Text)
                ]
                []
            )
  where
    release version =
        versionValue
            (versionSpec "thing" version ("https://registry.npmjs.org/thing/-/thing-" <> version <> ".tgz"))
                { vsIntegrity = Just validSha512Sri
                , vsShasum = Just validSha1
                }
    withDistTags = \case
        Object fields -> Object (KeyMap.insert "dist-tags" (object ["latest" .= ("4.0.0" :: Text), "beta" .= ("2.0.0" :: Text), "legacy" .= ("3.0.0" :: Text)]) fields)
        other -> other

-- Three releases, uploaded out of version order, with two files for 2.0.
pypiCapture :: ByteString
pypiCapture =
    LBS.toStrict . encode $
        simpleIndexWith
            "thing"
            ["meta" .= object ["api-version" .= ("1.1" :: Text)], "versions" .= (["1.0", "2.0", "3.0"] :: [Text])]
            [ uploadedAt "2020-01-01T00:00:00Z" "thing-1.0.tar.gz"
            , uploadedAt "2020-01-03T00:00:00Z" "thing-2.0.tar.gz"
            , uploadedAt "2020-01-03T00:00:00Z" "thing-2.0-py3-none-any.whl"
            , uploadedAt "2020-01-02T00:00:00Z" "thing-3.0.tar.gz"
            ]
  where
    uploadedAt time filename = withFileKeys [("upload-time", String time)] (simpleFile filename)

keysOf :: Text -> Value -> [Text]
keysOf field = \case
    Object fields | Just (Object inner) <- KeyMap.lookup (Key.fromText field) fields -> sort (map Key.toText (KeyMap.keys inner))
    _ -> []

stringsAt :: Text -> Value -> [Text]
stringsAt field = \case
    Object fields | Just (Array items) <- KeyMap.lookup (Key.fromText field) fields -> [text | String text <- toList items]
    _ -> []

filenamesOf :: Value -> [Text]
filenamesOf = \case
    Object fields | Just (Array files) <- KeyMap.lookup "files" fields -> [name | Object file <- toList files, Just (String name) <- [KeyMap.lookup "filename" file]]
    _ -> []
