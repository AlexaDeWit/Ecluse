-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Policy timestamps and artifact coordinates stay joined to their source release key.
module Ecluse.Core.Registry.Npm.StreamingProjectionSpec (spec) where

import Control.Monad (foldM)
import Data.Aeson (Value (Null, Number, String), object, (.=))
import Data.Map.Strict qualified as Map
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (InvalidEntryKind (..), PackageDetails (pkgPublishedAt), PackageInfo (infoDistTags, infoInvalidEntries, infoVersions), invalidKey, invalidKind, invalidValue)
import Ecluse.Core.Package.Merge (Provenance (GatedSource), mergePackuments)
import Ecluse.Core.Registry.CachedDocument (npmCached)
import Ecluse.Core.Registry.Npm.Filter (assembleMergedPackument)
import Ecluse.Core.Registry.Npm.Metadata (selectNpmVersionDoc)
import Ecluse.Core.Registry.Npm.Streaming (NpmContainer (..), NpmField (..))
import Ecluse.Core.Registry.Npm.StreamingProjection
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Snapshot (Snapshot (..))
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Json (fieldAt, withKeys)
import Ecluse.Test.Package (unscopedNpm)
import Ecluse.Test.Registry.Npm qualified as NpmFixture
import Ecluse.Test.Snapshot (digestOf)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "finishProjection" $ do
    it "joins release timestamps and tags independently of source map order" $ hedgehog $ do
        ordered <- forAll (Gen.shuffle sourceFields)
        project (concat ordered) === project (concat sourceFields)

    it "retains only raw bookkeeping while typed timestamps and tags survive" $ do
        (info, raw) <- expectRight (project (concat sourceFields))
        fieldAt "time" raw `shouldBe` Just bookkeeping
        fieldAt "dist-tags" raw `shouldBe` Nothing
        (Map.lookup "1.0.0" (infoVersions info) >>= pkgPublishedAt) `shouldSatisfy` isJust
        Map.lookup "latest" (infoDistTags info) `shouldBe` Just (mkVersion Npm "1.0.0")
        infoInvalidEntries info `shouldBe` []

    it "preserves the first time, tag and bookkeeping entries and first containers" $ do
        let duplicateFields =
                [ [NameField (String "thing")]
                , [BeginContainer VersionsContainer, VersionField "1.0.0" (Just release)]
                , [BeginContainer TimeContainer, TimeField "1.0.0" timestamp, TimeField "1.0.0" Null, TimeField "created" Null, TimeField "created" timestamp, TimeField "modified" (Number 7)]
                , [BeginContainer TagsContainer, TagField "latest" (String "1.0.0"), TagField "latest" Null]
                , [BeginContainer TimeContainer, TimeField "1.0.0" Null, TimeField "created" timestamp]
                , [BeginContainer TagsContainer, TagField "latest" Null, TagField "other" (String "1.0.0")]
                ]
        project (concat duplicateFields) `shouldBe` project (concat sourceFields)

    it "keeps first invalid diagnostics without replacing them with later valid entries" $ do
        let fields =
                [ NameField (String "thing")
                , BeginContainer VersionsContainer
                , VersionField "1.0.0" (Just release)
                , BeginContainer TimeContainer
                , TimeField "1.0.0" (Number 9)
                , TimeField "1.0.0" timestamp
                , TimeField "absent" Null
                , BeginContainer TagsContainer
                , TagField "latest" (Number 8)
                , TagField "latest" (String "1.0.0")
                ]
        (info, raw) <- expectRight (project fields)
        map (\entry -> (invalidKind entry, invalidKey entry, invalidValue entry)) (infoInvalidEntries info)
            `shouldBe` [(InvalidDistTag, "latest", Number 8), (InvalidPublishTime, "1.0.0", Number 9)]
        (Map.lookup "1.0.0" (infoVersions info) >>= pkgPublishedAt) `shouldBe` Nothing
        infoDistTags info `shouldBe` mempty
        fieldAt "time" raw `shouldBe` Just (object [])

    it "assembles and selects mirror metadata identically without the discarded raw maps" $ do
        (info, compact) <- expectRight (project (concat sourceFields))
        let expanded = withKeys [("time", withKeys [("1.0.0", timestamp)] bookkeeping), ("dist-tags", object ["latest" .= ("1.0.0" :: Text)])] compact
            snapshot = Snapshot (digestOf "same source bytes")
            assemble plan raw = assembleMergedPackument "https://proxy.example/npm" (Map.singleton 0 (snapshot raw)) plan raw
            select raw = selectNpmVersionDoc (mkVersion Npm "1.0.0") (fst npmCached raw)
        plan <- expectRight (maybeToRight ("expected merge plan" :: Text) (mergePackuments [(GatedSource, snapshot info)]))
        assemble plan compact `shouldBe` assemble plan expanded
        select compact `shouldBe` select expanded

sourceFields :: [[NpmField]]
sourceFields =
    [ [NameField (String "thing")]
    , [BeginContainer VersionsContainer, VersionField "1.0.0" (Just release), IgnoredField]
    , [BeginContainer TimeContainer, TimeField "1.0.0" timestamp, TimeField "created" Null, TimeField "modified" (Number 7), IgnoredField]
    , [BeginContainer TagsContainer, TagField "latest" (String "1.0.0"), IgnoredField]
    ]

project :: [NpmField] -> Either Text (PackageInfo, Value)
project fields = do
    collected <- first show (foldM (collectField defaultLimits name) emptyProjection fields)
    first show (finishProjection defaultLimits name "See source" collected)
  where
    name = unscopedNpm "thing"

release :: Value
release = NpmFixture.versionValue (NpmFixture.versionSpec "thing" "1.0.0" "https://source.example/one.tgz")

timestamp :: Value
timestamp = String "2020-01-01T00:00:00Z"

bookkeeping :: Value
bookkeeping = object ["created" .= Null, "modified" .= Number 7]
