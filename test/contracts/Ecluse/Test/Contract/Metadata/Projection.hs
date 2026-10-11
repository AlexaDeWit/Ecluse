-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Full and selected projection laws, below native envelope and file-grouping tests.
module Ecluse.Test.Contract.Metadata.Projection (ProjectionDriver (..), metadataProjectionContract) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Package (PackageDetails, PackageInfo (infoVersions))
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataBoundExceeded, MetadataNameMismatch, MetadataUndecodable))
import Ecluse.Core.Security (LimitError (TooManyArtifacts, TooManyVersions), Limits (maxArtifactCount, maxVersionCount), defaultLimits)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Support (expectRight)

-- | Projection operations and native fixtures. The two releases each contain one artifact.
data ProjectionDriver = ProjectionDriver
    { projectFull :: Limits -> ByteString -> Either MetadataError PackageInfo
    , projectSelected :: Limits -> Text -> ByteString -> Either MetadataError (Maybe PackageDetails)
    , projectionDocument :: Text -> [Text] -> ByteString
    , projectionPackageName :: Text
    , projectionOtherName :: Text
    , projectionVersions :: [Text]
    , projectionMissingName :: ByteString
    }

-- | Compare only shared promises. Selected artifact ceilings remain native protocol tests.
metadataProjectionContract :: Text -> ProjectionDriver -> Spec
metadataProjectionContract provider driver = describe "shared metadata projection" $ do
    contractExample "malformed" "rejects malformed input on full and selected reads" $ do
        void (full defaultLimits "{not json") `shouldBe` Left MetadataUndecodable
        for_ versions $ \version ->
            void (selected defaultLimits version "{not json") `shouldBe` Left MetadataUndecodable
    contractExample "missing-name" "rejects a missing identity on full and selected reads" $ do
        void (full defaultLimits (projectionMissingName driver)) `shouldBe` Left MetadataUndecodable
        for_ versions $ \version ->
            void (selected defaultLimits version (projectionMissingName driver)) `shouldBe` Left MetadataUndecodable
    contractExample "name-mismatch" "rejects another package's identity on full and selected reads" $ do
        let body = projectionDocument driver (projectionOtherName driver) versions
            expected = Left (MetadataNameMismatch (projectionOtherName driver))
        void (full defaultLimits body) `shouldBe` expected
        for_ versions $ \version ->
            void (selected defaultLimits version body) `shouldBe` expected
    contractExample "version-bound" "rejects two entries above a one-entry version ceiling" $ do
        let limits = defaultLimits{maxVersionCount = 1}
            expected = Left (MetadataBoundExceeded (TooManyVersions 2 1))
        length versions `shouldBe` 2
        void (full limits document) `shouldBe` expected
        for_ versions $ \version ->
            void (selected limits version document) `shouldBe` expected
    contractExample "artifact-bound" "bounds full-read artifacts and accepts the exact ceiling" $ do
        void (full defaultLimits{maxArtifactCount = 1} document) `shouldBe` Left (MetadataBoundExceeded (TooManyArtifacts 2 1))
        full defaultLimits{maxArtifactCount = 2} document `shouldSatisfy` isRight
    contractExample "missing-version" "returns no details for a missing version" $
        selected defaultLimits "9.9.9" document `shouldBe` Right Nothing
    contractExample "selected-parity" "projects every selected version identically to the full document" $ do
        info <- expectRight (full defaultLimits document)
        Map.keys (infoVersions info) `shouldBe` sort versions
        for_ versions $ \version -> do
            Map.lookup version (infoVersions info) `shouldSatisfy` isJust
            selected defaultLimits version document `shouldBe` Right (Map.lookup version (infoVersions info))
  where
    contractExample identifier description = it (caseLabel ("core.metadata.projection." <> provider <> "." <> identifier) description)
    full = projectFull driver
    selected = projectSelected driver
    versions = projectionVersions driver
    document = projectionDocument driver (projectionPackageName driver) versions
