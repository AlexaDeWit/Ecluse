-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Keep the buffered measurement reference independent of streaming acceptance.
module Ecluse.Test.Registry.Metadata.ProjectionSpec (spec) where

import Data.Aeson (object, (.=))
import Data.Map.Strict qualified as Map
import Ecluse.Core.Package (PackageInfo (infoVersions))
import Ecluse.Core.Registry (ParseError (ParseError))
import Ecluse.Core.Registry.Metadata (Manifest (manifestInfo), MetadataError (MetadataBoundExceeded, MetadataNameMismatch, MetadataUndecodable))
import Ecluse.Core.Registry.WireSupport (Projection (NameMismatch, Projected))
import Ecluse.Core.Security (LimitError (TooDeeplyNested, TooManyArtifacts, TooManyVersions), Limits (maxArtifactCount, maxNestingDepth, maxVersionCount), defaultLimits)
import Ecluse.Test.Package (sampleManifest, thingName, v1_0_0)
import Ecluse.Test.Registry.Metadata.Projection (projectMetadata)
import Test.Hspec

spec :: Spec
spec = do
    describe "projectMetadata" $ do
        for_ ["{", "{} trailing", "{} {}"] $ \body ->
            it ("rejects malformed or trailing JSON: " <> show body) $
                projectMetadata (const (Right (NameMismatch "other"))) defaultLimits body
                    `shouldBe` Left MetadataUndecodable

        it "checks depth before the ecosystem projector" $
            projectMetadata (const (Left (ParseError "projector failed"))) defaultLimits{maxNestingDepth = 1} "{\"nested\":null}"
                `shouldBe` Left (MetadataBoundExceeded (TooDeeplyNested 1))

        it "maps an ecosystem projector failure before count checks" $
            projectMetadata (const (Left (ParseError "bad name"))) noEntries "{}"
                `shouldBe` Left MetadataUndecodable

        it "preserves a name mismatch before count checks" $
            projectMetadata (const (Right (NameMismatch "Other.Project"))) noEntries "{}"
                `shouldBe` Left (MetadataNameMismatch "Other.Project")

        it "checks version count before artifact count with exact counts" $
            projectMetadata (const (Right (Projected oneVersion))) noEntries "{}"
                `shouldBe` Left (MetadataBoundExceeded (TooManyVersions 1 0))

        it "checks artifact count after an accepted version count" $
            projectMetadata (const (Right (Projected oneVersion))) defaultLimits{maxArtifactCount = 0} "{}"
                `shouldBe` Left (MetadataBoundExceeded (TooManyArtifacts 1 0))

        it "counts projected entries rather than raw fields" $
            projectMetadata (const (Right (Projected emptyInfo))) noEntries "{\"raw\":[1,2,3]}"
                `shouldBe` Right (emptyInfo, object ["raw" .= ([1, 2, 3] :: [Int])])

        it "gives the projector the same raw value retained for assembly" $ do
            let raw = object ["unknown" .= object ["kept" .= True], "number" .= (7 :: Int)]
                project value = if value == raw then Right (Projected oneVersion) else Left (ParseError "changed input")
            projectMetadata project defaultLimits "{\"unknown\":{\"kept\":true},\"number\":7}"
                `shouldBe` Right (oneVersion, raw)

noEntries :: Limits
noEntries = defaultLimits{maxVersionCount = 0, maxArtifactCount = 0}

oneVersion :: PackageInfo
oneVersion = manifestInfo (sampleManifest thingName [v1_0_0])

emptyInfo :: PackageInfo
emptyInfo = oneVersion{infoVersions = Map.empty}
