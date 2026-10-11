-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Deterministic negative controls for the live metadata verdict.
module Ecluse.Test.Contract.RegistrySmokeSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Package (PackageInfo (infoVersions), renderPackageName)
import Ecluse.Core.Registry.Metadata (Manifest (manifestInfo), MetadataError (MetadataNameMismatch))
import Ecluse.Core.Registry.Npm.Metadata (npmRead)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Test.Contract.RegistrySmoke (liveMetadata)
import Ecluse.Test.Corpus.Npm (benchPackageName, syntheticPackumentBytes)
import Ecluse.Test.Registry.Metadata.Fetch (heldManifest)
import Ecluse.Test.Support (expectRight)

-- | Broken provider facts cannot pass as live protocol evidence.
spec :: Spec
spec = describe "liveMetadata" $ do
    it "accepts a matching non-empty full read" $ do
        manifest <- sample
        info <- liveMetadata (renderPackageName benchPackageName) (pure (Right manifest))
        Map.size (infoVersions info) `shouldBe` 1
    it "rejects a provider that reports a different package" $ do
        manifest <- sample
        liveMetadata "different" (pure (Right manifest)) `shouldThrow` anyException
    it "rejects an empty provider result" $ do
        manifest <- sample
        let empty = manifest{manifestInfo = (manifestInfo manifest){infoVersions = Map.empty}}
        liveMetadata (renderPackageName benchPackageName) (pure (Right empty)) `shouldThrow` anyException
    it "fails a projection refusal instead of reporting successful coverage" $
        liveMetadata "expected" (pure (Left (MetadataNameMismatch "different"))) `shouldThrow` anyException

sample :: IO Manifest
sample = heldManifest npmRead defaultLimits "https://registry.npmjs.org" benchPackageName [syntheticPackumentBytes 1] >>= expectRight
