-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | How the load fixtures read the corpus as a private upstream's cut copy.
module Ecluse.BenchLoad.FixtureSpec (spec) where

import Data.Aeson (encode, toJSON)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.BenchLoad.Error (BenchLoadError (BenchLoadError))
import Ecluse.BenchLoad.Fixture (loadCorpusCuts)
import Ecluse.Core.Package (renderPackageName)
import Ecluse.Test.Corpus (CorpusPackage (cpPath, cpTier), CorpusTier (Medium), corpusPackages, cpName)

spec :: Spec
spec = describe "loadCorpusCuts" $ do
    it "serves each capture as its cut, keyed by the package name" $ do
        sizes <- traverse (fmap BS.length . readFileBS . cpPath) packages
        cuts <- loadCorpusCuts (\name bytes -> Right (toJSON (renderPackageName name, BS.length bytes))) packages
        cuts `shouldBe` Map.fromList [(cpName cp, encode (cpName cp, size)) | (cp, size) <- zip packages sizes]

    it "refuses a capture the cut rejects" $
        loadCorpusCuts (\_ _ -> Left "no versions") packages
            `shouldThrow` (\(BenchLoadError message) -> "no versions" `T.isInfixOf` message)
  where
    packages = filter ((== Medium) . cpTier) corpusPackages
