-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | How the load fixtures read the corpus as a private upstream's cut copy.
module Ecluse.BenchLoad.FixtureSpec (spec) where

import Data.Aeson (Value, encode, toJSON)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Data.Text qualified as T
import Test.Hspec

import Ecluse.BenchLoad.Error (BenchLoadError (BenchLoadError))
import Ecluse.BenchLoad.Fixture (loadCorpusCuts)
import Ecluse.Core.Package (PackageInfo (infoVersions), PackageName, renderPackageName)
import Ecluse.Core.Registry.Metadata (MetadataError)
import Ecluse.Core.Security (Limits, defaultLimits)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpPath, cpTier), CorpusTier (Medium), corpusPackages, cpName, pypiCorpusPackages)
import Ecluse.Test.Corpus.Subset (newestNpmShare, newestPyPIShare)
import Ecluse.Test.Registry.Npm.Metadata (projectNpmManifest)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIIndex)

spec :: Spec
spec = describe "loadCorpusCuts" $ do
    it "serves each capture as its cut, keyed by the package name" $ do
        sizes <- traverse (fmap BS.length . readFileBS . cpPath) mediumNpm
        cuts <- loadCorpusCuts (\name bytes -> Right (toJSON (renderPackageName name, BS.length bytes))) mediumNpm
        cuts `shouldBe` Map.fromList [(cpName cp, encode (cpName cp, size)) | (cp, size) <- zip mediumNpm sizes]

    it "refuses a capture the cut rejects" $
        loadCorpusCuts (\_ _ -> Left "no versions") mediumNpm
            `shouldThrow` (\(BenchLoadError message) -> "no versions" `T.isInfixOf` message)

    it "keeps the newest 5% of every npm capture: fewer versions than the capture, and at least one" $
        keepsFewer newestNpmShare projectNpmManifest corpusPackages

    it "keeps the newest 5% of every PyPI capture: fewer versions than the capture, and at least one" $
        keepsFewer newestPyPIShare projectPyPIIndex pypiCorpusPackages
  where
    mediumNpm = filter ((== Medium) . cpTier) corpusPackages

keepsFewer ::
    (Rational -> PackageName -> ByteString -> Either String Value) ->
    (Limits -> PackageName -> ByteString -> Either MetadataError (PackageInfo, b)) ->
    [CorpusPackage] ->
    Expectation
keepsFewer cut project packages = do
    cuts <- loadCorpusCuts (cut (5 % 100)) packages
    for_ packages $ \cp -> do
        capture <- readFileBS (cpPath cp)
        let versions :: ByteString -> Either Text Int
            versions = bimap show (Map.size . infoVersions . fst) . project defaultLimits (cpPackage cp)
        (cpName cp, versions . toStrict <$> Map.lookup (cpName cp) cuts, versions capture) `shouldSatisfy` \case
            (_, Just (Right kept), Right whole) -> kept >= 1 && kept < whole
            _ -> False
